"""
Scattering annotation points over the stills.

CoralNet-Toolbox samples random points and asks an annotator to label what sits
under each one; percent cover is then the share of points falling on each
class. This does the same sampling ourselves, so a transect can go to
Zooniverse without passing through Toolbox first.

Two properties matter and are worth stating outright:

* **The sample is reproducible.** Each image's points come from a generator
  seeded with the run seed *and the image's name*, so re-running a transect
  lands on exactly the same points, and adding or dropping one image does not
  move the points in any of the others.
* **Points stay clear of the edges.** A point closer to the frame edge than the
  margin gives a Zooniverse crop that is mostly black border, and a classifier
  patch that is half out of frame.
"""

from __future__ import annotations

import random
import threading
from collections.abc import Callable, Iterable
from dataclasses import dataclass
from pathlib import Path

from .config import SampleSettings
from .logging_setup import get_logger

log = get_logger("sampling")


@dataclass(frozen=True)
class Point:
    """One annotation point, in the source image's pixel coordinates."""

    name: str          # the source image's file name
    path: Path         # the source image
    row: int           # y
    column: int        # x
    patch_size: int


def image_size(path: Path) -> tuple[int, int] | None:
    """(width, height) from the file header, without decoding the pixels."""
    try:
        from PIL import Image

        with Image.open(path) as im:
            return im.size
    except Exception as exc:
        log.warning(f"Could not read image size for {path.name}: {exc}")
        return None


def safe_inset(patch_size: int, margin: int, scale: float) -> int:
    """How far from the frame edge the nearest point may sit.

    Two things have to clear the edge, and the larger of them decides:

    * **the margin** -- no point lands within ``margin`` px of the frame edge.
    * **the Zooniverse crop**, ``patch_size * scale`` across. It has to fit
      inside the frame whole, or it gets slid back in and the green box is no
      longer in the middle of what the volunteer sees. So the point sits at
      least ``crop/2`` in.

    Working this out *before* sampling is the whole point. Sampling against
    the margin alone and sliding the overhanging crops back inside afterwards
    leaves the box off centre on exactly those patches -- on our 4606x4030
    stills, 17% of them, by up to 167 px -- and that is a silent change to
    what the volunteer is being asked about.

    At our settings the crop decides: 224 x 4.5 is a 1,008 px crop, so the
    inset is 504 rather than 224. A scale small enough to fit inside the
    margin leaves the margin in charge.
    """
    crop_half = int(round(patch_size * scale)) // 2
    return max(margin, crop_half)


def sample_image(width: int, height: int, count: int, patch_size: int,
                 margin: int, rng: random.Random,
                 scale: float = 1.0) -> list[tuple[int, int]]:
    """`count` distinct (row, column) points inside the safe area.

    The inset is trimmed rather than enforced when an image is too small for
    it: a smaller safe area still produces usable patches, whereas refusing the
    image loses the frame entirely. A trimmed inset is the one case where a
    crop can still overhang, and `patches.geometry` reports it.
    """
    inset = safe_inset(patch_size, margin, scale)
    safe_x = min(inset, max(0, (width - 1) // 2))
    safe_y = min(inset, max(0, (height - 1) // 2))
    x_lo, x_hi = safe_x, max(safe_x, width - 1 - safe_x)
    y_lo, y_hi = safe_y, max(safe_y, height - 1 - safe_y)

    # Distinct points keep patch filenames unique, since the filename is built
    # from the coordinates. Rejection is cheap: 50 points in a 4,000 px frame
    # collide about once in a hundred thousand draws.
    seen: set[tuple[int, int]] = set()
    attempts = 0
    limit = max(1, count) * 50
    while len(seen) < count and attempts < limit:
        attempts += 1
        seen.add((rng.randint(y_lo, y_hi), rng.randint(x_lo, x_hi)))
    return sorted(seen)


def sample(images: Iterable[Path], settings: SampleSettings,
           scale: float = 1.0,
           progress: Callable[[float, str], None] | None = None,
           cancel: threading.Event | None = None) -> list[Point]:
    """Points for every image, in image order.

    `scale` is the crop stage 2 will cut around each point. Sampling needs it
    because the crop, not the margin, is usually what has to clear the frame
    edge -- see `safe_inset`.
    """
    images = list(images)
    points: list[Point] = []
    skipped = 0

    for i, path in enumerate(images):
        if cancel is not None and cancel.is_set():
            break
        size = image_size(path)
        if size is None:
            skipped += 1
            continue
        width, height = size
        rng = random.Random(f"{settings.seed}:{path.name}")
        for row, column in sample_image(width, height,
                                        settings.points_per_image,
                                        settings.patch_size,
                                        settings.margin, rng, scale):
            points.append(Point(path.name, path, row, column,
                                settings.patch_size))
        if progress:
            progress((i + 1) / max(1, len(images)),
                     f"Sampling points - {i + 1}/{len(images)} images")

    if skipped:
        log.warning(f"{skipped} image(s) could not be read and were skipped.")
    inset = safe_inset(settings.patch_size, settings.margin, scale)
    log.info(f"Sampled {len(points):,} point(s) across "
             f"{len(images) - skipped} image(s); no point within {inset} px "
             f"of the frame edge (margin {settings.margin}, "
             f"crop {int(round(settings.patch_size * scale))}).")
    return points
