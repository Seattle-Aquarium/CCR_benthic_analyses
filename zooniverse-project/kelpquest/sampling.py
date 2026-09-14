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


def sample_image(width: int, height: int, count: int, patch_size: int,
                 margin: int, rng: random.Random) -> list[tuple[int, int]]:
    """`count` distinct (row, column) points inside the margin.

    The margin is trimmed rather than enforced when an image is too small for
    it: a smaller safe area still produces usable patches, whereas refusing the
    image loses the frame entirely.
    """
    safe_x = min(margin, max(0, (width - 1) // 2))
    safe_y = min(margin, max(0, (height - 1) // 2))
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
           progress: Callable[[float, str], None] | None = None,
           cancel: threading.Event | None = None) -> list[Point]:
    """Points for every image, in image order."""
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
                                        settings.margin, rng):
            points.append(Point(path.name, path, row, column,
                                settings.patch_size))
        if progress:
            progress((i + 1) / max(1, len(images)),
                     f"Sampling points - {i + 1}/{len(images)} images")

    if skipped:
        log.warning(f"{skipped} image(s) could not be read and were skipped.")
    log.info(f"Sampled {len(points):,} point(s) across "
             f"{len(images) - skipped} image(s).")
    return points
