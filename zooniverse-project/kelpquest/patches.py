"""
Cutting and drawing the patch a volunteer sees.

Each subject is a crop several times wider than the annotation patch, so there
is enough context to judge what the point is sitting on, with three things
drawn over it:

* a **green box** around the patch itself - the area the label describes,
* a **red crosshair** on the exact point, which is what percent cover counts,
* the **predicted label**, which the volunteer is asked to agree or disagree
  with.

The drawing follows ``scripts/toolbox_to_subjects.py`` so patches made here sit
alongside the ones already on Zooniverse without looking like a different task,
with one deliberate departure: the label's outline is thicker. See
``HALO_PER_SCALE`` for why. Patches made before that change have a thinner
outline; nothing else about them differs.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

import cv2
import numpy as np

GREEN = (0, 255, 0)
RED = (0, 0, 255)
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)


# --------------------------------------------------------------------------
#  File IO
# --------------------------------------------------------------------------
#
# cv2.imread and cv2.imwrite go through the ANSI Windows API and fail on any
# path the local code page cannot represent -- which on a Dropbox share is one
# accented surname away. Reading the bytes ourselves and letting OpenCV decode
# them sidesteps the whole question.


def imread(path: str | Path):
    """Decode an image, whatever the path looks like. None if unreadable."""
    try:
        buf = np.fromfile(str(path), dtype=np.uint8)
    except OSError:
        return None
    if buf.size == 0:
        return None
    return cv2.imdecode(buf, cv2.IMREAD_COLOR)


def imwrite(path: str | Path, image, jpeg_quality: int = 100) -> bool:
    """Encode and write. Returns whether it landed."""
    path = Path(path)
    ok, buf = cv2.imencode(path.suffix or ".jpg", image,
                           [cv2.IMWRITE_JPEG_QUALITY, int(jpeg_quality)])
    if not ok:
        return False
    try:
        path.parent.mkdir(parents=True, exist_ok=True)
        buf.tofile(str(path))
    except OSError:
        return False
    return True


# --------------------------------------------------------------------------
#  Geometry
# --------------------------------------------------------------------------


@dataclass(frozen=True)
class Geometry:
    """Where the crop sits in the source, and the patch inside the crop."""

    left: int
    top: int
    right: int
    bottom: int
    rect_left: int
    rect_top: int
    rect_right: int
    rect_bottom: int
    cx: int            # the annotation point, in crop coordinates
    cy: int


def geometry(width: int, height: int, row: int, column: int,
             patch_size: int, scale: float) -> Geometry:
    """Crop bounds for one point, shifted inward at the frame edges.

    The crop keeps its full requested size wherever possible by sliding back
    into the frame rather than clipping, so patches are all one size and the
    point is only off-centre for the handful that sit near an edge.
    """
    half = patch_size // 2
    small_top = max(row - half, 0)
    small_bottom = min(row + half, height)
    small_left = max(column - half, 0)
    small_right = min(column + half, width)

    crop = int(round(patch_size * scale))
    crop_half = crop // 2
    left = max(0, column - crop_half)
    top = max(0, row - crop_half)
    right = left + crop
    bottom = top + crop
    if right > width:
        left -= right - width
        right = width
    if bottom > height:
        top -= bottom - height
        bottom = height
    left = max(0, left)
    top = max(0, top)

    return Geometry(
        left=left, top=top, right=right, bottom=bottom,
        rect_left=small_left - left, rect_top=small_top - top,
        rect_right=small_right - left, rect_bottom=small_bottom - top,
        cx=column - left, cy=row - top,
    )


def tight_patch(image, row: int, column: int, patch_size: int):
    """The patch itself, at native resolution - what the classifier sees.

    Taken from the source image rather than from the saved crop: the crop is a
    JPEG, and classifying a re-compressed copy of the training geometry is a
    quiet way to lose a point or two of accuracy.
    """
    height, width = image.shape[:2]
    half = patch_size // 2
    top = max(0, min(row - half, height - patch_size))
    left = max(0, min(column - half, width - patch_size))
    return image[top:top + patch_size, left:left + patch_size]


def patch_filename(image_name: str, row: int, column: int) -> str:
    """<source stem>_r<row>_c<col>.jpg - the name the upload log keys on."""
    stem = Path(image_name).stem
    return f"{stem}_r{row}_c{column}.jpg"


# --------------------------------------------------------------------------
#  Overlays
# --------------------------------------------------------------------------


def draw_patch_box(image, geom: Geometry) -> None:
    """Black / green / black, so the box reads on a light or a dark seafloor."""
    cv2.rectangle(image, (geom.rect_left - 2, geom.rect_top - 2),
                  (geom.rect_right + 2, geom.rect_bottom + 2),
                  BLACK, 1, lineType=cv2.LINE_AA)
    cv2.rectangle(image, (geom.rect_left, geom.rect_top),
                  (geom.rect_right, geom.rect_bottom),
                  GREEN, 2, lineType=cv2.LINE_AA)
    cv2.rectangle(image, (geom.rect_left + 2, geom.rect_top + 2),
                  (geom.rect_right - 2, geom.rect_bottom - 2),
                  BLACK, 1, lineType=cv2.LINE_AA)


def draw_center_marker(image, x: int, y: int, patch_size: int) -> None:
    """Ring, centre dot and four ticks on the exact point.

    Sizes scale with the patch but are clamped: on a 224 px patch the ring has
    to be big enough to find and small enough not to hide what it marks.
    """
    height, width = image.shape[:2]
    if not (0 <= x < width and 0 <= y < height):
        return

    radius = max(6, min(int(patch_size * 0.06), 14))
    ring = max(1, min(int(patch_size * 0.01), 3))
    tick = max(10, min(int(patch_size * 0.10), 22))
    tick_w = max(1, min(int(patch_size * 0.01), 3))
    gap = max(3, min(int(patch_size * 0.02), 6))

    cv2.circle(image, (x, y), radius, RED, ring, lineType=cv2.LINE_AA)
    cv2.circle(image, (x, y), 2, RED, -1, lineType=cv2.LINE_AA)
    for dx, dy in ((0, -1), (0, 1), (-1, 0), (1, 0)):
        start = (x + dx * (radius + gap), y + dy * (radius + gap))
        end = (x + dx * (radius + gap + tick), y + dy * (radius + gap + tick))
        cv2.line(image, start, end, RED, tick_w, lineType=cv2.LINE_AA)


#: Black halo width as a share of the glyph height. The outline is what makes
#: the label readable at all, and the ground it sits on is not ours to choose:
#: a silt frame is pale, low-contrast and almost the same value as white type.
#:
#: A fixed ``thickness + 3`` -- which is what this and the older script did --
#: gives 1.5 px of black either side of a 27 px-tall letter. Six percent of the
#: glyph, which reads as no outline at all on exactly the frames that need one.
#:
#: Measured from the text's own height rather than from the nominal font scale,
#: because the two are not proportional across OpenCV versions: at the same
#: scale, 5.0's font measures 34 px tall against 4.13's 27 px, and is visibly
#: bolder. Taking 12% of whichever it is keeps the outline looking the same
#: weight on both.
HALO_PER_TEXT_HEIGHT = 0.12


def _halo(text_height: int) -> int:
    return max(2, int(round(text_height * HALO_PER_TEXT_HEIGHT)))


def outlined_text(image, text: str, org: tuple[int, int], font: int,
                  scale: float, thickness: int, halo: int) -> None:
    """White text with a black halo `halo` px wide, in any OpenCV version.

    Deliberately *not* two ``putText`` passes at different thicknesses, which
    is the usual trick and what this and ``toolbox_to_subjects.py`` used to do.
    OpenCV 5 rewrote ``putText`` and saturates thickness at 2: passes at 2, 5,
    8 and 14 all lay down identical ink, so the wider black pass is drawn at
    the white pass's width and then completely covered by it. Measured on
    OpenCV 5.0.0, the black area came out at 0 px -- no outline at all, which
    is exactly what turned up on a run made through the launcher's venv while
    the same code looked right under OpenCV 4.13.

    Dilating a text mask gives an exact halo, keeps the antialiasing, and
    behaves the same on both. It works on a crop around the text, so the cost
    is a few hundred pixels rather than the whole patch.
    """
    (text_w, text_h), baseline = cv2.getTextSize(text, font, scale, thickness)
    pad = halo + 2
    height, width = image.shape[:2]
    x0 = max(0, org[0] - pad)
    y0 = max(0, org[1] - text_h - pad)
    x1 = min(width, org[0] + text_w + pad)
    y1 = min(height, org[1] + baseline + pad)
    if x1 <= x0 or y1 <= y0:
        return

    roi = image[y0:y1, x0:x1]
    # Antialiased coverage for the glyphs alone, as an alpha channel.
    glyph = np.zeros(roi.shape[:2], dtype=np.uint8)
    cv2.putText(glyph, text, (org[0] - x0, org[1] - y0), font, scale,
                255, thickness, cv2.LINE_AA)
    kernel = cv2.getStructuringElement(cv2.MORPH_ELLIPSE,
                                       (2 * halo + 1, 2 * halo + 1))
    outline = cv2.dilate(glyph, kernel)

    out = roi.astype(np.float32)
    a = (outline.astype(np.float32) / 255.0)[:, :, None]
    out *= 1.0 - a                                   # black, so nothing to add
    a = (glyph.astype(np.float32) / 255.0)[:, :, None]
    out = out * (1.0 - a) + 255.0 * a
    image[y0:y1, x0:x1] = np.clip(out, 0, 255).astype(np.uint8)


def draw_label(image, geom: Geometry, text: str, patch_size: int = 224) -> None:
    """White type with a black outline, placed clear of the green box."""
    if not text:
        return
    font = cv2.FONT_HERSHEY_SIMPLEX
    scale = max(0.6, min(patch_size / 180.0, 2.0))
    thickness = max(1, int(scale * 2))
    height, width = image.shape[:2]

    # The outline sticks out past the glyphs, so it is what has to fit -- both
    # for the shrink-to-width test and for the clamp that keeps the text on the
    # crop. Recomputed as the scale shrinks; the halo shrinks with it, so the
    # available width only ever grows and the loop still terminates.
    (text_w, text_h), _baseline = cv2.getTextSize(text, font, scale, thickness)
    halo = _halo(text_h)
    max_width = max(8, width - 2 * (4 + halo))
    while text_w > max_width and scale > 0.4:
        scale *= 0.9
        thickness = max(1, int(scale * 2))
        (text_w, text_h), _baseline = cv2.getTextSize(text, font, scale, thickness)
        halo = _halo(text_h)
        max_width = max(8, width - 2 * (4 + halo))
    if text_w > max_width:
        while len(text) > 1:
            text = text[:-1]
            candidate = text + "..."
            (text_w, _), _ = cv2.getTextSize(candidate, font, scale, thickness)
            if text_w <= max_width:
                text = candidate
                break

    # The halo is added to the gap as well, so a thicker outline cannot come
    # down and touch the green box it is meant to sit clear of.
    padding = max(12, int(10 * scale)) + halo
    edge = 4 + halo
    centre = int(geom.rect_left + (geom.rect_right - geom.rect_left) / 2)
    x = max(edge, min(width - edge - text_w, centre - text_w // 2))
    if geom.rect_top - padding - text_h >= 0:
        y = geom.rect_top - padding
    elif geom.rect_bottom + padding + text_h <= height:
        y = geom.rect_bottom + padding + text_h
    else:
        y = max(text_h + edge, min(height - edge, geom.rect_top - padding))

    outlined_text(image, text, (x, y), font, scale, thickness, halo)


def render(image, row: int, column: int, patch_size: int, scale: float,
           label: str = ""):
    """The finished subject image for one point, and its geometry.

    Returns ``(None, geom)`` when the crop came out empty, which happens only
    for a point on a frame narrower than the patch itself.
    """
    height, width = image.shape[:2]
    geom = geometry(width, height, row, column, patch_size, scale)
    crop = image[geom.top:geom.bottom, geom.left:geom.right].copy()
    if crop.size == 0:
        return None, geom
    draw_patch_box(crop, geom)
    draw_center_marker(crop, geom.cx, geom.cy, patch_size)
    if label:
        draw_label(crop, geom, label, patch_size)
    return crop, geom
