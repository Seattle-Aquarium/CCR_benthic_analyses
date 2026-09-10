"""
Patch cropping and augmentation.

Both operations share one assumption worth stating once, because every other
choice follows from it: **a patch is centred on its annotated point.** The
class label describes what is at the centre pixel, not what fills the frame. So
cropping is centred, augmentation pivots on the centre, and nothing here ever
translates the image off-centre.
"""

from __future__ import annotations

import random

import cv2
import numpy as np

#: A crop clipped by the image edge is kept only if it retains at least this
#: fraction of the requested size in both dimensions. Below that there is too
#: little context around the annotated point for the label to mean much.
MIN_EDGE_FRACTION = 0.5


def crop_patch(img: np.ndarray, row: int, col: int, size: int) -> np.ndarray | None:
    """``size`` x ``size`` centred on ``(row, col)``, clipped at image edges.

    Returns None when the point is so close to an edge that the surviving crop
    is under ``MIN_EDGE_FRACTION`` of the requested size in either dimension.
    """
    half = size // 2
    h, w = img.shape[:2]

    top = max(row - half, 0)
    bottom = min(row + half, h)
    left = max(col - half, 0)
    right = min(col + half, w)

    patch = img[top:bottom, left:right]
    if patch.size == 0:
        return None
    if (patch.shape[0] < size * MIN_EDGE_FRACTION
            or patch.shape[1] < size * MIN_EDGE_FRACTION):
        return None
    return patch.copy()


def augment_image(img: np.ndarray, rng: random.Random) -> np.ndarray:
    """One randomly augmented copy of *img*.

    Never returns the input unmodified: rotation, scale and colour jitter are
    always applied, so an augmented patch is never a byte-identical duplicate
    of its source. That matters -- ``hashing.py`` would otherwise flag the
    floor-augmentation output as duplication.

    Rotation and zoom go through a single ``warpAffine`` pivoted on the image
    centre, keeping the annotated point at the centre where the label applies.
    """
    out = img.copy()

    # Benthic patches have no canonical "up", so both flips are fair game.
    if rng.random() < 0.5:
        out = cv2.flip(out, 1)
    if rng.random() < 0.5:
        out = cv2.flip(out, 0)

    angle = rng.uniform(-15, 15)
    scale = rng.uniform(0.8, 1.2)
    h, w = out.shape[:2]
    m = cv2.getRotationMatrix2D((w / 2, h / 2), angle, scale)
    out = cv2.warpAffine(out, m, (w, h), borderMode=cv2.BORDER_REFLECT101)

    # Underwater imagery carries a lot of natural colour cast from depth,
    # turbidity and camera white balance, so hue/saturation jitter is realistic
    # variation rather than noise.
    hsv = cv2.cvtColor(out, cv2.COLOR_BGR2HSV).astype(np.int16)
    hsv[..., 0] = (hsv[..., 0] + rng.uniform(-10, 10)) % 180
    hsv[..., 1] = np.clip(hsv[..., 1] * rng.uniform(0.7, 1.3), 0, 255)
    out = cv2.cvtColor(hsv.astype(np.uint8), cv2.COLOR_HSV2BGR)

    return cv2.convertScaleAbs(out, alpha=rng.uniform(0.85, 1.15),
                               beta=rng.uniform(-15, 15))


def imread(path: str) -> np.ndarray | None:
    """``cv2.imread`` that tolerates non-ASCII paths.

    OpenCV's own reader goes through the C locale on Windows and returns None
    for any path with a non-ANSI character in it -- which includes several real
    site names in this project.
    """
    img = cv2.imread(path)
    if img is not None:
        return img
    try:
        data = np.fromfile(path, dtype=np.uint8)
        return cv2.imdecode(data, cv2.IMREAD_COLOR) if data.size else None
    except OSError:
        return None


def imwrite(path: str, img: np.ndarray, quality: int = 95) -> bool:
    """``cv2.imwrite`` with the same non-ASCII path tolerance as ``imread``."""
    params = [cv2.IMWRITE_JPEG_QUALITY, int(quality)]
    if cv2.imwrite(path, img, params):
        return True
    try:
        ok, buf = cv2.imencode(".jpg", img, params)
        if not ok:
            return False
        buf.tofile(path)
        return True
    except OSError:
        return False
