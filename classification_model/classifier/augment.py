"""
Augmentations Ultralytics' classification path does not offer.

Ultralytics builds its classification transforms from torchvision and reads
only ``hsv_*``, ``fliplr``, ``flipud``, ``scale``, ``erasing`` and
``auto_augment``. Its rotation, translation and mixing arguments belong to the
detection augmenter and are silently ignored here. So anything beyond colour
and flips has to be added from outside -- by subclassing the trainer and
editing the transform chain it builds.

Everything in this module keeps the centre of the patch where it is, because
the label describes what is there:

**Rot90** turns the image by a random multiple of 90 degrees. Exact -- a pixel
permutation, no interpolation, no padded corners -- and with the two flips it
gives all eight orientations a seafloor patch can legitimately take. The
seafloor has no "up"; the model should not learn one.

**CenterZoom** crops a central region and resizes it back, i.e. zooms in on
the annotated point by up to a set fraction. ROV altitude varies between
frames and transects, so the same taxon is seen at slightly different scales;
this shows the model that variation without ever moving the point off centre
or bringing in anything from outside the frame. It is what Ultralytics'
random-resized crop would be if it were pinned to the centre.

**Blur** applies a mild Gaussian blur some of the time. Turbidity and focus
vary shot to shot underwater; a model that needs every edge crisp is
memorising the good frames.
"""

from __future__ import annotations

import random

from PIL import Image

#: Which keys a preset may set here, with what a zero means.
KNOWN = {
    "rot90": "random 0/90/180/270 rotation (True/False)",
    "center_zoom": "max zoom-in fraction, centre pinned (0 = off)",
    "blur": "probability of a mild Gaussian blur (0 = off)",
}


class Rot90:
    """Rotate by 0, 90, 180 or 270 degrees, chosen uniformly."""

    _OPS = (None, Image.Transpose.ROTATE_90, Image.Transpose.ROTATE_180,
            Image.Transpose.ROTATE_270)

    def __call__(self, im: Image.Image) -> Image.Image:
        op = random.choice(self._OPS)
        return im if op is None else im.transpose(op)

    def __repr__(self) -> str:
        return "Rot90(k in {0,1,2,3})"


class CenterZoom:
    """Zoom in on the centre by a random factor in [1, 1 + max_zoom]."""

    def __init__(self, max_zoom: float, p: float = 0.5):
        self.max_zoom = float(max_zoom)
        self.p = float(p)

    def __call__(self, im: Image.Image) -> Image.Image:
        if self.max_zoom <= 0 or random.random() >= self.p:
            return im
        z = random.uniform(1.0, 1.0 + self.max_zoom)
        w, h = im.size
        cw, ch = max(1, round(w / z)), max(1, round(h / z))
        left, top = (w - cw) // 2, (h - ch) // 2
        return im.crop((left, top, left + cw, top + ch)).resize(
            (w, h), Image.Resampling.BILINEAR)

    def __repr__(self) -> str:
        return f"CenterZoom(max_zoom={self.max_zoom}, p={self.p})"


def build(cfg: dict) -> tuple[list, list]:
    """The transforms a preset asks for, split by where they belong.

    Returns ``(before_resize, before_tensor)``. Geometry goes first so the
    single resize Ultralytics already does is the only interpolation; blur
    goes after the colour jitter, on the resized image, so its sigma means
    the same thing whatever the source patch size was.
    """
    import torchvision.transforms as T

    cfg = cfg or {}
    early, late = [], []
    if cfg.get("rot90"):
        early.append(Rot90())
    if cfg.get("center_zoom", 0):
        early.append(CenterZoom(cfg["center_zoom"]))
    if cfg.get("blur", 0):
        late.append(T.RandomApply(
            [T.GaussianBlur(kernel_size=5, sigma=(0.1, 1.5))], p=float(cfg["blur"])))
    return early, late


def install(dataset, cfg: dict) -> int:
    """Splice the preset's extras into a ClassificationDataset's chain.

    Ultralytics' chain is ``[RandomResizedCrop, flips..., ColorJitter,
    ToTensor, Normalize, RandomErasing]``. Extras that change geometry go in
    front; extras that need the resized image go just before ``ToTensor``.
    Returns how many transforms were added.
    """
    import torchvision.transforms as T

    early, late = build(cfg)
    if not early and not late:
        return 0
    chain = list(dataset.torch_transforms.transforms)
    tensor_at = next((i for i, t in enumerate(chain) if isinstance(t, T.ToTensor)),
                     len(chain))
    chain = early + chain[:tensor_at] + late + chain[tensor_at:]
    dataset.torch_transforms = T.Compose(chain)
    return len(early) + len(late)


def make_trainer(cfg: dict):
    """A ClassificationTrainer whose training dataset carries the extras.

    Only the training split is touched: validation must see the image as the
    model will see it in the field. Built as a subclass rather than a patch so
    Ultralytics' own trainer is untouched for every other caller.
    """
    from ultralytics.models.yolo.classify.train import ClassificationTrainer

    extras = dict(cfg or {})

    class CentredAugmentationTrainer(ClassificationTrainer):
        def build_dataset(self, img_path, mode="train", batch=None):
            ds = super().build_dataset(img_path, mode, batch)
            if mode == "train":
                added = install(ds, extras)
                if added:
                    from ultralytics.utils import LOGGER
                    LOGGER.info(f"Added {added} centre-preserving transform(s) "
                                f"to the training chain: {describe(extras)}")
            return ds

    return CentredAugmentationTrainer


def describe(cfg: dict) -> str:
    """One line, for the log and run_notes.md."""
    parts = []
    if cfg.get("rot90"):
        parts.append("rot90")
    if cfg.get("center_zoom", 0):
        parts.append(f"center_zoom={cfg['center_zoom']}")
    if cfg.get("blur", 0):
        parts.append(f"blur={cfg['blur']}")
    return ", ".join(parts) or "none"
