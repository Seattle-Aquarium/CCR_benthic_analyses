"""
Our own classifier, run over the patches.

A thin wrapper around an Ultralytics YOLO classification checkpoint - the kind
``classification_model`` trains. It answers one question, in batches: given
these patch crops, what are the five most likely labels for each?

Ultralytics is imported lazily. It drags in torch, which costs several seconds
and a few hundred megabytes, and the person who only wants to cut patches and
upload them should not pay that at startup.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

from .logging_setup import get_logger

log = get_logger("classify")

#: How many suggestions CoralNet-Toolbox records per annotation.
TOP_N = 5


@dataclass(frozen=True)
class Suggestion:
    code: str
    confidence: float


def available() -> bool:
    """Whether ultralytics can be imported at all."""
    import importlib.util

    return importlib.util.find_spec("ultralytics") is not None


def resolve_device(requested: str) -> str:
    """Fall back to CPU when CUDA was asked for but is not there.

    Ultralytics raises on a missing device, which would end a run that could
    have finished slowly. A transect is a few thousand patches; on CPU that is
    minutes, not hours.
    """
    want = (requested or "cpu").strip().lower()
    if want in ("", "cpu"):
        return "cpu"
    try:
        import torch

        if torch.cuda.is_available():
            return requested
        log.warning(f"Device '{requested}' was asked for but no CUDA device is "
                    "visible - running on the CPU instead.")
    except Exception:
        log.warning("torch could not report a CUDA device - running on the CPU.")
    return "cpu"


class Classifier:
    """A loaded checkpoint, ready to score batches of patches."""

    def __init__(self, weights: str | Path, device: str = "cpu",
                 imgsz: int = 256, batch: int = 32):
        self.weights = Path(weights)
        if not self.weights.is_file():
            raise FileNotFoundError(f"Model weights not found: {self.weights}")
        self.device = resolve_device(device)
        self.imgsz = int(imgsz)
        self.batch = max(1, int(batch))

        from ultralytics import YOLO

        log.info(f"Loading model: {self.weights}")
        self._model = YOLO(str(self.weights))
        names = self._model.names
        self.names: dict[int, str] = (
            dict(names) if isinstance(names, dict)
            else {i: n for i, n in enumerate(names)}
        )
        log.info(f"Model knows {len(self.names)} class(es); running on "
                 f"{self.device} at {self.imgsz} px.")

    @property
    def class_codes(self) -> list[str]:
        return [self.names[k] for k in sorted(self.names)]

    def describe(self) -> str:
        return (f"{self.weights.name} - {len(self.names)} classes, "
                f"{self.device}, {self.imgsz} px")

    # ------------------------------------------------------------------

    def predict(self, images: list) -> list[list[Suggestion]]:
        """Top-N suggestions for each image, most likely first.

        `images` are BGR arrays as OpenCV produces them, which is what
        Ultralytics expects from an in-memory source.
        """
        if not images:
            return []
        out: list[list[Suggestion]] = []
        for start in range(0, len(images), self.batch):
            chunk = images[start:start + self.batch]
            results = self._model.predict(
                chunk, imgsz=self.imgsz, device=self.device, verbose=False
            )
            for res in results:
                out.append(self._suggestions(res))
        return out

    def _suggestions(self, result) -> list[Suggestion]:
        probs = getattr(result, "probs", None)
        if probs is None:
            return []
        # top5/top5conf are tensors; tolist() keeps this free of a torch import.
        try:
            idxs = list(probs.top5)
            confs = list(probs.top5conf.tolist())
        except Exception:
            return []
        return [
            Suggestion(self.names.get(int(i), str(i)), round(float(c), 4))
            for i, c in zip(idxs[:TOP_N], confs[:TOP_N], strict=True)
        ]
