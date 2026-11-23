"""
Which survey photo did a patch come from?

The byte-identical audit in ``hashing`` answers "is this exact patch in the
training set". It cannot answer the question that matters more for a held-out
set: "was this patch cut from a photo the model trained on". Two annotation
points a few hundred pixels apart on one seafloor image are near-duplicates
-- same lighting, same substrate, same turbidity, often the same organisms --
and a model trained on one will score suspiciously well on the other while
the hash check reports everything clean.

Stage 1 names every patch ``<photo stem>_r<row>_c<col>.jpg``, so the photo is
recoverable from the filename alone, with no CSV in hand. That is what this
module reads. A held-out set is independent when its *photos* are disjoint
from the training set's photos; that is the test applied when patches are
added to one, and re-checked before any number is reported.
"""

from __future__ import annotations

import re
from collections import defaultdict
from pathlib import Path

from .fsutil import IMG_EXTS

#: ``<stem>_r<row>_c<col>`` with the optional suffixes the pipeline itself
#: appends: ``_augN`` from stage 2's augmentation, ``__N`` from a filename
#: collision. Anything else in the name belongs to the photo.
_PATCH_NAME = re.compile(r"^(?P<stem>.+)_r\d+_c\d+(?:_aug\d+|__\d+)?$", re.I)


def photo_of(patch_name: str) -> str | None:
    """The source photo's stem, or None if the name is not a pipeline patch."""
    m = _PATCH_NAME.match(Path(patch_name).stem)
    return m.group("stem") if m else None


def photos_in(folder: str | Path, *, splits: tuple[str, ...] | None = None
              ) -> dict[str, list[Path]]:
    """Every photo that contributed a patch under *folder*, and which patches.

    With *splits* given, only those subfolders are read (a training set's
    ``train`` and ``val``); without, the folder is read flat (a held-out set).
    Folders whose name starts with an underscore -- quarantine, exclusions --
    are skipped either way: what has been set aside did not contribute.
    """
    root = Path(folder)
    roots = [root / s for s in splits] if splits else [root]
    out: dict[str, list[Path]] = defaultdict(list)
    for base in roots:
        if not base.is_dir():
            continue
        for p in base.rglob("*"):
            if not p.is_file() or p.suffix.lower() not in IMG_EXTS:
                continue
            if any(part.startswith("_") for part in p.relative_to(base).parts[:-1]):
                continue
            stem = photo_of(p.name)
            if stem:
                out[stem].append(p)
    return dict(out)


def training_photos(dataset_dir: str | Path) -> set[str]:
    """The photos a training set was cut from -- train/ and val/ together.

    val/ counts as training for this purpose: the model never updates on it,
    but it selected the epoch, and it shares every photo with train/ anyway.
    """
    return set(photos_in(dataset_dir, splits=("train", "val")))


def shared_photos(holdout_dir: str | Path, dataset_dir: str | Path
                  ) -> tuple[set[str], list[Path]]:
    """Photos in both, and the held-out patches that came from them."""
    hold = photos_in(holdout_dir)
    shared = set(hold) & training_photos(dataset_dir)
    affected = sorted(p for s in shared for p in hold[s])
    return shared, affected
