"""
Filesystem helpers shared by every stage.

These were previously copy-pasted across seven scripts, which is how the two
copies of ``augment_image`` and three copies of ``list_class_images`` came to
drift apart. One definition each, here.
"""

from __future__ import annotations

import getpass
import os
import re
from pathlib import Path

#: Extensions treated as patch images. Lowercase; compare against
#: ``Path.suffix.lower()``.
IMG_EXTS = {".jpg", ".jpeg", ".png", ".bmp", ".tif", ".tiff"}


# --------------------------------------------------------------------------
#  Source-image path resolution
# --------------------------------------------------------------------------

_USER_PATH_RE = re.compile(r"^([A-Za-z]:[\\/]Users[\\/])[^\\/]+([\\/].*)$")


def localize_path(path: str) -> str:
    """Point a Windows user-profile path at *this* user's profile.

    Annotation CSVs are exported by whoever ran CoralNet-Toolbox, so their
    ``Path`` column is full of ``C:\\Users\\someone-else\\...``. The shared
    Dropbox tree beneath ``Users/<name>/`` is identical across the team, so
    only the username segment needs rewriting.
    """
    match = _USER_PATH_RE.match(path)
    if not match:
        return path
    return f"{match.group(1)}{getpass.getuser()}{match.group(2)}"


def remap_path(path: str, remaps: list[tuple[str, str]]) -> str:
    """Apply ordered ``(old, new)`` substring replacements to a path.

    For folders renamed or reorganised since the annotation CSV was exported.
    Applied *after* ``localize_path``. Slashes are normalised to ``/`` first, so
    it does not matter which direction the CSV or the remap happens to use --
    Windows accepts either in the result.
    """
    path = path.replace("\\", "/")
    for old, new in remaps:
        old = old.replace("\\", "/")
        new = new.replace("\\", "/")
        if old in path:
            path = path.replace(old, new)
    return path


def resolve_source(path: str, remaps: list[tuple[str, str]]) -> str:
    """localize + remap, in the one order that is ever correct."""
    return remap_path(localize_path(str(path)), remaps)


def parse_remap(text: str) -> tuple[str, str] | None:
    """Parse one ``OLD=>NEW`` remap string. Returns None if it is not one."""
    if "=>" not in text:
        return None
    old, _, new = text.partition("=>")
    old, new = old.strip(), new.strip()
    return (old, new) if old else None


def parse_remaps(lines) -> list[tuple[str, str]]:
    """Parse many remap strings, skipping blanks and malformed entries."""
    out = []
    for line in lines:
        line = str(line).strip()
        if not line:
            continue
        pair = parse_remap(line)
        if pair:
            out.append(pair)
    return out


# --------------------------------------------------------------------------
#  Naming
# --------------------------------------------------------------------------

_UNSAFE_RE = re.compile(r'[<>:"/\\|?*]')


def sanitize_label(label: str) -> str:
    """Make a class label safe to use as a folder name."""
    return _UNSAFE_RE.sub("_", str(label).strip())


def unique_dest(dest_dir: str | Path, filename: str) -> str:
    """A path inside *dest_dir* that does not already exist.

    Appends ``__2``, ``__3``, ... on collision. Note that a collision here means
    two patches share a filename, which is worth being suspicious about -- see
    ``hashing.py``, which catches the case where they also share *content*.
    """
    dest_dir = str(dest_dir)
    base, ext = os.path.splitext(filename)
    candidate = filename
    n = 2
    while os.path.exists(os.path.join(dest_dir, candidate)):
        candidate = f"{base}__{n}{ext}"
        n += 1
    return os.path.join(dest_dir, candidate)


# --------------------------------------------------------------------------
#  Dataset inventory
# --------------------------------------------------------------------------

def list_class_images(root: str | Path, split: str | None = None) -> dict[str, list[str]]:
    """``{class_name: [image_path, ...]}`` for one split, or for a flat folder.

    Pass ``split=None`` for a flat ``<root>/<Label>/*.jpg`` layout -- which is
    what the held-out evaluation set uses, deliberately having no train/val
    split at all.
    """
    base = Path(root) / split if split else Path(root)
    result: dict[str, list[str]] = {}
    if not base.is_dir():
        return result
    for class_dir in sorted(base.iterdir()):
        if not class_dir.is_dir() or class_dir.name.startswith("_"):
            continue
        files = [str(f) for f in sorted(class_dir.iterdir())
                 if f.suffix.lower() in IMG_EXTS]
        if files:
            result[class_dir.name] = files
    return result


def list_class_dirs(root: str | Path, split: str | None = None) -> set[str]:
    """Class folder names under a split, *including empty ones*.

    An emptied-out class still needs to show up as a structural mismatch, which
    is why this cannot just be ``set(list_class_images(...))``.
    """
    base = Path(root) / split if split else Path(root)
    if not base.is_dir():
        return set()
    return {d.name for d in base.iterdir()
            if d.is_dir() and not d.name.startswith("_")}


def merge_file_lists(*dicts: dict[str, list[str]]) -> dict[str, list[str]]:
    """Merge several ``{class: [files]}`` dicts, concatenating the lists."""
    merged: dict[str, list[str]] = {}
    for d in dicts:
        for cls, files in d.items():
            merged.setdefault(cls, []).extend(files)
    return merged


def looks_like_split_dataset(root: str | Path) -> bool:
    """True when *root* has the ``train/`` + ``val/`` layout training needs."""
    root = Path(root)
    return (root / "train").is_dir() and (root / "val").is_dir()


def inventory_table(train: dict[str, list], val: dict[str, list] | None = None) -> list[str]:
    """Render a per-class count table as lines, for the log pane."""
    val = val or {}
    classes = sorted(set(train) | set(val))
    width = max([15] + [len(c) for c in classes]) if classes else 15

    lines = ["=" * (width + 20)]
    lines.append(f"{'Class':<{width}} {'train':>8} {'val':>8}")
    lines.append("-" * (width + 20))
    for cls in classes:
        lines.append(f"{cls:<{width}} {len(train.get(cls, [])):>8} "
                     f"{len(val.get(cls, [])):>8}")
    lines.append("-" * (width + 20))
    total_train = sum(len(v) for v in train.values())
    total_val = sum(len(v) for v in val.values())
    lines.append(f"{'TOTAL':<{width}} {total_train:>8} {total_val:>8}"
                 f"  ({len(classes)} classes)")
    lines.append("=" * (width + 20))

    counts = sorted(len(v) for v in train.values())
    if counts:
        lines.append(f"train class counts - min={counts[0]}, "
                     f"median={counts[len(counts) // 2]}, max={counts[-1]}")
    return lines
