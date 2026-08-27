"""
Byte-identical duplicate detection.

Why this module exists, stated plainly: a held-out evaluation set is only
meaningful if the model has never seen its images. In an earlier round, 1,929
of 15,380 held-out patches (12.5%) turned out to be byte-identical to training
images, and the reported accuracy was inflated from a true 62.81% to an
apparent 65.85%. Nothing in the pipeline noticed, because the merge step
compared *filenames* -- and the same patch extracted twice under two different
annotation rows gets two different names.

So duplicates are detected by content, and the check is not an optional tool
you have to remember to run: the balance stage refuses to write a dataset that
leaks into the holdout, and the evaluate stage re-verifies independence before
it will report a number.

Scope, and the limit of it: this finds *byte-identical* files only. It does not
find near-duplicates, and that is the correct behaviour in both directions --
augmented copies are deliberately not identical and must not be flagged, and
two annotation points a few pixels apart are genuinely different samples even
though they look alike to a human.
"""

from __future__ import annotations

import os
from collections import defaultdict
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass
from pathlib import Path

from .fsutil import list_class_images
from .progress import CancelledError, ProgressCB, is_cancelled

#: blake2b is faster than md5 on modern CPUs and has no collision caveats worth
#: reasoning about. The digest is only ever compared to other digests from this
#: same run, so the choice of algorithm is free.
_DIGEST_BYTES = 16
_CHUNK = 1 << 20  # 1 MiB


def hash_file(path: str | Path) -> str | None:
    """Content digest of one file, or None if it could not be read."""
    import hashlib

    h = hashlib.blake2b(digest_size=_DIGEST_BYTES)
    try:
        with open(path, "rb") as fh:
            while chunk := fh.read(_CHUNK):
                h.update(chunk)
    except OSError:
        return None
    return h.hexdigest()


@dataclass(frozen=True)
class Occurrence:
    """One file, and where in the pipeline it sits."""

    group: str      # "train" | "val" | "holdout"
    label: str      # class folder name
    path: str

    def describe(self) -> str:
        return f"{self.group}/{self.label}/{os.path.basename(self.path)}"


@dataclass
class DuplicateGroup:
    """A set of files that are byte-identical to each other."""

    digest: str
    occurrences: list[Occurrence]

    @property
    def category(self) -> str:
        """What kind of problem this duplicate group represents.

        Ordered by severity, because a group can be several things at once and
        the worst one is what should be reported:

        ``holdout_leak``    also present in train or val -- invalidates the
                            evaluation's independence. Critical.
        ``train_val_leak``  present in both train and val -- the val split is
                            no longer measuring generalisation. Serious.
        ``within_train`` / ``within_val`` / ``within_holdout``
                            redundant copies inside one split. Mild: it skews
                            class balance but breaks no guarantee.
        """
        groups = {o.group for o in self.occurrences}
        if "holdout" in groups and (groups & {"train", "val"}):
            return "holdout_leak"
        if {"train", "val"} <= groups:
            return "train_val_leak"
        if groups == {"holdout"}:
            return "within_holdout"
        if groups == {"val"}:
            return "within_val"
        return "within_train"

    @property
    def labels(self) -> set[str]:
        return {o.label for o in self.occurrences}

    @property
    def label_conflict(self) -> bool:
        """True when identical images are filed under different class labels.

        Always worth surfacing: the same image cannot be two classes, so one of
        the annotations is wrong.
        """
        return len(self.labels) > 1


#: Categories that make a dataset unfit for its purpose, as opposed to merely
#: untidy. The balance and evaluate stages block on these.
BLOCKING = {"holdout_leak", "train_val_leak"}


def collect(dataset_dir: str | Path | None = None,
            holdout_dir: str | Path | None = None) -> list[Occurrence]:
    """Enumerate every patch to be hashed, tagged with where it came from."""
    out: list[Occurrence] = []
    if dataset_dir:
        for split in ("train", "val"):
            for label, files in list_class_images(dataset_dir, split).items():
                out.extend(Occurrence(split, label, f) for f in files)
    if holdout_dir:
        for label, files in list_class_images(holdout_dir, None).items():
            out.extend(Occurrence("holdout", label, f) for f in files)
    return out


def index(occurrences: list[Occurrence], *, progress: ProgressCB | None = None,
          cancel=None, workers: int | None = None) -> dict[str, list[Occurrence]]:
    """Hash every occurrence and group them by digest.

    Hashing is I/O bound, so it is threaded -- on a 15k-patch dataset this is
    the difference between a few seconds and the better part of a minute, and
    it runs on every balance and every evaluation now.
    """
    total = len(occurrences)
    if not total:
        return {}
    workers = workers or min(16, (os.cpu_count() or 4) * 2)

    by_digest: dict[str, list[Occurrence]] = defaultdict(list)
    done = 0
    with ThreadPoolExecutor(max_workers=workers) as pool:
        for occ, digest in zip(occurrences,
                               pool.map(lambda o: hash_file(o.path), occurrences)):
            done += 1
            if digest is not None:
                by_digest[digest].append(occ)
            if progress and (done % 512 == 0 or done == total):
                progress(done / total, f"hashing patches… {done:,}/{total:,}")
            if done % 512 == 0 and is_cancelled(cancel):
                raise CancelledError("cancelled during hashing")
    return dict(by_digest)


def duplicate_groups(by_digest: dict[str, list[Occurrence]]) -> list[DuplicateGroup]:
    """Just the digests seen more than once, worst category first."""
    groups = [DuplicateGroup(d, occ) for d, occ in by_digest.items() if len(occ) > 1]
    order = {"holdout_leak": 0, "train_val_leak": 1, "within_train": 2,
             "within_val": 3, "within_holdout": 4}
    groups.sort(key=lambda g: (order.get(g.category, 9), -len(g.occurrences)))
    return groups


@dataclass
class DuplicateReport:
    groups: list[DuplicateGroup]
    total_files: int = 0

    def by_category(self) -> dict[str, list[DuplicateGroup]]:
        out: dict[str, list[DuplicateGroup]] = defaultdict(list)
        for g in self.groups:
            out[g.category].append(g)
        return dict(out)

    def counts(self) -> dict[str, int]:
        return {k: len(v) for k, v in self.by_category().items()}

    def redundant_files(self, category: str) -> int:
        """How many *files* could be removed from that category (n-1 per group)."""
        return sum(len(g.occurrences) - 1
                   for g in self.groups if g.category == category)

    @property
    def blocking(self) -> list[DuplicateGroup]:
        return [g for g in self.groups if g.category in BLOCKING]

    @property
    def conflicts(self) -> list[DuplicateGroup]:
        return [g for g in self.groups if g.label_conflict]

    def summary_lines(self) -> list[str]:
        if not self.groups:
            return [f"No byte-identical duplicates among {self.total_files:,} patches."]
        lines = [f"Byte-identical duplicate groups among {self.total_files:,} patches:"]
        labels = {
            "holdout_leak": "holdout leak (CRITICAL - held-out image also in training)",
            "train_val_leak": "train/val leak (val no longer independent)",
            "within_train": "duplicated inside train (mild - skews balance)",
            "within_val": "duplicated inside val (mild)",
            "within_holdout": "duplicated inside holdout (mild)",
        }
        for cat, n in sorted(self.counts().items(),
                             key=lambda kv: list(labels).index(kv[0])
                             if kv[0] in labels else 9):
            lines.append(f"  {n:>6,} {labels.get(cat, cat)}"
                         f"  ({self.redundant_files(cat):,} redundant file(s))")
        if self.conflicts:
            lines.append(f"  {len(self.conflicts):>6,} of these are ALSO filed under "
                         f"conflicting class labels")
        return lines


def audit(dataset_dir: str | Path | None = None,
          holdout_dir: str | Path | None = None, *,
          progress: ProgressCB | None = None, cancel=None) -> DuplicateReport:
    """Hash a dataset (and optionally a holdout set) and categorise duplicates."""
    occurrences = collect(dataset_dir, holdout_dir)
    by_digest = index(occurrences, progress=progress, cancel=cancel)
    return DuplicateReport(duplicate_groups(by_digest), total_files=len(occurrences))


def write_report_csv(report: DuplicateReport, path: str | Path) -> str:
    """Every duplicate group, one row each, for review outside the app."""
    import csv

    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(path, "w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh)
        w.writerow(["digest", "category", "copies", "label_conflict",
                    "labels", "locations"])
        for g in report.groups:
            w.writerow([
                g.digest, g.category, len(g.occurrences),
                "yes" if g.label_conflict else "",
                "|".join(sorted(g.labels)),
                " | ".join(o.describe() for o in g.occurrences),
            ])
    return str(path)


@dataclass
class DropPlan:
    """Which copy of each duplicate group survives, and which are discarded.

    Kept as a value object rather than a bare set because the *counts* matter
    to the operator: "3,100 duplicates removed" is not actionable, whereas
    "2,900 redundant copies inside train, 200 val images that were also in
    train" says what was wrong with the inputs.
    """

    drops: set[str]
    #: category -> how many files that category contributed to ``drops``
    by_category: dict[str, int]
    #: label conflicts settled by the nominated dataset, and by fallback order
    resolved_by_authority: int = 0
    resolved_by_fallback: int = 0

    def __len__(self) -> int:
        return len(self.drops)

    def __contains__(self, path: object) -> bool:
        return path in self.drops


def choose_drops(groups: list[DuplicateGroup], *,
                 owner: dict[str, str] | None = None,
                 authority: str | None = None) -> DropPlan:
    """Pick which copy of each duplicate group to keep; return the rest.

    Exactly one copy of every group survives, so the merged dataset holds no
    byte-identical pairs anywhere -- not inside train, not inside val, and not
    across the two.

    Which copy survives is decided in this order:

    1. **Never the held-out copy, if a training copy exists.** A held-out leak
       is resolved by discarding it from the *evaluation* set. Dropping the
       training copy instead would shrink the training data in a way that
       flatters the metric -- the opposite of what the check is for.
    2. **The dataset nominated as the label authority**, when the group carries
       conflicting labels and *authority* names one of the input datasets.
       Whichever copy is kept takes its own dataset's label with it, so this is
       what decides the label of a contested patch.
    3. **train over val**, because a val image the model trained on measures
       nothing.
    4. **First by path**, so repeat runs on the same inputs agree.

    Nothing here touches the input datasets -- the "drops" are simply files the
    merge does not copy to the output. The only file this pipeline ever moves
    is a leaked held-out patch, and only when quarantine is enabled.
    """
    rank = {"train": 0, "val": 1, "holdout": 2}
    order = lambda o: (rank[o.group], o.path)          # noqa: E731

    drops: set[str] = set()
    per_category: dict[str, int] = defaultdict(int)
    by_authority = by_fallback = 0

    for g in groups:
        keep = None
        if g.label_conflict and authority and owner:
            # A held-out copy can never be the keeper while a training copy
            # exists, so authority is only consulted among the dataset copies.
            candidates = [o for o in g.occurrences
                          if o.group != "holdout" and owner.get(o.path) == authority]
            if candidates:
                keep = min(candidates, key=order)
                by_authority += 1
        if keep is None:
            keep = min(g.occurrences, key=order)
            if g.label_conflict:
                by_fallback += 1

        for o in g.occurrences:
            if o.path != keep.path:
                drops.add(o.path)
                per_category[g.category] += 1

    return DropPlan(drops, dict(per_category), by_authority, by_fallback)
