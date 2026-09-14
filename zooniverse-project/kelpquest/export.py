"""
Getting the volunteers' classifications back out.

Zooniverse builds a classifications export on its own servers and then hands
over a CSV. Two things about that shape this module:

* **Generating one takes minutes, not seconds.** Panoptes queues the job, so
  the call has to wait rather than assume. Asking without waiting hands back
  whatever export was generated last, which on a set that has had a fresh batch
  of classifications is quietly the wrong file.
* **A subject set that nobody has classified yet has no export at all.** That
  is not an error to retry; it is worth saying plainly.

Follows ``scripts/export_subjectset.py``, which does the same call.
"""

from __future__ import annotations

import threading
from collections.abc import Callable
from dataclasses import dataclass, field
from pathlib import Path

from .logging_setup import get_logger
from .zooniverse import connect

log = get_logger("export")

#: How long to wait for Zooniverse to build the export. A transect's worth of
#: classifications is usually a couple of minutes; a whole project can be much
#: longer, which is why this is generous and cancellable rather than short.
WAIT_TIMEOUT_S = 1800


@dataclass
class ExportResult:
    subject_set_ids: list[str] = field(default_factory=list)
    #: id -> display name, for whatever was reachable.
    subject_set_names: dict[str, str] = field(default_factory=dict)
    per_set_csv: list[Path] = field(default_factory=list)
    combined_csv: Path | None = None
    rows: int = 0
    duplicates_dropped: int = 0
    bytes_written: int = 0
    dry_run: bool = False
    cancelled: bool = False
    lines: list[str] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)
    errors: list[str] = field(default_factory=list)

    @property
    def ok(self) -> bool:
        return not self.errors and not self.cancelled

    def summary(self) -> str:
        head = ("Checked only - no export was generated." if self.dry_run
                else f"Exported {self.rows:,} classification row(s) from "
                     f"{len(self.per_set_csv)} subject set(s)")
        out = [head]
        out += self.lines
        if self.duplicates_dropped:
            out.append(f"{self.duplicates_dropped:,} duplicate "
                       "classification(s) dropped where sets overlapped.")
        # The files it wrote are reported by the stage, which owns `outputs`.
        # Warnings and errors are rendered by the stage, which owns
        # them; repeating them here printed each one twice.
        return "\n".join(out)


def default_csv_name(subject_set_id: str, subject_set_name: str = "") -> str:
    from datetime import date

    stem = (subject_set_name or f"subjectset_{subject_set_id}").strip()
    safe = "".join(c if (c.isalnum() or c in "-_") else "_" for c in stem)
    return f"{date.today():%Y_%m_%d}_{safe}_classifications.csv"


def parse_ids(text: str) -> list[str]:
    """Subject set IDs out of whatever the operator typed.

    Commas, spaces and newlines all separate, because the ids get pasted from
    a spreadsheet column as often as they get typed. Order is kept and
    duplicates dropped.
    """
    seen: list[str] = []
    for chunk in str(text or "").replace(",", " ").replace(";", " ").split():
        digits = chunk.strip()
        if digits.isdigit() and digits not in seen:
            seen.append(digits)
    return seen


def list_project_sets() -> list[tuple[str, str, int]]:
    """(id, name, subject count) for every subject set in the project.

    A subject moves between sets as it retires -- yes/no, then multiple
    choice, then an expert set -- so the sets that hold one transect's
    subjects are not knowable from the upload log alone. Listing them is a
    few dozen rows and lets the operator pick, which beats asking them to
    remember ids.
    """
    from panoptes_client import Project, SubjectSet

    project_id = connect()
    project = Project.find(project_id)
    out = []
    for subject_set in SubjectSet.where(project_id=project.id):
        count = getattr(subject_set, "set_member_subjects_count", None) or 0
        out.append((str(subject_set.id), str(subject_set.display_name),
                    int(count)))
    out.sort(key=lambda row: int(row[0]))
    log.info(f"Project {project_id} has {len(out)} subject set(s).")
    return out


def combine(csv_paths: list[Path], target: Path) -> tuple[int, int]:
    """Concatenate exports into one CSV, deduped on classification_id.

    Overlap is expected rather than exceptional: a subject that moved from the
    yes/no set to the multiple-choice set is a member of both, so its
    classifications come back in both exports. Counting them twice would
    inflate the vote totals the thresholds are measured against.
    """
    import pandas as pd

    frames = []
    for path in csv_paths:
        try:
            frames.append(pd.read_csv(path, low_memory=False))
        except (OSError, ValueError) as exc:
            log.warning(f"Could not read {Path(path).name}: {exc}")
    if not frames:
        raise ValueError("None of the exports could be read.")
    joined = pd.concat(frames, ignore_index=True)
    before = len(joined)
    if "classification_id" in joined.columns:
        joined = joined.drop_duplicates(subset="classification_id")
    target.parent.mkdir(parents=True, exist_ok=True)
    joined.to_csv(target, index=False)
    log.info(f"Combined {len(csv_paths)} export(s) into {target.name}: "
             f"{len(joined):,} classification(s), {before - len(joined):,} "
             "duplicate(s) dropped.")
    return len(joined), before - len(joined)


def inspect(subject_set_id: str) -> tuple[str, int]:
    """(display name, subject count) for a subject set. Used by the check."""
    from panoptes_client import SubjectSet

    connect()
    subject_set = SubjectSet.find(str(subject_set_id))
    count = getattr(subject_set, "set_member_subjects_count", None)
    if count is None:
        count = 0
    return str(subject_set.display_name), int(count)


def _fetch_one(subject_set_id: str, target: Path,
               result: ExportResult) -> bytes | None:
    """Generate and download one subject set's classifications export."""
    from panoptes_client import SubjectSet

    subject_set = SubjectSet.find(str(subject_set_id))
    log.info(f"Requesting a fresh export for subject set {subject_set_id} "
             f"('{subject_set.display_name}'). Zooniverse builds it "
             "server-side; this waits rather than taking the previous one.")
    try:
        response = subject_set.get_export(
            "classifications", generate=True, wait=True,
            wait_timeout=WAIT_TIMEOUT_S)
    except TypeError:
        # Older panoptes_client has no wait/wait_timeout. Falling back means
        # the export may be the previously generated one, so say so.
        log.warning("This panoptes_client cannot wait for a generated export; "
                    "taking whatever Zooniverse has ready.")
        response = subject_set.get_export("classifications", generate=True)
        result.warnings.append(
            "panoptes-client is too old to wait for the export — a CSV may be "
            "an earlier one. Upgrade with: pip install -U panoptes-client")
    return response.content


def export(subject_set_ids, output_dir: str | Path,
           combined_name: str = "",
           dry_run: bool = True,
           progress: Callable[[float, str], None] | None = None,
           cancel: threading.Event | None = None) -> ExportResult:
    """Export several subject sets and combine them into one CSV.

    Several, not one, because a subject moves: it starts in the yes/no set and
    on retirement moves to the multiple-choice set and then perhaps an expert
    set. Its classifications live with whichever set it was in at the time, so
    the full picture for a transect needs every set its subjects have passed
    through. The combined CSV is deduplicated on classification_id, since a
    subject that is a member of two sets comes back in both exports.
    """
    ids = subject_set_ids if isinstance(subject_set_ids, list) \
        else parse_ids(subject_set_ids)
    result = ExportResult(subject_set_ids=list(ids), dry_run=dry_run)
    if not ids:
        raise ValueError(
            "Give at least one subject set ID — the last number in a subject "
            "set's Zooniverse URL. Several can be separated by commas.")

    out = Path(output_dir)

    # ---- check: name every set and count its subjects, generate nothing ----
    if progress:
        progress(0.05, "Asking Zooniverse about the subject sets…")
    total_subjects = 0
    for i, set_id in enumerate(ids):
        if cancel is not None and cancel.is_set():
            result.cancelled = True
            return result
        try:
            name, count = inspect(set_id)
        except Exception as exc:
            result.errors.append(f"subject set {set_id}: {exc}")
            continue
        result.subject_set_names[set_id] = name
        total_subjects += count
        result.lines.append(f"  {set_id}  {name}  —  {count:,} subject(s)")
        if progress:
            progress(0.05 + 0.15 * (i + 1) / len(ids), f"Checked {set_id}…")
    result.lines.insert(0, f"{len(result.subject_set_names)} subject set(s), "
                           f"{total_subjects:,} subject(s) in total")

    if result.errors:
        return result

    if dry_run:
        result.lines.append(f"Exports would be written into {out}, then "
                            "combined and deduplicated.")
        if progress:
            progress(1.0, "Check finished.")
        return result

    # ---- the real thing -----------------------------------------------
    out.mkdir(parents=True, exist_ok=True)
    for i, set_id in enumerate(ids):
        if cancel is not None and cancel.is_set():
            result.cancelled = True
            break
        if progress:
            progress(0.2 + 0.6 * i / len(ids),
                     f"Generating the export for {set_id} — this can take "
                     "several minutes…")
        name = result.subject_set_names.get(set_id, set_id)
        target = out / default_csv_name(set_id, name)
        try:
            content = _fetch_one(set_id, target, result)
        except Exception as exc:
            result.errors.append(f"subject set {set_id}: {exc}")
            continue
        if not content:
            result.warnings.append(
                f"Subject set {set_id} ('{name}') returned an empty export — "
                "nobody has classified those subjects yet.")
            continue
        target.write_bytes(content)
        result.per_set_csv.append(target)
        result.bytes_written += len(content)
        log.info(f"Saved {target.name} ({len(content) / 1e6:.1f} MB)")

    if not result.per_set_csv:
        if not result.errors:
            result.errors.append(
                "No export came back with any classifications.")
        return result

    if progress:
        progress(0.85, "Combining the exports…")
    combined = out / (combined_name or "all_classifications.csv")
    result.rows, result.duplicates_dropped = combine(result.per_set_csv,
                                                     combined)
    result.combined_csv = combined
    if progress:
        progress(1.0, "Exports saved.")
    return result
