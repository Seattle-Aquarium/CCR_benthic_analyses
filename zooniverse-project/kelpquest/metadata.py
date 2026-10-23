"""
The sheets a run produces.

``patches.csv`` is the manifest: one row per patch, written when the patches
are cut and filled in again when they are classified. It is the seam between
the stages, so classification can be re-run with different weights without
cutting every patch again.

The rest are the deliverables:

* **the Toolbox sheet** - the same columns CoralNet-Toolbox exports, so the
  whole transect can be imported there and verified point by point;
* **the Zooniverse sheet** - ``metadata.csv`` beside the patch images, which is
  what the upload reads and what becomes each subject's metadata;
* **annotations + telemetry** - written only when a UTC transect CSV is in
  play: every point with the depth and position of the still it sits on, which
  is what a cover-versus-depth analysis needs.
"""

from __future__ import annotations

import csv
from dataclasses import dataclass, field
from pathlib import Path

from . import telemetry as telemetry_mod
from .classify import TOP_N, Suggestion
from .labelset import Labelset
from .logging_setup import get_logger

log = get_logger("metadata")

MANIFEST_NAME = "patches.csv"
ZOONIVERSE_NAME = "metadata.csv"

#: Zooniverse hides any metadata key beginning with "#" from volunteers. The
#: model's confidence is useful downstream and must not be on screen while
#: somebody is deciding whether they agree with the model.
HIDDEN_PREFIX = "#"


@dataclass
class PatchRecord:
    """One patch: where it came from, and what the model made of it."""

    filename: str
    name: str                 # source image file name
    path: str                 # source image, full path
    row: int
    column: int
    patch_size: int
    scale: float
    suggestions: list[Suggestion] = field(default_factory=list)
    #: Set once the patch has actually been cut and written. Not a column --
    #: a record only reaches a sheet when it is true, so reading one back
    #: always gives True. It exists so a stopped run writes sheets describing
    #: the patches that are on disk rather than the ones it meant to make.
    done: bool = True

    @property
    def code(self) -> str:
        return self.suggestions[0].code if self.suggestions else ""

    @property
    def confidence(self) -> float:
        return self.suggestions[0].confidence if self.suggestions else 0.0

    @property
    def classified(self) -> bool:
        return bool(self.suggestions)

    @property
    def source_image(self) -> str:
        """Without the extension, so Zooniverse does not read it as media.

        A subject's metadata values are shown to volunteers and indexed; a
        value ending in .jpg has been mistaken for a second image to fetch.
        """
        return Path(self.name).stem


# --------------------------------------------------------------------------
#  Manifest
# --------------------------------------------------------------------------

_MANIFEST_HEAD = ["filename", "name", "path", "row", "column",
                  "patch_size", "scale", "pred_code", "pred_confidence"]
_MANIFEST_HEAD += [f"suggestion_{i}" for i in range(1, TOP_N + 1)]
_MANIFEST_HEAD += [f"confidence_{i}" for i in range(1, TOP_N + 1)]


def write_manifest(path: str | Path, records: list[PatchRecord]) -> Path:
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh)
        w.writerow(_MANIFEST_HEAD)
        for r in records:
            sug = r.suggestions[:TOP_N]
            sug += [Suggestion("", 0.0)] * (TOP_N - len(sug))
            w.writerow([r.filename, r.name, r.path, r.row, r.column,
                        r.patch_size, r.scale, r.code,
                        f"{r.confidence:.4f}" if r.classified else ""]
                       + [s.code for s in sug]
                       + [f"{s.confidence:.4f}" if s.code else "" for s in sug])
    log.info(f"Wrote manifest: {path}  ({len(records):,} patches)")
    return path


def read_manifest(path: str | Path) -> list[PatchRecord]:
    """Load a manifest so a later stage can pick the run up where it stopped."""
    path = Path(path)
    records: list[PatchRecord] = []
    with path.open(newline="", encoding="utf-8") as fh:
        for row in csv.DictReader(fh):
            suggestions = []
            for i in range(1, TOP_N + 1):
                code = (row.get(f"suggestion_{i}") or "").strip()
                if not code:
                    continue
                try:
                    conf = float(row.get(f"confidence_{i}") or 0.0)
                except ValueError:
                    conf = 0.0
                suggestions.append(Suggestion(code, conf))
            records.append(PatchRecord(
                filename=row["filename"], name=row["name"], path=row["path"],
                row=int(row["row"]), column=int(row["column"]),
                patch_size=int(row["patch_size"]), scale=float(row["scale"]),
                suggestions=suggestions,
            ))
    return records


# --------------------------------------------------------------------------
#  Toolbox
# --------------------------------------------------------------------------

TOOLBOX_HEAD = ["Name", "Path", "Row", "Column", "Patch Size",
                "Annotation Type", "Label", "Long Label", "Verified"]
for _i in range(1, TOP_N + 1):
    TOOLBOX_HEAD += [f"Machine confidence {_i}", f"Machine suggestion {_i}"]


def write_toolbox_csv(path: str | Path, records: list[PatchRecord],
                      labels: Labelset) -> Path:
    """The annotations sheet, in the column order Toolbox exports.

    ``Verified`` is False on every row: these are the model's opinions, and the
    point of importing them is that a person goes through and confirms them.
    """
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh)
        w.writerow(TOOLBOX_HEAD)
        for r in records:
            row = [r.name, r.path, r.row, r.column, r.patch_size, "Patch",
                   r.code, labels.long_name(r.code) if r.code else "", "False"]
            sug = r.suggestions[:TOP_N]
            sug += [Suggestion("", 0.0)] * (TOP_N - len(sug))
            for s in sug:
                # Three decimals, matching the exports already in the annotation
                # folders, so a diff between ours and Toolbox's is meaningful.
                row += [f"{s.confidence:.3f}" if s.code else "", s.code]
            w.writerow(row)
    log.info(f"Wrote Toolbox sheet: {path}  ({len(records):,} annotations)")
    return path


# --------------------------------------------------------------------------
#  Zooniverse
# --------------------------------------------------------------------------

ZOONIVERSE_HEAD = ["filename", "source_image", "row", "column",
                   "model_pred_code", "model_pred_name",
                   "site_name", "survey_date", "transect_number",
                   "transect_id",
                   HIDDEN_PREFIX + "model_confidence",
                   HIDDEN_PREFIX + "patch_size"]

#: Telemetry rides along hidden. It is for the analysis afterwards, and depth
#: or position on screen while somebody is judging what a patch shows is
#: context they were not asked to weigh.
ZOONIVERSE_TELEMETRY_HEAD = [HIDDEN_PREFIX + name
                             for name in telemetry_mod.FIELDS]


def write_zooniverse_csv(path: str | Path, records: list[PatchRecord],
                         labels: Labelset, site_name: str = "",
                         survey_date: str = "",
                         transect_number: str = "",
                         transect_id: str = "",
                         telemetry: telemetry_mod.Telemetry | None = None
                         ) -> Path:
    """``metadata.csv`` beside the patch images - one row per subject."""
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    head = list(ZOONIVERSE_HEAD)
    if telemetry is not None:
        head += ZOONIVERSE_TELEMETRY_HEAD
    with path.open("w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh)
        w.writerow(head)
        for r in records:
            row = [
                r.filename, r.source_image, r.row, r.column,
                r.code, labels.long_name(r.code) if r.code else "",
                site_name, survey_date, transect_number, transect_id,
                f"{r.confidence:.4f}" if r.classified else "",
                r.patch_size,
            ]
            if telemetry is not None:
                values = telemetry.for_still(r.name)
                row += [values.get(name, "") for name in telemetry_mod.FIELDS]
            w.writerow(row)
    log.info(f"Wrote Zooniverse sheet: {path}  ({len(records):,} subjects)")
    return path


JOINED_NAME_SUFFIX = "_annotations_telemetry.csv"


def write_joined_csv(path: str | Path, records: list[PatchRecord],
                     labels: Labelset,
                     telemetry: telemetry_mod.Telemetry) -> Path:
    """Every annotation with the telemetry for the still it sits on.

    The same join ``scripts/join_percent_cover_telemetry.py`` does after the
    fact, done here while both halves are already in hand. Percent cover is
    the share of points on each class, so a per-point table carrying depth and
    position is what the cover-versus-depth analysis actually needs.

    Written separately from the Toolbox sheet on purpose: that one has to keep
    the exact columns Toolbox exports, and extra ones risk its importer.
    """
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    head = (["Name", "Row", "Column", "Patch Size", "Label", "Long Label",
             "Machine confidence 1", "transect_id", "site_name", "survey_date",
             "transect_number"]
            + list(telemetry_mod.FIELDS))
    with path.open("w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh)
        w.writerow(head)
        for r in records:
            values = telemetry.for_still(r.name)
            w.writerow([
                r.name, r.row, r.column, r.patch_size,
                r.code, labels.long_name(r.code) if r.code else "",
                f"{r.confidence:.3f}" if r.classified else "",
                telemetry.transect_id, telemetry.site_code,
                telemetry.survey_date, telemetry.transect_number,
            ] + [values.get(name, "") for name in telemetry_mod.FIELDS])
    log.info(f"Wrote annotations + telemetry: {path}  ({len(records):,} rows)")
    return path


def sheet_stem(site_name: str, survey_date: str,
               transect_number: str) -> str:
    """``2025_01_28_EBM_T6`` — the stem both output sheets are named from."""
    date_part = (survey_date or "").replace("-", "_")
    bits = [b for b in (date_part, site_name, transect_number) if b]
    return "_".join(bits) if bits else "transect"


def toolbox_csv_name(site_name: str, survey_date: str,
                     transect_number: str) -> str:
    """``2025_01_28_EBM_T6_toolbox_annotations.csv``, matching the existing files."""
    return sheet_stem(site_name, survey_date,
                      transect_number) + "_toolbox_annotations.csv"


def joined_csv_name(site_name: str, survey_date: str,
                    transect_number: str) -> str:
    """``2025_01_28_EBM_T6_annotations_telemetry.csv``."""
    return sheet_stem(site_name, survey_date,
                      transect_number) + JOINED_NAME_SUFFIX


def label_summary(records: list[PatchRecord], labels: Labelset,
                  top: int = 12) -> list[str]:
    """Counts per predicted label - the first thing to look at after a run.

    A transect that comes back 90% one class usually means the wrong weights,
    and seeing that on screen is cheaper than finding it after the upload.
    """
    counts: dict[str, int] = {}
    for r in records:
        if r.code:
            counts[r.code] = counts.get(r.code, 0) + 1
    total = sum(counts.values()) or 1
    lines = []
    for code, n in sorted(counts.items(), key=lambda kv: -kv[1])[:top]:
        lines.append(f"  {code:<12} {n:>6,}  {100 * n / total:5.1f}%  "
                     f"{labels.long_name(code)}")
    if len(counts) > top:
        lines.append(f"  … and {len(counts) - top} more label(s)")
    return lines
