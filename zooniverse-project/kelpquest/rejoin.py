"""
Bringing the volunteers' answers back to the Toolbox annotations.

``scripts/zooni_to_toolbox_annot.py`` holds the label determination rules --
which workflow wins, how many votes each needs, how a Zooniverse choice maps
onto a labelset code. Those rules are the project's, they mirror what Caesar is
configured to do, and there must be exactly one copy of them, so this loads
that module and calls its ``link_annotations``. Nothing here re-decides a
label.

What this module adds is the part the script does not answer: **where is every
point now?** A subject does not sit still. It starts in the yes/no set; on
retirement it may move to the multiple-choice set, and from there to an expert
set. The same subject id therefore appears in several subject-set exports, and
a point is only finished when some workflow reached a threshold. Four outcomes
matter, and they are not the same question as the label:

``verified``       a rule resolved it. The label is settled.
``needs_toolbox``  Zooniverse is done with it and it is still unresolved --
                   denied, voted "not sure", a consensus label the labelset
                   does not have, or retired on count without agreement. No
                   further votes are coming; a person has to label it.
``on_zooniverse``  unresolved, but not retired everywhere it lives. More votes
                   are still arriving.
``not_classified`` no classification anywhere in the exports given. Either it
                   was never uploaded, or the subject set it went to was not
                   among the ones exported.

That last one is why the export stage takes several subject sets: a point that
has moved on looks exactly like a point that was never uploaded if you only
export the set it started in.
"""

from __future__ import annotations

import importlib.util
import json
import sys
import threading
from collections.abc import Callable
from dataclasses import dataclass, field
from pathlib import Path

from .logging_setup import get_logger

log = get_logger("rejoin")

#: The module holding the rules.
LINKER = (Path(__file__).resolve().parent.parent / "scripts"
          / "zooni_to_toolbox_annot.py")

TOOLBOX_IMPORT_NAME = "toolbox_import.csv"
TOOLBOX_JSON_NAME = "toolbox_import_annotations.json"
QAQC_NAME = "qaqc_classifications.csv"
UNMAPPED_NAME = "unmapped_multi_consensus_labels.csv"
STATUS_NAME = "subject_status.csv"

#: zoon_status values that mean Zooniverse has said its piece and the answer
#: was "a person needs to look at this".
_DEAD_END = {"deny_pred", "voted_review", "multi_consensus_unmapped"}

STATUS_LABELS = {
    "verified": "Verified — label settled",
    "needs_toolbox": "Needs review in Toolbox",
    "on_zooniverse": "Still being classified on Zooniverse",
    "not_classified": "No classifications found",
}


@dataclass
class RejoinResult:
    toolbox_rows: int = 0
    matched_rows: int = 0
    counts: dict[str, int] = field(default_factory=dict)
    retired_verified: int = 0
    by_reason: dict[str, int] = field(default_factory=dict)
    subject_sets_seen: int = 0
    workflows_seen: dict[str, int] = field(default_factory=dict)
    outputs: list[Path] = field(default_factory=list)
    dry_run: bool = False
    cancelled: bool = False
    lines: list[str] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)
    errors: list[str] = field(default_factory=list)

    @property
    def ok(self) -> bool:
        return not self.errors and not self.cancelled

    def status_block(self) -> str:
        """Just the counts, for the panel card.

        Kept apart from ``summary`` on purpose: the card is the answer to "how
        much is done?", and leading it with which files were written pushes the
        numbers out of view. The file detail belongs in the log.
        """
        total = max(1, self.toolbox_rows)
        out = [f"{self.toolbox_rows:,} annotation point(s) in the transect",
               f"{self.matched_rows:,} matched a classification", ""]
        for key in ("verified", "needs_toolbox", "on_zooniverse",
                    "not_classified"):
            n = self.counts.get(key, 0)
            out.append(f"  {STATUS_LABELS[key]:<38} {n:>7,}  "
                       f"{100 * n / total:5.1f}%")
            # Directly under the row it qualifies, not at the end of the
            # block, where it reads as belonging to whatever precedes it.
            if key == "verified" and n:
                out.append(f"    {'of which retired on Zooniverse':<36} "
                           f"{self.retired_verified:>7,}")
        if self.by_reason:
            out += ["", "Why the unresolved ones are unresolved:"]
            for reason, n in sorted(self.by_reason.items(),
                                    key=lambda kv: -kv[1]):
                out.append(f"  {reason:<38} {n:>7,}")
        return "\n".join(out)

    def summary(self) -> str:
        out = ["Checked only - nothing was written." if self.dry_run
               else f"{self.toolbox_rows:,} annotation point(s) resolved"]
        out += self.lines
        out += ["", self.status_block()]
        # Warnings and errors are rendered by the stage, which owns
        # them; repeating them here printed each one twice.
        return "\n".join(out)


def available() -> bool:
    return LINKER.is_file()


def _load_linker():
    """Import scripts/zooni_to_toolbox_annot.py as a module."""
    if not LINKER.is_file():
        raise FileNotFoundError(
            f"The linker is missing: {LINKER}. It ships alongside this app in "
            "the scripts folder.")
    name = "kelpquest_zooni_to_toolbox"
    if name in sys.modules:
        return sys.modules[name]
    spec = importlib.util.spec_from_file_location(name, LINKER)
    if spec is None or spec.loader is None:
        raise ImportError(f"Could not load {LINKER}")
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


def workflow_summary() -> list[str]:
    """The rules as configured, for the GUI to show before a run."""
    z = _load_linker()
    ids = lambda s: ", ".join(str(w) for w in sorted(s))       # noqa: E731
    return [
        f"Yes/No crowd        {ids(z.WORKFLOW_YESNO):<16} "
        f"n>={z.YESNO_AGREE_MIN_N}, >={z.YESNO_AGREE_MIN_FRAC:.0%} agree",
        f"Yes/No expert       {ids(z.WORKFLOW_YESNO_EXPERT):<16} "
        f"n>={z.EXPERT_MIN_N}, >={z.YESNO_AGREE_MIN_FRAC:.0%} agree",
        f"Multi-choice crowd  {ids(z.WORKFLOW_MULTI):<16} "
        f"n>={z.MULTI_AGREE_MIN_N}, >={z.MULTI_AGREE_MIN_FRAC:.0%} agree",
        f"Multi-choice expert {ids(z.WORKFLOW_MULTI_EXPERT):<16} "
        f"n>={z.EXPERT_MIN_N}, >={z.MULTI_AGREE_MIN_FRAC:.0%} agree",
    ]


# --------------------------------------------------------------------------
#  Retirement
# --------------------------------------------------------------------------


def retirement_by_point(zc) -> object:
    """Per (source_image, row, column): is it retired, and where.

    ``subject_data`` records retirement as it stood when each classification
    was made, and per workflow -- a subject retired in yes/no keeps arriving
    un-retired in the multi set until it retires there too. So a point counts
    as retired only when *every* workflow that has seen it has retired it;
    anything less means votes are still coming in somewhere.
    """
    import pandas as pd

    rows = []
    for value in zc.get("subject_data", []):
        try:
            data = json.loads(value) if isinstance(value, str) else {}
        except (TypeError, ValueError):
            continue
        for subject_id, meta in (data or {}).items():
            if not isinstance(meta, dict):
                continue
            retired = meta.get("retired") or {}
            rows.append({
                "source_image": meta.get("source_image"),
                "Row_int": pd.to_numeric(meta.get("row"), errors="coerce"),
                "Column_int": pd.to_numeric(meta.get("column"), errors="coerce"),
                "subject_id": str(subject_id),
                "is_retired": bool(retired),
                "retirement_reason": retired.get("retirement_reason"),
            })
    frame = pd.DataFrame(rows)
    if frame.empty:
        return pd.DataFrame(columns=["source_image", "Row_int", "Column_int",
                                     "subject_ids", "n_subjects",
                                     "all_retired", "any_retired",
                                     "retirement_reasons"])

    keys = ["source_image", "Row_int", "Column_int"]
    # One row per (point, subject): a subject is retired if any classification
    # of it recorded retirement.
    per_subject = (frame.groupby(keys + ["subject_id"], dropna=False)
                        .agg(is_retired=("is_retired", "max"),
                             retirement_reason=("retirement_reason", "last"))
                        .reset_index())
    return (per_subject.groupby(keys, dropna=False)
            .agg(subject_ids=("subject_id",
                              lambda s: ";".join(sorted(set(s)))),
                 n_subjects=("subject_id", "nunique"),
                 all_retired=("is_retired", "min"),
                 any_retired=("is_retired", "max"),
                 retirement_reasons=("retirement_reason",
                                     lambda s: ";".join(
                                         sorted({str(x) for x in s if x}))))
            .reset_index())


# --------------------------------------------------------------------------
#  The run
# --------------------------------------------------------------------------


def rejoin(toolbox_csv: str | Path,
           classifications_csv: str | Path,
           labelset_json: str | Path,
           output_dir: str | Path,
           annotations_json: str | Path | None = None,
           use_yn: bool = True, use_yn_exp: bool = True,
           use_multi: bool = True, use_multi_exp: bool = True,
           dry_run: bool = True,
           progress: Callable[[float, str], None] | None = None,
           cancel: threading.Event | None = None) -> RejoinResult:
    """Apply the rules, then say where every point stands."""
    import pandas as pd

    result = RejoinResult(dry_run=dry_run)
    linker = _load_linker()

    toolbox_csv = Path(toolbox_csv)
    classifications_csv = Path(classifications_csv)
    if not toolbox_csv.is_file():
        raise FileNotFoundError(f"Toolbox annotations not found: {toolbox_csv}")
    if not classifications_csv.is_file():
        raise FileNotFoundError(
            f"Classifications export not found: {classifications_csv}")

    if progress:
        progress(0.05, f"Reading {toolbox_csv.name}…")
    ds = pd.read_csv(toolbox_csv)
    linker.ensure_required_columns(ds)
    result.toolbox_rows = len(ds)

    if progress:
        progress(0.15, f"Reading {classifications_csv.name}…")
    zc = pd.read_csv(classifications_csv, low_memory=False)
    before = len(zc)
    if "classification_id" in zc.columns:
        # Several subject-set exports may overlap: a subject that moved sets
        # is in both, and so are its classifications.
        zc = zc.drop_duplicates(subset="classification_id")
        if len(zc) < before:
            result.lines.append(
                f"{before - len(zc):,} duplicate classification(s) dropped "
                "across the exports.")
    result.lines.append(f"{len(zc):,} classification(s) read.")
    if "workflow_id" in zc.columns:
        result.workflows_seen = {str(k): int(v) for k, v in
                                 zc["workflow_id"].value_counts().items()}

    labelset_raw = json.loads(Path(labelset_json).read_text(encoding="utf-8"))
    if not isinstance(labelset_raw, list):
        raise ValueError(
            "The labelset JSON should be a list of label objects. Did you pick "
            "the annotation JSON by mistake?")

    if cancel is not None and cancel.is_set():
        result.cancelled = True
        return result

    if progress:
        progress(0.4, "Applying the label rules…")
    merged = linker.link_annotations(
        ds, zc, linker.build_label_map(labelset_raw),
        dict(linker.DEFAULT_MANUAL_OVERRIDES),
        use_yn=use_yn, use_yn_exp=use_yn_exp,
        use_multi=use_multi, use_multi_exp=use_multi_exp)
    result.matched_rows = len(merged)
    result.lines.append(
        f"{len(merged):,} of {len(ds):,} point(s) matched a classification.")

    if progress:
        progress(0.6, "Reading retirement…")
    retired = retirement_by_point(zc)
    result.subject_sets_seen = int(retired["n_subjects"].sum()) if len(retired) else 0

    status = _statuses(ds, merged, retired)
    result.counts = {k: int(v) for k, v in
                     status["kelpquest_status"].value_counts().items()}
    for key in STATUS_LABELS:
        result.counts.setdefault(key, 0)
    result.retired_verified = int(
        ((status["kelpquest_status"] == "verified") &
         status["any_retired"].fillna(False)).sum())
    unresolved = status[status["kelpquest_status"].isin(
        ("needs_toolbox", "on_zooniverse"))]
    result.by_reason = {str(k): int(v) for k, v in
                        unresolved["status_reason"].value_counts().items()}

    if dry_run:
        for name in (TOOLBOX_IMPORT_NAME, QAQC_NAME, STATUS_NAME):
            result.lines.append(f"would write {name}")
        if annotations_json:
            result.lines.append(f"would write {TOOLBOX_JSON_NAME}")
        if progress:
            progress(1.0, "Check finished.")
        return result

    out = Path(output_dir)
    out.mkdir(parents=True, exist_ok=True)
    if progress:
        progress(0.75, "Writing the Toolbox import…")

    target = out / TOOLBOX_IMPORT_NAME
    linker.export_toolbox_strict(merged, target)
    result.outputs.append(target)

    if annotations_json:
        raw_json = linker.load_toolbox_annotation_json(str(annotations_json))
        json_target = out / TOOLBOX_JSON_NAME
        linker.export_toolbox_json(merged, raw_json, labelset_raw,
                                   str(json_target))
        result.outputs.append(json_target)
    else:
        result.warnings.append(
            "No Toolbox annotation JSON given, so only the CSV was written. "
            "Toolbox needs the JSON to keep pixel-level fields.")

    if progress:
        progress(0.9, "Writing the reports…")
    qaqc = out / QAQC_NAME
    _write_qaqc(merged, qaqc)
    result.outputs.append(qaqc)

    status_path = out / STATUS_NAME
    status.to_csv(status_path, index=False)
    result.outputs.append(status_path)

    unmapped = _unmapped_report(merged, out)
    if unmapped is not None:
        result.outputs.append(unmapped)
        result.warnings.append(
            "Some multi-choice consensus labels are not in the labelset — see "
            f"{UNMAPPED_NAME}. Add them to the expansions table in "
            f"{LINKER.name}.")

    if progress:
        progress(1.0, "Rejoined.")
    return result


def _statuses(ds, merged, retired):
    """One row per Toolbox point with its status and why.

    Left join, deliberately: the interesting rows are the ones Zooniverse has
    never seen, and an inner join hides exactly those.
    """
    import numpy as np
    import pandas as pd

    keys = ["source_image", "Row_int", "Column_int"]
    base = ds.copy()
    base["source_image"] = base["Name"].astype(str).str.replace(
        ".jpg", "", regex=False)
    base["Row_int"] = pd.to_numeric(base["Row"], errors="coerce")
    base["Column_int"] = pd.to_numeric(base["Column"], errors="coerce")

    cols = keys + ["Name", "Row", "Column", "Label", "Long Label", "Verified",
                   "zoon_status"]
    have = [c for c in cols if c in merged.columns]
    frame = base[["Name", "Row", "Column", *keys]].merge(
        merged[have].rename(columns={
            "Label": "resolved_label",
            "Long Label": "resolved_long_label",
            "Verified": "resolved_verified"}),
        on=keys, how="left", suffixes=("", "_m"))
    if len(retained := retired) and not retained.empty:
        frame = frame.merge(retained, on=keys, how="left")
    else:
        for c in ("subject_ids", "n_subjects", "all_retired", "any_retired",
                  "retirement_reasons"):
            frame[c] = np.nan

    verified = frame["resolved_verified"].fillna(False).astype(bool)
    seen = frame["zoon_status"].notna()
    dead_end = frame["zoon_status"].isin(_DEAD_END)
    # Retired everywhere it lives and still not resolved: no more votes are
    # coming, so it is a Toolbox job rather than a waiting game.
    exhausted = frame["all_retired"].fillna(False).astype(bool)

    frame["kelpquest_status"] = np.select(
        [verified,
         seen & (dead_end | exhausted),
         seen],
        ["verified", "needs_toolbox", "on_zooniverse"],
        default="not_classified")

    frame["status_reason"] = np.select(
        [verified,
         frame["zoon_status"].eq("deny_pred"),
         frame["zoon_status"].eq("voted_review"),
         frame["zoon_status"].eq("multi_consensus_unmapped"),
         seen & exhausted,
         seen],
        ["resolved",
         "denied in yes/no",
         "volunteers voted 'not sure'",
         "consensus label not in labelset",
         "retired without consensus",
         "waiting for more votes"],
        default="never classified")
    return frame.drop(columns=[c for c in frame.columns
                               if c.endswith("_m")], errors="ignore")


def _write_qaqc(merged, path: Path) -> None:
    """The per-point vote detail, with the reason each row is where it is."""
    import pandas as pd

    rename = {
        "yn_n": "yesno_n_votes", "yn_yes": "yesno_n_yes", "yn_no": "yesno_n_no",
        "yn_yes_frac": "yesno_yes_frac", "yn_no_frac": "yesno_no_frac",
        "yn_exp_n": "yesno_expert_n_votes", "yn_exp_yes": "yesno_expert_n_yes",
        "yn_exp_no": "yesno_expert_n_no",
        "yn_exp_yes_frac": "yesno_expert_yes_frac",
        "yn_exp_no_frac": "yesno_expert_no_frac",
        "m_n": "multi_n_votes", "m_top_label": "multi_top_label",
        "m_top_count": "multi_top_count", "m_agreement": "multi_agreement",
        "m_exp_n": "multi_expert_n_votes",
        "m_exp_top_label": "multi_expert_top_label",
        "m_exp_top_count": "multi_expert_top_count",
        "m_exp_agreement": "multi_expert_agreement",
    }
    wanted = ["Name", "Row", "Column", "Label", "Long Label", "Verified",
              "zoon_status", *rename]
    cols = [c for c in dict.fromkeys(wanted) if c in merged.columns]
    qa = merged[cols].rename(columns={k: v for k, v in rename.items()
                                      if k in merged.columns})
    for c in ("yesno_yes_frac", "yesno_no_frac", "yesno_expert_yes_frac",
              "yesno_expert_no_frac", "multi_agreement",
              "multi_expert_agreement"):
        if c in qa.columns:
            qa[c] = pd.to_numeric(qa[c], errors="coerce").round(3)
    qa["label_source"] = qa["zoon_status"].map({
        "multi_expert": "Expert multi-choice consensus",
        "multi_consensus": "Volunteer multi-choice consensus",
        "confirm_expert": "Confirmed by expert yes/no vote",
        "confirm_pred": "Confirmed by volunteer yes/no vote",
        "deny_pred": "Review - denied in yes/no workflow",
        "multi_consensus_unmapped": "Review - label not in labelset",
        "voted_review": "Review - volunteers voted 'not sure'",
        "needs_more_votes": "Review - insufficient votes",
    }).fillna("Review - unknown")
    qa.to_csv(path, index=False)
    log.info(f"Wrote QA/QC: {path}  ({len(qa):,} rows)")


def _unmapped_report(merged, out: Path) -> Path | None:
    """Which consensus labels the labelset could not name, and how often."""
    import pandas as pd

    rows = merged[merged["zoon_status"] == "multi_consensus_unmapped"]
    if rows.empty:
        return None
    labels = pd.Series(dtype=object)
    for col in ("m_exp_top_label", "m_top_label"):
        if col in rows.columns:
            labels = pd.concat([labels, rows[col].dropna().astype(str)])
    if labels.empty:
        return None
    counts = labels.value_counts()
    target = out / UNMAPPED_NAME
    pd.DataFrame({"raw_label": counts.index,
                  "count": counts.values}).to_csv(target, index=False)
    log.info(f"Wrote unmapped labels: {target}")
    return target
