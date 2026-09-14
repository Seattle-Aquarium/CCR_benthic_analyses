"""
The Excel summary, built from a classifications export.

``scripts/analyse_classifications.py`` already does this well: it flattens the
raw export -- annotations, subject_data and metadata all arrive as JSON inside
CSV cells -- and builds seven styled sheets. That is eight hundred lines of
pandas and openpyxl, and re-implementing it here would leave two versions to
keep in step, so this loads that module and calls its functions.

It is loaded by path rather than imported. ``scripts/`` is a folder of
standalone tools, not a package, so there is no ``scripts.analyse_...`` to
import; a path load also keeps this working if the folder is ever renamed
around it, with one place to change.

Two things about that module need handling here, both because it was written to
be a command-line tool:

* ``load_and_flatten`` calls ``sys.exit`` when a filter matches no rows. Inside
  a GUI worker that would raise ``SystemExit`` through the thread and leave the
  window with a spinning bar, so it is caught and turned into a message.
* It configures the root logger on import. Ours is a named logger that does not
  propagate, so the two do not collide.
"""

from __future__ import annotations

import importlib.util
import sys
import threading
from collections.abc import Callable
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path

from .logging_setup import get_logger

log = get_logger("report")

#: The analysis tool this delegates to.
ANALYSER = (Path(__file__).resolve().parent.parent / "scripts"
            / "analyse_classifications.py")

#: Sheet name -> builder attribute on that module, in the order they appear in
#: the workbook.
SHEETS = (
    ("Overview", "build_overview"),
    ("Workflow Summary", "build_workflow_summary"),
    ("Subject Summary", "build_subject_summary"),
    ("User Summary", "build_user_summary"),
    ("Answer Breakdown", "build_answer_breakdown"),
    ("Source Image", "build_source_image_summary"),
    ("Time Stats", "build_time_stats"),
)


@dataclass
class ReportResult:
    export_csv: Path | None = None
    output_xlsx: Path | None = None
    rows: int = 0
    sheet_rows: dict[str, int] = field(default_factory=dict)
    dry_run: bool = False
    cancelled: bool = False
    lines: list[str] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)
    errors: list[str] = field(default_factory=list)

    @property
    def ok(self) -> bool:
        return not self.errors and not self.cancelled

    def summary(self) -> str:
        head = ("Checked only - no workbook was written." if self.dry_run
                else f"Report built from {self.rows:,} classification row(s)")
        out = [head]
        out += self.lines
        for name, count in self.sheet_rows.items():
            out.append(f"  {name:<18} {count:>6,} rows")
        # The file it wrote is reported by the stage, which owns `outputs`.
        # Warnings and errors are rendered by the stage, which owns
        # them; repeating them here printed each one twice.
        return "\n".join(out)


def available() -> bool:
    return ANALYSER.is_file()


def _load_analyser():
    """Import scripts/analyse_classifications.py as a module."""
    if not ANALYSER.is_file():
        raise FileNotFoundError(
            f"The analysis tool is missing: {ANALYSER}. It ships alongside "
            "this app in the scripts folder.")
    name = "kelpquest_analyse_classifications"
    if name in sys.modules:
        return sys.modules[name]
    spec = importlib.util.spec_from_file_location(name, ANALYSER)
    if spec is None or spec.loader is None:
        raise ImportError(f"Could not load {ANALYSER}")
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    # Point its logger at ours. It configures the root logger to stdout on
    # import, and a windowed build has no stdout at all -- while a console one
    # is cp1252 here, so its own "Report saved ->" line raises
    # UnicodeEncodeError on the arrow and is dropped. Ours is UTF-8 and reaches
    # the GUI's log pane, which is where somebody is actually looking.
    module.log = get_logger("analyse")
    return module


def default_xlsx_name(workflow_id: str = "", source_image: str = "") -> str:
    bits = ["classification_report", f"{datetime.now():%Y%m%d_%H%M%S}"]
    if workflow_id:
        bits.append(f"wf{workflow_id}")
    if source_image:
        bits.append(str(source_image).replace(".", "_"))
    return "_".join(bits) + ".xlsx"


def build(export_csv: str | Path, output_dir: str | Path,
          workflow_id: str = "", source_image: str = "",
          dry_run: bool = True,
          progress: Callable[[float, str], None] | None = None,
          cancel: threading.Event | None = None) -> ReportResult:
    """Flatten an export and write the multi-sheet workbook."""
    export_csv = Path(export_csv)
    result = ReportResult(export_csv=export_csv, dry_run=dry_run)
    if not export_csv.is_file():
        raise FileNotFoundError(f"Export CSV not found: {export_csv}")

    analyser = _load_analyser()
    wf: int | None = None
    if str(workflow_id).strip():
        try:
            wf = int(str(workflow_id).strip())
        except ValueError as exc:
            raise ValueError("Workflow ID must be a whole number, or empty "
                             "for all workflows.") from exc
    source = str(source_image).strip() or None

    if progress:
        progress(0.15, f"Reading {export_csv.name}…")
    try:
        flat = analyser.load_and_flatten(str(export_csv), workflow_id=wf,
                                         source_image_filter=source)
    except SystemExit:
        # The tool exits on an empty filter result. In a GUI that has to be a
        # sentence, not a dead worker thread.
        which = ", ".join(
            p for p in (f"workflow {wf}" if wf else "",
                        f"source image '{source}'" if source else "") if p)
        result.errors.append(
            f"No classifications matched {which or 'those filters'}. Clear the "
            "filters, or check the workflow ID against the export.")
        return result

    result.rows = len(flat)
    result.lines.append(f"{len(flat):,} classification row(s) after filtering")
    if cancel is not None and cancel.is_set():
        result.cancelled = True
        return result

    if progress:
        progress(0.5, "Building the summary sheets…")
    filters = {"workflow_id": wf, "source_image": source}
    sheets = {}
    for i, (name, builder) in enumerate(SHEETS):
        if cancel is not None and cancel.is_set():
            result.cancelled = True
            return result
        fn = getattr(analyser, builder, None)
        if fn is None:
            result.warnings.append(f"{ANALYSER.name} has no {builder}; the "
                                   f"'{name}' sheet was skipped.")
            continue
        try:
            frame = fn(flat, filters) if builder == "build_overview" else fn(flat)
        except Exception as exc:
            result.warnings.append(f"'{name}' could not be built: {exc}")
            continue
        sheets[name] = frame
        result.sheet_rows[name] = len(frame)
        if progress:
            progress(0.5 + 0.35 * (i + 1) / len(SHEETS), f"Built '{name}'…")

    if not sheets:
        result.errors.append("None of the summary sheets could be built — see "
                             "the warnings above.")
        return result

    if dry_run:
        result.lines.append("A workbook would be written to "
                            f"{Path(output_dir) / default_xlsx_name(workflow_id, source or '')}")
        if progress:
            progress(1.0, "Check finished.")
        return result

    target = Path(output_dir) / default_xlsx_name(workflow_id, source or "")
    target.parent.mkdir(parents=True, exist_ok=True)
    if progress:
        progress(0.9, "Writing the workbook…")
    colors = getattr(analyser, "SHEET_COLORS", None) or {
        "Overview": getattr(analyser, "TEAL_DARK", None),
        "Workflow Summary": getattr(analyser, "TEAL_MID", None),
        "Subject Summary": getattr(analyser, "TEAL_MID", None),
        "User Summary": "5D6D7E",
        "Answer Breakdown": getattr(analyser, "GREEN_DARK", None),
        "Source Image": getattr(analyser, "ORANGE", None),
        "Time Stats": "7D3C98",
    }
    analyser.write_report(sheets, target,
                          header_colors={k: v for k, v in colors.items() if v})
    result.output_xlsx = target
    log.info(f"Report written: {target}")
    if progress:
        progress(1.0, "Report written.")
    return result
