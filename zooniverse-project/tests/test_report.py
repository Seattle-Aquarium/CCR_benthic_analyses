"""
Stage 8: finding the analysis tool, and narrowing a report to one transect.

The transect filter is the part worth testing properly. The multiple-choice
and expert subject sets are shared by the whole project, so an export of them
carries every transect's subjects -- a report built without the filter
silently summarises twenty transects when somebody asked about one. These
build a two-transect export and check the filter actually narrows it.

The workbook itself is not written here: that is openpyxl's job and it is slow.
What is checked is the row count after filtering, which is what every sheet is
then built from.
"""

from __future__ import annotations

import csv
import json
import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from kelpquest import report

MULTI_CROWD = 30752


# --------------------------------------------------------------------------
#  finding the tool
# --------------------------------------------------------------------------


def test_the_analysis_tool_is_found_under_the_name_it_actually_has():
    """It shipped as ``analyze_``; the README and its own docstring say
    ``analyse_``. Looking for one spelling only reported the tool missing
    while it sat in the scripts folder."""
    assert report.available(), (
        f"not found as any of {report.ANALYSER_NAMES} in {report.SCRIPTS_DIR}")
    assert report.analyser_path().is_file()


def test_both_spellings_are_looked_for():
    assert set(report.ANALYSER_NAMES) == {"analyze_classifications.py",
                                          "analyse_classifications.py"}


def test_the_sheet_list_matches_the_tools_builders():
    """A builder named here that the module does not have is skipped with a
    warning, so a typo would cost a whole sheet silently."""
    analyser = report._load_analyser()
    missing = [fn for _name, fn in report.SHEETS
               if not hasattr(analyser, fn)]
    assert not missing, f"no such builder(s): {missing}"


def test_the_transect_sheet_is_included():
    """It is the per-transect completion table -- the sheet somebody opens to
    ask how far along a transect is."""
    assert "Transect Completion" in [name for name, _fn in report.SHEETS]


# --------------------------------------------------------------------------
#  naming the workbook
# --------------------------------------------------------------------------


def test_the_filename_says_what_was_filtered():
    """A whole-project report and one narrowed to a transect are different
    documents; telling them apart by timestamp means opening both."""
    name = report.default_xlsx_name("30752", "", "EBM_W25_T6")
    assert "wf30752" in name
    assert "EBM_W25_T6" in name
    assert name.endswith(".xlsx")


def test_several_transects_still_give_one_usable_filename():
    name = report.default_xlsx_name("", "", "EBM_W25_T6, PKB_S25_T1")
    assert " " not in name
    assert "," not in name


# --------------------------------------------------------------------------
#  the transect filter
# --------------------------------------------------------------------------


HEAD = ["classification_id", "user_name", "user_id", "workflow_id",
        "workflow_name", "workflow_version", "created_at", "subject_ids",
        "metadata", "annotations", "subject_data"]


def _export(path: Path, rows: list[tuple[str, str, int]]) -> Path:
    """rows: (subject id, transect id, how many classifications)."""
    cid = 1
    with path.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=HEAD)
        w.writeheader()
        for subject_id, transect_id, times in rows:
            for _ in range(times):
                w.writerow({
                    "classification_id": cid,
                    "user_name": f"volunteer{cid}",
                    "user_id": cid,
                    "workflow_id": MULTI_CROWD,
                    "workflow_name": "Multiple Choice",
                    "workflow_version": "1.1",
                    "created_at": "2025-02-01 00:00:00 UTC",
                    "subject_ids": subject_id,
                    "metadata": json.dumps({
                        "started_at": "2025-02-01T00:00:00.000Z",
                        "finished_at": "2025-02-01T00:00:08.000Z",
                        "user_agent": "Mozilla/5.0"}),
                    "annotations": json.dumps([{"task": "T1",
                                                "value": "Silt"}]),
                    "subject_data": json.dumps({subject_id: {
                        "source_image": f"{transect_id}_still",
                        "transect_id": transect_id,
                        "row": 10, "column": 20,
                        "filename": f"{transect_id}_r10_c20.jpg",
                        "retired": None}}),
                })
                cid += 1
    return path


@pytest.fixture
def two_transects(tmp_path: Path) -> Path:
    return _export(tmp_path / "export.csv", [
        ("s1", "EBM_W25_T6", 3),
        ("s2", "EBM_W25_T6", 3),
        ("s3", "PKB_S25_T1", 4),
    ])


def test_without_a_filter_every_transect_is_in_the_report(two_transects, tmp_path):
    result = report.build(two_transects, tmp_path / "out", dry_run=True)
    assert result.ok, result.errors
    assert result.rows == 10
    assert any("2 transect(s)" in line for line in result.lines), result.lines


def test_the_filter_narrows_the_report_to_one_transect(two_transects, tmp_path):
    result = report.build(two_transects, tmp_path / "out",
                          transect_id="EBM_W25_T6", dry_run=True)
    assert result.ok, result.errors
    assert result.rows == 6
    assert any("EBM_W25_T6" in line for line in result.lines)
    assert not any("PKB_S25_T1" in line for line in result.lines)


def test_several_transects_can_be_asked_for_at_once(two_transects, tmp_path):
    result = report.build(two_transects, tmp_path / "out",
                          transect_id="EBM_W25_T6, PKB_S25_T1", dry_run=True)
    assert result.rows == 10


def test_a_filter_that_matches_nothing_is_a_sentence_not_a_dead_thread(
        two_transects, tmp_path):
    """The tool calls sys.exit on an empty filter. Inside a GUI worker that
    would raise SystemExit through the thread and leave the bar spinning."""
    result = report.build(two_transects, tmp_path / "out",
                          transect_id="EBM_T6", dry_run=True)
    assert not result.ok
    assert result.errors
    assert "EBM_T6" in result.errors[0]
    # And it says what a transect id has to look like, since the usual cause
    # is the prefix being left off.
    assert "EBM_W25_T6" in result.errors[0]


def test_untagged_subjects_are_reported_before_anybody_filters(tmp_path):
    """Subjects uploaded before transect_id was stamped have none, so a
    filtered report cannot see them. Said on the unfiltered run, which is the
    only run where the number is knowable."""
    path = _export(tmp_path / "export.csv", [("s1", "EBM_W25_T6", 2)])
    rows = list(csv.DictReader(path.open(newline="", encoding="utf-8")))
    older = dict(rows[0])
    older["classification_id"] = "99"
    older["subject_data"] = json.dumps({"s9": {
        "source_image": "old_still", "row": 1, "column": 2,
        "filename": "old.jpg", "retired": None}})
    with path.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=HEAD)
        w.writeheader()
        w.writerows([*rows, older])

    result = report.build(path, tmp_path / "out", dry_run=True)
    assert any("no transect_id" in w for w in result.warnings), result.warnings


def test_a_check_writes_no_workbook(two_transects, tmp_path):
    out = tmp_path / "out"
    result = report.build(two_transects, out, dry_run=True)
    assert result.ok
    assert result.output_xlsx is None
    assert not out.exists() or not list(out.glob("*.xlsx"))


def test_a_missing_export_is_raised_before_anything_is_loaded(tmp_path):
    with pytest.raises(FileNotFoundError):
        report.build(tmp_path / "nope.csv", tmp_path / "out")
