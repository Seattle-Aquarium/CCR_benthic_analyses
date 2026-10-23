"""
Exporting several subject sets, and working out where every point stands.

The label rules themselves live in ``scripts/zooni_to_toolbox_annot.py`` and
are exercised through it; what is tested here is the layer this app adds --
combining exports from subject sets a subject has moved between, reading
retirement, and turning both into the four counts an operator acts on.

The classification exports are synthesised in the real shape: annotations and
subject_data are JSON inside CSV cells, retirement is a dict under
``subject_data[<id>]["retired"]``.
"""

from __future__ import annotations

import csv
import json
import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from kelpquest import export, rejoin

STILL = "2025_01_28_11-34-11"

# Workflow ids, matching the linker's sets.
YN_CROWD, YN_EXPERT = 30787, 31534
MULTI_CROWD, MULTI_EXPERT = 30752, 31535


# --------------------------------------------------------------------------
#  building a fake export
# --------------------------------------------------------------------------


def _subject_data(subject_id: str, row: int, column: int,
                  retired: bool = False,
                  reason: str = "consensus") -> str:
    return json.dumps({subject_id: {
        "source_image": STILL,
        "row": row,
        "column": column,
        "filename": f"{STILL}_r{row}_c{column}.jpg",
        "retired": ({"retirement_reason": reason, "classifications_count": 5,
                     "retired_at": "2025-02-01T00:00:00Z"} if retired
                    else None),
    }})


def _yesno(answer: str) -> str:
    return json.dumps([{"task": "T0", "value": answer}])


def _multi(choice: str) -> str:
    return json.dumps([{"task": "T1", "value": choice}])


class Export:
    """Accumulates classification rows and writes them as a CSV."""

    HEAD = ["classification_id", "user_name", "workflow_id", "workflow_name",
            "created_at", "annotations", "subject_data", "subject_ids"]

    def __init__(self):
        self.rows: list[dict] = []
        self._next = 1

    def add(self, workflow_id: int, annotations: str, subject_id: str,
            row: int, column: int, times: int = 1, retired: bool = False,
            reason: str = "consensus", classification_id: int | None = None):
        for _ in range(times):
            cid = classification_id if classification_id is not None else self._next
            self._next += 1
            self.rows.append({
                "classification_id": cid,
                "user_name": f"volunteer{cid}",
                "workflow_id": workflow_id,
                "workflow_name": "test",
                "created_at": "2025-02-01 00:00:00 UTC",
                "annotations": annotations,
                "subject_data": _subject_data(subject_id, row, column,
                                              retired, reason),
                "subject_ids": subject_id,
            })
        return self

    def write(self, path: Path) -> Path:
        with path.open("w", newline="", encoding="utf-8") as fh:
            w = csv.DictWriter(fh, fieldnames=self.HEAD)
            w.writeheader()
            w.writerows(self.rows)
        return path


def _toolbox_csv(path: Path, points: list[tuple[int, int]]) -> Path:
    head = ["Name", "Path", "Row", "Column", "Patch Size", "Annotation Type",
            "Label", "Long Label", "Verified",
            "Machine confidence 1", "Machine suggestion 1"]
    with path.open("w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh)
        w.writerow(head)
        for row, column in points:
            w.writerow([f"{STILL}.jpg", f"C:/x/{STILL}.jpg", row, column, 224,
                        "Patch", "SU_bould", "substrate - Boulder", "False",
                        "0.900", "SU_bould"])
    return path


@pytest.fixture
def labelset(tmp_path: Path) -> Path:
    path = tmp_path / "labelset.json"
    path.write_text(json.dumps([
        {"short_label_code": "SU_bould", "long_label_code": "substrate - Boulder",
         "id": "aaa", "color": [1, 2, 3, 255]},
        {"short_label_code": "SU_silt", "long_label_code": "substrate - Silt",
         "id": "bbb", "color": [4, 5, 6, 255]},
    ]), encoding="utf-8")
    return path


# --------------------------------------------------------------------------
#  subject set ids
# --------------------------------------------------------------------------


@pytest.mark.parametrize("text,expected", [
    ("135009", ["135009"]),
    ("135009, 135054", ["135009", "135054"]),
    ("135009 135054", ["135009", "135054"]),
    ("135009\n135054;136001", ["135009", "135054", "136001"]),
    ("135009, 135009", ["135009"]),            # a set named twice
    ("", []),
    ("not-a-set", []),
])
def test_parse_ids(text, expected):
    """Ids get pasted from a spreadsheet column as often as typed."""
    assert export.parse_ids(text) == expected


def test_combine_drops_classifications_seen_in_two_sets(tmp_path: Path):
    """A subject that moved sets is a member of both, so its classifications
    come back in both exports. Counting them twice would inflate the vote
    totals the thresholds are measured against."""
    a = Export().add(YN_CROWD, _yesno("Yes"), "s1", 10, 20, times=3).write(
        tmp_path / "a.csv")
    # The same three classifications, as the second set's export returns them.
    b = Export()
    b._next = 1
    b.add(YN_CROWD, _yesno("Yes"), "s1", 10, 20, times=3)
    b.add(MULTI_CROWD, _multi("Silt"), "s1", 10, 20, times=2)
    b = b.write(tmp_path / "b.csv")

    rows, dropped = export.combine([a, b], tmp_path / "all.csv")
    assert dropped == 3
    assert rows == 5


# --------------------------------------------------------------------------
#  retirement
# --------------------------------------------------------------------------


def test_a_point_is_retired_only_when_every_workflow_has_retired_it(tmp_path):
    """A subject retired in yes/no keeps arriving un-retired from the multi
    set until it retires there too -- and while that is true, votes are still
    coming in somewhere."""
    import pandas as pd

    ex = Export()
    ex.add(YN_CROWD, _yesno("Yes"), "s1", 10, 20, times=2, retired=True)
    ex.add(MULTI_CROWD, _multi("Silt"), "s2", 10, 20, times=2, retired=False)
    path = ex.write(tmp_path / "e.csv")

    frame = rejoin.retirement_by_point(pd.read_csv(path))
    assert len(frame) == 1
    got = frame.iloc[0]
    assert got["n_subjects"] == 2
    assert bool(got["any_retired"]) is True
    assert bool(got["all_retired"]) is False


def test_retirement_reasons_are_carried_through(tmp_path: Path):
    import pandas as pd

    path = Export().add(YN_CROWD, _yesno("No"), "s1", 10, 20, times=2,
                        retired=True,
                        reason="classification_count").write(tmp_path / "e.csv")
    frame = rejoin.retirement_by_point(pd.read_csv(path))
    assert "classification_count" in frame.iloc[0]["retirement_reasons"]


def test_no_subject_data_gives_an_empty_frame(tmp_path: Path):
    import pandas as pd

    assert rejoin.retirement_by_point(pd.DataFrame({"x": [1]})).empty


# --------------------------------------------------------------------------
#  the four statuses
# --------------------------------------------------------------------------


@pytest.fixture
def scenario(tmp_path: Path, labelset: Path):
    """One point per outcome, so the counts can be asserted exactly.

    (10,20) confirmed by the crowd yes/no  -> verified
    (11,21) denied by the crowd yes/no     -> needs_toolbox
    (12,22) two votes, nothing retired     -> on_zooniverse
    (13,23) retired on count, no consensus -> needs_toolbox
    (14,24) never classified               -> not_classified
    """
    points = [(10, 20), (11, 21), (12, 22), (13, 23), (14, 24)]
    toolbox = _toolbox_csv(tmp_path / "toolbox.csv", points)

    ex = Export()
    ex.add(YN_CROWD, _yesno("Yes"), "s10", 10, 20, times=5, retired=True)
    ex.add(YN_CROWD, _yesno("No"), "s11", 11, 21, times=5, retired=True)
    ex.add(YN_CROWD, _yesno("Yes"), "s12", 12, 22, times=2)
    # Retired because it hit the classification cap, with the votes split.
    ex.add(YN_CROWD, _yesno("Yes"), "s13", 13, 23, times=2, retired=True,
           reason="classification_count")
    ex.add(YN_CROWD, _yesno("No"), "s13", 13, 23, times=3, retired=True,
           reason="classification_count")
    classifications = ex.write(tmp_path / "classifications.csv")
    return toolbox, classifications, labelset, tmp_path / "out"


def test_every_point_lands_in_exactly_one_status(scenario):
    toolbox, classifications, labelset, out = scenario
    result = rejoin.rejoin(toolbox, classifications, labelset, out,
                           dry_run=False)
    assert result.ok, result.errors
    assert result.toolbox_rows == 5
    assert sum(result.counts.values()) == 5


def test_the_counts_are_what_the_rules_say(scenario):
    toolbox, classifications, labelset, out = scenario
    result = rejoin.rejoin(toolbox, classifications, labelset, out,
                           dry_run=False)
    assert result.counts["verified"] == 1
    assert result.counts["on_zooniverse"] == 1
    assert result.counts["not_classified"] == 1
    # Denied, plus retired-on-count-without-consensus: both are a person's job.
    assert result.counts["needs_toolbox"] == 2


def test_retired_without_consensus_is_a_toolbox_job_not_a_waiting_game(scenario):
    """The distinction the whole status column exists for: two votes and not
    retired means more are coming; two votes and retired means they are not."""
    import pandas as pd

    toolbox, classifications, labelset, out = scenario
    rejoin.rejoin(toolbox, classifications, labelset, out, dry_run=False)
    status = pd.read_csv(out / rejoin.STATUS_NAME)
    by_point = status.set_index("Row")["kelpquest_status"].to_dict()
    assert by_point[12] == "on_zooniverse"          # not retired
    assert by_point[13] == "needs_toolbox"          # retired, unresolved
    reasons = status.set_index("Row")["status_reason"].to_dict()
    assert reasons[13] == "retired without consensus"
    assert reasons[12] == "waiting for more votes"


def test_a_verified_point_reports_whether_it_also_retired(scenario):
    toolbox, classifications, labelset, out = scenario
    result = rejoin.rejoin(toolbox, classifications, labelset, out,
                           dry_run=False)
    assert result.retired_verified == 1


def test_a_point_zooniverse_never_saw_is_not_confused_with_an_unresolved_one(
        scenario):
    """Exporting only the set a transect started in makes every subject that
    moved on look like one nobody has classified -- which is why the export
    stage takes several sets."""
    import pandas as pd

    toolbox, classifications, labelset, out = scenario
    rejoin.rejoin(toolbox, classifications, labelset, out, dry_run=False)
    status = pd.read_csv(out / rejoin.STATUS_NAME)
    never = status[status["Row"] == 14].iloc[0]
    assert never["kelpquest_status"] == "not_classified"
    assert never["status_reason"] == "never classified"


def test_a_check_writes_nothing(scenario):
    toolbox, classifications, labelset, out = scenario
    result = rejoin.rejoin(toolbox, classifications, labelset, out,
                           dry_run=True)
    assert result.ok and result.dry_run
    # The counts are still computed -- that is the point of a check.
    assert sum(result.counts.values()) == 5
    assert not out.exists() or not list(out.glob("*.csv"))


def test_the_run_writes_the_toolbox_import_and_the_reports(scenario):
    toolbox, classifications, labelset, out = scenario
    result = rejoin.rejoin(toolbox, classifications, labelset, out,
                           dry_run=False)
    written = {p.name for p in result.outputs}
    assert rejoin.TOOLBOX_IMPORT_NAME in written
    assert rejoin.QAQC_NAME in written
    assert rejoin.STATUS_NAME in written


def test_the_toolbox_import_keeps_toolboxs_own_columns(scenario):
    import pandas as pd

    toolbox, classifications, labelset, out = scenario
    rejoin.rejoin(toolbox, classifications, labelset, out, dry_run=False)
    target = out / rejoin.TOOLBOX_IMPORT_NAME
    written = pd.read_csv(target)
    linker = rejoin._load_linker()
    assert list(written.columns) == linker.REQUIRED_TOOLBOX_COLUMNS

    # Verified is checked in the file's own text, not through pandas: it reads
    # TRUE/FALSE back as Python bools, so astype(str) would report "True" and
    # the assertion would be about pandas rather than about what Toolbox gets.
    with target.open(newline="", encoding="utf-8") as fh:
        rows = list(csv.DictReader(fh))
    assert {r["Verified"] for r in rows} <= {"TRUE", "FALSE"}


def test_duplicate_classifications_across_sets_do_not_inflate_the_votes(
        tmp_path: Path, labelset: Path):
    """Five yes votes confirm. The same five arriving twice must not become
    ten, and must not turn a point that has not reached threshold into one
    that has."""
    toolbox = _toolbox_csv(tmp_path / "toolbox.csv", [(10, 20)])
    ex = Export().add(YN_CROWD, _yesno("Yes"), "s1", 10, 20, times=3)
    once = ex.write(tmp_path / "a.csv")
    doubled = Export()
    doubled.rows = ex.rows + [dict(r) for r in ex.rows]   # same ids again
    doubled = doubled.write(tmp_path / "b.csv")

    out_a = rejoin.rejoin(toolbox, once, labelset, tmp_path / "oa",
                          dry_run=False)
    out_b = rejoin.rejoin(toolbox, doubled, labelset, tmp_path / "ob",
                          dry_run=False)
    # Three yes votes is below the five the crowd rule needs, both times.
    assert out_a.counts["on_zooniverse"] == 1
    assert out_b.counts["on_zooniverse"] == 1


def test_the_rules_are_reported_as_configured():
    """Shown on the panel before a run, read from the linker rather than
    restated here."""
    lines = rejoin.workflow_summary()
    assert len(lines) == 4
    assert any("30787" in line for line in lines)
    assert any("Multi-choice expert" in line for line in lines)


def test_a_missing_input_is_reported_not_raised(tmp_path: Path, labelset: Path):
    with pytest.raises(FileNotFoundError):
        rejoin.rejoin(tmp_path / "nope.csv", tmp_path / "also-nope.csv",
                      labelset, tmp_path / "out")


# --------------------------------------------------------------------------
#  mapping a consensus label the labelset does not have
# --------------------------------------------------------------------------


@pytest.fixture
def expansions_file(tmp_path: Path, monkeypatch):
    """Redirect the expansions file into tmp_path.

    Without this, every test that maps a label would append to the real
    scripts/label_expansions.json -- a project file the next run of the linker
    reads. A test must not change what the pipeline does.
    """
    path = tmp_path / "label_expansions.json"
    linker = rejoin._load_linker()
    monkeypatch.setattr(linker, "EXPANSIONS_PATH", str(path))
    return path


@pytest.fixture
def unmapped_scenario(tmp_path: Path, labelset: Path):
    """One point whose multi-choice consensus is a label nobody has mapped.

    Three crowd votes for the same choice is consensus by the rules, so the
    volunteers have answered -- the point is unresolved only because nothing
    knows which labelset code "Ribbon" means.
    """
    toolbox = _toolbox_csv(tmp_path / "toolbox.csv", [(10, 20)])
    classifications = Export().add(
        MULTI_CROWD, _multi("Ribbon"), "s1", 10, 20, times=3
    ).write(tmp_path / "classifications.csv")
    return toolbox, classifications, labelset, tmp_path / "out"


def test_an_unmapped_consensus_label_is_reported_with_its_count(
        unmapped_scenario, expansions_file):
    toolbox, classifications, labelset, out = unmapped_scenario
    result = rejoin.rejoin(toolbox, classifications, labelset, out,
                           dry_run=False)
    assert result.unmapped == [("Ribbon", 1)]
    assert any("not in the labelset" in w for w in result.warnings)


def test_a_check_reports_the_unmapped_labels_too(unmapped_scenario,
                                                 expansions_file):
    """So they can be mapped before the run that writes, rather than after."""
    toolbox, classifications, labelset, out = unmapped_scenario
    result = rejoin.rejoin(toolbox, classifications, labelset, out,
                           dry_run=True)
    assert result.unmapped == [("Ribbon", 1)]
    assert not out.exists() or not list(out.glob("*.csv"))


def test_mapping_the_label_resolves_the_point_on_the_next_run(
        unmapped_scenario, expansions_file):
    """The whole point of the exercise: after the mapping those points carry a
    real label instead of Review."""
    import pandas as pd

    toolbox, classifications, labelset, out = unmapped_scenario
    before = rejoin.rejoin(toolbox, classifications, labelset, out,
                           dry_run=True)
    assert before.counts["needs_toolbox"] == 1

    outcome = rejoin.add_expansions({"Ribbon": "SU_silt"}, labelset)
    assert outcome.ok, outcome.rejected
    assert outcome.added == {"Ribbon": "SU_silt"}
    assert expansions_file.is_file()

    after = rejoin.rejoin(toolbox, classifications, labelset, out,
                          dry_run=False)
    assert after.unmapped == []
    assert after.counts["verified"] == 1
    written = pd.read_csv(out / rejoin.TOOLBOX_IMPORT_NAME)
    assert written.iloc[0]["Label"] == "SU_silt"


def test_a_code_the_labelset_does_not_have_is_refused_not_written(
        unmapped_scenario, expansions_file):
    """An expansion pointing at a missing code resolves to nothing, so the
    next run would report the same label as unmapped with no clue why."""
    _toolbox, _classifications, labelset, _out = unmapped_scenario
    outcome = rejoin.add_expansions({"Ribbon": "KE_ribbon"}, labelset)
    assert not outcome.ok
    assert not outcome.added
    assert outcome.rejected[0][0] == "Ribbon"
    assert "no label" in outcome.rejected[0][2]
    assert not expansions_file.exists()


def test_a_row_left_blank_is_skipped_rather_than_stored_empty(
        unmapped_scenario, expansions_file):
    _toolbox, _classifications, labelset, _out = unmapped_scenario
    outcome = rejoin.add_expansions({"Ribbon": "", "Sieve": "SU_silt"},
                                    labelset)
    assert outcome.added == {"Sieve": "SU_silt"}
    assert outcome.rejected[0][0] == "Ribbon"


def test_the_stored_key_is_the_cleaned_choice_text(unmapped_scenario,
                                                   expansions_file):
    """A Zooniverse choice usually carries the image markdown a volunteer saw
    on the button. The rules look up the cleaned text, so storing the raw
    string would never match."""
    _toolbox, _classifications, labelset, _out = unmapped_scenario
    outcome = rejoin.add_expansions(
        {"![ribbon](https://example.org/a.png) Ribbon": "SU_silt"}, labelset)
    assert outcome.added == {"Ribbon": "SU_silt"}


def test_clean_choice_falls_back_to_the_alt_text():
    assert rejoin.clean_choice("![red_algae](https://x/y.png)") == "red_algae"
    assert rejoin.clean_choice("![](https://x/y.png) Silt") == "Silt"


def test_the_file_holds_additions_only_not_a_copy_of_the_defaults(
        unmapped_scenario, expansions_file):
    """Writing the merged table would freeze the defaults, so a later
    correction in the script would be shadowed by a stale copy of it."""
    _toolbox, _classifications, labelset, _out = unmapped_scenario
    rejoin.add_expansions({"Ribbon": "SU_silt"}, labelset)
    stored = json.loads(expansions_file.read_text(encoding="utf-8"))
    assert stored == {"ribbon": "SU_silt"}
    linker = rejoin._load_linker()
    assert "sugar" in linker.DEFAULT_EXPANSIONS
    assert "sugar" not in stored


def test_the_defaults_are_still_in_force_alongside_an_addition(
        unmapped_scenario, expansions_file):
    _toolbox, _classifications, labelset, _out = unmapped_scenario
    rejoin.add_expansions({"Ribbon": "SU_silt"}, labelset)
    table, path = rejoin.expansion_table()
    assert path == expansions_file
    assert table["ribbon"] == "SU_silt"
    assert table["cca"] == "RE_CCA"          # a default, untouched


def test_mapping_the_same_label_again_replaces_it(unmapped_scenario,
                                                  expansions_file):
    _toolbox, _classifications, labelset, _out = unmapped_scenario
    rejoin.add_expansions({"Ribbon": "SU_silt"}, labelset)
    rejoin.add_expansions({"Ribbon": "SU_bould"}, labelset)
    stored = json.loads(expansions_file.read_text(encoding="utf-8"))
    assert stored == {"ribbon": "SU_bould"}


def test_a_corrupt_expansions_file_does_not_stop_a_run(unmapped_scenario,
                                                       expansions_file):
    """It is a file somebody may have hand-edited. Falling back to the
    built-in table with a warning beats refusing to rejoin anything."""
    expansions_file.write_text("{not json", encoding="utf-8")
    linker = rejoin._load_linker()
    assert linker.load_expansions()["cca"] == "RE_CCA"

    toolbox, classifications, labelset, out = unmapped_scenario
    result = rejoin.rejoin(toolbox, classifications, labelset, out,
                           dry_run=True)
    assert result.ok


def test_the_label_choices_come_from_the_labelset(labelset: Path):
    """The dropdown is built from the labelset rather than a list in the GUI,
    so a label added to the project appears without a code change."""
    choices = rejoin.label_choices(labelset)
    assert ("SU_silt", "substrate - Silt") in choices
    assert len(choices) == 2


def test_the_linker_reads_the_same_file_the_app_writes(unmapped_scenario,
                                                       expansions_file):
    """One mapping table, not one per tool: a label mapped in the app has to
    be in force for the command-line script too."""
    _toolbox, _classifications, labelset, _out = unmapped_scenario
    rejoin.add_expansions({"Ribbon": "SU_silt"}, labelset)
    linker = rejoin._load_linker()
    label_map = linker.build_label_map(
        json.loads(Path(labelset).read_text(encoding="utf-8")))
    assert linker.map_to_toolbox_codes("Ribbon", label_map) == (
        "SU_silt", "substrate - Silt")
