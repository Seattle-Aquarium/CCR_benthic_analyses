"""
Stage 7: taking a transect's subjects out of circulation.

This stage reaches out and changes somebody else's live project, so what is
pinned here is the restraint rather than the plumbing: that a check sends
nothing, that a workflow holding none of these subjects is left alone, that
the number reported as changed is the number that changed, that every
subject acted on is written down so the undo has a source, and that Stop
leaves a partial run reported as partial instead of reported as done.

The first real run of this stage retired a transect in all six of the
project's workflows and reported 4,100 changed in each -- including two
belonging to entirely different transects, and one where nothing could
change because everything was already retired. Zooniverse accepts a
retirement for a subject a workflow has never held: it writes the row and
returns success. Several of the tests below exist only because of that.
"""

from __future__ import annotations

import csv
import sys
import threading
import types
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from kelpquest import retire

LOG_HEAD = ["subject_id", "filename", "source_id", "transect_id",
            "subject_set_id", "uploaded_at"]


def _upload_log(path: Path, rows, subject_set_id="136817") -> Path:
    """rows: (subject_id, transect_id) pairs, as stage 5 would have left them."""
    with path.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.writer(fh)
        writer.writerow(LOG_HEAD)
        for subject_id, transect_id in rows:
            writer.writerow([subject_id, f"{subject_id}.jpg",
                             f"{subject_id}.jpg", transect_id,
                             subject_set_id, "2026-05-27T10:27:10"])
    return path


class FakeWorkflow:
    """A workflow that holds some subjects and records what it was asked.

    Like the real one, it accepts a retirement for a subject it has never
    held -- that is the behaviour the stage has to work around rather than
    rely on.
    """

    #: workflow id -> (subject set ids, {subject id: retired?})
    world: dict = {}
    made: dict = {}

    def __init__(self, workflow_id):
        self.id = str(workflow_id)
        self.display_name = f"workflow {workflow_id}"
        sets, _held = FakeWorkflow.world.get(self.id, (set(), {}))
        self.links = types.SimpleNamespace(
            subject_sets=[types.SimpleNamespace(id=s) for s in sets])
        self.retired: list[list[str]] = []
        self.unretired: list[list[str]] = []
        self.reasons: list[str] = []
        FakeWorkflow.made[self.id] = self

    @classmethod
    def find(cls, workflow_id):
        return cls(workflow_id)

    # The default is deliberately not "other": the test below is about the
    # reason being passed on purpose, not about a default agreeing by luck.
    def retire_subjects(self, subjects, reason="unset"):
        self.retired.append(list(subjects))
        self.reasons.append(reason)

    def unretire_subjects(self, subjects):
        self.unretired.append(list(subjects))


@pytest.fixture
def world(monkeypatch):
    """A stand-in Zooniverse. `world[wid] = (set ids, {subject: retired?})`."""
    FakeWorkflow.made = {}
    FakeWorkflow.world = {}
    module = types.ModuleType("panoptes_client")
    module.Workflow = FakeWorkflow
    monkeypatch.setitem(sys.modules, "panoptes_client", module)
    monkeypatch.setattr(retire, "connect", lambda *a, **k: "24397")

    def holdings(workflow_id, subject_ids, *a, **k):
        _sets, held = FakeWorkflow.world.get(str(workflow_id), (set(), {}))
        mine = {s for s in subject_ids if s in held}
        return mine, {s for s in mine if held[s]}

    monkeypatch.setattr(retire, "holdings", holdings)
    return FakeWorkflow.world


@pytest.fixture
def log(tmp_path):
    return _upload_log(tmp_path / "upload_log.csv",
                       [(str(100 + i), "FPR_S25_T1") for i in range(250)]
                       + [("900", "SOS_S25_T1")])


# --------------------------------------------------------------------------
#  Which subjects
# --------------------------------------------------------------------------


def test_subjects_come_from_the_upload_log_for_one_transect(log):
    ids, counts = retire.subjects_in(log, "FPR_S25_T1")
    assert len(ids) == 250
    assert "900" not in ids
    assert counts == {"FPR_S25_T1": 250}


def test_no_transect_means_every_transect_in_the_log(log):
    ids, counts = retire.subjects_in(log)
    assert len(ids) == 251
    assert counts == {"FPR_S25_T1": 250, "SOS_S25_T1": 1}


def test_a_subject_logged_twice_is_acted_on_once(tmp_path):
    """Two upload runs into one folder append, so the log can repeat."""
    path = _upload_log(tmp_path / "upload_log.csv",
                       [("1", "T1"), ("2", "T1"), ("1", "T1")])
    ids, _ = retire.subjects_in(path, "T1")
    assert ids == ["1", "2"]


def test_the_log_is_summarised_before_anything_is_chosen(log):
    assert retire.transects_in(log) == {"FPR_S25_T1": 250, "SOS_S25_T1": 1}


def test_the_log_also_says_which_set_they_were_uploaded_into(log):
    assert retire.origin_sets(log, "FPR_S25_T1") == {"136817"}


def test_a_missing_log_says_where_it_should_be(tmp_path):
    with pytest.raises(FileNotFoundError, match="upload_log.csv"):
        retire.subjects_in(tmp_path / "upload_log.csv")


def test_a_log_without_subject_ids_is_refused(tmp_path):
    path = tmp_path / "upload_log.csv"
    path.write_text("filename,transect_id\na.jpg,T1\n", encoding="utf-8")
    with pytest.raises(ValueError, match="no subject_id"):
        retire.subjects_in(path)


# --------------------------------------------------------------------------
#  Restraint
# --------------------------------------------------------------------------


def test_a_check_sends_nothing(world):
    """The whole point of the check box on a page that changes a live project."""
    world["28901"] = ({"136817"}, {"1": False, "2": False, "3": False})
    result = retire.retire(["1", "2", "3"], ["28901"], dry_run=True)
    assert result.ok
    assert result.dry_run
    assert FakeWorkflow.made.get("28901") is None or \
        FakeWorkflow.made["28901"].retired == []


def test_a_workflow_holding_none_of_them_is_left_alone(world):
    """Zooniverse would accept the request. There is nothing there to stop."""
    world["28901"] = ({"136817"}, {"1": False, "2": False})
    world["30787"] = ({"135009"}, {})           # another transect's sets
    result = retire.retire(["1", "2"], ["28901", "30787"], dry_run=False)
    assert "30787" not in FakeWorkflow.made or \
        FakeWorkflow.made["30787"].retired == []
    by_id = {p.workflow_id: p for p in result.plans}
    assert by_id["30787"].members == 0
    assert by_id["30787"].sent == 0
    assert by_id["28901"].sent == 2
    assert "hold none of these subjects" in result.summary()


def test_a_workflow_that_has_already_retired_them_is_not_sent_again(world):
    """The run that prompted all this reported 4,100 changed here. It was 0."""
    world["32022"] = ({"136817"}, {"1": True, "2": True, "3": True})
    result = retire.retire(["1", "2", "3"], ["32022"], dry_run=False)
    assert result.plans[0].already == 3
    assert result.plans[0].sent == 0
    assert result.total_sent == 0
    assert FakeWorkflow.made["32022"].retired == []


def test_only_the_difference_is_sent_and_reported(world):
    world["32023"] = ({"136818"}, {"1": True, "2": False, "3": False})
    result = retire.retire(["1", "2", "3"], ["32023"], dry_run=False)
    plan = result.plans[0]
    assert (plan.members, plan.already, plan.sent) == (3, 1, 2)
    assert FakeWorkflow.made["32023"].retired == [["2", "3"]]


def test_the_check_reports_the_same_numbers_the_run_will_send(world):
    """A check whose numbers the run then contradicts is worse than none."""
    world["32023"] = ({"136818"}, {"1": True, "2": False, "3": False})
    checked = retire.retire(["1", "2", "3"], ["32023"], dry_run=True)
    done = retire.retire(["1", "2", "3"], ["32023"], dry_run=False)
    assert checked.plans[0].todo == done.plans[0].sent == 2


def test_the_origin_set_beats_a_silent_status_endpoint(world):
    """A set just linked, nobody served yet: the log knows, the status may not."""
    world["28901"] = ({"136817"}, {})
    result = retire.retire(["1", "2"], ["28901"], dry_run=False,
                           origin_sets={"136817"})
    assert result.plans[0].members == 2
    assert FakeWorkflow.made["28901"].retired == [["1", "2"]]


def test_asking_anyway_is_possible_but_says_what_it_costs(world):
    world["30787"] = ({"135009"}, {})
    result = retire.retire(["1", "2"], ["30787"], dry_run=False,
                           include_future=True)
    assert FakeWorkflow.made["30787"].retired == [["1", "2"]]
    assert any("hold none of them" in w for w in result.warnings)


def test_a_workflow_that_will_not_say_gets_everything(world, monkeypatch):
    monkeypatch.setattr(retire, "holdings", lambda *a, **k: None)
    world["28901"] = ({"136817"}, {})
    result = retire.retire(["1", "2"], ["28901"], dry_run=False)
    assert result.plans[0].already is None
    assert not result.plans[0].status_read
    assert FakeWorkflow.made["28901"].retired == [["1", "2"]]
    assert "-" in result.summary()


def test_no_workflows_is_refused_rather_than_doing_nothing(world):
    """Silently retiring in none of them would read as success."""
    with pytest.raises(ValueError, match="at least one workflow"):
        retire.retire(["1"], [], dry_run=False)


def test_no_subjects_is_refused(world):
    with pytest.raises(ValueError, match="No subjects"):
        retire.retire([], ["28901"], dry_run=False)


# --------------------------------------------------------------------------
#  The act
# --------------------------------------------------------------------------


def test_every_subject_reaches_every_workflow_that_holds_it(world, tmp_path):
    ids = [str(i) for i in range(250)]
    for wid in ("28901", "29104"):
        world[wid] = ({"136817"}, {s: False for s in ids})
    result = retire.retire(ids, ["28901", "29104"], dry_run=False,
                           log_dir=tmp_path)
    assert result.ok
    for workflow_id in ("28901", "29104"):
        sent = [s for chunk in FakeWorkflow.made[workflow_id].retired
                for s in chunk]
        assert sent == ids


def test_requests_are_chunked_so_stop_and_the_bar_can_move(world):
    """250 subjects is three calls, not one blocking request."""
    ids = [str(i) for i in range(250)]
    world["28901"] = ({"136817"}, {s: False for s in ids})
    retire.retire(ids, ["28901"], dry_run=False)
    sizes = [len(chunk) for chunk in FakeWorkflow.made["28901"].retired]
    assert sizes == [100, 100, 50]


def test_the_reason_given_is_other(world):
    """None of Zooniverse's other reasons happened; claiming one would lie."""
    world["28901"] = ({"136817"}, {"1": False})
    retire.retire(["1"], ["28901"], dry_run=False)
    assert FakeWorkflow.made["28901"].reasons == ["other"]


def test_putting_them_back_only_touches_the_retired_ones(world):
    """Un-retiring a subject that was never retired is not a no-op to report."""
    world["28901"] = ({"136817"}, {"1": True, "2": False, "3": True})
    result = retire.retire(["1", "2", "3"], ["28901"], unretire=True,
                           dry_run=False)
    assert FakeWorkflow.made["28901"].unretired == [["1", "3"]]
    assert FakeWorkflow.made["28901"].retired == []
    assert result.plans[0].sent == 2


def test_what_was_done_is_written_down(world, tmp_path):
    """So the undo has a source, and so there is an answer in six months."""
    for wid in ("28901", "29104"):
        world[wid] = ({"136817"}, {"1": False, "2": False})
    result = retire.retire(["1", "2"], ["28901", "29104"],
                           transect_id="FPR_S25_T1", dry_run=False,
                           log_dir=tmp_path)
    assert result.log_csv == tmp_path / retire.RETIRE_LOG_NAME
    rows = list(csv.DictReader(result.log_csv.open(encoding="utf-8")))
    assert len(rows) == 4
    assert {r["workflow_id"] for r in rows} == {"28901", "29104"}
    assert {r["subject_id"] for r in rows} == {"1", "2"}
    assert all(r["action"] == "retire" for r in rows)
    assert all(r["transect_id"] == "FPR_S25_T1" for r in rows)


def test_the_log_records_what_was_sent_not_what_was_asked(world, tmp_path):
    """A workflow that held nothing must not appear as if it was changed."""
    world["28901"] = ({"136817"}, {"1": False})
    world["30787"] = ({"135009"}, {})
    retire.retire(["1"], ["28901", "30787"], dry_run=False, log_dir=tmp_path)
    rows = list(csv.DictReader(
        (tmp_path / retire.RETIRE_LOG_NAME).open(encoding="utf-8")))
    assert [r["workflow_id"] for r in rows] == ["28901"]


def test_a_second_run_appends_rather_than_replacing(world, tmp_path):
    world["28901"] = ({"136817"}, {"1": False})
    retire.retire(["1"], ["28901"], dry_run=False, log_dir=tmp_path)
    world["28901"] = ({"136817"}, {"1": True})
    retire.retire(["1"], ["28901"], unretire=True, dry_run=False,
                  log_dir=tmp_path)
    rows = list(csv.DictReader(
        (tmp_path / retire.RETIRE_LOG_NAME).open(encoding="utf-8")))
    assert [r["action"] for r in rows] == ["retire", "unretire"]


def test_a_check_writes_no_log(world, tmp_path):
    world["28901"] = ({"136817"}, {"1": False})
    retire.retire(["1"], ["28901"], dry_run=True, log_dir=tmp_path)
    assert not (tmp_path / retire.RETIRE_LOG_NAME).exists()


def test_stop_reports_a_partial_run_as_partial(world, tmp_path):
    """Half-retired and reported as done is the one unrecoverable outcome."""
    ids = [str(i) for i in range(250)]
    world["28901"] = ({"136817"}, {s: False for s in ids})
    cancel = threading.Event()
    original = FakeWorkflow.retire_subjects

    def stop_after_one(self, subjects, reason="unset"):
        original(self, subjects, reason)
        cancel.set()

    FakeWorkflow.retire_subjects = stop_after_one
    try:
        result = retire.retire(ids, ["28901"], dry_run=False,
                               log_dir=tmp_path, cancel=cancel)
    finally:
        FakeWorkflow.retire_subjects = original
    assert result.cancelled
    assert not result.ok
    assert result.total_sent == 100
    # The 100 that did go out are written down, because they are retired.
    rows = list(csv.DictReader(
        (tmp_path / retire.RETIRE_LOG_NAME).open(encoding="utf-8")))
    assert len(rows) == 100


def test_a_failed_workflow_is_named_and_the_others_still_run(world, tmp_path):
    for wid in ("28901", "29104"):
        world[wid] = ({"136817"}, {"1": False})
    original = FakeWorkflow.retire_subjects

    def fail_on_one(self, subjects, reason="unset"):
        if self.id == "28901":
            raise RuntimeError("422 Unprocessable Entity")
        original(self, subjects, reason)

    FakeWorkflow.retire_subjects = fail_on_one
    try:
        result = retire.retire(["1"], ["28901", "29104"], dry_run=False,
                               log_dir=tmp_path)
    finally:
        FakeWorkflow.retire_subjects = original
    assert not result.ok
    assert any("28901" in e and "422" in e for e in result.errors)
    assert {p.workflow_id: p.sent for p in result.plans}["29104"] == 1


def test_workflow_ids_are_parsed_like_the_export_page():
    assert retire.parse_ids("28901, 29104") == ["28901", "29104"]
    assert retire.parse_ids("28901 28901\n29104") == ["28901", "29104"]
    assert retire.parse_ids("not an id") == []
