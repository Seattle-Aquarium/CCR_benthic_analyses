"""
Stage 6: not downloading the same 200 MB twice.

The multiple-choice and expert subject sets are shared by the whole project,
so working through transects one at a time their exports are the same file
every time, and Zooniverse takes minutes to build the big one. Only the
transect's own yes/no set is new. What is tested here is that reuse picks the
right file, that it can be overridden per set, and that it says how stale the
file is -- because reuse trades a correct answer for a fast one, and the
operator has to be able to see the trade.

Zooniverse is not called: `inspect` and `_fetch_one` are the whole of the
boundary and both are replaced.
"""

from __future__ import annotations

import csv
import sys
import time
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from kelpquest import export

#: What each fake subject set is called on Zooniverse.
NAMES = {
    "136807": "2025_10_08_FPR_T1",
    "136818": "Multiple Choice - Part 2",
    "137142": "Yes_No - Expert review - Part 2",
    "137143": "Multiple Choice - Expert review - Part 2",
}


def _csv_bytes(first_id: int, rows: int = 3) -> bytes:
    head = "classification_id,workflow_id,subject_ids,annotations\n"
    body = "".join(f"{first_id + i},30752,s{i},[]\n" for i in range(rows))
    return (head + body).encode("utf-8")


@pytest.fixture
def fake_zooniverse(monkeypatch):
    """Stand in for the two calls that leave the machine.

    `asked` records which sets were actually downloaded, which is the thing
    every test here is really asserting about.
    """
    asked: list[str] = []

    def inspect(set_id):
        return NAMES.get(str(set_id), f"set {set_id}"), 100

    def fetch(set_id, target, result):
        asked.append(str(set_id))
        return _csv_bytes(1000 * (int(set_id) % 10), rows=3)

    monkeypatch.setattr(export, "inspect", inspect)
    monkeypatch.setattr(export, "_fetch_one", fetch)
    return asked


@pytest.fixture
def folder(tmp_path: Path) -> Path:
    out = tmp_path / "exports"
    out.mkdir()
    return out


def _preexisting(folder: Path, set_id: str, days_old: float = 0.0) -> Path:
    """An export sitting in the folder from an earlier session.

    Written under the name the app itself would have used, and *not* recorded
    in the log -- which is the state the exports downloaded before the log
    existed are in.
    """
    path = folder / export.default_csv_name(set_id, NAMES[set_id])
    path.write_bytes(_csv_bytes(500, rows=2))
    if days_old:
        when = time.time() - days_old * 86400
        import os

        os.utime(path, (when, when))
    return path


# --------------------------------------------------------------------------
#  finding what is already there
# --------------------------------------------------------------------------


def test_nothing_on_disk_is_not_an_error(folder):
    assert export.find_existing(folder, "136818") is None
    assert export.read_log(folder) == []


def test_a_recorded_download_is_found_by_its_subject_set_id(folder):
    path = folder / "whatever_the_name_was.csv"
    path.write_bytes(_csv_bytes(1))
    export.record(folder, "136818", NAMES["136818"], path)
    assert export.find_existing(folder, "136818") == path


def test_an_export_from_before_the_log_is_still_found_by_its_filename(folder):
    """The set id is not in the filename but the sanitised set name is, and
    those are unique within a project. Without this fallback, the exports
    already on disk would all be downloaded again once."""
    path = _preexisting(folder, "136818")
    assert export.read_log(folder) == []
    assert export.find_existing(folder, "136818", NAMES["136818"]) == path
    # Without the name there is nothing to match on, so it says so rather
    # than guessing.
    assert export.find_existing(folder, "136818") is None


def test_the_newest_recorded_download_wins(folder):
    old = folder / "old.csv"
    new = folder / "new.csv"
    for p in (old, new):
        p.write_bytes(_csv_bytes(1))
    export.record(folder, "136818", NAMES["136818"], old)
    export.record(folder, "136818", NAMES["136818"], new)
    assert export.find_existing(folder, "136818") == new


def test_a_recorded_file_that_has_been_deleted_is_skipped(folder):
    gone = folder / "gone.csv"
    gone.write_bytes(_csv_bytes(1))
    export.record(folder, "136818", NAMES["136818"], gone)
    gone.unlink()
    assert export.find_existing(folder, "136818") is None


def test_the_age_is_said_in_the_terms_the_decision_is_made_in(folder):
    today = _preexisting(folder, "136818")
    assert export.describe_age(today).startswith("today")
    old = _preexisting(folder, "137142", days_old=4)
    assert "4 days old" in export.describe_age(old)


# --------------------------------------------------------------------------
#  reuse
# --------------------------------------------------------------------------


def test_without_reuse_every_set_is_downloaded(folder, fake_zooniverse):
    _preexisting(folder, "136818")
    result = export.export(["136807", "136818"], folder, "combined.csv",
                           dry_run=False)
    assert result.ok, result.errors
    assert fake_zooniverse == ["136807", "136818"]
    assert not result.reused


def test_reuse_downloads_only_the_set_that_is_new(folder, fake_zooniverse):
    """The transect's own yes/no set. The three shared ones are on disk."""
    for shared in ("136818", "137142", "137143"):
        _preexisting(folder, shared)
    result = export.export(list(NAMES), folder, "combined.csv",
                           reuse_existing=True, dry_run=False)
    assert result.ok, result.errors
    assert fake_zooniverse == ["136807"]
    assert set(result.reused) == {"136818", "137142", "137143"}
    assert result.generated == ["136807"]


def test_the_combine_still_covers_every_set(folder, fake_zooniverse):
    """Stage 7 reads the combined CSV, and it has to hold the reused exports
    as well as the new one -- that is the whole point of reusing them."""
    import pandas as pd

    _preexisting(folder, "136818")
    result = export.export(["136807", "136818"], folder, "combined.csv",
                           reuse_existing=True, dry_run=False)
    assert result.combined_csv is not None
    assert len(result.per_set_csv) == 2
    joined = pd.read_csv(result.combined_csv)
    # Three rows from the download, two from the file already there.
    assert len(joined) == 5


def test_a_named_set_is_re_downloaded_even_so(folder, fake_zooniverse):
    """For a set that has collected votes since it was last exported."""
    for shared in ("136818", "137142"):
        _preexisting(folder, shared)
    result = export.export(["136807", "136818", "137142"], folder,
                           "combined.csv", reuse_existing=True,
                           refresh_ids="136818", dry_run=False)
    assert result.ok, result.errors
    assert sorted(fake_zooniverse) == ["136807", "136818"]
    assert list(result.reused) == ["137142"]


def test_reuse_says_how_stale_the_oldest_file_is(folder, fake_zooniverse):
    """Reuse trades a correct answer for a fast one. A point that reached
    consensus since the file was downloaded reads as still being classified,
    so the age has to be on screen."""
    _preexisting(folder, "136818", days_old=6)
    result = export.export(["136807", "136818"], folder, "combined.csv",
                           reuse_existing=True, dry_run=True)
    assert result.warnings
    assert "6 days old" in result.warnings[0]
    assert "not in them" in result.warnings[0]


def test_a_check_says_which_sets_it_would_download(folder, fake_zooniverse):
    _preexisting(folder, "136818")
    result = export.export(["136807", "136818"], folder, "combined.csv",
                           reuse_existing=True, dry_run=True)
    assert result.ok
    assert not fake_zooniverse                       # nothing was downloaded
    text = "\n".join(result.lines)
    assert "136807" in text and "generate" in text
    assert "reuse" in text
    assert "1 reused from the folder, 1 to generate." in result.lines


def test_reusing_records_the_file_so_the_next_run_matches_by_id(
        folder, fake_zooniverse):
    """Filename matching is the fallback; once a file has been reused it is
    in the log and the name no longer matters."""
    _preexisting(folder, "136818")
    export.export(["136818"], folder, "combined.csv", reuse_existing=True,
                  dry_run=False)
    assert export.find_existing(folder, "136818") is not None
    rows = export.read_log(folder)
    assert [r["subject_set_id"] for r in rows] == ["136818"]


def test_reuse_does_not_pile_up_duplicate_log_rows(folder, fake_zooniverse):
    _preexisting(folder, "136818")
    for _ in range(3):
        export.export(["136818"], folder, "combined.csv",
                      reuse_existing=True, dry_run=False)
    assert len(export.read_log(folder)) == 1


def test_a_download_is_recorded_with_the_set_it_belongs_to(folder,
                                                           fake_zooniverse):
    export.export(["136807"], folder, "combined.csv", dry_run=False)
    rows = export.read_log(folder)
    assert len(rows) == 1
    assert rows[0]["subject_set_id"] == "136807"
    assert rows[0]["subject_set_name"] == NAMES["136807"]
    assert (folder / rows[0]["file"]).is_file()
    assert int(rows[0]["bytes"]) > 0


def test_a_log_that_cannot_be_parsed_does_not_stop_a_run(folder,
                                                          fake_zooniverse):
    (folder / export.EXPORT_LOG_NAME).write_bytes(b"\x00\x01 not a csv")
    result = export.export(["136807"], folder, "combined.csv", dry_run=False)
    assert result.ok, result.errors


# --------------------------------------------------------------------------
#  the combined file
# --------------------------------------------------------------------------


def test_the_combined_file_refuses_to_overwrite_one_of_its_inputs(folder):
    """Reachable only with reuse: a per-set export in the folder could be
    named the same as the combined file, and reading a file while writing it
    truncates it to nothing."""
    a = folder / "one.csv"
    a.write_bytes(_csv_bytes(1))
    with pytest.raises(ValueError, match="own name"):
        export.combine([a], a)


def test_two_transects_do_not_share_a_combined_filename(folder,
                                                        fake_zooniverse):
    """Each transect's combined export is the file its stage 7 reads, so the
    second transect must not land on the first one's name by default."""
    first = export.export(["136807"], folder, "FPR_S25_T1.csv", dry_run=False)
    second = export.export(["136807"], folder, "FPR_S25_T2.csv", dry_run=False)
    assert first.combined_csv != second.combined_csv
    assert first.combined_csv.is_file() and second.combined_csv.is_file()


# --------------------------------------------------------------------------
#  what is in an export
# --------------------------------------------------------------------------


def test_the_transects_in_an_export_can_be_listed(tmp_path: Path):
    """An export of the shared sets holds whichever transects happened to be
    in them, which is not something anybody can recall."""
    import json

    from kelpquest import report

    path = tmp_path / "export.csv"
    rows = [("s1", "EBM_W25_T6"), ("s1", "EBM_W25_T6"), ("s2", "EBM_W25_T6"),
            ("s3", "PKB_S25_T1"), ("s4", "")]
    with path.open("w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh)
        w.writerow(["classification_id", "subject_ids", "subject_data"])
        for i, (subject, transect) in enumerate(rows):
            meta = {"source_image": "still", "row": 1, "column": 2}
            if transect:
                meta["transect_id"] = transect
            w.writerow([i, subject, json.dumps({subject: meta})])

    found = report.transects_in(path)
    assert found[0] == ("EBM_W25_T6", 3, 2)
    assert ("PKB_S25_T1", 1, 1) in found
    # The untagged rows are reported as such rather than dropped: they are
    # exactly the ones a filter cannot see.
    assert ("", 1, 1) in found
