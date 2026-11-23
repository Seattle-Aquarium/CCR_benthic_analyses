"""
Stage 9: points into cover, joined to the second of telemetry.

The arithmetic is somebody's percent cover for a paper, so what is pinned
here is the arithmetic rather than the plumbing: that each image is divided
by its own point total, that several files are concatenated before grouping
rather than after, that an image the telemetry misses is kept and counted
instead of vanishing, and that leaving undecided points in the denominator is
visible rather than silent.
"""

from __future__ import annotations

import csv
import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

pytest.importorskip("pandas")

from kelpquest import percent_cover

ANNOTATION_HEAD = ["Name", "Row", "Column", "Label", "Long Label"]
TELEMETRY_HEAD = ["Date", "Time", "Depth", "Latitude", "Longitude", "Area_m2"]


def _annotations(path: Path, rows) -> Path:
    """rows: (image name, label) pairs, one per annotated point."""
    with path.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.writer(fh)
        writer.writerow(ANNOTATION_HEAD)
        for i, (name, label) in enumerate(rows):
            writer.writerow([name, i, i, label, f"long {label}"])
    return path


def _telemetry(path: Path, seconds) -> Path:
    """seconds: (date, time) pairs, one row each."""
    with path.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.writer(fh)
        writer.writerow(TELEMETRY_HEAD)
        for i, (date, time) in enumerate(seconds):
            writer.writerow([date, time, -8.0 - i, 47.6 + i / 1e4,
                             -122.3 - i / 1e4, 1.5])
    return path


def _still(second: int) -> str:
    return f"2024_10_08_10-07-{second:02d}.jpg"


@pytest.fixture
def one_transect(tmp_path: Path):
    """Three images: 4 points, 4 points, and 2 — the uneven one matters."""
    rows = (
        [(_still(1), lab) for lab in ("KE_sugar", "KE_sugar", "SU_peb",
                                      "GR_ulva")]
        + [(_still(2), lab) for lab in ("SU_peb", "SU_peb", "SU_peb",
                                        "GR_ulva")]
        + [(_still(3), lab) for lab in ("GR_ulva", "GR_ulva")]
    )
    annotations = _annotations(tmp_path / "annotations.csv", rows)
    telemetry = _telemetry(tmp_path / "telemetry.csv",
                           [("2024-10-08", f"10:07:0{s}") for s in (1, 2, 3)])
    return annotations, telemetry, tmp_path


def _read(path: Path):
    import pandas as pd

    return pd.read_csv(path)


# --------------------------------------------------------------------------
#  the arithmetic
# --------------------------------------------------------------------------


def test_every_image_sums_to_one(one_transect):
    annotations, telemetry, tmp = one_transect
    out = tmp / "cover.csv"
    result = percent_cover.build([annotations], [telemetry], out,
                                 dry_run=False)
    assert result.ok, result.errors
    table = _read(out)
    sums = table[result.labels].sum(axis=1)
    assert sums.round(9).tolist() == [1.0, 1.0, 1.0]


def test_each_image_is_divided_by_its_own_point_total(one_transect):
    """Not by 50. The third image has two points, so one of them is half of
    it -- dividing by a fixed 50 would call it 2%."""
    annotations, telemetry, tmp = one_transect
    out = tmp / "cover.csv"
    percent_cover.build([annotations], [telemetry], out, dry_run=False)
    table = _read(out).set_index("Name")
    assert table.loc[_still(1), "KE_sugar"] == pytest.approx(0.5)   # 2 of 4
    assert table.loc[_still(2), "SU_peb"] == pytest.approx(0.75)    # 3 of 4
    assert table.loc[_still(3), "GR_ulva"] == pytest.approx(1.0)    # 2 of 2


def test_a_label_absent_from_an_image_is_zero_not_missing(one_transect):
    annotations, telemetry, tmp = one_transect
    out = tmp / "cover.csv"
    percent_cover.build([annotations], [telemetry], out, dry_run=False)
    table = _read(out).set_index("Name")
    assert table.loc[_still(3), "KE_sugar"] == 0.0
    assert not table[["KE_sugar", "SU_peb", "GR_ulva"]].isna().any().any()


def test_several_files_are_concatenated_before_grouping(tmp_path: Path):
    """A site-season table is several transects in one file. Grouping each
    file separately and stacking the results would give one row per image per
    file, and double-count any image that appears in two."""
    a = _annotations(tmp_path / "t1.csv",
                     [(_still(1), "KE_sugar"), (_still(1), "SU_peb")])
    b = _annotations(tmp_path / "t2.csv",
                     [(_still(2), "GR_ulva"), (_still(2), "GR_ulva")])
    telemetry = _telemetry(tmp_path / "tel.csv",
                           [("2024-10-08", "10:07:01"),
                            ("2024-10-08", "10:07:02")])
    out = tmp_path / "cover.csv"
    result = percent_cover.build([a, b], [telemetry], out, dry_run=False)
    assert result.images == 2
    assert result.points == 4
    table = _read(out).set_index("Name")
    assert len(table) == 2
    assert table.loc[_still(1), "KE_sugar"] == pytest.approx(0.5)
    assert table.loc[_still(2), "GR_ulva"] == pytest.approx(1.0)


# --------------------------------------------------------------------------
#  the telemetry join
# --------------------------------------------------------------------------


def test_telemetry_is_matched_on_the_timestamp_in_the_image_name(one_transect):
    annotations, telemetry, tmp = one_transect
    out = tmp / "cover.csv"
    result = percent_cover.build([annotations], [telemetry], out,
                                 dry_run=False)
    assert (result.matched, result.unmatched) == (3, 0)
    table = _read(out).set_index("Name")
    assert table.loc[_still(1), "Depth"] == pytest.approx(-8.0)
    assert table.loc[_still(3), "Depth"] == pytest.approx(-10.0)


def test_an_image_with_no_telemetry_is_kept_and_counted(tmp_path: Path):
    """Dropped, it would silently shrink the transect. Kept, the gap is
    visible in the table and in the warning."""
    annotations = _annotations(tmp_path / "a.csv",
                               [(_still(1), "KE_sugar"), (_still(9), "SU_peb")])
    telemetry = _telemetry(tmp_path / "t.csv", [("2024-10-08", "10:07:01")])
    out = tmp_path / "cover.csv"
    result = percent_cover.build([annotations], [telemetry], out,
                                 dry_run=False)
    assert result.images == 2
    assert (result.matched, result.unmatched) == (1, 1)
    assert any("no telemetry row" in w for w in result.warnings)
    table = _read(out).set_index("Name")
    assert _still(9) in table.index
    assert table.loc[_still(9), "Depth"] != table.loc[_still(9), "Depth"]  # NaN
    # The cover numbers are still right for the unmatched image.
    assert table.loc[_still(9), "SU_peb"] == pytest.approx(1.0)


def test_a_name_with_no_timestamp_is_reported(tmp_path: Path):
    annotations = _annotations(tmp_path / "a.csv",
                               [("not_a_timestamp.jpg", "KE_sugar")])
    telemetry = _telemetry(tmp_path / "t.csv", [("2024-10-08", "10:07:01")])
    result = percent_cover.build([annotations], [telemetry],
                                 tmp_path / "cover.csv", dry_run=False)
    assert result.unparsed_names == 1
    assert any("no YYYY_MM_DD_HH-MM-SS" in w or "timestamp" in w
               for w in result.warnings)


def test_the_telemetry_columns_come_before_the_labels(one_transect):
    """The published tables are Name, telemetry, then labels, and anything
    reading them by position depends on it."""
    annotations, telemetry, tmp = one_transect
    out = tmp / "cover.csv"
    result = percent_cover.build([annotations], [telemetry], out,
                                 dry_run=False)
    columns = list(_read(out).columns)
    assert columns[0] == "Name"
    assert columns[1:1 + len(TELEMETRY_HEAD)] == TELEMETRY_HEAD
    assert set(columns[1 + len(TELEMETRY_HEAD):]) == set(result.labels)


# --------------------------------------------------------------------------
#  undecided points
# --------------------------------------------------------------------------


def test_review_points_are_reported_even_when_they_are_kept(tmp_path: Path):
    """Left in the denominator they make every real class on that image read
    lower, so the count is on screen whether or not anybody acts on it."""
    rows = [(_still(1), "KE_sugar")] * 3 + [(_still(1), "Review")]
    annotations = _annotations(tmp_path / "a.csv", rows)
    telemetry = _telemetry(tmp_path / "t.csv", [("2024-10-08", "10:07:01")])
    result = percent_cover.build([annotations], [telemetry],
                                 tmp_path / "cover.csv", dry_run=False)
    assert result.review_points == 1
    assert result.review_images == {_still(1): 1}
    assert any("still marked 'Review'" in w for w in result.warnings)
    table = _read(tmp_path / "cover.csv").set_index("Name")
    assert table.loc[_still(1), "KE_sugar"] == pytest.approx(0.75)


def test_unknown_is_a_real_category_and_is_always_counted(tmp_path: Path):
    """"Cannot be determined from the image" is a finding about the seafloor.
    Dropping it would inflate every other class on that image."""
    rows = [(_still(1), "KE_sugar")] * 3 + [(_still(1), "unknown")]
    annotations = _annotations(tmp_path / "a.csv", rows)
    telemetry = _telemetry(tmp_path / "t.csv", [("2024-10-08", "10:07:01")])
    out = tmp_path / "cover.csv"
    result = percent_cover.build([annotations], [telemetry], out,
                                 dry_run=False)
    assert result.review_points == 0
    assert not result.warnings
    table = _read(out).set_index("Name")
    assert "unknown" in table.columns
    assert table.loc[_still(1), "unknown"] == pytest.approx(0.25)
    assert table.loc[_still(1), "KE_sugar"] == pytest.approx(0.75)
    # And it cannot be excluded by the Review option.
    excluded = percent_cover.build(
        [annotations], [telemetry], tmp_path / "b.csv",
        exclude_labels=(percent_cover.REVIEW_LABEL,), dry_run=False)
    assert "unknown" in excluded.labels


def test_the_points_still_needing_review_are_listed_for_toolbox(tmp_path: Path):
    """"Label them in Toolbox" needs a list with coordinates, not a count."""
    rows = [(_still(1), "KE_sugar"), (_still(1), "Review"),
            (_still(2), "Review"), (_still(2), "SU_peb")]
    annotations = _annotations(tmp_path / "a.csv", rows)
    telemetry = _telemetry(tmp_path / "t.csv",
                           [("2024-10-08", "10:07:01"),
                            ("2024-10-08", "10:07:02")])
    result = percent_cover.build([annotations], [telemetry],
                                 tmp_path / "cover.csv", dry_run=False)
    assert result.review_points == 2
    assert result.review_csv is not None and result.review_csv.is_file()
    listed = _read(result.review_csv)
    assert set(listed["Name"]) == {_still(1), _still(2)}
    assert {"Name", "Row", "Column"} <= set(listed.columns)
    assert (listed["Label"] == "Review").all()


def test_excluding_them_shares_the_rest_out(tmp_path: Path):
    rows = [(_still(1), "KE_sugar")] * 3 + [(_still(1), "Review")]
    annotations = _annotations(tmp_path / "a.csv", rows)
    telemetry = _telemetry(tmp_path / "t.csv", [("2024-10-08", "10:07:01")])
    out = tmp_path / "cover.csv"
    result = percent_cover.build([annotations], [telemetry], out,
                                 exclude_labels=("Review",), dry_run=False)
    assert result.excluded == ["Review"]
    table = _read(out).set_index("Name")
    assert table.loc[_still(1), "KE_sugar"] == pytest.approx(1.0)
    assert "Review" not in table.columns


def test_excluding_everything_is_refused_rather_than_written(tmp_path: Path):
    annotations = _annotations(tmp_path / "a.csv", [(_still(1), "Review")])
    telemetry = _telemetry(tmp_path / "t.csv", [("2024-10-08", "10:07:01")])
    with pytest.raises(ValueError, match="no points"):
        percent_cover.build([annotations], [telemetry],
                            tmp_path / "cover.csv",
                            exclude_labels=("Review",), dry_run=False)


# --------------------------------------------------------------------------
#  refusing the wrong input
# --------------------------------------------------------------------------


def test_a_percent_cover_table_given_instead_of_annotations_is_refused(
        tmp_path: Path):
    """Easy to do -- both are CSVs with a Name column, and the output of this
    stage lives next to its input."""
    wrong = tmp_path / "already_cover.csv"
    wrong.write_text("Name,KE_sugar,SU_peb\na.jpg,0.5,0.5\n", encoding="utf-8")
    telemetry = _telemetry(tmp_path / "t.csv", [("2024-10-08", "10:07:01")])
    with pytest.raises(ValueError, match="one row per"):
        percent_cover.build([wrong], [telemetry], tmp_path / "out.csv",
                            dry_run=False)


def test_a_missing_file_says_which_one(tmp_path: Path):
    telemetry = _telemetry(tmp_path / "t.csv", [("2024-10-08", "10:07:01")])
    with pytest.raises(FileNotFoundError, match="gone.csv"):
        percent_cover.build([tmp_path / "gone.csv"], [telemetry],
                            tmp_path / "out.csv", dry_run=False)


def test_a_check_writes_nothing(one_transect):
    annotations, telemetry, tmp = one_transect
    out = tmp / "cover.csv"
    result = percent_cover.build([annotations], [telemetry], out,
                                 dry_run=True)
    assert result.ok
    assert result.images == 3
    assert not out.exists()
    assert any("would write" in line for line in result.lines)


def test_the_output_is_named_after_the_transect():
    assert (percent_cover.default_csv_name("CNL_S24_T1")
            == "CNL_S24_T1_percent_cover_telemetry.csv")
    assert percent_cover.default_csv_name("") == \
        "percent_cover_telemetry.csv"


# --------------------------------------------------------------------------
#  Naming: the stage is routinely run out of order
# --------------------------------------------------------------------------


def test_the_name_comes_from_the_annotations_not_from_stage_one():
    """The whole point. Stage 1 may be pointed at a different transect."""
    name, source = percent_cover.suggest_name(
        ["/a/b/FPR_S25_T1.csv"], ["/c/2025_10_08_T1.csv"],
        fallback="SOS_S25_T1")
    assert name == "FPR_S25_T1_percent_cover_telemetry.csv"
    assert "annotation" in source


def test_several_transects_of_one_site_collapse_to_the_site_season():
    """`CNL_S24_T1..T6` is one site-season table, and is named like one."""
    name, _ = percent_cover.suggest_name(
        [f"/x/CNL_S24_T{n}.csv" for n in range(1, 7)])
    assert name == "CNL_S24_percent_cover_telemetry.csv"


def test_files_with_nothing_in_common_fall_through_to_the_telemetry():
    name, source = percent_cover.suggest_name(
        ["/x/alpha.csv", "/x/beta.csv"], ["/y/CNL_S24_T1.csv"])
    assert name == "CNL_S24_T1_percent_cover_telemetry.csv"
    assert "telemetry" in source


def test_the_transect_id_is_the_last_resort_and_says_so():
    name, source = percent_cover.suggest_name(fallback="SOS_S25_T1")
    assert name == "SOS_S25_T1_percent_cover_telemetry.csv"
    assert "stage 1" in source

    name, source = percent_cover.suggest_name()
    assert name == "percent_cover_telemetry.csv"
    assert "choose the annotations" in source
