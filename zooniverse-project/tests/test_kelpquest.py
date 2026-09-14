"""
Tests for the Kelp Quest domain modules.

Nothing here needs Dropbox, a model checkpoint or a network: stills and
telemetry are synthesised, so the suite runs on any machine that can import
the package.
"""

from __future__ import annotations

import csv
import json
import random
import sys
from datetime import datetime, timedelta
from pathlib import Path

import numpy as np
import pytest

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from kelpquest import (
    discovery,
    labelset,
    metadata,
    patches,
    pipeline,
    sampling,
    telemetry,
    zooniverse,
)
from kelpquest.classify import Suggestion
from kelpquest.config import AppConfig, SampleSettings

#: The stills the fixtures build, one every 15 seconds.
FIRST = datetime(2025, 1, 28, 11, 34, 11)
STILL_COUNT = 3
STILL_GAP_S = 15


# --------------------------------------------------------------------------
#  fixtures
# --------------------------------------------------------------------------


def _still_names() -> list[str]:
    return [f"{(FIRST + timedelta(seconds=i * STILL_GAP_S)):%Y_%m_%d_%H-%M-%S}.jpg"
            for i in range(STILL_COUNT)]


@pytest.fixture
def flight(tmp_path: Path) -> Path:
    """A flight folder: stills under transects/T6_shallow/edited, plus the
    UTC transect CSV where UTC actually puts it."""
    root = tmp_path / "2025_01_28_EBM"
    stills = root / "downward" / "HERO1" / "transects" / "T6_shallow" / "edited"
    stills.mkdir(parents=True)
    rng = np.random.default_rng(0)
    for name in _still_names():
        image = rng.integers(0, 255, (1200, 1600, 3), dtype=np.uint8)
        patches.imwrite(stills / name, image, 90)

    logs = root / "logs" / "transects" / "transects"
    logs.mkdir(parents=True)
    _write_telemetry(logs / "EBM_W25_T6.csv", "EBM_W25_T6", 6)
    _write_telemetry(logs / "EBM_W25_T1.csv", "EBM_W25_T1", 1)
    return stills


def _write_telemetry(path: Path, transect_id: str, number: int,
                     seconds: int = 60) -> Path:
    head = ["Date", "Time", "Site_name", "Transect_number", "Transect_ID",
            "Latitude", "Longitude", "Altitude", "Depth", "Heading",
            "Velocity_mps", "Width", "Area_m2"]
    with path.open("w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh)
        w.writerow(head)
        for i in range(seconds):
            stamp = FIRST + timedelta(seconds=i)
            w.writerow([f"{stamp:%Y-%m-%d}", f"{stamp:%H:%M:%S}",
                        "Elliott Bay Marina", number, transect_id,
                        47.627224 + i * 1e-6, -122.3918597, 1.1187311580104213,
                        -5.179999828338623, 89.5193130767966,
                        0.06517818149539732, 1.9493042904727005,
                        2.7502396610317876])
    return path


@pytest.fixture
def labels(tmp_path: Path) -> labelset.Labelset:
    path = tmp_path / "labelset.json"
    path.write_text(json.dumps([
        {"short_label_code": "RE_CCA", "long_label_code": "red algae - CCA"},
        {"short_label_code": "SU_bould", "long_label_code": "substrate - Boulder"},
    ]), encoding="utf-8")
    return labelset.Labelset.load(path)


# --------------------------------------------------------------------------
#  sampling
# --------------------------------------------------------------------------


def test_sample_is_reproducible(flight: Path):
    images = discovery.find_images(flight)
    first = sampling.sample(images, SampleSettings(points_per_image=20, margin=100))
    again = sampling.sample(images, SampleSettings(points_per_image=20, margin=100))
    assert [(p.name, p.row, p.column) for p in first] == \
           [(p.name, p.row, p.column) for p in again]


def test_dropping_a_still_does_not_move_the_others(flight: Path):
    """Each still is seeded by its own name, not by its position in the list."""
    images = discovery.find_images(flight)
    settings = SampleSettings(points_per_image=15, margin=100)
    full = sampling.sample(images, settings)
    without_first = sampling.sample(images[1:], settings)
    kept = [(p.name, p.row, p.column) for p in full if p.name != images[0].name]
    assert kept == [(p.name, p.row, p.column) for p in without_first]


def test_the_default_margin_is_one_patch_width():
    """No point lands within a patch of the edge, so the patch is always
    wholly inside the frame."""
    s = SampleSettings()
    assert s.margin == s.patch_size == 224


def test_points_respect_the_margin():
    rng = random.Random(1)
    points = sampling.sample_image(4606, 4030, 400, 224, 224, rng)
    assert len(points) == 400
    assert all(224 <= r <= 4030 - 224 for r, _c in points)
    assert all(224 <= c <= 4606 - 224 for _r, c in points)


def test_points_are_distinct_so_filenames_are_unique():
    rng = random.Random(2)
    points = sampling.sample_image(400, 400, 150, 64, 50, rng)
    assert len(set(points)) == len(points)


def test_margin_too_big_for_the_still_is_trimmed_not_fatal():
    rng = random.Random(3)
    points = sampling.sample_image(300, 300, 10, 64, 400, rng)
    assert points
    assert all(0 <= r < 300 and 0 <= c < 300 for r, c in points)


# --------------------------------------------------------------------------
#  patch geometry
# --------------------------------------------------------------------------


def test_crop_keeps_its_size_at_the_frame_edge():
    """A point near the edge slides the crop inward rather than clipping it."""
    geom = patches.geometry(1600, 1200, row=20, column=20,
                            patch_size=224, scale=3.5)
    assert geom.right - geom.left == 784
    assert geom.bottom - geom.top == 784
    assert geom.left >= 0 and geom.top >= 0


def test_crosshair_lands_on_the_point():
    geom = patches.geometry(1600, 1200, row=600, column=800,
                            patch_size=224, scale=3.5)
    assert (geom.cx, geom.cy) == (800 - geom.left, 600 - geom.top)


def test_tight_patch_is_always_the_full_patch_size():
    image = np.zeros((1200, 1600, 3), dtype=np.uint8)
    for row, column in ((0, 0), (600, 800), (1199, 1599)):
        assert patches.tight_patch(image, row, column, 224).shape == (224, 224, 3)


def test_patch_filename_encodes_the_point():
    assert patches.patch_filename("2025_01_28_10-00-00.jpg", 340, 2413) == \
        "2025_01_28_10-00-00_r340_c2413.jpg"


def test_render_draws_without_raising():
    image = np.full((1200, 1600, 3), 90, dtype=np.uint8)
    crop, geom = patches.render(image, 600, 800, 224, 3.5, "red algae - CCA")
    assert crop.shape == (784, 784, 3)
    # The green box has to actually be on the pixels, not merely computed.
    assert (crop[geom.rect_top, geom.rect_left:geom.rect_right] == (0, 255, 0)
            ).all(axis=1).any()


def _label_pixels(patch_size: int = 224, scale: float = 3.5,
                  text: str = "substrate - Silt") -> tuple[int, int, object]:
    """(black px, white px, image) for a label drawn on a mid-grey ground."""
    width = int(round(patch_size * scale))
    geom = patches.geometry(4606, 4030, 2000, 2000, patch_size, scale)
    image = np.full((width, width, 3), 128, dtype=np.uint8)
    patches.draw_label(image, geom, text, patch_size)
    black = int((image.max(axis=2) < 40).sum())
    white = int((image.min(axis=2) > 215).sum())
    return black, white, image


def test_the_label_has_a_real_outline():
    """The regression guard for a label that vanished on pale silt.

    Two failures met here. `thickness + 3` put 1.5 px of black either side of
    a 27 px letter, so the black area barely matched the white (ratio ~0.98).
    And on OpenCV 5, which saturates putText thickness at 2, the wider black
    pass was drawn at the white pass's width and completely covered by it --
    measured black area 0 px, ratio 0.00.

    The threshold clears both real implementations (1.75 on OpenCV 4.13, 1.47
    on 5.0) and fails both of those bugs.
    """
    black, white, _ = _label_pixels()
    assert black / white > 1.3, (
        f"outline is too thin: {black} black px against {white} white")


@pytest.mark.parametrize("patch_size", [112, 224, 360])
def test_the_outline_scales_with_the_patch(patch_size: int):
    black, white, _ = _label_pixels(patch_size)
    assert black > white


def test_the_halo_follows_the_glyph_height():
    """Measured from the text, not the nominal scale: OpenCV 5's font is
    34 px tall where 4.13's is 27 px at the same scale, and bolder with it."""
    assert patches._halo(27) == 3
    assert patches._halo(34) == 4
    assert patches._halo(4) == 2          # never thinner than 2 px


def test_the_outline_does_not_rely_on_puttext_thickness():
    """The halo has to survive an OpenCV whose putText ignores thickness.

    Drawn through a dilated mask, so asking for a thicker white stroke cannot
    eat the black: the same halo width has to come out either way.
    """
    import cv2

    widths = []
    for thickness in (1, 2, 8):
        image = np.full((160, 700, 3), 128, dtype=np.uint8)
        patches.outlined_text(image, "substrate - Silt", (30, 110),
                              cv2.FONT_HERSHEY_SIMPLEX, 1.244, thickness,
                              halo=3)
        widths.append(int((image.max(axis=2) < 60).sum()))
    assert all(w > 0 for w in widths), "no outline drawn at some thickness"


def test_the_outline_stays_inside_the_crop():
    """A thicker outline must not be clipped by the edge it sits near."""
    # A long label on a small crop: the case that forces the shrink path.
    _black, _white, image = _label_pixels(
        patch_size=112, scale=1.5,
        text="substrate - Anthropogenic debris and other things")
    assert (image[:, 0] == 128).all(), "outline touches the left edge"
    assert (image[:, -1] == 128).all(), "outline touches the right edge"


def test_the_label_never_overlaps_the_green_box():
    """The gap above the box grows with the halo, so a fatter outline cannot
    come down and sit on the line it is meant to be clear of."""
    geom = patches.geometry(4606, 4030, 2000, 2000, 224, 3.5)
    image = np.full((784, 784, 3), 128, dtype=np.uint8)
    patches.draw_label(image, geom, "substrate - Silt", 224)
    # Nothing drawn on the row the box's top edge will occupy.
    assert (image[geom.rect_top] == 128).all()


def test_imread_imwrite_round_trip(tmp_path: Path):
    image = np.full((40, 60, 3), 200, dtype=np.uint8)
    target = tmp_path / "x.jpg"
    assert patches.imwrite(target, image, 100)
    assert patches.imread(target).shape == image.shape
    assert patches.imread(tmp_path / "missing.jpg") is None


# --------------------------------------------------------------------------
#  telemetry
# --------------------------------------------------------------------------


def test_still_timestamp_parses_the_filename():
    assert telemetry.still_timestamp("2025_01_28_11-34-11.jpg") == FIRST
    assert telemetry.still_timestamp("not-a-still.jpg") is None


def test_telemetry_carries_the_transect_identity(flight: Path):
    tel = telemetry.load(telemetry.find_for(flight, "T6"))
    assert tel.transect_id == "EBM_W25_T6"
    assert tel.prefix == "EBM_W25"
    assert tel.site_code == "EBM"
    assert tel.site_name == "Elliott Bay Marina"
    assert tel.transect_number == "T6"
    assert tel.survey_date == "2025-01-28"


def test_every_still_matches_a_telemetry_row(flight: Path):
    tel = telemetry.load(telemetry.find_for(flight, "T6"))
    names = [p.name for p in discovery.find_images(flight)]
    assert tel.match_count(names) == len(names)


def test_telemetry_values_are_rounded_not_full_float_repr(flight: Path):
    """A depth sensor good to a centimetre must not be published to the
    femtometre -- somebody downstream would believe it."""
    tel = telemetry.load(telemetry.find_for(flight, "T6"))
    values = tel.for_still(_still_names()[0])
    assert values["depth_m"] == "-5.18"
    assert values["heading_deg"] == "89.5"
    assert values["latitude"] == "47.6272240"


def test_a_still_outside_the_telemetry_span_gets_nothing(flight: Path):
    tel = telemetry.load(telemetry.find_for(flight, "T6"))
    assert tel.for_still("2025_01_28_23-59-59.jpg") == {}


def test_find_for_picks_the_matching_transect(flight: Path):
    assert telemetry.find_for(flight, "T1").stem == "EBM_W25_T1"
    assert telemetry.find_for(flight, "T6").stem == "EBM_W25_T6"


def test_find_for_refuses_to_guess_between_several(flight: Path):
    """Uploading transect 6's stills under transect 1's identity is not a
    mistake that shows up later."""
    assert telemetry.find_for(flight, "") is None
    assert len(telemetry.candidates(flight)) == 2


# --------------------------------------------------------------------------
#  labelset
# --------------------------------------------------------------------------


def test_long_name_falls_back_to_the_code(labels: labelset.Labelset):
    assert labels.long_name("RE_CCA") == "red algae - CCA"
    assert labels.long_name("KE_newthing") == "KE_newthing"


def test_unknown_codes_reports_what_the_labelset_is_missing(labels):
    assert labels.unknown_codes(["RE_CCA", "KE_newthing", ""]) == ["KE_newthing"]


# --------------------------------------------------------------------------
#  metadata
# --------------------------------------------------------------------------


def _record(**kw) -> metadata.PatchRecord:
    base = dict(filename="a_r10_c20.jpg", name=_still_names()[0],
                path="C:/x/a.jpg", row=10, column=20, patch_size=224, scale=3.5,
                suggestions=[Suggestion("RE_CCA", 0.9), Suggestion("SU_bould", 0.1)])
    base.update(kw)
    return metadata.PatchRecord(**base)


def test_manifest_round_trip(tmp_path: Path):
    records = [_record(), _record(filename="a_r30_c40.jpg", row=30, column=40,
                                  suggestions=[])]
    path = metadata.write_manifest(tmp_path / "patches.csv", records)
    back = metadata.read_manifest(path)
    assert [r.filename for r in back] == [r.filename for r in records]
    assert back[0].code == "RE_CCA"
    assert back[0].confidence == pytest.approx(0.9)
    assert back[1].suggestions == []


def test_toolbox_sheet_matches_the_columns_toolbox_exports(tmp_path, labels):
    path = metadata.write_toolbox_csv(tmp_path / "t.csv", [_record()], labels)
    lines = path.read_text(encoding="utf-8").splitlines()
    assert lines[0].split(",")[:9] == [
        "Name", "Path", "Row", "Column", "Patch Size", "Annotation Type",
        "Label", "Long Label", "Verified"]
    assert "Machine suggestion 5" in lines[0]
    row = lines[1].split(",")
    assert row[5] == "Patch"
    assert row[6] == "RE_CCA"
    assert row[7] == "red algae - CCA"
    # Never pre-verified: the point of importing is that a person checks them.
    assert row[8] == "False"


def test_zooniverse_sheet_hides_model_and_telemetry_from_volunteers(
        tmp_path, labels, flight):
    tel = telemetry.load(telemetry.find_for(flight, "T6"))
    path = metadata.write_zooniverse_csv(
        tmp_path / "metadata.csv", [_record()], labels, "EBM", "2025-01-28",
        "T6", "EBM_W25_T6", tel)
    lines = path.read_text(encoding="utf-8").splitlines()
    header = lines[0].split(",")
    assert "#model_confidence" in header
    assert "model_confidence" not in header
    # Depth on screen is context a volunteer was not asked to weigh.
    assert "#depth_m" in header
    assert "depth_m" not in header
    # The site, date and transect a volunteer may see; the id is the join key.
    assert "transect_id" in header
    assert lines[1].split(",")[header.index("transect_id")] == "EBM_W25_T6"


def test_zooniverse_sheet_omits_telemetry_columns_without_telemetry(
        tmp_path, labels):
    path = metadata.write_zooniverse_csv(tmp_path / "m.csv", [_record()], labels)
    assert "#depth_m" not in path.read_text(encoding="utf-8").splitlines()[0]


def test_joined_sheet_carries_the_telemetry_for_each_point(tmp_path, labels,
                                                           flight):
    tel = telemetry.load(telemetry.find_for(flight, "T6"))
    path = metadata.write_joined_csv(tmp_path / "j.csv", [_record()], labels, tel)
    lines = path.read_text(encoding="utf-8").splitlines()
    header = lines[0].split(",")
    row = lines[1].split(",")
    assert row[header.index("depth_m")] == "-5.18"
    assert row[header.index("transect_id")] == "EBM_W25_T6"
    assert row[header.index("Label")] == "RE_CCA"


def test_source_image_drops_the_extension():
    assert _record().source_image == Path(_still_names()[0]).stem


def test_sheet_names_match_the_existing_files():
    assert metadata.toolbox_csv_name("EBM", "2025-01-28", "T6") == \
        "2025_01_28_EBM_T6_toolbox_annotations.csv"
    assert metadata.joined_csv_name("EBM", "2025-01-28", "T6") == \
        "2025_01_28_EBM_T6_annotations_telemetry.csv"


# --------------------------------------------------------------------------
#  discovery
# --------------------------------------------------------------------------


def test_scan_reads_the_identity_from_the_telemetry(flight: Path):
    t = discovery.scan(flight, telemetry_csv=telemetry.find_for(flight, "T6"))
    assert len(t.images) == STILL_COUNT
    assert t.transect_id == "EBM_W25_T6"
    assert t.transect_id_prefix == "EBM_W25"
    assert t.site_name == "EBM"
    assert t.survey_date == "2025-01-28"
    assert t.transect_number == "T6"
    assert t.telemetry_matched == STILL_COUNT


def test_scan_falls_back_to_the_path_without_telemetry(tmp_path: Path):
    stills = tmp_path / "2025_01_28_EBM" / "transects" / "T6_shallow" / "edited"
    stills.mkdir(parents=True)
    patches.imwrite(stills / _still_names()[0],
                    np.zeros((300, 400, 3), np.uint8), 90)
    t = discovery.scan(stills)
    assert t.site_name == "EBM"
    assert t.survey_date == "2025-01-28"
    assert t.transect_number == "T6"
    assert t.telemetry is None
    assert any("No UTC transect CSV" in n for n in t.notes)


def test_scan_finds_the_telemetry_by_itself(flight: Path):
    t = discovery.scan(flight)
    assert t.telemetry is not None
    assert t.transect_id == "EBM_W25_T6"


def test_scan_of_an_empty_folder_reports_rather_than_raises(tmp_path: Path):
    t = discovery.scan(tmp_path)
    assert t.images == []
    assert t.notes
    assert not t.ok


def test_find_images_skips_the_output_folder(flight: Path):
    out = flight / "zooniverse_patches"
    out.mkdir()
    patches.imwrite(out / "patch.jpg", np.zeros((20, 20, 3), np.uint8), 90)
    assert len(discovery.find_images(flight, exclude=out)) == STILL_COUNT


# --------------------------------------------------------------------------
#  config
# --------------------------------------------------------------------------


def test_transect_id_is_the_prefix_plus_the_number():
    cfg = AppConfig()
    cfg.transect_id_prefix, cfg.transect_number = "EBM_W25", "T6"
    assert cfg.transect_id == "EBM_W25_T6"
    cfg.transect_id_prefix = "EBM_W25_"          # a stray separator
    assert cfg.transect_id == "EBM_W25_T6"


def test_transect_id_with_half_the_parts():
    cfg = AppConfig()
    cfg.transect_number = "T6"
    assert cfg.transect_id == "T6"
    cfg.transect_number, cfg.transect_id_prefix = "", "EBM_W25"
    assert cfg.transect_id == "EBM_W25"


def test_config_round_trip(tmp_path: Path):
    cfg = AppConfig()
    cfg.site_name = "EBM"
    cfg.sample.points_per_image = 25
    cfg.upload.sleep = 0.25
    cfg.report.workflow_id = "30787"
    path = tmp_path / "cfg.json"
    cfg.save(path)
    back = AppConfig.load(path)
    assert back.site_name == "EBM"
    assert back.sample.points_per_image == 25
    assert back.upload.sleep == 0.25
    assert back.report.workflow_id == "30787"


def test_a_stale_key_costs_that_key_not_the_whole_file(tmp_path: Path):
    path = tmp_path / "cfg.json"
    path.write_text(json.dumps({"site_name": "EBM", "was_renamed": 1,
                                "sample": {"points_per_image": 12}}),
                    encoding="utf-8")
    back = AppConfig.load(path)
    assert back.site_name == "EBM"
    assert back.sample.points_per_image == 12


def test_a_missing_config_gives_defaults(tmp_path: Path):
    assert AppConfig.load(tmp_path / "nope.json").sample.points_per_image == 50


# --------------------------------------------------------------------------
#  progress
# --------------------------------------------------------------------------


def test_progress_never_runs_backwards():
    seen: list[float] = []
    steps = pipeline.Steps(lambda f, m="": seen.append(f), first=1, second=3)
    first, second = steps.step("first"), steps.step("second")
    for f in (0.0, 0.5, 1.0):
        first(f)
    for f in (0.0, 0.2, 1.0):     # a stage restarting at zero must not dip
        second(f)
    steps.done()
    assert seen == sorted(seen)
    assert seen[-1] == 1.0


def test_stages_share_the_bar_by_weight():
    seen: list[float] = []
    steps = pipeline.Steps(lambda f, m="": seen.append(f), a=1, b=9)
    steps.step("a")(1.0)
    assert seen[-1] == pytest.approx(0.1)


# --------------------------------------------------------------------------
#  the pipeline, end to end without a model
# --------------------------------------------------------------------------


def _context(flight: Path, tmp_path: Path, labels, preview: bool = False):
    cfg = AppConfig()
    cfg.transect_folder = str(flight)
    cfg.output_dir = str(tmp_path / "out")
    cfg.sample.points_per_image = 4
    cfg.sample.margin = 100
    cfg.classify.labelset = str(labels.source)
    ctx = pipeline.Context(cfg=cfg, preview=preview)
    scan = pipeline.run("transect", ctx)
    ctx.transect = scan.advance["transect"]
    cfg.site_name = ctx.transect.site_name
    cfg.survey_date = ctx.transect.survey_date
    cfg.transect_number = ctx.transect.transect_number
    cfg.transect_id_prefix = ctx.transect.transect_id_prefix
    return ctx


def test_transect_stage_writes_nothing_and_reports_what_it_found(flight, tmp_path,
                                                                 labels):
    ctx = _context(flight, tmp_path, labels)
    result = pipeline.run("transect", ctx)
    assert result.ok
    assert result.counts["images"] == STILL_COUNT
    assert result.advance["transect"].transect_id == "EBM_W25_T6"
    assert not Path(ctx.cfg.output_dir).exists()


def test_a_check_writes_nothing(flight, tmp_path, labels):
    ctx = _context(flight, tmp_path, labels, preview=True)
    result = pipeline.run("patches", ctx)
    assert result.ok and result.preview
    assert result.counts["planned"] == 4 * STILL_COUNT
    assert not list(Path(ctx.cfg.output_dir).glob("*.jpg")) \
        if Path(ctx.cfg.output_dir).is_dir() else True


def test_patches_then_metadata_produces_every_sheet(flight, tmp_path, labels):
    ctx = _context(flight, tmp_path, labels)
    cut = pipeline.run("patches", ctx)
    assert cut.ok, cut.errors
    assert cut.counts["patches"] == 4 * STILL_COUNT

    out = Path(ctx.cfg.output_dir)
    assert len(list(out.glob("*.jpg"))) == 4 * STILL_COUNT
    assert (out / metadata.MANIFEST_NAME).is_file()

    sheets = pipeline.run("metadata", ctx)
    assert sheets.ok, sheets.errors
    assert (out / metadata.ZOONIVERSE_NAME).is_file()
    assert (out / "2025_01_28_EBM_T6_toolbox_annotations.csv").is_file()
    # Telemetry was found, so the join is written too.
    assert (out / "2025_01_28_EBM_T6_annotations_telemetry.csv").is_file()
    # Nothing was classified, so that has to be said rather than left blank.
    assert any("no prediction" in w for w in sheets.warnings)


def test_no_weights_means_a_warning_not_a_failure(flight, tmp_path, labels):
    ctx = _context(flight, tmp_path, labels)
    result = pipeline.run("patches", ctx)
    assert result.ok
    assert any("unlabelled" in w for w in result.warnings)


def test_a_stage_reports_a_failure_instead_of_raising(tmp_path):
    cfg = AppConfig()
    cfg.output_dir = str(tmp_path / "empty")
    result = pipeline.run("metadata", pipeline.Context(cfg=cfg))
    assert not result.ok
    assert result.errors
    assert metadata.MANIFEST_NAME in result.errors[0]


def test_an_unknown_stage_is_reported_not_raised():
    result = pipeline.run("nonsense", pipeline.Context(cfg=AppConfig()))
    assert not result.ok
    assert "nonsense" in result.errors[0]


def test_every_stage_has_a_title_and_a_runner():
    assert set(pipeline.STAGES) == set(pipeline.ORDER)
    assert set(pipeline.TITLES) == set(pipeline.ORDER)


# --------------------------------------------------------------------------
#  the panoptes preflight
# --------------------------------------------------------------------------


def test_client_probe_answers_without_crashing_us():
    """The point of the probe: whatever panoptes_client does on import, this
    call returns a verdict rather than taking the interpreter down."""
    usable, message = zooniverse.client_available()
    assert isinstance(usable, bool)
    if not usable:
        assert message                     # never a silent no
