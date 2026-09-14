"""
That the window builds, and that its layout is the size it should be.

Checked by asking the widgets rather than by photographing the screen. It gives
a number and a cause instead of an impression, and a screen grab captures a
region: if the app is not frontmost at that instant it silently photographs
whatever is, which on a real desktop can be somebody's private browser tab.

Skipped where there is no display, so the suite still runs headless.
"""

from __future__ import annotations

import sys
import tkinter
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

ctk = pytest.importorskip("customtkinter")

#: Panel keys, in rail order. Imported rather than restated so a new stage
#: cannot be added without the smoke test noticing.
from kelpquest.pipeline import ORDER as STAGES  # noqa: E402


@pytest.fixture(scope="module")
def settings_path(tmp_path_factory) -> Path:
    return tmp_path_factory.mktemp("kelpquest") / "settings.json"


@pytest.fixture(scope="module")
def app(settings_path):
    """One window for the whole module, realised and ready to measure.

    Module-scoped on purpose. Creating and tearing down a Tk root per test
    intermittently fails with "tk wasn't installed properly" -- Tk does not
    much like being re-initialised inside one interpreter -- and a flaky skip
    in a smoke test is worse than no smoke test. Nothing here mutates state
    another test depends on.
    """
    import kelpquest.config as config

    with pytest.MonkeyPatch.context() as patch:
        patch.setattr(config, "CONFIG_PATH", settings_path)
        from kelpquest.gui.app import App

        try:
            window = App()
        except tkinter.TclError as exc:
            # Only a real display failure is a skip. Catching Exception here
            # once turned a genuine crash -- a panel reading a config field
            # that had been renamed -- into eleven "no display available"
            # skips, which is a worse outcome than a red test.
            pytest.skip(f"no display available: {exc}")
        for _ in range(8):
            window.update()
        yield window
        window.destroy()


def test_every_panel_builds(app):
    assert list(app.panels) == list(STAGES)


def test_panels_are_laid_out_not_collapsed(app):
    for key in STAGES:
        app.select(key)
        for _ in range(4):
            app.update()
        panel = app.panels[key]
        assert panel.winfo_width() > 200, f"{key} has no width"
        assert panel.winfo_height() > 150, f"{key} has no height"


def test_the_rail_fits_every_row(app):
    """A CTkFrame defaults to 200px in both directions and keeps it with
    propagation off, which once made every rail row 200px tall and pushed the
    last stage off the bottom of the window."""
    bottom = max(b.winfo_y() + b.winfo_height()
                 for b in app.rail._buttons.values())
    assert bottom <= app.rail.winfo_height()


def test_the_rail_shows_status_with_a_mark_not_only_colour(app):
    """Colour is never the only signal."""
    app.rail.set_status("transect", "done")
    app.update()
    assert app.rail.MARKS["done"] in app.rail._buttons["transect"].cget("text")


def test_the_run_button_says_what_it_will_do(app):
    app.select("patches")
    app.check_var.set(True)
    app._check_toggled()
    app.update()
    assert app.run_btn.cget("text") == "Check"
    app.check_var.set(False)
    app._check_toggled()
    app.update()
    assert app.run_btn.cget("text") == app.panels["patches"].run_text


def test_a_read_only_stage_hides_the_check_box(app):
    """Stage 1 only ever reads; offering a check would imply Run writes."""
    app.select("transect")
    app.update()
    assert not app.check_box.winfo_ismapped()
    app.select("patches")
    app.update()
    assert app.check_box.winfo_ismapped()


def test_the_window_fits_the_screen(app):
    """The footer carries Stop and the progress bar; off-screen is unusable."""
    assert app.winfo_rooty() + app.winfo_height() <= app.winfo_screenheight() * 3
    scaling = ctk.ScalingTracker.get_window_scaling(app) or 1.0
    assert app.winfo_height() / scaling <= app.winfo_screenheight()


def test_both_themes_apply(app):
    for wanted in ("light", "dark"):
        app.theme_switch.select() if wanted == "dark" else app.theme_switch.deselect()
        app._toggle_theme()
        app.update()
        assert app.mode == wanted


def test_settings_survive_a_round_trip(app, settings_path, tmp_path):
    panel = app.panels["transect"]
    panel.site.set("EBM")
    panel.number.set("T6")
    panel.prefix.set("EBM_W25")
    panel.output.set(str(tmp_path / "out"))
    app.save_config()

    from kelpquest.config import AppConfig

    back = AppConfig.load(settings_path)
    assert back.site_name == "EBM"
    assert back.transect_number == "T6"
    assert back.transect_id == "EBM_W25_T6"


def test_the_transect_id_is_composed_not_typed_twice(app):
    """The prefix is asked for once; the number is appended."""
    panel = app.panels["transect"]
    panel.prefix.set("EBM_W25")
    panel.number.set("T3")
    app.collect_all()
    assert app.cfg.transect_id == "EBM_W25_T3"
    # And the field says so, rather than leaving it to be worked out.
    assert "EBM_W25_T3" in panel.prefix.status.cget("text")


def test_composing_the_transect_id_does_not_recurse(app):
    """Both halves' change handlers touch the other's note; without a
    notify=False path that pair calls itself until Tk runs out of stack."""
    panel = app.panels["transect"]
    for value in ("EBM_W25", "CNL_S24", "EBM_W25"):
        panel.prefix.set(value)
        panel.number.set("T6")
        app.update()
    assert app.panels["transect"].prefix.get() == "EBM_W25"


def test_the_unmapped_label_card_is_hidden_until_there_are_some(app):
    """An empty 'labels the labelset could not name' card is a puzzle, not
    information."""
    panel = app.panels["rejoin"]
    panel._show_unmapped([])
    app.update()
    assert not panel.unmapped_card.winfo_ismapped()


def test_an_unmapped_label_gets_a_row_and_a_labelset_dropdown(app, tmp_path):
    """The codes come from the labelset, so a label added to the project turns
    up in the dropdown without a code change here."""
    import json

    labelset = tmp_path / "labelset.json"
    labelset.write_text(json.dumps([
        {"short_label_code": "SU_silt", "long_label_code": "substrate - Silt"},
        {"short_label_code": "KE_sugar", "long_label_code": "kelp - Sugar"},
    ]), encoding="utf-8")
    was = app.cfg.classify.labelset
    app.cfg.classify.labelset = str(labelset)
    try:
        panel = app.panels["rejoin"]
        panel._show_unmapped([("![](https://x/y.png) Ribbon", 12)])
        app.update()
        assert panel.unmapped_card.winfo_ismapped()
        assert len(panel._unmapped) == 1
        raw, picker = panel._unmapped[0]
        # The raw choice is kept for the mapping; the row shows it cleaned.
        assert raw == "![](https://x/y.png) Ribbon"
        assert "SU_silt — substrate - Silt" in picker.cget("values")
    finally:
        app.cfg.classify.labelset = was
        app.panels["rejoin"]._show_unmapped([])
        app.update()


def test_mapping_with_nothing_picked_says_so_rather_than_saving(app, tmp_path):
    import json

    labelset = tmp_path / "labelset.json"
    labelset.write_text(json.dumps(
        [{"short_label_code": "SU_silt", "long_label_code": "silt"}]),
        encoding="utf-8")
    was = app.cfg.classify.labelset
    app.cfg.classify.labelset = str(labelset)
    try:
        panel = app.panels["rejoin"]
        panel._show_unmapped([("Ribbon", 3)])
        app.update()
        panel._add_expansions()
        app.update()
        assert "Pick a label" in panel.map_note.cget("text")
    finally:
        app.cfg.classify.labelset = was
        app.panels["rejoin"]._show_unmapped([])
        app.update()


def test_the_report_stage_can_be_filtered_to_one_transect(app):
    """The shared multiple-choice and expert sets carry every transect's
    subjects, so without this a report meant for one summarises all of them."""
    app.panels["transect"].prefix.set("EBM_W25")
    app.panels["transect"].number.set("T6")
    app.select("report")
    app.update()
    panel = app.panels["report"]
    panel.load()
    app.update()
    # Defaults to the transect stage 1 was pointed at rather than being typed
    # a second time.
    assert panel.transect.get() == "EBM_W25_T6"
    app.collect_all()
    assert app.cfg.report.transect_id == "EBM_W25_T6"


def test_the_report_stage_knows_where_the_analysis_tool_is(app):
    """It shipped as analyze_; the docs say analyse_. Stage 8 once reported it
    missing while it sat in the scripts folder."""
    app.select("report")
    app.update()
    assert "missing" not in app.panels["report"].tool.cget("text")


def test_reuse_is_off_until_it_is_asked_for(app):
    """A reused export has none of the votes cast since it was downloaded, so
    the default is the correct answer rather than the fast one."""
    app.select("export")
    app.update()
    panel = app.panels["export"]
    panel.reuse.set(False)
    panel._reuse_toggled()
    app.update()
    assert "generated fresh" in panel.reuse_note.cget("text")


def test_the_re_download_field_only_matters_when_reuse_is_on(app):
    app.select("export")
    app.update()
    panel = app.panels["export"]
    panel.reuse.set(True)
    panel._reuse_toggled()
    app.update()
    assert panel.refresh.entry.cget("state") == "normal"
    panel.reuse.set(False)
    panel._reuse_toggled()
    app.update()
    assert panel.refresh.entry.cget("state") == "disabled"


def test_the_panel_says_which_sets_are_already_downloaded(app, tmp_path):
    """The file's age is the whole decision, so it goes on screen next to
    the choice rather than being left to be looked up."""
    from kelpquest import export

    folder = tmp_path / "exports"
    folder.mkdir()
    existing = folder / "an_export.csv"
    existing.write_text("classification_id\n1\n", encoding="utf-8")
    export.record(folder, "136818", "Multiple Choice - Part 2", existing)

    app.select("export")
    panel = app.panels["export"]
    was_out, was_ids = panel.output.get(), panel.set_ids.get()
    try:
        panel.output.set(str(folder))
        panel.set_ids.set("136807, 136818")
        panel.reuse.set(True)
        panel._reuse_toggled()
        app.update()
        text = panel.reuse_note.cget("text")
        assert "136818" in text and "today" in text
        # And it does not claim the other one will be downloaded: the run
        # matches on the set's name too, which needs Zooniverse.
        assert "may still be reused" in text
    finally:
        panel.output.set(was_out)
        panel.set_ids.set(was_ids)
        panel.reuse.set(False)
        panel._reuse_toggled()


def test_the_export_settings_survive_a_round_trip(app, settings_path):
    app.select("export")
    panel = app.panels["export"]
    panel.reuse.set(True)
    panel.refresh.set("136818")
    app.save_config()
    from kelpquest.config import AppConfig

    back = AppConfig.load(settings_path)
    assert back.export.reuse_existing is True
    assert back.export.refresh_ids == "136818"
    panel.reuse.set(False)
    panel.refresh.set("")


def test_stage_8_lists_the_transects_in_the_export(app, tmp_path):
    """An export of the shared subject sets holds whichever transects happened
    to be in them, which is not something anybody can recall."""
    import csv
    import json

    path = tmp_path / "export.csv"
    with path.open("w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh)
        w.writerow(["classification_id", "subject_ids", "subject_data"])
        for i, transect in enumerate(["EBM_W25_T6", "EBM_W25_T6",
                                      "PKB_S25_T1"]):
            w.writerow([i, f"s{i}", json.dumps(
                {f"s{i}": {"transect_id": transect, "row": 1, "column": 2}})])

    app.select("report")
    panel = app.panels["report"]
    was = panel.export_csv.get()
    try:
        panel.export_csv.set(str(path))
        panel._list_transects()
        app.update()
        shown = panel.transects_box.get("1.0", "end")
        assert "EBM_W25_T6" in shown and "PKB_S25_T1" in shown
        # Both, comma-separated, ready to paste into the filter.
        assert "EBM_W25_T6, PKB_S25_T1" in shown
    finally:
        panel.export_csv.set(was)


def test_the_transect_filter_takes_several(app):
    app.select("report")
    panel = app.panels["report"]
    panel.transect.set("EBM_W25_T6, PKB_S25_T1")
    panel.transect.refresh()
    app.update()
    assert "2 transect(s)" in panel.transect.status.cget("text")
    app.collect_all()
    assert app.cfg.report.transect_id == "EBM_W25_T6, PKB_S25_T1"
