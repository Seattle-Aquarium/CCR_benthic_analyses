"""
The Kelp Quest desktop application.

Eight stages down the left, the selected stage's settings in the middle, one
shared log and progress bar along the bottom. Finishing a stage fills in the
next stage's inputs and moves the selection there, so paths are never retyped
and a transect cannot be pointed at the wrong stage by accident.

Threading: a stage runs on a worker thread and reports through a queue the Tk
main loop drains on a timer. Tk is not thread-safe, so no worker ever touches a
widget -- including via the logger, which reaches the log pane through that
same queue.
"""

from __future__ import annotations

import logging
import os
import queue
import threading
import traceback
from pathlib import Path
from tkinter import messagebox

import customtkinter as ctk

from .. import APP_NAME, brand, discovery, logging_setup, pipeline
from ..config import AppConfig
from . import theme as T
from .panels import PANELS
from .widgets import StageRail, button


class App(ctk.CTk):
    def __init__(self) -> None:
        super().__init__()
        self.title(APP_NAME)
        self._fit_to_screen(1280, 940)

        self.cfg = AppConfig.load()
        self.mode = self.cfg.theme if self.cfg.theme in ("dark", "light") else "dark"
        T.apply(ctk, self.mode)
        self.configure(fg_color=T.BG)
        self._set_icon()

        self.transect: discovery.Transect | None = None
        self._queue: queue.Queue[tuple] = queue.Queue()
        self._worker: threading.Thread | None = None
        self._cancel = threading.Event()
        self._logo_img = None
        self.current = pipeline.ORDER[0]

        logging_setup.setup()
        # The sink runs on whichever thread logged, so it queues rather than
        # writing to the widget -- the same rule as everything else here.
        logging_setup.add_sink(
            lambda line, level=logging.INFO: self._queue.put(("log", line)))

        self.grid_columnconfigure(1, weight=1)
        self.grid_rowconfigure(1, weight=1)

        self._build_header()
        self._build_rail()
        self._build_panels()
        self._build_footer()

        self.select(self.current)
        self.after(80, self._drain)
        self.protocol("WM_DELETE_WINDOW", self._on_close)

    # ------------------------------------------------------------------
    #  chrome
    # ------------------------------------------------------------------

    def _fit_to_screen(self, width: int, height: int) -> None:
        """Ask for a window that size, but never one taller than the screen.

        CustomTkinter multiplies the requested geometry by the display scaling
        on its way to Tk, so on a 250% laptop panel a request for 940 becomes
        2,350 real pixels and the footer -- progress, status and Stop -- ends
        up below the bottom edge, where nobody can reach it.

        ``winfo_screenwidth`` reports in the same units ``geometry`` takes
        (1382x864 on the 3456x2160 panel this was found on), so the clamp
        compares them directly. Dividing by the scaling factor as well shrinks
        the window to a quarter of the screen -- which was the first attempt at
        this, and looked like a layout bug rather than an arithmetic one.
        """
        max_w = self.winfo_screenwidth() - 60
        max_h = self.winfo_screenheight() - 110
        width = max(760, min(width, max_w))
        height = max(520, min(height, max_h))
        # Placed as well as sized. Tk's own default position put the window a
        # third of the way down, which on a short panel pushed the footer off
        # the bottom even though the window itself fitted.
        x = max(0, (self.winfo_screenwidth() - width) // 2)
        y = max(0, (self.winfo_screenheight() - height) // 3)
        self.geometry(f"{width}x{height}+{x}+{y}")
        self.minsize(min(1040, width), min(700, height))

    def _set_icon(self) -> None:
        path = Path(__file__).resolve().parent.parent.parent / "assets" / "app.ico"
        if path.is_file():
            try:
                self.iconbitmap(str(path))
            except Exception:      # a missing icon is never worth failing over
                pass

    def _build_header(self) -> None:
        h = ctk.CTkFrame(self, fg_color=T.SURFACE, corner_radius=0, height=78)
        h.grid(row=0, column=0, columnspan=2, sticky="ew")
        h.grid_columnconfigure(2, weight=1)
        h.grid_propagate(False)

        self.logo_label = ctk.CTkLabel(h, text="")
        self.logo_label.grid(row=0, column=0, rowspan=2, padx=(18, 14), pady=12)
        self._load_logo()

        ctk.CTkLabel(h, text=APP_NAME, font=T.FONT_TITLE, text_color=T.HEADING
                     ).grid(row=0, column=1, sticky="sw", pady=(15, 0))
        ctk.CTkLabel(h, text="Transect stills to classified Zooniverse subjects",
                     font=T.FONT_SMALL, text_color=T.TEXT_MUTED
                     ).grid(row=1, column=1, sticky="nw", pady=(0, 15))

        self.theme_switch = ctk.CTkSwitch(
            h, text="Dark mode", command=self._toggle_theme,
            font=T.FONT_SMALL, text_color=T.TEXT,
            progress_color=T.ACCENT, button_color=T.SURFACE_ALT,
        )
        if self.mode == "dark":
            self.theme_switch.select()
        else:
            self.theme_switch.deselect()
            self.theme_switch.configure(text="Light mode")
        self.theme_switch.grid(row=0, column=3, rowspan=2, padx=18)

    def _load_logo(self) -> None:
        path = T.logo_for(self.mode)
        if not path:
            self.logo_label.configure(text="Seattle Aquarium", font=T.FONT_H2,
                                      text_color=T.HEADING)
            return
        try:
            from PIL import Image

            im = Image.open(path).convert("RGBA")
            height = 46
            width = max(1, int(im.width * height / im.height))
            self._logo_img = ctk.CTkImage(light_image=im, dark_image=im,
                                          size=(width, height))
            self.logo_label.configure(image=self._logo_img, text="")
        except Exception:
            self.logo_label.configure(text="Seattle Aquarium", font=T.FONT_H2,
                                      text_color=T.HEADING)

    def _toggle_theme(self) -> None:
        self.mode = "dark" if self.theme_switch.get() else "light"
        self.cfg.theme = self.mode
        T.apply(ctk, self.mode)
        self.theme_switch.configure(text="Dark mode" if self.mode == "dark"
                                    else "Light mode")
        self._load_logo()

    def _build_rail(self) -> None:
        self.rail = StageRail(
            self, [(k, pipeline.TITLES[k]) for k in pipeline.ORDER], self.select)
        self.rail.grid(row=1, column=0, rowspan=2, sticky="nsw")

    def _build_panels(self) -> None:
        self.holder = ctk.CTkFrame(self, fg_color=T.BG)
        self.holder.grid(row=1, column=1, sticky="nsew", padx=(14, 16),
                         pady=(14, 6))
        self.holder.grid_columnconfigure(0, weight=1)
        self.holder.grid_rowconfigure(2, weight=1)

        self.heading = ctk.CTkLabel(self.holder, text="", font=T.FONT_H1,
                                    text_color=T.HEADING, anchor="w")
        self.heading.grid(row=0, column=0, sticky="ew")
        self.subheading = ctk.CTkLabel(self.holder, text="", font=T.FONT_SMALL,
                                       text_color=T.TEXT_MUTED, anchor="w",
                                       justify="left", wraplength=880)
        self.subheading.grid(row=1, column=0, sticky="ew", pady=(4, 0))

        self.panels: dict[str, object] = {}
        for klass in PANELS:
            panel = klass(self.holder, self)
            panel.grid(row=2, column=0, sticky="nsew", pady=(12, 0))
            panel.grid_remove()
            self.panels[klass.key] = panel

    def _build_footer(self) -> None:
        """Progress, status, log and the one Run button.

        One button rather than one per page: only one worker exists, and a
        button on every page would have to mean something different on each.
        """
        f = ctk.CTkFrame(self, fg_color=T.SURFACE, corner_radius=0)
        f.grid(row=2, column=1, sticky="ew")
        f.grid_columnconfigure(0, weight=1)

        # Deliberately short. The log is a running commentary, not the work;
        # on a 1366x768 laptop a taller pane leaves room for barely one card.
        self.log_box = ctk.CTkTextbox(f, height=104, font=T.FONT_MONO,
                                      fg_color=T.FIELD_BG, text_color=T.TEXT,
                                      border_width=1, border_color=T.BORDER,
                                      corner_radius=6, wrap="word")
        self.log_box.grid(row=0, column=0, columnspan=4, sticky="ew",
                          padx=16, pady=(12, 8))
        self.log_box.tag_config("warn", foreground=brand.CORAL)
        self.log_box.tag_config("ok", foreground=brand.ALGAE)
        self.log_box.configure(state="disabled")

        self.progress = ctk.CTkProgressBar(f, height=12, corner_radius=6,
                                           progress_color=T.ACCENT,
                                           fg_color=T.SURFACE_ALT)
        self.progress.set(0.0)
        self.progress.grid(row=1, column=0, sticky="ew", padx=(16, 12),
                           pady=(0, 6))

        self.status = ctk.CTkLabel(f, text="Ready.", font=T.FONT_SMALL,
                                   text_color=T.TEXT_MUTED, anchor="w")
        self.status.grid(row=2, column=0, sticky="ew", padx=16, pady=(0, 12))

        self.check_var = ctk.BooleanVar(value=True)
        self.check_box = ctk.CTkCheckBox(
            f, text="Check only", variable=self.check_var,
            command=self._check_toggled, font=T.FONT_BODY, text_color=T.TEXT,
            fg_color=T.ACCENT, hover_color=T.ACCENT_HOVER,
            checkmark_color=T.ACCENT_TEXT, border_color=T.FIELD_BORDER,
            corner_radius=4, border_width=2)
        self.check_box.grid(row=1, column=1, rowspan=2, padx=(0, 14),
                            pady=(0, 12), sticky="w")

        self.run_btn = button(f, "Run", self._start, "primary", 190)
        self.run_btn.grid(row=1, column=2, rowspan=2, padx=(0, 8), pady=(0, 12))
        self.cancel_btn = button(f, "Stop", self._cancel_run, "danger", 90)
        self.cancel_btn.grid(row=1, column=3, rowspan=2, padx=(0, 16),
                             pady=(0, 12))
        self.cancel_btn.configure(state="disabled")

    # ------------------------------------------------------------------
    #  navigation
    # ------------------------------------------------------------------

    def select(self, key: str) -> None:
        if self.busy:
            return
        if self.current in self.panels:
            self.panels[self.current].grid_remove()
        # Collect first: a number typed on the page being left has to reach the
        # config before the page being entered reads it.
        self.collect_all()
        self.current = key
        panel = self.panels[key]
        refresh = getattr(panel, "refresh", None)
        if callable(refresh):
            try:
                refresh()
            except Exception:
                self._log("Could not refresh that page:\n"
                          + traceback.format_exc(), "warn")
        panel.grid()
        self.heading.configure(text=panel.title)
        self.subheading.configure(text=panel.subtitle)
        self.run_btn.configure(text=panel.run_text)
        self.check_box.configure(text=panel.check_text)
        if panel.can_check:
            self.check_box.grid()
        else:
            # A read-only stage has nothing to check against; offering the
            # toggle would imply the Run does something it does not.
            self.check_box.grid_remove()
        self._check_toggled()
        self.rail.select(key)

    def _check_toggled(self) -> None:
        panel = self.panels[self.current]
        checking = bool(self.check_var.get()) and panel.can_check
        self.run_btn.configure(text="Check" if checking else panel.run_text)

    # ------------------------------------------------------------------
    #  shared state
    # ------------------------------------------------------------------

    def collect_all(self) -> None:
        """Pull every panel's fields into the config."""
        for panel in getattr(self, "panels", {}).values():
            try:
                panel.collect()
            except Exception:
                self._log("Could not read one page's settings:\n"
                          + traceback.format_exc(), "warn")

    def save_config(self) -> None:
        self.collect_all()
        self.cfg.theme = self.mode
        self.cfg.save()

    def warn(self, message: str) -> None:
        messagebox.showinfo(APP_NAME, message)

    def log_lines(self, text: str, warn: bool = False) -> None:
        """Put something in the log pane from a panel's own button.

        Panels normally say nothing directly -- the footer's Run reports
        through the stage result. A button that acts on its own, like mapping
        a label, has no stage result to report through, and its outcome
        belongs in the same scrolling record as everything else.
        """
        for line in str(text).splitlines():
            self._log(line, "warn" if warn else None)

    # ------------------------------------------------------------------
    #  running
    # ------------------------------------------------------------------

    @property
    def busy(self) -> bool:
        return bool(self._worker and self._worker.is_alive())

    def _start(self) -> None:
        if self.busy:
            return
        panel = self.panels[self.current]
        self.collect_all()

        problem = panel.validate()
        if problem:
            self.warn(problem)
            return

        preview = bool(self.check_var.get()) and panel.can_check
        if not preview:
            question = panel.confirm()
            if question and not messagebox.askyesno(APP_NAME, question):
                return

        self.save_config()
        self._cancel.clear()
        self.run_btn.configure(state="disabled")
        self.cancel_btn.configure(state="normal")
        self.progress.set(0.0)
        self._log("-" * 74)
        self._log(panel.title + ("   (check only)" if preview else ""))

        stage = self.current
        ctx = pipeline.Context(cfg=self.cfg, transect=self.transect,
                               preview=preview)

        def work() -> None:
            try:
                result = pipeline.run(
                    stage, ctx,
                    lambda frac, message="": self._queue.put(
                        ("progress", frac, message)),
                    self._cancel)
                self._queue.put(("done", stage, result))
            except Exception:
                self._queue.put(("crash", traceback.format_exc()))

        self._worker = threading.Thread(target=work, daemon=True)
        self._worker.start()

    def _cancel_run(self) -> None:
        if self.busy:
            self._cancel.set()
            self.status.configure(text="Stopping — finishing the current still…")

    def _drain(self) -> None:
        try:
            while True:
                item = self._queue.get_nowait()
                kind = item[0]
                if kind == "progress":
                    _kind, frac, message = item
                    self.progress.set(max(0.0, min(1.0, frac)))
                    if message:
                        self.status.configure(text=message)
                elif kind == "log":
                    line = item[1]
                    tag = "warn" if "[WARNING]" in line or "[ERROR]" in line else None
                    self._log(line, tag)
                elif kind == "done":
                    self._finish(item[1], item[2])
                elif kind == "crash":
                    self._log("Unexpected error:\n" + item[1], "warn")
                    self.status.configure(text="Failed — see the log.")
                    self._reset()
        except queue.Empty:
            pass
        self.after(80, self._drain)

    def _finish(self, stage: str, result: pipeline.StageResult) -> None:
        self._reset()
        self.progress.set(1.0 if result.ok else self.progress.get())

        self._log("")
        for line in result.summary().splitlines():
            tag = "warn" if line.startswith(("ERROR", "WARNING")) else None
            self._log(line, tag)

        panel = self.panels.get(stage)
        if panel is not None:
            try:
                panel.on_result(result)
            except Exception:
                self._log("After the run:\n" + traceback.format_exc(), "warn")

        if result.cancelled:
            self.status.configure(text="Stopped. What finished is on disk.")
            self.rail.set_status(stage, "todo")
            return
        if result.errors:
            self.status.configure(text="Failed — see the log.")
            self.rail.set_status(stage, "blocked")
            return

        if result.preview:
            self.status.configure(text="Checked — nothing was written.")
            # A check does not mark a stage done; it says the inputs are sound.
            self.rail.set_note("Checks out. Untick 'Check only' to run it.")
            return

        self.status.configure(text=f"{pipeline.TITLES[stage]} complete.")
        self.rail.set_status(stage, "done")
        self._advance(stage, result)

    def _advance(self, stage: str, result: pipeline.StageResult) -> None:
        """Hand this stage's output to whichever stage consumes it."""
        cfg = self.cfg
        nxt = None

        if stage == "transect":
            t = result.advance.get("transect")
            if t is not None:
                self.transect = t
                nxt = "patches"
        elif stage == "patches":
            # A fused run classified as it cut, so stage 3 is already done and
            # the useful next step is the sheets.
            if result.advance.get("classified"):
                self.rail.set_status("classify", "done")
                nxt = "metadata"
            else:
                nxt = "classify"
        elif stage == "classify":
            nxt = "metadata"
        elif stage == "metadata":
            nxt = "upload"
        elif stage == "upload":
            set_id = result.advance.get("subject_set_id")
            if set_id:
                # Seed the export list with the set just created; more can be
                # added there once subjects start moving between sets.
                cfg.export.subject_set_ids = str(set_id)
                self.panels["export"].load()
            nxt = "export"
        elif stage == "export":
            export_csv = result.advance.get("export_csv")
            if export_csv:
                # The combined export feeds both of the stages that read it.
                cfg.rejoin.classifications_csv = str(export_csv)
                cfg.report.export_csv = str(export_csv)
                self.panels["rejoin"].load()
                self.panels["report"].load()
            nxt = "rejoin"
        elif stage == "rejoin":
            nxt = "report"
        elif stage == "report":
            self._reveal(result.outputs[0].parent if result.outputs else None)

        self.save_config()
        if result.outputs and stage in ("metadata", "rejoin", "report"):
            self._reveal(Path(result.outputs[0]).parent)
        if not nxt:
            return

        self.panels[nxt].load()
        hint = pipeline.NEXT_HINT.get(nxt, "")
        self.rail.set_note(hint)
        self._log(f"-> {hint} Moving to {pipeline.TITLES[nxt]}.", "ok")
        # Every stage after the first writes or sends something, so the check
        # goes back on when the selection moves.
        self.check_var.set(True)
        self.select(nxt)

    def _reveal(self, path: Path | None) -> None:
        """Open a folder in Explorer, best effort."""
        if not path:
            return
        try:
            os.startfile(Path(path))          # noqa: S606
        except Exception:
            pass

    def _reset(self) -> None:
        self.run_btn.configure(state="normal")
        self.cancel_btn.configure(state="disabled")

    def _log(self, text: str, tag: str | None = None) -> None:
        self.log_box.configure(state="normal")
        self.log_box.insert("end", text + "\n", tag or "")
        self.log_box.see("end")
        self.log_box.configure(state="disabled")

    def _on_close(self) -> None:
        if self.busy:
            if not messagebox.askyesno(
                    APP_NAME,
                    "A stage is still running. Quit anyway?\n\nAnything "
                    "already written stays on disk, and uploaded subjects stay "
                    "in the upload log."):
                return
            self._cancel.set()
        try:
            self.save_config()
        except Exception:
            pass
        self.destroy()


def main() -> None:
    App().mainloop()
