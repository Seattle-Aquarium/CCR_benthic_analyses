"""
The CCR Benthic Classifier desktop application.

Four stages down the left, the selected stage's settings in the middle, one
shared log and progress bar along the bottom. Finishing a stage fills in the
next stage's inputs and moves the selection there, so the paths are never
retyped and a dataset cannot be pointed at the wrong stage by accident.

Threading: a stage runs on a worker thread and reports through a queue that the
Tk main loop drains on a timer. Tk is not thread-safe, so no worker ever touches
a widget -- including via the logger, which reaches the log pane through that
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

from .. import brand
from ..config import PipelineState, default_state_path
from ..logging_setup import (QueueHandler, attach, configure, detach,
                             ensure_streams)
from ..stages import ORDER, TITLES, StageResult
from . import theme as T
from .panels import PANELS
from .widgets import StageRail, button

APP_NAME = "CCR Benthic Classifier"
LOG_FILENAME = "classifier_log.txt"

#: What the next stage needs before it can run, phrased as the reason to go
#: there. Shown in the rail once the previous stage commits.
NEXT_HINT = {
    "balance": "Stage 1's output was added to the merge list.",
    "train": "Stage 2's merged dataset is now stage 3's input.",
    "evaluate": "The trained weights are now stage 4's model.",
}


class App(ctk.CTk):
    def __init__(self) -> None:
        super().__init__()
        self.title(APP_NAME)
        self.geometry("1240x980")
        self.minsize(1060, 760)

        self.state_path = default_state_path()
        self.pipeline = PipelineState.load(self.state_path)

        self.mode = self.pipeline.theme or "dark"
        T.apply(ctk, self.mode)
        self.configure(fg_color=T.BG)
        self._set_icon()

        self._queue: "queue.Queue[tuple]" = queue.Queue()
        self._worker: threading.Thread | None = None
        self._cancel = threading.Event()
        self._log_handler: logging.Handler | None = None
        self._logo_img = None
        self.current = ORDER[0]

        configure(Path(__file__).resolve().parent.parent.parent / LOG_FILENAME)

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

    def _set_icon(self) -> None:
        path = brand.icon_path()
        if path:
            try:
                self.iconbitmap(path)
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
        ctk.CTkLabel(h, text="Verified annotations to an independently "
                             "evaluated benthic classification model",
                     font=T.FONT_SMALL, text_color=T.TEXT_MUTED
                     ).grid(row=1, column=1, sticky="nw", pady=(0, 15))

        self.theme_switch = ctk.CTkSwitch(
            h, text="Dark mode", command=self._toggle_theme,
            font=T.FONT_SMALL, text_color=T.TEXT,
            progress_color=T.ACCENT, button_color=T.SURFACE_ALT)
        if self.mode == "dark":
            self.theme_switch.select()
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
        self.pipeline.theme = self.mode
        T.apply(ctk, self.mode)
        self.theme_switch.configure(
            text="Dark mode" if self.mode == "dark" else "Light mode")
        self._load_logo()

    def _build_rail(self) -> None:
        self.rail = StageRail(self, [(k, TITLES[k]) for k in ORDER], self.select)
        self.rail.grid(row=1, column=0, rowspan=2, sticky="nsw")

    def _build_panels(self) -> None:
        self.holder = ctk.CTkFrame(self, fg_color=T.BG)
        self.holder.grid(row=1, column=1, sticky="nsew", padx=(14, 16), pady=(14, 6))
        self.holder.grid_columnconfigure(0, weight=1)
        self.holder.grid_rowconfigure(2, weight=1)

        self.heading = ctk.CTkLabel(self.holder, text="", font=T.FONT_H1,
                                    text_color=T.HEADING, anchor="w")
        self.heading.grid(row=0, column=0, sticky="ew")
        self.subheading = ctk.CTkLabel(self.holder, text="", font=T.FONT_SMALL,
                                       text_color=T.TEXT_MUTED, anchor="w",
                                       justify="left", wraplength=880)
        self.subheading.grid(row=1, column=0, sticky="ew", pady=(4, 0))

        self.panels = {}
        for klass in PANELS:
            panel = klass(self.holder, self.pipeline)
            panel.grid(row=2, column=0, sticky="nsew", pady=(12, 0))
            panel.grid_remove()
            self.panels[klass.key] = panel

    def _build_footer(self) -> None:
        f = ctk.CTkFrame(self, fg_color=T.SURFACE, corner_radius=0)
        f.grid(row=2, column=1, sticky="ew", padx=0, pady=0)
        f.grid_columnconfigure(0, weight=1)

        self.log = ctk.CTkTextbox(f, height=148, font=T.FONT_MONO,
                                  fg_color=T.FIELD_BG, text_color=T.TEXT,
                                  border_width=1, border_color=T.BORDER,
                                  corner_radius=6, wrap="word")
        self.log.grid(row=0, column=0, columnspan=4, sticky="ew",
                      padx=16, pady=(12, 8))
        self.log.tag_config("warn", foreground=brand.CORAL)
        self.log.tag_config("ok", foreground=brand.ALGAE)
        self.log.configure(state="disabled")

        self.progress = ctk.CTkProgressBar(f, height=12, corner_radius=6,
                                           progress_color=T.ACCENT,
                                           fg_color=T.SURFACE_ALT)
        self.progress.set(0.0)
        self.progress.grid(row=1, column=0, sticky="ew", padx=(16, 12), pady=(0, 6))

        self.status = ctk.CTkLabel(f, text="Ready.", font=T.FONT_SMALL,
                                   text_color=T.TEXT_MUTED, anchor="w")
        self.status.grid(row=2, column=0, sticky="ew", padx=16, pady=(0, 12))

        self.preview_box = ctk.CTkCheckBox(
            f, text="Preview only", font=T.FONT_BODY, text_color=T.TEXT,
            fg_color=T.ACCENT, hover_color=T.ACCENT_HOVER,
            checkmark_color=T.ACCENT_TEXT, border_color=T.FIELD_BORDER,
            corner_radius=4, command=self._preview_toggled)
        self.preview_box.grid(row=1, column=1, rowspan=2, padx=(0, 14),
                              pady=(0, 12), sticky="w")

        self.run_btn = button(f, "Run", self._start, "primary", 176)
        self.run_btn.grid(row=1, column=2, rowspan=2, padx=(0, 8), pady=(0, 12))
        self.cancel_btn = button(f, "Stop", self._cancel_run, "danger", 90)
        self.cancel_btn.grid(row=1, column=3, rowspan=2, padx=(0, 16), pady=(0, 12))
        self.cancel_btn.configure(state="disabled")

    # ------------------------------------------------------------------
    #  navigation
    # ------------------------------------------------------------------

    def select(self, key: str) -> None:
        if self._busy():
            return
        if self.current in self.panels:
            self.panels[self.current].grid_remove()
        self.current = key
        panel = self.panels[key]
        panel.grid()
        self.heading.configure(text=panel.title)
        self.subheading.configure(text=panel.subtitle)
        self.preview_box.configure(text=panel.preview_text)
        self.run_btn.configure(text=panel.run_text)
        self._sync_preview_from_panel()
        self.rail.select(key)

    def _sync_preview_from_panel(self) -> None:
        panel = self.panels[self.current]
        if panel.preview.get():
            self.preview_box.select()
        else:
            self.preview_box.deselect()

    def _preview_toggled(self) -> None:
        self.panels[self.current].preview.set(bool(self.preview_box.get()))

    # ------------------------------------------------------------------
    #  running
    # ------------------------------------------------------------------

    def _busy(self) -> bool:
        return bool(self._worker and self._worker.is_alive())

    def _start(self) -> None:
        if self._busy():
            return
        panel = self.panels[self.current]
        panel.preview.set(bool(self.preview_box.get()))
        panel.collect()

        problem = panel.validate()
        if problem:
            messagebox.showinfo(APP_NAME, problem)
            return

        if not panel.preview.get() and not self._confirm_commit(panel):
            return

        self._save_state()

        self._cancel.clear()
        self.run_btn.configure(state="disabled")
        self.cancel_btn.configure(state="normal")
        self.progress.set(0.0)
        self._log("─" * 78)
        self._log(f"{panel.title}"
                  f"{'  (preview)' if panel.preview.get() else ''}")

        self._log_handler = attach(QueueHandler(self._queue))

        stage_key = self.current
        cfg = getattr(self.pipeline, stage_key)
        runner = _stage_runner(stage_key)

        def work() -> None:
            try:
                result = runner(cfg, progress=self._report, cancel=self._cancel)
                self._queue.put(("done", stage_key, result))
            except Exception:
                self._queue.put(("crash", traceback.format_exc()))

        self._worker = threading.Thread(target=work, daemon=True)
        self._worker.start()

    def _confirm_commit(self, panel) -> bool:
        """Ask before anything is written, named for what it will actually do."""
        messages = {
            "extract": "Extract patches and write them to disk?",
            "balance": "Write the merged dataset?"
                       + ("\n\nLeaked held-out patches will be moved to a "
                          "_quarantined_leaked folder."
                          if self.pipeline.balance.quarantine_holdout_leaks
                          and self.pipeline.balance.holdout_dir else ""),
            "train": f"Start training for up to "
                     f"{self.pipeline.train.epochs} epochs?\n\nThis can take "
                     f"hours. The window stays usable and Stop finishes the "
                     f"current epoch cleanly.",
            "evaluate": "Run inference over the whole held-out set?",
        }
        return messagebox.askyesno(APP_NAME, messages.get(panel.key, "Proceed?"))

    def _report(self, fraction: float, message: str = "") -> None:
        """ProgressCB handed to the stage. Called from the worker thread."""
        self._queue.put(("progress", fraction, message))

    def _cancel_run(self) -> None:
        if self._busy():
            self._cancel.set()
            self.status.configure(text="Stopping…")

    def _drain(self) -> None:
        try:
            while True:
                item = self._queue.get_nowait()
                kind = item[0]
                if kind == "progress":
                    _, fraction, message = item
                    self.progress.set(max(0.0, min(1.0, fraction)))
                    if message:
                        self.status.configure(text=message)
                elif kind == "log":
                    _, level, text = item
                    self._log(text, "warn" if level >= logging.WARNING else None)
                elif kind == "done":
                    self._finish(item[1], item[2])
                elif kind == "crash":
                    self._log("Unexpected error:\n" + item[1], "warn")
                    self._reset()
        except queue.Empty:
            pass
        self.after(80, self._drain)

    def _finish(self, stage_key: str, result: StageResult) -> None:
        self._reset()
        self.progress.set(1.0 if result.ok else self.progress.get())

        self._log("")
        for line in result.summary().splitlines():
            tag = "warn" if line.startswith(("ERROR", "WARNING", "BLOCKED")) else None
            self._log(line, tag)
        self._log(f"({result.elapsed_s / 60:.1f} min)")

        if result.cancelled:
            self.status.configure(text="Stopped.")
            self.rail.set_status(stage_key, "todo")
            return
        if result.blocked:
            self.status.configure(text="Blocked — see the log.")
            self.rail.set_status(stage_key, "blocked")
            messagebox.showwarning(APP_NAME, result.blocked[0])
            return
        if result.errors:
            self.status.configure(text="Failed — see the log.")
            self.rail.set_status(stage_key, "blocked")
            return

        if result.preview:
            self.status.configure(text="Preview complete — nothing written.")
            return

        self.status.configure(text=f"{TITLES[stage_key]} complete.")
        self.rail.set_status(stage_key, "done")
        self._advance(stage_key, result)

    def _advance(self, stage_key: str, result: StageResult) -> None:
        """Hand this stage's output to whichever stage consumes it."""
        outputs = result.outputs
        nxt = None
        if stage_key == "extract" and outputs.get("output_dir"):
            nxt = self.pipeline.advance_from_extract(
                outputs["output_dir"], bool(outputs.get("no_split")))
        elif stage_key == "balance" and self.pipeline.balance.output_dir:
            nxt = self.pipeline.advance_from_balance(
                self.pipeline.balance.output_dir)
        elif stage_key == "train" and outputs.get("weights"):
            nxt = self.pipeline.advance_from_train(outputs["weights"])
        elif stage_key == "evaluate":
            self._offer_reports(outputs)

        self._save_state()
        if not nxt:
            return

        self.panels[nxt].load()
        self.rail.set_note(NEXT_HINT.get(nxt, ""))
        self._log(f"→ {NEXT_HINT.get(nxt, '')} Moving to {TITLES[nxt]}.", "ok")
        self.select(nxt)

    def _offer_reports(self, outputs: dict) -> None:
        folder = outputs.get("report_path")
        if not folder:
            return
        try:
            os.startfile(Path(folder).parent)      # noqa: S606
        except Exception:
            pass

    def _reset(self) -> None:
        self.run_btn.configure(state="normal")
        self.cancel_btn.configure(state="disabled")
        if self._log_handler is not None:
            detach(self._log_handler)
            self._log_handler = None

    # ------------------------------------------------------------------
    #  misc
    # ------------------------------------------------------------------

    def _log(self, text: str, tag: str | None = None) -> None:
        self.log.configure(state="normal")
        self.log.insert("end", text + "\n", tag or "")
        self.log.see("end")
        self.log.configure(state="disabled")

    def _save_state(self) -> None:
        for panel in self.panels.values():
            panel.collect()
        try:
            self.pipeline.save(self.state_path)
        except OSError as ex:
            self._log(f"Could not save settings: {ex}", "warn")

    def _on_close(self) -> None:
        if self._busy():
            if not messagebox.askyesno(
                    APP_NAME, "A stage is still running. Quit anyway?"):
                return
            self._cancel.set()
        self._save_state()
        self.destroy()


def _stage_runner(key: str):
    """Import a stage lazily -- ultralytics and torch cost seconds to import,
    and the window should not wait on them to appear."""
    from ..stages import balance, evaluate, extract, train
    return {"extract": extract.run, "balance": balance.run,
            "train": train.run, "evaluate": evaluate.run}[key]


def main() -> None:
    # Before anything else: a windowed (pythonw) process has no sys.stdout at
    # all, and Ultralytics writes its training table straight to it. Without
    # this, training dies with "'NoneType' object has no attribute 'write'"
    # moments after the base model loads.
    ensure_streams()
    ctk.set_default_color_theme("blue")
    App().mainloop()


if __name__ == "__main__":
    main()
