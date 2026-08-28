"""
The model comparison, on one page.

Stage 5 can produce a great deal of detail, and the workbook keeps all of it.
Almost none of it changes a decision. This window shows the part that does --
which model to use, what it costs elsewhere in the taxonomy, and what to try
next -- so the workbook is somewhere to go when a number looks surprising
rather than the only way to read the result.

It renders from the same digest the log summary prints, so the two cannot
disagree with each other.
"""

from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path

import customtkinter as ctk

from . import theme as T
from .widgets import button, hint, label


def show(parent, digest: dict) -> "TearSheet | None":
    """Open the summary window, or do nothing if there is nothing to show."""
    if not digest or not digest.get("recommended"):
        return None
    return TearSheet(parent, digest)


class TearSheet(ctk.CTkToplevel):
    def __init__(self, parent, digest: dict):
        super().__init__(parent)
        self.digest = digest
        self._images: list = []          # Tk drops un-referenced images

        self.title("Model comparison")
        self.geometry("1080x900")
        self.minsize(820, 600)
        self.configure(fg_color=T.BG)
        self.grid_columnconfigure(0, weight=1)
        self.grid_rowconfigure(1, weight=1)

        self._build_header()
        self.body = ctk.CTkScrollableFrame(self, fg_color=T.BG)
        self.body.grid(row=1, column=0, sticky="nsew", padx=16, pady=(0, 8))
        self.body.grid_columnconfigure(0, weight=1)
        self._row = 0

        self._build_verdict()
        self._build_table()
        self._build_caveats()
        self._build_next_steps()
        self._build_movers()
        self._build_weakest()
        self._build_figures()
        self._build_footer()

        # Above the main window, but not above everything else on the desktop.
        self.transient(parent)
        self.after(120, self.lift)

    # ---- scaffolding -------------------------------------------------

    def _card(self, title: str, subtitle: str = "") -> ctk.CTkFrame:
        frame = ctk.CTkFrame(self.body, fg_color=T.SURFACE, corner_radius=T.RADIUS)
        frame.grid(row=self._row, column=0, sticky="ew", pady=(0, 12))
        frame.grid_columnconfigure(0, weight=1)
        self._row += 1

        ctk.CTkLabel(frame, text=title, font=T.FONT_H2, text_color=T.HEADING,
                     anchor="w").grid(row=0, column=0, sticky="ew",
                                      padx=T.PAD, pady=(T.PAD, 0))
        if subtitle:
            ctk.CTkLabel(frame, text=subtitle, font=T.FONT_SMALL,
                         text_color=T.TEXT_MUTED, anchor="w", justify="left",
                         wraplength=940).grid(row=1, column=0, sticky="ew",
                                              padx=T.PAD, pady=(2, 0))
        inner = ctk.CTkFrame(frame, fg_color="transparent")
        inner.grid(row=2, column=0, sticky="ew", padx=T.PAD, pady=T.PAD)
        inner.grid_columnconfigure(0, weight=1)
        return inner

    def _mono(self, parent, text: str, row: int, color=None) -> None:
        ctk.CTkLabel(parent, text=text, font=T.FONT_MONO, anchor="w",
                     justify="left", text_color=color or T.TEXT
                     ).grid(row=row, column=0, sticky="ew")

    # ---- sections ----------------------------------------------------

    def _build_header(self) -> None:
        h = ctk.CTkFrame(self, fg_color=T.SURFACE, corner_radius=0, height=84)
        h.grid(row=0, column=0, sticky="ew")
        h.grid_columnconfigure(0, weight=1)
        h.grid_propagate(False)

        ctk.CTkLabel(h, text="Use this model", font=T.FONT_SMALL,
                     text_color=T.TEXT_MUTED, anchor="w"
                     ).grid(row=0, column=0, sticky="sw", padx=18, pady=(14, 0))
        ctk.CTkLabel(h, text=self.digest["recommended"], font=T.FONT_TITLE,
                     text_color=T.ACCENT, anchor="w"
                     ).grid(row=1, column=0, sticky="nw", padx=18, pady=(0, 14))

    def _build_verdict(self) -> None:
        d = self.digest
        best = next((r for r in d["table"] if r["is_winner"]), None)
        if not best:
            return
        inner = self._card(
            "The headline",
            f"Ranked by macro F1 on {d['images']:,} held-out images - images "
            f"no model in this comparison was trained on.")

        cover = ("-" if best["cover_bias"] is None
                 else f"{best['cover_bias']:.0f}%")
        stats = ctk.CTkFrame(inner, fg_color="transparent")
        stats.grid(row=0, column=0, sticky="ew")
        for i, (value, caption) in enumerate((
                (f"{best['macro_f1']:.3f}", "macro F1"),
                (f"{best['top1']:.1%}", "top-1 accuracy"),
                (f"{best['weak']}", f"classes below F1 0.60"),
                (cover, "mean cover error"))):
            stats.grid_columnconfigure(i, weight=1)
            cell = ctk.CTkFrame(stats, fg_color="transparent")
            cell.grid(row=0, column=i, sticky="w", padx=(0, 26))
            ctk.CTkLabel(cell, text=value, font=(T.FAMILY_SEMIBOLD, 26),
                         text_color=T.TEXT).grid(row=0, column=0, sticky="w")
            ctk.CTkLabel(cell, text=caption, font=T.FONT_SMALL,
                         text_color=T.TEXT_MUTED).grid(row=1, column=0, sticky="w")

        for i, line in enumerate(d["why"], start=1):
            ctk.CTkLabel(inner, text="- " + line, font=T.FONT_BODY,
                         text_color=T.TEXT, anchor="w", justify="left",
                         wraplength=920).grid(row=i, column=0, sticky="ew",
                                              pady=(8 if i == 1 else 3, 0))

    def _build_table(self) -> None:
        d = self.digest
        if len(d["table"]) < 2:
            return
        inner = self._card("Every model, side by side")
        width = max(len(r["model"]) for r in d["table"])
        self._mono(inner,
                   f"{'':2}{'model':<{width}}  {'macro F1':>8}  {'top-1':>7}  "
                   f"{'weak':>5}  {'cover err':>9}  training",
                   0, T.TEXT_MUTED)
        for i, r in enumerate(d["table"], start=1):
            cover = "-" if r["cover_bias"] is None else f"{r['cover_bias']:.0f}%"
            mark = "> " if r["is_winner"] else "  "
            self._mono(
                inner,
                f"{mark}{r['model']:<{width}}  {r['macro_f1']:>8.3f}  "
                f"{r['top1']:>7.1%}  {r['weak']:>5}  {cover:>9}  "
                f"{r['verdict']} (best epoch {r['best_epoch']}/{r['epochs']})",
                i, T.ACCENT if r["is_winner"] else T.TEXT)

    def _build_caveats(self) -> None:
        if not self.digest["caveats"]:
            return
        inner = self._card("Read this first")
        for i, c in enumerate(self.digest["caveats"]):
            ctk.CTkLabel(inner, text="! " + c, font=T.FONT_BODY,
                         text_color=T.WARN, anchor="w", justify="left",
                         wraplength=920).grid(row=i, column=0, sticky="ew",
                                              pady=(0 if i == 0 else 4, 0))

    def _build_next_steps(self) -> None:
        steps = self.digest["next_steps"]
        if not steps:
            return
        inner = self._card("What to try next", "Ordered by expected payoff.")
        for i, step in enumerate(steps):
            ctk.CTkLabel(inner, text=f"{i + 1}.  {step}", font=T.FONT_BODY,
                         text_color=T.TEXT, anchor="w", justify="left",
                         wraplength=920).grid(row=i, column=0, sticky="ew",
                                              pady=(0 if i == 0 else 5, 0))

    def _build_movers(self) -> None:
        movers = self.digest["movers"]
        if not movers:
            return
        inner = self._card(
            "Where the models disagree most",
            "A class the survey depends on can move the opposite way to the "
            "headline number, so this is the part worth checking by name.")
        self._mono(inner,
                   f"{'class':<12}{'points':>8}   {'chosen':>7} {'other':>7}   "
                   f"difference", 0, T.TEXT_MUTED)
        for i, m in enumerate(movers, start=1):
            better = m["delta"] > 0
            self._mono(
                inner,
                f"{m['label']:<12}{m['support']:>8,}   "
                f"{m['winner_f1']:>7.2f} {m['other_f1']:>7.2f}   "
                f"{abs(m['delta']):.2f} {'better' if better else 'worse'}",
                i, T.OK if better else T.WARN)

    def _build_weakest(self) -> None:
        weakest = self.digest["weakest"]
        if not weakest:
            return
        inner = self._card(
            "The work queue",
            "Weakest classes ordered by how many held-out points they get "
            "wrong, not by score - a big class at 0.7 costs more than a tiny "
            "one at 0.1. 'Cover' is how far the reported abundance would be "
            "from the truth.")
        self._mono(inner,
                   f"{'class':<12}{'points':>8}  {'F1':>5}  {'prec':>5}  "
                   f"{'rec':>5}   {'limited by':<11} cover", 0, T.TEXT_MUTED)
        for i, c in enumerate(weakest, start=1):
            bias = ("-" if c["cover_bias_pct"] is None
                    else f"{c['cover_bias_pct']:+.0f}%")
            self._mono(
                inner,
                f"{c['label']:<12}{c['support']:>8,}  {c['f1']:>5.2f}  "
                f"{c['precision']:>5.2f}  {c['recall']:>5.2f}   "
                f"{c['bottleneck']:<11} {bias}", i)

    def _build_figures(self) -> None:
        figures = self.digest.get("figures") or {}
        if not figures:
            return
        inner = self._card(
            "Figures",
            "Per-class F1 for every model, and the learning curves behind the "
            "overfitting verdicts.")
        row = 0
        for key, caption in (("per_class", "Per-class F1"),
                             ("curves", "Learning curves")):
            path = figures.get(key)
            if not path or not Path(path).is_file():
                continue
            image = self._load(path, max_width=900)
            if image is None:
                continue
            ctk.CTkLabel(inner, text=caption, font=T.FONT_SMALL,
                         text_color=T.TEXT_MUTED, anchor="w"
                         ).grid(row=row, column=0, sticky="ew", pady=(6, 2))
            ctk.CTkLabel(inner, text="", image=image).grid(row=row + 1, column=0,
                                                           sticky="w")
            row += 2

    def _load(self, path: str, max_width: int):
        """Scale a figure to fit, keeping a reference so Tk does not free it."""
        try:
            from PIL import Image
            im = Image.open(path)
            scale = min(1.0, max_width / im.width)
            size = (int(im.width * scale), int(im.height * scale))
            img = ctk.CTkImage(light_image=im, dark_image=im, size=size)
            self._images.append(img)
            return img
        except Exception:
            return None

    def _build_footer(self) -> None:
        f = ctk.CTkFrame(self, fg_color=T.SURFACE, corner_radius=0)
        f.grid(row=2, column=0, sticky="ew")
        f.grid_columnconfigure(0, weight=1)

        workbook = self.digest.get("workbook")
        hint(f, "The workbook holds every per-class number, the full confusion "
                "tables and the training arguments." if workbook else ""
             ).grid(row=0, column=0, sticky="w", padx=18, pady=14)

        if workbook:
            button(f, "Open workbook", lambda: _open(workbook), "ghost",
                   width=150).grid(row=0, column=1, padx=(6, 6), pady=12)
            button(f, "Open folder", lambda: _open(Path(workbook).parent),
                   "ghost", width=130).grid(row=0, column=2, padx=(0, 6), pady=12)
        button(f, "Close", self.destroy, "primary", width=110
               ).grid(row=0, column=3, padx=(0, 18), pady=12)


def _open(path) -> None:
    """Hand a path to the desktop, without ever raising into the GUI thread."""
    try:
        path = str(path)
        if sys.platform == "win32":
            os.startfile(path)                                  # noqa: S606
        elif sys.platform == "darwin":
            subprocess.Popen(["open", path])
        else:
            subprocess.Popen(["xdg-open", path])
    except Exception:
        pass
