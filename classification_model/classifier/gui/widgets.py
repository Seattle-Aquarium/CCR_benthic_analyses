"""Reusable GUI pieces: cards, fields, the path pickers, and the stage rail."""

from __future__ import annotations

from pathlib import Path
from tkinter import filedialog
from typing import Callable

import customtkinter as ctk

from . import theme as T


# --------------------------------------------------------------------------
#  Primitives
# --------------------------------------------------------------------------

class Card(ctk.CTkFrame):
    """A titled section panel."""

    def __init__(self, master, title: str, subtitle: str = "", **kw):
        super().__init__(master, fg_color=T.SURFACE, corner_radius=T.RADIUS,
                         border_width=1, border_color=T.BORDER, **kw)
        self.grid_columnconfigure(0, weight=1)

        head = ctk.CTkFrame(self, fg_color="transparent")
        head.grid(row=0, column=0, sticky="ew", padx=T.PAD, pady=(T.PAD, 4))
        head.grid_columnconfigure(0, weight=1)
        ctk.CTkLabel(head, text=title, font=T.FONT_H1, text_color=T.HEADING,
                     anchor="w").grid(row=0, column=0, sticky="w")
        if subtitle:
            ctk.CTkLabel(head, text=subtitle, font=T.FONT_SMALL,
                         text_color=T.TEXT_MUTED, justify="left", anchor="w",
                         wraplength=820
                         ).grid(row=1, column=0, sticky="w", pady=(3, 0))

        self.body = ctk.CTkFrame(self, fg_color="transparent")
        self.body.grid(row=1, column=0, sticky="nsew", padx=T.PAD, pady=(6, T.PAD))
        self.body.grid_columnconfigure(0, weight=1)


def entry(master, placeholder: str = "", width: int = 140, **kw) -> ctk.CTkEntry:
    return ctk.CTkEntry(
        master, placeholder_text=placeholder, width=width,
        font=T.FONT_BODY, text_color=T.TEXT, fg_color=T.FIELD_BG,
        border_color=T.FIELD_BORDER, border_width=1, corner_radius=6, **kw)


def label(master, text: str, muted: bool = False, font=None, **kw) -> ctk.CTkLabel:
    return ctk.CTkLabel(master, text=text, font=font or T.FONT_BODY,
                        text_color=T.TEXT_MUTED if muted else T.TEXT,
                        anchor="w", **kw)


def button(master, text: str, command, kind: str = "primary", width: int = 120):
    if kind == "primary":
        return ctk.CTkButton(master, text=text, command=command, width=width,
                             font=T.FONT_H2, fg_color=T.ACCENT,
                             hover_color=T.ACCENT_HOVER, text_color=T.ACCENT_TEXT,
                             corner_radius=6)
    if kind == "danger":
        return ctk.CTkButton(master, text=text, command=command, width=width,
                             font=T.FONT_BODY, fg_color="transparent",
                             hover_color=T.SURFACE_ALT, text_color=T.WARN,
                             border_width=1, border_color=T.BORDER, corner_radius=6)
    return ctk.CTkButton(master, text=text, command=command, width=width,
                         font=T.FONT_BODY, fg_color="transparent",
                         hover_color=T.SURFACE_ALT, text_color=T.TEXT,
                         border_width=1, border_color=T.BORDER, corner_radius=6)


def checkbox(master, text: str, variable, **kw) -> ctk.CTkCheckBox:
    return ctk.CTkCheckBox(master, text=text, variable=variable,
                           font=T.FONT_BODY, text_color=T.TEXT,
                           fg_color=T.ACCENT, hover_color=T.ACCENT_HOVER,
                           checkmark_color=T.ACCENT_TEXT,
                           border_color=T.FIELD_BORDER, corner_radius=4, **kw)


def hint(master, text: str) -> ctk.CTkLabel:
    """Small muted explanatory text under a control."""
    return ctk.CTkLabel(master, text=text, font=T.FONT_SMALL,
                        text_color=T.TEXT_MUTED, anchor="w", justify="left",
                        wraplength=800)


# --------------------------------------------------------------------------
#  Path pickers
# --------------------------------------------------------------------------

class PathRow(ctk.CTkFrame):
    """One path with a Browse button. Folder or file, per ``mode``."""

    def __init__(self, master, caption: str, mode: str = "folder",
                 placeholder: str = "", filetypes=None,
                 on_change: Callable[[str], None] | None = None):
        super().__init__(master, fg_color="transparent")
        self.grid_columnconfigure(1, weight=1)
        self.mode = mode
        self.filetypes = filetypes or [("CSV", "*.csv"), ("All files", "*.*")]
        self._on_change = on_change

        label(self, caption, muted=True, width=132).grid(
            row=0, column=0, sticky="w", padx=(0, 8))
        self.entry = entry(self, placeholder or "Not set", width=520)
        self.entry.grid(row=0, column=1, sticky="ew", padx=(0, 8))
        button(self, "Browse…", self._browse, "ghost", width=92
               ).grid(row=0, column=2)

        if on_change:
            self.entry.bind("<FocusOut>", lambda _e: on_change(self.get()))

    def _browse(self) -> None:
        start = self.get() or None
        if self.mode == "folder":
            picked = filedialog.askdirectory(title="Select a folder",
                                             initialdir=start)
        else:
            picked = filedialog.askopenfilename(title="Select a file",
                                                initialdir=start,
                                                filetypes=self.filetypes)
        if picked:
            self.set(picked)
            if self._on_change:
                self._on_change(picked)

    def get(self) -> str:
        return self.entry.get().strip()

    def set(self, value: str) -> None:
        self.entry.delete(0, "end")
        self.entry.insert(0, value or "")


class PathList(ctk.CTkFrame):
    """An editable list of paths -- several CSVs, or several datasets to merge.

    Order is meaningful for the merge, so the rows can be moved up and down.
    """

    def __init__(self, master, mode: str = "file", filetypes=None,
                 add_text: str = "+ Add", empty_text: str = "Nothing selected."):
        super().__init__(master, fg_color="transparent")
        self.grid_columnconfigure(0, weight=1)
        self.mode = mode
        self.filetypes = filetypes or [("CSV", "*.csv"), ("All files", "*.*")]
        self.empty_text = empty_text
        self._paths: list[str] = []

        self.rows = ctk.CTkFrame(self, fg_color="transparent")
        self.rows.grid(row=0, column=0, sticky="ew")
        self.rows.grid_columnconfigure(1, weight=1)

        foot = ctk.CTkFrame(self, fg_color="transparent")
        foot.grid(row=1, column=0, sticky="ew", pady=(6, 0))
        button(foot, add_text, self._add, "ghost", width=150
               ).grid(row=0, column=0, sticky="w")
        self.count = label(foot, "", muted=True)
        self.count.grid(row=0, column=1, sticky="w", padx=(12, 0))

        self._render()

    # ---- data ----------------------------------------------------

    def get(self) -> list[str]:
        return list(self._paths)

    def set(self, paths) -> None:
        self._paths = [str(p) for p in (paths or []) if str(p).strip()]
        self._render()

    def add(self, path: str) -> None:
        if path and path not in self._paths:
            self._paths.append(path)
            self._render()

    # ---- ui ------------------------------------------------------

    def _add(self) -> None:
        if self.mode == "folder":
            picked = filedialog.askdirectory(title="Select a dataset folder")
            picked = [picked] if picked else []
        else:
            picked = filedialog.askopenfilenames(title="Select annotation CSV(s)",
                                                 filetypes=self.filetypes)
        for p in picked:
            if p not in self._paths:
                self._paths.append(p)
        self._render()

    def _remove(self, index: int) -> None:
        if 0 <= index < len(self._paths):
            self._paths.pop(index)
            self._render()

    def _move(self, index: int, delta: int) -> None:
        target = index + delta
        if 0 <= index < len(self._paths) and 0 <= target < len(self._paths):
            self._paths[index], self._paths[target] = (self._paths[target],
                                                       self._paths[index])
            self._render()

    def _render(self) -> None:
        for child in self.rows.winfo_children():
            child.destroy()

        if not self._paths:
            hint(self.rows, self.empty_text).grid(row=0, column=0, columnspan=4,
                                                  sticky="w", pady=2)
            self.count.configure(text="")
            return

        for i, path in enumerate(self._paths):
            ctk.CTkLabel(self.rows, text=f"{i + 1}.", font=T.FONT_SMALL,
                         text_color=T.TEXT_MUTED, width=22, anchor="w"
                         ).grid(row=i, column=0, sticky="w", pady=2)
            ctk.CTkLabel(self.rows, text=_shorten(path), font=T.FONT_SMALL,
                         text_color=T.TEXT, anchor="w"
                         ).grid(row=i, column=1, sticky="ew", padx=(0, 8), pady=2)
            if len(self._paths) > 1:
                button(self.rows, "↑", lambda i=i: self._move(i, -1),
                       "ghost", width=30).grid(row=i, column=2, padx=2)
                button(self.rows, "↓", lambda i=i: self._move(i, 1),
                       "ghost", width=30).grid(row=i, column=3, padx=2)
            button(self.rows, "Remove", lambda i=i: self._remove(i),
                   "danger", width=76).grid(row=i, column=4, padx=(6, 0))

        self.count.configure(text=f"{len(self._paths)} selected")


def _shorten(path: str, keep: int = 3) -> str:
    """Show the tail of a long path -- the leading Dropbox tree is never the
    part that distinguishes one dataset from another."""
    parts = Path(path).parts
    return path if len(parts) <= keep else "…" + "\\".join(("",) + parts[-keep:])


# --------------------------------------------------------------------------
#  Stage rail
# --------------------------------------------------------------------------

class StageRail(ctk.CTkFrame):
    """The four stages down the left, showing which are done and which is next."""

    MARKS = {"todo": "○", "active": "●", "done": "✓", "blocked": "!"}

    def __init__(self, master, stages: list[tuple[str, str]],
                 on_select: Callable[[str], None]):
        super().__init__(master, fg_color=T.SURFACE, corner_radius=0, width=234)
        self.grid_propagate(False)
        self.grid_columnconfigure(0, weight=1)
        self._on_select = on_select
        self._buttons: dict[str, ctk.CTkButton] = {}
        self._status: dict[str, str] = {}
        self._current = stages[0][0] if stages else ""

        ctk.CTkLabel(self, text="PIPELINE", font=T.FONT_SMALL,
                     text_color=T.TEXT_MUTED, anchor="w"
                     ).grid(row=0, column=0, sticky="ew", padx=18, pady=(18, 8))

        for i, (key, title) in enumerate(stages):
            self._status[key] = "todo"
            btn = ctk.CTkButton(
                self, text="", command=lambda k=key: self._on_select(k),
                anchor="w", height=44, corner_radius=6, font=T.FONT_BODY,
                fg_color="transparent", hover_color=T.SURFACE_ALT,
                text_color=T.TEXT, border_width=0)
            btn.grid(row=i + 1, column=0, sticky="ew", padx=10, pady=2)
            self._buttons[key] = btn
            btn._title = f"{i + 1}.  {title}"       # noqa: SLF001 - local convention

        self.note = ctk.CTkLabel(
            self, text="", font=T.FONT_SMALL, text_color=T.TEXT_MUTED,
            anchor="nw", justify="left", wraplength=196)
        self.note.grid(row=len(stages) + 1, column=0, sticky="new",
                       padx=18, pady=(16, 18))
        self.grid_rowconfigure(len(stages) + 1, weight=1)
        self.refresh()

    def select(self, key: str) -> None:
        self._current = key
        self.refresh()

    def set_status(self, key: str, status: str) -> None:
        self._status[key] = status
        self.refresh()

    def set_note(self, text: str) -> None:
        self.note.configure(text=text)

    def refresh(self) -> None:
        for key, btn in self._buttons.items():
            status = self._status.get(key, "todo")
            active = key == self._current
            colour = {"done": T.OK, "blocked": T.WARN}.get(
                status, T.TEXT if active else T.TEXT_MUTED)
            btn.configure(
                text=f"  {self.MARKS.get(status, '○')}   {btn._title}",  # noqa: SLF001
                fg_color=T.SURFACE_ALT if active else "transparent",
                text_color=colour,
                font=T.FONT_H2 if active else T.FONT_BODY)
