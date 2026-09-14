"""Reusable GUI pieces: cards, fields, the path pickers, and the stage rail."""

from __future__ import annotations

from collections.abc import Callable
from pathlib import Path
from tkinter import filedialog

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
        self._subtitle = None
        if subtitle:
            self._subtitle = ctk.CTkLabel(head, text=subtitle, font=T.FONT_SMALL,
                                          text_color=T.TEXT_MUTED,
                                          justify="left", anchor="w")
            self._subtitle.grid(row=1, column=0, sticky="ew", pady=(3, 0))
            # A label will not wrap unless it is given a width, and the card's
            # width is not known until it has been laid out. Without this a long
            # subtitle runs off the right edge instead of flowing onto a second
            # line, and the end of the sentence is simply lost.
            self.bind("<Configure>", self._fit_subtitle, add="+")

        self.body = ctk.CTkFrame(self, fg_color="transparent")
        self.body.grid(row=1, column=0, sticky="nsew", padx=T.PAD, pady=(6, T.PAD))
        self.body.grid_columnconfigure(0, weight=1)

    def _fit_subtitle(self, event) -> None:
        """Keep the subtitle wrapped to the card's current width.

        The width has to be divided by the display scaling before it is handed
        over: CustomTkinter multiplies wraplength by the same factor on its way
        to the underlying label, so on a 150% display passing the measured
        pixel width asks for a wrap point half again wider than the card.
        """
        if self._subtitle is None:
            return
        width = event.width - 2 * T.PAD - 16
        if width < 120:
            return
        try:
            scaling = ctk.ScalingTracker.get_widget_scaling(self)
        except Exception:
            scaling = 1.0
        target = int(width / (scaling or 1.0))
        try:
            current = int(self._subtitle.cget("wraplength"))
        except (TypeError, ValueError):
            current = 0
        # Re-wrapping changes the label's height, which fires <Configure>
        # again; ignoring changes of a few pixels stops that becoming a loop.
        if abs(current - target) > 8:
            self._subtitle.configure(wraplength=target)


def entry(master, placeholder: str = "", width: int = 140, **kw) -> ctk.CTkEntry:
    return ctk.CTkEntry(
        master, placeholder_text=placeholder, width=width,
        font=T.FONT_BODY, text_color=T.TEXT, fg_color=T.FIELD_BG,
        border_color=T.FIELD_BORDER, border_width=1, corner_radius=6, **kw
    )


def label(master, text: str, muted: bool = False, font=None, **kw) -> ctk.CTkLabel:
    return ctk.CTkLabel(master, text=text, font=font or T.FONT_BODY,
                        text_color=T.TEXT_MUTED if muted else T.TEXT,
                        anchor="w", **kw)


def hint(master, text: str, width: int = 800) -> ctk.CTkLabel:
    """Small muted explanatory text under a control."""
    return ctk.CTkLabel(master, text=text, font=T.FONT_SMALL,
                        text_color=T.TEXT_MUTED, anchor="w", justify="left",
                        wraplength=width)


def button(master, text, command, kind: str = "primary", width: int = 120):
    """One of three kinds: the action that starts work, a secondary, a remove."""
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


def checkbox(master, text: str, command=None, variable=None) -> ctk.CTkCheckBox:
    return ctk.CTkCheckBox(master, text=text, command=command,
                           variable=variable,
                           font=T.FONT_BODY, text_color=T.TEXT,
                           fg_color=T.ACCENT, hover_color=T.ACCENT_HOVER,
                           checkmark_color=T.ACCENT_TEXT,
                           border_color=T.FIELD_BORDER,
                           corner_radius=4, border_width=2)


def combobox(master, values, variable=None, width: int = 200,
             command=None) -> ctk.CTkComboBox:
    """A dropdown that can also be typed into.

    Editable rather than a plain option menu: the label list is thirty-odd
    entries and typing three characters beats scrolling to find one. What is
    typed is still checked against the list by the caller, so a typo is
    refused rather than silently stored.
    """
    return ctk.CTkComboBox(
        master, values=list(values), variable=variable, width=width,
        command=command, font=T.FONT_BODY, dropdown_font=T.FONT_BODY,
        text_color=T.TEXT, fg_color=T.FIELD_BG, border_color=T.FIELD_BORDER,
        button_color=T.FIELD_BORDER, button_hover_color=T.ACCENT,
        dropdown_fg_color=T.SURFACE, dropdown_text_color=T.TEXT,
        dropdown_hover_color=T.SURFACE_ALT,
        border_width=1, corner_radius=6)


def textbox(master, height: int = 120) -> ctk.CTkTextbox:
    box = ctk.CTkTextbox(master, height=height, font=T.FONT_MONO,
                         fg_color=T.FIELD_BG, text_color=T.TEXT,
                         border_width=1, border_color=T.BORDER,
                         corner_radius=6, wrap="none")
    box.configure(state="disabled")
    return box


def set_text(box: ctk.CTkTextbox, text: str) -> None:
    box.configure(state="normal")
    box.delete("1.0", "end")
    box.insert("1.0", text)
    box.configure(state="disabled")


class Field(ctk.CTkFrame):
    """A labelled entry that says what its value means as you type.

    Inline feedback beats a dialog on submit: people fix a number while their
    attention is still on it. Valid shows the consequence, invalid shows what
    is wrong, empty shows nothing at all.
    """

    def __init__(self, master, caption: str, value: str = "",
                 width: int = 90, hint_text: str = "",
                 caption_width: int = 132,
                 validate: Callable[[str], tuple[bool, str]] | None = None,
                 on_change: Callable[[], None] | None = None):
        super().__init__(master, fg_color="transparent")
        self.grid_columnconfigure(2, weight=1)
        self._validate = validate
        # Wired only after the first refresh. Setting the initial value fires
        # the callback, and during __init__ the widgets the owner wants to
        # update from it do not exist yet.
        self._on_change = None

        label(self, caption, muted=True, width=caption_width).grid(
            row=0, column=0, sticky="w", padx=(0, 8))
        self.var = ctk.StringVar(value="" if value is None else str(value))
        self.entry = entry(self, hint_text, width=width, textvariable=self.var)
        self.entry.grid(row=0, column=1, sticky="w")
        self.status = ctk.CTkLabel(self, text="", font=T.FONT_SMALL,
                                   text_color=T.TEXT_MUTED, anchor="w")
        self.status.grid(row=0, column=2, sticky="ew", padx=(12, 0))
        self.var.trace_add("write", lambda *_a: self.refresh())
        self.refresh()
        self._on_change = on_change

    def get(self) -> str:
        return self.var.get().strip()

    def set(self, value) -> None:
        self.var.set("" if value is None else str(value))

    def int_value(self, fallback: int = 0) -> int:
        try:
            return int(float(self.get()))
        except ValueError:
            return fallback

    def float_value(self, fallback: float = 0.0) -> float:
        try:
            return float(self.get())
        except ValueError:
            return fallback

    def note(self, text: str, ok: bool | None = None) -> None:
        colour = T.TEXT_MUTED if ok is None else (T.OK if ok else T.WARN)
        self.status.configure(text=text, text_color=colour)

    def enable(self, on: bool) -> None:
        self.entry.configure(state="normal" if on else "disabled")

    def refresh(self, notify: bool = True) -> None:
        """Re-validate, and tell the owner unless asked not to.

        ``notify=False`` exists for the case where the owner's own change
        handler wants to refresh this field: two fields whose notes depend on
        each other otherwise call each other until Tk runs out of stack.
        """
        if self._validate is not None:
            text = self.get()
            if not text:
                self.note("")
            else:
                ok, message = self._validate(text)
                self.note(message, ok)
        if notify and self._on_change:
            self._on_change()


class PathField(ctk.CTkFrame):
    """A path, a Browse button, and a note about what is at that path."""

    def __init__(self, master, caption: str, value: str = "",
                 kind: str = "folder", title: str = "",
                 filetypes: tuple[tuple[str, str], ...] = (),
                 save: bool = False, caption_width: int = 132,
                 on_change: Callable[[str], None] | None = None):
        super().__init__(master, fg_color="transparent")
        self.grid_columnconfigure(1, weight=1)
        self._kind = kind
        self._title = title or caption
        self._filetypes = filetypes
        self._save = save
        # As with Field: wired after the first pass, because the initial value
        # fires the callback and the owner is still being built.
        self._on_change = None

        label(self, caption, muted=True, width=caption_width).grid(
            row=0, column=0, sticky="w", padx=(0, 8))
        self.var = ctk.StringVar(value=str(value or ""))
        self.entry = entry(self, "Not set", width=200, textvariable=self.var)
        self.entry.grid(row=0, column=1, sticky="ew")
        button(self, "Browse…", self.browse, "ghost", width=92
               ).grid(row=0, column=2, padx=(8, 0))
        self.status = ctk.CTkLabel(self, text="", font=T.FONT_SMALL,
                                   text_color=T.TEXT_MUTED, anchor="w",
                                   justify="left")
        self.status.grid(row=1, column=1, columnspan=2, sticky="ew", pady=(2, 0))
        self.var.trace_add("write", lambda *_a: self._changed())
        self._changed()
        self._on_change = on_change

    def get(self) -> str:
        return self.var.get().strip()

    def set(self, value) -> None:
        self.var.set(str(value or ""))

    def browse(self) -> None:
        start = self.get() or str(Path.home())
        initial_dir = str(Path(start).parent if Path(start).suffix else start)
        if self._kind == "folder":
            chosen = filedialog.askdirectory(title=self._title, initialdir=start)
        elif self._save:
            chosen = filedialog.asksaveasfilename(
                title=self._title, initialdir=initial_dir,
                initialfile=Path(start).name if Path(start).suffix else "",
                defaultextension=".csv",
                filetypes=list(self._filetypes) or None)
        else:
            chosen = filedialog.askopenfilename(
                title=self._title, initialdir=initial_dir,
                filetypes=list(self._filetypes) or None)
        if chosen:
            self.set(chosen)

    def note(self, text: str, ok: bool | None = None) -> None:
        colour = T.TEXT_MUTED if ok is None else (T.OK if ok else T.WARN)
        self.status.configure(text=text, text_color=colour)

    def _changed(self) -> None:
        text = self.get()
        if not text:
            self.note("")
        elif self._kind == "folder":
            there = Path(text).is_dir()
            self.note("" if there else "no folder there yet",
                      None if there else False)
        elif self._save:
            self.note("")
        else:
            there = Path(text).is_file()
            self.note("" if there else "no file at that path",
                      None if there else False)
        if self._on_change:
            self._on_change(text)


# --------------------------------------------------------------------------
#  Stage rail
# --------------------------------------------------------------------------


class StageRail(ctk.CTkFrame):
    """The stages down the left, showing which are done and which is next.

    Selection is marked by fill and type weight, never by colour alone: one
    text colour has to read on both states, so an accent fill would force dark
    type that then sits near 1.1:1 on the unselected rows. Status *does* use
    colour, and pairs it with a mark so colour is never the only signal.
    """

    MARKS = {"todo": "○", "active": "●", "done": "✓", "blocked": "!"}
    WIDTH = 246

    def __init__(self, master, stages: list[tuple[str, str]],
                 on_select: Callable[[str], None]):
        super().__init__(master, fg_color=T.SURFACE, corner_radius=0,
                         width=self.WIDTH)
        self.grid_propagate(False)
        self.grid_columnconfigure(0, weight=1)
        self._on_select = on_select
        self._buttons: dict[str, ctk.CTkButton] = {}
        self._titles: dict[str, str] = {}
        self._status: dict[str, str] = {}
        self._current = stages[0][0] if stages else ""

        ctk.CTkLabel(self, text="WORKFLOW", font=T.FONT_SMALL,
                     text_color=T.TEXT_MUTED, anchor="w"
                     ).grid(row=0, column=0, sticky="ew", padx=18, pady=(18, 8))

        for i, (key, title) in enumerate(stages):
            self._status[key] = "todo"
            self._titles[key] = f"{i + 1}.  {title}"
            btn = ctk.CTkButton(
                self, text="", command=lambda k=key: self._on_select(k),
                anchor="w", height=42, corner_radius=6, font=T.FONT_BODY,
                fg_color="transparent", hover_color=T.SURFACE_ALT,
                text_color=T.TEXT, border_width=0)
            btn.grid(row=i + 1, column=0, sticky="ew", padx=10, pady=2)
            self._buttons[key] = btn

        self.note = ctk.CTkLabel(
            self, text="", font=T.FONT_SMALL, text_color=T.TEXT_MUTED,
            anchor="nw", justify="left", wraplength=self.WIDTH - 44)
        self.note.grid(row=len(stages) + 1, column=0, sticky="new",
                       padx=18, pady=(16, 18))
        # Park all the slack in the note row. Without it the rail shares its
        # leftover height out among the buttons, which spreads them apart.
        self.grid_rowconfigure(len(stages) + 1, weight=1)
        self.refresh()

    def select(self, key: str) -> None:
        self._current = key
        self.refresh()

    def status(self, key: str) -> str:
        return self._status.get(key, "todo")

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
                text=f"  {self.MARKS.get(status, '○')}   {self._titles[key]}",
                fg_color=T.SURFACE_ALT if active else "transparent",
                text_color=colour,
                font=T.FONT_H2 if active else T.FONT_BODY)
