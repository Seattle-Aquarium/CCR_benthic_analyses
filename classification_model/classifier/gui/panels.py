"""
One panel per pipeline stage.

Each panel is a view onto its stage's config dataclass: ``load()`` fills the
widgets from the config, ``collect()`` writes them back. Nothing else in the
GUI knows what a stage's settings are, and no stage knows a GUI exists.

Every panel's commit action is guarded by a preview toggle that is checked by
default, so the first click on a new set of paths always reports what *would*
happen. Given that three of these stages write thousands of files and a fourth
costs an hour of GPU time, the default is worth the extra click.
"""

from __future__ import annotations

from pathlib import Path

import customtkinter as ctk

from ..config import (AUGMENTATION_PRESETS, PRESET_NOTES,
                      BalanceConfig, CompareConfig, EvalConfig,
                      ExtractConfig, TrainConfig)
from . import theme as T
from .widgets import (Card, PathList, PathRow, button, checkbox, entry,
                      hint, label)

#: Shown in the label-authority dropdown when no dataset has been nominated.
NO_AUTHORITY = "(no preference - resolved by path order)"


# --------------------------------------------------------------------------
#  Field helpers
# --------------------------------------------------------------------------

class Field:
    """A labelled entry that round-trips one typed value.

    Blank means "not set" and is returned as None, which the stages take as
    "use the library default" rather than "use zero" -- the distinction that
    keeps this GUI from quietly inventing training hyperparameters.
    """

    def __init__(self, master, row: int, column: int, caption: str,
                 kind=str, width: int = 92, placeholder: str = "",
                 note: str = ""):
        pad = (0, 6) if column == 0 else (18, 6)
        label(master, caption, muted=True).grid(
            row=row, column=column, sticky="w", padx=pad, pady=5)
        self.kind = kind
        self.entry = entry(master, placeholder, width=width)
        self.entry.grid(row=row, column=column + 1, sticky="w", pady=5)
        if note:
            hint(master, note).grid(row=row, column=column + 2, sticky="w",
                                    padx=(12, 0))

    def get(self):
        raw = self.entry.get().strip()
        if not raw:
            return None
        try:
            return self.kind(raw)
        except ValueError:
            return None

    def set(self, value) -> None:
        self.entry.delete(0, "end")
        if value not in (None, ""):
            self.entry.insert(0, str(value))


class StagePanel(ctk.CTkScrollableFrame):
    """Base class: a scrollable column of Cards for one stage."""

    key = ""
    title = ""
    subtitle = ""
    run_text = "Run"
    preview_text = "Preview only"

    def __init__(self, master, state):
        super().__init__(master, fg_color=T.BG)
        self.grid_columnconfigure(0, weight=1)
        self.state = state
        self._row = 0
        self.preview = ctk.BooleanVar(value=True)
        self.build()
        self.load()

    # ---- subclass hooks ------------------------------------------

    def build(self) -> None: ...
    def load(self) -> None: ...
    def collect(self) -> None: ...

    def validate(self) -> str | None:
        """Return a message to show instead of running, or None to proceed."""
        return None

    # ---- helpers -------------------------------------------------

    def card(self, title: str, subtitle: str = "") -> Card:
        c = Card(self, title, subtitle)
        c.grid(row=self._row, column=0, sticky="ew", pady=(0, 12))
        self._row += 1
        return c

    def grid_body(self, card: Card) -> ctk.CTkFrame:
        f = ctk.CTkFrame(card.body, fg_color="transparent")
        f.grid(row=0, column=0, sticky="ew")
        return f


# --------------------------------------------------------------------------
#  1 - Extract
# --------------------------------------------------------------------------

class ExtractPanel(StagePanel):
    key = "extract"
    title = "1.  Extract patches"
    subtitle = ("Crop a square patch around every verified annotation point. "
                "Each source image is decoded once, however many points it carries.")
    run_text = "Extract patches"

    def build(self) -> None:
        c = self.card(
            "Annotation CSVs",
            "CoralNet-Toolbox schema, as written by zooni_to_toolbox_annot.py. "
            "Several are concatenated before filtering.")
        self.csvs = PathList(c.body, mode="file", add_text="+ Add CSV…",
                             empty_text="No annotation CSV selected yet.")
        self.csvs.grid(row=0, column=0, sticky="ew")

        c = self.card("Output folder",
                      "The patch dataset is written here.")
        self.out = PathRow(c.body, "Dataset folder", "folder")
        self.out.grid(row=0, column=0, sticky="ew")

        self.no_split = ctk.BooleanVar(value=False)
        checkbox(c.body, "Held-out set - no train/val split",
                 self.no_split, command=self._split_changed
                 ).grid(row=1, column=0, sticky="w", pady=(10, 0))
        self.split_note = hint(c.body, "")
        self.split_note.grid(row=2, column=0, sticky="w", pady=(4, 0))

        # Held-out mode only: shown and hidden by _split_changed.
        self.holdout_card = self.card(
            "Keeping the held-out set independent",
            "Point at an existing held-out folder as the output and new patches "
            "are added to it; points already there are skipped. Name the "
            "training set to stay independent of, and any point on a photo "
            "that fed it is left out before anything is cut - the hash check "
            "cannot see two different points on the same photo, and they are "
            "near-duplicates.")
        self.independent_of = PathRow(self.holdout_card.body,
                                      "Independent of", "folder")
        self.independent_of.grid(row=0, column=0, sticky="ew")
        g = ctk.CTkFrame(self.holdout_card.body, fg_color="transparent")
        g.grid(row=1, column=0, sticky="ew", pady=(8, 0))
        self.holdout_target = Field(g, 0, 0, "Target per class", int,
                                    note="below this, a class's held-out F1 "
                                         "is decided by one or two images")
        hint(self.holdout_card.body,
             "The preview lists which classes would still be missing or short "
             "after this run, so the next round of annotation can be aimed at "
             "them."
             ).grid(row=2, column=0, sticky="w", pady=(8, 0))

        c = self.card("Split and quality")
        g = self.grid_body(c)
        self.val_frac = Field(g, 0, 0, "Val fraction", float, note="of each class")
        self.min_val = Field(g, 0, 3, "Min for val", int,
                             note="smaller classes go entirely to train")
        self.quality = Field(g, 1, 0, "JPEG quality", int)
        self.seed = Field(g, 1, 3, "Seed", int, note="reproduces the same split")

        c = self.card(
            "Labels to exclude",
            "Comma-separated. 'Review' is the Zooniverse outcome for "
            "'no consensus', so it is a workflow state rather than a taxon.")
        self.exclude = entry(c.body, "Review", width=460)
        self.exclude.grid(row=0, column=0, sticky="w")

        c = self.card(
            "Path remaps",
            "One OLD=>NEW per line, for folders renamed since the CSV was "
            "exported. The Windows username in each path is corrected "
            "automatically and needs no remap.")
        self.remaps = ctk.CTkTextbox(
            c.body, height=76, font=T.FONT_MONO, fg_color=T.FIELD_BG,
            text_color=T.TEXT, border_width=1, border_color=T.BORDER,
            corner_radius=6, wrap="none")
        self.remaps.grid(row=0, column=0, sticky="ew")
        hint(c.body, "e.g.  CCR_highlights=>field_highlights"
             ).grid(row=1, column=0, sticky="w", pady=(6, 0))

    def _split_changed(self) -> None:
        held_out = bool(self.no_split.get())
        self.split_note.configure(
            text=("Flat <Label>/ folders. This output feeds stage 4 only - it "
                  "must never be trained on."
                  if held_out else
                  "train/ and val/ subfolders. This output feeds stage 2."),
            text_color=T.WARN if held_out else T.TEXT_MUTED)
        if held_out:
            self.holdout_card.grid()
        else:
            self.holdout_card.grid_remove()

    def load(self) -> None:
        cfg: ExtractConfig = self.state.extract
        self.csvs.set(cfg.annotation_csvs)
        self.out.set(cfg.output_dir)
        self.no_split.set(cfg.no_split)
        self.independent_of.set(cfg.independent_of)
        self.holdout_target.set(cfg.holdout_target)
        self.val_frac.set(cfg.val_frac)
        self.min_val.set(cfg.min_val_count)
        self.quality.set(cfg.jpeg_quality)
        self.seed.set(cfg.seed)
        self.exclude.delete(0, "end")
        self.exclude.insert(0, ", ".join(cfg.exclude_labels))
        self.remaps.delete("1.0", "end")
        self.remaps.insert("1.0", "\n".join(cfg.path_remaps))
        self.preview.set(cfg.dry_run)
        self._split_changed()

    def collect(self) -> None:
        cfg: ExtractConfig = self.state.extract
        cfg.annotation_csvs = self.csvs.get()
        cfg.output_dir = self.out.get()
        cfg.no_split = bool(self.no_split.get())
        cfg.independent_of = self.independent_of.get()
        cfg.holdout_target = self.holdout_target.get() or 30
        cfg.val_frac = self.val_frac.get() or 0.2
        cfg.min_val_count = self.min_val.get() or 5
        cfg.jpeg_quality = self.quality.get() or 95
        cfg.seed = self.seed.get() if self.seed.get() is not None else 42
        cfg.exclude_labels = [s.strip() for s in self.exclude.get().split(",")
                              if s.strip()]
        cfg.path_remaps = [ln.strip() for ln in
                           self.remaps.get("1.0", "end").splitlines() if ln.strip()]
        cfg.dry_run = bool(self.preview.get())

    def validate(self) -> str | None:
        if not self.csvs.get():
            return "Add at least one annotation CSV."
        if not self.out.get():
            return "Choose an output folder for the patches."
        return None


# --------------------------------------------------------------------------
#  2 - Merge and balance
# --------------------------------------------------------------------------

class BalancePanel(StagePanel):
    key = "balance"
    title = "2.  Merge & balance"
    subtitle = ("Combine patch datasets into one training set, remove every "
                "byte-identical duplicate, then cap and augment towards balance.")
    run_text = "Merge & balance"
    preview_text = "Inventory only"

    def build(self) -> None:
        c = self.card(
            "Datasets to merge",
            "Every patch is hashed, and only one copy of each survives - so "
            "the merged set has no byte-identical pairs inside train, inside "
            "val, or across the two. The inputs themselves are never modified.")
        self.datasets = PathList(c.body, mode="folder", add_text="+ Add dataset…",
                                 empty_text="No dataset selected yet.",
                                 on_change=lambda _p: self._refresh_authority())
        self.datasets.grid(row=0, column=0, sticky="ew")

        c = self.card(
            "Conflicting labels",
            "When two byte-identical patches are filed under different "
            "classes, one annotation is wrong. The copy kept is the one that "
            "sets the label, so this chooses which dataset to believe.")
        g = ctk.CTkFrame(c.body, fg_color="transparent")
        g.grid(row=0, column=0, sticky="ew")
        label(g, "Trust the labels in", muted=True, width=132).grid(
            row=0, column=0, sticky="w", padx=(0, 8))
        self.authority = ctk.CTkOptionMenu(
            g, values=[NO_AUTHORITY], width=340, font=T.FONT_BODY,
            corner_radius=6, fg_color=T.FIELD_BG, button_color=T.FIELD_BORDER,
            button_hover_color=T.ACCENT, text_color=T.TEXT,
            dropdown_font=T.FONT_BODY, dropdown_fg_color=T.SURFACE,
            dropdown_text_color=T.TEXT, dropdown_hover_color=T.SURFACE_ALT)
        self.authority.grid(row=0, column=1, sticky="w")
        hint(c.body,
             "Left unset, the surviving label is whichever sorts first by "
             "path - arbitrary, and not something to leave to chance when the "
             "datasets disagree."
             ).grid(row=1, column=0, sticky="w", pady=(8, 0))

        c = self.card(
            "Output folder",
            "The merged dataset, plus duplicate_patches_report.csv listing "
            "every duplicate group found and how it was resolved.")
        self.out = PathRow(c.body, "Merged dataset", "folder")
        self.out.grid(row=0, column=0, sticky="ew")

        c = self.card(
            "Held-out set",
            "Audited against the merge, never merged in. A held-out patch "
            "identical to a training one is a leak: the training copy is kept "
            "and used, and it is the held-out copy that is removed - dropping "
            "the training copy instead would shrink the training set to "
            "flatter the evaluation.")
        self.holdout = PathRow(c.body, "Held-out folder", "folder")
        self.holdout.grid(row=0, column=0, sticky="ew")
        self.quarantine = ctk.BooleanVar(value=True)
        checkbox(c.body, "Quarantine leaked held-out patches", self.quarantine
                 ).grid(row=1, column=0, sticky="w", pady=(10, 0))
        hint(c.body,
             "Moves them to <held-out>/_quarantined_leaked/<class>/. This is "
             "the only file the pipeline ever moves; everything else is a "
             "copy into the output folder. Moved, never deleted."
             ).grid(row=2, column=0, sticky="w", pady=(4, 0))

        c = self.card(
            "Balance",
            "Applied to train/ only. val/ is merged exactly as it is, so this "
            "round's validation number stays comparable with the last.")
        g = self.grid_body(c)
        self.cap = Field(g, 0, 0, "Cap", int,
                         note="undersample classes above this; blank = no cap")
        self.floor = Field(g, 1, 0, "Floor", int,
                           note="augment classes below this; blank = no floor")
        self.seed = Field(g, 2, 0, "Seed", int)
        hint(c.body, "Run an inventory first - it prints the min, median and "
                     "max class size to choose these from."
             ).grid(row=1, column=0, sticky="w", pady=(8, 0))

        c = self.card(
            "Per-class cap overrides",
            "One CLASS=COUNT per line, for classes where the global cap throws "
            "away information that matters.")
        self.class_caps = ctk.CTkTextbox(
            c.body, height=76, font=T.FONT_MONO, fg_color=T.FIELD_BG,
            text_color=T.TEXT, border_width=1, border_color=T.BORDER,
            corner_radius=6, wrap="none")
        self.class_caps.grid(row=0, column=0, sticky="ew")
        hint(c.body, "e.g.  KE_sieve=4000").grid(row=1, column=0, sticky="w",
                                                 pady=(6, 0))

    def _refresh_authority(self, want: str | None = None) -> None:
        """Keep the dropdown in step with the dataset list.

        Datasets are shown by folder name because the Dropbox prefix they
        share is the part that does not distinguish them; the full path is
        what gets stored.
        """
        # PathList fires on_change from its constructor, before the dropdown
        # below it has been built. Nothing to sync yet in that case.
        if not hasattr(self, "authority"):
            return
        want = self._authority_path() if want is None else want
        self._authority_paths = list(self.datasets.get())
        names = [Path(d).name or d for d in self._authority_paths]
        # Two batches can share a folder name; disambiguate with the parent.
        for i, n in enumerate(names):
            if names.count(n) > 1:
                names[i] = str(Path(self._authority_paths[i]).parent.name) + "/" + n
        self.authority.configure(values=[NO_AUTHORITY] + names)
        keep = next((n for d, n in zip(self._authority_paths, names)
                     if want and _same_path(d, want)), NO_AUTHORITY)
        self.authority.set(keep)

    def _authority_path(self) -> str:
        """The full path behind the dropdown's current selection."""
        chosen = self.authority.get() if hasattr(self, "authority") else ""
        if not chosen or chosen == NO_AUTHORITY:
            return ""
        values = list(self.authority.cget("values"))
        try:
            return self._authority_paths[values.index(chosen) - 1]
        except (ValueError, IndexError, AttributeError):
            return ""

    def load(self) -> None:
        cfg: BalanceConfig = self.state.balance
        self.datasets.set(cfg.datasets)
        self._refresh_authority(cfg.label_authority)
        self.out.set(cfg.output_dir)
        self.holdout.set(cfg.holdout_dir)
        self.quarantine.set(cfg.quarantine_holdout_leaks)
        self.cap.set(cfg.cap)
        self.floor.set(cfg.floor)
        self.seed.set(cfg.seed)
        self.class_caps.delete("1.0", "end")
        self.class_caps.insert("1.0", "\n".join(
            f"{k}={v}" for k, v in sorted(cfg.class_caps.items())))
        self.preview.set(cfg.inventory_only)

    def collect(self) -> None:
        cfg: BalanceConfig = self.state.balance
        cfg.datasets = self.datasets.get()
        cfg.label_authority = self._authority_path()
        cfg.output_dir = self.out.get()
        cfg.holdout_dir = self.holdout.get()
        cfg.quarantine_holdout_leaks = bool(self.quarantine.get())
        cfg.cap = self.cap.get()
        cfg.floor = self.floor.get()
        cfg.seed = self.seed.get() if self.seed.get() is not None else 42
        cfg.class_caps = _parse_class_caps(self.class_caps.get("1.0", "end"))
        cfg.inventory_only = bool(self.preview.get())

    def validate(self) -> str | None:
        if not self.datasets.get():
            return "Add at least one dataset to merge."
        if not self.preview.get() and not self.out.get():
            return "Choose an output folder for the merged dataset."
        return None


def _parse_extra_args(text: str) -> dict:
    """KEY=VALUE lines into a dict, with values typed the way YAML would.

    Ultralytics distinguishes 0.0 from False and from "none", so the strings
    are converted rather than passed through -- ``cos_lr=True`` has to arrive
    as a bool to do anything.
    """
    out: dict = {}
    for line in text.splitlines():
        line = line.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        key, _, raw = line.partition("=")
        key, raw = key.strip(), raw.strip()
        if not key:
            continue
        lowered = raw.lower()
        if lowered in ("true", "false"):
            out[key] = lowered == "true"
        elif lowered in ("none", "null", ""):
            out[key] = None
        else:
            try:
                out[key] = int(raw) if raw.lstrip("-").isdigit() else float(raw)
            except ValueError:
                out[key] = raw
    return out


def _same_path(a: str, b: str) -> bool:
    try:
        return Path(a).resolve() == Path(b).resolve()
    except OSError:
        return str(a).strip() == str(b).strip()


def _parse_class_caps(text: str) -> dict[str, int]:
    out: dict[str, int] = {}
    for line in text.splitlines():
        line = line.strip()
        if not line or "=" not in line:
            continue
        name, _, value = line.partition("=")
        try:
            out[name.strip()] = int(value.strip())
        except ValueError:
            continue
    return out


# --------------------------------------------------------------------------
#  3 - Train
# --------------------------------------------------------------------------

class TrainPanel(StagePanel):
    key = "train"
    title = "3.  Train model"
    subtitle = ("Fine-tune a YOLO classification model, stopping once it stops "
                "improving. Leave a field blank to use the Ultralytics default.")
    run_text = "Start training"
    preview_text = "Validate only"

    def build(self) -> None:
        c = self.card("Dataset",
                      "The merged dataset from stage 2, with train/ and val/.")
        self.data = PathRow(c.body, "Dataset folder", "folder")
        self.data.grid(row=0, column=0, sticky="ew")

        self.exclude_mismatched = ctk.BooleanVar(value=True)
        checkbox(c.body, "Set aside classes missing from either split",
                 self.exclude_mismatched
                 ).grid(row=1, column=0, sticky="w", pady=(10, 0))
        hint(c.body,
             "Ultralytics needs identical class folders in train and val. Given "
             "a mismatch it does not raise - it trains to chance accuracy. "
             "Folders are moved aside for the run and restored afterwards."
             ).grid(row=2, column=0, sticky="w", pady=(4, 0))

        c = self.card(
            "Notes for the record",
            "What is different about this run, and why you are trying it. "
            "Written into the run folder beside the weights, and read back by "
            "stage 5 - so the comparison months from now says what each model "
            "was for, not only how it scored.")
        self.notes = ctk.CTkTextbox(
            c.body, height=72, font=T.FONT_BODY, fg_color=T.FIELD_BG,
            text_color=T.TEXT, border_width=1, border_color=T.BORDER,
            corner_radius=6, wrap="word")
        self.notes.grid(row=0, column=0, sticky="ew")
        hint(c.body, "e.g.  Raised KE_sieve cap to 4000 after it scored 0.21; "
                     "everything else as 08_24."
             ).grid(row=1, column=0, sticky="w", pady=(6, 0))

        c = self.card(
            "Starting weights",
            "A stock Ultralytics name (yolo26s-cls.pt, downloaded on first use) "
            "starts from ImageNet features. Browse to a best.pt from an earlier "
            "run to continue from what that model already knows.")
        self.model = PathRow(c.body, "Base model", "file",
                             filetypes=[("PyTorch weights", "*.pt"),
                                        ("All files", "*.*")])
        self.model.grid(row=0, column=0, sticky="ew")
        hint(c.body,
             "Starting from an earlier model means its training data counts "
             "as seen by this one. The held-out set must be independent of "
             "every dataset in that lineage, not only the one trained on here."
             ).grid(row=1, column=0, sticky="w", pady=(8, 0))

        c = self.card("Schedule")
        g = self.grid_body(c)

        self.epochs = Field(g, 0, 0, "Epochs", int)
        self.patience = Field(g, 1, 0, "Patience", int,
                              note="stop after this many epochs without improving")
        self.imgsz = Field(g, 2, 0, "Image size", int,
                           note="the main driver of GPU memory use")
        self.seed = Field(g, 3, 0, "Seed", int)

        hint(c.body,
             "Ultralytics defaults patience to 100, which with 100 epochs can "
             "never fire - every run trains to the last epoch whether or not it "
             "is still learning. best.pt is the best epoch either way; patience "
             "decides how long the run continues past it."
             ).grid(row=1, column=0, sticky="w", pady=(8, 0))

        c = self.card(
            "Classes to leave out",
            "Comma-separated class names to hold out of this run - a workflow "
            "state like 'unknown', or a taxon not worth modelling. The folders "
            "are moved aside for the run and put back afterwards.")
        self.exclude_classes = entry(c.body, "unknown", width=460)
        self.exclude_classes.grid(row=0, column=0, sticky="w")
        hint(c.body,
             "Renaming a folder or nesting it inside another does not work: "
             "every directory inside train/ is a class to Ultralytics, "
             "whatever it is called. A folder called '_dont_train' becomes a "
             "class named '_dont_train'."
             ).grid(row=1, column=0, sticky="w", pady=(6, 0))

        c = self.card(
            "Augmentation and regularisation",
            "How much each training image is varied between epochs, and how "
            "hard the model is held back from memorising. This is the setting "
            "to change when a run overfits or underfits.")
        g = ctk.CTkFrame(c.body, fg_color="transparent")
        g.grid(row=0, column=0, sticky="ew")
        label(g, "Preset", muted=True, width=132).grid(row=0, column=0,
                                                       sticky="w", padx=(0, 8))
        self.augmentation = ctk.CTkOptionMenu(
            g, values=list(AUGMENTATION_PRESETS), width=220, font=T.FONT_BODY,
            corner_radius=6, fg_color=T.FIELD_BG, button_color=T.FIELD_BORDER,
            button_hover_color=T.ACCENT, text_color=T.TEXT,
            dropdown_font=T.FONT_BODY, dropdown_fg_color=T.SURFACE,
            dropdown_text_color=T.TEXT, dropdown_hover_color=T.SURFACE_ALT,
            command=lambda _v: self._preset_changed())
        self.augmentation.grid(row=0, column=1, sticky="w")

        self.preset_note = hint(c.body, "")
        self.preset_note.grid(row=1, column=0, sticky="w", pady=(8, 0))
        self.preset_values = hint(c.body, "")
        self.preset_values.grid(row=2, column=0, sticky="w", pady=(4, 0))

        hint(c.body,
             "Patches are centred on the annotated point, so anything that "
             "can crop, shift or erase the middle of the frame changes what "
             "the image shows without changing its label. Every preset except "
             "'ultralytics defaults' keeps those off."
             ).grid(row=3, column=0, sticky="w", pady=(8, 0))

        c = self.card(
            "Anything else",
            "One KEY=VALUE per line, passed straight to Ultralytics and "
            "applied after the preset. Checked before the run starts.")
        self.extra_args = ctk.CTkTextbox(
            c.body, height=64, font=T.FONT_MONO, fg_color=T.FIELD_BG,
            text_color=T.TEXT, border_width=1, border_color=T.BORDER,
            corner_radius=6, wrap="none")
        self.extra_args.grid(row=0, column=0, sticky="ew")
        hint(c.body, "e.g.  dropout=0.3      cos_lr=True      lr0=0.005"
             ).grid(row=1, column=0, sticky="w", pady=(6, 0))

        c = self.card("Hardware", "Blank uses the Ultralytics default.")
        g = self.grid_body(c)
        self.batch = Field(g, 0, 0, "Batch", int, note="blank = auto (16)")
        label(g, "Device", muted=True).grid(row=1, column=0, sticky="w",
                                            padx=(0, 6), pady=5)
        self.device = entry(g, "auto", width=92)
        self.device.grid(row=1, column=1, sticky="w", pady=5)
        hint(g, "blank = auto,  0 = first GPU,  cpu = force CPU"
             ).grid(row=1, column=2, sticky="w", padx=(12, 0))
        hint(c.body,
             "On out-of-memory, reduce Image size first - it drives the "
             "allocation far more than Batch does on this machine. Device 'cpu' "
             "always works, slowly."
             ).grid(row=1, column=0, sticky="w", pady=(8, 0))

        c = self.card(
            "Output",
            "Weights and plots are written to a run folder inside the output "
            "folder, so several attempts at one model stay together.")
        self.project = PathRow(c.body, "Output folder", "folder",
                               on_change=lambda _p: self._preview_path())
        self.project.grid(row=0, column=0, sticky="ew")

        s = ctk.CTkFrame(c.body, fg_color="transparent")
        s.grid(row=1, column=0, sticky="ew", pady=(6, 0))
        button(s, "Suggest a name", self._suggest_name, "ghost", width=150
               ).grid(row=0, column=0, sticky="w")
        self.suggest_note = hint(s, "")
        self.suggest_note.grid(row=0, column=1, sticky="w", padx=(12, 0))

        g = ctk.CTkFrame(c.body, fg_color="transparent")
        g.grid(row=2, column=0, sticky="ew", pady=(10, 0))
        label(g, "Run name", muted=True, width=132).grid(row=0, column=0,
                                                         sticky="w", padx=(0, 8))
        self.name = entry(g, "train", width=240)
        self.name.grid(row=0, column=1, sticky="w")
        hint(g, "a subfolder; blank uses 'train'").grid(row=0, column=2,
                                                        sticky="w", padx=(12, 0))
        self.name.bind("<KeyRelease>", lambda _e: self._preview_path())

        self.path_preview = hint(c.body, "")
        self.path_preview.grid(row=3, column=0, sticky="w", pady=(8, 0))

    def _suggest_name(self) -> None:
        """Fill the output folder by the naming convention.

        YYYY_MM_DD_<dataset>_<preset>, under the models root from stage 5 --
        the same folder stage 5 scans, so a run named this way joins the
        history without being pointed at. The date sorts it; the dataset says
        what it was trained on; the preset is the most likely thing varied.
        The operator trims the tail to whatever this run is really testing.
        """
        from datetime import date

        root = (self.state.compare.models_root.strip()
                or str(Path(self.project.get()).parent) if self.project.get()
                else "")
        if not root:
            self.suggest_note.configure(
                text="Set a models root on stage 5 first, so the name has "
                     "somewhere to go.", text_color=T.WARN)
            return

        dataset = Path(self.data.get()).name if self.data.get() else "dataset"
        # A dataset already carries its own date; two dates in one name says
        # nothing the folder listing does not.
        parts = dataset.split("_")
        if len(parts) > 3 and all(p.isdigit() for p in parts[:3]):
            dataset = "_".join(parts[3:]) or dataset
        preset = self.augmentation.get().replace(" ", "_")
        name = f"{date.today():%Y_%m_%d}_{dataset}_{preset}"

        self.project.set(str(Path(root) / name))
        self._preview_path()
        self.suggest_note.configure(
            text="Trim the tail to the one thing this run is testing - "
                 "the full recipe is recorded in run_notes.md.",
            text_color=T.TEXT_MUTED)

    def _preset_changed(self) -> None:
        """Show what the chosen preset actually does, rather than its name."""
        name = self.augmentation.get()
        self.preset_note.configure(
            text=PRESET_NOTES.get(name, ""),
            text_color=T.WARN if name == "ultralytics defaults" else T.TEXT_MUTED)
        values = AUGMENTATION_PRESETS.get(name, {})
        interesting = ("hsv_s", "degrees", "translate", "mixup", "dropout",
                       "weight_decay", "erasing", "scale")
        shown = [f"{k}={values[k]}" for k in interesting if k in values]
        self.preset_values.configure(
            text=("Ultralytics decides everything." if not shown
                  else "Sets  " + "   ".join(shown)))

    def _preview_path(self) -> None:
        """Show exactly where best.pt will land.

        The output folder and run name combine into a path rather than either
        one being 'the' destination, which is not guessable from two labelled
        boxes -- so it is spelled out instead of explained.
        """
        folder = self.project.get()
        name = self.name.get().strip() or "train"
        if not folder:
            self.path_preview.configure(
                text="Choose an output folder to see where the weights will go.",
                text_color=T.TEXT_MUTED)
            return
        existing = Path(folder) / name
        note = ("  (this run folder already exists - Ultralytics will add a "
                f"suffix, e.g. {name}2)" if existing.is_dir() else "")
        self.path_preview.configure(
            text=f"Best weights → {existing / 'weights' / 'best.pt'}{note}",
            text_color=T.WARN if note else T.TEXT_MUTED)

    def load(self) -> None:
        cfg: TrainConfig = self.state.train
        self.data.set(cfg.data_dir)
        self.exclude_mismatched.set(cfg.exclude_mismatched)
        self.model.set(cfg.model)
        self.epochs.set(cfg.epochs)
        self.patience.set(cfg.patience)
        self.imgsz.set(cfg.imgsz)
        self.seed.set(cfg.seed)
        self.batch.set(cfg.batch)
        self.device.delete(0, "end")
        self.device.insert(0, cfg.device)
        self.project.set(cfg.project)
        self.name.delete(0, "end")
        self.name.insert(0, cfg.name)
        self.exclude_classes.delete(0, "end")
        self.exclude_classes.insert(0, ", ".join(cfg.exclude_classes or []))
        self.notes.delete("1.0", "end")
        self.notes.insert("1.0", cfg.notes)
        self.augmentation.set(cfg.augmentation if cfg.augmentation
                              in AUGMENTATION_PRESETS else "standard")
        self.extra_args.delete("1.0", "end")
        self.extra_args.insert("1.0", "\n".join(
            f"{k}={v}" for k, v in sorted((cfg.extra_args or {}).items())))
        self.preview.set(cfg.validate_only)
        self._preset_changed()
        self._preview_path()

    def collect(self) -> None:
        cfg: TrainConfig = self.state.train
        cfg.data_dir = self.data.get()
        cfg.exclude_mismatched = bool(self.exclude_mismatched.get())
        cfg.model = self.model.get().strip() or "yolo26s-cls.pt"
        cfg.epochs = self.epochs.get() or 100
        cfg.patience = self.patience.get() if self.patience.get() is not None else 20
        cfg.imgsz = self.imgsz.get() or 256
        cfg.seed = self.seed.get() if self.seed.get() is not None else 42
        cfg.batch = self.batch.get()
        cfg.device = self.device.get().strip()
        cfg.project = self.project.get()
        cfg.name = self.name.get().strip()
        cfg.exclude_classes = [c.strip() for c
                               in self.exclude_classes.get().split(",")
                               if c.strip()]
        cfg.notes = self.notes.get("1.0", "end").strip()
        cfg.augmentation = self.augmentation.get()
        cfg.extra_args = _parse_extra_args(self.extra_args.get("1.0", "end"))
        cfg.validate_only = bool(self.preview.get())

    def validate(self) -> str | None:
        if not self.data.get():
            return "Choose the merged dataset to train on."
        return None


# --------------------------------------------------------------------------
#  4 - Evaluate
# --------------------------------------------------------------------------

class EvaluatePanel(StagePanel):
    key = "evaluate"
    title = "4.  Evaluate on held-out data"
    subtitle = ("Score the model against transects it has never seen, using the "
                "same columns as the original field-verified accuracy report.")
    run_text = "Evaluate"

    def build(self) -> None:
        c = self.card("Model", "best.pt from stage 3 - the best epoch, not the last.")
        self.model = PathRow(c.body, "Weights", "file",
                             filetypes=[("PyTorch weights", "*.pt"),
                                        ("All files", "*.*")])
        self.model.grid(row=0, column=0, sticky="ew")

        c = self.card(
            "Held-out set",
            "A flat <Label>/*.jpg folder from stage 1 with 'Held-out set' "
            "ticked. Never trained or validated on.")
        self.eval_dir = PathRow(c.body, "Held-out folder", "folder")
        self.eval_dir.grid(row=0, column=0, sticky="ew")

        c = self.card(
            "Independence check",
            "Hashes the held-out patches against the training set before "
            "reporting anything. Last time this was skipped, 12.5% of the "
            "held-out set had leaked and the reported 65.85% was really 62.81%.")
        self.verify = ctk.BooleanVar(value=True)
        checkbox(c.body, "Verify the held-out set is unseen", self.verify
                 ).grid(row=0, column=0, sticky="w")
        self.train_dir = PathRow(c.body, "Training dataset", "folder")
        self.train_dir.grid(row=1, column=0, sticky="ew", pady=(10, 0))
        hint(c.body, "The stage refuses to report a number if any held-out "
                     "image is byte-identical to a training image."
             ).grid(row=2, column=0, sticky="w", pady=(6, 0))

        c = self.card("Output")
        self.out = PathRow(c.body, "Reports folder", "folder")
        self.out.grid(row=0, column=0, sticky="ew")
        g = ctk.CTkFrame(c.body, fg_color="transparent")
        g.grid(row=1, column=0, sticky="ew", pady=(10, 0))
        label(g, "Label", muted=True).grid(row=0, column=0, sticky="w",
                                           padx=(0, 6))
        self.dataset_label = entry(g, "e.g. 2026_08_19_holdout", width=300)
        self.dataset_label.grid(row=0, column=1, sticky="w")
        hint(g, "prefixes the report filenames").grid(row=0, column=2,
                                                     sticky="w", padx=(12, 0))

        c = self.card("Inference")
        g = self.grid_body(c)
        self.imgsz = Field(g, 0, 0, "Image size", int,
                           note="match what the model was trained at")
        self.batch = Field(g, 1, 0, "Batch", int)
        label(g, "Device", muted=True).grid(row=2, column=0, sticky="w",
                                            padx=(0, 6), pady=5)
        self.device = entry(g, "auto", width=92)
        self.device.grid(row=2, column=1, sticky="w", pady=5)
        self.fp16 = ctk.BooleanVar(value=False)
        checkbox(c.body, "FP16 inference", self.fp16
                 ).grid(row=1, column=0, sticky="w", pady=(10, 0))

        c = self.card("Baseline",
                      "Shown beside the result so the comparison is never made "
                      "from memory.")
        g = self.grid_body(c)
        self.baseline = Field(g, 0, 0, "Field-verified %", float, width=110)

    def load(self) -> None:
        cfg: EvalConfig = self.state.evaluate
        self.model.set(cfg.model_path)
        self.eval_dir.set(cfg.eval_dir)
        self.verify.set(cfg.verify_independence)
        self.train_dir.set(cfg.train_dataset_dir)
        self.out.set(cfg.output_dir)
        self.dataset_label.delete(0, "end")
        self.dataset_label.insert(0, cfg.dataset_label)
        self.imgsz.set(cfg.imgsz)
        self.batch.set(cfg.batch)
        self.device.delete(0, "end")
        self.device.insert(0, cfg.device)
        self.fp16.set(cfg.fp16)
        self.baseline.set(cfg.baseline_accuracy)
        self.preview.set(cfg.preview_only)

    def collect(self) -> None:
        cfg: EvalConfig = self.state.evaluate
        cfg.model_path = self.model.get()
        cfg.eval_dir = self.eval_dir.get()
        cfg.verify_independence = bool(self.verify.get())
        cfg.train_dataset_dir = self.train_dir.get()
        cfg.output_dir = self.out.get()
        cfg.dataset_label = self.dataset_label.get().strip()
        cfg.imgsz = self.imgsz.get() or 256
        cfg.batch = self.batch.get() or 32
        cfg.device = self.device.get().strip()
        cfg.fp16 = bool(self.fp16.get())
        cfg.baseline_accuracy = self.baseline.get()
        cfg.preview_only = bool(self.preview.get())

    def validate(self) -> str | None:
        if not self.eval_dir.get():
            return "Choose the held-out folder to evaluate against."
        if not self.preview.get() and not self.model.get():
            return "Choose the model weights to evaluate."
        return None



# --------------------------------------------------------------------------
#  5 - Compare
# --------------------------------------------------------------------------

class ComparePanel(StagePanel):
    key = "compare"
    title = "5.  Compare models"
    subtitle = ("Rank candidate models on the same held-out images, per class "
                "as well as overall, and say which one to move forward with.")
    run_text = "Compare models"
    preview_text = "Preview only"

    def build(self) -> None:
        c = self.card(
            "Models to compare",
            "Each entry is a model's output folder from stage 3 -- the one "
            "stage 4 also wrote its reports into. A run only joins the "
            "ranking once it has been evaluated on the held-out set.")
        self.runs = PathList(c.body, mode="folder", add_text="+ Add model…",
                             empty_text="No model selected yet - or set a "
                                        "models root below.")
        self.runs.grid(row=0, column=0, sticky="ew")
        hint(c.body,
             "Models are only ranked against each other when they were "
             "evaluated on the same held-out images. Different evaluation "
             "sets are reported, and the ranking is withheld rather than "
             "quietly comparing numbers that do not mean the same thing."
             ).grid(row=1, column=0, sticky="w", pady=(10, 0))

        c = self.card(
            "Models root",
            "Optional. Every run folder beneath it is compared automatically, "
            "so this is 'compare everything I have trained' in one field. The "
            "record lives here too: model_history.md, regenerated from the run "
            "folders each time, and decisions.md, which only ever grows.")
        self.models_root = PathRow(c.body, "Models root", "folder")
        self.models_root.grid(row=0, column=0, sticky="ew")
        hint(c.body,
             "An archive/ folder beneath it is skipped. Runs without a stage 4 "
             "evaluation appear in the history but not the ranking."
             ).grid(row=1, column=0, sticky="w", pady=(8, 0))

        c = self.card(
            "Report",
            "One workbook plus two figures: the ranking and why, per-class F1 "
            "for every model, the training curves, and what to try next.")
        self.out = PathRow(c.body, "Output folder", "folder")
        self.out.grid(row=0, column=0, sticky="ew")
        hint(c.body,
             "Writes model_comparison.xlsx, model_comparison_curves.png and "
             "model_comparison_per_class.png. With no models root set, the "
             "history and decisions files land here as well."
             ).grid(row=1, column=0, sticky="w", pady=(8, 0))

    def load(self) -> None:
        cfg: CompareConfig = self.state.compare
        self.runs.set(cfg.runs)
        self.models_root.set(cfg.models_root)
        self.out.set(cfg.output_dir)
        self.preview.set(cfg.preview_only)

    def collect(self) -> None:
        cfg: CompareConfig = self.state.compare
        cfg.runs = self.runs.get()
        cfg.models_root = self.models_root.get()
        cfg.output_dir = self.out.get()
        cfg.preview_only = bool(self.preview.get())

    def validate(self) -> str | None:
        if len(self.runs.get()) < 2 and not self.models_root.get():
            return ("Add at least two model folders, or set a models root - "
                    "a comparison needs something to compare against.")
        if not self.preview.get() and not self.out.get():
            return "Choose an output folder for the comparison report."
        return None

PANELS = (ExtractPanel, BalancePanel, TrainPanel, EvaluatePanel, ComparePanel)
