"""
One panel per pipeline stage.

Each panel is a view onto its stage's config dataclass: ``load()`` fills the
widgets from the config, ``collect()`` writes them back. Nothing else in the
GUI knows what a stage's settings are, and no stage knows a GUI exists.

Every panel's commit action is guarded by a preview toggle that is checked by
default, so the first click on a new set of paths always reports what *would*
happen. Given that three of these four stages write thousands of files and the
fourth costs an hour of GPU time, the default is worth the extra click.
"""

from __future__ import annotations

from pathlib import Path

import customtkinter as ctk

from ..config import BalanceConfig, EvalConfig, ExtractConfig, TrainConfig
from . import theme as T
from .widgets import (Card, PathList, PathRow, checkbox, entry, hint, label)


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
        self.split_note.configure(
            text=("Flat <Label>/ folders. This output feeds stage 4 only - it "
                  "must never be trained on."
                  if self.no_split.get() else
                  "train/ and val/ subfolders. This output feeds stage 2."),
            text_color=T.WARN if self.no_split.get() else T.TEXT_MUTED)

    def load(self) -> None:
        cfg: ExtractConfig = self.state.extract
        self.csvs.set(cfg.annotation_csvs)
        self.out.set(cfg.output_dir)
        self.no_split.set(cfg.no_split)
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
            "Merged in order. When identical patches disagree on a label, the "
            "later dataset wins, on the assumption that it was verified more "
            "recently.")
        self.datasets = PathList(c.body, mode="folder", add_text="+ Add dataset…",
                                 empty_text="No dataset selected yet.")
        self.datasets.grid(row=0, column=0, sticky="ew")

        c = self.card("Output folder")
        self.out = PathRow(c.body, "Merged dataset", "folder")
        self.out.grid(row=0, column=0, sticky="ew")

        c = self.card(
            "Held-out set",
            "Audited against the merge, never merged in. A training patch "
            "identical to a held-out one is a leak, and it is the held-out "
            "copy that gets moved aside - dropping the training copy instead "
            "would shrink the training set to flatter the evaluation.")
        self.holdout = PathRow(c.body, "Held-out folder", "folder")
        self.holdout.grid(row=0, column=0, sticky="ew")
        self.quarantine = ctk.BooleanVar(value=True)
        checkbox(c.body, "Quarantine leaked held-out patches", self.quarantine
                 ).grid(row=1, column=0, sticky="w", pady=(10, 0))
        hint(c.body, "Moves them to <held-out>/_quarantined_leaked/. Moved, "
                     "never deleted."
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

    def load(self) -> None:
        cfg: BalanceConfig = self.state.balance
        self.datasets.set(cfg.datasets)
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

        c = self.card("Model and schedule")
        g = self.grid_body(c)
        label(g, "Base model", muted=True).grid(row=0, column=0, sticky="w",
                                                padx=(0, 6), pady=5)
        self.model = entry(g, "yolo26s-cls.pt", width=220)
        self.model.grid(row=0, column=1, sticky="w", pady=5)
        hint(g, "downloaded on first use").grid(row=0, column=2, sticky="w",
                                                padx=(12, 0))

        self.epochs = Field(g, 1, 0, "Epochs", int)
        self.patience = Field(g, 2, 0, "Patience", int,
                              note="stop after this many epochs without improving")
        self.imgsz = Field(g, 3, 0, "Image size", int,
                           note="the main driver of GPU memory use")
        self.seed = Field(g, 4, 0, "Seed", int)

        hint(c.body,
             "Ultralytics defaults patience to 100, which with 100 epochs can "
             "never fire - every run trains to the last epoch whether or not it "
             "is still learning. best.pt is the best epoch either way; patience "
             "decides how long the run continues past it."
             ).grid(row=1, column=0, sticky="w", pady=(8, 0))

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

        g = ctk.CTkFrame(c.body, fg_color="transparent")
        g.grid(row=1, column=0, sticky="ew", pady=(10, 0))
        label(g, "Run name", muted=True, width=132).grid(row=0, column=0,
                                                         sticky="w", padx=(0, 8))
        self.name = entry(g, "train", width=240)
        self.name.grid(row=0, column=1, sticky="w")
        hint(g, "a subfolder; blank uses 'train'").grid(row=0, column=2,
                                                        sticky="w", padx=(12, 0))
        self.name.bind("<KeyRelease>", lambda _e: self._preview_path())

        self.path_preview = hint(c.body, "")
        self.path_preview.grid(row=2, column=0, sticky="w", pady=(8, 0))

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
        self.model.delete(0, "end")
        self.model.insert(0, cfg.model)
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
        self.preview.set(cfg.validate_only)
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


PANELS = (ExtractPanel, BalancePanel, TrainPanel, EvaluatePanel)
