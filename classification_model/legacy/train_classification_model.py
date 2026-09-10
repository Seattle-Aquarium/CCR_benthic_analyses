#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
train_model_gui.py — Train/retrain a YOLO classification model on a merged
dataset (the kind produced by merge_and_balance_dataset.py), in the same
GUI style as the other scripts in this pipeline.

Expects a standard Ultralytics classification dataset layout:

    data_dir/train/<Label>/*.jpg
    data_dir/val/<Label>/*.jpg

This is a thin, explicit wrapper around the Ultralytics Python API
(model.train(...)) — it does not invent any training behavior of its own.
Any field left BLANK is simply not passed to model.train(), so Ultralytics'
own defaults apply for that setting rather than this script silently
picking a number.

Exceptions — a few Ultralytics classification defaults fight the fact that
every patch here is deliberately cropped centered on the annotated point
(see extract_training_patches.py), so this script always overrides them
regardless of GUI input:
    - erasing=0.0 (default 0.4) — random-erasing/cutout runs on-the-fly
      every epoch; risks blanking out the exact center feature being
      classified.
    - scale=0.0 (default 0.5) — classify training crops via
      RandomResizedCrop, which picks a random crop SIZE *and random
      POSITION*, not necessarily centered; at scale=0.5 it can and does
      shift or cut off the labeled subject. Zeroing it collapses the crop
      to the full frame, so scale/zoom variety comes only from the
      center-pivoted zoom already baked in offline by
      merge_and_balance_dataset.py's augment_image().
    - auto_augment=None (default 'randaugment') — RandAugment's policy
      (posterize/solarize/equalize/shear/translate/rotate) is tuned for
      natural ImageNet-style photos, not underwater imagery, and silently
      overrides hsv_h/hsv_s/hsv_v when active. Disabling it falls back to
      the tunable hsv_h/hsv_s/hsv_v color jitter instead.
    - flipud=0.5 (default 0.0) — these patches have no canonical "up"
      (same reasoning as the offline vertical flip), so enabling it online
      too is free extra variety.

Usage:
    python train_model_gui.py

    A window opens to select:
      1. Data folder — train/val ImageFolder root (required)
      2. Model — a base checkpoint name (e.g. yolo26s-cls.pt) or a path to
         an existing .pt to fine-tune from (required)
      3. Epochs / image size / seed (required — the run's core identity)
      4. Batch size / device / patience / project / run name (all optional
         — leave blank to use Ultralytics' own defaults)
      5. Validate dataset only checkbox — checks the folder structure and
         prints per-class train/val counts WITHOUT starting training, so
         you can catch a bad path before a multi-hour run

Requirements:
    pip install ultralytics
"""

import json
import logging
import shutil
import sys
import tkinter as tk
from tkinter import ttk, filedialog, messagebox
from pathlib import Path

# ── Logging ──────────────────────────────────────────────────────────────
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[
        logging.FileHandler("train_model_log.txt", encoding="utf-8"),
        logging.StreamHandler(sys.stdout),
    ],
)
log = logging.getLogger(__name__)

IMG_EXTS = {".jpg", ".jpeg", ".png"}

_CONFIG_PATH = Path(__file__).parent / ".train_model_config.json"


def _load_gui_config() -> dict:
    try:
        with open(_CONFIG_PATH, "r", encoding="utf-8") as f:
            return json.load(f)
    except Exception:
        return {}


def _save_gui_config(cfg: dict) -> None:
    try:
        with open(_CONFIG_PATH, "w", encoding="utf-8") as f:
            json.dump(cfg, f, indent=2)
    except Exception:
        pass


# ============================================================
# DATASET VALIDATION (no training, just a structural check)
# ============================================================
def validate_dataset(data_dir: str) -> dict:
    root = Path(data_dir)
    report = {"train": {}, "val": {}, "problems": [], "only_train": [], "only_val": []}

    for split in ("train", "val"):
        split_dir = root / split
        if not split_dir.is_dir():
            report["problems"].append(f"Missing '{split}/' folder under {data_dir}")
            continue
        for class_dir in sorted(split_dir.iterdir()):
            if not class_dir.is_dir():
                continue
            n = len([f for f in class_dir.iterdir() if f.suffix.lower() in IMG_EXTS])
            report[split][class_dir.name] = n

    train_classes = set(report["train"])
    val_classes = set(report["val"])
    only_train = sorted(train_classes - val_classes)
    only_val = sorted(val_classes - train_classes)
    report["only_train"] = only_train
    report["only_val"] = only_val
    if only_train:
        report["problems"].append(f"Classes with train images but no val folder: {only_train}")
    if only_val:
        report["problems"].append(f"Classes with val images but no train folder: {only_val}")

    empty_train = [c for c, n in report["train"].items() if n == 0]
    if empty_train:
        report["problems"].append(f"Empty train class folders: {empty_train}")

    return report


def log_validation_report(report: dict) -> None:
    log.info("=" * 70)
    log.info(f"{'Class':<15} {'train':>8} {'val':>8}")
    log.info("-" * 70)
    all_classes = sorted(set(report["train"]) | set(report["val"]))
    for cls in all_classes:
        log.info(f"{cls:<15} {report['train'].get(cls, 0):>8} {report['val'].get(cls, 0):>8}")
    log.info("-" * 70)
    total_train = sum(report["train"].values())
    total_val = sum(report["val"].values())
    log.info(f"{'TOTAL':<15} {total_train:>8} {total_val:>8}  ({len(all_classes)} classes)")
    log.info("=" * 70)

    if report["problems"]:
        for p in report["problems"]:
            log.warning(f"  \u26a0 {p}")
    else:
        log.info("Dataset structure looks consistent \u2713")


# ============================================================
# EXCLUDE / RESTORE MISMATCHED CLASSES
# ============================================================
def exclude_mismatched_classes(data_dir: str, only_train: list, only_val: list) -> list:
    """
    Temporarily MOVES (not deletes) any class folder present in only one of
    train/val out of the dataset, so Ultralytics sees a consistent class
    count on both sides. Moved folders go under
    data_dir/_excluded_from_training/<split>/<Label>/ and are restored to
    their original location by restore_excluded_classes() afterward — the
    merged dataset is never permanently altered by this.

    Returns a list of (original_path, backup_path) tuples for restoration.
    """
    moved = []
    backup_root = Path(data_dir) / "_excluded_from_training"

    for cls in only_train:
        src = Path(data_dir) / "train" / cls
        if src.is_dir():
            dst = backup_root / "train" / cls
            dst.parent.mkdir(parents=True, exist_ok=True)
            shutil.move(str(src), str(dst))
            moved.append((str(src), str(dst)))
            log.info(f"Excluded '{cls}' (train-only) from this run — "
                     f"moved to {dst}")

    for cls in only_val:
        src = Path(data_dir) / "val" / cls
        if src.is_dir():
            dst = backup_root / "val" / cls
            dst.parent.mkdir(parents=True, exist_ok=True)
            shutil.move(str(src), str(dst))
            moved.append((str(src), str(dst)))
            log.info(f"Excluded '{cls}' (val-only) from this run — "
                     f"moved to {dst}")

    return moved


def restore_excluded_classes(moved: list) -> None:
    """Moves every folder excluded by exclude_mismatched_classes() back to
    its original location. Always call this in a finally block."""
    for src, dst in moved:
        if Path(dst).is_dir():
            Path(src).parent.mkdir(parents=True, exist_ok=True)
            shutil.move(dst, src)
            log.info(f"Restored {src}")


# ============================================================
# CORE TRAINING
# ============================================================
def train_model(data_dir: str, model_name: str, epochs: int, imgsz: int, seed: int,
                 batch=None, device: str = "", patience=None,
                 project: str = "", name: str = "",
                 validate_only: bool = False, exclude_mismatched: bool = True) -> dict:
    report = validate_dataset(data_dir)
    log_validation_report(report)

    stats = {"validate_only": validate_only, "problems": report["problems"],
              "excluded_classes": []}

    if validate_only:
        return stats

    moved = []
    if exclude_mismatched and (report["only_train"] or report["only_val"]):
        moved = exclude_mismatched_classes(data_dir, report["only_train"], report["only_val"])
        stats["excluded_classes"] = sorted(set(report["only_train"]) | set(report["only_val"]))
        log.info(f"Excluded {len(stats['excluded_classes'])} mismatched class(es) "
                 f"from this run: {stats['excluded_classes']}")
    elif report["problems"]:
        log.warning("Proceeding despite the structural warnings above — "
                    "review them if training results look off.")

    from ultralytics import YOLO

    try:
        log.info(f"Loading model: {model_name}")
        model = YOLO(model_name)

        train_kwargs = {
            "data": data_dir,
            "epochs": epochs,
            "imgsz": imgsz,
            "seed": seed,
            # See "Exceptions" in the module docstring — these overrides keep
            # online (per-epoch) augmentation from working against the fact
            # that every patch is centered on its annotated point.
            "erasing": 0.0,        # no on-the-fly cutout (default 0.4)
            "scale": 0.0,          # no random off-center RandomResizedCrop (default 0.5)
            "auto_augment": None,  # no RandAugment; falls back to hsv_h/s/v jitter (default 'randaugment')
            "flipud": 0.5,         # vertical flip online too — no canonical "up" (default 0.0)
        }
        if batch not in (None, ""):
            train_kwargs["batch"] = batch
        if device:
            train_kwargs["device"] = device
        if patience not in (None, ""):
            train_kwargs["patience"] = patience
        if project:
            train_kwargs["project"] = project
        if name:
            train_kwargs["name"] = name

        log.info(f"Starting training with: {train_kwargs}")
        results = model.train(**train_kwargs)

        save_dir = getattr(results, "save_dir", None) or getattr(model.trainer, "save_dir", None)
        stats["save_dir"] = str(save_dir) if save_dir else None

        # Pull final validation metrics if available (classify task -> top1/top5)
        try:
            metrics = model.trainer.metrics
            stats["metrics"] = {k: float(v) for k, v in metrics.items()} if metrics else {}
        except Exception as e:
            log.warning(f"Could not read final metrics: {e}")
            stats["metrics"] = {}

        log.info("=" * 60)
        log.info("Training complete.")
        if stats["save_dir"]:
            log.info(f"Results saved to: {stats['save_dir']}")
            log.info(f"Best weights: {Path(stats['save_dir']) / 'weights' / 'best.pt'}")
        if stats["metrics"]:
            log.info(f"Final metrics: {stats['metrics']}")
        log.info("=" * 60)
    finally:
        if moved:
            log.info("Restoring excluded class folders to the merged dataset...")
            restore_excluded_classes(moved)

    return stats


# ============================================================
# GUI
# ============================================================
def get_args_via_gui():
    result = {}
    cfg = _load_gui_config()

    root = tk.Tk()
    root.title("Train Classification Model")
    root.resizable(False, False)
    pad = {"padx": 10, "pady": 5}

    ttk.Label(root, text="Train Classification Model",
              font=("Helvetica", 13, "bold")).grid(
        row=0, column=0, columnspan=3, pady=(14, 2), padx=14)
    ttk.Label(root,
              text="Thin wrapper around Ultralytics model.train() — blank fields use Ultralytics' own defaults",
              foreground="grey").grid(row=1, column=0, columnspan=3, pady=(0, 4))
    ttk.Separator(root, orient="horizontal").grid(
        row=2, column=0, columnspan=3, sticky="ew", padx=10, pady=4)

    # ── Data folder ──────────────────────────────────────────────────────
    ttk.Label(root, text="Data folder (train/val root):").grid(row=3, column=0, sticky="e", **pad)
    data_var = tk.StringVar(value=cfg.get("data_dir", ""))
    ttk.Entry(root, textvariable=data_var, width=55).grid(row=3, column=1, **pad)

    def browse_data():
        p = filedialog.askdirectory(title="Select dataset folder (contains train/ and val/)")
        if p:
            data_var.set(p)

    ttk.Button(root, text="Browse...", command=browse_data).grid(row=3, column=2, **pad)

    # ── Model ────────────────────────────────────────────────────────────
    ttk.Label(root, text="Model (name or path to .pt):").grid(row=4, column=0, sticky="e", **pad)
    model_var = tk.StringVar(value=cfg.get("model_name", "yolo26s-cls.pt"))
    ttk.Entry(root, textvariable=model_var, width=55).grid(row=4, column=1, **pad)

    def browse_model():
        p = filedialog.askopenfilename(
            title="Select a .pt checkpoint to fine-tune from (optional — cancel to keep typed name)",
            filetypes=[("PyTorch weights", "*.pt"), ("All files", "*.*")],
        )
        if p:
            model_var.set(p)

    ttk.Button(root, text="Browse...", command=browse_model).grid(row=4, column=2, **pad)

    ttk.Separator(root, orient="horizontal").grid(
        row=5, column=0, columnspan=3, sticky="ew", padx=10, pady=6)

    # ── Core required params ────────────────────────────────────────────
    ttk.Label(root, text="Epochs:").grid(row=6, column=0, sticky="e", **pad)
    epochs_var = tk.StringVar(value=str(cfg.get("epochs", 100)))
    ttk.Entry(root, textvariable=epochs_var, width=15).grid(row=6, column=1, sticky="w", **pad)

    ttk.Label(root, text="Image size (imgsz):").grid(row=7, column=0, sticky="e", **pad)
    imgsz_var = tk.StringVar(value=str(cfg.get("imgsz", 256)))
    ttk.Entry(root, textvariable=imgsz_var, width=15).grid(row=7, column=1, sticky="w", **pad)

    ttk.Label(root, text="Random seed:").grid(row=8, column=0, sticky="e", **pad)
    seed_var = tk.StringVar(value=str(cfg.get("seed", 42)))
    ttk.Entry(root, textvariable=seed_var, width=15).grid(row=8, column=1, sticky="w", **pad)

    ttk.Separator(root, orient="horizontal").grid(
        row=9, column=0, columnspan=3, sticky="ew", padx=10, pady=6)

    # ── Optional params (blank = Ultralytics default) ──────────────────
    ttk.Label(root, text="Optional — leave blank for Ultralytics defaults:",
              foreground="grey").grid(row=10, column=0, columnspan=3, sticky="w", padx=10)

    ttk.Label(root, text="Batch size:").grid(row=11, column=0, sticky="e", **pad)
    batch_var = tk.StringVar(value=str(cfg.get("batch", "")))
    ttk.Entry(root, textvariable=batch_var, width=15).grid(row=11, column=1, sticky="w", **pad)

    ttk.Label(root, text="Device (e.g. 'cpu' or '0'):").grid(row=12, column=0, sticky="e", **pad)
    device_var = tk.StringVar(value=cfg.get("device", ""))
    ttk.Entry(root, textvariable=device_var, width=15).grid(row=12, column=1, sticky="w", **pad)

    ttk.Label(root, text="Patience (early stopping epochs):").grid(row=13, column=0, sticky="e", **pad)
    patience_var = tk.StringVar(value=str(cfg.get("patience", "")))
    ttk.Entry(root, textvariable=patience_var, width=15).grid(row=13, column=1, sticky="w", **pad)

    ttk.Label(root, text="Project (output root folder):").grid(row=14, column=0, sticky="e", **pad)
    project_var = tk.StringVar(value=cfg.get("project", ""))
    ttk.Entry(root, textvariable=project_var, width=40).grid(row=14, column=1, sticky="w", **pad)

    ttk.Label(root, text="Run name:").grid(row=15, column=0, sticky="e", **pad)
    name_var = tk.StringVar(value=cfg.get("name", ""))
    ttk.Entry(root, textvariable=name_var, width=40).grid(row=15, column=1, sticky="w", **pad)

    ttk.Separator(root, orient="horizontal").grid(
        row=16, column=0, columnspan=3, sticky="ew", padx=10, pady=6)

    # ── Exclude mismatched classes ──────────────────────────────────────
    exclude_var = tk.BooleanVar(value=cfg.get("exclude_mismatched", True))
    ttk.Checkbutton(
        root,
        text="Exclude classes not present in both train/ and val/ from this run\n"
             "(temporarily moved aside, automatically restored afterward — nothing is deleted)",
        variable=exclude_var,
    ).grid(row=17, column=0, columnspan=3, pady=(0, 4))

    # ── Validate only ────────────────────────────────────────────────────
    validate_var = tk.BooleanVar(value=cfg.get("validate_only", True))
    ttk.Checkbutton(
        root,
        text="Validate dataset only  (check train/val structure and print class\n"
             "counts — do NOT start training)",
        variable=validate_var,
    ).grid(row=18, column=0, columnspan=3, pady=(0, 4))

    ttk.Separator(root, orient="horizontal").grid(
        row=19, column=0, columnspan=3, sticky="ew", padx=10, pady=6)

    # ── Run / Cancel buttons ────────────────────────────────────────────
    btn_frame = ttk.Frame(root)
    btn_frame.grid(row=20, column=0, columnspan=3, pady=(0, 14))

    def on_run():
        data_dir = data_var.get().strip()
        model_name = model_var.get().strip()

        if not data_dir or not Path(data_dir).is_dir():
            messagebox.showerror("Missing input", "Please select a valid data folder.")
            return
        if not model_name:
            messagebox.showerror("Missing input", "Please enter a model name or path.")
            return

        try:
            epochs = int(epochs_var.get())
        except ValueError:
            messagebox.showerror("Invalid value", "Epochs must be a whole number.")
            return

        try:
            imgsz = int(imgsz_var.get())
        except ValueError:
            messagebox.showerror("Invalid value", "Image size must be a whole number.")
            return

        try:
            seed = int(seed_var.get())
        except ValueError:
            messagebox.showerror("Invalid value", "Random seed must be a whole number.")
            return

        batch_str = batch_var.get().strip()
        batch = None
        if batch_str:
            try:
                batch = int(batch_str)
            except ValueError:
                messagebox.showerror("Invalid value", "Batch size must be a whole number.")
                return

        patience_str = patience_var.get().strip()
        patience = None
        if patience_str:
            try:
                patience = int(patience_str)
            except ValueError:
                messagebox.showerror("Invalid value", "Patience must be a whole number.")
                return

        device = device_var.get().strip()
        project = project_var.get().strip()
        name = name_var.get().strip()
        validate_only = validate_var.get()
        exclude_mismatched = exclude_var.get()

        result["data_dir"] = data_dir
        result["model_name"] = model_name
        result["epochs"] = epochs
        result["imgsz"] = imgsz
        result["seed"] = seed
        result["batch"] = batch
        result["device"] = device
        result["patience"] = patience
        result["project"] = project
        result["name"] = name
        result["validate_only"] = validate_only
        result["exclude_mismatched"] = exclude_mismatched
        result["submitted"] = True

        _save_gui_config({
            "data_dir": data_dir,
            "model_name": model_name,
            "epochs": epochs,
            "imgsz": imgsz,
            "seed": seed,
            "batch": batch_str,
            "device": device,
            "patience": patience_str,
            "project": project,
            "name": name,
            "validate_only": validate_only,
            "exclude_mismatched": exclude_mismatched,
        })
        root.destroy()

    def on_cancel():
        root.destroy()

    ttk.Button(btn_frame, text="  Run  ", command=on_run).pack(side="left", padx=8)
    ttk.Button(btn_frame, text="Cancel", command=on_cancel).pack(side="left", padx=8)

    root.mainloop()

    if not result.get("submitted"):
        print("Cancelled by user.")
        sys.exit(0)

    return result


# ============================================================
# ENTRY POINT
# ============================================================
def main():
    args = get_args_via_gui()

    log.info("Train classification model")
    log.info(f"  data dir   : {args['data_dir']}")
    log.info(f"  model      : {args['model_name']}")
    log.info(f"  epochs     : {args['epochs']}")
    log.info(f"  imgsz      : {args['imgsz']}")
    log.info(f"  seed       : {args['seed']}")
    if args["batch"] is not None:
        log.info(f"  batch      : {args['batch']}")
    if args["device"]:
        log.info(f"  device     : {args['device']}")
    if args["patience"] is not None:
        log.info(f"  patience   : {args['patience']}")
    if args["project"]:
        log.info(f"  project    : {args['project']}")
    if args["name"]:
        log.info(f"  name       : {args['name']}")
    log.info(f"  exclude mismatched classes : {args['exclude_mismatched']}")
    if args["validate_only"]:
        log.info("  mode       : VALIDATE DATASET ONLY")

    stats = train_model(
        data_dir=args["data_dir"],
        model_name=args["model_name"],
        epochs=args["epochs"],
        imgsz=args["imgsz"],
        seed=args["seed"],
        batch=args["batch"],
        device=args["device"],
        patience=args["patience"],
        project=args["project"],
        name=args["name"],
        validate_only=args["validate_only"],
        exclude_mismatched=args["exclude_mismatched"],
    )

    # ── Summary popup ────────────────────────────────────────────────────
    lines = []
    if stats["validate_only"]:
        lines.append("VALIDATE ONLY — training not started.")
        if stats["problems"]:
            lines.append("")
            lines.append("\u26a0 Issues found (see log):")
            for p in stats["problems"]:
                lines.append(f"  - {p}")
        else:
            lines.append("")
            lines.append("Dataset structure looks consistent \u2713")
        messagebox.showinfo("Validation complete", "\n".join(lines))
        return

    lines.append("Training complete.")
    if stats.get("excluded_classes"):
        lines.append("")
        lines.append(f"Excluded from this run (moved aside, restored after): "
                     f"{', '.join(stats['excluded_classes'])}")
    if stats.get("save_dir"):
        lines.append("")
        lines.append(f"Saved to:\n{stats['save_dir']}")
    if stats.get("metrics"):
        lines.append("")
        for k, v in stats["metrics"].items():
            lines.append(f"{k}: {v:.4f}")

    messagebox.showinfo("Training complete", "\n".join(lines))


if __name__ == "__main__":
    main()