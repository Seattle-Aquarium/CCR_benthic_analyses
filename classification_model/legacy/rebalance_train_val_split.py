#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
rebalance_train_val_split.py — restore the train/val split ratio for an
existing ImageFolder-style dataset (dataset/{train,val}/<Label>/*.jpg).

Meant to be run after something has changed the per-class file counts
without touching the split ratio deliberately — most commonly, after
remove_duplicate_images.py deleted (or moved out) some train/ or val/
images and left the two splits off the original target ratio.

For each class, this recomputes what the train/val split WOULD look like
if extract_training_patches.py's assign_splits() were run fresh against
the current combined set of files for that class (same algorithm: single
seeded RNG, classes visited in alphabetical order, per-class shuffle),
then moves only the files whose split actually needs to change to reach
that target — not a full re-shuffle, minimal churn.

Classes with fewer than <min val count> total files are kept 100% in
train, same as assign_splits().

Optionally, after the move, train-only augmentation (same method as
merge_and_balance_dataset.py's floor logic — horizontal + vertical flip,
small rotation, multi-scale zoom, hue/saturation jitter, brightness/
contrast jitter, never an exact duplicate) tops any class whose train
count is still below a floor back up. val is never augmented and never
touched by this step, for the same reason merge_and_balance_dataset.py
excludes it: an augmented copy of a val image is a near-duplicate of a
real image, which would leak between splits if the two ever ended up on
opposite sides.

Usage:
    python rebalance_train_val_split.py

    A window opens to select:
      1. Dataset root folder — must contain train/ and val/ subfolders
         (required)
      2. Target val fraction / min val count / seed (defaults match
         extract_training_patches.py: 0.15 / 5 / 42)
      3. Floor — min train images per class after rebalancing; classes
         below it are topped up with augmented copies (optional, blank
         to disable)
      4. Dry run checkbox (default ON — report only, no files moved or
         augmented)

Requirements:
    pip install opencv-python-headless numpy
"""

import csv
import json
import logging
import os
import random
import shutil
import sys
import tkinter as tk
from tkinter import ttk, filedialog, messagebox
from pathlib import Path

import cv2
import numpy as np

# ── Logging ──────────────────────────────────────────────────────────────
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[
        logging.FileHandler("rebalance_train_val_split_log.txt", encoding="utf-8"),
        logging.StreamHandler(sys.stdout),
    ],
)
log = logging.getLogger(__name__)

_CONFIG_PATH = Path(__file__).parent / ".rebalance_train_val_split_config.json"
IMG_EXTS = {".jpg", ".jpeg", ".png"}


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
# INVENTORY
# ============================================================
def list_class_images(dataset_root: str, split: str) -> dict:
    """Return {class_name: [file_path, ...]} for a given split folder."""
    split_dir = Path(dataset_root) / split
    result = {}
    if not split_dir.is_dir():
        return result
    for class_dir in sorted(split_dir.iterdir()):
        if not class_dir.is_dir():
            continue
        files = [str(f) for f in class_dir.iterdir()
                 if f.suffix.lower() in IMG_EXTS]
        if files:
            result[class_dir.name] = files
    return result


def unique_dest(dest_dir: str, filename: str) -> str:
    base, ext = os.path.splitext(filename)
    candidate = filename
    n = 2
    while os.path.exists(os.path.join(dest_dir, candidate)):
        candidate = f"{base}__{n}{ext}"
        n += 1
    return os.path.join(dest_dir, candidate)


# ============================================================
# TARGET SPLIT (same algorithm as extract_training_patches.assign_splits,
# but driven off {class: [current train + val files]} instead of a df)
# ============================================================
def compute_target_split(train_files: dict, val_files: dict, val_frac: float,
                          min_val_count: int, seed: int) -> dict:
    """
    Returns {class: {"val": set(paths), "train": set(paths)}} — the target
    membership for every file currently in train or val for that class.
    """
    rng = random.Random(seed)
    all_classes = sorted(set(train_files) | set(val_files))
    target = {}

    for cls in all_classes:
        combined = list(train_files.get(cls, [])) + list(val_files.get(cls, []))
        rng.shuffle(combined)
        n = len(combined)

        if n < min_val_count:
            target[cls] = {"val": set(), "train": set(combined)}
            continue

        n_val = max(1, round(n * val_frac))
        target[cls] = {
            "val": set(combined[:n_val]),
            "train": set(combined[n_val:]),
        }

    return target


def plan_moves(train_files: dict, val_files: dict, target: dict) -> list:
    """
    Returns a list of (path, from_split, to_split, class) for every file
    whose current split differs from its target split.
    """
    moves = []
    for cls, t in target.items():
        current_val = set(val_files.get(cls, []))
        current_train = set(train_files.get(cls, []))

        to_val = t["val"] - current_val        # currently train, target val
        to_train = t["train"] - current_train   # currently val, target train

        for p in sorted(to_val):
            moves.append((p, "train", "val", cls))
        for p in sorted(to_train):
            moves.append((p, "val", "train", cls))

    return moves


def apply_moves(moves: list, dataset_root: str) -> list:
    """Physically move files, returns list of (old_path, new_path, split_from, split_to, cls)."""
    applied = []
    for path, from_split, to_split, cls in moves:
        dest_dir = os.path.join(dataset_root, to_split, cls)
        Path(dest_dir).mkdir(parents=True, exist_ok=True)
        dest = unique_dest(dest_dir, os.path.basename(path))
        shutil.move(path, dest)
        applied.append((path, dest, from_split, to_split, cls))
        log.info(f"  Moved: {path} -> {dest}")
    return applied


def write_report_csv(moves: list, dataset_root: str, dry_run: bool) -> str:
    out_path = os.path.join(dataset_root, "rebalance_moves_report.csv")
    with open(out_path, "w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        if dry_run:
            writer.writerow(["class", "from_split", "to_split", "path"])
            for path, from_split, to_split, cls in moves:
                writer.writerow([cls, from_split, to_split, path])
        else:
            writer.writerow(["class", "from_split", "to_split", "old_path", "new_path"])
            for old_path, new_path, from_split, to_split, cls in moves:
                writer.writerow([cls, from_split, to_split, old_path, new_path])
    return out_path


# ============================================================
# TRAIN-ONLY FLOOR AUGMENTATION
# (same method as merge_and_balance_dataset.py's write_train_balanced —
# copied rather than imported to keep this script standalone)
# ============================================================
def augment_image(img, rng: random.Random):
    """
    Apply a random combination of flip / small rotation / multi-scale zoom /
    hue-saturation jitter / brightness-contrast jitter. Never returns the
    image unmodified — always at least one transform is applied, so
    augmented outputs are not exact duplicates. Rotation and scale are
    pivoted on the image center, so a zoomed patch still keeps the
    annotated point centered.
    """
    out = img.copy()

    if rng.random() < 0.5:
        out = cv2.flip(out, 1)

    if rng.random() < 0.5:
        out = cv2.flip(out, 0)

    angle = rng.uniform(-15, 15)
    scale = rng.uniform(0.8, 1.2)
    h, w = out.shape[:2]
    M = cv2.getRotationMatrix2D((w / 2, h / 2), angle, scale)
    out = cv2.warpAffine(out, M, (w, h), borderMode=cv2.BORDER_REFLECT101)

    hsv = cv2.cvtColor(out, cv2.COLOR_BGR2HSV).astype(np.int16)
    hsv[..., 0] = (hsv[..., 0] + rng.uniform(-10, 10)) % 180
    hsv[..., 1] = np.clip(hsv[..., 1] * rng.uniform(0.7, 1.3), 0, 255)
    out = cv2.cvtColor(hsv.astype(np.uint8), cv2.COLOR_HSV2BGR)

    alpha = rng.uniform(0.85, 1.15)  # contrast
    beta = rng.uniform(-15, 15)      # brightness
    out = cv2.convertScaleAbs(out, alpha=alpha, beta=beta)

    return out


def plan_floor_augmentation(target: dict, floor: int) -> dict:
    """
    Returns {class: {"current": n, "needed": n}} for every class whose
    target train count falls below floor. Source images for augmentation
    are drawn from that class's target train set (real images only —
    never from val, and never from other augmented copies).
    """
    plan = {}
    for cls, t in target.items():
        current = len(t["train"])
        if current == 0:
            continue  # nothing to augment from
        if current < floor:
            plan[cls] = {"current": current, "needed": floor - current,
                         "sources": sorted(t["train"])}
    return plan


def apply_floor_augmentation(plan: dict, dataset_root: str, seed: int) -> list:
    """
    Writes augmented copies into train/<class>/ for every class in plan.
    Returns a list of (class, source_path, dest_path).
    """
    rng = random.Random(seed)
    written = []

    for cls, info in plan.items():
        dest_dir = os.path.join(dataset_root, "train", cls)
        Path(dest_dir).mkdir(parents=True, exist_ok=True)
        sources = info["sources"]

        for i in range(info["needed"]):
            src = rng.choice(sources)
            img = cv2.imread(src)
            if img is None:
                log.warning(f"  Could not read {src} — skipped an augmented copy for {cls}")
                continue
            aug = augment_image(img, rng)
            base = os.path.splitext(os.path.basename(src))[0]
            dest = unique_dest(dest_dir, f"{base}_aug{i + 1}.jpg")
            cv2.imwrite(dest, aug, [cv2.IMWRITE_JPEG_QUALITY, 95])
            written.append((cls, src, dest))
            log.info(f"  Augmented: {cls}: {src} -> {dest}")

    return written


def write_augment_report_csv(written: list, dataset_root: str) -> str:
    out_path = os.path.join(dataset_root, "rebalance_augment_report.csv")
    with open(out_path, "w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(["class", "source_path", "augmented_path"])
        for cls, src, dest in written:
            writer.writerow([cls, src, dest])
    return out_path


# ============================================================
# CORE
# ============================================================
def rebalance(dataset_root: str, val_frac: float, min_val_count: int, seed: int,
              dry_run: bool, floor: int = None) -> dict:
    train_files = list_class_images(dataset_root, "train")
    val_files = list_class_images(dataset_root, "val")

    if not train_files and not val_files:
        raise ValueError(f"No train/ or val/ class folders found under {dataset_root}")

    log.info("Current counts:")
    for cls in sorted(set(train_files) | set(val_files)):
        n_train = len(train_files.get(cls, []))
        n_val = len(val_files.get(cls, []))
        n = n_train + n_val
        ratio = n_val / n if n else 0
        log.info(f"  {cls}: train={n_train}, val={n_val}, val_frac={ratio:.2f}")

    target = compute_target_split(train_files, val_files, val_frac, min_val_count, seed)
    moves = plan_moves(train_files, val_files, target)

    stats = {
        "classes": len(target),
        "moves_planned": len(moves),
        "to_val": sum(1 for m in moves if m[2] == "val"),
        "to_train": sum(1 for m in moves if m[1] == "val"),
        "dry_run": dry_run,
        "report_path": None,
        "floor": floor,
        "augment_report_path": None,
        "augmented_classes": 0,
        "augmented_written": 0,
    }

    floor_plan = plan_floor_augmentation(target, floor) if floor else {}

    if dry_run:
        for path, from_split, to_split, cls in moves:
            log.info(f"  [DRY RUN] would move {cls}: {from_split} -> {to_split}: {path}")
        stats["report_path"] = write_report_csv(moves, dataset_root, dry_run=True)
        log.info(f"DRY RUN — {len(moves)} file(s) would move. "
                 f"Report: {stats['report_path']}")

        if floor_plan:
            for cls, info in floor_plan.items():
                log.info(f"  [DRY RUN] would augment {cls}: {info['current']} -> "
                         f"{info['current'] + info['needed']} (floor={floor})")
            stats["augmented_classes"] = len(floor_plan)
            stats["augmented_written"] = sum(v["needed"] for v in floor_plan.values())
        return stats

    applied = apply_moves(moves, dataset_root)
    stats["report_path"] = write_report_csv(applied, dataset_root, dry_run=False)
    log.info(f"Done. {len(applied)} file(s) moved. Report: {stats['report_path']}")

    if floor_plan:
        augmented = apply_floor_augmentation(floor_plan, dataset_root, seed)
        stats["augment_report_path"] = write_augment_report_csv(augmented, dataset_root)
        stats["augmented_classes"] = len(floor_plan)
        stats["augmented_written"] = len(augmented)
        log.info(f"Augmented {len(augmented)} file(s) across {len(floor_plan)} class(es) "
                 f"to reach floor={floor}. Report: {stats['augment_report_path']}")

    return stats


# ============================================================
# GUI
# ============================================================
def get_args_via_gui():
    result = {}
    cfg = _load_gui_config()

    root = tk.Tk()
    root.title("Rebalance Train/Val Split")
    root.resizable(False, False)
    pad = {"padx": 10, "pady": 5}

    ttk.Label(root, text="Rebalance Train/Val Split",
              font=("Helvetica", 13, "bold")).grid(
        row=0, column=0, columnspan=3, pady=(14, 2), padx=14)
    ttk.Label(root,
              text="Moves files between train/ and val/ to restore the target split ratio",
              foreground="grey").grid(row=1, column=0, columnspan=3, pady=(0, 4))
    ttk.Separator(root, orient="horizontal").grid(
        row=2, column=0, columnspan=3, sticky="ew", padx=10, pady=4)

    ttk.Label(root, text="Dataset root folder:").grid(row=3, column=0, sticky="e", **pad)
    root_var = tk.StringVar(value=cfg.get("dataset_root", ""))
    ttk.Entry(root, textvariable=root_var, width=55).grid(row=3, column=1, **pad)

    def browse_root():
        p = filedialog.askdirectory(title="Select dataset root (contains train/ and val/)")
        if p:
            root_var.set(p)

    ttk.Button(root, text="Browse...", command=browse_root).grid(row=3, column=2, **pad)

    ttk.Separator(root, orient="horizontal").grid(
        row=4, column=0, columnspan=3, sticky="ew", padx=10, pady=6)

    ttk.Label(root, text="Target val fraction:").grid(row=5, column=0, sticky="e", **pad)
    val_frac_var = tk.StringVar(value=str(cfg.get("val_frac", 0.15)))
    ttk.Entry(root, textvariable=val_frac_var, width=15).grid(row=5, column=1, sticky="w", **pad)

    ttk.Label(root, text="Min val count:").grid(row=6, column=0, sticky="e", **pad)
    min_val_var = tk.StringVar(value=str(cfg.get("min_val_count", 5)))
    ttk.Entry(root, textvariable=min_val_var, width=15).grid(row=6, column=1, sticky="w", **pad)

    ttk.Label(root, text="Random seed:").grid(row=7, column=0, sticky="e", **pad)
    seed_var = tk.StringVar(value=str(cfg.get("seed", 42)))
    ttk.Entry(root, textvariable=seed_var, width=15).grid(row=7, column=1, sticky="w", **pad)

    ttk.Label(root, text="Floor (min train images/class, blank=disabled):").grid(
        row=8, column=0, sticky="e", **pad)
    floor_var = tk.StringVar(value=str(cfg.get("floor", "")))
    ttk.Entry(root, textvariable=floor_var, width=15).grid(row=8, column=1, sticky="w", **pad)

    ttk.Separator(root, orient="horizontal").grid(
        row=9, column=0, columnspan=3, sticky="ew", padx=10, pady=6)

    dry_run_var = tk.BooleanVar(value=cfg.get("dry_run", True))
    ttk.Checkbutton(
        root,
        text="Dry run  (write the report only — don't move or augment any files)",
        variable=dry_run_var,
    ).grid(row=10, column=0, columnspan=3, pady=(0, 4))

    ttk.Separator(root, orient="horizontal").grid(
        row=11, column=0, columnspan=3, sticky="ew", padx=10, pady=6)

    btn_frame = ttk.Frame(root)
    btn_frame.grid(row=12, column=0, columnspan=3, pady=(0, 14))

    def on_run():
        dataset_root = root_var.get().strip()
        if not dataset_root or not Path(dataset_root).is_dir():
            messagebox.showerror("Missing input", "Please select a valid dataset root folder.")
            return

        try:
            val_frac = float(val_frac_var.get())
            if not (0 < val_frac < 1):
                raise ValueError
        except ValueError:
            messagebox.showerror("Invalid value", "Val fraction must be a number between 0 and 1.")
            return

        try:
            min_val_count = int(min_val_var.get())
        except ValueError:
            messagebox.showerror("Invalid value", "Min val count must be a whole number.")
            return

        try:
            seed = int(seed_var.get())
        except ValueError:
            messagebox.showerror("Invalid value", "Random seed must be a whole number.")
            return

        floor_raw = floor_var.get().strip()
        floor = None
        if floor_raw:
            try:
                floor = int(floor_raw)
                if floor < 1:
                    raise ValueError
            except ValueError:
                messagebox.showerror("Invalid value", "Floor must be a positive whole number, or blank to disable.")
                return

        result["dataset_root"] = dataset_root
        result["val_frac"] = val_frac
        result["min_val_count"] = min_val_count
        result["seed"] = seed
        result["floor"] = floor
        result["dry_run"] = dry_run_var.get()
        result["submitted"] = True

        _save_gui_config({
            "dataset_root": dataset_root,
            "val_frac": val_frac,
            "min_val_count": min_val_count,
            "seed": seed,
            "floor": floor_raw,
            "dry_run": dry_run_var.get(),
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

    log.info("Rebalance Train/Val Split")
    log.info(f"  dataset root  : {args['dataset_root']}")
    log.info(f"  val fraction  : {args['val_frac']}")
    log.info(f"  min val count : {args['min_val_count']}")
    log.info(f"  seed          : {args['seed']}")
    log.info(f"  floor         : {args['floor'] if args['floor'] else '(disabled)'}")
    if args["dry_run"]:
        log.info("  mode          : DRY RUN")

    try:
        stats = rebalance(
            dataset_root=args["dataset_root"],
            val_frac=args["val_frac"],
            min_val_count=args["min_val_count"],
            seed=args["seed"],
            dry_run=args["dry_run"],
            floor=args["floor"],
        )
    except ValueError as e:
        log.error(str(e))
        messagebox.showerror("Error", str(e))
        sys.exit(1)

    if stats["moves_planned"] == 0 and stats["augmented_written"] == 0:
        messagebox.showinfo("Already balanced",
                             "Every class is already at the target val fraction "
                             "(and at/above the floor, if set) — nothing to do.")
        return

    if stats["dry_run"]:
        lines = [
            "DRY RUN — no files moved or augmented.",
            "",
            f"Classes checked: {stats['classes']:,}",
            f"Files that would move train -> val: {stats['to_val']:,}",
            f"Files that would move val -> train: {stats['to_train']:,}",
            "",
            f"Report written to:\n{stats['report_path']}",
        ]
        if stats["floor"]:
            lines.append("")
            lines.append(f"Classes below floor={stats['floor']}: {stats['augmented_classes']:,}")
            lines.append(f"Augmented copies that would be added: {stats['augmented_written']:,}")
        messagebox.showinfo("Dry run complete", "\n".join(lines))
    else:
        lines = [
            f"Classes rebalanced: {stats['classes']:,}",
            f"Moved train -> val: {stats['to_val']:,}",
            f"Moved val -> train: {stats['to_train']:,}",
            "",
            f"Report written to:\n{stats['report_path']}",
        ]
        if stats["floor"]:
            lines.append("")
            lines.append(f"Classes topped up to floor={stats['floor']}: {stats['augmented_classes']:,}")
            lines.append(f"Augmented copies added: {stats['augmented_written']:,}")
            if stats["augment_report_path"]:
                lines.append(f"Augment report:\n{stats['augment_report_path']}")
        messagebox.showinfo("Rebalance complete", "\n".join(lines))


if __name__ == "__main__":
    main()
