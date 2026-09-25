#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
merge_and_balance_dataset.py — Merge + balance YOLO classification datasets
Version: 2.2 (GUI)

Combines two ImageFolder-style datasets (the kind produced by
extract_training_patches.py):

    existing_dataset/{train,val}/<Label>/*.jpg
    new_dataset/{train,val}/<Label>/*.jpg

into one balanced output dataset:

    output_dir/{train,val}/<Label>/*.jpg

The new dataset folder is optional — leave it blank to just balance a
single existing dataset (cap/floor applied to train, val copied as-is)
with no merge involved. Duplicate-filename scanning still runs in this
mode, since a single dataset can still have the same patch filename in
both train/ and val/ (a leak) even with nothing to merge in.

Before merging (always, even on an inventory-only run), this script:
    - Compares the class (Label) folders present in each dataset and
      reports any that only exist in one of the two (e.g. a class the
      new dataset hasn't annotated yet, or one that's been dropped).
    - Scans both datasets for duplicate filenames — the same source patch
      showing up in more than one place — and keeps exactly ONE copy of
      each. When the two occurrences disagree on class label (a labeling
      conflict), the NEW dataset's label wins, on the assumption that
      "new" represents a more recent/verified re-classification (e.g.
      Zooniverse-verified) than whatever is in "existing". When they agree
      on label but differ only in split (a train/val leak), "existing"'s
      split placement is kept, then train over val as a tiebreak.
      A full list of every leak and conflict — not just the first 25 — is
      always written to duplicate_report.csv next to the log file, so you
      can audit exactly which label was kept for every conflict.

Balancing rules (train split ONLY — val is merged untouched):
    - Classes with MORE than <cap> train images: randomly undersampled
      down to cap (deterministic, seeded).
    - Classes with FEWER than <floor> train images: augmented copies
      (horizontal + vertical flip, small rotation, multi-scale zoom,
      hue/saturation jitter, brightness/contrast jitter — never exact
      duplicates) are added until the class reaches floor.
    - Classes in between: copied as-is, untouched.

val/ is never capped, never augmented — it's merged as-is so accuracy
metrics stay comparable to your existing accuracy-report baseline.

Usage:
    python merge_and_balance_dataset.py

    A window opens to select:
      1. Dataset folder (required) and new dataset folder (optional —
         leave blank to just balance the one dataset)
      2. Output dataset folder (required)
      3. Cap / floor / seed (required unless Inventory only)
      4. Inventory only checkbox — report combined counts, class-folder
         mismatches, and duplicate filenames without writing any files

Requirements:
    pip install opencv-python-headless numpy tqdm
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
from tqdm import tqdm

# ── Logging ──────────────────────────────────────────────────────────────
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[
        logging.FileHandler("merge_and_balance_log.txt", encoding="utf-8"),
        logging.StreamHandler(sys.stdout),
    ],
)
log = logging.getLogger(__name__)

IMG_EXTS = {".jpg", ".jpeg", ".png"}

# Path for persisting GUI state between runs
_CONFIG_PATH = Path(__file__).parent / ".merge_and_balance_dataset_config.json"
_DUP_REPORT_PATH = Path(__file__).parent / "duplicate_report.csv"


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


def list_class_dirs(dataset_root: str, split: str) -> set:
    """
    Return the set of class folder names under a split, regardless of
    whether they currently contain any images — used for the class-folder
    match check so an emptied-out class still shows up as a mismatch.
    """
    split_dir = Path(dataset_root) / split
    if not split_dir.is_dir():
        return set()
    return {d.name for d in split_dir.iterdir() if d.is_dir()}


def merge_file_lists(*dicts) -> dict:
    """Merge several {class: [files]} dicts into one, concatenating lists."""
    merged = {}
    for d in dicts:
        for cls, files in d.items():
            merged.setdefault(cls, []).extend(files)
    return merged


def print_inventory(train_counts: dict, val_counts: dict) -> None:
    all_classes = sorted(set(train_counts) | set(val_counts))
    log.info("=" * 70)
    log.info(f"{'Class':<15} {'train':>8} {'val':>8}")
    log.info("-" * 70)
    for cls in all_classes:
        log.info(f"{cls:<15} {len(train_counts.get(cls, [])):>8} "
                  f"{len(val_counts.get(cls, [])):>8}")
    log.info("-" * 70)
    total_train = sum(len(v) for v in train_counts.values())
    total_val = sum(len(v) for v in val_counts.values())
    log.info(f"{'TOTAL':<15} {total_train:>8} {total_val:>8}")
    log.info("=" * 70)

    counts_only = sorted(len(v) for v in train_counts.values())
    if counts_only:
        log.info(f"train class counts — min={counts_only[0]}, "
                  f"median={counts_only[len(counts_only)//2]}, "
                  f"max={counts_only[-1]}")


# ============================================================
# CLASS-FOLDER MATCH CHECK
# ============================================================
def check_class_folders(existing_dataset: str, new_dataset: str) -> dict:
    """
    Compare the class (Label) folders present in each dataset, per split,
    and log any that only appear in one of the two. Purely informational —
    a mismatch isn't necessarily an error (e.g. a newly-annotated class),
    but it's easy to miss otherwise.

    If new_dataset is not given (single-dataset balance-only mode), there's
    nothing to compare — returns an empty (no-mismatch) report.
    """
    if not new_dataset:
        log.info("Single dataset mode — skipping class-folder comparison "
                  "(no new dataset provided).")
        return {"train": {"only_existing": [], "only_new": []},
                "val": {"only_existing": [], "only_new": []}}

    report = {}
    log.info("-" * 70)
    log.info("Class-folder match check (existing dataset vs. new dataset):")
    any_mismatch = False
    for split in ("train", "val"):
        existing_classes = list_class_dirs(existing_dataset, split)
        new_classes = list_class_dirs(new_dataset, split)
        only_existing = sorted(existing_classes - new_classes)
        only_new = sorted(new_classes - existing_classes)
        report[split] = {"only_existing": only_existing, "only_new": only_new}

        if only_existing:
            any_mismatch = True
            log.warning(f"  [{split}] In existing dataset only (missing from new): {only_existing}")
        if only_new:
            any_mismatch = True
            log.warning(f"  [{split}] In new dataset only (missing from existing): {only_new}")
        if not only_existing and not only_new:
            log.info(f"  [{split}] Class folders match ✓")
    if not any_mismatch:
        log.info("All class folders match between datasets ✓")
    log.info("-" * 70)
    return report


# ============================================================
# DUPLICATE-NAME CHECK
# ============================================================
def build_name_index(train_val_map: dict) -> dict:
    """
    train_val_map: {"existing_train": {cls: [paths]}, "existing_val": {...},
                     "new_train": {...}, "new_val": {...}}

    Returns {basename: [{"source": "existing"/"new", "split": "train"/"val",
                          "class": cls, "path": path}, ...]}
    """
    index = {}
    for key, class_files in train_val_map.items():
        source, split = key.split("_", 1)
        for cls, files in class_files.items():
            for f in files:
                name = os.path.basename(f)
                index.setdefault(name, []).append(
                    {"source": source, "split": split, "class": cls, "path": f}
                )
    return index


def resolve_duplicates(index: dict):
    """
    For every filename that appears more than once across the two datasets,
    keep exactly one copy and mark the rest to be dropped from the merge.

    Resolution priority (first match wins):
      1. Prefer source == "new" over "existing" — the new dataset (e.g. a
         fresh Zooniverse-verified batch) is assumed to reflect a more
         recent/authoritative classification than whatever's already in
         the existing dataset.
      2. Within the same source, prefer split == "train" over "val".

    Returns (drop_paths: set, leaks: list, conflicts: list, dup_count: int,
             kept_map: dict[name -> occurrence kept])
      - leaks: duplicates that span both train AND val (would leak between
        splits if both copies were kept)
      - conflicts: duplicates whose occurrences disagree on class label
    """
    drop_paths = set()
    leaks = []
    conflicts = []
    kept_map = {}
    dup_count = 0

    for name, occurrences in index.items():
        if len(occurrences) <= 1:
            continue
        dup_count += 1

        if len({o["split"] for o in occurrences}) > 1:
            leaks.append((name, occurrences))
        if len({o["class"] for o in occurrences}) > 1:
            conflicts.append((name, occurrences))

        kept, *rest = sorted(
            occurrences,
            key=lambda o: (o["source"] != "new", o["split"] != "train"),
        )
        kept_map[name] = kept
        for o in rest:
            drop_paths.add(o["path"])

    return drop_paths, leaks, conflicts, dup_count, kept_map


def _describe(occurrences) -> str:
    return ", ".join(f"{o['source']}/{o['split']}/{o['class']}" for o in occurrences)


def write_duplicate_report_csv(leaks: list, conflicts: list, kept_map: dict, path: Path) -> None:
    """
    Write EVERY leak and conflict (not just the first 25 shown in the log)
    to a CSV for full review: filename, type (leak/conflict), which
    occurrence was kept, and all occurrences with their source/split/class.
    """
    leak_names = {name for name, _ in leaks}
    conflict_names = {name for name, _ in conflicts}
    all_names = leak_names | conflict_names

    with open(path, "w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(["filename", "type", "kept_source", "kept_split", "kept_class",
                          "all_occurrences"])
        for name, occurrences in leaks + [(n, o) for n, o in conflicts if n not in leak_names]:
            dtype = []
            if name in leak_names:
                dtype.append("leak")
            if name in conflict_names:
                dtype.append("conflict")
            kept = kept_map.get(name)
            writer.writerow([
                name,
                "+".join(dtype),
                kept["source"] if kept else "",
                kept["split"] if kept else "",
                kept["class"] if kept else "",
                _describe(occurrences),
            ])
    log.info(f"Full duplicate report ({len(all_names)} rows) written to {path}")


def report_duplicates(drop_paths: set, leaks: list, conflicts: list, dup_count: int,
                       kept_map: dict) -> None:
    log.info("-" * 70)
    if dup_count == 0:
        log.info("No duplicate filenames found across the merged dataset ✓")
        log.info("-" * 70)
        return

    log.warning(f"Found {dup_count} duplicate filename(s) across the merged dataset "
                f"— keeping one copy of each, dropping {len(drop_paths)} file(s).")

    if leaks:
        log.warning(f"  {len(leaks)} duplicate(s) spanned BOTH train and val "
                    f"(would have leaked between splits) — resolved to a single split:")
        for name, occ in leaks[:25]:
            log.warning(f"    LEAK: {name} — [{_describe(occ)}] -> kept {kept_map[name]['source']}/{kept_map[name]['split']}")
        if len(leaks) > 25:
            log.warning(f"    ... and {len(leaks) - 25} more (see duplicate_report.csv)")

    if conflicts:
        log.warning(f"  {len(conflicts)} duplicate(s) have CONFLICTING class labels "
                    f"between datasets — 'new' dataset's label kept in each case:")
        for name, occ in conflicts[:25]:
            log.warning(f"    CONFLICT: {name} — [{_describe(occ)}] -> kept {kept_map[name]['class']} ({kept_map[name]['source']})")
        if len(conflicts) > 25:
            log.warning(f"    ... and {len(conflicts) - 25} more (see duplicate_report.csv)")

    write_duplicate_report_csv(leaks, conflicts, kept_map, _DUP_REPORT_PATH)
    log.info("-" * 70)


def drop_from_inventory(class_files: dict, drop_paths: set) -> dict:
    if not drop_paths:
        return class_files
    return {cls: [f for f in files if f not in drop_paths]
            for cls, files in class_files.items()}


# ============================================================
# AUGMENTATION (train split only)
# ============================================================
def augment_image(img: np.ndarray, rng: random.Random) -> np.ndarray:
    """
    Apply a random combination of flip / small rotation / multi-scale
    zoom / hue-saturation jitter / brightness-contrast jitter. Never
    returns the image unmodified — always at least one transform is
    applied, so augmented outputs are not exact duplicates.

    Rotation and scale are applied as a single warpAffine pivoted on the
    image center, so a zoomed-in/out patch still keeps whatever's at the
    center in the center — these patches are always extracted centered on
    the annotated point (see extract_training_patches.py), so the subject
    of interest is the center pixel, not the frame edges.
    """
    out = img.copy()

    # Horizontal flip (~50% chance)
    if rng.random() < 0.5:
        out = cv2.flip(out, 1)

    # Vertical flip (~50% chance) — benthic patches have no canonical "up"
    if rng.random() < 0.5:
        out = cv2.flip(out, 0)

    # Small rotation (-15 to +15 degrees) + multi-scale zoom (0.8x-1.2x),
    # both centered on the image midpoint
    angle = rng.uniform(-15, 15)
    scale = rng.uniform(0.8, 1.2)
    h, w = out.shape[:2]
    M = cv2.getRotationMatrix2D((w / 2, h / 2), angle, scale)
    out = cv2.warpAffine(out, M, (w, h), borderMode=cv2.BORDER_REFLECT101)

    # Hue / saturation jitter — underwater imagery has a lot of natural
    # color-cast variation from depth, turbidity, and camera white balance
    hsv = cv2.cvtColor(out, cv2.COLOR_BGR2HSV).astype(np.int16)
    hsv[..., 0] = (hsv[..., 0] + rng.uniform(-10, 10)) % 180
    hsv[..., 1] = np.clip(hsv[..., 1] * rng.uniform(0.7, 1.3), 0, 255)
    out = cv2.cvtColor(hsv.astype(np.uint8), cv2.COLOR_HSV2BGR)

    # Brightness / contrast jitter
    alpha = rng.uniform(0.85, 1.15)  # contrast
    beta = rng.uniform(-15, 15)      # brightness
    out = cv2.convertScaleAbs(out, alpha=alpha, beta=beta)

    return out


# ============================================================
# COPY / BALANCE
# ============================================================
def unique_dest(dest_dir: str, filename: str) -> str:
    base, ext = os.path.splitext(filename)
    candidate = filename
    n = 2
    while os.path.exists(os.path.join(dest_dir, candidate)):
        candidate = f"{base}__{n}{ext}"
        n += 1
    return os.path.join(dest_dir, candidate)


def write_val(class_files: dict, output_dir: str) -> int:
    """Copy val files as-is — no cap, no augmentation. Returns count written."""
    written = 0
    for cls, files in tqdm(class_files.items(), desc="val (unmodified)"):
        dest_class_dir = os.path.join(output_dir, "val", cls)
        Path(dest_class_dir).mkdir(parents=True, exist_ok=True)
        for f in files:
            dest = unique_dest(dest_class_dir, os.path.basename(f))
            shutil.copy2(f, dest)
            written += 1
    return written


def write_train_balanced(class_files: dict, output_dir: str, cap: int,
                          floor: int, seed: int) -> dict:
    rng = random.Random(seed)
    written = 0
    augmented = 0
    capped_classes = []
    floored_classes = []

    for cls, files in tqdm(class_files.items(), desc="train (balancing)"):
        dest_class_dir = os.path.join(output_dir, "train", cls)
        Path(dest_class_dir).mkdir(parents=True, exist_ok=True)

        files = list(files)
        rng.shuffle(files)
        n = len(files)

        if n > cap:
            kept = files[:cap]
            capped_classes.append(cls)
            log.info(f"  {cls}: {n} -> capped to {cap} "
                      f"({n - cap} undersampled out)")
        else:
            kept = files

        for f in kept:
            dest = unique_dest(dest_class_dir, os.path.basename(f))
            shutil.copy2(f, dest)
            written += 1

        current_count = len(kept)
        if current_count < floor:
            needed = floor - current_count
            floored_classes.append(cls)
            log.info(f"  {cls}: {current_count} -> augmenting "
                      f"{needed} more to reach floor={floor}")
            for i in range(needed):
                src = rng.choice(kept) if kept else None
                if src is None:
                    log.warning(f"  {cls}: no source images to augment from — skipped")
                    break
                img = cv2.imread(src)
                if img is None:
                    continue
                aug = augment_image(img, rng)
                base = os.path.splitext(os.path.basename(src))[0]
                dest_name = f"{base}_aug{i+1}.jpg"
                dest = unique_dest(dest_class_dir, dest_name)
                cv2.imwrite(dest, aug, [cv2.IMWRITE_JPEG_QUALITY, 95])
                written += 1
                augmented += 1

    return {
        "written": written,
        "augmented": augmented,
        "capped_classes": capped_classes,
        "floored_classes": floored_classes,
    }


# ============================================================
# CORE MERGE
# ============================================================
def merge_and_balance(existing_dataset: str, new_dataset: str, output_dir: str,
                       cap: int = None, floor: int = None, seed: int = 42,
                       inventory_only: bool = False) -> dict:
    """
    new_dataset may be None/empty for single-dataset "balance only" mode —
    the existing dataset is capped/floored on its own, nothing is merged in.
    """
    existing_train = list_class_images(existing_dataset, "train")
    existing_val = list_class_images(existing_dataset, "val")
    new_train = list_class_images(new_dataset, "train") if new_dataset else {}
    new_val = list_class_images(new_dataset, "val") if new_dataset else {}

    folder_report = check_class_folders(existing_dataset, new_dataset)

    index = build_name_index({
        "existing_train": existing_train,
        "existing_val": existing_val,
        "new_train": new_train,
        "new_val": new_val,
    })
    drop_paths, leaks, conflicts, dup_count, kept_map = resolve_duplicates(index)
    report_duplicates(drop_paths, leaks, conflicts, dup_count, kept_map)

    existing_train = drop_from_inventory(existing_train, drop_paths)
    existing_val = drop_from_inventory(existing_val, drop_paths)
    new_train = drop_from_inventory(new_train, drop_paths)
    new_val = drop_from_inventory(new_val, drop_paths)

    merged_train = merge_file_lists(existing_train, new_train)
    merged_val = merge_file_lists(existing_val, new_val)

    print_inventory(merged_train, merged_val)

    stats = {
        "folder_report": folder_report,
        "dup_count": dup_count,
        "dropped_duplicates": len(drop_paths),
        "leak_count": len(leaks),
        "conflict_count": len(conflicts),
        "inventory_only": inventory_only,
        "total_train": sum(len(v) for v in merged_train.values()),
        "total_val": sum(len(v) for v in merged_val.values()),
    }

    if inventory_only:
        log.info("Inventory-only run — no files written. "
                 "Re-run with cap and floor set to actually merge.")
        return stats

    log.info(f"Writing merged, balanced dataset to {output_dir} "
             f"(cap={cap}, floor={floor})")

    train_stats = write_train_balanced(merged_train, output_dir, cap, floor, seed)
    val_written = write_val(merged_val, output_dir)

    stats.update({
        "train_written": train_stats["written"],
        "train_augmented": train_stats["augmented"],
        "capped_classes": train_stats["capped_classes"],
        "floored_classes": train_stats["floored_classes"],
        "val_written": val_written,
    })

    log.info("Done.")
    return stats


# ============================================================
# GUI
# ============================================================
def get_args_via_gui():
    result = {}
    cfg = _load_gui_config()

    root = tk.Tk()
    root.title("Merge & Balance Datasets")
    root.resizable(False, False)
    pad = {"padx": 10, "pady": 5}

    ttk.Label(root, text="Merge & Balance Datasets",
              font=("Helvetica", 13, "bold")).grid(
        row=0, column=0, columnspan=3, pady=(14, 2), padx=14)
    ttk.Label(root,
              text="Combine two ImageFolder-style datasets into one balanced output dataset\n"
                   "(or leave \"new dataset\" blank to just balance one dataset)",
              foreground="grey", justify="center").grid(row=1, column=0, columnspan=3, pady=(0, 4))
    ttk.Separator(root, orient="horizontal").grid(
        row=2, column=0, columnspan=3, sticky="ew", padx=10, pady=4)

    # ── Existing dataset ────────────────────────────────────────────────
    ttk.Label(root, text="Dataset folder:").grid(row=3, column=0, sticky="e", **pad)
    existing_var = tk.StringVar(value=cfg.get("existing_dataset", ""))
    ttk.Entry(root, textvariable=existing_var, width=55).grid(row=3, column=1, **pad)

    def browse_existing():
        p = filedialog.askdirectory(title="Select dataset folder")
        if p:
            existing_var.set(p)

    ttk.Button(root, text="Browse...", command=browse_existing).grid(row=3, column=2, **pad)

    # ── New dataset ─────────────────────────────────────────────────────
    ttk.Label(root, text="New dataset folder (optional):").grid(row=4, column=0, sticky="e", **pad)
    new_var = tk.StringVar(value=cfg.get("new_dataset", ""))
    ttk.Entry(root, textvariable=new_var, width=55).grid(row=4, column=1, **pad)

    def browse_new():
        p = filedialog.askdirectory(title="Select new dataset folder")
        if p:
            new_var.set(p)

    ttk.Button(root, text="Browse...", command=browse_new).grid(row=4, column=2, **pad)

    # ── Output directory ────────────────────────────────────────────────
    ttk.Label(root, text="Output dataset folder:").grid(row=5, column=0, sticky="e", **pad)
    outdir_var = tk.StringVar(value=cfg.get("output_dir", ""))
    ttk.Entry(root, textvariable=outdir_var, width=55).grid(row=5, column=1, **pad)

    def browse_outdir():
        p = filedialog.askdirectory(title="Select output dataset folder")
        if p:
            outdir_var.set(p)

    ttk.Button(root, text="Browse...", command=browse_outdir).grid(row=5, column=2, **pad)

    ttk.Separator(root, orient="horizontal").grid(
        row=6, column=0, columnspan=3, sticky="ew", padx=10, pady=6)

    # ── Numeric params ──────────────────────────────────────────────────
    cap_label = ttk.Label(root, text="Cap (max train images/class):")
    cap_label.grid(row=7, column=0, sticky="e", **pad)
    cap_var = tk.StringVar(value=str(cfg.get("cap", "")))
    cap_entry = ttk.Entry(root, textvariable=cap_var, width=15)
    cap_entry.grid(row=7, column=1, sticky="w", **pad)

    floor_label = ttk.Label(root, text="Floor (min train images/class):")
    floor_label.grid(row=8, column=0, sticky="e", **pad)
    floor_var = tk.StringVar(value=str(cfg.get("floor", "")))
    floor_entry = ttk.Entry(root, textvariable=floor_var, width=15)
    floor_entry.grid(row=8, column=1, sticky="w", **pad)

    ttk.Label(root, text="Random seed:").grid(row=9, column=0, sticky="e", **pad)
    seed_var = tk.StringVar(value=str(cfg.get("seed", 42)))
    ttk.Entry(root, textvariable=seed_var, width=15).grid(row=9, column=1, sticky="w", **pad)

    ttk.Separator(root, orient="horizontal").grid(
        row=10, column=0, columnspan=3, sticky="ew", padx=10, pady=6)

    # ── Inventory only ──────────────────────────────────────────────────
    inventory_var = tk.BooleanVar(value=cfg.get("inventory_only", True))

    def toggle_cap_floor():
        state = "disabled" if inventory_var.get() else "normal"
        cap_entry.config(state=state)
        floor_entry.config(state=state)
        cap_label.config(foreground="grey" if state == "disabled" else "black")
        floor_label.config(foreground="grey" if state == "disabled" else "black")

    ttk.Checkbutton(
        root,
        text="Inventory only  (report combined counts, class-folder mismatches, and\n"
             "duplicate filenames without writing any files)",
        variable=inventory_var,
        command=toggle_cap_floor,
    ).grid(row=11, column=0, columnspan=3, pady=(0, 4))
    toggle_cap_floor()  # apply initial state on window open

    ttk.Separator(root, orient="horizontal").grid(
        row=12, column=0, columnspan=3, sticky="ew", padx=10, pady=6)

    # ── Run / Cancel buttons ────────────────────────────────────────────
    btn_frame = ttk.Frame(root)
    btn_frame.grid(row=13, column=0, columnspan=3, pady=(0, 14))

    def on_run():
        existing_dataset = existing_var.get().strip()
        new_dataset = new_var.get().strip()
        output_dir = outdir_var.get().strip()

        if not existing_dataset or not Path(existing_dataset).is_dir():
            messagebox.showerror("Missing input", "Please select a valid dataset folder.")
            return
        if new_dataset and not Path(new_dataset).is_dir():
            messagebox.showerror("Missing input", "New dataset folder was entered but doesn't exist:\n"
                                                    f"{new_dataset}\n\nLeave it blank to just balance the "
                                                    "existing dataset.")
            return
        if not output_dir:
            messagebox.showerror("Missing input", "Please select an output dataset folder.")
            return

        inventory_only = inventory_var.get()

        cap = floor = None
        if not inventory_only:
            try:
                cap = int(cap_var.get())
            except ValueError:
                messagebox.showerror("Invalid value", "Cap must be a whole number.")
                return
            try:
                floor = int(floor_var.get())
            except ValueError:
                messagebox.showerror("Invalid value", "Floor must be a whole number.")
                return

        try:
            seed = int(seed_var.get())
        except ValueError:
            messagebox.showerror("Invalid value", "Random seed must be a whole number.")
            return

        result["existing_dataset"] = existing_dataset
        result["new_dataset"] = new_dataset
        result["output_dir"] = output_dir
        result["cap"] = cap
        result["floor"] = floor
        result["seed"] = seed
        result["inventory_only"] = inventory_only
        result["submitted"] = True

        _save_gui_config({
            "existing_dataset": existing_dataset,
            "new_dataset": new_dataset,
            "output_dir": output_dir,
            "cap": cap_var.get(),
            "floor": floor_var.get(),
            "seed": seed,
            "inventory_only": inventory_only,
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

    log.info("Merge + balance YOLO classification datasets")
    log.info(f"  dataset          : {args['existing_dataset']}")
    log.info(f"  new dataset      : {args['new_dataset'] or '(none — balance only)'}")
    log.info(f"  output dir       : {args['output_dir']}")
    if args["inventory_only"]:
        log.info("  mode             : INVENTORY ONLY")
    else:
        log.info(f"  cap / floor      : {args['cap']} / {args['floor']}")
    log.info(f"  seed             : {args['seed']}")

    stats = merge_and_balance(
        existing_dataset=args["existing_dataset"],
        new_dataset=args["new_dataset"],
        output_dir=args["output_dir"],
        cap=args["cap"],
        floor=args["floor"],
        seed=args["seed"],
        inventory_only=args["inventory_only"],
    )

    # ── Summary popup ────────────────────────────────────────────────────
    lines = []
    if stats["inventory_only"]:
        lines.append("INVENTORY ONLY — no files written.")
        lines.append("")
        lines.append(f"Combined: {stats['total_train']:,} train / {stats['total_val']:,} val")
    else:
        lines.append(f"Train written: {stats['train_written']:,} "
                      f"({stats['train_augmented']:,} augmented)")
        lines.append(f"Val written: {stats['val_written']:,}")
        if stats["capped_classes"]:
            lines.append(f"Capped classes: {len(stats['capped_classes'])}")
        if stats["floored_classes"]:
            lines.append(f"Augmented-up classes: {len(stats['floored_classes'])}")
        lines.append("")
        lines.append(f"Saved to:\n{args['output_dir']}")

    lines.append("")
    if stats["dup_count"]:
        lines.append(f"⚠ {stats['dup_count']:,} duplicate filename(s) found "
                      f"({stats['dropped_duplicates']:,} dropped, "
                      f"{stats['leak_count']} train/val leaks, "
                      f"{stats['conflict_count']} label conflicts — full list in "
                      f"duplicate_report.csv).")
    else:
        lines.append("No duplicate filenames found ✓")

    balance_only = not args["new_dataset"]
    mismatch = any(stats["folder_report"][s]["only_existing"] or stats["folder_report"][s]["only_new"]
                   for s in ("train", "val"))
    if balance_only:
        pass  # no second dataset to compare class folders against
    elif mismatch:
        lines.append("⚠ Class folders differ between datasets — see log for details.")
    else:
        lines.append("Class folders match between datasets ✓")

    if stats["inventory_only"]:
        title = "Inventory complete"
    elif balance_only:
        title = "Balance complete"
    else:
        title = "Merge complete"
    if stats["dup_count"] or mismatch:
        messagebox.showwarning(title, "\n".join(lines))
    else:
        messagebox.showinfo(title, "\n".join(lines))


if __name__ == "__main__":
    main()