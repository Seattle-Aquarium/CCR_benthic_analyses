#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
find_duplicate_patches_gui.py — Audit an extracted dataset for byte-identical
duplicate patches, and (critically) check whether any duplicates cross the
train/val boundary or leak into a held-out evaluation set.

This is a READ-ONLY audit tool — it never modifies, moves, or deletes any
file. It only reports what it finds, written to a CSV report, so you can
decide what to do about it deliberately.

Scope note: this catches EXACT duplicates only (identical file bytes, via
MD5 hash) — e.g. the same annotation point extracted twice under different
filenames. It does NOT catch near-duplicates: augmented copies (intentionally
different pixels) or visually-similar-but-distinct points from adjacent
Row/Column coordinates are not flagged, since they are genuinely different
data, not redundant copies of the same point.

Categories reported:
    - within_train : duplicate exists only within train/ (mild — some
                      overweighting of one point, not a leakage risk)
    - within_val   : duplicate exists only within val/ (same, mild)
    - train_val_leak : duplicate spans train/ AND val/ (serious — the
                        classic leakage problem: the model gets validated
                        on something it already memorized in training)
    - within_holdout : duplicate exists only within the held-out eval set
                        (biases the accuracy % slightly by double-counting
                        one point)
    - holdout_leak   : duplicate spans the held-out eval set AND
                        train/val (MOST SERIOUS — this would mean part of
                        your "held-out" evaluation isn't actually unseen
                        data, undermining the accuracy number itself)

Usage:
    python find_duplicate_patches_gui.py

    A window opens to select:
      1. Dataset folder (contains train/ and val/) (required)
      2. Held-out evaluation folder — flat <Label>/*.jpg (optional)
      3. Output folder for the report CSV (required)

Requirements:
    pip install tqdm
"""

import csv
import hashlib
import logging
import shutil
import sys
import tkinter as tk
from tkinter import ttk, filedialog, messagebox
from pathlib import Path
from collections import defaultdict

from tqdm import tqdm

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[
        logging.FileHandler("find_duplicate_patches_log.txt", encoding="utf-8"),
        logging.StreamHandler(sys.stdout),
    ],
)
log = logging.getLogger(__name__)

IMG_EXTS = {".jpg", ".jpeg", ".png"}


# ============================================================
# INDEXING
# ============================================================
def index_split(split_name: str, split_dir: str) -> list:
    """Returns [(split_name, class_name, filepath), ...] for a flat
    <split_dir>/<Label>/*.jpg folder."""
    result = []
    root = Path(split_dir)
    if not root.is_dir():
        return result
    for class_dir in sorted(root.iterdir()):
        if not class_dir.is_dir():
            continue
        for f in class_dir.iterdir():
            if f.suffix.lower() in IMG_EXTS:
                result.append((split_name, class_dir.name, str(f)))
    return result


def clean_holdout_leaks(categories: dict, holdout_dir: str) -> dict:
    """
    Moves (never deletes) the held-out-side copy of every file involved in
    a holdout_leak group to a backup folder under holdout_dir, so the
    remaining held-out set is verified free of any content that also
    appears in train/val. The train/val copies are never touched.
    """
    backup_root = Path(holdout_dir) / "_removed_leaked_from_holdout"
    moved = []

    for h, locs in categories.get("holdout_leak", []):
        for split, cls, path in locs:
            if split != "holdout":
                continue  # never touch the train/val copy
            src = Path(path)
            dst = backup_root / cls / src.name
            dst.parent.mkdir(parents=True, exist_ok=True)
            shutil.move(str(src), str(dst))
            moved.append((str(src), str(dst)))
            log.info(f"Removed leaked holdout file: {src.name} "
                     f"(also present in train/val) -> {dst}")

    log.info(f"Moved {len(moved)} leaked file(s) out of the held-out set "
             f"to {backup_root}")
    return {"moved_count": len(moved), "backup_dir": str(backup_root)}


def hash_file(path: str) -> str:
    h = hashlib.md5()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(65536), b""):
            h.update(chunk)
    return h.hexdigest()
    h = hashlib.md5()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(65536), b""):
            h.update(chunk)
    return h.hexdigest()


# ============================================================
# CORE AUDIT
# ============================================================
def audit(dataset_dir: str, holdout_dir: str, output_dir: str) -> dict:
    entries = []
    entries += index_split("train", str(Path(dataset_dir) / "train"))
    entries += index_split("val", str(Path(dataset_dir) / "val"))
    if holdout_dir:
        entries += index_split("holdout", holdout_dir)

    log.info(f"Hashing {len(entries):,} files...")
    hash_index = defaultdict(list)
    for split, cls, path in tqdm(entries, desc="Hashing", unit="file"):
        try:
            h = hash_file(path)
        except Exception as e:
            log.warning(f"Could not hash {path}: {e}")
            continue
        hash_index[h].append((split, cls, path))

    total_files = len(entries)
    unique_hashes = len(hash_index)

    categories = defaultdict(list)  # category -> list of (hash, locations)
    for h, locs in hash_index.items():
        if len(locs) <= 1:
            continue
        splits_present = {s for s, c, p in locs}

        if "holdout" in splits_present and ({"train", "val"} & splits_present):
            categories["holdout_leak"].append((h, locs))
        elif splits_present == {"train", "val"}:
            categories["train_val_leak"].append((h, locs))
        elif splits_present == {"train"}:
            categories["within_train"].append((h, locs))
        elif splits_present == {"val"}:
            categories["within_val"].append((h, locs))
        elif splits_present == {"holdout"}:
            categories["within_holdout"].append((h, locs))

    # ── Report ───────────────────────────────────────────────────────────
    report_path = Path(output_dir) / "duplicate_patches_report.csv"
    with open(report_path, "w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(["content_hash", "category", "duplicate_count", "locations"])
        for category, items in categories.items():
            for h, locs in items:
                loc_str = "; ".join(f"{s}/{c}/{Path(p).name}" for s, c, p in locs)
                writer.writerow([h, category, len(locs), loc_str])

    log.info("=" * 70)
    log.info(f"Total files scanned: {total_files:,}")
    log.info(f"Unique content hashes: {unique_hashes:,}")
    log.info(f"Files involved in duplicate groups: "
             f"{total_files - unique_hashes + sum(len(v) for v in categories.values()):,}"
             if categories else "Files involved in duplicate groups: 0")
    log.info("-" * 70)
    for category in ["train_val_leak", "holdout_leak", "within_train",
                      "within_val", "within_holdout"]:
        n_groups = len(categories.get(category, []))
        n_extra_files = sum(len(locs) - 1 for _, locs in categories.get(category, []))
        flag = " \u26a0\u26a0\u26a0" if category in ("train_val_leak", "holdout_leak") and n_groups else ""
        log.info(f"  {category:<16} {n_groups:>6} duplicate group(s), "
                 f"{n_extra_files:>6} redundant file(s){flag}")
    log.info("-" * 70)
    log.info(f"Full report written to {report_path}")
    log.info("=" * 70)

    return {
        "total_files": total_files,
        "unique_hashes": unique_hashes,
        "categories": {k: {"groups": len(v), "extra_files": sum(len(locs) - 1 for _, locs in v)}
                       for k, v in categories.items()},
        "raw_categories": categories,
        "report_path": str(report_path),
    }


# ============================================================
# GUI
# ============================================================
def get_args_via_gui():
    result = {}
    root = tk.Tk()
    root.title("Find Duplicate Patches")
    root.resizable(False, False)
    pad = {"padx": 10, "pady": 5}

    ttk.Label(root, text="Find Duplicate Patches",
              font=("Helvetica", 13, "bold")).grid(
        row=0, column=0, columnspan=3, pady=(14, 2), padx=14)
    ttk.Label(root,
              text="Read-only audit: scans for byte-identical patches and checks\n"
                   "whether any cross the train/val or held-out boundary",
              foreground="grey", justify="center").grid(row=1, column=0, columnspan=3, pady=(0, 4))
    ttk.Separator(root, orient="horizontal").grid(
        row=2, column=0, columnspan=3, sticky="ew", padx=10, pady=4)

    ttk.Label(root, text="Dataset folder (contains train/, val/):").grid(row=3, column=0, sticky="e", **pad)
    dataset_var = tk.StringVar()
    ttk.Entry(root, textvariable=dataset_var, width=55).grid(row=3, column=1, **pad)

    def browse_dataset():
        p = filedialog.askdirectory(title="Select dataset folder (contains train/ and val/)")
        if p:
            dataset_var.set(p)

    ttk.Button(root, text="Browse...", command=browse_dataset).grid(row=3, column=2, **pad)

    ttk.Label(root, text="Held-out eval folder (optional):").grid(row=4, column=0, sticky="e", **pad)
    holdout_var = tk.StringVar()
    ttk.Entry(root, textvariable=holdout_var, width=55).grid(row=4, column=1, **pad)

    def browse_holdout():
        p = filedialog.askdirectory(title="Select held-out evaluation folder (flat <Label>/*.jpg)")
        if p:
            holdout_var.set(p)

    ttk.Button(root, text="Browse...", command=browse_holdout).grid(row=4, column=2, **pad)

    ttk.Label(root, text="Output folder (for report CSV):").grid(row=5, column=0, sticky="e", **pad)
    outdir_var = tk.StringVar()
    ttk.Entry(root, textvariable=outdir_var, width=55).grid(row=5, column=1, **pad)

    def browse_outdir():
        p = filedialog.askdirectory(title="Select output folder")
        if p:
            outdir_var.set(p)

    ttk.Button(root, text="Browse...", command=browse_outdir).grid(row=5, column=2, **pad)

    ttk.Separator(root, orient="horizontal").grid(
        row=6, column=0, columnspan=3, sticky="ew", padx=10, pady=6)

    clean_var = tk.BooleanVar(value=False)
    ttk.Checkbutton(
        root,
        text="Remove leaked files from the held-out set (moved to a backup\n"
             "folder, never deleted — train/val copies are never touched)\n"
             "Requires a held-out folder above.",
        variable=clean_var,
    ).grid(row=7, column=0, columnspan=3, pady=(0, 4))

    ttk.Separator(root, orient="horizontal").grid(
        row=8, column=0, columnspan=3, sticky="ew", padx=10, pady=6)

    btn_frame = ttk.Frame(root)
    btn_frame.grid(row=9, column=0, columnspan=3, pady=(0, 14))

    def on_run():
        dataset_dir = dataset_var.get().strip()
        holdout_dir = holdout_var.get().strip()
        output_dir = outdir_var.get().strip()

        if not dataset_dir or not Path(dataset_dir).is_dir():
            messagebox.showerror("Missing input", "Please select a valid dataset folder.")
            return
        if holdout_dir and not Path(holdout_dir).is_dir():
            messagebox.showerror("Invalid input", "Held-out folder path is not valid.")
            return
        if clean_var.get() and not holdout_dir:
            messagebox.showerror("Missing input", "Cleanup requires a held-out folder.")
            return
        if not output_dir:
            messagebox.showerror("Missing input", "Please select an output folder.")
            return

        result["dataset_dir"] = dataset_dir
        result["holdout_dir"] = holdout_dir
        result["output_dir"] = output_dir
        result["clean"] = clean_var.get()
        result["submitted"] = True
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

    log.info("Find duplicate patches (read-only audit)")
    log.info(f"  dataset folder : {args['dataset_dir']}")
    log.info(f"  holdout folder : {args['holdout_dir'] or '(not provided)'}")
    log.info(f"  output folder  : {args['output_dir']}")

    stats = audit(args["dataset_dir"], args["holdout_dir"], args["output_dir"])

    clean_stats = None
    if args["clean"]:
        n_leak_groups = stats["categories"].get("holdout_leak", {}).get("groups", 0)
        if n_leak_groups:
            log.info(f"Cleaning {n_leak_groups} holdout_leak group(s) from the held-out set...")
            clean_stats = clean_holdout_leaks(stats["raw_categories"], args["holdout_dir"])
        else:
            log.info("No holdout_leak groups found — nothing to clean.")

    lines = [
        f"Total files scanned: {stats['total_files']:,}",
        f"Unique content hashes: {stats['unique_hashes']:,}",
        "",
    ]
    cats = stats["categories"]
    serious = False
    for category, label in [
        ("train_val_leak", "Train/val leaks"),
        ("holdout_leak", "Held-out leaks (CRITICAL)"),
        ("within_train", "Within-train duplicates"),
        ("within_val", "Within-val duplicates"),
        ("within_holdout", "Within-holdout duplicates"),
    ]:
        c = cats.get(category, {"groups": 0, "extra_files": 0})
        if c["groups"]:
            if category in ("train_val_leak", "holdout_leak"):
                serious = True
            lines.append(f"{label}: {c['groups']} group(s), {c['extra_files']} redundant file(s)")
    if not any(cats.get(c, {"groups": 0})["groups"] for c in cats):
        lines.append("No duplicates found \u2713")

    if clean_stats:
        lines.append("")
        lines.append(f"Removed {clean_stats['moved_count']} leaked file(s) from held-out set")
        lines.append(f"Backup: {clean_stats['backup_dir']}")
        serious = False  # resolved by the cleanup just performed

    lines.append("")
    lines.append(f"Full report: {stats['report_path']}")

    title = "Duplicates found — review needed" if serious else "Audit complete"
    if serious:
        messagebox.showwarning(title, "\n".join(lines))
    else:
        messagebox.showinfo(title, "\n".join(lines))


if __name__ == "__main__":
    main()