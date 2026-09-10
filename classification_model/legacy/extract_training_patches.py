#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
extract_training_patches.py — Toolbox annotation CSV(s) -> YOLO classification dataset
Version: 2.1 (CLI, multi-file input)

This sits DOWNSTREAM of zooni_to_toolbox_annot.py in the workflow:

    toolbox_import.csv (one or more)  ->  [this script]  ->  dataset/train/<Label>/*.jpg
                                                                dataset/val/<Label>/*.jpg

You can select one or more Toolbox-format annotation CSVs at once — they're
concatenated before filtering/extraction, so e.g. several transects' worth
of verified exports can be processed together in a single run.

For each row where Verified == True, this script:
    - Loads the source image (via the same Windows-user-path localization
      trick used in toolbox_to_subjects.py, plus optional OLD=>NEW folder
      remaps for paths that have moved/been renamed since export)
    - Crops EXACTLY Patch Size x Patch Size centered on (Row, Column) —
      no overlay, no scaling, no burned-in text (unlike the Zooniverse
      review crops)
    - Saves it to dataset/<split>/<Label>/<source>_r<row>_c<col>.jpg
    - Assigns each row to train or val with a per-class stratified split,
      so rare classes aren't accidentally left out of one split entirely

On every real (non-dry-run) extraction, the filtered/deduplicated
annotation rows are also written out to
dataset/annotations_updated_paths.csv, with the Path column rewritten to
the resolved (localized + remapped) path actually used to load each
image — so a later run against the same dataset doesn't need the same
path remaps supplied again.

This does NOT merge with your existing balanced training set or run
any class-balancing itself — it only extracts and splits what's in
the input CSV(s). Merge/balance as a separate, explicit step so you can
inspect class counts before combining.

Usage:
    python extract_training_patches.py

    A window opens to select:
      1. One or more Toolbox annotation CSVs (required)
      2. Output dataset folder (required)
      3. Val fraction / min val count / JPEG quality / seed (optional, have defaults)
      4. Labels to exclude even if Verified=True (optional, defaults to "Review")
      5. Path remaps for folders that were renamed since the CSV was exported (optional)
      6. No split checkbox — write flat <Label>/ folders instead of train/val
         (for held-out evaluation transects that should never be trained on)
      7. Dry run checkbox — validate + count without writing files

Requirements:
    pip install opencv-python-headless pandas tqdm
"""

import getpass
import json
import logging
import os
import random
import re
import sys
import tkinter as tk
from tkinter import ttk, filedialog, messagebox
from pathlib import Path

import cv2
import pandas as pd
from tqdm import tqdm

# ── Logging ──────────────────────────────────────────────────────────────
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[
        logging.FileHandler("extract_training_patches_log.txt", encoding="utf-8"),
        logging.StreamHandler(sys.stdout),
    ],
)
log = logging.getLogger(__name__)

# Path for persisting GUI state between runs
_CONFIG_PATH = Path(__file__).parent / ".extract_training_patches_config.json"


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
# PATH LOCALIZATION (identical logic to toolbox_to_subjects.py)
# ============================================================
_USER_PATH_RE = re.compile(r"^([A-Za-z]:[\\/]Users[\\/])[^\\/]+([\\/].*)$")


def localize_path(path: str) -> str:
    """
    Rewrite a Windows user-profile path so it points at the current user's
    profile instead of whoever exported the annotation CSV. The shared
    Dropbox tree under Users/<name>/ is identical across users, so only the
    username segment needs to change.
    """
    match = _USER_PATH_RE.match(path)
    if not match:
        return path
    return f"{match.group(1)}{getpass.getuser()}{match.group(2)}"


def remap_path(path: str, remaps: list) -> str:
    """
    Apply an ordered list of (old, new) substring replacements to a path,
    e.g. to account for a folder having been renamed/reorganized since the
    annotation CSV was exported. Applied AFTER localize_path().

    Matching is done with slashes normalized to "/" first, so it doesn't
    matter whether the CSV's path or the remap's old/new values use
    forward or back slashes -- Windows accepts either in the result.
    """
    path = path.replace("\\", "/")
    for old, new in remaps:
        old = old.replace("\\", "/")
        new = new.replace("\\", "/")
        if old in path:
            path = path.replace(old, new)
    return path


# ============================================================
# FILENAME HELPER (identical logic to toolbox_to_subjects.py)
# ============================================================
def unique_filename(output_dir: str, filename: str) -> str:
    """
    Return a filename that does not already exist in output_dir.
    Appends __2, __3, ... if a collision is found.
    """
    base, ext = os.path.splitext(filename)
    candidate = filename
    n = 2
    while os.path.exists(os.path.join(output_dir, candidate)):
        candidate = f"{base}__{n}{ext}"
        n += 1
    return candidate


def sanitize_label(label: str) -> str:
    """Make a label safe to use as a folder name."""
    label = str(label).strip()
    return re.sub(r'[<>:"/\\|?*]', "_", label)


# ============================================================
# TRAIN/VAL SPLIT
# ============================================================
def assign_splits(df: pd.DataFrame, val_frac: float, min_val_count: int,
                   seed: int) -> pd.Series:
    """
    Per-class stratified train/val assignment.

    For classes with fewer than min_val_count total samples, every sample
    goes to train (val_frac is not applied) and a warning is logged, since
    holding out a val sample for a near-empty class is not meaningful.
    """
    rng = random.Random(seed)
    split = pd.Series(index=df.index, dtype=object)

    for label, group in df.groupby("Label"):
        idx = list(group.index)
        rng.shuffle(idx)
        n = len(idx)

        if n < min_val_count:
            split.loc[idx] = "train"
            log.warning(
                f"  Class '{label}': only {n} sample(s) — all assigned to "
                f"train (below min-val-count={min_val_count})"
            )
            continue

        n_val = max(1, round(n * val_frac))
        val_idx = idx[:n_val]
        train_idx = idx[n_val:]
        split.loc[val_idx] = "val"
        split.loc[train_idx] = "train"

    return split


# ============================================================
# LOAD + CONCAT MULTIPLE ANNOTATION CSVs
# ============================================================
def _load_and_concat_csvs(paths: list) -> pd.DataFrame:
    frames = []
    for p in paths:
        log.info(f"Loading annotation CSV from {p}…")
        df = pd.read_csv(p)
        log.info(f"  {len(df):,} rows")
        frames.append(df)
    return pd.concat(frames, ignore_index=True)


# ============================================================
# UPDATED-PATHS CSV
# ============================================================
def write_updated_annotations_csv(df: pd.DataFrame, path_remaps: list, output_dir: str) -> str:
    """
    Write a copy of the (filtered, deduplicated) annotation rows used for
    this extraction, with the Path column rewritten to the resolved path
    (after localize_path + remap_path) that was actually used to load each
    source image — so a later run against the same dataset doesn't need
    the same --path-remaps supplied again.
    """
    out_df = df.drop(columns=["_split"], errors="ignore").copy()
    out_df["Path"] = out_df["Path"].apply(
        lambda p: remap_path(localize_path(str(p)), path_remaps))
    Path(output_dir).mkdir(parents=True, exist_ok=True)
    out_path = os.path.join(output_dir, "annotations_updated_paths.csv")
    out_df.to_csv(out_path, index=False)
    return out_path


# ============================================================
# CORE EXTRACTION
# ============================================================
def extract(annotation_csvs, output_dir: str, val_frac: float,
            min_val_count: int, jpeg_quality: int, seed: int,
            exclude_labels: list, path_remaps: list = None,
            dry_run: bool = False, no_split: bool = False) -> dict:
    """
    annotation_csvs: a single path (str) or a list of paths. If a list,
    all CSVs are concatenated before filtering/extraction — useful for
    processing several transects' verified exports in one run.

    Returns a stats dict describing what happened (or would happen).

    If no_split=True, every row is written flat to output_dir/<Label>/ with
    no train/val split at all — intended for held-out evaluation transects
    that should never be trained or validated on, only used for inference
    afterward (e.g. to reproduce your accuracy-report methodology on data
    the retrained model has never seen).
    """
    path_remaps = path_remaps or []

    if isinstance(annotation_csvs, (str, Path)):
        annotation_csvs = [annotation_csvs]

    df = _load_and_concat_csvs(annotation_csvs)
    log.info(f"Loaded {len(df):,} annotation rows total from "
             f"{len(annotation_csvs)} file(s)")

    required_cols = {"Name", "Path", "Row", "Column", "Patch Size", "Label", "Verified"}
    missing_cols = required_cols - set(df.columns)
    if missing_cols:
        raise ValueError(f"Input CSV is missing required column(s): {missing_cols}")

    before = len(df)
    df = df[df["Verified"] == True].copy()  # noqa: E712
    log.info(f"Filtered to Verified=True rows: {len(df)} of {before}")

    if exclude_labels:
        before = len(df)
        df = df[~df["Label"].isin(exclude_labels)].copy()
        log.info(f"Excluded labels {exclude_labels}: {len(df)} of {before} remain")

    # Deduplicate rows pointing at the exact same annotation point
    # (same source image, same Row, same Column). Duplicate rows produce
    # byte-identical output patches under different filenames (the __2,
    # __3 collision-avoidance suffix), silently inflating class counts and
    # risking train/val leakage if the two copies land in different splits.
    dedup_cols = ["Path", "Row", "Column"]
    dup_mask = df.duplicated(subset=dedup_cols, keep=False)
    duplicate_rows_dropped = 0
    if dup_mask.any():
        dup_groups = df[dup_mask].groupby(dedup_cols)
        conflicting = 0
        for _, group in dup_groups:
            if group["Label"].nunique() > 1:
                conflicting += 1
                labels = ", ".join(sorted(group["Label"].unique()))
                sample = group.iloc[0]
                log.warning(
                    f"  Duplicate point with CONFLICTING labels kept as "
                    f"'{group.iloc[0]['Label']}' (first occurrence): "
                    f"{sample['Path']} row={sample['Row']} col={sample['Column']} "
                    f"-> labels seen: {labels}"
                )
        before = len(df)
        df = df.drop_duplicates(subset=dedup_cols, keep="first").copy()
        duplicate_rows_dropped = before - len(df)
        log.warning(
            f"Dropped {duplicate_rows_dropped} duplicate annotation row(s) "
            f"(same Path/Row/Column) before extraction — {conflicting} of the "
            f"duplicate groups had conflicting labels (see above; first "
            f"occurrence was kept in each case)."
        )

    stats = {
        "total_rows": len(df),
        "class_counts": {},
        "train_count": 0,
        "val_count": 0,
        "written": 0,
        "skipped": 0,
        "missing_images": 0,
        "duplicate_rows_dropped": duplicate_rows_dropped,
        "dry_run": dry_run,
        "no_split": no_split,
        "updated_csv_path": None,
    }

    if df.empty:
        log.warning("No rows to process after filtering.")
        return stats

    log.info("Class counts (Verified rows to be extracted):")
    for label, count in df["Label"].value_counts().items():
        log.info(f"  {label}: {count}")
        stats["class_counts"][label] = int(count)

    if no_split:
        log.info("No-split mode: all rows go to a flat <Label>/ folder "
                 "(no train/val split) — intended for held-out evaluation data.")
        df["_split"] = ""  # written directly under output_dir/<Label>/, no split subfolder
    else:
        df["_split"] = assign_splits(df, val_frac, min_val_count, seed)
        split_counts = df["_split"].value_counts()
        stats["train_count"] = int(split_counts.get("train", 0))
        stats["val_count"] = int(split_counts.get("val", 0))

    if dry_run:
        log.info(f"DRY RUN — {len(df)} patches would be extracted.")
        if no_split:
            log.info(f"  (no-split mode — all {len(df)} would go to flat <Label>/ folders)")
        else:
            log.info(df["_split"].value_counts().to_string())
        missing = 0
        for _, row in df.iterrows():
            image_path = remap_path(localize_path(str(row["Path"])), path_remaps)
            if not os.path.isfile(image_path):
                log.warning(f"  MISSING source image: {image_path}")
                missing += 1
        stats["missing_images"] = missing
        if missing:
            log.warning(f"{missing} source image(s) not found.")
        else:
            log.info("All source images found \u2713")
        return stats

    updated_csv_path = write_updated_annotations_csv(df, path_remaps, output_dir)
    stats["updated_csv_path"] = updated_csv_path
    log.info(f"Wrote updated-paths annotation CSV: {updated_csv_path}")

    written = 0
    skipped = 0

    for _, row in tqdm(df.iterrows(), total=len(df), desc="Extracting patches", unit="patch"):

        image_path = remap_path(localize_path(str(row["Path"])), path_remaps)
        image_name = str(row["Name"])
        label = sanitize_label(row["Label"])
        split = row["_split"]

        class_dir = os.path.join(output_dir, label) if no_split \
            else os.path.join(output_dir, split, label)
        Path(class_dir).mkdir(parents=True, exist_ok=True)

        img = cv2.imread(image_path)
        if img is None:
            log.warning(f"Could not read image: {image_path}")
            skipped += 1
            continue

        patch_size = int(row["Patch Size"])
        half = patch_size // 2
        r = int(row["Row"])
        c = int(row["Column"])
        h, w = img.shape[:2]

        top = max(r - half, 0)
        bottom = min(r + half, h)
        left = max(c - half, 0)
        right = min(c + half, w)

        patch = img[top:bottom, left:right].copy()
        if patch.size == 0 or patch.shape[0] < patch_size * 0.5 or patch.shape[1] < patch_size * 0.5:
            # Point too close to an image edge to get a usable crop
            log.warning(f"Undersized/empty crop at row={r}, col={c} in {image_path} — skipped")
            skipped += 1
            continue

        base = os.path.splitext(os.path.basename(image_name))[0]
        desired_filename = f"{base}_r{r}_c{c}.jpg"
        patch_filename = unique_filename(class_dir, desired_filename)
        patch_output_path = os.path.join(class_dir, patch_filename)

        encode_params = [cv2.IMWRITE_JPEG_QUALITY, jpeg_quality]
        if not cv2.imwrite(patch_output_path, patch, encode_params):
            log.warning(f"Failed to write: {patch_output_path}")
            skipped += 1
            continue

        written += 1

    stats["written"] = written
    stats["skipped"] = skipped

    log.info("=" * 60)
    log.info(f"Done. {written} patches written, {skipped} skipped.")
    log.info(f"Dataset root: {output_dir}")
    log.info("=" * 60)

    return stats


# ============================================================
# GUI
# ============================================================
def get_args_via_gui():
    result = {}
    cfg = _load_gui_config()

    root = tk.Tk()
    root.title("Extract Training Patches")
    root.resizable(False, False)
    pad = {"padx": 10, "pady": 5}

    ttk.Label(root, text="Extract Training Patches",
              font=("Helvetica", 13, "bold")).grid(
        row=0, column=0, columnspan=3, pady=(14, 2), padx=14)
    ttk.Label(root,
              text="Toolbox annotation CSV(s) -> YOLO classification dataset",
              foreground="grey").grid(row=1, column=0, columnspan=3, pady=(0, 4))
    ttk.Separator(root, orient="horizontal").grid(
        row=2, column=0, columnspan=3, sticky="ew", padx=10, pady=4)

    # ── Annotation CSV(s) — multi-select list, same pattern as
    #    percent_cover_telemetry.py's file-list sections ─────────────────
    ttk.Label(root, text="Toolbox annotation CSV(s):").grid(
        row=3, column=0, columnspan=3, sticky="w", padx=10, pady=(6, 0))

    annotation_list_frame = ttk.Frame(root)
    annotation_list_frame.grid(row=4, column=0, columnspan=3, padx=10, pady=(2, 0), sticky="ew")
    annotation_scrollbar = ttk.Scrollbar(annotation_list_frame, orient="vertical")
    annotation_listbox = tk.Listbox(annotation_list_frame, height=4, width=70,
                                     selectmode="extended",
                                     yscrollcommand=annotation_scrollbar.set)
    annotation_scrollbar.config(command=annotation_listbox.yview)
    annotation_listbox.pack(side="left", fill="x", expand=True)
    annotation_scrollbar.pack(side="right", fill="y")

    annotation_btn_row = ttk.Frame(root)
    annotation_btn_row.grid(row=5, column=0, columnspan=3, pady=(2, 8))

    def add_annotation_files():
        paths = filedialog.askopenfilenames(
            title="Select Toolbox annotation CSV(s)",
            filetypes=[("CSV files", "*.csv"), ("All files", "*.*")],
        )
        for p in paths:
            if p not in annotation_listbox.get(0, "end"):
                annotation_listbox.insert("end", p)

    def remove_selected_annotation():
        for i in reversed(annotation_listbox.curselection()):
            annotation_listbox.delete(i)

    ttk.Button(annotation_btn_row, text="Add files...", command=add_annotation_files).pack(side="left", padx=4)
    ttk.Button(annotation_btn_row, text="Remove selected", command=remove_selected_annotation).pack(side="left", padx=4)

    # Restore last-used annotation CSVs (only those that still exist on disk)
    for p in cfg.get("annotation_csvs", []):
        if Path(p).is_file():
            annotation_listbox.insert("end", p)

    # ── Output directory ────────────────────────────────────────────────
    ttk.Label(root, text="Output dataset folder:").grid(row=6, column=0, sticky="e", **pad)
    outdir_var = tk.StringVar(value=cfg.get("output_dir", ""))
    ttk.Entry(root, textvariable=outdir_var, width=55).grid(row=6, column=1, **pad)

    def browse_outdir():
        p = filedialog.askdirectory(title="Select output dataset folder")
        if p:
            outdir_var.set(p)

    ttk.Button(root, text="Browse...", command=browse_outdir).grid(row=6, column=2, **pad)

    ttk.Separator(root, orient="horizontal").grid(
        row=7, column=0, columnspan=3, sticky="ew", padx=10, pady=6)

    # ── Numeric params ──────────────────────────────────────────────────
    val_frac_label = ttk.Label(root, text="Val fraction:")
    val_frac_label.grid(row=8, column=0, sticky="e", **pad)
    val_frac_var = tk.StringVar(value=str(cfg.get("val_frac", 0.15)))
    val_frac_entry = ttk.Entry(root, textvariable=val_frac_var, width=15)
    val_frac_entry.grid(row=8, column=1, sticky="w", **pad)

    min_val_label = ttk.Label(root, text="Min val count:")
    min_val_label.grid(row=9, column=0, sticky="e", **pad)
    min_val_var = tk.StringVar(value=str(cfg.get("min_val_count", 5)))
    min_val_entry = ttk.Entry(root, textvariable=min_val_var, width=15)
    min_val_entry.grid(row=9, column=1, sticky="w", **pad)

    ttk.Label(root, text="JPEG quality:").grid(row=10, column=0, sticky="e", **pad)
    jpeg_var = tk.StringVar(value=str(cfg.get("jpeg_quality", 95)))
    ttk.Entry(root, textvariable=jpeg_var, width=15).grid(row=10, column=1, sticky="w", **pad)

    ttk.Label(root, text="Random seed:").grid(row=11, column=0, sticky="e", **pad)
    seed_var = tk.StringVar(value=str(cfg.get("seed", 42)))
    ttk.Entry(root, textvariable=seed_var, width=15).grid(row=11, column=1, sticky="w", **pad)

    ttk.Label(root, text="Exclude labels (comma-separated):").grid(row=12, column=0, sticky="e", **pad)
    exclude_var = tk.StringVar(value=cfg.get("exclude_labels", "Review"))
    ttk.Entry(root, textvariable=exclude_var, width=40).grid(row=12, column=1, sticky="w", **pad)

    ttk.Separator(root, orient="horizontal").grid(
        row=13, column=0, columnspan=3, sticky="ew", padx=10, pady=6)

    # ── Path remaps ─────────────────────────────────────────────────────
    ttk.Label(root, text="Path remaps (folder renamed since CSV was exported):").grid(
        row=14, column=0, columnspan=3, sticky="w", padx=10, pady=(2, 0))

    remap_frame = ttk.Frame(root)
    remap_frame.grid(row=15, column=0, columnspan=3, padx=10, pady=(2, 0), sticky="ew")
    ttk.Label(remap_frame, text="Old:").pack(side="left")
    old_var = tk.StringVar()
    ttk.Entry(remap_frame, textvariable=old_var, width=22).pack(side="left", padx=(2, 8))
    ttk.Label(remap_frame, text="New:").pack(side="left")
    new_var = tk.StringVar()
    ttk.Entry(remap_frame, textvariable=new_var, width=22).pack(side="left", padx=(2, 8))

    remap_list_frame = ttk.Frame(root)
    remap_list_frame.grid(row=16, column=0, columnspan=3, padx=10, pady=(4, 0), sticky="ew")
    remap_scrollbar = ttk.Scrollbar(remap_list_frame, orient="vertical")
    remap_listbox = tk.Listbox(remap_list_frame, height=3, width=70,
                                selectmode="extended", yscrollcommand=remap_scrollbar.set)
    remap_scrollbar.config(command=remap_listbox.yview)
    remap_listbox.pack(side="left", fill="x", expand=True)
    remap_scrollbar.pack(side="right", fill="y")

    remap_btn_row = ttk.Frame(root)
    remap_btn_row.grid(row=17, column=0, columnspan=3, pady=(2, 8))

    def add_remap():
        old = old_var.get().strip()
        new = new_var.get().strip()
        if not old:
            messagebox.showerror("Missing input", "Enter the OLD folder/substring to replace.")
            return
        remap_listbox.insert("end", f"{old}=>{new}")
        old_var.set("")
        new_var.set("")

    def remove_selected_remap():
        for i in reversed(remap_listbox.curselection()):
            remap_listbox.delete(i)

    ttk.Button(remap_btn_row, text="Add remap", command=add_remap).pack(side="left", padx=4)
    ttk.Button(remap_btn_row, text="Remove selected", command=remove_selected_remap).pack(side="left", padx=4)

    # Restore saved remaps
    for r in cfg.get("path_remaps", []):
        remap_listbox.insert("end", r)

    ttk.Separator(root, orient="horizontal").grid(
        row=18, column=0, columnspan=3, sticky="ew", padx=10, pady=6)

    # ── No split ─────────────────────────────────────────────────────────
    no_split_var = tk.BooleanVar(value=cfg.get("no_split", False))

    def toggle_val_fields():
        state = "disabled" if no_split_var.get() else "normal"
        val_frac_entry.config(state=state)
        min_val_entry.config(state=state)
        val_frac_label.config(foreground="grey" if state == "disabled" else "black")
        min_val_label.config(foreground="grey" if state == "disabled" else "black")

    ttk.Checkbutton(
        root,
        text="No split  (write all patches flat to <Label>/ folders — for held-out\n"
             "evaluation transects that should never be trained/validated on)",
        variable=no_split_var,
        command=toggle_val_fields,
    ).grid(row=19, column=0, columnspan=3, pady=(0, 4))
    toggle_val_fields()  # apply initial state on window open

    # ── Dry run ──────────────────────────────────────────────────────────
    dry_run_var = tk.BooleanVar(value=cfg.get("dry_run", True))
    ttk.Checkbutton(
        root,
        text="Dry run  (validate inputs and count patches without writing any files)",
        variable=dry_run_var,
    ).grid(row=20, column=0, columnspan=3, pady=(0, 4))

    ttk.Separator(root, orient="horizontal").grid(
        row=21, column=0, columnspan=3, sticky="ew", padx=10, pady=6)

    # ── Run / Cancel buttons ────────────────────────────────────────────
    btn_frame = ttk.Frame(root)
    btn_frame.grid(row=22, column=0, columnspan=3, pady=(0, 14))

    def on_run():
        annotation_csvs = list(annotation_listbox.get(0, "end"))
        output_dir = outdir_var.get().strip()

        if not annotation_csvs:
            messagebox.showerror("Missing input", "Please add at least one Toolbox annotation CSV.")
            return
        for p in annotation_csvs:
            if not Path(p).is_file():
                messagebox.showerror("File not found", f"File not found:\n{p}")
                return
        if not output_dir:
            messagebox.showerror("Missing input", "Please select an output dataset folder.")
            return

        try:
            val_frac = float(val_frac_var.get())
            if not (0 < val_frac < 1):
                raise ValueError
        except ValueError:
            messagebox.showerror("Invalid value", "Val fraction must be a number between 0 and 1 (e.g. 0.15).")
            return

        try:
            min_val_count = int(min_val_var.get())
        except ValueError:
            messagebox.showerror("Invalid value", "Min val count must be a whole number.")
            return

        try:
            jpeg_quality = int(jpeg_var.get())
            if not (1 <= jpeg_quality <= 100):
                raise ValueError
        except ValueError:
            messagebox.showerror("Invalid value", "JPEG quality must be a whole number 1-100.")
            return

        try:
            seed = int(seed_var.get())
        except ValueError:
            messagebox.showerror("Invalid value", "Random seed must be a whole number.")
            return

        exclude_labels = [s.strip() for s in exclude_var.get().split(",") if s.strip()]

        path_remaps = []
        for item in remap_listbox.get(0, "end"):
            old, _, new = item.partition("=>")
            path_remaps.append((old, new))

        result["annotation_csvs"] = annotation_csvs
        result["output_dir"] = output_dir
        result["val_frac"] = val_frac
        result["min_val_count"] = min_val_count
        result["jpeg_quality"] = jpeg_quality
        result["seed"] = seed
        result["exclude_labels"] = exclude_labels
        result["path_remaps"] = path_remaps
        result["dry_run"] = dry_run_var.get()
        result["no_split"] = no_split_var.get()
        result["submitted"] = True

        _save_gui_config({
            "annotation_csvs": annotation_csvs,
            "output_dir": output_dir,
            "val_frac": val_frac,
            "min_val_count": min_val_count,
            "jpeg_quality": jpeg_quality,
            "seed": seed,
            "exclude_labels": exclude_var.get(),
            "path_remaps": list(remap_listbox.get(0, "end")),
            "dry_run": dry_run_var.get(),
            "no_split": no_split_var.get(),
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

    log.info("Toolbox annotation CSV(s) -> YOLO classification dataset")
    log.info(f"  annotation CSVs : {args['annotation_csvs']}")
    log.info(f"  output dir      : {args['output_dir']}")
    log.info(f"  val fraction    : {args['val_frac']}")
    log.info(f"  min val count   : {args['min_val_count']}")
    log.info(f"  exclude labels  : {args['exclude_labels']}")
    if args["path_remaps"]:
        log.info(f"  path remaps     : {args['path_remaps']}")
    if args["no_split"]:
        log.info("  mode            : NO SPLIT (flat <Label>/ folders, held-out eval data)")
    if args["dry_run"]:
        log.info("  mode            : DRY RUN")

    try:
        stats = extract(
            annotation_csvs=args["annotation_csvs"],
            output_dir=args["output_dir"],
            val_frac=args["val_frac"],
            min_val_count=args["min_val_count"],
            jpeg_quality=args["jpeg_quality"],
            seed=args["seed"],
            exclude_labels=args["exclude_labels"],
            path_remaps=args["path_remaps"],
            dry_run=args["dry_run"],
            no_split=args["no_split"],
        )
    except ValueError as e:
        log.error(str(e))
        messagebox.showerror("Invalid annotation CSV", str(e))
        sys.exit(1)

    # ── Summary popup ────────────────────────────────────────────────────
    if stats["dry_run"]:
        lines = [
            f"DRY RUN — no files written.",
            "",
            f"Verified rows to extract: {stats['total_rows']:,}",
            f"Classes: {len(stats['class_counts'])}",
        ]
        if stats["duplicate_rows_dropped"]:
            lines.append(f"\u26a0 {stats['duplicate_rows_dropped']:,} duplicate annotation "
                         f"row(s) (same Path/Row/Column) would be dropped before extraction "
                         f"— see log for details")
        if stats["no_split"]:
            lines.append(f"Mode: no split — all {stats['total_rows']:,} would go to "
                         f"flat <Label>/ folders")
        else:
            lines.append(f"Would assign: {stats['train_count']:,} train / "
                         f"{stats['val_count']:,} val")
        if stats["missing_images"]:
            lines.append("")
            lines.append(f"\u26a0 {stats['missing_images']:,} source image(s) NOT found "
                          f"— see log for paths.")
        else:
            lines.append("")
            lines.append("All source images found \u2713")
        messagebox.showinfo("Dry run complete", "\n".join(lines))
    else:
        lines = [
            f"Patches written: {stats['written']:,}",
            f"Skipped (missing/undersized): {stats['skipped']:,}",
        ]
        if stats["duplicate_rows_dropped"]:
            lines.append(f"\u26a0 {stats['duplicate_rows_dropped']:,} duplicate annotation "
                         f"row(s) dropped before extraction — see log for details")
        if stats["no_split"]:
            lines.append("Mode: no split (flat <Label>/ folders)")
        else:
            lines.append(f"Train / val: {stats['train_count']:,} / {stats['val_count']:,}")
        lines.append("")
        lines.append(f"Saved to:\n{args['output_dir']}")
        if stats["updated_csv_path"]:
            lines.append(f"Updated-paths CSV:\n{stats['updated_csv_path']}")
        if stats["skipped"]:
            messagebox.showwarning("Extraction complete", "\n".join(lines))
        else:
            messagebox.showinfo("Extraction complete", "\n".join(lines))


if __name__ == "__main__":
    main()