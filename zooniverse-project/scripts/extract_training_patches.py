#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
extract_training_patches.py — Toolbox annotation CSV -> YOLO classification dataset

This sits DOWNSTREAM of zooni_to_toolbox_annot.py in the workflow:

    toolbox_import.csv  ->  [this script]  ->  dataset/train/<Label>/*.jpg
                                                 dataset/val/<Label>/*.jpg

For each row where Verified == True, this script:
    - Loads the source image (via the same Windows-user-path localization
      trick used in toolbox_to_subjects.py)
    - Crops EXACTLY Patch Size x Patch Size centered on (Row, Column) —
      no overlay, no scaling, no burned-in text (unlike the Zooniverse
      review crops)
    - Saves it to dataset/<split>/<Label>/<source>_r<row>_c<col>.jpg
    - Assigns each row to train or val with a per-class stratified split,
      so rare classes aren't accidentally left out of one split entirely

Rows where Verified != True are skipped by default (their Label is a
placeholder, e.g. "Review", not a real classification).

This does NOT merge with your existing balanced training set or run
any class-balancing itself — it only extracts and splits what's in
the input CSV. Merge/balance as a separate, explicit step so you can
inspect class counts before combining.

Usage:
    python extract_training_patches.py \
        --annotation-csv toolbox_import.csv \
        --output-dir dataset_2025_01_28_EBM_T1 \
        --val-frac 0.15 \
        --min-val-count 5

    Add --dry-run to validate inputs and print per-class counts without
    writing any files.

Requirements:
    pip install opencv-python-headless pandas tqdm
"""

import argparse
import getpass
import logging
import os
import random
import re
import sys
from pathlib import Path

import cv2
import pandas as pd
from tqdm import tqdm

# ── Logging ──────────────────────────────────────────────────────────────
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[
        logging.FileHandler("extract_training_patches_log.txt"),
        logging.StreamHandler(sys.stdout),
    ],
)
log = logging.getLogger(__name__)


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
                f"train (below --min-val-count={min_val_count})"
            )
            continue

        n_val = max(1, round(n * val_frac))
        val_idx = idx[:n_val]
        train_idx = idx[n_val:]
        split.loc[val_idx] = "val"
        split.loc[train_idx] = "train"

    return split


# ============================================================
# CORE EXTRACTION
# ============================================================
def extract(annotation_csv: str, output_dir: str, val_frac: float,
            min_val_count: int, jpeg_quality: int, seed: int,
            exclude_labels: list, dry_run: bool = False) -> None:

    df = pd.read_csv(annotation_csv)

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

    if df.empty:
        log.warning("No rows to process after filtering.")
        return

    log.info("Class counts (Verified rows to be extracted):")
    for label, count in df["Label"].value_counts().items():
        log.info(f"  {label}: {count}")

    df["_split"] = assign_splits(df, val_frac, min_val_count, seed)

    if dry_run:
        log.info(f"DRY RUN — {len(df)} patches would be extracted.")
        log.info(df["_split"].value_counts().to_string())
        missing = 0
        for _, row in df.iterrows():
            image_path = localize_path(str(row["Path"]))
            if not os.path.isfile(image_path):
                log.warning(f"  MISSING source image: {image_path}")
                missing += 1
        if missing:
            log.warning(f"{missing} source image(s) not found.")
        else:
            log.info("All source images found \u2713")
        return

    written = 0
    skipped = 0

    for _, row in tqdm(df.iterrows(), total=len(df), desc="Extracting patches", unit="patch"):

        image_path = localize_path(str(row["Path"]))
        image_name = str(row["Name"])
        label = sanitize_label(row["Label"])
        split = row["_split"]

        class_dir = os.path.join(output_dir, split, label)
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

    log.info("=" * 60)
    log.info(f"Done. {written} patches written, {skipped} skipped.")
    log.info(f"Dataset root: {output_dir}")
    log.info("=" * 60)


# ============================================================
# ENTRY POINT
# ============================================================
def main():
    parser = argparse.ArgumentParser(
        description="Extract Verified=True patches from a Toolbox annotation CSV "
                     "into a YOLO-classification-ready train/val folder structure."
    )
    parser.add_argument("--annotation-csv", required=True,
                         help="Path to the Toolbox-format annotation CSV "
                              "(e.g. toolbox_import.csv)")
    parser.add_argument("--output-dir", required=True,
                         help="Root output folder. Will contain train/<Label>/ "
                              "and val/<Label>/ subfolders.")
    parser.add_argument("--val-frac", type=float, default=0.15,
                         help="Fraction of each class held out for validation "
                              "(default 0.15)")
    parser.add_argument("--min-val-count", type=int, default=5,
                         help="Classes with fewer than this many Verified samples "
                              "go entirely to train (default 5)")
    parser.add_argument("--jpeg-quality", type=int, default=95)
    parser.add_argument("--seed", type=int, default=42)
    parser.add_argument("--exclude-labels", nargs="*", default=["Review"],
                         help="Label values to drop even if Verified=True "
                              "(default: Review)")
    parser.add_argument("--dry-run", action="store_true",
                         help="Validate inputs and print counts without writing files")
    args = parser.parse_args()

    log.info("Toolbox annotation CSV -> YOLO classification dataset")
    log.info(f"  annotation CSV  : {args.annotation_csv}")
    log.info(f"  output dir      : {args.output_dir}")
    log.info(f"  val fraction    : {args.val_frac}")
    log.info(f"  min val count   : {args.min_val_count}")
    log.info(f"  exclude labels  : {args.exclude_labels}")
    if args.dry_run:
        log.info("  mode            : DRY RUN")

    extract(
        annotation_csv=args.annotation_csv,
        output_dir=args.output_dir,
        val_frac=args.val_frac,
        min_val_count=args.min_val_count,
        jpeg_quality=args.jpeg_quality,
        seed=args.seed,
        exclude_labels=args.exclude_labels,
        dry_run=args.dry_run,
    )


if __name__ == "__main__":
    main()