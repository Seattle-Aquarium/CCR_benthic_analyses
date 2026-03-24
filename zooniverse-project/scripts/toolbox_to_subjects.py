#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
toolbox_to_subjects.py  —  CoralNet-Toolbox → Zooniverse patch extractor
Version: 1.0

This script sits UPSTREAM of import_subjects.py in the workflow:

    dataset.csv  →  [this script]  →  patches/ + metadata.csv
                                            ↓
                                   [import_subjects.py]
                                            ↓
                                       Zooniverse

For each annotation row in dataset.csv this script:
    - Computes the original small patch bounds (Patch Size around Row/Column)
    - Extracts a LARGER crop centered on the same point (--scale × Patch Size)
    - Overlays a rectangle showing the original small patch area
    - Overlays a crosshair center-point marker (for percent cover validation)
    - Burns the Toolbox model prediction label onto the image
    - Saves the large crop as:  <source_image>_r<row>_c<col>.jpg
    - Writes metadata.csv ready for import_subjects.py:
        filename, source_image, row, column, model_pred_code, model_pred_name

NOTE:
    source_image is the original image name WITHOUT the file extension so
    Zooniverse does not mistake it for another media file to upload.

Usage:
    python toolbox_to_subjects.py \\
        --dataset-csv  /path/to/dataset.csv \\
        --output-dir   /path/to/patches/ \\
        --metadata-csv /path/to/patches/metadata.csv

    # Larger crops (3.5× the Toolbox patch size):
    python toolbox_to_subjects.py \\
        --dataset-csv dataset.csv \\
        --output-dir  patches/ \\
        --metadata-csv patches/metadata.csv \\
        --scale 3.5

    # Dry run — validate inputs and count rows without writing any files:
    python toolbox_to_subjects.py \\
        --dataset-csv dataset.csv \\
        --output-dir  patches/ \\
        --metadata-csv patches/metadata.csv \\
        --dry-run

Requirements:
    pip install opencv-python-headless pandas tqdm
"""

import os
import sys
import argparse
import logging
from pathlib import Path

import cv2
import pandas as pd
from tqdm import tqdm

# ── Logging ──────────────────────────────────────────────────────────────────
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[
        logging.FileHandler("toolbox_extract_log.txt"),
        logging.StreamHandler(sys.stdout),
    ],
)
log = logging.getLogger(__name__)


# ============================================================
# CLI
# ============================================================
def parse_args():
    p = argparse.ArgumentParser(
        description="Extract Zooniverse-ready patches from CoralNet-Toolbox annotations",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    p.add_argument("--dataset-csv",  required=True,
                   help="Path to the dataset.csv exported from CoralNet-Toolbox")
    p.add_argument("--output-dir",   required=True,
                   help="Directory where cropped patch images will be saved")
    p.add_argument("--metadata-csv", required=True,
                   help="Output path for metadata.csv (consumed by import_subjects.py)")
    p.add_argument("--scale",        type=float, default=3.5,
                   help="Crop size multiplier relative to Toolbox Patch Size (must be ≥ 1.0)")
    p.add_argument("--jpeg-quality", type=int, default=95,
                   help="JPEG output quality 1–100")
    p.add_argument("--dry-run",      action="store_true",
                   help="Validate inputs and count rows without writing any files")
    return p.parse_args()


def validate_args(args):
    if not Path(args.dataset_csv).is_file():
        log.error(f"dataset.csv not found: {args.dataset_csv}")
        sys.exit(1)
    if args.scale < 1.0:
        log.warning(f"--scale {args.scale} is less than 1.0; clamping to 1.0")
        args.scale = 1.0
    if not (1 <= args.jpeg_quality <= 100):
        log.warning(f"--jpeg-quality {args.jpeg_quality} out of range; clamping to 95")
        args.jpeg_quality = 95


# ============================================================
# IMAGE OVERLAY HELPERS
# (Logic unchanged from original — only docstrings added)
# ============================================================

def draw_center_marker(img_bgr, x: int, y: int, patch_size: int) -> None:
    """
    Draw a crosshair (ring + ticks) at the exact annotation point.
    Marker dimensions scale with patch_size but are clamped to stay readable.
    """
    h, w = img_bgr.shape[:2]
    if not (0 <= x < w and 0 <= y < h):
        return

    color = (0, 0, 255)  # red (BGR)

    inner_radius  = max(6,  min(int(patch_size * 0.06), 14))
    ring_thickness = max(1, min(int(patch_size * 0.01),  3))
    tick_length   = max(10, min(int(patch_size * 0.10), 22))
    tick_thickness = max(1, min(int(patch_size * 0.01),  3))
    gap           = max(3,  min(int(patch_size * 0.02),  6))

    # Ring
    cv2.circle(img_bgr, (x, y), inner_radius, color, ring_thickness, lineType=cv2.LINE_AA)
    # Filled centre dot (true point-intercept)
    cv2.circle(img_bgr, (x, y), 2, color, -1, lineType=cv2.LINE_AA)
    # Ticks (up / down / left / right)
    cv2.line(img_bgr, (x, y - inner_radius - gap),
             (x, y - inner_radius - gap - tick_length), color, tick_thickness, lineType=cv2.LINE_AA)
    cv2.line(img_bgr, (x, y + inner_radius + gap),
             (x, y + inner_radius + gap + tick_length), color, tick_thickness, lineType=cv2.LINE_AA)
    cv2.line(img_bgr, (x - inner_radius - gap, y),
             (x - inner_radius - gap - tick_length, y), color, tick_thickness, lineType=cv2.LINE_AA)
    cv2.line(img_bgr, (x + inner_radius + gap, y),
             (x + inner_radius + gap + tick_length, y), color, tick_thickness, lineType=cv2.LINE_AA)


def draw_model_label(img_bgr, model_pred_name: str, model_pred_code: str = None,
                     patch_size: int = 224,
                     rect_left=None, rect_top=None,
                     rect_right=None, rect_bottom=None) -> None:
    """
    Burn the Toolbox model prediction label onto the patch image.
    White text with a black outline; positioned to avoid overlapping the rectangle.
    """
    text = model_pred_name if model_pred_name else "Model: (missing)"
    font = cv2.FONT_HERSHEY_SIMPLEX

    font_scale = max(0.6, min(patch_size / 180.0, 2.0))
    thickness  = max(1, int(font_scale * 2))
    h, w       = img_bgr.shape[:2]
    max_width  = max(8, w - 8)

    (text_width, text_height), baseline = cv2.getTextSize(text, font, font_scale, thickness)

    # Shrink font until text fits horizontally
    while text_width > max_width and font_scale > 0.4:
        font_scale *= 0.9
        thickness   = max(1, int(font_scale * 2))
        (text_width, text_height), baseline = cv2.getTextSize(text, font, font_scale, thickness)

    # Truncate with ellipsis if still too wide
    if text_width > max_width:
        while len(text) > 1:
            text = text[:-1]
            candidate = text + "..."
            (text_width, _), _ = cv2.getTextSize(candidate, font, font_scale, thickness)
            if text_width <= max_width:
                text = candidate
                break

    x       = max(4, (w // 2) - text_width // 2)
    padding = max(12, int(10 * font_scale))

    if all(v is not None for v in (rect_left, rect_top, rect_right, rect_bottom)):
        rect_center_x = int(rect_left + (rect_right - rect_left) / 2)
        x = max(4, min(w - 4 - text_width, rect_center_x - text_width // 2))
        if rect_top - padding - text_height >= 0:
            y = rect_top - padding
        elif rect_bottom + padding + text_height <= h:
            y = rect_bottom + padding + text_height
        else:
            y = max(text_height + 4, min(h - 4, rect_top - padding))
    else:
        y = max(text_height + baseline + 4, int(15 * font_scale + 8))

    cv2.putText(img_bgr, text, (x, y), font, font_scale, (0, 0, 0),   thickness + 3, cv2.LINE_AA)
    cv2.putText(img_bgr, text, (x, y), font, font_scale, (255, 255, 255), thickness, cv2.LINE_AA)


def draw_patch_box(img_bgr,
                   rect_left: int, rect_top: int,
                   rect_right: int, rect_bottom: int) -> None:
    """
    Draw the three-layer rectangle that marks the original Toolbox patch boundary:
      outer black outline → bright green main box → inner black border.
    """
    cv2.rectangle(img_bgr,
                  (rect_left - 2, rect_top - 2),
                  (rect_right + 2, rect_bottom + 2),
                  (0, 0, 0), 1, lineType=cv2.LINE_AA)
    cv2.rectangle(img_bgr,
                  (rect_left, rect_top),
                  (rect_right, rect_bottom),
                  (0, 255, 0), 2, lineType=cv2.LINE_AA)
    cv2.rectangle(img_bgr,
                  (rect_left + 2, rect_top + 2),
                  (rect_right - 2, rect_bottom - 2),
                  (0, 0, 0), 1, lineType=cv2.LINE_AA)


# ============================================================
# FILENAME HELPER
# ============================================================
def unique_filename(output_dir: str, filename: str) -> str:
    """
    Return a filename that does not already exist in output_dir.
    Appends __2, __3, … if a collision is found.
    """
    base, ext = os.path.splitext(filename)
    candidate = filename
    n = 2
    while os.path.exists(os.path.join(output_dir, candidate)):
        candidate = f"{base}__{n}{ext}"
        n += 1
    return candidate


# ============================================================
# CORE EXTRACTION
# ============================================================
def extract_patches(dataset_csv: str, output_dir: str, metadata_csv: str,
                    scale_factor: float, jpeg_quality: int,
                    dry_run: bool = False) -> int:
    """
    Main extraction loop.
    Returns the number of patches successfully written (or that would be written
    in dry-run mode).
    """
    df = pd.read_csv(dataset_csv)

    # Keep only Patch-type annotations if the column exists
    if "Annotation Type" in df.columns:
        before = len(df)
        df = df[df["Annotation Type"] == "Patch"].copy()
        log.info(f"Filtered to Patch annotations: {len(df)} of {before} rows")
    else:
        log.info(f"No 'Annotation Type' column found — processing all {len(df)} rows")

    if df.empty:
        log.warning("No rows to process after filtering.")
        return 0

    if dry_run:
        log.info(f"DRY RUN — {len(df)} patches would be extracted. No files written.")
        # Validate that source images actually exist
        missing = 0
        for _, row in df.iterrows():
            if not os.path.isfile(str(row["Path"])):
                log.warning(f"  MISSING source image: {row['Path']}")
                missing += 1
        if missing:
            log.warning(f"{missing} source image(s) not found.")
        else:
            log.info("All source images found ✓")
        return len(df) - missing

    Path(output_dir).mkdir(parents=True, exist_ok=True)

    metadata_rows = []
    skipped       = 0

    for _, row in tqdm(df.iterrows(), total=len(df), desc="Extracting patches", unit="patch"):

        image_path = str(row["Path"])
        image_name = str(row["Name"])

        img = cv2.imread(image_path)
        if img is None:
            log.warning(f"Could not read image: {image_path}")
            skipped += 1
            continue

        patch_size = int(row["Patch Size"])
        half       = patch_size // 2
        r          = int(row["Row"])
        c          = int(row["Column"])
        h, w       = img.shape[:2]

        # ── Small patch bounds (drawn as rectangle overlay) ───────────────
        small_top    = max(r - half, 0)
        small_bottom = min(r + half, h)
        small_left   = max(c - half, 0)
        small_right  = min(c + half, w)

        # ── Large crop bounds (square, shifted away from edges) ───────────
        crop_size = int(round(patch_size * scale_factor))
        crop_half = crop_size // 2
        left  = max(0, c - crop_half)
        top   = max(0, r - crop_half)
        right = left + crop_size
        bottom = top + crop_size
        if right > w:
            left  -= (right - w)
            right  = w
        if bottom > h:
            top   -= (bottom - h)
            bottom = h
        left = max(0, left)
        top  = max(0, top)

        large_patch = img[top:bottom, left:right].copy()
        if large_patch.size == 0:
            log.warning(f"Empty crop at row={r}, col={c} in {image_path}")
            skipped += 1
            continue

        # ── Rectangle coordinates relative to the crop origin ────────────
        rect_left   = small_left   - left
        rect_top    = small_top    - top
        rect_right  = small_right  - left
        rect_bottom = small_bottom - top

        # ── Overlays ──────────────────────────────────────────────────────
        draw_patch_box(large_patch, rect_left, rect_top, rect_right, rect_bottom)

        model_pred_code = (str(row["Label"]).strip()
                           if "Label" in df.columns and pd.notna(row.get("Label"))
                           else "")
        model_pred_name = (str(row["Long Label"]).strip()
                           if "Long Label" in df.columns and pd.notna(row.get("Long Label"))
                           else "")

        draw_center_marker(large_patch, c - left, r - top, patch_size)
        draw_model_label(large_patch, model_pred_name, model_pred_code,
                         patch_size, rect_left, rect_top, rect_right, rect_bottom)

        # ── Save patch ────────────────────────────────────────────────────
        base             = os.path.splitext(os.path.basename(image_name))[0]
        desired_filename = f"{base}_r{r}_c{c}.jpg"
        patch_filename   = unique_filename(output_dir, desired_filename)
        patch_output_path = os.path.join(output_dir, patch_filename)

        encode_params = [cv2.IMWRITE_JPEG_QUALITY, jpeg_quality]
        if not cv2.imwrite(patch_output_path, large_patch, encode_params):
            log.warning(f"Failed to write: {patch_output_path}")
            skipped += 1
            continue

        # source_image stored WITHOUT extension so Zooniverse doesn't try to
        # upload it as a media file
        source_image = os.path.splitext(image_name)[0]

        metadata_rows.append({
            "filename":        patch_filename,
            "source_image":    source_image,
            "row":             r,
            "column":          c,
            "model_pred_code": model_pred_code,
            "model_pred_name": model_pred_name,
        })

    # ── Write metadata.csv ────────────────────────────────────────────────────
    if metadata_rows:
        Path(metadata_csv).parent.mkdir(parents=True, exist_ok=True)
        pd.DataFrame(metadata_rows).to_csv(metadata_csv, index=False)
        log.info(f"Wrote {len(metadata_rows)} rows to {metadata_csv}")
    else:
        log.warning("No patches extracted — metadata.csv not written.")

    if skipped:
        log.warning(f"{skipped} patch(es) skipped (see log above for details).")

    return len(metadata_rows)


# ============================================================
# ENTRY POINT
# ============================================================
def main():
    args = parse_args()
    validate_args(args)

    log.info("CoralNet-Toolbox → Zooniverse patch extractor")
    log.info(f"  dataset CSV  : {args.dataset_csv}")
    log.info(f"  output dir   : {args.output_dir}")
    log.info(f"  metadata CSV : {args.metadata_csv}")
    log.info(f"  scale factor : {args.scale}×")
    log.info(f"  JPEG quality : {args.jpeg_quality}")
    if args.dry_run:
        log.info("  mode         : DRY RUN")

    n = extract_patches(
        dataset_csv  = args.dataset_csv,
        output_dir   = args.output_dir,
        metadata_csv = args.metadata_csv,
        scale_factor = args.scale,
        jpeg_quality = args.jpeg_quality,
        dry_run      = args.dry_run,
    )

    if not args.dry_run:
        log.info("=" * 60)
        log.info(f"✅  Done!  {n} patches written to {args.output_dir}")
        log.info(f"    Next step: run import_subjects.py pointing at {args.output_dir}")
        log.info("=" * 60)


if __name__ == "__main__":
    main()
