#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
toolbox_to_subjects.py  —  CoralNet-Toolbox → Zooniverse patch extractor

This script sits UPSTREAM of import_subjects.py in the workflow:

    annotation.csv  →  [this script]  →  patches/ + metadata.csv
                                            ↓
                                   [import_subjects.py]
                                            ↓
                                       Zooniverse

For each annotation row in annotation.csv this script:
    - Computes the original small patch bounds (Patch Size around Row/Column)
    - Extracts a LARGER crop centered on the same point (--scale × Patch Size)
    - Overlays a rectangle showing the original small patch area
    - Overlays a crosshair center-point marker (for percent cover validation)
    - Burns the Toolbox model prediction label onto the image
    - Saves the large crop as:  <source_image>_r<row>_c<col>.jpg
    - Writes metadata.csv ready for import_subjects.py:
        filename, source_image, row, column, model_pred_code, model_pred_name,
        site_name, survey_date, transect_number

NOTE:
    source_image is the original image name WITHOUT the file extension so
    Zooniverse does not mistake it for another media file to upload.

Usage:
    python toolbox_to_subjects.py

    A window will open asking you to select:
      1. The annotation.csv exported from CoralNet-Toolbox
      2. The output folder where patch images will be saved
      3. The path for the output metadata.csv
            4. Site name, survey date, and transect number
            5. The scale factor (default 3.5)
            6. Whether to do a dry run only

Requirements:
    pip install opencv-python-headless pandas tqdm
"""

import os
import re
import sys
import types
import getpass
import logging
import tkinter as tk
from tkinter import ttk, filedialog, messagebox
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
# PATH LOCALIZATION
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
# GUI INPUT FORM
# ============================================================
def get_args_via_gui():
    """Open a friendly GUI form to collect all run parameters."""
    result = {}

    root = tk.Tk()
    root.title("CoralNet-Toolbox → Zooniverse Patch Extractor")
    root.resizable(False, False)

    pad = {"padx": 10, "pady": 5}

    # ── Header ────────────────────────────────────────────────────────────
    ttk.Label(root,
              text="CoralNet-Toolbox  →  Zooniverse Patch Extractor",
              font=("Helvetica", 13, "bold")
              ).grid(row=0, column=0, columnspan=3, pady=(14, 4), padx=14)
    ttk.Label(root,
              text="Fill in the fields below, then click Run.",
              foreground="grey"
              ).grid(row=1, column=0, columnspan=3, pady=(0, 4))
    ttk.Separator(root, orient="horizontal").grid(
        row=2, column=0, columnspan=3, sticky="ew", padx=10, pady=4)

    # ── annotation CSV ───────────────────────────────────────────────────────
    ttk.Label(root, text="Annotation CSV:").grid(row=3, column=0, sticky="e", **pad)
    annotation_var = tk.StringVar()
    ttk.Entry(root, textvariable=annotation_var, width=55).grid(row=3, column=1, **pad)

    def browse_annotation():
        path = filedialog.askopenfilename(
            title="Select the annotation.csv exported from CoralNet-Toolbox",
            filetypes=[("CSV files", "*.csv"), ("All files", "*.*")]
        )
        if path:
            annotation_var.set(path)
            _auto_fill_metadata()

    ttk.Button(root, text="Browse…", command=browse_annotation).grid(row=3, column=2, **pad)

    # ── Output Directory ──────────────────────────────────────────────────
    ttk.Label(root, text="Output folder:").grid(row=4, column=0, sticky="e", **pad)
    outdir_var = tk.StringVar()
    ttk.Entry(root, textvariable=outdir_var, width=55).grid(row=4, column=1, **pad)

    def browse_outdir():
        path = filedialog.askdirectory(
            title="Select (or create) the folder where patch images will be saved")
        if path:
            outdir_var.set(path)
            _auto_fill_metadata()

    ttk.Button(root, text="Browse…", command=browse_outdir).grid(row=4, column=2, **pad)

    # ── Metadata CSV ──────────────────────────────────────────────────────
    ttk.Label(root, text="Metadata CSV output:").grid(row=5, column=0, sticky="e", **pad)
    meta_var = tk.StringVar()
    ttk.Entry(root, textvariable=meta_var, width=55).grid(row=5, column=1, **pad)

    def browse_meta():
        path = filedialog.asksaveasfilename(
            title="Save metadata CSV as…",
            defaultextension=".csv",
            filetypes=[("CSV files", "*.csv"), ("All files", "*.*")]
        )
        if path:
            meta_var.set(path)

    ttk.Button(root, text="Browse…", command=browse_meta).grid(row=5, column=2, **pad)

    def _auto_fill_metadata():
        """Auto-suggest metadata.csv path inside the chosen output folder."""
        if outdir_var.get() and not meta_var.get():
            meta_var.set(str(Path(outdir_var.get()) / "metadata.csv"))

    ttk.Separator(root, orient="horizontal").grid(
        row=6, column=0, columnspan=3, sticky="ew", padx=10, pady=4)

    # ── Survey metadata (written to every metadata.csv row) ─────────────
    ttk.Label(root, text="Site name:").grid(row=7, column=0, sticky="e", **pad)
    site_name_var = tk.StringVar()
    ttk.Entry(root, textvariable=site_name_var, width=55).grid(row=7, column=1, **pad)

    ttk.Label(root, text="Survey date (YYYY-MM-DD):").grid(row=8, column=0, sticky="e", **pad)
    survey_date_var = tk.StringVar()
    ttk.Entry(root, textvariable=survey_date_var, width=55).grid(row=8, column=1, **pad)

    ttk.Label(root, text="Transect number:").grid(row=9, column=0, sticky="e", **pad)
    transect_number_var = tk.StringVar()
    ttk.Entry(root, textvariable=transect_number_var, width=55).grid(row=9, column=1, **pad)

    # ── Scale factor ──────────────────────────────────────────────────────
    ttk.Label(root, text="Scale factor:").grid(row=10, column=0, sticky="e", **pad)
    scale_var = tk.StringVar(value="3.5")
    sf = ttk.Frame(root)
    sf.grid(row=10, column=1, sticky="w", **pad)
    ttk.Entry(sf, textvariable=scale_var, width=8).pack(side="left")
    ttk.Label(sf, text="  ×  patch size   (e.g. 3.5 = crop 3.5× the Toolbox patch)",
              foreground="grey").pack(side="left")

    # ── JPEG quality ──────────────────────────────────────────────────────
    ttk.Label(root, text="JPEG quality:").grid(row=11, column=0, sticky="e", **pad)
    quality_var = tk.StringVar(value="95")
    qf = ttk.Frame(root)
    qf.grid(row=11, column=1, sticky="w", **pad)
    ttk.Entry(qf, textvariable=quality_var, width=8).pack(side="left")
    ttk.Label(qf, text="  1–100   (95 is a good default)",
              foreground="grey").pack(side="left")

    # ── Dry run ───────────────────────────────────────────────────────────
    dry_run_var = tk.BooleanVar(value=False)
    df = ttk.Frame(root)
    df.grid(row=12, column=0, columnspan=3, sticky="w", padx=18, pady=4)
    ttk.Checkbutton(
        df,
        text="Dry run  (validate inputs and count patches without writing any files)",
        variable=dry_run_var,
    ).pack(side="left")

    ttk.Separator(root, orient="horizontal").grid(
        row=13, column=0, columnspan=3, sticky="ew", padx=10, pady=6)

    # ── Run / Cancel buttons ──────────────────────────────────────────────
    btn_frame = ttk.Frame(root)
    btn_frame.grid(row=14, column=0, columnspan=3, pady=(0, 14))

    def on_run():
        if not annotation_var.get():
            messagebox.showerror("Missing input", "Please select a annotation CSV file.")
            return
        if not Path(annotation_var.get()).is_file():
            messagebox.showerror("File not found",
                                 f"annotation CSV not found:\n{annotation_var.get()}")
            return
        if not outdir_var.get():
            messagebox.showerror("Missing input", "Please select an output folder.")
            return
        if not meta_var.get():
            messagebox.showerror("Missing input",
                                 "Please specify a path for the metadata CSV output.")
            return
        site_name = site_name_var.get().strip()
        survey_date = survey_date_var.get().strip()
        transect_number = transect_number_var.get().strip()
        if not site_name:
            messagebox.showerror("Missing input", "Please enter a site name.")
            return
        if not survey_date:
            messagebox.showerror("Missing input", "Please enter a survey date.")
            return
        if not transect_number:
            messagebox.showerror("Missing input", "Please enter a transect number.")
            return
        try:
            scale = float(scale_var.get())
            if scale < 1.0:
                messagebox.showwarning("Scale clamped",
                    "Scale factor is less than 1.0 — it will be set to 1.0.")
                scale = 1.0
        except ValueError:
            messagebox.showerror("Invalid scale",
                                 "Scale factor must be a number (e.g. 3.5).")
            return
        try:
            quality = int(quality_var.get())
            if not (1 <= quality <= 100):
                messagebox.showwarning("Quality clamped",
                    "JPEG quality must be 1–100. It will be set to 95.")
                quality = 95
        except ValueError:
            messagebox.showerror("Invalid quality",
                                 "JPEG quality must be a whole number (e.g. 95).")
            return

        result["annotation_csv"]  = annotation_var.get()
        result["output_dir"]   = outdir_var.get()
        result["metadata_csv"] = meta_var.get()
        result["site_name"] = site_name
        result["survey_date"] = survey_date
        result["transect_number"] = transect_number
        result["scale"]        = scale
        result["jpeg_quality"] = quality
        result["dry_run"]      = dry_run_var.get()
        result["submitted"]    = True
        root.destroy()

    def on_cancel():
        root.destroy()

    ttk.Button(btn_frame, text="  Run  ", command=on_run).pack(side="left", padx=8)
    ttk.Button(btn_frame, text="Cancel",  command=on_cancel).pack(side="left", padx=8)

    root.mainloop()

    if not result.get("submitted"):
        print("Cancelled by user.")
        sys.exit(0)

    return types.SimpleNamespace(
        annotation_csv  = result["annotation_csv"],
        output_dir   = result["output_dir"],
        metadata_csv = result["metadata_csv"],
        site_name    = result["site_name"],
        survey_date  = result["survey_date"],
        transect_number = result["transect_number"],
        scale        = result["scale"],
        jpeg_quality = result["jpeg_quality"],
        dry_run      = result["dry_run"],
    )


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
def extract_patches(annotation_csv: str, output_dir: str, metadata_csv: str,
                    site_name: str, survey_date: str, transect_number: str,
                    scale_factor: float, jpeg_quality: int,
                    dry_run: bool = False) -> int:
    """
    Main extraction loop.
    Returns the number of patches successfully written (or that would be written
    in dry-run mode).
    """
    df = pd.read_csv(annotation_csv)

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
            image_path = localize_path(str(row["Path"]))
            if not os.path.isfile(image_path):
                log.warning(f"  MISSING source image: {image_path}")
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

        image_path = localize_path(str(row["Path"]))
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
            "site_name":       site_name,
            "survey_date":     survey_date,
            "transect_number": transect_number,
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
    args = get_args_via_gui()

    log.info("CoralNet-Toolbox → Zooniverse patch extractor")
    log.info(f"  annotation CSV  : {args.annotation_csv}")
    log.info(f"  output dir   : {args.output_dir}")
    log.info(f"  metadata CSV : {args.metadata_csv}")
    log.info(f"  site name    : {args.site_name}")
    log.info(f"  survey date  : {args.survey_date}")
    log.info(f"  transect no. : {args.transect_number}")
    log.info(f"  scale factor : {args.scale}×")
    log.info(f"  JPEG quality : {args.jpeg_quality}")
    if args.dry_run:
        log.info("  mode         : DRY RUN")

    n = extract_patches(
        annotation_csv  = args.annotation_csv,
        output_dir   = args.output_dir,
        metadata_csv = args.metadata_csv,
        site_name    = args.site_name,
        survey_date  = args.survey_date,
        transect_number = args.transect_number,
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
