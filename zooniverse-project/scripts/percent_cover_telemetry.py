"""
percent_cover_telemetry.py — Toolbox annotations → percent cover → telemetry join
Version: 1.0

Combines two steps into one GUI tool:
  1. Percent cover : groups a CoralNet-Toolbox annotation.csv by image (Name),
                      counts points per Label, and divides by that image's
                      own total point count (robust to images that don't
                      have exactly 50 points).
  2. Telemetry join: parses the YYYY_MM_DD_HH-MM-SS timestamp out of each
                      image name and left-joins the matching dive telemetry
                      row (matched on Date + Time) onto the percent-cover
                      table. Every percent-cover row is kept even if no
                      telemetry match is found.

Usage:
    python percent_cover_telemetry.py

    A window opens to select:
      1. Toolbox annotation.csv (required)
      2. Telemetry CSV, with Date + Time columns (required)
      3. Output folder

Requirements:
    pip install pandas
"""

import json
import logging
import re
import sys
import tkinter as tk
from tkinter import ttk, filedialog, messagebox
from pathlib import Path

import pandas as pd

# ── Logging ──────────────────────────────────────────────────────────────────
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[logging.StreamHandler(sys.stdout)],
)
log = logging.getLogger(__name__)

NAME_TIMESTAMP_RE = re.compile(
    r"(\d{4})_(\d{2})_(\d{2})_(\d{2})-(\d{2})-(\d{2})"
)

# Path for persisting GUI state between runs
_CONFIG_PATH = Path(__file__).parent / ".percent_cover_telemetry_config.json"


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
# GUI
# ============================================================
def get_args_via_gui():
    result = {}
    cfg = _load_gui_config()

    root = tk.Tk()
    root.title("Percent Cover + Telemetry Join")
    root.resizable(False, False)
    pad = {"padx": 10, "pady": 5}

    ttk.Label(root, text="Percent Cover + Telemetry Join",
              font=("Helvetica", 13, "bold")).grid(
        row=0, column=0, columnspan=3, pady=(14, 2), padx=14)
    ttk.Label(root,
              text="Select files, then click Run.",
              foreground="grey").grid(row=1, column=0, columnspan=3, pady=(0, 4))
    ttk.Separator(root, orient="horizontal").grid(
        row=2, column=0, columnspan=3, sticky="ew", padx=10, pady=4)

    def make_file_row(row, label, filetypes, is_dir=False):
        ttk.Label(root, text=label).grid(row=row, column=0, sticky="e", **pad)
        var = tk.StringVar()
        ttk.Entry(root, textvariable=var, width=55).grid(row=row, column=1, **pad)

        def browse():
            if is_dir:
                p = filedialog.askdirectory(title=f"Select {label}")
            else:
                p = filedialog.askopenfilename(title=f"Select {label}", filetypes=filetypes)
            if p:
                var.set(p)

        ttk.Button(root, text="Browse...", command=browse).grid(row=row, column=2, **pad)
        return var

    annotation_var = make_file_row(3, "Toolbox annotation.csv:",
                                   [("CSV files", "*.csv"), ("All files", "*.*")])
    telemetry_var  = make_file_row(4, "Telemetry CSV:",
                                   [("CSV files", "*.csv"), ("All files", "*.*")])

    ttk.Label(root, text="Output CSV file:").grid(row=5, column=0, sticky="e", **pad)
    output_var = tk.StringVar()
    ttk.Entry(root, textvariable=output_var, width=55).grid(row=5, column=1, **pad)

    def browse_output():
        existing = output_var.get().strip()
        initial_dir = str(Path(existing).parent) if existing else str(Path.home())
        if existing:
            initial_file = Path(existing).name
        elif annotation_var.get().strip():
            initial_file = Path(annotation_var.get().strip()).stem + "_percent_cover_telemetry.csv"
        else:
            initial_file = "percent_cover_telemetry.csv"
        p = filedialog.asksaveasfilename(
            title="Save output CSV as",
            defaultextension=".csv",
            filetypes=[("CSV files", "*.csv"), ("All files", "*.*")],
            initialdir=initial_dir,
            initialfile=initial_file,
        )
        if p:
            output_var.set(p)

    ttk.Button(root, text="Browse...", command=browse_output).grid(row=5, column=2, **pad)

    # Restore last-used paths
    annotation_var.set(cfg.get("annotation", ""))
    telemetry_var.set(cfg.get("telemetry", ""))
    output_var.set(cfg.get("output", ""))

    ttk.Separator(root, orient="horizontal").grid(
        row=6, column=0, columnspan=3, sticky="ew", padx=10, pady=6)

    btn_frame = ttk.Frame(root)
    btn_frame.grid(row=7, column=0, columnspan=3, pady=(0, 14))

    def on_run():
        annotation = annotation_var.get().strip()
        telemetry  = telemetry_var.get().strip()
        output     = output_var.get().strip()

        if not annotation:
            messagebox.showerror("Missing input", "Please select the Toolbox annotation.csv.")
            return
        if not telemetry:
            messagebox.showerror("Missing input", "Please select the telemetry CSV.")
            return
        if not output:
            messagebox.showerror("Missing input", "Please name the output CSV file.")
            return
        for p, nm in [(annotation, "Toolbox annotation.csv"), (telemetry, "telemetry CSV")]:
            if not Path(p).is_file():
                messagebox.showerror("File not found", f"{nm} not found:\n{p}")
                return

        result["annotation"] = annotation
        result["telemetry"]  = telemetry
        result["output"]     = output
        result["submitted"]  = True
        _save_gui_config({
            "annotation": annotation,
            "telemetry":  telemetry,
            "output":     output,
        })
        root.destroy()

    def on_cancel():
        root.destroy()

    ttk.Button(btn_frame, text="  Run  ", command=on_run).pack(side="left", padx=8)
    ttk.Button(btn_frame, text="Cancel", command=on_cancel).pack(side="left", padx=8)

    root.mainloop()

    if not result.get("submitted"):
        print("Cancelled.")
        sys.exit(0)

    return result


# ============================================================
# STEP 1: Percent cover
# ============================================================
def annotations_to_percent_cover(df: pd.DataFrame) -> pd.DataFrame:
    required_cols = {"Name", "Label"}
    missing = required_cols - set(df.columns)
    if missing:
        raise ValueError(f"Toolbox annotation.csv is missing expected column(s): {missing}")

    # Count points per (Name, Label)
    counts = df.groupby(["Name", "Label"]).size().unstack(fill_value=0)

    # Divide by each image's actual total point count (robust to images
    # that don't have exactly 50 points)
    totals = df.groupby("Name").size()
    percent_cover = counts.div(totals, axis=0)

    return percent_cover.reset_index()


# ============================================================
# STEP 2: Telemetry join
# ============================================================
def extract_timestamp(name: str) -> pd.Timestamp:
    match = NAME_TIMESTAMP_RE.search(str(name))
    if not match:
        return pd.NaT
    year, month, day, hour, minute, second = match.groups()
    try:
        return pd.Timestamp(
            year=int(year), month=int(month), day=int(day),
            hour=int(hour), minute=int(minute), second=int(second),
        )
    except ValueError:
        return pd.NaT


def join_with_telemetry(percent_cover: pd.DataFrame, telemetry: pd.DataFrame) -> tuple[pd.DataFrame, dict]:
    required_telemetry_cols = {"Date", "Time"}
    missing = required_telemetry_cols - set(telemetry.columns)
    if missing:
        raise ValueError(f"Telemetry CSV is missing expected column(s): {missing}")

    percent_cover_label_cols = [c for c in percent_cover.columns if c != "Name"]
    telemetry_cols = list(telemetry.columns)

    percent_cover = percent_cover.copy()
    telemetry = telemetry.copy()

    percent_cover["timestamp"] = percent_cover["Name"].apply(extract_timestamp)
    telemetry["timestamp"] = pd.to_datetime(
        telemetry["Date"].astype(str) + " " + telemetry["Time"].astype(str)
    )

    n_images = len(percent_cover)
    n_unparsed = int(percent_cover["timestamp"].isna().sum())

    n_telemetry_rows = len(telemetry)
    telemetry_dedup = telemetry.drop_duplicates(subset="timestamp")
    n_duplicate_telemetry = n_telemetry_rows - len(telemetry_dedup)

    merged = percent_cover.merge(telemetry_dedup, on="timestamp", how="left")
    n_unmatched = int(merged["Date"].isna().sum())
    n_matched = n_images - n_unmatched

    merged = merged.drop(columns="timestamp")

    # Column order: image name, then telemetry columns, then percent-cover labels
    merged = merged[["Name"] + telemetry_cols + percent_cover_label_cols]

    stats = {
        "n_images": n_images,
        "n_unparsed_names": n_unparsed,
        "n_telemetry_rows": n_telemetry_rows,
        "n_duplicate_telemetry_dropped": n_duplicate_telemetry,
        "n_matched": n_matched,
        "n_unmatched": n_unmatched,
    }
    return merged, stats


# ============================================================
# ENTRY POINT
# ============================================================
def main():
    args = get_args_via_gui()

    annotation_path = Path(args["annotation"])
    telemetry_path  = Path(args["telemetry"])
    out_path        = Path(args["output"])
    if out_path.suffix.lower() != ".csv":
        out_path = out_path.with_suffix(".csv")

    log.info(f"Loading Toolbox annotations from {annotation_path}…")
    annotation_df = pd.read_csv(annotation_path)
    log.info(f"Loaded {len(annotation_df):,} annotation points")

    try:
        percent_cover = annotations_to_percent_cover(annotation_df)
    except ValueError as e:
        log.error(str(e))
        messagebox.showerror("Invalid annotation CSV", str(e))
        sys.exit(1)
    log.info(f"Built percent cover for {len(percent_cover):,} images, "
             f"{len(percent_cover.columns) - 1} label columns")

    log.info(f"Loading telemetry from {telemetry_path}…")
    telemetry_df = pd.read_csv(telemetry_path)
    log.info(f"Loaded {len(telemetry_df):,} telemetry rows")

    try:
        merged, stats = join_with_telemetry(percent_cover, telemetry_df)
    except ValueError as e:
        log.error(str(e))
        messagebox.showerror("Invalid telemetry CSV", str(e))
        sys.exit(1)

    # ── Warnings ──────────────────────────────────────────────────────────
    if stats["n_unparsed_names"]:
        log.warning(
            f"{stats['n_unparsed_names']} of {stats['n_images']} image name(s) did not "
            "match the expected 'YYYY_MM_DD_HH-MM-SS' timestamp pattern and "
            "will have no telemetry data."
        )
    if stats["n_duplicate_telemetry_dropped"]:
        log.warning(
            f"{stats['n_duplicate_telemetry_dropped']} telemetry row(s) shared a "
            "timestamp with another row and were dropped (kept first occurrence)."
        )
    if stats["n_unmatched"]:
        log.warning(
            f"{stats['n_unmatched']} of {stats['n_images']} image(s) had no matching "
            "telemetry row."
        )

    if stats["n_matched"] == 0:
        log.error("No image timestamps matched any telemetry row.")
        messagebox.showwarning(
            "No matches found",
            "None of the image timestamps matched a telemetry row.\n\n"
            "Double-check that the annotation.csv and telemetry CSV are from "
            "the same dive/transect and date."
        )

    # ── Write output ──────────────────────────────────────────────────────
    out_path.parent.mkdir(parents=True, exist_ok=True)
    merged.to_csv(out_path, index=False)

    # ── Summary ───────────────────────────────────────────────────────────
    log.info("=" * 60)
    log.info("Done!")
    log.info(f"    Images                 : {stats['n_images']:,}")
    log.info(f"    Matched to telemetry   : {stats['n_matched']:,}")
    log.info(f"    Unmatched              : {stats['n_unmatched']:,}")
    log.info(f"    Unparsed image names   : {stats['n_unparsed_names']:,}")
    log.info(f"    Duplicate telemetry dropped : {stats['n_duplicate_telemetry_dropped']:,}")
    log.info(f"    Output                 : {out_path}")
    log.info("=" * 60)

    summary_lines = [
        f"Images: {stats['n_images']:,}",
        f"Matched to telemetry: {stats['n_matched']:,}",
        f"Unmatched: {stats['n_unmatched']:,}",
        f"Unparsed image names: {stats['n_unparsed_names']:,}",
        f"Duplicate telemetry rows dropped: {stats['n_duplicate_telemetry_dropped']:,}",
        "",
        f"Saved to:\n{out_path}",
    ]
    if stats["n_unmatched"] or stats["n_unparsed_names"]:
        messagebox.showwarning("Percent cover + telemetry join complete", "\n".join(summary_lines))
    else:
        messagebox.showinfo("Percent cover + telemetry join complete", "\n".join(summary_lines))


if __name__ == "__main__":
    main()
