"""
patch_subject_metadata.py  —  Backfill subject metadata on Zooniverse

Adds or overwrites transect_id, transect_number, site_name, and survey_date
on existing Zooniverse subjects. Designed for transects uploaded before these
fields were stamped at upload time.

Subject IDs can be sourced two ways:
  1. From Zooniverse directly — provide a Subject Set ID and the script
     fetches all subjects in that set via the API (recommended when you
     don't have a local upload_log.csv with subject IDs).
  2. From a local upload_log.csv — must have a subject_id column.

You can also supply a per-row metadata CSV to assign different values to
individual subjects (e.g. if a subject set spans multiple sites/dates).

Credentials:
    Same .env setup as import_subjects.py:
        ZOONIVERSE_USERNAME=...
        ZOONIVERSE_PASSWORD=...
        ZOONIVERSE_PROJECT_ID=...

Usage:
    python patch_subject_metadata.py

    A GUI form opens and asks for:
      1. Subject source: Subject Set ID  OR  upload_log.csv
      2. Transect ID, Transect Number, Site name, Survey date (applied to all subjects)
      3. Per-row metadata CSV (optional) — columns: subject_id + any patch fields
      4. Dry run checkbox — prints changes without saving
"""

import os
import sys
import time
import types
import logging
import tkinter as tk
from tkinter import ttk, filedialog, messagebox
from pathlib import Path

import pandas as pd
from dotenv import load_dotenv
from tqdm import tqdm
from panoptes_client import Panoptes, Subject, SubjectSet

# ── Logging ───────────────────────────────────────────────────────────────────
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[logging.StreamHandler(sys.stdout)],
)
log = logging.getLogger(__name__)

PATCH_FIELDS = ["transect_id", "transect_number", "site_name", "survey_date"]


# ============================================================
# GUI
# ============================================================
def get_args_via_gui():
    result = {}

    root = tk.Tk()
    root.title("Patch Subject Metadata")
    root.resizable(False, False)
    pad = {"padx": 10, "pady": 5}

    ttk.Label(root, text="Patch Zooniverse Subject Metadata",
              font=("Helvetica", 13, "bold")).grid(
        row=0, column=0, columnspan=3, pady=(14, 2), padx=14)
    ttk.Label(root,
              text="Backfills transect_id, transect_number, site_name, and survey_date on existing subjects.",
              foreground="grey").grid(row=1, column=0, columnspan=3, pady=(0, 4))
    ttk.Separator(root, orient="horizontal").grid(
        row=2, column=0, columnspan=3, sticky="ew", padx=10, pady=4)

    # ── Subject source ────────────────────────────────────────────────────────
    ttk.Label(root, text="Subject source:",
              font=("Helvetica", 10, "bold")).grid(
        row=3, column=0, columnspan=3, sticky="w", padx=14, pady=(4, 0))

    source_var = tk.StringVar(value="subjectset")
    source_frame = ttk.Frame(root)
    source_frame.grid(row=4, column=0, columnspan=3, sticky="w", padx=18, pady=2)

    ttk.Radiobutton(source_frame, text="Subject Set ID (fetch from Zooniverse)",
                    variable=source_var, value="subjectset").grid(
        row=0, column=0, sticky="w")
    ttk.Radiobutton(source_frame, text="Upload log CSV (local file)",
                    variable=source_var, value="uploadlog").grid(
        row=1, column=0, sticky="w")

    # Subject Set ID entry
    ss_frame = ttk.Frame(root)
    ss_frame.grid(row=5, column=0, columnspan=3, sticky="w", padx=14, pady=2)
    ttk.Label(ss_frame, text="Subject Set ID:").grid(row=0, column=0, sticky="e", padx=(0, 6))
    ss_id_var = tk.StringVar()
    ttk.Entry(ss_frame, textvariable=ss_id_var, width=20).grid(row=0, column=1, sticky="w")
    ttk.Label(ss_frame, text="(find this in Zooniverse Lab → Subject Sets)",
              foreground="grey").grid(row=0, column=2, sticky="w", padx=(8, 0))

    def _autofill_cache(*_):
        """Auto-populate cache path when Subject Set ID is typed, if field is empty."""
        ss_id = ss_id_var.get().strip()
        if ss_id and not cache_csv_var.get().strip():
            script_dir = Path(__file__).parent
            cache_csv_var.set(str(script_dir / f"subject_id_cache_{ss_id}.csv"))

    ss_id_var.trace_add("write", _autofill_cache)

    # Upload log entry
    ul_frame = ttk.Frame(root)
    ul_frame.grid(row=6, column=0, columnspan=3, sticky="w", padx=14, pady=2)
    ttk.Label(ul_frame, text="Upload log CSV:").grid(row=0, column=0, sticky="e", padx=(0, 6))
    upload_log_var = tk.StringVar()
    ttk.Entry(ul_frame, textvariable=upload_log_var, width=50).grid(row=0, column=1, sticky="w")

    def browse_upload_log():
        p = filedialog.askopenfilename(
            title="Select upload_log.csv",
            filetypes=[("CSV files", "*.csv"), ("All files", "*.*")])
        if p:
            upload_log_var.set(p)

    ttk.Button(ul_frame, text="Browse...", command=browse_upload_log).grid(
        row=0, column=2, padx=(6, 0))

    ttk.Separator(root, orient="horizontal").grid(
        row=7, column=0, columnspan=3, sticky="ew", padx=10, pady=6)

    # ── Fixed metadata values ─────────────────────────────────────────────────
    ttk.Label(root, text="Apply to all subjects:",
              font=("Helvetica", 10, "bold")).grid(
        row=8, column=0, columnspan=3, sticky="w", padx=14, pady=(2, 0))

    ttk.Label(root, text="Transect ID:").grid(row=9, column=0, sticky="e", **pad)
    transect_var = tk.StringVar()
    ttk.Entry(root, textvariable=transect_var, width=40).grid(
        row=9, column=1, sticky="w", **pad)

    ttk.Label(root, text="Transect number:").grid(row=10, column=0, sticky="e", **pad)
    transect_num_var = tk.StringVar()
    ttk.Entry(root, textvariable=transect_num_var, width=20).grid(
        row=10, column=1, sticky="w", **pad)

    ttk.Label(root, text="Site name:").grid(row=11, column=0, sticky="e", **pad)
    site_var = tk.StringVar()
    ttk.Entry(root, textvariable=site_var, width=40).grid(
        row=11, column=1, sticky="w", **pad)

    ttk.Label(root, text="Survey date:").grid(row=12, column=0, sticky="e", **pad)
    date_var = tk.StringVar()
    ttk.Entry(root, textvariable=date_var, width=20).grid(
        row=12, column=1, sticky="w", **pad)
    ttk.Label(root, text="(YYYY-MM-DD)", foreground="grey").grid(
        row=12, column=2, sticky="w")

    ttk.Separator(root, orient="horizontal").grid(
        row=13, column=0, columnspan=3, sticky="ew", padx=10, pady=4)

    # ── Per-row metadata CSV ──────────────────────────────────────────────────
    ttk.Label(root, text="Per-row metadata CSV (optional — overrides fields above):",
              font=("Helvetica", 10, "bold")).grid(
        row=14, column=0, columnspan=3, sticky="w", padx=14, pady=(4, 0))
    ttk.Label(root,
              text="Required columns: subject_id  +  any of: transect_id, transect_number, site_name, survey_date",
              foreground="grey").grid(row=15, column=0, columnspan=3, sticky="w", padx=14)

    meta_frame = ttk.Frame(root)
    meta_frame.grid(row=16, column=0, columnspan=3, sticky="w", padx=14, pady=2)
    meta_csv_var = tk.StringVar()
    ttk.Entry(meta_frame, textvariable=meta_csv_var, width=55).grid(
        row=0, column=0, sticky="w")

    def browse_meta_csv():
        p = filedialog.askopenfilename(
            title="Select per-row metadata CSV",
            filetypes=[("CSV files", "*.csv"), ("All files", "*.*")])
        if p:
            meta_csv_var.set(p)

    ttk.Button(meta_frame, text="Browse...", command=browse_meta_csv).grid(
        row=0, column=1, padx=(6, 0))

    ttk.Separator(root, orient="horizontal").grid(
        row=17, column=0, columnspan=3, sticky="ew", padx=10, pady=4)

    # ── Subject ID cache ──────────────────────────────────────────────────────
    ttk.Label(root, text="Subject ID cache (optional):",
              font=("Helvetica", 10, "bold")).grid(
        row=18, column=0, columnspan=3, sticky="w", padx=14, pady=(4, 0))
    ttk.Label(root,
              text="If the file exists: load IDs from it (skip Zooniverse fetch). "
                   "If it doesn't exist: fetch from Zooniverse and save here.",
              foreground="grey").grid(row=19, column=0, columnspan=3, sticky="w", padx=14)

    cache_frame = ttk.Frame(root)
    cache_frame.grid(row=20, column=0, columnspan=3, sticky="w", padx=14, pady=2)
    cache_csv_var = tk.StringVar()
    ttk.Entry(cache_frame, textvariable=cache_csv_var, width=55).grid(
        row=0, column=0, sticky="w")

    def browse_cache_csv():
        p = filedialog.asksaveasfilename(
            title="Save/load subject ID cache CSV",
            defaultextension=".csv",
            filetypes=[("CSV files", "*.csv"), ("All files", "*.*")])
        if p:
            cache_csv_var.set(p)

    ttk.Button(cache_frame, text="Browse...", command=browse_cache_csv).grid(
        row=0, column=1, padx=(6, 0))

    ttk.Separator(root, orient="horizontal").grid(
        row=21, column=0, columnspan=3, sticky="ew", padx=10, pady=4)

    # ── Options ───────────────────────────────────────────────────────────────
    dry_run_var = tk.BooleanVar(value=True)
    ttk.Checkbutton(root, text="Dry run (print changes only — nothing saved to Zooniverse)",
                    variable=dry_run_var).grid(
        row=22, column=0, columnspan=3, sticky="w", padx=18, pady=4)

    sleep_frame = ttk.Frame(root)
    sleep_frame.grid(row=23, column=0, columnspan=3, sticky="w", padx=18, pady=(0, 6))
    ttk.Label(sleep_frame, text="Sleep between saves (s):").pack(side="left")
    sleep_var = tk.StringVar(value="0.2")
    ttk.Entry(sleep_frame, textvariable=sleep_var, width=8).pack(side="left", padx=6)

    ttk.Separator(root, orient="horizontal").grid(
        row=24, column=0, columnspan=3, sticky="ew", padx=10, pady=6)

    btn_frame = ttk.Frame(root)
    btn_frame.grid(row=25, column=0, columnspan=3, pady=(0, 14))

    def on_run():
        source         = source_var.get()
        ss_id          = ss_id_var.get().strip()
        upload_log     = upload_log_var.get().strip()
        meta_csv       = meta_csv_var.get().strip()
        transect       = transect_var.get().strip()
        transect_num   = transect_num_var.get().strip()
        site           = site_var.get().strip()
        date           = date_var.get().strip()

        if source == "subjectset":
            if not ss_id:
                messagebox.showerror("Missing input", "Please enter a Subject Set ID.")
                return
        else:
            if not upload_log:
                messagebox.showerror("Missing input", "Please select an upload log CSV.")
                return
            if not Path(upload_log).is_file():
                messagebox.showerror("File not found", f"Upload log not found:\n{upload_log}")
                return

        if meta_csv and not Path(meta_csv).is_file():
            messagebox.showerror("File not found", f"Per-row metadata CSV not found:\n{meta_csv}")
            return

        if not meta_csv and not any([transect, transect_num, site, date]):
            messagebox.showerror(
                "Missing input",
                "Enter at least one of Transect ID / Transect Number / Site name / Survey date,\n"
                "or supply a per-row metadata CSV.")
            return

        try:
            sleep_s = float(sleep_var.get().strip())
            if sleep_s < 0:
                raise ValueError
        except ValueError:
            messagebox.showerror("Invalid input", "Sleep must be a number >= 0.")
            return

        result["source"]           = source
        result["ss_id"]            = ss_id or None
        result["upload_log"]       = upload_log or None
        result["meta_csv"]         = meta_csv or None
        result["cache_csv"]        = cache_csv_var.get().strip() or None
        result["transect_id"]      = transect or None
        result["transect_number"]  = transect_num or None
        result["site_name"]        = site or None
        result["survey_date"]      = date or None
        result["dry_run"]          = dry_run_var.get()
        result["sleep"]            = sleep_s
        result["submitted"]        = True
        root.destroy()

    def on_cancel():
        root.destroy()

    ttk.Button(btn_frame, text="  Run  ", command=on_run).pack(side="left", padx=8)
    ttk.Button(btn_frame, text="Cancel", command=on_cancel).pack(side="left", padx=8)

    root.mainloop()

    if not result.get("submitted"):
        print("Cancelled by user.")
        sys.exit(0)

    return types.SimpleNamespace(**result)


# ============================================================
# CREDENTIALS
# ============================================================
def connect_to_zooniverse():
    script_dir = Path(__file__).parent
    for env_path in [script_dir / ".env", script_dir / "config.env",
                     script_dir / "config.example.env", Path.cwd() / ".env"]:
        if env_path.exists():
            load_dotenv(env_path, override=False)

    username = os.environ.get("ZOONIVERSE_USERNAME", "").strip()
    password = os.environ.get("ZOONIVERSE_PASSWORD", "").strip()

    missing = [k for k, v in [("ZOONIVERSE_USERNAME", username),
                               ("ZOONIVERSE_PASSWORD", password)] if not v]
    if missing:
        raise ValueError(f"Missing credentials in .env: {', '.join(missing)}")

    max_attempts = 3
    for attempt in range(1, max_attempts + 1):
        try:
            if attempt > 1:
                log.info(f"Retrying connection ({attempt}/{max_attempts})…")
            Panoptes.connect(username=username, password=password)
            log.info(f"Authenticated as {username}")
            return
        except Exception as exc:
            log.warning(f"Connection attempt {attempt} failed: {exc}")
            if attempt < max_attempts:
                time.sleep(attempt * 2)

    raise ConnectionError(
        f"Could not connect to Zooniverse after {max_attempts} attempts. "
        "Check your credentials, network, and whether Zooniverse is reachable."
    )


# ============================================================
# SUBJECT ID COLLECTION
# ============================================================
def get_subject_ids_from_set(ss_id: str, cache_csv: str | None = None) -> list[str]:
    """
    Fetch all subject IDs from a Zooniverse subject set.
    If cache_csv is provided and the file exists, load from cache instead.
    If cache_csv is provided and the file does not exist, fetch and save to cache.
    """
    cache_path = Path(cache_csv) if cache_csv else None

    if cache_path and cache_path.exists():
        log.info(f"Loading subject IDs from cache: {cache_path}")
        df = pd.read_csv(cache_path, dtype=str)
        if "subject_id" not in df.columns:
            raise ValueError(f"Cache file must have a 'subject_id' column: {cache_path}")
        ids = df["subject_id"].dropna().str.strip().tolist()
        log.info(f"Loaded {len(ids)} subject IDs from cache.")
        return ids

    log.info(f"Fetching subjects from subject set {ss_id}…")
    ss = SubjectSet.find(ss_id)
    log.info(f"Subject set: '{ss.display_name}'")

    subject_ids = []
    for subject in tqdm(ss.subjects, desc="Fetching subject IDs", unit="subject"):
        subject_ids.append(str(subject.id))

    log.info(f"Found {len(subject_ids)} subjects in set.")

    if cache_path:
        pd.DataFrame({"subject_id": subject_ids}).to_csv(cache_path, index=False)
        log.info(f"Saved subject ID cache → {cache_path}")

    return subject_ids


def get_subject_ids_from_log(upload_log_path: str) -> list[str]:
    """Read subject IDs from a local upload_log.csv."""
    df = pd.read_csv(upload_log_path, dtype=str)
    if "subject_id" not in df.columns:
        raise ValueError("upload_log.csv must have a 'subject_id' column.")
    ids = df["subject_id"].dropna().str.strip().tolist()
    log.info(f"Loaded {len(ids)} subject IDs from upload log.")
    return ids


# ============================================================
# PATCH MAP
# ============================================================
def build_patch_map(subject_ids: list[str], args) -> dict[str, dict]:
    """
    Returns {subject_id: {field: value, ...}} for every subject to patch.
    Per-row metadata CSV overrides the fixed GUI values on a per-subject basis.
    Only fields with non-empty values are included.
    """
    fixed = {f: getattr(args, f) for f in PATCH_FIELDS if getattr(args, f, None)}

    per_row: dict[str, dict] = {}
    if args.meta_csv:
        meta = pd.read_csv(args.meta_csv, dtype=str)
        if "subject_id" not in meta.columns:
            raise ValueError("Per-row metadata CSV must have a 'subject_id' column.")
        present_fields = [f for f in PATCH_FIELDS if f in meta.columns]
        if not present_fields:
            raise ValueError(
                f"Per-row metadata CSV has no recognised patch columns. "
                f"Expected at least one of: {PATCH_FIELDS}")
        for _, row in meta.iterrows():
            sid = str(row["subject_id"]).strip()
            per_row[sid] = {f: str(row[f]).strip()
                            for f in present_fields
                            if pd.notna(row[f]) and str(row[f]).strip()}

    patch_map = {}
    for sid in subject_ids:
        updates = {**fixed, **per_row.get(sid, {})}
        if updates:
            patch_map[sid] = updates

    return patch_map


# ============================================================
# PATCH
# ============================================================
def run_patch(patch_map: dict[str, dict], dry_run: bool, sleep_s: float):
    patched = 0
    failed  = 0
    skipped = 0

    for sid, updates in tqdm(list(patch_map.items()),
                             desc="Patching subjects", unit="subject"):
        try:
            subject = Subject.find(sid)
            current = dict(subject.metadata)

            # Only update fields that are actually changing
            changes = {k: v for k, v in updates.items() if current.get(k) != v}
            if not changes:
                log.debug(f"[SKIP] {sid} — already up to date")
                skipped += 1
                continue

            log.info(f"[{'DRY RUN' if dry_run else 'PATCH'}] {sid}: {changes}")

            if not dry_run:
                for k, v in changes.items():
                    subject.metadata[k] = v
                subject.save()
                if sleep_s > 0:
                    time.sleep(sleep_s)

            patched += 1

        except Exception as e:
            log.error(f"[FAIL] subject {sid}: {e}")
            failed += 1

    return patched, skipped, failed


# ============================================================
# ENTRY POINT
# ============================================================
def main():
    args = get_args_via_gui()

    # Always connect — needed to fetch subjects or patch them
    connect_to_zooniverse()

    # Collect subject IDs
    if args.source == "subjectset":
        subject_ids = get_subject_ids_from_set(args.ss_id, cache_csv=args.cache_csv)
    else:
        subject_ids = get_subject_ids_from_log(args.upload_log)

    if not subject_ids:
        log.info("No subjects found. Exiting.")
        return

    log.info("Building patch map…")
    patch_map = build_patch_map(subject_ids, args)
    log.info(f"Subjects to patch: {len(patch_map)}")

    if not patch_map:
        log.info("Nothing to patch. Exiting.")
        return

    patched, skipped, failed = run_patch(patch_map, args.dry_run, args.sleep)

    log.info("=" * 60)
    log.info(f"{'DRY RUN — ' if args.dry_run else ''}Patch complete!")
    log.info(f"  Patched  : {patched}")
    log.info(f"  Skipped (already correct): {skipped}")
    log.info(f"  Failed   : {failed}")
    if args.dry_run:
        log.info("  Re-run without dry run to apply changes.")
    log.info("=" * 60)


if __name__ == "__main__":
    main()
