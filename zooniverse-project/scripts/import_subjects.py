"""
import_subjects.py  —  Upload images to a Zooniverse subject set
Version: 2.0

What's new in v2.0:
  - Reads metadata.csv from the image directory (all columns become subject metadata)
  - Logs every uploaded subject ID to a CSV so re-runs skip already-uploaded images
  - Checkpoints every N subjects so a crash doesn't lose progress
  - Logs failures separately so you can review and retry
  - Optional server-side duplicate check (slow; off by default)
  - .env credentials + argparse CLI (no hardcoded settings)

Usage:
    python import_subjects.py

    A window opens and asks for required fields:
      1. Transect ID
      2. Image directory (must contain images and optional metadata.csv)
      3. Subject set target (new name or existing ID)

Requirements:
    pip install panoptes-client python-dotenv tqdm pandas

Credentials:
    Copy config.example.env → .env and fill in your details:
        ZOONIVERSE_USERNAME=...
        ZOONIVERSE_PASSWORD=...
        ZOONIVERSE_PROJECT_ID=...
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
from requests.exceptions import RequestException
from panoptes_client import Panoptes, Project, SubjectSet, Subject

# ── Constants ────────────────────────────────────────────────────────────────
SUPPORTED_EXTENSIONS = {".jpg", ".jpeg", ".png", ".tif", ".tiff"}
UNIQUE_ID_FIELD      = "source_id"   # metadata key used to detect duplicates


# ── Logging ──────────────────────────────────────────────────────────────────
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[
        logging.FileHandler("import_log.txt"),
        logging.StreamHandler(sys.stdout),
    ],
)
log = logging.getLogger(__name__)


# ============================================================
# GUI INPUT FORM
# ============================================================
def get_args_via_gui():
    """Open a GUI form for required fields and return args namespace."""
    result = {}

    root = tk.Tk()
    root.title("Import Subjects to Zooniverse")
    root.resizable(False, False)

    pad = {"padx": 10, "pady": 5}

    ttk.Label(
        root,
        text="Zooniverse Subject Import",
        font=("Helvetica", 13, "bold"),
    ).grid(row=0, column=0, columnspan=3, pady=(14, 4), padx=14)
    ttk.Label(
        root,
        text="Enter required fields, then click Run.",
        foreground="grey",
    ).grid(row=1, column=0, columnspan=3, pady=(0, 4))
    ttk.Separator(root, orient="horizontal").grid(
        row=2, column=0, columnspan=3, sticky="ew", padx=10, pady=4
    )

    ttk.Label(root, text="Transect ID:").grid(row=3, column=0, sticky="e", **pad)
    transect_var = tk.StringVar()
    ttk.Entry(root, textvariable=transect_var, width=55).grid(row=3, column=1, **pad)

    ttk.Label(root, text="Image folder:").grid(row=4, column=0, sticky="e", **pad)
    image_dir_var = tk.StringVar()
    ttk.Entry(root, textvariable=image_dir_var, width=55).grid(row=4, column=1, **pad)

    def browse_image_dir():
        path = filedialog.askdirectory(
            title="Select folder containing images and optional metadata.csv"
        )
        if path:
            image_dir_var.set(path)

    ttk.Button(root, text="Browse...", command=browse_image_dir).grid(
        row=4, column=2, **pad
    )

    ttk.Separator(root, orient="horizontal").grid(
        row=5, column=0, columnspan=3, sticky="ew", padx=10, pady=4
    )

    ttk.Label(root, text="Subject set:").grid(row=6, column=0, sticky="ne", **pad)
    subject_mode_var = tk.StringVar(value="name")
    subject_frame = ttk.Frame(root)
    subject_frame.grid(row=6, column=1, sticky="w", **pad)

    ttk.Radiobutton(
        subject_frame,
        text="Create new",
        variable=subject_mode_var,
        value="name",
    ).grid(row=0, column=0, sticky="w")
    ttk.Radiobutton(
        subject_frame,
        text="Use existing ID",
        variable=subject_mode_var,
        value="id",
    ).grid(row=1, column=0, sticky="w")

    new_name_var = tk.StringVar()
    existing_id_var = tk.StringVar()

    ttk.Label(subject_frame, text="New subject set name:").grid(
        row=0, column=1, sticky="e", padx=(10, 6)
    )
    ttk.Entry(subject_frame, textvariable=new_name_var, width=32).grid(
        row=0, column=2, sticky="w"
    )

    ttk.Label(subject_frame, text="Existing subject set ID:").grid(
        row=1, column=1, sticky="e", padx=(10, 6)
    )
    ttk.Entry(subject_frame, textvariable=existing_id_var, width=32).grid(
        row=1, column=2, sticky="w"
    )

    dry_run_var = tk.BooleanVar(value=False)
    ttk.Checkbutton(
        root,
        text="Dry run (validate files only; no upload)",
        variable=dry_run_var,
    ).grid(row=7, column=0, columnspan=3, sticky="w", padx=18, pady=4)

    advanced_frame = ttk.LabelFrame(root, text="Advanced options (optional)")
    advanced_frame.grid(row=8, column=0, columnspan=3, sticky="ew", padx=10, pady=(2, 6))

    ttk.Label(advanced_frame, text="Filename column:").grid(
        row=0, column=0, sticky="e", padx=10, pady=4
    )
    filename_column_var = tk.StringVar(value="filename")
    ttk.Entry(advanced_frame, textvariable=filename_column_var, width=20).grid(
        row=0, column=1, sticky="w", padx=(0, 12), pady=4
    )

    ttk.Label(advanced_frame, text="Checkpoint every:").grid(
        row=0, column=2, sticky="e", padx=(0, 6), pady=4
    )
    checkpoint_every_var = tk.StringVar(value="100")
    ttk.Entry(advanced_frame, textvariable=checkpoint_every_var, width=10).grid(
        row=0, column=3, sticky="w", pady=4
    )
    ttk.Label(advanced_frame, text="rows", foreground="grey").grid(
        row=0, column=4, sticky="w", padx=(6, 10), pady=4
    )

    ttk.Label(advanced_frame, text="Sleep between uploads:").grid(
        row=1, column=0, sticky="e", padx=10, pady=4
    )
    sleep_var = tk.StringVar(value="0.1")
    ttk.Entry(advanced_frame, textvariable=sleep_var, width=10).grid(
        row=1, column=1, sticky="w", pady=4
    )
    ttk.Label(advanced_frame, text="seconds", foreground="grey").grid(
        row=1, column=2, sticky="w", padx=(6, 10), pady=4
    )

    ttk.Label(advanced_frame, text="Limit rows:").grid(
        row=1, column=3, sticky="e", padx=(0, 6), pady=4
    )
    limit_var = tk.StringVar(value="")
    ttk.Entry(advanced_frame, textvariable=limit_var, width=10).grid(
        row=1, column=4, sticky="w", pady=4
    )

    check_server_dupes_var = tk.BooleanVar(value=False)
    skip_missing_var = tk.BooleanVar(value=True)
    ttk.Checkbutton(
        advanced_frame,
        text="Check server for duplicates (slow)",
        variable=check_server_dupes_var,
    ).grid(row=2, column=0, columnspan=3, sticky="w", padx=10, pady=(2, 6))
    ttk.Checkbutton(
        advanced_frame,
        text="Skip missing files",
        variable=skip_missing_var,
    ).grid(row=2, column=3, columnspan=2, sticky="w", padx=10, pady=(2, 6))

    ttk.Separator(root, orient="horizontal").grid(
        row=9, column=0, columnspan=3, sticky="ew", padx=10, pady=6
    )

    btn_frame = ttk.Frame(root)
    btn_frame.grid(row=10, column=0, columnspan=3, pady=(0, 14))

    def on_run():
        transect_id = transect_var.get().strip()
        image_dir = image_dir_var.get().strip()
        mode = subject_mode_var.get()
        subject_set_name = new_name_var.get().strip()
        subject_set_id = existing_id_var.get().strip()
        filename_column = filename_column_var.get().strip()

        if not transect_id:
            messagebox.showerror("Missing input", "Please enter a transect ID.")
            return

        if not image_dir:
            messagebox.showerror("Missing input", "Please select an image folder.")
            return
        if not Path(image_dir).is_dir():
            messagebox.showerror("Folder not found", f"Image folder not found:\n{image_dir}")
            return

        if mode == "name":
            if not subject_set_name:
                messagebox.showerror(
                    "Missing input",
                    "Enter a new subject set name or switch to existing ID.",
                )
                return
            subject_set_id = None
        else:
            if not subject_set_id:
                messagebox.showerror(
                    "Missing input",
                    "Enter an existing subject set ID or switch to new name.",
                )
                return
            subject_set_name = None

        if not filename_column:
            messagebox.showerror("Invalid input", "Filename column cannot be empty.")
            return

        try:
            checkpoint_every = int(checkpoint_every_var.get().strip())
            if checkpoint_every < 1:
                raise ValueError
        except ValueError:
            messagebox.showerror(
                "Invalid input",
                "Checkpoint every must be a whole number >= 1.",
            )
            return

        try:
            sleep_seconds = float(sleep_var.get().strip())
            if sleep_seconds < 0:
                raise ValueError
        except ValueError:
            messagebox.showerror(
                "Invalid input",
                "Sleep must be a number >= 0 (for example 0.1).",
            )
            return

        limit_text = limit_var.get().strip()
        if limit_text:
            try:
                limit_value = int(limit_text)
                if limit_value < 1:
                    raise ValueError
            except ValueError:
                messagebox.showerror(
                    "Invalid input",
                    "Limit rows must be blank or a whole number >= 1.",
                )
                return
        else:
            limit_value = None

        result["transect_id"] = transect_id
        result["image_dir"] = image_dir
        result["subject_set_name"] = subject_set_name
        result["subject_set_id"] = subject_set_id
        result["filename_column"] = filename_column
        result["checkpoint_every"] = checkpoint_every
        result["sleep"] = sleep_seconds
        result["limit"] = limit_value
        result["check_server_duplicates"] = check_server_dupes_var.get()
        result["skip_missing"] = skip_missing_var.get()
        result["dry_run"] = dry_run_var.get()
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

    return types.SimpleNamespace(
        transect_id=result["transect_id"],
        image_dir=result["image_dir"],
        subject_set_name=result["subject_set_name"],
        subject_set_id=result["subject_set_id"],
        filename_column=result["filename_column"],
        upload_log=None,
        fail_log=None,
        checkpoint_every=result["checkpoint_every"],
        sleep=result["sleep"],
        limit=result["limit"],
        check_server_duplicates=result["check_server_duplicates"],
        skip_missing=result["skip_missing"],
        dry_run=result["dry_run"],
    )


# ============================================================
# METADATA HELPERS
# ============================================================
def load_metadata(image_dir: Path, filename_column: str) -> pd.DataFrame | None:
    """Load metadata.csv from the image directory."""
    csv_path = image_dir / "metadata.csv"
    if not csv_path.exists():
        log.warning(
            f"No metadata.csv found in {image_dir} — "
            "filenames only will be used as metadata."
        )
        return None

    df = pd.read_csv(csv_path)
    if df.empty:
        raise ValueError(f"metadata.csv is empty: {csv_path}")
    if filename_column not in df.columns:
        raise ValueError(
            f"Filename column '{filename_column}' not found in metadata.csv.\n"
            f"Available columns: {list(df.columns)}"
        )
    log.info(f"Loaded metadata.csv — {len(df)} rows, columns: {list(df.columns)}")
    return df


def clean_value(value) -> str:
    """Convert any pandas value to a metadata-safe string."""
    if pd.isna(value):
        return ""
    if hasattr(value, "item"):
        try:
            return value.item()
        except Exception:
            pass
    return str(value)


# ============================================================
# UPLOAD LOG HELPERS
# ============================================================
def load_upload_log(log_path: Path) -> set:
    """Return set of source_ids already successfully uploaded."""
    if not log_path.exists():
        return set()
    try:
        df = pd.read_csv(log_path, dtype=str)
        if UNIQUE_ID_FIELD not in df.columns:
            return set()
        ids = set(df[UNIQUE_ID_FIELD].dropna().str.strip())
        log.info(
            f"Upload log found — {len(ids)} subjects already uploaded, "
            "these will be skipped."
        )
        return ids
    except Exception as e:
        log.warning(f"Could not read upload log ({log_path}): {e}")
        return set()


def append_rows(csv_path: Path, rows: list[dict]) -> None:
    """Append rows to a CSV, writing the header only if the file is new."""
    if not rows:
        return
    df = pd.DataFrame(rows)
    write_header = not csv_path.exists()
    df.to_csv(csv_path, mode="a", header=write_header, index=False)


# ============================================================
# ZOONIVERSE HELPERS
# ============================================================
def connect_to_zooniverse() -> str:
    """Authenticate from env file(s); return project_id string."""
    script_dir = Path(__file__).parent
    env_candidates = [
        script_dir / ".env",
        script_dir / "config.env",
        script_dir / "config.example.env",
        Path.cwd() / ".env",
    ]

    loaded_paths = []
    for env_path in env_candidates:
        if env_path.exists():
            load_dotenv(env_path, override=False)
            loaded_paths.append(str(env_path))

    if loaded_paths:
        log.info("Loaded env settings from: " + "; ".join(loaded_paths))

    username = os.environ.get("ZOONIVERSE_USERNAME", "").strip()
    password = os.environ.get("ZOONIVERSE_PASSWORD", "").strip()
    project_id = os.environ.get("ZOONIVERSE_PROJECT_ID", "").strip()

    missing = []
    if not username:
        missing.append("ZOONIVERSE_USERNAME")
    if not password:
        missing.append("ZOONIVERSE_PASSWORD")
    if not project_id:
        missing.append("ZOONIVERSE_PROJECT_ID")
    if missing:
        checked = "\n  - ".join(str(p) for p in env_candidates)
        raise ValueError(
            "Missing required Zooniverse credential(s): "
            + ", ".join(missing)
            + "\nChecked env files:\n  - "
            + checked
            + "\nFix: add these keys to scripts/.env (recommended) or scripts/config.example.env."
        )

    max_attempts = 3
    last_error = None
    for attempt in range(1, max_attempts + 1):
        try:
            if attempt > 1:
                log.info(f"Retrying Zooniverse connection ({attempt}/{max_attempts})...")
            Panoptes.connect(username=username, password=password)
            break
        except RequestException as exc:
            last_error = exc
            if attempt == max_attempts:
                raise ConnectionError(
                    "Could not connect to Zooniverse after 3 attempts. "
                    "The server closed the network connection during login. "
                    "Common causes are a temporary Zooniverse outage, a corporate proxy/firewall, "
                    "VPN filtering, or SSL inspection on the network. "
                    "Try again in a few minutes, or test from a different network/browser."
                ) from exc
            time.sleep(attempt * 2)
        except Exception as exc:
            last_error = exc
            raise

    if last_error and not getattr(Panoptes._local, "panoptes_client", None):
        raise last_error

    log.info(f"Authenticated as {username} (project {project_id})")
    return project_id


def get_or_create_subject_set(project_id: str,
                               name: str | None,
                               existing_id: str | None) -> SubjectSet:
    """Fetch an existing subject set or create a new one."""
    project = Project.find(project_id)
    if existing_id:
        ss = SubjectSet.find(existing_id)
        log.info(f"Using existing subject set '{ss.display_name}' (ID: {ss.id})")
    else:
        ss = SubjectSet()
        ss.links.project = project
        ss.display_name  = name
        ss.save()
        log.info(f"Created subject set '{name}' — ID: {ss.id}")
        log.info("👉  Record this Subject Set ID in tracker.xlsx!")
    return ss


def server_subject_exists(project_id: str, source_id: str) -> bool:
    """
    Check Zooniverse server for an existing subject with this source_id.
    This is slow (iterates all subjects) — only enable with --check-server-duplicates.
    """
    try:
        for subj in Subject.where(project_id=project_id):
            meta = getattr(subj, "metadata", {}) or {}
            if str(meta.get(UNIQUE_ID_FIELD, "")).strip() == source_id:
                return True
        return False
    except Exception as e:
        log.warning(f"Server duplicate check failed for {source_id}: {e}")
        return False


# ============================================================
# CHECKPOINT
# ============================================================
def flush_checkpoint(subject_set: SubjectSet,
                     pending_subjects: list,
                     upload_rows: list,
                     fail_rows: list,
                     upload_log: Path,
                     fail_log: Path) -> None:
    """
    Add all pending subjects to the subject set and write both log files.
    Called every --checkpoint-every uploads AND once at the very end.
    Clears all three buffers after writing.
    """
    if pending_subjects:
        log.info(f"[CHECKPOINT] Adding {len(pending_subjects)} subjects to subject set…")
        subject_set.add(pending_subjects)

    if upload_rows:
        log.info(f"[CHECKPOINT] Writing {len(upload_rows)} rows to upload log…")
        append_rows(upload_log, upload_rows)

    if fail_rows:
        log.info(f"[CHECKPOINT] Writing {len(fail_rows)} rows to fail log…")
        append_rows(fail_log, fail_rows)

    pending_subjects.clear()
    upload_rows.clear()
    fail_rows.clear()


# ============================================================
# MAIN UPLOAD LOOP
# ============================================================
def upload_images(image_dir: Path,
                  df: pd.DataFrame | None,
                  filename_column: str,
                  metadata_columns: list[str],
                  subject_set: SubjectSet,
                  project: Project,
                  already_uploaded: set,
                  args) -> tuple[int, int]:
    """
    Iterate over images (guided by metadata.csv if present), upload each
    subject, and checkpoint progress regularly.
    Returns (total_uploaded, total_skipped_or_failed).
    """
    upload_log = Path(args.upload_log)
    fail_log   = Path(args.fail_log)

    pending_subjects: list  = []
    upload_rows: list[dict] = []
    fail_rows: list[dict]   = []
    total_uploaded  = 0
    total_failed    = 0

    # Build the iteration list from metadata rows or raw image files
    if df is not None:
        rows = list(df.itertuples(index=True))
        if args.limit:
            rows = rows[:args.limit]
        iterator = tqdm(rows, desc="Uploading", unit="subject")
    else:
        images = sorted(
            f for f in image_dir.iterdir()
            if f.is_file() and f.suffix.lower() in SUPPORTED_EXTENSIONS
        )
        if args.limit:
            images = images[:args.limit]
        iterator = tqdm(images, desc="Uploading", unit="image")

    try:
        for loop_idx, item in enumerate(iterator, start=1):

            # ── Resolve filename & source_id ──────────────────────────────
            if df is not None:
                filename   = str(getattr(item, filename_column)).strip()
                source_id  = filename
                image_path = image_dir / filename
                row_meta   = {col: clean_value(getattr(item, col))
                              for col in metadata_columns}
            else:
                image_path = item          # item is a Path object
                filename   = item.name
                source_id  = filename
                row_meta   = {}

            # ── Duplicate checks ──────────────────────────────────────────
            if source_id in already_uploaded:
                log.info(f"[SKIP] Already in upload log: {source_id}")
                fail_rows.append({
                    "filename":       filename,
                    UNIQUE_ID_FIELD:  source_id,
                    "reason":         "Already in upload log",
                })
                total_failed += 1
                continue

            if args.check_server_duplicates and \
               server_subject_exists(str(project.id), source_id):
                log.info(f"[SKIP] Already on Zooniverse server: {source_id}")
                fail_rows.append({
                    "filename":       filename,
                    UNIQUE_ID_FIELD:  source_id,
                    "reason":         "Already exists on Zooniverse server",
                })
                total_failed += 1
                continue

            # ── File existence ────────────────────────────────────────────
            if not image_path.exists():
                msg = f"Image file not found: {image_path}"
                if args.skip_missing:
                    log.warning(f"[SKIP] {msg}")
                    fail_rows.append({
                        "filename":      filename,
                        UNIQUE_ID_FIELD: source_id,
                        "reason":        msg,
                    })
                    total_failed += 1
                    continue
                raise FileNotFoundError(msg)

            # ── Create & save subject ─────────────────────────────────────
            try:
                subject = Subject()
                subject.links.project = project
                subject.add_location(str(image_path))

                # All metadata.csv columns
                for col, val in row_meta.items():
                    subject.metadata[col] = val

                # Always stamp these fields
                subject.metadata[UNIQUE_ID_FIELD] = source_id
                subject.metadata["transect_id"]   = args.transect_id
                subject.metadata["filename"]       = filename

                subject.save()

                # Write the Zooniverse subject ID back into the subject metadata
                subject.metadata["subject_id"] = str(subject.id)
                subject.save()

                pending_subjects.append(subject)
                already_uploaded.add(source_id)

                upload_rows.append({
                    "subject_id":     subject.id,
                    "filename":       filename,
                    UNIQUE_ID_FIELD:  source_id,
                    "transect_id":    args.transect_id,
                    "subject_set_id": subject_set.id,
                    "uploaded_at":    pd.Timestamp.now().isoformat(),
                })

                total_uploaded += 1
                log.debug(f"[OK] {filename} → subject {subject.id}")

                if args.sleep > 0:
                    time.sleep(args.sleep)

            except Exception as e:
                log.error(f"[FAIL] {filename}: {e}")
                fail_rows.append({
                    "filename":      filename,
                    UNIQUE_ID_FIELD: source_id,
                    "reason":        str(e),
                })
                total_failed += 1

            # ── Checkpoint every N rows ───────────────────────────────────
            if loop_idx % args.checkpoint_every == 0:
                log.info(f"\n[CHECKPOINT] Processed {loop_idx} rows. Flushing…\n")
                flush_checkpoint(subject_set, pending_subjects, upload_rows,
                                 fail_rows, upload_log, fail_log)

        # Final flush after loop ends
        log.info("\n[FINAL CHECKPOINT] Flushing remaining subjects…\n")
        flush_checkpoint(subject_set, pending_subjects, upload_rows,
                         fail_rows, upload_log, fail_log)

    except KeyboardInterrupt:
        log.warning("\n[INTERRUPTED] Ctrl+C — saving progress before exit…")
        flush_checkpoint(subject_set, pending_subjects, upload_rows,
                         fail_rows, upload_log, fail_log)
        raise

    except Exception as e:
        log.error(f"\n[FATAL] {e} — saving progress before exit…")
        flush_checkpoint(subject_set, pending_subjects, upload_rows,
                         fail_rows, upload_log, fail_log)
        raise

    return total_uploaded, total_failed


# ============================================================
# ENTRY POINT
# ============================================================
def main():
    args      = get_args_via_gui()
    image_dir = Path(args.image_dir)

    if not image_dir.is_dir():
        log.error(f"Image directory not found: {image_dir}")
        sys.exit(1)

    # Default log paths live inside the image directory
    if args.upload_log is None:
        args.upload_log = str(image_dir / "upload_log.csv")
    if args.fail_log is None:
        args.fail_log = str(image_dir / "fail_log.csv")

    log.info(f"Upload log : {args.upload_log}")
    log.info(f"Fail log   : {args.fail_log}")

    # Load metadata.csv (optional — falls back to filename-only metadata)
    df = load_metadata(image_dir, args.filename_column)
    metadata_columns = list(df.columns) if df is not None else []

    # ── Dry run ───────────────────────────────────────────────────────────────
    if args.dry_run:
        log.info("DRY RUN — nothing will be uploaded.")
        if df is not None:
            items = list(df[args.filename_column].astype(str))
        else:
            items = [
                f.name for f in sorted(image_dir.iterdir())
                if f.suffix.lower() in SUPPORTED_EXTENSIONS
            ]
        if args.limit:
            items = items[:args.limit]
        for name in items:
            exists = "✓" if (image_dir / name).exists() else "✗ MISSING"
            print(f"  {exists}  {name}")
        log.info(f"Total: {len(items)} images would be uploaded.")
        return

    # ── Connect & resolve subject set ─────────────────────────────────────────
    project_id  = connect_to_zooniverse()
    project     = Project.find(project_id)
    subject_set = get_or_create_subject_set(
        project_id, args.subject_set_name, args.subject_set_id
    )

    # Load already-uploaded IDs from the local log
    already_uploaded = load_upload_log(Path(args.upload_log))

    # ── Upload ────────────────────────────────────────────────────────────────
    total_uploaded, total_failed = upload_images(
        image_dir, df, args.filename_column, metadata_columns,
        subject_set, project, already_uploaded, args,
    )

    # ── Summary ───────────────────────────────────────────────────────────────
    log.info("=" * 60)
    log.info("✅  Upload complete!")
    log.info(f"    Transect:        {args.transect_id}")
    log.info(f"    Subject Set:     {subject_set.display_name}")
    log.info(f"    Subject Set ID:  {subject_set.id}  ← record in tracker.xlsx")
    log.info(f"    Uploaded:        {total_uploaded}")
    log.info(f"    Skipped/failed:  {total_failed}")
    log.info(f"    Upload log:      {args.upload_log}")
    log.info(f"    Fail log:        {args.fail_log}")
    log.info("=" * 60)


if __name__ == "__main__":
    main()
