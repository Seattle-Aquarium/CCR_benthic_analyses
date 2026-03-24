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
    python import_subjects.py \
        --transect-id T-006 \
        --subject-set-name "WestLagoon_Jan2026" \
        --image-dir /path/to/images/T-006/

    # Upload into an existing subject set instead of creating a new one:
    python import_subjects.py \
        --transect-id T-006 \
        --subject-set-id 135054 \
        --image-dir /path/to/images/T-006/

    # Test with first 10 images only, no actual upload:
    python import_subjects.py \
        --transect-id T-006 \
        --subject-set-name "Test" \
        --image-dir /path/to/images/ \
        --dry-run --limit 10

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
import argparse
import logging
from pathlib import Path

import pandas as pd
from dotenv import load_dotenv
from tqdm import tqdm
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
# CLI
# ============================================================
def parse_args():
    p = argparse.ArgumentParser(
        description="Upload images + metadata to a Zooniverse subject set",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )

    # Required
    p.add_argument("--transect-id",  required=True,
                   help="Your internal transect ID, e.g. T-006")
    p.add_argument("--image-dir",    required=True,
                   help="Directory containing images AND metadata.csv")

    # Subject set — provide one of these two
    ss = p.add_mutually_exclusive_group(required=True)
    ss.add_argument("--subject-set-name", default=None,
                    help="Name for a NEW subject set to create")
    ss.add_argument("--subject-set-id",   default=None,
                    help="Existing subject set ID to add images into")

    # Optional
    p.add_argument("--filename-column",  default="filename",
                   help="Column name in metadata.csv that holds the image filename")
    p.add_argument("--upload-log",       default=None,
                   help="CSV to record successfully uploaded subjects "
                        "(default: <image-dir>/upload_log.csv)")
    p.add_argument("--fail-log",         default=None,
                   help="CSV to record skipped/failed subjects "
                        "(default: <image-dir>/fail_log.csv)")
    p.add_argument("--checkpoint-every", type=int, default=100,
                   help="Flush subjects to the set every N uploads")
    p.add_argument("--sleep",            type=float, default=0.1,
                   help="Seconds to sleep between subject saves (reduces API pressure)")
    p.add_argument("--limit",            type=int, default=None,
                   help="Only process the first N rows (useful for testing)")
    p.add_argument("--check-server-duplicates", action="store_true",
                   help="Also check Zooniverse server for duplicates (slow)")
    p.add_argument("--skip-missing",     action="store_true", default=True,
                   help="Skip rows where the image file is not found instead of crashing")
    p.add_argument("--dry-run",          action="store_true",
                   help="Validate inputs and list images without uploading anything")

    return p.parse_args()


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
    """Authenticate from .env; return project_id string."""
    load_dotenv(Path(__file__).parent / ".env")
    username   = os.environ["ZOONIVERSE_USERNAME"]
    password   = os.environ["ZOONIVERSE_PASSWORD"]
    project_id = os.environ["ZOONIVERSE_PROJECT_ID"]
    Panoptes.connect(username=username, password=password)
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
    args      = parse_args()
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
