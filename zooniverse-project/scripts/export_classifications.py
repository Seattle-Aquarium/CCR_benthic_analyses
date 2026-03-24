"""
export_classifications.py  —  Download & flatten Zooniverse classification exports
Version: 1.0

Usage:
    python export_classifications.py \
        --workflow-id 9876 \
        --subject-set-id 12349 \
        --output-dir exports/

Requirements:
    pip install panoptes-client python-dotenv pandas tqdm

Credentials:
    Copy config.example.env → .env and fill in your details.
"""

import os
import sys
import time
import argparse
import logging
from pathlib import Path
from datetime import datetime

import pandas as pd
from dotenv import load_dotenv
from panoptes_client import Panoptes, Project

# ── Logging ──────────────────────────────────────────────────────────────────
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[
        logging.FileHandler("export_log.txt"),
        logging.StreamHandler(sys.stdout),
    ],
)
log = logging.getLogger(__name__)


def parse_args():
    p = argparse.ArgumentParser(description="Download Zooniverse classification export")
    p.add_argument("--workflow-id",      required=True, help="Zooniverse workflow ID")
    p.add_argument("--subject-set-id",   default=None,  help="Filter to a specific subject set ID")
    p.add_argument("--output-dir",       default="exports/", help="Where to save CSVs (default: exports/)")
    p.add_argument("--generate-new",     action="store_true",
                   help="Request a fresh export from Zooniverse (may take several minutes)")
    return p.parse_args()


def connect_to_zooniverse() -> str:
    load_dotenv(Path(__file__).parent / ".env")
    username   = os.environ["ZOONIVERSE_USERNAME"]
    password   = os.environ["ZOONIVERSE_PASSWORD"]
    project_id = os.environ["ZOONIVERSE_PROJECT_ID"]
    Panoptes.connect(username=username, password=password)
    log.info(f"Authenticated as {username}")
    return project_id


def request_export(project_id: str, workflow_id: str, generate_new: bool) -> pd.DataFrame:
    """Fetch the classification export CSV for a given workflow."""
    project = Project.find(project_id)

    if generate_new:
        log.info("Requesting fresh export from Zooniverse (this may take a few minutes)…")
        project.generate_export("classifications")
        # Poll until the export is ready
        for attempt in range(30):
            time.sleep(20)
            log.info(f"  Waiting for export… attempt {attempt + 1}/30")
            try:
                export = project.get_export("classifications", generate=False)
                break
            except Exception:
                continue
        else:
            log.error("Export did not become ready within 10 minutes. Try again later.")
            sys.exit(1)
    else:
        log.info("Fetching most recent export (use --generate-new for a fresh one)…")
        export = project.get_export("classifications", generate=False)

    df = pd.read_csv(export.content, low_memory=False)
    log.info(f"Downloaded {len(df):,} raw classification rows")
    return df


def filter_by_subject_set(df: pd.DataFrame, subject_set_id: str) -> pd.DataFrame:
    """Filter to rows belonging to the specified subject set."""
    # The subject_set_id is nested in the subject_data JSON column
    mask = df["subject_data"].str.contains(f'"subject_set_id":{subject_set_id}', na=False)
    filtered = df[mask].copy()
    log.info(f"Filtered to {len(filtered):,} rows for subject set {subject_set_id}")
    return filtered


def flatten_annotations(df: pd.DataFrame) -> pd.DataFrame:
    """
    Basic flattening of the annotations JSON column.
    Adapt this function to match your workflow's question/task structure.
    """
    import json

    records = []
    for _, row in df.iterrows():
        base = {
            "classification_id": row.get("classification_id"),
            "user_name":         row.get("user_name"),
            "user_id":           row.get("user_id"),
            "workflow_id":       row.get("workflow_id"),
            "workflow_version":  row.get("workflow_version"),
            "created_at":        row.get("created_at"),
            "subject_ids":       row.get("subject_ids"),
        }

        # Parse subject metadata
        try:
            subject_data = json.loads(row["subject_data"])
            for subj_id, meta in subject_data.items():
                base["transect_id"] = meta.get("transect_id", "")
                base["filename"]    = meta.get("filename", "")
        except Exception:
            pass

        # Parse annotations — edit to match your task structure
        try:
            annotations = json.loads(row["annotations"])
            for task in annotations:
                task_key = task.get("task", "")
                value    = task.get("value", "")
                base[f"task_{task_key}"] = value
        except Exception:
            pass

        records.append(base)

    flat = pd.DataFrame(records)
    log.info(f"Flattened to {len(flat):,} rows, {len(flat.columns)} columns")
    return flat


def save_output(df: pd.DataFrame, output_dir: Path, workflow_id: str,
                subject_set_id: str | None) -> Path:
    output_dir.mkdir(parents=True, exist_ok=True)
    date_str  = datetime.today().strftime("%Y%m%d")
    ss_suffix = f"_ss{subject_set_id}" if subject_set_id else ""
    filename  = f"workflow{workflow_id}{ss_suffix}_{date_str}_classifications.csv"
    out_path  = output_dir / filename
    df.to_csv(out_path, index=False)
    log.info(f"Saved: {out_path}  ({len(df):,} rows)")
    return out_path


def main():
    args       = parse_args()
    output_dir = Path(args.output_dir)

    project_id = connect_to_zooniverse()
    df_raw     = request_export(project_id, args.workflow_id, args.generate_new)

    if args.subject_set_id:
        df_raw = filter_by_subject_set(df_raw, args.subject_set_id)

    df_flat = flatten_annotations(df_raw)
    out_path = save_output(df_flat, output_dir, args.workflow_id, args.subject_set_id)

    log.info("=" * 60)
    log.info("✅  Export complete!")
    log.info(f"    Workflow ID:     {args.workflow_id}")
    log.info(f"    Subject Set ID:  {args.subject_set_id or 'all'}")
    log.info(f"    Rows exported:   {len(df_flat):,}")
    log.info(f"    Output file:     {out_path}")
    log.info("    👉  Record this in tracker.xlsx Export Log sheet")
    log.info("=" * 60)


if __name__ == "__main__":
    main()
