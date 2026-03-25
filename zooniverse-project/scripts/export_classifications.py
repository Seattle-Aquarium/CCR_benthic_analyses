"""
export_classifications.py  —  Download & flatten Zooniverse classification exports
Version: 1.0

Usage:
    python export_classifications.py

    A window opens and asks for:
      1. Workflow ID
      2. Optional subject set ID filter
      3. Output folder
      4. Whether to request a fresh export

Requirements:
    pip install panoptes-client python-dotenv pandas tqdm

Credentials:
    Copy config.example.env → .env and fill in your details.
"""

import io
import os
import sys
import time
import types
import logging
import tkinter as tk
from tkinter import ttk, filedialog, messagebox
from pathlib import Path
from datetime import datetime

import pandas as pd
from dotenv import load_dotenv
from requests.exceptions import RequestException
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


def get_args_via_gui():
    """Open a simple GUI form to collect export parameters."""
    result = {}

    root = tk.Tk()
    root.title("Export Zooniverse Classifications")
    root.resizable(False, False)

    pad = {"padx": 10, "pady": 5}

    ttk.Label(
        root,
        text="Zooniverse Classification Export",
        font=("Helvetica", 13, "bold"),
    ).grid(row=0, column=0, columnspan=3, pady=(14, 4), padx=14)
    ttk.Label(
        root,
        text="Enter the export settings below, then click Run.",
        foreground="grey",
    ).grid(row=1, column=0, columnspan=3, pady=(0, 4))
    ttk.Separator(root, orient="horizontal").grid(
        row=2, column=0, columnspan=3, sticky="ew", padx=10, pady=4
    )

    ttk.Label(root, text="Workflow ID:").grid(row=3, column=0, sticky="e", **pad)
    workflow_id_var = tk.StringVar()
    ttk.Entry(root, textvariable=workflow_id_var, width=40).grid(row=3, column=1, **pad)

    ttk.Label(root, text="Subject set ID:").grid(row=4, column=0, sticky="e", **pad)
    subject_set_id_var = tk.StringVar()
    sf = ttk.Frame(root)
    sf.grid(row=4, column=1, sticky="w", **pad)
    ttk.Entry(sf, textvariable=subject_set_id_var, width=20).pack(side="left")
    ttk.Label(sf, text="  optional filter", foreground="grey").pack(side="left")

    ttk.Label(root, text="Output folder:").grid(row=5, column=0, sticky="e", **pad)
    output_dir_var = tk.StringVar(value=str(Path(__file__).resolve().parent.parent / "exports"))
    ttk.Entry(root, textvariable=output_dir_var, width=55).grid(row=5, column=1, **pad)

    def browse_output_dir():
        path = filedialog.askdirectory(title="Select folder for exported CSV files")
        if path:
            output_dir_var.set(path)

    ttk.Button(root, text="Browse...", command=browse_output_dir).grid(
        row=5, column=2, **pad
    )

    generate_new_var = tk.BooleanVar(value=False)
    ttk.Checkbutton(
        root,
        text="Request a fresh export from Zooniverse (can take several minutes)",
        variable=generate_new_var,
    ).grid(row=6, column=0, columnspan=3, sticky="w", padx=18, pady=4)

    ttk.Separator(root, orient="horizontal").grid(
        row=7, column=0, columnspan=3, sticky="ew", padx=10, pady=6
    )

    btn_frame = ttk.Frame(root)
    btn_frame.grid(row=8, column=0, columnspan=3, pady=(0, 14))

    def on_run():
        workflow_id = workflow_id_var.get().strip()
        subject_set_id = subject_set_id_var.get().strip()
        output_dir = output_dir_var.get().strip()

        if not workflow_id:
            messagebox.showerror("Missing input", "Please enter a workflow ID.")
            return
        if not output_dir:
            messagebox.showerror("Missing input", "Please select an output folder.")
            return

        result["workflow_id"] = workflow_id
        result["subject_set_id"] = subject_set_id or None
        result["output_dir"] = output_dir
        result["generate_new"] = generate_new_var.get()
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
        workflow_id=result["workflow_id"],
        subject_set_id=result["subject_set_id"],
        output_dir=result["output_dir"],
        generate_new=result["generate_new"],
    )


def connect_to_zooniverse() -> str:
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
                    "Try again in a few minutes, or test from a different network."
                ) from exc
            time.sleep(attempt * 2)
        except Exception:
            raise

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

    df = pd.read_csv(io.BytesIO(export.content), low_memory=False)
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
    args       = get_args_via_gui()
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
