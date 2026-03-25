"""
export_subjectset.py  -  Export classifications for a single subject set

This script:
  1. Loads Zooniverse credentials from scripts/config.env
  2. Opens a GUI for Subject Set ID and output file path
  3. Requests a fresh classifications export for that subject set
  4. Saves the CSV to disk
"""

import os
import sys
import types
from pathlib import Path
import tkinter as tk
from tkinter import ttk, filedialog, messagebox

from dotenv import load_dotenv
from panoptes_client import Panoptes, SubjectSet


def get_args_via_gui():
    """Prompt for subject set ID and output CSV path."""
    result = {}

    root = tk.Tk()
    root.title("Export Subject Set Classifications")
    root.resizable(False, False)

    pad = {"padx": 10, "pady": 5}

    ttk.Label(
        root,
        text="Zooniverse Subject Set Export",
        font=("Helvetica", 13, "bold"),
    ).grid(row=0, column=0, columnspan=3, pady=(14, 4), padx=14)
    ttk.Label(
        root,
        text="Enter the subject set ID and where to save the export.",
        foreground="grey",
    ).grid(row=1, column=0, columnspan=3, pady=(0, 4))
    ttk.Separator(root, orient="horizontal").grid(
        row=2, column=0, columnspan=3, sticky="ew", padx=10, pady=4
    )

    ttk.Label(root, text="Subject set ID:").grid(row=3, column=0, sticky="e", **pad)
    subject_set_id_var = tk.StringVar()
    ttk.Entry(root, textvariable=subject_set_id_var, width=25).grid(
        row=3, column=1, sticky="w", **pad
    )

    ttk.Label(root, text="Save CSV as:").grid(row=4, column=0, sticky="e", **pad)
    output_path_var = tk.StringVar(value=str(Path.cwd() / "subject_set_classifications.csv"))
    ttk.Entry(root, textvariable=output_path_var, width=55).grid(row=4, column=1, **pad)

    def browse_output():
        path = filedialog.asksaveasfilename(
            title="Save classifications CSV as...",
            defaultextension=".csv",
            filetypes=[("CSV files", "*.csv"), ("All files", "*.*")],
            initialfile="subject_set_classifications.csv",
        )
        if path:
            output_path_var.set(path)

    ttk.Button(root, text="Browse...", command=browse_output).grid(
        row=4, column=2, **pad
    )

    ttk.Separator(root, orient="horizontal").grid(
        row=5, column=0, columnspan=3, sticky="ew", padx=10, pady=6
    )

    btn_frame = ttk.Frame(root)
    btn_frame.grid(row=6, column=0, columnspan=3, pady=(0, 14))

    def on_run():
        subject_set_id_text = subject_set_id_var.get().strip()
        output_path = output_path_var.get().strip()

        if not subject_set_id_text:
            messagebox.showerror("Missing input", "Please enter a subject set ID.")
            return
        try:
            subject_set_id = int(subject_set_id_text)
        except ValueError:
            messagebox.showerror("Invalid input", "Subject set ID must be a whole number.")
            return

        if not output_path:
            messagebox.showerror("Missing input", "Please choose where to save the CSV.")
            return

        result["subject_set_id"] = subject_set_id
        result["output_path"] = output_path
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
        subject_set_id=result["subject_set_id"],
        output_path=result["output_path"],
    )


def connect_to_zooniverse() -> None:
    """Connect using credentials from scripts/config.env."""
    env_path = Path(__file__).parent / "config.env"
    load_dotenv(env_path)

    username = os.environ.get("ZOONIVERSE_USERNAME", "").strip()
    password = os.environ.get("ZOONIVERSE_PASSWORD", "").strip()

    missing = []
    if not username:
        missing.append("ZOONIVERSE_USERNAME")
    if not password:
        missing.append("ZOONIVERSE_PASSWORD")
    if missing:
        raise ValueError(
            "Missing required credentials in scripts/config.env: "
            + ", ".join(missing)
        )

    Panoptes.connect(username=username, password=password)


def main():
    args = get_args_via_gui()
    connect_to_zooniverse()

    subject_set = SubjectSet.find(args.subject_set_id)
    response = subject_set.get_export("classifications", generate=True)

    out_path = Path(args.output_path)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_bytes(response.content)

    print(f"Done: {out_path}")


if __name__ == "__main__":
    main()