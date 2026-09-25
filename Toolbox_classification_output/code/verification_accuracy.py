# Takes one or more Toolbox dataset csv files (located in 'data') that have been
# verified via Zooniverse/Toolbox review as input, i.e. each has a 'Verified' column.
# Compares model predictions ("Machine suggestion 1") against the verified
# ground-truth label ("Label"), restricted to rows where Verified == TRUE.
# Produces the same metrics report as model_metrics.py plus a confusion matrix
# plot as generate_confusion_matrix.py, both scoped to verified rows only.
# Multiple input CSVs are concatenated before scoring.

import json
import sys
import tkinter as tk
from tkinter import ttk, filedialog, messagebox
from pathlib import Path
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.metrics import confusion_matrix, ConfusionMatrixDisplay

# Path for persisting GUI state between runs
_CONFIG_PATH = Path(__file__).parent / ".verification_accuracy_config.json"


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


def get_args_via_gui():
    result = {}
    cfg = _load_gui_config()

    root = tk.Tk()
    root.title("Verification Accuracy")
    root.resizable(False, False)
    pad = {"padx": 10, "pady": 5}

    ttk.Label(root, text="Verification Accuracy",
              font=("Helvetica", 13, "bold")).grid(
        row=0, column=0, columnspan=3, pady=(14, 2), padx=14)
    ttk.Label(root,
              text="Select one or more verified Toolbox CSV(s), then click Run.",
              foreground="grey").grid(row=1, column=0, columnspan=3, pady=(0, 4))
    ttk.Separator(root, orient="horizontal").grid(
        row=2, column=0, columnspan=3, sticky="ew", padx=10, pady=4)

    ttk.Label(root, text="Verified Toolbox CSV file(s):").grid(
        row=3, column=0, columnspan=3, sticky="w", padx=10, pady=(6, 0))

    list_frame = ttk.Frame(root)
    list_frame.grid(row=4, column=0, columnspan=3, padx=10, pady=(2, 0), sticky="ew")

    scrollbar = ttk.Scrollbar(list_frame, orient="vertical")
    listbox = tk.Listbox(list_frame, height=6, width=70, selectmode="extended",
                          yscrollcommand=scrollbar.set)
    scrollbar.config(command=listbox.yview)
    listbox.pack(side="left", fill="x", expand=True)
    scrollbar.pack(side="right", fill="y")

    btn_row = ttk.Frame(root)
    btn_row.grid(row=5, column=0, columnspan=3, pady=(2, 8))

    def add_files():
        paths = filedialog.askopenfilenames(
            title="Select verified Toolbox CSV(s)",
            filetypes=[("CSV files", "*.csv"), ("All files", "*.*")])
        for p in paths:
            if p not in listbox.get(0, "end"):
                listbox.insert("end", p)

    def remove_selected():
        for i in reversed(listbox.curselection()):
            listbox.delete(i)

    ttk.Button(btn_row, text="Add files...", command=add_files).pack(side="left", padx=4)
    ttk.Button(btn_row, text="Remove selected", command=remove_selected).pack(side="left", padx=4)

    ttk.Label(root, text="Output directory:").grid(row=6, column=0, sticky="e", **pad)
    output_var = tk.StringVar()
    ttk.Entry(root, textvariable=output_var, width=55).grid(row=6, column=1, **pad)

    def browse_output():
        initial_dir = output_var.get().strip() or str(Path.home())
        d = filedialog.askdirectory(title="Select output directory", initialdir=initial_dir)
        if d:
            output_var.set(d)

    ttk.Button(root, text="Browse...", command=browse_output).grid(row=6, column=2, **pad)

    ttk.Label(root, text="Output file name:").grid(row=7, column=0, sticky="e", **pad)
    name_var = tk.StringVar()
    ttk.Entry(root, textvariable=name_var, width=55).grid(row=7, column=1, **pad)
    ttk.Label(root, text="e.g. 'HSIL' -> HSIL_accuracy_report.csv, HSIL_confusion_matrix.png",
              foreground="grey").grid(row=8, column=0, columnspan=3, sticky="w", padx=10, pady=(0, 4))

    # Restore last-used paths (only those that still exist on disk)
    for p in cfg.get("csv_paths", []):
        if Path(p).is_file():
            listbox.insert("end", p)
    output_var.set(cfg.get("output_dir", ""))
    name_var.set(cfg.get("output_name", ""))

    ttk.Separator(root, orient="horizontal").grid(
        row=9, column=0, columnspan=3, sticky="ew", padx=10, pady=6)

    btn_frame = ttk.Frame(root)
    btn_frame.grid(row=10, column=0, columnspan=3, pady=(0, 14))

    def on_run():
        csv_paths = list(listbox.get(0, "end"))
        output_dir = output_var.get().strip()
        output_name = name_var.get().strip()

        if not csv_paths:
            messagebox.showerror("Missing input", "Please add at least one verified CSV.")
            return
        if not output_dir:
            messagebox.showerror("Missing input", "Please choose an output directory.")
            return
        if not output_name:
            messagebox.showerror("Missing input", "Please enter an output file name.")
            return
        for p in csv_paths:
            if not Path(p).is_file():
                messagebox.showerror("File not found", f"File not found:\n{p}")
                return

        result["csv_paths"] = csv_paths
        result["output_dir"] = output_dir
        result["output_name"] = output_name
        result["submitted"] = True
        _save_gui_config({
            "csv_paths": csv_paths,
            "output_dir": output_dir,
            "output_name": output_name,
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


def load_verified(input_csv):
    df = pd.read_csv(input_csv)

    # Standardize column names
    df.columns = [col.strip().lower().replace(" ", "_") for col in df.columns]

    if "verified" not in df.columns:
        raise ValueError("Input CSV has no 'Verified' column.")

    # Normalize Verified to boolean (handles True/False, TRUE/FALSE, or bool dtype)
    verified = df["verified"].astype(str).str.strip().str.upper() == "TRUE"
    df = df[verified].copy()

    if df.empty:
        raise ValueError("No rows with Verified == TRUE were found.")

    # Machine confidence of exactly 1 is unreliable and skews accuracy metrics
    if "machine_confidence_1" in df.columns:
        df = df[df["machine_confidence_1"] != 1].copy()

    if df.empty:
        raise ValueError("No rows remaining after excluding machine_confidence_1 == 1.")

    # Drop rows missing a label or machine suggestion (can't be scored or plotted)
    df = df.dropna(subset=["label", "machine_suggestion_1"]).copy()

    if df.empty:
        raise ValueError("No rows remaining after excluding missing label/machine_suggestion_1.")

    return df


def calculate_accuracy(df, output_csv):
    # Check for match (True/False)
    df["is_correct"] = df["label"] == df["machine_suggestion_1"]

    # Model Accuracy
    output = (
        df.groupby("label")
          .agg(
              number_accurate=("is_correct", "sum"),
              number_incorrect=("is_correct", lambda x: (~x).sum())
          )
          .reset_index()
    )

    output["total_annotations"] = output["number_accurate"] + output["number_incorrect"]
    output["percent_accuracy"] = (
        output["number_accurate"] / output["total_annotations"] * 100
    ).round(2)

    # Model Confidence
    def mean_or_nan(x):
        return np.nan if len(x) == 0 else np.mean(x)

    def se_or_nan(x):
        return np.nan if len(x) == 0 else (np.std(x, ddof=1) / np.sqrt(len(x)))

    conf_stats = (
        df.groupby(["label", "is_correct"])["machine_confidence_1"]
          .agg(mean_conf=mean_or_nan, se_conf=se_or_nan)
          .reset_index()
    ).round(2)

    # Pivot so correct/incorrect split into columns
    conf_pivot = conf_stats.pivot(index="label", columns="is_correct")
    conf_pivot.columns = [
        "avg_confidence_incorrect", "avg_confidence_correct",
        "std_error_incorrect", "std_error_correct"
    ]
    conf_pivot = conf_pivot.reset_index()

    # Merge back with output
    output = output.merge(conf_pivot, on="label", how="left")

    # Reorder columns
    col_order = [
        "label",
        "total_annotations",
        "number_accurate",
        "number_incorrect",
        "percent_accuracy",
        "avg_confidence_correct",
        "std_error_correct",
        "avg_confidence_incorrect",
        "std_error_incorrect"
    ]
    output = output[col_order]

    # Save
    output.to_csv(output_csv, na_rep='NULL', index=False)
    print(f"Verified accuracy report saved to {output_csv}")


def plot_confusion_matrix(df, csv_stem, output_png):
    y_true = df["label"]
    y_pred = df["machine_suggestion_1"]

    labels = sorted(set(y_true) | set(y_pred))

    cm_normalized = confusion_matrix(y_true, y_pred, labels=labels, normalize="true")

    fig, ax = plt.subplots(figsize=(18, 14))
    disp = ConfusionMatrixDisplay(confusion_matrix=cm_normalized, display_labels=labels)
    disp.plot(include_values=True, cmap="Blues", ax=ax, xticks_rotation=90, values_format=".2f")

    ax.set_xlabel("Predicted Label (Machine suggestion 1)", fontsize=14)
    ax.set_ylabel("Verified Label", fontsize=14)
    ax.set_title(f"Normalized Confusion Matrix - Verified Rows ({csv_stem})", fontsize=16)
    plt.tight_layout()

    plt.savefig(output_png, dpi=300)
    plt.close()

    print(f"Confusion matrix saved to: {output_png}")


def main():
    args = get_args_via_gui()
    csv_paths = [Path(p) for p in args["csv_paths"]]
    output_dir = Path(args["output_dir"])
    output_dir.mkdir(parents=True, exist_ok=True)

    dfs = []
    skipped = []
    for csv_path in csv_paths:
        try:
            dfs.append(load_verified(csv_path))
        except ValueError as e:
            skipped.append(f"{csv_path.name}: {e}")

    if not dfs:
        messagebox.showerror("No verified rows",
                              "No verified rows were found in any input file.")
        sys.exit(1)

    df = pd.concat(dfs, ignore_index=True)
    output_name = args["output_name"]

    metrics_output = output_dir / f"{output_name}_accuracy_report.csv"
    calculate_accuracy(df, metrics_output)

    cm_output = output_dir / f"{output_name}_confusion_matrix.png"
    plot_confusion_matrix(df, output_name, cm_output)

    summary_lines = [
        f"Files processed: {len(dfs)} of {len(csv_paths)}",
        f"Verified rows scored: {len(df):,}",
        "",
        f"Accuracy report:\n{metrics_output}",
        f"Confusion matrix:\n{cm_output}",
    ]
    if skipped:
        summary_lines.append("\nSkipped:\n" + "\n".join(skipped))
        messagebox.showwarning("Verification accuracy complete", "\n".join(summary_lines))
    else:
        messagebox.showinfo("Verification accuracy complete", "\n".join(summary_lines))


if __name__ == "__main__":
    main()
