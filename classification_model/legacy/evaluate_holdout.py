#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
evaluate_holdout.py — Run a trained YOLO classification model against a
held-out evaluation set and produce an accuracy report + confusion matrix,
in the same style/methodology as EBM_CNL_S24_W25_accuracy_report.csv /
EBM_CNL_S24_W25_confusion_matrix.png.

Expects the held-out set as a FLAT ImageFolder — <eval_dir>/<Label>/*.jpg —
i.e. exactly what extract_training_patches.py produces with "No split"
checked. This should be data the model has never been trained or
validated on (a held-out transect), not a train/val split.

For every image:
    - Runs inference with the given model weights
    - Records the true label (parent folder name), predicted label
      (model's top-1), and top-1 confidence

Outputs (written to the chosen output folder):
    - accuracy_report.csv    — per-class total/correct/incorrect/accuracy/
                                avg confidence (correct vs incorrect),
                                same columns as your existing accuracy report
    - confusion_matrix.png   — normalized confusion matrix, true label (rows,
                                only classes present in the held-out set) vs
                                predicted label (columns, full model taxonomy)
    - predictions_detail.csv — one row per image: filepath, true label,
                                predicted label, confidence, correct — for
                                tracing any individual misclassification back
                                to its source image

Usage:
    python evaluate_holdout_gui.py

    A window opens to select:
      1. Model weights (.pt) file (required)
      2. Held-out evaluation folder — flat <Label>/*.jpg (required)
      3. Output folder for the report + confusion matrix (required)
      4. Image size / batch size / device (optional, have defaults)
      5. Dataset label (used in the confusion matrix title/filenames)
      6. Baseline accuracy to compare against (optional — shows a delta
         in the summary popup, e.g. against your 47.38% field baseline)
      7. Preview only checkbox — count images per class without running
         the model, to sanity-check the folder before a long inference run

Requirements:
    pip install ultralytics pandas numpy matplotlib tqdm
"""

import json
import logging
import sys
import tkinter as tk
from tkinter import ttk, filedialog, messagebox
from pathlib import Path

import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from tqdm import tqdm

try:
    import torch
    # Disable cuDNN algorithm autotuning. On the first forward pass for a
    # given input shape (exactly what model.warmup() triggers), cuDNN's
    # benchmark-mode search can try convolution algorithms that request a
    # disproportionately large workspace allocation, independent of batch
    # size — a known failure mode, not something that scales with batch.
    # This trades a little steady-state speed for avoiding that case.
    torch.backends.cudnn.benchmark = False
except ImportError:
    pass

# ── Logging ──────────────────────────────────────────────────────────────
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[
        logging.FileHandler("evaluate_holdout_log.txt", encoding="utf-8"),
        logging.StreamHandler(sys.stdout),
    ],
)
log = logging.getLogger(__name__)

IMG_EXTS = {".jpg", ".jpeg", ".png"}

_CONFIG_PATH = Path(__file__).parent / ".evaluate_holdout_config.json"


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
# COLLECT HELD-OUT IMAGES (flat <Label>/*.jpg, no split)
# ============================================================
def list_holdout_images(eval_dir: str) -> dict:
    """Return {class_name: [file_path, ...]} for a flat <Label>/*.jpg folder."""
    root = Path(eval_dir)
    result = {}
    if not root.is_dir():
        return result
    for class_dir in sorted(root.iterdir()):
        if not class_dir.is_dir():
            continue
        files = [str(f) for f in class_dir.iterdir()
                 if f.suffix.lower() in IMG_EXTS]
        if files:
            result[class_dir.name] = files
    return result


# ============================================================
# INFERENCE
# ============================================================
def run_inference(model, class_files: dict, imgsz: int, batch: int, device: str,
                   half: bool = False) -> pd.DataFrame:
    """
    Runs the model against every image in class_files and returns a
    DataFrame with one row per image: filepath, true_label, pred_label,
    confidence, correct.
    """
    rows = []
    all_paths = []
    all_true_labels = []
    for true_label, files in class_files.items():
        all_paths.extend(files)
        all_true_labels.extend([true_label] * len(files))

    total = len(all_paths)
    log.info(f"Running inference on {total:,} images...")

    predict_kwargs = dict(source=all_paths, imgsz=imgsz, batch=batch,
                           stream=True, verbose=False)
    if device:
        predict_kwargs["device"] = device
    if half:
        # FP16 inference: roughly halves memory for weights/activations.
        # Confirmed current Ultralytics predictor argument name (as of this
        # version) is `quantize=16`, not the older `half=True`.
        predict_kwargs["quantize"] = 16
        log.info("FP16 (quantize=16) inference enabled")

    results_iter = model.predict(**predict_kwargs)

    for path, true_label, result in tqdm(
            zip(all_paths, all_true_labels, results_iter),
            total=total, desc="Evaluating", unit="img"):
        try:
            top1_idx = result.probs.top1
            pred_label = result.names[top1_idx]
            confidence = float(result.probs.top1conf)
        except Exception as e:
            log.warning(f"Inference failed on {path}: {e}")
            continue

        rows.append({
            "filepath": path,
            "true_label": true_label,
            "pred_label": pred_label,
            "confidence": confidence,
            "correct": pred_label == true_label,
        })

    return pd.DataFrame(rows)


# ============================================================
# ACCURACY REPORT (same columns as EBM_CNL_S24_W25_accuracy_report.csv)
# ============================================================
def build_accuracy_report(df: pd.DataFrame) -> pd.DataFrame:
    records = []
    for label, group in df.groupby("true_label"):
        total = len(group)
        correct = group[group["correct"]]
        incorrect = group[~group["correct"]]
        records.append({
            "label": label,
            "total_annotations": total,
            "number_accurate": len(correct),
            "number_incorrect": len(incorrect),
            "percent_accuracy": round(100 * len(correct) / total, 2) if total else 0.0,
            "avg_confidence_correct": round(correct["confidence"].mean(), 4) if len(correct) else np.nan,
            "avg_confidence_incorrect": round(incorrect["confidence"].mean(), 4) if len(incorrect) else np.nan,
        })
    report = pd.DataFrame(records).sort_values("label").reset_index(drop=True)
    return report


# ============================================================
# CONFUSION MATRIX
# ============================================================
def build_confusion_matrix(df: pd.DataFrame, all_model_classes: list) -> pd.DataFrame:
    """
    Rows: classes actually present in the held-out ground truth (true_label).
    Columns: full model taxonomy (a prediction could land on any class the
    model knows, even one absent from this particular held-out set).
    Normalized per row (each row sums to 1.0 across all columns it hit).
    """
    true_labels = sorted(df["true_label"].unique())
    columns = sorted(all_model_classes)

    matrix = pd.DataFrame(0, index=true_labels, columns=columns, dtype=float)
    counts = df.groupby(["true_label", "pred_label"]).size()
    for (t, p), n in counts.items():
        if p in matrix.columns:
            matrix.loc[t, p] = n
        else:
            # Shouldn't happen if all_model_classes is complete, but don't
            # silently lose predictions if it does
            matrix[p] = matrix.get(p, 0)
            matrix.loc[t, p] = n

    row_sums = matrix.sum(axis=1)
    normalized = matrix.div(row_sums.replace(0, np.nan), axis=0).fillna(0)
    return normalized


def plot_confusion_matrix(matrix: pd.DataFrame, title: str, out_path: str) -> None:
    fig_w = max(10, 0.35 * len(matrix.columns))
    fig_h = max(8, 0.35 * len(matrix.index))
    fig, ax = plt.subplots(figsize=(fig_w, fig_h))

    im = ax.imshow(matrix.values, cmap="Blues", vmin=0, vmax=1, aspect="auto")

    ax.set_xticks(range(len(matrix.columns)))
    ax.set_xticklabels(matrix.columns, rotation=90, fontsize=7)
    ax.set_yticks(range(len(matrix.index)))
    ax.set_yticklabels(matrix.index, fontsize=7)

    for i in range(matrix.shape[0]):
        for j in range(matrix.shape[1]):
            val = matrix.values[i, j]
            if val > 0:
                color = "white" if val > 0.5 else "black"
                ax.text(j, i, f"{val:.2f}", ha="center", va="center",
                        fontsize=5.5, color=color)

    ax.set_xlabel("Predicted Label (Machine suggestion 1)")
    ax.set_ylabel("Verified Label")
    ax.set_title(title)
    fig.colorbar(im, ax=ax, fraction=0.03, pad=0.02)
    fig.tight_layout()
    fig.savefig(out_path, dpi=150)
    plt.close(fig)


# ============================================================
# CORE EVALUATION
# ============================================================
def evaluate(model_path: str, eval_dir: str, output_dir: str, imgsz: int,
             batch: int, device: str, dataset_label: str,
             baseline_accuracy: float = None, preview_only: bool = False,
             half: bool = False) -> dict:
    class_files = list_holdout_images(eval_dir)

    stats = {
        "classes": len(class_files),
        "total_images": sum(len(v) for v in class_files.values()),
        "preview_only": preview_only,
        "overall_accuracy": None,
        "baseline_accuracy": baseline_accuracy,
    }

    log.info("=" * 70)
    log.info(f"{'Class':<15} {'images':>8}")
    log.info("-" * 70)
    for cls, files in sorted(class_files.items()):
        log.info(f"{cls:<15} {len(files):>8}")
    log.info("-" * 70)
    log.info(f"{'TOTAL':<15} {stats['total_images']:>8}  ({stats['classes']} classes)")
    log.info("=" * 70)

    if preview_only or stats["total_images"] == 0:
        if stats["total_images"] == 0:
            log.warning("No images found — check the folder path and structure "
                        "(expected <eval_dir>/<Label>/*.jpg).")
        return stats

    from ultralytics import YOLO
    log.info(f"Loading model from {model_path}...")
    model = YOLO(model_path)
    all_model_classes = list(model.names.values())
    log.info(f"Model has {len(all_model_classes)} classes.")

    df = run_inference(model, class_files, imgsz, batch, device, half=half)
    stats["evaluated"] = len(df)
    stats["failed"] = stats["total_images"] - len(df)

    if df.empty:
        log.warning("No images were successfully evaluated.")
        return stats

    overall_accuracy = 100 * df["correct"].sum() / len(df)
    stats["overall_accuracy"] = round(overall_accuracy, 2)
    log.info(f"Overall accuracy: {overall_accuracy:.2f}% ({df['correct'].sum():,}/{len(df):,})")

    report = build_accuracy_report(df)
    report_path = Path(output_dir) / f"{dataset_label}_accuracy_report.csv"
    report.to_csv(report_path, index=False)
    log.info(f"Accuracy report written to {report_path}")

    detail_path = Path(output_dir) / f"{dataset_label}_predictions_detail.csv"
    df.to_csv(detail_path, index=False)
    log.info(f"Per-image predictions written to {detail_path}")

    matrix = build_confusion_matrix(df, all_model_classes)
    cm_path = Path(output_dir) / f"{dataset_label}_confusion_matrix.png"
    plot_confusion_matrix(
        matrix,
        title=f"Normalized Confusion Matrix - Held-out Evaluation ({dataset_label})",
        out_path=str(cm_path),
    )
    log.info(f"Confusion matrix written to {cm_path}")

    stats["report_path"] = str(report_path)
    stats["detail_path"] = str(detail_path)
    stats["cm_path"] = str(cm_path)

    return stats


# ============================================================
# GUI
# ============================================================
def get_args_via_gui():
    result = {}
    cfg = _load_gui_config()

    root = tk.Tk()
    root.title("Evaluate Held-Out Data")
    root.resizable(False, False)
    pad = {"padx": 10, "pady": 5}

    ttk.Label(root, text="Evaluate Model Against Held-Out Data",
              font=("Helvetica", 13, "bold")).grid(
        row=0, column=0, columnspan=3, pady=(14, 2), padx=14)
    ttk.Label(root,
              text="Run a trained model on a flat <Label>/*.jpg held-out set and "
                   "produce an accuracy report + confusion matrix",
              foreground="grey").grid(row=1, column=0, columnspan=3, pady=(0, 4))
    ttk.Separator(root, orient="horizontal").grid(
        row=2, column=0, columnspan=3, sticky="ew", padx=10, pady=4)

    # ── Model weights ────────────────────────────────────────────────────
    ttk.Label(root, text="Model weights (.pt):").grid(row=3, column=0, sticky="e", **pad)
    model_var = tk.StringVar(value=cfg.get("model_path", ""))
    ttk.Entry(root, textvariable=model_var, width=55).grid(row=3, column=1, **pad)

    def browse_model():
        p = filedialog.askopenfilename(
            title="Select model weights",
            filetypes=[("PyTorch weights", "*.pt"), ("All files", "*.*")],
        )
        if p:
            model_var.set(p)

    ttk.Button(root, text="Browse...", command=browse_model).grid(row=3, column=2, **pad)

    # ── Held-out eval folder ────────────────────────────────────────────
    ttk.Label(root, text="Held-out evaluation folder:").grid(row=4, column=0, sticky="e", **pad)
    eval_var = tk.StringVar(value=cfg.get("eval_dir", ""))
    ttk.Entry(root, textvariable=eval_var, width=55).grid(row=4, column=1, **pad)

    def browse_eval():
        p = filedialog.askdirectory(title="Select held-out evaluation folder (flat <Label>/*.jpg)")
        if p:
            eval_var.set(p)

    ttk.Button(root, text="Browse...", command=browse_eval).grid(row=4, column=2, **pad)

    # ── Output directory ────────────────────────────────────────────────
    ttk.Label(root, text="Output folder (report + confusion matrix):").grid(row=5, column=0, sticky="e", **pad)
    outdir_var = tk.StringVar(value=cfg.get("output_dir", ""))
    ttk.Entry(root, textvariable=outdir_var, width=55).grid(row=5, column=1, **pad)

    def browse_outdir():
        p = filedialog.askdirectory(title="Select output folder")
        if p:
            outdir_var.set(p)

    ttk.Button(root, text="Browse...", command=browse_outdir).grid(row=5, column=2, **pad)

    ttk.Separator(root, orient="horizontal").grid(
        row=6, column=0, columnspan=3, sticky="ew", padx=10, pady=6)

    # ── Numeric / text params ───────────────────────────────────────────
    ttk.Label(root, text="Image size (imgsz):").grid(row=7, column=0, sticky="e", **pad)
    imgsz_var = tk.StringVar(value=str(cfg.get("imgsz", 256)))
    ttk.Entry(root, textvariable=imgsz_var, width=15).grid(row=7, column=1, sticky="w", **pad)

    ttk.Label(root, text="Batch size:").grid(row=8, column=0, sticky="e", **pad)
    batch_var = tk.StringVar(value=str(cfg.get("batch", 32)))
    ttk.Entry(root, textvariable=batch_var, width=15).grid(row=8, column=1, sticky="w", **pad)

    ttk.Label(root, text="Device (blank = auto, e.g. 'cpu' or '0'):").grid(row=9, column=0, sticky="e", **pad)
    device_var = tk.StringVar(value=cfg.get("device", ""))
    ttk.Entry(root, textvariable=device_var, width=15).grid(row=9, column=1, sticky="w", **pad)

    half_var = tk.BooleanVar(value=cfg.get("half", False))
    ttk.Checkbutton(
        root,
        text="FP16 inference (roughly halves GPU memory use — try if hitting CUDA OOM)",
        variable=half_var,
    ).grid(row=9, column=2, sticky="w")

    ttk.Label(root, text="Dataset label (used in filenames/title):").grid(row=10, column=0, sticky="e", **pad)
    label_var = tk.StringVar(value=cfg.get("dataset_label", "holdout_eval"))
    ttk.Entry(root, textvariable=label_var, width=30).grid(row=10, column=1, sticky="w", **pad)

    ttk.Label(root, text="Baseline accuracy % to compare against (optional):").grid(row=11, column=0, sticky="e", **pad)
    baseline_var = tk.StringVar(value=str(cfg.get("baseline_accuracy", "")))
    ttk.Entry(root, textvariable=baseline_var, width=15).grid(row=11, column=1, sticky="w", **pad)

    ttk.Separator(root, orient="horizontal").grid(
        row=12, column=0, columnspan=3, sticky="ew", padx=10, pady=6)

    # ── Preview only ─────────────────────────────────────────────────────
    preview_var = tk.BooleanVar(value=cfg.get("preview_only", True))
    ttk.Checkbutton(
        root,
        text="Preview only  (count images per class, don't run the model —\n"
             "use this first to sanity-check the folder before a long inference run)",
        variable=preview_var,
    ).grid(row=13, column=0, columnspan=3, pady=(0, 4))

    ttk.Separator(root, orient="horizontal").grid(
        row=14, column=0, columnspan=3, sticky="ew", padx=10, pady=6)

    # ── Run / Cancel buttons ────────────────────────────────────────────
    btn_frame = ttk.Frame(root)
    btn_frame.grid(row=15, column=0, columnspan=3, pady=(0, 14))

    def on_run():
        model_path = model_var.get().strip()
        eval_dir = eval_var.get().strip()
        output_dir = outdir_var.get().strip()
        preview_only = preview_var.get()

        if not preview_only:
            if not model_path or not Path(model_path).is_file():
                messagebox.showerror("Missing input", "Please select a valid model weights (.pt) file.")
                return
        if not eval_dir or not Path(eval_dir).is_dir():
            messagebox.showerror("Missing input", "Please select a valid held-out evaluation folder.")
            return
        if not output_dir:
            messagebox.showerror("Missing input", "Please select an output folder.")
            return

        try:
            imgsz = int(imgsz_var.get())
        except ValueError:
            messagebox.showerror("Invalid value", "Image size must be a whole number.")
            return

        try:
            batch = int(batch_var.get())
        except ValueError:
            messagebox.showerror("Invalid value", "Batch size must be a whole number.")
            return

        device = device_var.get().strip()

        dataset_label = label_var.get().strip() or "holdout_eval"

        baseline_accuracy = None
        baseline_str = baseline_var.get().strip()
        if baseline_str:
            try:
                baseline_accuracy = float(baseline_str)
            except ValueError:
                messagebox.showerror("Invalid value", "Baseline accuracy must be a number (e.g. 47.38).")
                return

        result["model_path"] = model_path
        result["eval_dir"] = eval_dir
        result["output_dir"] = output_dir
        result["imgsz"] = imgsz
        result["batch"] = batch
        result["device"] = device
        result["dataset_label"] = dataset_label
        result["baseline_accuracy"] = baseline_accuracy
        result["preview_only"] = preview_only
        result["half"] = half_var.get()
        result["submitted"] = True

        _save_gui_config({
            "model_path": model_path,
            "eval_dir": eval_dir,
            "output_dir": output_dir,
            "imgsz": imgsz,
            "batch": batch,
            "device": device,
            "dataset_label": dataset_label,
            "baseline_accuracy": baseline_str,
            "preview_only": preview_only,
            "half": half_var.get(),
        })
        root.destroy()

    def on_cancel():
        root.destroy()

    ttk.Button(btn_frame, text="  Run  ", command=on_run).pack(side="left", padx=8)
    ttk.Button(btn_frame, text="Cancel", command=on_cancel).pack(side="left", padx=8)

    root.mainloop()

    if not result.get("submitted"):
        print("Cancelled by user.")
        sys.exit(0)

    return result


# ============================================================
# ENTRY POINT
# ============================================================
def main():
    args = get_args_via_gui()

    log.info("Evaluate model against held-out data")
    log.info(f"  model weights   : {args['model_path']}")
    log.info(f"  eval folder     : {args['eval_dir']}")
    log.info(f"  output dir      : {args['output_dir']}")
    log.info(f"  imgsz / batch   : {args['imgsz']} / {args['batch']}")
    log.info(f"  device          : {args['device'] or '(auto)'}")
    log.info(f"  dataset label   : {args['dataset_label']}")
    if args["preview_only"]:
        log.info("  mode            : PREVIEW ONLY")

    stats = evaluate(
        model_path=args["model_path"],
        eval_dir=args["eval_dir"],
        output_dir=args["output_dir"],
        imgsz=args["imgsz"],
        batch=args["batch"],
        device=args["device"],
        dataset_label=args["dataset_label"],
        baseline_accuracy=args["baseline_accuracy"],
        preview_only=args["preview_only"],
        half=args["half"],
    )

    # ── Summary popup ────────────────────────────────────────────────────
    lines = [f"Classes: {stats['classes']}", f"Total images: {stats['total_images']:,}"]

    if stats["preview_only"]:
        lines.insert(0, "PREVIEW ONLY — model not run.")
        messagebox.showinfo("Preview complete", "\n".join(lines))
        return

    if stats["overall_accuracy"] is None:
        lines.append("")
        lines.append("\u26a0 No images were evaluated — check the log.")
        messagebox.showwarning("Evaluation incomplete", "\n".join(lines))
        return

    lines.append(f"Evaluated: {stats['evaluated']:,}  (failed to load: {stats.get('failed', 0):,})")
    lines.append("")
    lines.append(f"Overall accuracy: {stats['overall_accuracy']:.2f}%")
    if stats["baseline_accuracy"] is not None:
        delta = stats["overall_accuracy"] - stats["baseline_accuracy"]
        sign = "+" if delta >= 0 else ""
        lines.append(f"Baseline: {stats['baseline_accuracy']:.2f}%  (\u0394 {sign}{delta:.2f} pts)")
    lines.append("")
    lines.append(f"Saved to:\n{args['output_dir']}")

    messagebox.showinfo("Evaluation complete", "\n".join(lines))


if __name__ == "__main__":
    main()