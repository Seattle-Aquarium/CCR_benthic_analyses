"""
Stage 4 - a trained model + a held-out set -> the accuracy report.

This stage exists because the internal numbers lie. The previous model reported
97.86% on its own test split and 47.38% when checked against field-verified
annotations. The split was not the problem; the provenance was. Train, val and
test all came out of the same curated pipeline, so the test set measured how
well the model had learned that pipeline, not how well it classifies the
seafloor.

So: held-out transects, never trained or validated on, scored with the same
columns as the original field accuracy report so the two are directly
comparable.

**Independence is verified, not assumed.** Before reporting anything, the
held-out patches are hashed against the training set. If any are byte-identical,
the stage reports the overlap and refuses to produce a number. That check is
here because its absence cost a real evaluation: 12.5% of the held-out set had
leaked, and the reported 65.85% was actually 62.81%.
"""

from __future__ import annotations

import time
from pathlib import Path

import numpy as np
import pandas as pd

from .. import hashing
from ..fsutil import list_class_images, looks_like_split_dataset
from ..logging_setup import get_logger
from ..progress import ProgressCB, Stages, check_cancelled
from . import StageResult, guard

log = get_logger("evaluate")


def run(cfg, *, progress: ProgressCB | None = None, cancel=None) -> StageResult:
    started = time.time()
    return guard(lambda: _run(cfg, progress, cancel), started)


def _run(cfg, progress, cancel) -> StageResult:
    res = StageResult(preview=cfg.preview_only)
    st = Stages(progress).plan(scan=2, verify=13, infer=80, report=5)

    if not cfg.eval_dir or not Path(cfg.eval_dir).is_dir():
        res.errors.append(f"Held-out folder not found: {cfg.eval_dir or '(none)'}")
        return res
    if looks_like_split_dataset(cfg.eval_dir):
        res.errors.append(
            f"{cfg.eval_dir} has train/ and val/ subfolders, so it is a training "
            f"dataset, not a held-out set. A held-out set is flat: "
            f"<folder>/<Label>/*.jpg. Re-extract it with 'No train/val split'.")
        return res

    class_files = list_class_images(cfg.eval_dir, None)
    total = sum(len(v) for v in class_files.values())
    if not total:
        res.errors.append(
            f"No images under {cfg.eval_dir}. Expected <folder>/<Label>/*.jpg.")
        return res

    log.info(f"Held-out set: {total:,} image(s) across {len(class_files)} class(es)")
    for cls, files in sorted(class_files.items()):
        log.info(f"  {cls:<16} {len(files):>7,}")

    res.outputs.update({"classes": len(class_files), "total_images": total})
    st.finish("scan", "held-out set inventoried")

    # ---- independence ----------------------------------------------
    leaked: set[str] = set()
    if cfg.verify_independence and cfg.train_dataset_dir:
        if not Path(cfg.train_dataset_dir).is_dir():
            res.warnings.append(
                f"Training dataset not found at {cfg.train_dataset_dir} - "
                f"independence could NOT be verified.")
        else:
            leaked = _verify_independence(cfg, res, st.sub("verify"), cancel)
            if leaked and not cfg.preview_only:
                return res
    elif cfg.verify_independence:
        res.warnings.append(
            "No training dataset given, so independence could not be verified. "
            "The accuracy below is only trustworthy if you are certain these "
            "images were never trained on.")
    else:
        res.warnings.append(
            "Independence checking is off. If any held-out image was trained "
            "on, the accuracy below overstates the model.")
    st.finish("verify", "independence checked")

    if cfg.preview_only:
        res.say(f"{total:,} image(s) across {len(class_files)} class(es) in "
                f"{cfg.eval_dir}")
        if leaked:
            res.say(f"{len(leaked):,} of them are byte-identical to training "
                    f"data and would invalidate the result.")
        elif cfg.verify_independence and cfg.train_dataset_dir:
            res.say("Verified independent of the training set.")
        res.say("Nothing evaluated - clear 'Preview only' to run inference.")
        return res

    if not cfg.model_path or not Path(cfg.model_path).is_file():
        res.errors.append(f"Model weights not found: {cfg.model_path or '(none)'}")
        return res

    output_dir = Path(cfg.output_dir or Path(cfg.model_path).parent.parent)
    output_dir.mkdir(parents=True, exist_ok=True)
    label = cfg.dataset_label or Path(cfg.eval_dir).name

    # ---- inference --------------------------------------------------
    from ultralytics import YOLO

    log.info(f"Loading model: {cfg.model_path}")
    model = YOLO(cfg.model_path)
    model_classes = list(model.names.values())
    log.info(f"Model knows {len(model_classes)} class(es).")

    untrained = sorted(set(class_files) - set(model_classes))
    if untrained:
        log.info(f"Held-out classes the model was not trained on: {untrained} "
                 f"- scored separately, not counted against it.")

    df = _infer(model, class_files, cfg, st.sub("infer"), cancel)
    if df.empty:
        res.errors.append("No images were successfully evaluated.")
        return res

    # A model cannot be right about a class it has never seen, so those images
    # would be guaranteed misses -- and worse, whatever the model calls each
    # one lands as a false positive on a class it *does* know, deflating that
    # class's precision and inflating its apparent cover. Leaving a class out
    # of training is a deliberate choice now, so its held-out images are set
    # aside from every metric and reported on their own terms: what did the
    # model make of them?
    foreign = df[~df["true_label"].isin(model_classes)]
    df = df[df["true_label"].isin(model_classes)].reset_index(drop=True)
    if len(foreign):
        _report_untrained(foreign, output_dir, label, res)
    if df.empty:
        res.errors.append("Every held-out image belongs to a class the model "
                          "was not trained on - nothing to score.")
        return res

    res.outputs["evaluated"] = len(df)
    res.outputs["failed"] = total - len(df) - len(foreign)
    res.outputs["untrained_images"] = int(len(foreign))

    # ---- report -----------------------------------------------------
    accuracy = 100.0 * df["correct"].sum() / len(df)
    res.outputs["overall_accuracy"] = round(accuracy, 2)
    log.info(f"Overall accuracy: {accuracy:.2f}% "
             f"({int(df['correct'].sum()):,}/{len(df):,})")

    paths = _write_reports(df, model_classes, output_dir, label, res)
    res.outputs.update(paths)
    # Stage 5 reads this folder rather than the weights: the comparison needs
    # the reports written here, beside the run's own results.csv.
    res.outputs["output_dir"] = str(output_dir)
    st.finish("report", "reports written")

    res.say(f"Held-out accuracy: {accuracy:.2f}%  "
            f"({int(df['correct'].sum()):,}/{len(df):,})")
    if cfg.baseline_accuracy:
        delta = accuracy - cfg.baseline_accuracy
        res.say(f"Field-verified baseline: {cfg.baseline_accuracy:.2f}%  "
                f"({delta:+.2f} points)")
    if cfg.verify_independence and cfg.train_dataset_dir and not leaked:
        res.say("Verified: no held-out image appears in the training set.")

    worst = _worst_classes(df)
    if worst:
        res.say("Weakest classes: " + ", ".join(
            f"{c} {a:.0f}% (n={n:,})" for c, a, n in worst))
    res.say(f"Reports written to {output_dir}")
    return res


def _report_untrained(foreign: pd.DataFrame, output_dir: Path, label: str,
                      res: StageResult) -> None:
    """What the model did with images from classes it does not know.

    Not an accuracy figure -- there is no right answer available to it -- but
    worth knowing for percent cover: in the field these points exist and will
    be given *some* label, and this is the label they get.
    """
    path = output_dir / f"{label}_untrained_classes.csv"
    foreign.to_csv(path, index=False)
    res.outputs["untrained_classes_csv"] = str(path)

    by_class = foreign.groupby("true_label")
    log.info("=" * 60)
    log.info(f"{len(foreign):,} held-out image(s) belong to class(es) the model "
             f"was not trained on. Excluded from every accuracy figure.")
    for cls, group in by_class:
        called = (group["pred_label"].value_counts(normalize=True).head(3))
        log.info(f"  {cls} ({len(group):,} images) - the model called them: "
                 + ", ".join(f"{p} {v:.0%}" for p, v in called.items()))
    log.info(f"  Full list: {path}")
    log.info("=" * 60)

    summary = "; ".join(
        f"{cls} ({len(g):,}) -> mostly {g['pred_label'].mode().iat[0]}"
        for cls, g in by_class)
    res.say(f"Set aside {len(foreign):,} image(s) from class(es) the model was "
            f"not trained on: {summary}. See {path.name}.")


# --------------------------------------------------------------------------
#  Independence
# --------------------------------------------------------------------------

def _verify_independence(cfg, res: StageResult, progress, cancel) -> set[str]:
    """Hash the held-out set against the training set. Returns leaked paths."""
    log.info("Verifying that the held-out set is disjoint from the training "
             "data…")
    report = hashing.audit(cfg.train_dataset_dir, cfg.eval_dir,
                           progress=progress, cancel=cancel)
    leaks = [g for g in report.groups if g.category == "holdout_leak"]
    if not leaks:
        log.info("No held-out image appears in the training set.")
        return set()

    leaked = {o.path for g in leaks for o in g.occurrences if o.group == "holdout"}
    pct = 100.0 * len(leaked) / max(1, sum(
        len(v) for v in list_class_images(cfg.eval_dir, None).values()))

    log.error("=" * 70)
    log.error(f"HELD-OUT SET IS NOT INDEPENDENT: {len(leaked):,} image(s) "
              f"({pct:.1f}%) are byte-identical to training data.")
    log.error("=" * 70)
    for g in leaks[:10]:
        log.error(f"    {' = '.join(o.describe() for o in g.occurrences)}")
    if len(leaks) > 10:
        log.error(f"    …and {len(leaks) - 10:,} more")

    path = hashing.write_report_csv(
        report, Path(cfg.output_dir or cfg.eval_dir) / "holdout_leak_report.csv")
    res.outputs["leak_report"] = path
    res.outputs["leaked"] = len(leaked)

    res.blocked.append(
        f"{len(leaked):,} of the held-out images ({pct:.1f}%) are byte-identical "
        f"to training data, so any accuracy measured here would be inflated by "
        f"memorisation. Full list: {path}. Re-run stage 2 with this folder as "
        f"the held-out set and 'Quarantine leaked held-out patches' enabled, "
        f"then evaluate again.")
    return leaked


# --------------------------------------------------------------------------
#  Inference and reporting
# --------------------------------------------------------------------------

def _infer(model, class_files: dict, cfg, progress: ProgressCB, cancel) -> pd.DataFrame:
    """One row per image: path, true label, predicted label, confidence."""
    paths, truths = [], []
    for label, files in sorted(class_files.items()):
        paths.extend(files)
        truths.extend([label] * len(files))

    kwargs = dict(source=paths, imgsz=cfg.imgsz, batch=cfg.batch,
                  stream=True, verbose=False)
    if cfg.device:
        kwargs["device"] = cfg.device
    if cfg.fp16:
        # Current Ultralytics spells FP16 inference `quantize=16`; the older
        # `half=True` is no longer a recognised predictor argument.
        kwargs["quantize"] = 16
        log.info("FP16 inference enabled.")

    log.info(f"Running inference on {len(paths):,} image(s)…")
    rows = []
    for i, (path, truth, result) in enumerate(
            zip(paths, truths, model.predict(**kwargs)), start=1):
        try:
            idx = result.probs.top1
            rows.append({
                "filepath": path,
                "true_label": truth,
                "pred_label": result.names[idx],
                "confidence": float(result.probs.top1conf),
                "correct": result.names[idx] == truth,
            })
        except Exception as ex:
            log.warning(f"Inference failed on {path}: {ex}")
        if i % 128 == 0 or i == len(paths):
            progress(i / len(paths), f"evaluating… {i:,}/{len(paths):,}")
            check_cancelled(cancel, "inference")

    return pd.DataFrame(rows)


def build_accuracy_report(df: pd.DataFrame) -> pd.DataFrame:
    """Per-class accuracy, in the same columns as the original field report."""
    records = []
    for label, group in df.groupby("true_label"):
        correct = group[group["correct"]]
        incorrect = group[~group["correct"]]
        total = len(group)
        records.append({
            "label": label,
            "total_annotations": total,
            "number_accurate": len(correct),
            "number_incorrect": len(incorrect),
            "percent_accuracy": round(100 * len(correct) / total, 2) if total else 0.0,
            "avg_confidence_correct": (round(correct["confidence"].mean(), 4)
                                       if len(correct) else np.nan),
            "avg_confidence_incorrect": (round(incorrect["confidence"].mean(), 4)
                                         if len(incorrect) else np.nan),
        })
    return pd.DataFrame(records).sort_values("label").reset_index(drop=True)


def build_confusion_matrix(df: pd.DataFrame, model_classes: list) -> pd.DataFrame:
    """Rows: classes in the held-out ground truth. Columns: full taxonomy.

    The two axes are deliberately different sets. A prediction can land on any
    class the model knows, including one absent from this held-out set, and
    hiding those columns would hide exactly where a class is going wrong.
    Normalised per row.
    """
    rows = sorted(df["true_label"].unique())
    columns = sorted(set(model_classes) | set(df["pred_label"].unique()))

    matrix = pd.DataFrame(0.0, index=rows, columns=columns)
    for (truth, pred), n in df.groupby(["true_label", "pred_label"]).size().items():
        matrix.loc[truth, pred] = n

    return matrix.div(matrix.sum(axis=1).replace(0, np.nan), axis=0).fillna(0)


def plot_confusion_matrix(matrix: pd.DataFrame, title: str, out_path: str) -> None:
    import matplotlib
    matplotlib.use("Agg")     # a worker thread must not touch a GUI backend
    import matplotlib.pyplot as plt

    fig, ax = plt.subplots(figsize=(max(10, 0.35 * len(matrix.columns)),
                                    max(8, 0.35 * len(matrix.index))))
    im = ax.imshow(matrix.values, cmap="Blues", vmin=0, vmax=1, aspect="auto")

    ax.set_xticks(range(len(matrix.columns)))
    ax.set_xticklabels(matrix.columns, rotation=90, fontsize=7)
    ax.set_yticks(range(len(matrix.index)))
    ax.set_yticklabels(matrix.index, fontsize=7)

    for i in range(matrix.shape[0]):
        for j in range(matrix.shape[1]):
            value = matrix.values[i, j]
            if value > 0:
                ax.text(j, i, f"{value:.2f}", ha="center", va="center",
                        fontsize=5.5, color="white" if value > 0.5 else "black")

    ax.set_xlabel("Predicted label")
    ax.set_ylabel("Verified label")
    ax.set_title(title)
    fig.colorbar(im, ax=ax, fraction=0.03, pad=0.02)
    fig.tight_layout()
    fig.savefig(out_path, dpi=150)
    plt.close(fig)


def _write_reports(df: pd.DataFrame, model_classes: list, output_dir: Path,
                   label: str, res: StageResult) -> dict:
    report_path = output_dir / f"{label}_accuracy_report.csv"
    build_accuracy_report(df).to_csv(report_path, index=False)
    log.info(f"Accuracy report: {report_path}")

    detail_path = output_dir / f"{label}_predictions_detail.csv"
    df.to_csv(detail_path, index=False)
    log.info(f"Per-image predictions: {detail_path}")

    cm_path = output_dir / f"{label}_confusion_matrix.png"
    try:
        plot_confusion_matrix(
            build_confusion_matrix(df, model_classes),
            f"Normalised confusion matrix - held-out evaluation ({label})",
            str(cm_path))
        log.info(f"Confusion matrix: {cm_path}")
    except Exception as ex:
        res.warnings.append(f"Confusion matrix could not be plotted: {ex}")
        cm_path = None

    return {"report_path": str(report_path), "detail_path": str(detail_path),
            "confusion_matrix": str(cm_path) if cm_path else ""}


def _worst_classes(df: pd.DataFrame, n: int = 4, min_n: int = 30) -> list:
    """The weakest classes that have enough samples for the number to mean much."""
    out = []
    for label, group in df.groupby("true_label"):
        if len(group) >= min_n:
            out.append((label, 100.0 * group["correct"].sum() / len(group), len(group)))
    out.sort(key=lambda t: t[1])
    return out[:n]
