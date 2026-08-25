"""
Stage 3 - a merged dataset -> a trained classification model.

A thin wrapper around Ultralytics' ``model.train()``. It deliberately does not
invent training behaviour: a setting left blank is omitted from the call so
Ultralytics' own default applies, rather than this file having an opinion that
silently diverges from upstream.

Three things it does have an opinion about, all of them earned:

**Early stopping actually fires.** Ultralytics ships ``patience=100``; with
``epochs=100`` that can never trigger, so every previous run trained to the
last epoch whether or not it had stopped improving hours earlier. The default
here is 20. ``best.pt`` is the best epoch either way -- patience only decides
how long the run keeps going after it -- so this buys time, and it makes the
gap between "best epoch" and "last epoch" visible, which is what overfitting
looks like from the outside.

**Class folders must match across train and val.** Ultralytics requires it and
does not enforce it: given a mismatch it trains to chance accuracy without ever
raising. A real run was lost to this -- val loss climbing every epoch, top-1
stuck around 2-3%, exactly 1/n for the class count. Mismatched classes are
moved aside for the run and restored in a ``finally``.

**Online augmentation is retuned for centre-annotated patches.** See
``config.TRAIN_AUGMENTATION_OVERRIDES``.
"""

from __future__ import annotations

import shutil
import time
from pathlib import Path

from ..config import TRAIN_AUGMENTATION_OVERRIDES
from ..fsutil import IMG_EXTS, inventory_table, looks_like_split_dataset
from ..logging_setup import get_logger
from ..progress import ProgressCB, check_cancelled
from . import StageResult, guard

log = get_logger("train")

#: Mismatched class folders are parked here for the duration of a run.
EXCLUDED_DIRNAME = "_excluded_from_training"


# --------------------------------------------------------------------------
#  Dataset validation
# --------------------------------------------------------------------------

def validate_dataset(data_dir: str) -> dict:
    """Per-class counts for both splits, plus anything structurally wrong."""
    root = Path(data_dir)
    report: dict = {"train": {}, "val": {}, "problems": [],
                    "only_train": [], "only_val": []}

    for split in ("train", "val"):
        split_dir = root / split
        if not split_dir.is_dir():
            report["problems"].append(f"Missing '{split}/' folder under {data_dir}")
            continue
        for class_dir in sorted(split_dir.iterdir()):
            if not class_dir.is_dir() or class_dir.name.startswith("_"):
                continue
            report[split][class_dir.name] = len(
                [f for f in class_dir.iterdir() if f.suffix.lower() in IMG_EXTS])

    only_train = sorted(set(report["train"]) - set(report["val"]))
    only_val = sorted(set(report["val"]) - set(report["train"]))
    report["only_train"] = only_train
    report["only_val"] = only_val

    if only_train:
        report["problems"].append(
            f"Classes in train but not val: {only_train}")
    if only_val:
        report["problems"].append(
            f"Classes in val but not train: {only_val}")
    empty = [c for c, n in report["train"].items() if n == 0]
    if empty:
        report["problems"].append(f"Empty train class folders: {empty}")

    return report


def exclude_mismatched(data_dir: str, only_train: list, only_val: list) -> list:
    """Move single-split class folders aside. Returns (src, dst) pairs."""
    moved = []
    backup = Path(data_dir) / EXCLUDED_DIRNAME
    for split, classes in (("train", only_train), ("val", only_val)):
        for cls in classes:
            src = Path(data_dir) / split / cls
            if not src.is_dir():
                continue
            dst = backup / split / cls
            dst.parent.mkdir(parents=True, exist_ok=True)
            if dst.exists():
                shutil.rmtree(dst, ignore_errors=True)
            shutil.move(str(src), str(dst))
            moved.append((str(src), str(dst)))
            log.info(f"  Excluded '{cls}' ({split}-only) for this run")
    return moved


def restore_excluded(moved: list, data_dir: str | None = None) -> None:
    """Put every excluded folder back. Always call from a ``finally``."""
    for src, dst in moved:
        if Path(dst).is_dir():
            Path(src).parent.mkdir(parents=True, exist_ok=True)
            try:
                shutil.move(dst, src)
            except OSError as ex:
                log.error(f"Could not restore {src}: {ex} - the folder is still "
                          f"at {dst} and must be moved back by hand.")
    if data_dir:
        # Tidy the now-empty scaffolding, but never recursively: anything left
        # inside it is a folder that failed to restore, and deleting that would
        # destroy training data.
        backup = Path(data_dir) / EXCLUDED_DIRNAME
        for d in sorted(backup.rglob("*"), reverse=True) + [backup]:
            try:
                d.rmdir()
            except OSError:
                pass


# --------------------------------------------------------------------------
#  The stage
# --------------------------------------------------------------------------

def run(cfg, *, progress: ProgressCB | None = None, cancel=None) -> StageResult:
    started = time.time()
    return guard(lambda: _run(cfg, progress, cancel), started)


def _run(cfg, progress, cancel) -> StageResult:
    res = StageResult(preview=cfg.validate_only)

    if not cfg.data_dir:
        res.errors.append("No dataset folder selected.")
        return res
    if not Path(cfg.data_dir).is_dir():
        res.errors.append(f"Dataset folder not found: {cfg.data_dir}")
        return res
    if not looks_like_split_dataset(cfg.data_dir):
        res.errors.append(
            f"{cfg.data_dir} has no train/ and val/ folders. Training needs a "
            f"split dataset - run stage 2 first.")
        return res

    report = validate_dataset(cfg.data_dir)
    for line in inventory_table(
            {c: [None] * n for c, n in report["train"].items()},
            {c: [None] * n for c, n in report["val"].items()}):
        log.info(line)

    n_classes = len(set(report["train"]) | set(report["val"]))
    res.outputs.update({
        "classes": n_classes,
        "train_images": sum(report["train"].values()),
        "val_images": sum(report["val"].values()),
        "problems": report["problems"],
    })

    if report["problems"]:
        for p in report["problems"]:
            log.warning(f"  {p}")
    else:
        log.info("Dataset structure is consistent.")

    if progress:
        progress(0.02, "dataset validated")

    if cfg.validate_only:
        res.say(f"{res.outputs['train_images']:,} train / "
                f"{res.outputs['val_images']:,} val across {n_classes} class(es).")
        if report["only_train"] or report["only_val"]:
            mismatched = sorted(set(report["only_train"]) | set(report["only_val"]))
            res.say(f"{len(mismatched)} class(es) exist in only one split: "
                    f"{mismatched}")
            res.say("Ultralytics trains to chance accuracy on a mismatch, so "
                    "these will be set aside for the run and restored after."
                    if cfg.exclude_mismatched else
                    "Enable 'Set aside mismatched classes' or training will be "
                    "silently worthless.")
        else:
            res.say("Class folders match across train and val.")
        res.say(f"Early stopping: patience={cfg.patience} epoch(s) of "
                f"{cfg.epochs}.")
        res.say("Nothing trained - clear 'Validate only' to start.")
        return res

    if not cfg.exclude_mismatched and (report["only_train"] or report["only_val"]):
        res.blocked.append(
            "Class folders differ between train and val, and 'Set aside "
            "mismatched classes' is off. Ultralytics does not raise on this - "
            "it trains to chance accuracy. Enable the option, or fix the "
            "dataset, before training.")
        return res

    check_cancelled(cancel, "validation")
    return _train(cfg, report, res, progress, cancel)


def _train(cfg, report, res: StageResult, progress, cancel) -> StageResult:
    from ultralytics import YOLO

    moved = []
    if cfg.exclude_mismatched and (report["only_train"] or report["only_val"]):
        moved = exclude_mismatched(cfg.data_dir, report["only_train"],
                                   report["only_val"])
        excluded = sorted(set(report["only_train"]) | set(report["only_val"]))
        res.outputs["excluded_classes"] = excluded
        res.warnings.append(
            f"{len(excluded)} class(es) set aside for this run (present in only "
            f"one split): {excluded}. They are restored afterwards, but the "
            f"model will not know them.")

    kwargs = {
        "data": cfg.data_dir,
        "epochs": cfg.epochs,
        "imgsz": cfg.imgsz,
        "seed": cfg.seed,
        "patience": cfg.patience,
        **TRAIN_AUGMENTATION_OVERRIDES,
    }
    for key, value in (("batch", cfg.batch), ("device", cfg.device),
                       ("project", cfg.project), ("name", cfg.name)):
        if value not in (None, ""):
            kwargs[key] = value

    try:
        log.info(f"Loading base model: {cfg.model}")
        model = YOLO(cfg.model)

        log.info(f"Training: {cfg.epochs} epoch(s), imgsz={cfg.imgsz}, "
                 f"patience={cfg.patience}, seed={cfg.seed}")
        log.info("Ultralytics prints its own per-epoch table below; the "
                 "progress bar tracks the run as a whole.")
        if progress:
            progress(0.05, f"training - up to {cfg.epochs} epochs…")

        _attach_epoch_progress(model, cfg, progress, cancel)
        results = model.train(**kwargs)

        save_dir = (getattr(results, "save_dir", None)
                    or getattr(getattr(model, "trainer", None), "save_dir", None))
        _collect_outputs(model, save_dir, cfg, res)
    finally:
        if moved:
            log.info("Restoring the class folders set aside for this run…")
            restore_excluded(moved, cfg.data_dir)

    if progress:
        progress(1.0, "training complete")
    return res


def _attach_epoch_progress(model, cfg, progress: ProgressCB | None, cancel) -> None:
    """Drive the progress bar and honour Stop, once per epoch.

    Ultralytics does not take a progress callback, but it does fire training
    callbacks, and ``on_fit_epoch_end`` is the natural per-epoch tick. It is
    also the only place a long run can notice that Stop was pressed -- so
    cancellation lands at an epoch boundary, after ``best.pt`` has been written
    for that epoch rather than in the middle of one.
    """
    if progress is None and cancel is None:
        return

    def on_epoch_end(trainer) -> None:
        epoch = getattr(trainer, "epoch", 0) + 1
        total = getattr(trainer, "epochs", cfg.epochs) or cfg.epochs
        if progress:
            # stopper.best_epoch is already 1-based -- Ultralytics calls the
            # stopper with `epoch + 1` -- unlike trainer.epoch, which is not.
            best = getattr(getattr(trainer, "stopper", None), "best_epoch", None)
            note = f"epoch {epoch}/{total}"
            if best:
                note += f"  (best so far: epoch {best})"
            # Cap below 1.0: validation and export still follow the last epoch.
            progress(min(0.95, 0.05 + 0.9 * epoch / max(1, total)), note)
        if cancel is not None and cancel.is_set():
            # Ultralytics polls this between epochs and shuts down cleanly.
            trainer.stop = True
            log.warning("Stop requested - finishing after this epoch. "
                        "best.pt is already saved.")

    try:
        model.add_callback("on_fit_epoch_end", on_epoch_end)
    except Exception as ex:      # a missing callback hook must not stop training
        log.debug(f"Could not attach epoch callback: {ex}")


def _collect_outputs(model, save_dir, cfg, res: StageResult) -> None:
    """Record where the weights landed and how the run actually went."""
    if not save_dir:
        res.warnings.append("Training finished but Ultralytics reported no "
                            "output folder; find the weights under runs/.")
        return

    save_dir = Path(save_dir)
    weights = save_dir / "weights" / "best.pt"
    res.outputs["save_dir"] = str(save_dir)
    res.outputs["weights"] = str(weights) if weights.is_file() else ""

    trainer = getattr(model, "trainer", None)
    stopper = getattr(trainer, "stopper", None)
    best_epoch = getattr(stopper, "best_epoch", None)
    last_epoch = getattr(trainer, "epoch", None)

    metrics = {}
    try:
        metrics = {k: float(v) for k, v in (trainer.metrics or {}).items()}
    except Exception:
        pass
    res.outputs["metrics"] = metrics

    log.info("=" * 60)
    log.info("Training complete.")
    log.info(f"Results: {save_dir}")
    if weights.is_file():
        log.info(f"Best weights: {weights}")
    for k, v in metrics.items():
        log.info(f"  {k}: {v:.4f}")
    log.info("=" * 60)

    res.say(f"Model trained. Best weights: {weights}")

    top1 = metrics.get("metrics/accuracy_top1")
    if top1 is not None:
        res.say(f"Validation top-1: {top1 * 100:.2f}%")

    if best_epoch and last_epoch is not None:
        # best_epoch is 1-based (the stopper is called with `epoch + 1`);
        # trainer.epoch is the 0-based loop counter.
        ran, best = int(last_epoch) + 1, int(best_epoch)
        res.outputs.update({"best_epoch": best, "epochs_run": ran})
        res.say(f"Best epoch {best} of {ran} run (limit {cfg.epochs}).")
        if ran < cfg.epochs:
            res.say(f"Early stopping fired - no improvement for "
                    f"{cfg.patience} epochs after epoch {best}.")
        elif best < ran - cfg.patience // 2:
            res.warnings.append(
                f"The run used all {cfg.epochs} epochs but stopped improving at "
                f"epoch {best}. best.pt is from epoch {best}, so the model is "
                f"fine - but the remaining {ran - best} epochs were wasted "
                f"time. A lower patience would end the next run sooner.")
        elif best >= ran - 1:
            res.warnings.append(
                f"Still improving at the last epoch ({best}/{cfg.epochs}). The "
                f"model may be undertrained - consider raising Epochs.")

    res.say("Validation accuracy is measured on val/, which came from the same "
            "annotation pipeline as train/. Stage 4 is the number to trust.")
