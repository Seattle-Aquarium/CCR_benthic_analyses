"""
Headless access to the same four stages.

Every stage runs identically here and in the GUI -- same config dataclass, same
``run()``. Saved settings are read from and written to the same file, so the
two can be mixed: set a stage up in the GUI, then run it overnight from a
terminal, or the reverse.

    python -m classifier.cli inventory  --dataset DS1 --dataset DS2 --holdout H
    python -m classifier.cli audit      --dataset DS --holdout H
    python -m classifier.cli extract    --csv A.csv --out DS [--commit]
    python -m classifier.cli balance    --dataset DS --out MERGED --cap 2000 --commit
    python -m classifier.cli train      --data MERGED --epochs 100 --commit
    python -m classifier.cli evaluate   --model best.pt --holdout H --commit

Every stage previews by default; ``--commit`` is what makes it write.
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

from .config import PipelineState, default_state_path
from .logging_setup import configure, get_logger
from .stages import StageResult

log = get_logger("cli")


def _progress(fraction: float, message: str = "") -> None:
    if message:
        sys.stderr.write(f"\r  {fraction * 100:5.1f}%  {message[:88]:<88}")
        sys.stderr.flush()


def _use_utf8() -> None:
    """Let both streams carry the non-ASCII characters the messages contain.

    A Windows console is cp1252 by default, which turns an ellipsis or an arrow
    into a replacement glyph. ``errors="replace"`` keeps that cosmetic rather
    than fatal on a terminal that genuinely cannot.
    """
    for stream in (sys.stdout, sys.stderr):
        try:
            stream.reconfigure(encoding="utf-8", errors="replace")
        except (AttributeError, ValueError, OSError):
            pass


def _report(result: StageResult) -> int:
    sys.stderr.write("\r" + " " * 100 + "\r")
    print()
    print(result.summary())
    print(f"({result.elapsed_s / 60:.1f} min)")
    if result.blocked:
        return 2
    return 1 if result.errors or result.cancelled else 0


# --------------------------------------------------------------------------
#  Commands
# --------------------------------------------------------------------------

def cmd_extract(args, state: PipelineState) -> int:
    from .stages import extract

    cfg = state.extract
    if args.csv:
        cfg.annotation_csvs = list(args.csv)
    if args.out:
        cfg.output_dir = args.out
    if args.no_split is not None:
        cfg.no_split = args.no_split
    for name in ("val_frac", "min_val_count", "jpeg_quality", "seed"):
        if getattr(args, name, None) is not None:
            setattr(cfg, name, getattr(args, name))
    if args.exclude is not None:
        cfg.exclude_labels = args.exclude
    if args.remap:
        cfg.path_remaps = list(args.remap)
    cfg.dry_run = not args.commit

    result = extract.run(cfg, progress=_progress)
    if result.committed:
        state.advance_from_extract(cfg.output_dir, cfg.no_split)
    state.save(args.state or default_state_path())
    return _report(result)


def cmd_balance(args, state: PipelineState) -> int:
    from .stages import balance

    cfg = state.balance
    if args.dataset:
        cfg.datasets = list(args.dataset)
    if args.out:
        cfg.output_dir = args.out
    if args.holdout is not None:
        cfg.holdout_dir = args.holdout
    for name in ("cap", "floor", "seed"):
        if getattr(args, name, None) is not None:
            setattr(cfg, name, getattr(args, name))
    if args.class_cap:
        cfg.class_caps.update(dict(args.class_cap))
    if args.label_authority:
        cfg.label_authority = args.label_authority
    if args.no_quarantine:
        cfg.quarantine_holdout_leaks = False
    cfg.inventory_only = not args.commit

    result = balance.run(cfg, progress=_progress)
    if result.committed:
        state.advance_from_balance(cfg.output_dir)
    state.save(args.state or default_state_path())
    return _report(result)


def cmd_train(args, state: PipelineState) -> int:
    from .stages import train

    cfg = state.train
    if args.data:
        cfg.data_dir = args.data
    for name in ("model", "epochs", "imgsz", "seed", "batch", "device",
                 "patience", "project", "name"):
        if getattr(args, name, None) is not None:
            setattr(cfg, name, getattr(args, name))
    cfg.validate_only = not args.commit

    result = train.run(cfg, progress=_progress)
    if result.committed and result.outputs.get("weights"):
        state.advance_from_train(result.outputs["weights"])
    state.save(args.state or default_state_path())
    return _report(result)


def cmd_evaluate(args, state: PipelineState) -> int:
    from .stages import evaluate

    cfg = state.evaluate
    if args.model:
        cfg.model_path = args.model
    if args.holdout:
        cfg.eval_dir = args.holdout
    if args.train_data is not None:
        cfg.train_dataset_dir = args.train_data
    for name in ("output_dir", "dataset_label", "imgsz", "batch", "device",
                 "baseline_accuracy"):
        if getattr(args, name, None) is not None:
            setattr(cfg, name, getattr(args, name))
    if args.fp16:
        cfg.fp16 = True
    if args.no_verify:
        cfg.verify_independence = False
    cfg.preview_only = not args.commit

    result = evaluate.run(cfg, progress=_progress)
    state.save(args.state or default_state_path())
    return _report(result)


def cmd_audit(args, state: PipelineState) -> int:
    """Hash a dataset (and optionally a holdout) and report duplicates.

    Read-only. The same check the balance and evaluate stages run, available on
    its own for when you just want to know.
    """
    from . import hashing

    dataset = args.dataset or state.last_dataset_dir
    holdout = args.holdout if args.holdout is not None else state.last_holdout_dir
    if not dataset and not holdout:
        print("Nothing to audit: pass --dataset and/or --holdout.")
        return 1

    report = hashing.audit(dataset or None, holdout or None, progress=_progress)
    sys.stderr.write("\r" + " " * 100 + "\r")
    print()
    for line in report.summary_lines():
        print(line)

    if args.out:
        print(f"\nFull report: {hashing.write_report_csv(report, args.out)}")
    return 2 if report.blocking else 0


def cmd_inventory(args, state: PipelineState) -> int:
    """Per-class counts across one or more datasets. Read-only."""
    from .fsutil import inventory_table, list_class_images, merge_file_lists

    datasets = args.dataset or ([state.last_dataset_dir]
                                if state.last_dataset_dir else [])
    if not datasets:
        print("Nothing to inventory: pass --dataset.")
        return 1

    train = merge_file_lists(*(list_class_images(d, "train") for d in datasets))
    val = merge_file_lists(*(list_class_images(d, "val") for d in datasets))
    if not train and not val:
        flat = merge_file_lists(*(list_class_images(d, None) for d in datasets))
        if not flat:
            print("No images found.")
            return 1
        print("Flat layout (no train/val) - a held-out set.")
        train = flat

    for line in inventory_table(train, val):
        print(line)
    return 0


# --------------------------------------------------------------------------
#  Argument parsing
# --------------------------------------------------------------------------

def _class_cap(text: str) -> tuple[str, int]:
    name, _, value = text.partition("=")
    if not name or not value:
        raise argparse.ArgumentTypeError("expected CLASS=COUNT")
    return name.strip(), int(value)


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        prog="python -m classifier.cli",
        description="CCR benthic classification pipeline, headless.",
        epilog="Every stage previews by default; --commit makes it write.")
    p.add_argument("--state", help="settings file (default: next to the package)")
    sub = p.add_subparsers(dest="command", required=True)

    e = sub.add_parser("extract", help="annotation CSVs -> patch dataset")
    e.add_argument("--csv", action="append", help="repeatable")
    e.add_argument("--out")
    e.add_argument("--no-split", action="store_true", default=None,
                   help="flat <Label>/ output for a held-out set")
    e.add_argument("--val-frac", type=float, dest="val_frac")
    e.add_argument("--min-val-count", type=int, dest="min_val_count")
    e.add_argument("--jpeg-quality", type=int, dest="jpeg_quality")
    e.add_argument("--seed", type=int)
    e.add_argument("--exclude", action="append", help="label to drop; repeatable")
    e.add_argument("--remap", action="append", help="OLD=>NEW; repeatable")
    e.add_argument("--commit", action="store_true")
    e.set_defaults(func=cmd_extract)

    b = sub.add_parser("balance", help="merge, deduplicate and balance datasets")
    b.add_argument("--dataset", action="append", help="repeatable, order matters")
    b.add_argument("--out")
    b.add_argument("--holdout", help="audited against, never merged in")
    b.add_argument("--cap", type=int)
    b.add_argument("--floor", type=int)
    b.add_argument("--class-cap", action="append", type=_class_cap,
                   dest="class_cap", metavar="CLASS=COUNT")
    b.add_argument("--seed", type=int)
    b.add_argument("--label-authority", metavar="DATASET",
                   help="when identical patches disagree on a label, keep the "
                        "copy from this dataset (must be one of --dataset)")
    b.add_argument("--no-quarantine", action="store_true",
                   help="report held-out leaks without moving them aside")
    b.add_argument("--commit", action="store_true")
    b.set_defaults(func=cmd_balance)

    t = sub.add_parser("train", help="train a classification model")
    t.add_argument("--data")
    t.add_argument("--model")
    t.add_argument("--epochs", type=int)
    t.add_argument("--patience", type=int, help="epochs without improvement")
    t.add_argument("--imgsz", type=int)
    t.add_argument("--batch", type=int)
    t.add_argument("--device")
    t.add_argument("--seed", type=int)
    t.add_argument("--project")
    t.add_argument("--name")
    t.add_argument("--commit", action="store_true")
    t.set_defaults(func=cmd_train)

    v = sub.add_parser("evaluate", help="score a model on a held-out set")
    v.add_argument("--model")
    v.add_argument("--holdout")
    v.add_argument("--train-data", dest="train_data",
                   help="training dataset, for the independence check")
    v.add_argument("--out", dest="output_dir")
    v.add_argument("--label", dest="dataset_label")
    v.add_argument("--imgsz", type=int)
    v.add_argument("--batch", type=int)
    v.add_argument("--device")
    v.add_argument("--baseline", type=float, dest="baseline_accuracy")
    v.add_argument("--fp16", action="store_true")
    v.add_argument("--no-verify", action="store_true",
                   help="skip the independence check (not recommended)")
    v.add_argument("--commit", action="store_true")
    v.set_defaults(func=cmd_evaluate)

    a = sub.add_parser("audit", help="find byte-identical duplicates")
    a.add_argument("--dataset")
    a.add_argument("--holdout")
    a.add_argument("--out", help="write the full report here")
    a.set_defaults(func=cmd_audit)

    i = sub.add_parser("inventory", help="per-class counts")
    i.add_argument("--dataset", action="append")
    i.set_defaults(func=cmd_inventory)

    return p


def main(argv=None) -> int:
    _use_utf8()
    args = build_parser().parse_args(argv)
    configure(Path(__file__).resolve().parent.parent / "classifier_log.txt")
    state = PipelineState.load(args.state or default_state_path())
    return args.func(args, state)


if __name__ == "__main__":
    raise SystemExit(main())
