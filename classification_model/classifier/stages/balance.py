"""
Stage 2 - several patch datasets -> one deduplicated, balanced training set.

This is where the pipeline's integrity guarantee lives. Three things happen, in
this order, and the order matters:

**1. Deduplicate by content.** Every patch in every input dataset is hashed and
grouped by digest. The previous implementation compared filenames, which misses
the case this pipeline actually produces: the same annotation point extracted in
two different batches gets two different filenames and two identical files.
Exactly one copy of each group survives, so the merged set holds no identical
pairs inside train, inside val, or across the two. A train/val leak is resolved
in favour of train, because a val image the model trained on measures nothing.

When the identical copies disagree about the *label*, one annotation is wrong
and no rule can tell which. ``label_authority`` names the dataset to believe;
its copy is the one kept, and a kept copy carries its own label. Left unset the
winner falls out of path order, which is arbitrary -- so the stage says so
rather than letting it pass silently.

None of this modifies an input dataset. A "dropped" file is one the merge did
not copy; the merged set is built fresh in the output folder, next to the
duplicate report describing every group and how it was settled.

**2. Check the held-out set.** Patches identical to something in the held-out
evaluation set are leaks, and they are resolved by moving the *holdout* copy
aside -- never the training copy. Dropping the training copy instead would
quietly shrink the training set in a way that flatters the evaluation.

**3. Balance.** Oversized classes are undersampled to ``cap``, undersized ones
augmented up to ``floor``. Only ``train/`` is touched. ``val/`` is merged
exactly as it is, on purpose: capping or augmenting it would make each round's
validation number incomparable with the last.
"""

from __future__ import annotations

import random
import shutil
import time
from pathlib import Path

from .. import hashing, imaging
from ..fsutil import (inventory_table, list_class_dirs, list_class_images,
                      merge_file_lists, unique_dest)
from ..logging_setup import get_logger
from ..progress import ProgressCB, Stages, check_cancelled
from . import StageResult, guard

log = get_logger("balance")

#: Leaked held-out patches are moved here, inside the holdout folder. Moved,
#: never deleted -- if the leak turns out to be a mistake in the audit rather
#: than in the data, nothing has been lost.
QUARANTINE_DIRNAME = "_quarantined_leaked"

DUPLICATE_REPORT = "duplicate_patches_report.csv"


def run(cfg, *, progress: ProgressCB | None = None, cancel=None) -> StageResult:
    started = time.time()
    return guard(lambda: _run(cfg, progress, cancel), started)


def _run(cfg, progress, cancel) -> StageResult:
    res = StageResult(preview=cfg.inventory_only)
    st = Stages(progress).plan(scan=3, hash=27, plan_=5, write=65)

    datasets = [d for d in cfg.datasets if str(d).strip()]
    if not datasets:
        res.errors.append("No input dataset selected.")
        return res
    for d in datasets:
        if not Path(d).is_dir():
            res.errors.append(f"Dataset folder not found: {d}")
            return res
    if not cfg.inventory_only and not cfg.output_dir:
        res.errors.append("No output folder selected.")
        return res
    if cfg.output_dir and any(_is_inside(cfg.output_dir, d) or _is_inside(d, cfg.output_dir)
                              for d in datasets):
        res.errors.append(
            "The output folder is inside one of the input datasets (or vice "
            "versa). Choose an output folder outside them, so the merge cannot "
            "read files it is in the middle of writing.")
        return res

    # ---- scan ------------------------------------------------------
    log.info(f"Merging {len(datasets)} dataset(s):")
    per_dataset = []
    for i, d in enumerate(datasets):
        train = list_class_images(d, "train")
        val = list_class_images(d, "val")
        per_dataset.append({"dir": d, "train": train, "val": val})
        log.info(f"  [{i + 1}] {Path(d).name}: "
                 f"{sum(len(v) for v in train.values()):,} train, "
                 f"{sum(len(v) for v in val.values()):,} val")
        if not train and not val:
            res.warnings.append(
                f"{Path(d).name} has no train/ or val/ images - is it a flat "
                f"held-out folder? Those belong in stage 4, not here.")

    _report_class_folders(datasets, res)
    st.finish("scan", "datasets scanned")

    # ---- hash ------------------------------------------------------
    occurrences: list[hashing.Occurrence] = []
    owner: dict[str, str] = {}          # path -> the dataset folder it came from
    for d in per_dataset:
        for split in ("train", "val"):
            for label, files in d[split].items():
                for f in files:
                    occurrences.append(hashing.Occurrence(split, label, f))
                    owner[f] = d["dir"]
    if cfg.holdout_dir:
        if not Path(cfg.holdout_dir).is_dir():
            res.warnings.append(f"Held-out folder not found: {cfg.holdout_dir}")
        else:
            for label, files in list_class_images(cfg.holdout_dir, None).items():
                occurrences.extend(
                    hashing.Occurrence("holdout", label, f) for f in files)

    if not occurrences:
        res.errors.append("No images found in the selected dataset(s).")
        return res

    log.info(f"Hashing {len(occurrences):,} patch(es) to find byte-identical "
             f"duplicates…")
    by_digest = hashing.index(occurrences, progress=st.sub("hash"), cancel=cancel)
    report = hashing.DuplicateReport(hashing.duplicate_groups(by_digest),
                                     total_files=len(occurrences))
    for line in report.summary_lines():
        log.info(line)
    st.finish("hash", "duplicate audit complete")

    # ---- decide what to drop ---------------------------------------
    authority = cfg.label_authority.strip() or None
    if authority and not any(_same_dir(authority, d) for d in datasets):
        res.warnings.append(
            f"Label authority '{Path(authority).name}' is not one of the input "
            f"datasets, so it decides nothing. Conflicts fall back to path order.")
        authority = None
    elif authority:
        authority = next(d for d in datasets if _same_dir(authority, d))

    plan = hashing.choose_drops(report.groups, owner=owner, authority=authority)
    group_of = {o.path: o.group for o in occurrences}
    holdout_drops = {p for p in plan.drops if group_of.get(p) == "holdout"}
    dataset_drops = plan.drops - holdout_drops

    _log_drop_plan(plan, authority)

    conflicts = report.conflicts
    if conflicts:
        log.warning(f"{len(conflicts):,} identical patch(es) are filed under "
                    f"conflicting labels - the same image cannot be two "
                    f"classes, so one annotation is wrong:")
        for g in conflicts[:15]:
            log.warning(f"    {sorted(g.labels)}  {g.occurrences[0].describe()}")
        if len(conflicts) > 15:
            log.warning(f"    …and {len(conflicts) - 15:,} more (see "
                        f"{DUPLICATE_REPORT})")
        if authority:
            res.warnings.append(
                f"{len(conflicts):,} byte-identical patch(es) carry conflicting "
                f"class labels. {plan.resolved_by_authority:,} took the label "
                f"from {Path(authority).name}"
                + (f"; {plan.resolved_by_fallback:,} had no copy in that "
                   f"dataset and fell back to path order"
                   if plan.resolved_by_fallback else "")
                + ". The disagreement is still in your annotation CSVs.")
        else:
            res.warnings.append(
                f"{len(conflicts):,} byte-identical patch(es) carry conflicting "
                f"class labels, and no label authority is set - which label "
                f"survives is arbitrary. Set one to decide this deliberately.")

    leaks = [g for g in report.groups if g.category == "holdout_leak"]
    if leaks:
        n = len(holdout_drops)
        log.warning("=" * 66)
        log.warning(f"HELD-OUT LEAK: {n:,} held-out patch(es) are byte-identical "
                    f"to training data.")
        log.warning("Any accuracy measured on those images is memorisation, "
                    "not generalisation.")
        log.warning("=" * 66)
        res.warnings.append(
            f"{n:,} held-out patch(es) are byte-identical to training data."
            + (" They will be quarantined out of the held-out folder."
               if cfg.quarantine_holdout_leaks
               else " Enable 'Quarantine leaked held-out patches' to remove "
                    "them, or stage 4 will refuse to report a number."))

    res.outputs["duplicates"] = report.counts()
    res.outputs["holdout_leaks"] = len(holdout_drops)

    # ---- merged inventory ------------------------------------------
    merged_train = _merge_dropping(per_dataset, "train", dataset_drops)
    merged_val = _merge_dropping(per_dataset, "val", dataset_drops)
    for line in inventory_table(merged_train, merged_val):
        log.info(line)

    res.outputs.update({
        "classes": len(set(merged_train) | set(merged_val)),
        "train_available": sum(len(v) for v in merged_train.values()),
        "val_available": sum(len(v) for v in merged_val.values()),
        "dropped_duplicates": len(dataset_drops),
    })
    st.finish("plan_", "merge planned")

    if cfg.inventory_only:
        if cfg.output_dir and report.groups:
            path = hashing.write_report_csv(
                report, Path(cfg.output_dir) / DUPLICATE_REPORT)
            res.outputs["duplicate_report"] = path
            res.say(f"Duplicate audit written to {path}")
            res.say("(that report is the only thing this preview wrote.)")
        res.say(f"{res.outputs['train_available']:,} train / "
                f"{res.outputs['val_available']:,} val across "
                f"{res.outputs['classes']} class(es), after removing "
                f"{len(dataset_drops):,} byte-identical duplicate(s).")
        counts = sorted(len(v) for v in merged_train.values())
        if counts:
            res.say(f"Train class sizes: min={counts[0]:,}, "
                    f"median={counts[len(counts) // 2]:,}, max={counts[-1]:,} "
                    f"- pick cap and floor from these.")
        res.say("Nothing written - clear 'Inventory only' to merge.")
        return res

    # ---- write -----------------------------------------------------
    if cfg.quarantine_holdout_leaks and holdout_drops:
        moved = _quarantine(holdout_drops, cfg.holdout_dir)
        res.outputs["quarantined"] = moved
        log.info(f"Quarantined {moved:,} leaked held-out patch(es) to "
                 f"{Path(cfg.holdout_dir) / QUARANTINE_DIRNAME}")

    out = Path(cfg.output_dir)
    out.mkdir(parents=True, exist_ok=True)
    report_path = hashing.write_report_csv(report, out / DUPLICATE_REPORT)
    res.outputs["duplicate_report"] = report_path

    written = _write_dataset(merged_train, merged_val, cfg, st.sub("write"), cancel)
    res.outputs.update(written)
    st.finish("write", "merged dataset written")

    res.say(f"Merged dataset written to {cfg.output_dir}")
    res.say(f"train: {written['train_written']:,} image(s) "
            f"({written['augmented']:,} augmented up to the floor)")
    res.say(f"val:   {written['val_written']:,} image(s), merged unchanged")
    for line in _drop_summary(plan):
        res.say(line)
    res.say(f"Duplicate audit written to {report_path}")
    if written["capped"]:
        res.say(f"Capped: {', '.join(written['capped'])}")
    if written["floored"]:
        res.say(f"Augmented to floor: {', '.join(written['floored'])}")
    if holdout_drops and not cfg.quarantine_holdout_leaks:
        res.say(f"{len(holdout_drops):,} held-out leak(s) left in place - "
                f"stage 4 will refuse to report until they are resolved.")
    return res


# --------------------------------------------------------------------------
#  Helpers
# --------------------------------------------------------------------------

#: How each duplicate category is resolved, in the operator's terms. The merge
#: never edits an input dataset -- a "dropped" file is simply one the merge did
#: not copy across. The single exception is the held-out quarantine, which is a
#: move, and is what the checkbox on the panel controls.
_DROP_WORDING = {
    "within_train": "redundant copies inside train (one kept)",
    "within_val": "redundant copies inside val (one kept)",
    "train_val_leak": "val copies that were also in train (train copy kept)",
    "within_holdout": "redundant copies inside the held-out set",
    "holdout_leak": "held-out copies that were also in training (training copy kept)",
}


def _log_drop_plan(plan, authority: str | None) -> None:
    """Say which copy won, per category, before anything is written."""
    if not plan.drops:
        log.info("No byte-identical duplicates to resolve.")
        return
    log.info("Resolving duplicates - one copy of each is kept:")
    for cat, n in sorted(plan.by_category.items(),
                         key=lambda kv: -kv[1]):
        log.info(f"  {n:>7,}  {_DROP_WORDING.get(cat, cat)}")
    if authority:
        log.info(f"  Label conflicts decided by: {Path(authority).name}")


def _drop_summary(plan) -> list[str]:
    """The same breakdown, condensed for the run summary."""
    if not plan.drops:
        return ["No byte-identical duplicates found - nothing to remove."]
    out = [f"Removed {len(plan.drops):,} byte-identical duplicate(s), "
           f"keeping one copy of each:"]
    out += [f"   {n:,} {_DROP_WORDING.get(cat, cat)}"
            for cat, n in sorted(plan.by_category.items(), key=lambda kv: -kv[1])]
    return out


def _same_dir(a: str, b: str) -> bool:
    try:
        return Path(a).resolve() == Path(b).resolve()
    except OSError:
        return str(a).strip() == str(b).strip()


def _is_inside(child: str, parent: str) -> bool:
    try:
        Path(child).resolve().relative_to(Path(parent).resolve())
        return True
    except (ValueError, OSError):
        return False


def _merge_dropping(per_dataset: list[dict], split: str,
                    drops: set[str]) -> dict[str, list[str]]:
    """Concatenate one split across datasets, minus the duplicate copies."""
    kept = []
    for d in per_dataset:
        kept.append({cls: [f for f in files if f not in drops]
                     for cls, files in d[split].items()})
    merged = merge_file_lists(*kept)
    return {cls: files for cls, files in merged.items() if files}


def _report_class_folders(datasets: list[str], res: StageResult) -> None:
    """Flag classes that appear in some datasets but not others.

    Informational, not an error -- a newly annotated class legitimately exists
    in only the newest batch -- but easy to miss otherwise.
    """
    if len(datasets) < 2:
        return
    for split in ("train", "val"):
        seen = [(d, list_class_dirs(d, split)) for d in datasets]
        everything = set().union(*(s for _, s in seen)) if seen else set()
        for d, classes in seen:
            missing = sorted(everything - classes)
            if missing:
                log.info(f"  [{split}] {Path(d).name} has no: {missing}")


def _quarantine(paths: set[str], holdout_dir: str) -> int:
    """Move leaked held-out patches aside, preserving their class folder."""
    root = Path(holdout_dir) / QUARANTINE_DIRNAME
    moved = 0
    for p in sorted(paths):
        src = Path(p)
        if not src.is_file():
            continue
        dest_dir = root / src.parent.name
        dest_dir.mkdir(parents=True, exist_ok=True)
        try:
            shutil.move(str(src), unique_dest(dest_dir, src.name))
            moved += 1
        except OSError as ex:
            log.warning(f"Could not quarantine {src}: {ex}")
    return moved


def _write_dataset(train: dict, val: dict, cfg, progress: ProgressCB,
                   cancel) -> dict:
    """Copy val as-is, and copy train with the cap/floor balancing applied."""
    rng = random.Random(cfg.seed)
    out = Path(cfg.output_dir)

    total = sum(len(v) for v in train.values()) + sum(len(v) for v in val.values())
    done = 0

    def tick(n: int = 1, note: str = "") -> None:
        nonlocal done
        done += n
        progress(min(1.0, done / total) if total else 1.0,
                 note or f"writing… {done:,}/{total:,}")

    # val first, and untouched: it is the only thing making this round's
    # numbers comparable with the last round's.
    val_written = 0
    for cls, files in sorted(val.items()):
        check_cancelled(cancel, "writing val")
        dest_dir = out / "val" / cls
        dest_dir.mkdir(parents=True, exist_ok=True)
        for f in files:
            shutil.copy2(f, unique_dest(dest_dir, Path(f).name))
            val_written += 1
        tick(len(files), f"val/{cls}")

    train_written = augmented = 0
    capped: list[str] = []
    floored: list[str] = []

    for cls, files in sorted(train.items()):
        check_cancelled(cancel, "writing train")
        dest_dir = out / "train" / cls
        dest_dir.mkdir(parents=True, exist_ok=True)

        files = list(files)
        rng.shuffle(files)

        cap = cfg.class_caps.get(cls, cfg.cap)
        if cap and len(files) > cap:
            log.info(f"  {cls}: {len(files):,} -> capped to {cap:,}"
                     + ("  (per-class override)" if cls in cfg.class_caps else ""))
            capped.append(f"{cls} {len(files):,}->{cap:,}")
            files = files[:cap]

        for f in files:
            shutil.copy2(f, unique_dest(dest_dir, Path(f).name))
            train_written += 1
        tick(len(files), f"train/{cls}")

        floor = cfg.floor
        if floor and files and len(files) < floor:
            needed = floor - len(files)
            log.info(f"  {cls}: {len(files):,} -> augmenting {needed:,} more "
                     f"to reach floor={floor:,}")
            floored.append(f"{cls} {len(files):,}->{floor:,}")
            made = _augment_up_to(files, dest_dir, needed, rng)
            train_written += made
            augmented += made
            if made < needed:
                log.warning(f"  {cls}: only {made:,} of {needed:,} augmented "
                            f"copies could be made")
        elif floor and not files:
            log.warning(f"  {cls}: no images to augment from - left empty")

    return {"train_written": train_written, "val_written": val_written,
            "augmented": augmented, "capped": capped, "floored": floored}


def _augment_up_to(sources: list[str], dest_dir: Path, needed: int,
                   rng: random.Random) -> int:
    """Write *needed* augmented copies drawn from *sources*."""
    made = 0
    attempts = 0
    limit = needed * 3          # a class of unreadable files must not spin
    while made < needed and attempts < limit:
        attempts += 1
        src = rng.choice(sources)
        img = imaging.imread(src)
        if img is None:
            continue
        dest = unique_dest(dest_dir, f"{Path(src).stem}_aug{made + 1}.jpg")
        if imaging.imwrite(dest, imaging.augment_image(img, rng), 95):
            made += 1
    return made
