"""
Stage 1 - Toolbox annotation CSV(s) -> a patch dataset.

Sits downstream of ``zooni_to_toolbox_annot.py``:

    toolbox_import.csv (one or more)  ->  [this stage]  ->  dataset/train/<Label>/*.jpg
                                                            dataset/val/<Label>/*.jpg

For every row with ``Verified == True`` it crops exactly ``Patch Size`` square,
centred on ``(Row, Column)`` -- no overlay, no scaling, no burned-in text,
unlike the Zooniverse review crops.

Two things this stage does that are worth knowing about:

**It decodes each source image once.** Annotation rows arrive ordered by CSV,
not by image, so cropping them in row order re-decoded the same multi-megapixel
survey photo once per annotated point on it. Grouping by source first turns
N decodes into one.

**It drops duplicate annotation points before splitting.** Two rows naming the
same ``(Path, Row, Column)`` describe one point, and would otherwise produce two
byte-identical patches under different names -- inflating that class's count
and, if the two landed in different splits, leaking train into val.
"""

from __future__ import annotations

import time
from pathlib import Path

import pandas as pd

from .. import imaging, provenance
from ..fsutil import (IMG_EXTS, parse_remaps, resolve_source, sanitize_label,
                      unique_dest)
from ..logging_setup import get_logger
from ..progress import ProgressCB, Stages, check_cancelled
from . import StageResult, guard

log = get_logger("extract")

REQUIRED_COLUMNS = {"Name", "Path", "Row", "Column", "Patch Size", "Label", "Verified"}

#: Written alongside the patches: the rows actually used, with ``Path``
#: rewritten to the resolved location. A later run against the same dataset
#: then needs no path remaps at all.
UPDATED_CSV = "annotations_updated_paths.csv"


# --------------------------------------------------------------------------
#  Loading and filtering
# --------------------------------------------------------------------------

def load_annotations(paths: list[str]) -> pd.DataFrame:
    frames = []
    for p in paths:
        df = pd.read_csv(p)
        log.info(f"  {Path(p).name}: {len(df):,} rows")
        frames.append(df)
    if not frames:
        return pd.DataFrame()
    return pd.concat(frames, ignore_index=True)


def drop_duplicate_points(df: pd.DataFrame) -> tuple[pd.DataFrame, int, int]:
    """Collapse rows describing the same annotation point.

    Returns ``(df, dropped, conflicting)``. A conflicting group is one where the
    same point carries more than one label -- resolved to the first occurrence,
    but logged individually, because it means two annotators disagreed and the
    disagreement is being silently resolved.
    """
    cols = ["Path", "Row", "Column"]
    mask = df.duplicated(subset=cols, keep=False)
    if not mask.any():
        return df, 0, 0

    conflicting = 0
    for _, group in df[mask].groupby(cols):
        if group["Label"].nunique() > 1:
            conflicting += 1
            row = group.iloc[0]
            log.warning(
                f"  Point labelled {sorted(group['Label'].unique())} in different "
                f"rows - keeping '{row['Label']}': "
                f"{row['Path']} r={row['Row']} c={row['Column']}"
            )

    before = len(df)
    df = df.drop_duplicates(subset=cols, keep="first").copy()
    return df, before - len(df), conflicting


def assign_splits(df: pd.DataFrame, val_frac: float, min_val_count: int,
                  seed: int) -> pd.Series:
    """Per-class stratified train/val assignment.

    Classes below ``min_val_count`` go entirely to train: holding a val sample
    out of a class with four examples measures nothing and costs the class a
    quarter of its training data.
    """
    import random

    rng = random.Random(seed)
    split = pd.Series(index=df.index, dtype=object)

    for label, group in df.groupby("Label"):
        idx = list(group.index)
        rng.shuffle(idx)
        n = len(idx)
        if n < min_val_count:
            split.loc[idx] = "train"
            log.warning(f"  Class '{label}': only {n} sample(s) - all to train "
                        f"(below min-val-count={min_val_count})")
            continue
        n_val = max(1, round(n * val_frac))
        split.loc[idx[:n_val]] = "val"
        split.loc[idx[n_val:]] = "train"

    return split


def write_updated_csv(df: pd.DataFrame, remaps, output_dir: str) -> str:
    out = df.drop(columns=["_split", "_src"], errors="ignore").copy()
    out["Path"] = df["_src"]
    Path(output_dir).mkdir(parents=True, exist_ok=True)
    path = Path(output_dir) / UPDATED_CSV
    out.to_csv(path, index=False)
    return str(path)


# --------------------------------------------------------------------------
#  The stage
# --------------------------------------------------------------------------

def run(cfg, *, progress: ProgressCB | None = None, cancel=None) -> StageResult:
    started = time.time()
    return guard(lambda: _run(cfg, progress, cancel), started)


def _run(cfg, progress, cancel) -> StageResult:
    res = StageResult(preview=cfg.dry_run)
    st = Stages(progress).plan(load=5, prepare=5, write=90)

    if not cfg.annotation_csvs:
        res.errors.append("No annotation CSV selected.")
        return res
    if not cfg.output_dir:
        res.errors.append("No output folder selected.")
        return res

    missing_csv = [p for p in cfg.annotation_csvs if not Path(p).is_file()]
    if missing_csv:
        res.errors.append(f"Annotation CSV not found: {missing_csv[0]}")
        return res

    remaps = parse_remaps(cfg.path_remaps)
    if remaps:
        log.info(f"Path remaps: {'; '.join(f'{o} => {n}' for o, n in remaps)}")

    # ---- load ------------------------------------------------------
    log.info(f"Loading {len(cfg.annotation_csvs)} annotation CSV(s)…")
    df = load_annotations(cfg.annotation_csvs)
    if df.empty:
        res.errors.append("Annotation CSV(s) contained no rows.")
        return res

    missing_cols = REQUIRED_COLUMNS - set(df.columns)
    if missing_cols:
        res.errors.append(
            f"Annotation CSV is missing required column(s): {sorted(missing_cols)}")
        return res
    st.finish("load", f"{len(df):,} annotation rows loaded")
    check_cancelled(cancel, "loading")

    # ---- filter ----------------------------------------------------
    total_rows = len(df)
    df = df[df["Verified"] == True].copy()  # noqa: E712
    log.info(f"Verified=True: {len(df):,} of {total_rows:,} rows")

    if cfg.exclude_labels:
        before = len(df)
        df = df[~df["Label"].isin(cfg.exclude_labels)].copy()
        if before != len(df):
            log.info(f"Excluded labels {cfg.exclude_labels}: "
                     f"{before - len(df):,} row(s) removed")

    df, dropped, conflicting = drop_duplicate_points(df)
    if dropped:
        log.warning(f"Dropped {dropped:,} duplicate annotation point(s); "
                    f"{conflicting} had conflicting labels")
        res.warnings.append(
            f"{dropped:,} duplicate annotation point(s) dropped"
            + (f", {conflicting} with conflicting labels" if conflicting else ""))

    if df.empty:
        res.errors.append("No rows left to extract after filtering.")
        return res

    # ---- resolve source paths --------------------------------------
    df["_src"] = df["Path"].map(lambda p: resolve_source(p, remaps))
    sources = df["_src"].unique()
    present = {s: Path(s).is_file() for s in sources}
    missing = sorted(s for s, ok in present.items() if not ok)

    log.info(f"{len(df):,} point(s) across {len(sources):,} source image(s)")
    if missing:
        n_rows = int((~df["_src"].map(present)).sum())
        log.warning(f"{len(missing):,} source image(s) not found "
                    f"({n_rows:,} point(s) affected). First few:")
        for s in missing[:5]:
            log.warning(f"    {s}")
        res.warnings.append(
            f"{len(missing):,} of {len(sources):,} source image(s) not found - "
            f"{n_rows:,} point(s) cannot be extracted. Check the path remaps.")

    # ---- keep a held-out set independent, at the photo level ---------
    excluded_photos = pd.DataFrame()
    already = 0
    if cfg.no_split and cfg.independent_of.strip():
        if not Path(cfg.independent_of).is_dir():
            res.errors.append(f"Training dataset not found: {cfg.independent_of}")
            return res
        trained_on = provenance.training_photos(cfg.independent_of)
        stems = df["Name"].map(lambda n: Path(str(n)).stem)
        clash = stems.isin(trained_on)
        excluded_photos = df[clash]
        df = df[~clash].reset_index(drop=True)
        if len(excluded_photos):
            by_cls = excluded_photos["Label"].value_counts()
            log.warning("=" * 66)
            log.warning(f"{len(excluded_photos):,} point(s) sit on "
                        f"{excluded_photos['Name'].nunique():,} photo(s) that "
                        f"also fed {Path(cfg.independent_of).name}. Left out: "
                        f"a held-out patch cut from a training photo is a "
                        f"near-duplicate the hash check cannot see.")
            for label, n in by_cls.head(8).items():
                log.warning(f"    {label:<14} {n:>6,}")
            log.warning("=" * 66)
        else:
            log.info(f"No annotation point shares a photo with "
                     f"{Path(cfg.independent_of).name} - independent.")
        if df.empty:
            res.errors.append(
                "Every annotation point is on a photo the training set already "
                "used. A held-out set needs photos the model has never seen - "
                "annotate transects that were kept out of training.")
            return res

    # ---- adding to a held-out set that already exists ----------------
    if cfg.no_split and Path(cfg.output_dir).is_dir():
        have = {p.name for p in Path(cfg.output_dir).rglob("*.jp*g")}
        names = df.apply(lambda r: f"{Path(str(r['Name'])).stem}_r{int(r['Row'])}"
                                   f"_c{int(r['Column'])}.jpg", axis=1)
        present = names.isin(have)
        already = int(present.sum())
        if already:
            log.info(f"{already:,} point(s) are already in {Path(cfg.output_dir).name} "
                     f"- not extracted again.")
            df = df[~present].reset_index(drop=True)

    # Recounted after the held-out exclusions above, so the preview says how
    # many photos will actually be cut, not how many the CSV mentioned.
    sources = df["_src"].unique()
    class_counts = df["Label"].value_counts()
    log.info("Class counts to extract:")
    for label, n in class_counts.items():
        log.info(f"  {label:<16} {n:>7,}")

    # ---- split -----------------------------------------------------
    if cfg.no_split:
        log.info("No-split mode: flat <Label>/ folders. This output is a "
                 "HELD-OUT set - it must never be trained or validated on.")
        df["_split"] = ""
    else:
        df["_split"] = assign_splits(df, cfg.val_frac, cfg.min_val_count, cfg.seed)
        counts = df["_split"].value_counts()
        log.info(f"Split: {counts.get('train', 0):,} train / "
                 f"{counts.get('val', 0):,} val")

    res.outputs.update({
        "output_dir": cfg.output_dir,
        "no_split": cfg.no_split,
        "points": len(df),
        "sources": len(sources),
        "missing_sources": len(missing),
        "classes": {str(k): int(v) for k, v in class_counts.items()},
        "duplicates_dropped": dropped,
        "excluded_shared_photo": int(len(excluded_photos)),
        "already_present": already,
    })
    st.finish("prepare", "ready to extract")

    if len(excluded_photos):
        res.warnings.append(
            f"{len(excluded_photos):,} point(s) left out because their photo "
            f"also fed {Path(cfg.independent_of).name}. To grow the held-out "
            f"set, annotate photos from transects that were never trained on.")

    if cfg.dry_run:
        res.say(f"Preview: {len(df):,} patch(es) from {len(sources):,} source "
                f"image(s), {len(class_counts)} class(es).")
        if cfg.no_split:
            res.say("Mode: no split - a held-out evaluation set.")
        else:
            counts = df["_split"].value_counts()
            res.say(f"Split: {counts.get('train', 0):,} train / "
                    f"{counts.get('val', 0):,} val.")
        res.say(f"{len(missing):,} source image(s) missing."
                if missing else "All source images found.")
        if cfg.no_split:
            for line in coverage_report(cfg, class_counts):
                res.say(line)
        res.say("Nothing written - clear 'Preview only' to extract.")
        return res

    # ---- write -----------------------------------------------------
    written, skipped = _extract_patches(df, cfg, st.sub("write"), cancel)

    res.outputs["updated_csv"] = write_updated_csv(df, remaps, cfg.output_dir)
    res.outputs.update({"written": written, "skipped": skipped})
    st.finish("write", f"{written:,} patches written")

    res.say(f"Extracted {written:,} patch(es) to {cfg.output_dir}")
    if skipped:
        res.say(f"{skipped:,} skipped (unreadable source, or too close to an edge).")
    res.say(f"Wrote {UPDATED_CSV} - a later run needs no path remaps.")
    if cfg.no_split:
        for line in coverage_report(cfg):
            res.say(line)
        res.say("This is a HELD-OUT set. Point stage 4 at it; never stage 2's "
                "merge list.")
    return res


def coverage_report(cfg, pending: "pd.Series | None" = None) -> list[str]:
    """Which classes the held-out set can and cannot yet measure.

    Measured against the classes of the training set it is kept independent
    of, since those are the classes a model will be asked about. Counts what
    is in the folder now plus, in a preview, what this run would add -- so
    the message is "after this, X is still short", which is the one that
    directs the next round of annotation.
    """
    out = Path(cfg.output_dir)
    have: dict[str, int] = {}
    if out.is_dir():
        for d in out.iterdir():
            if d.is_dir() and not d.name.startswith("_"):
                have[d.name] = sum(1 for f in d.iterdir()
                                   if f.suffix.lower() in IMG_EXTS)
    if pending is not None:
        for label, n in pending.items():
            have[str(label)] = have.get(str(label), 0) + int(n)

    taxonomy = set(have)
    if cfg.independent_of.strip() and Path(cfg.independent_of, "train").is_dir():
        taxonomy |= {d.name for d in Path(cfg.independent_of, "train").iterdir()
                     if d.is_dir() and not d.name.startswith("_")}
    if not taxonomy:
        return []

    target = cfg.holdout_target
    missing = sorted(c for c in taxonomy if have.get(c, 0) == 0)
    thin = sorted((c, have[c]) for c in taxonomy
                  if 0 < have.get(c, 0) < target)
    lines = [f"Held-out coverage ({'after this run, ' if pending is not None else ''}"
             f"target {target} per class): "
             f"{len(taxonomy) - len(missing) - len(thin)} of {len(taxonomy)} "
             f"classes at or above target."]
    if missing:
        lines.append(f"   Cannot be measured at all - no images: "
                     + ", ".join(missing))
    if thin:
        lines.append("   Too few to trust: " + ", ".join(
            f"{c} ({n}, need {target - n} more)" for c, n in thin))
    return lines


def _extract_patches(df: pd.DataFrame, cfg, progress: ProgressCB,
                     cancel) -> tuple[int, int]:
    """Crop and write every patch, decoding each source image exactly once."""
    written = skipped = 0
    groups = list(df.groupby("_src", sort=False))
    total = len(df)
    done = 0

    for src, rows in groups:
        check_cancelled(cancel, "extraction")

        img = imaging.imread(str(src))
        if img is None:
            log.warning(f"Could not read source image: {src}")
            skipped += len(rows)
            done += len(rows)
            progress(done / total, f"extracting… {done:,}/{total:,}")
            continue

        for _, row in rows.iterrows():
            done += 1
            label = sanitize_label(row["Label"])
            split = row["_split"]
            class_dir = (Path(cfg.output_dir) / label if cfg.no_split
                         else Path(cfg.output_dir) / split / label)
            class_dir.mkdir(parents=True, exist_ok=True)

            try:
                r, c, size = int(row["Row"]), int(row["Column"]), int(row["Patch Size"])
            except (TypeError, ValueError):
                log.warning(f"Non-numeric Row/Column/Patch Size in {src} - skipped")
                skipped += 1
                continue

            patch = imaging.crop_patch(img, r, c, size)
            if patch is None:
                log.warning(f"Point r={r} c={c} too close to the edge of "
                            f"{Path(src).name} - skipped")
                skipped += 1
                continue

            stem = Path(str(row["Name"])).stem
            dest = unique_dest(class_dir, f"{stem}_r{r}_c{c}.jpg")
            if not imaging.imwrite(dest, patch, cfg.jpeg_quality):
                log.warning(f"Failed to write {dest}")
                skipped += 1
                continue
            written += 1

        progress(done / total, f"extracting… {done:,}/{total:,}")

    return written, skipped
