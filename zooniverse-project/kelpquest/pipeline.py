"""
The eight stages, and the result contract they share.

Every stage is ``run(ctx, progress=None, cancel=None) -> StageResult`` and none
of them raise for an expected problem -- inspect the result instead. That keeps
the GUI's worker thread to one shape: call, get a result, report it.

Each stage runs on its own, so a step can be redone without redoing the
transect: re-classify with different weights, rewrite a sheet after correcting
the site. Where two stages want the same expensive thing, they can be fused --
cutting a patch and classifying it both need the source still decoded, and the
finished JPEG carries the predicted label, so `Cut patches` will do both in one
pass when asked, and each patch is then encoded once instead of twice.

Nothing here imports the GUI. Progress leaves through a callback and
cancellation arrives as an Event, which is the whole of the boundary.
"""

from __future__ import annotations

import threading
import traceback
from collections.abc import Callable
from dataclasses import dataclass, field
from pathlib import Path

from . import classify as classify_mod
from . import discovery, metadata, patches, sampling, zooniverse
from . import export as export_mod
from . import rejoin as rejoin_mod
from . import report as report_mod
from .config import AppConfig, default_output_dir
from .labelset import Labelset
from .logging_setup import get_logger
from .power import keep_awake

log = get_logger("pipeline")

Progress = Callable[[float, str], None]

#: Stage order, and the keys used to name a stage everywhere else.
ORDER = ("transect", "patches", "classify", "metadata", "upload", "export",
         "rejoin", "report")

TITLES = {
    "transect": "Transect folder",
    "patches": "Cut patches",
    "classify": "Classify patches",
    "metadata": "Write metadata",
    "upload": "Upload to Zooniverse",
    "export": "Export classifications",
    "rejoin": "Rejoin to Toolbox",
    "report": "Build the report",
}

#: Why the next stage is now worth visiting. Shown in the rail once a stage
#: commits.
NEXT_HINT = {
    "patches": "The transect is confirmed.",
    "classify": "The patches are cut and listed in patches.csv.",
    "metadata": "Every patch now carries a prediction.",
    "upload": "Both sheets are written.",
    "export": "The subject set exists — come back once volunteers have worked "
              "on it.",
    "rejoin": "The classifications are on disk.",
    "report": "The answers are joined; the report summarises the same export.",
}


# --------------------------------------------------------------------------
#  Contract
# --------------------------------------------------------------------------


@dataclass
class StageResult:
    """What one stage did. Stages return this; they do not raise."""

    stage: str = ""
    cancelled: bool = False
    preview: bool = False
    counts: dict[str, int] = field(default_factory=dict)
    outputs: list[Path] = field(default_factory=list)
    #: Named values the next stage consumes -- the scanned transect, a new
    #: subject set id. Kept apart from `outputs`, which is files written.
    advance: dict = field(default_factory=dict)
    lines: list[str] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)
    errors: list[str] = field(default_factory=list)

    @property
    def ok(self) -> bool:
        return not self.errors and not self.cancelled

    @property
    def committed(self) -> bool:
        """True when this run actually wrote its output rather than checking."""
        return self.ok and not self.preview

    def summary(self) -> str:
        head = TITLES.get(self.stage, self.stage) + ": "
        if self.errors:
            head += "failed"
        elif self.cancelled:
            head += "stopped early"
        elif self.preview:
            head += "checked — nothing written"
        else:
            head += "done"
        counts = "  ".join(f"{k} {v:,}" for k, v in self.counts.items())
        out = [head + (f"  ({counts})" if counts else "")]
        out += self.lines
        out += [f"WARNING: {w}" for w in self.warnings]
        out += [f"ERROR: {e}" for e in self.errors]
        out += [f"wrote {p}" for p in self.outputs]
        return "\n".join(out)


@dataclass
class Context:
    """Everything a stage is allowed to see."""

    cfg: AppConfig
    transect: discovery.Transect | None = None
    #: True on the first click of a new set of paths: report what would happen,
    #: write nothing.
    preview: bool = True

    @property
    def output_dir(self) -> Path:
        return Path(self.cfg.output_dir or
                    default_output_dir(self.cfg.transect_folder))

    @property
    def manifest(self) -> Path:
        return self.output_dir / metadata.MANIFEST_NAME


class Steps:
    """Give each step a slice of the bar, and never let it run backwards.

    A progress bar that jumps back to zero reads as a hang, and people kill the
    job. Each step maps its own 0-1 into its share of the whole.
    """

    def __init__(self, sink: Progress | None, **weights: float):
        self.sink = sink
        total = sum(weights.values()) or 1.0
        self._share = {k: v / total for k, v in weights.items()}
        self._base = {}
        running = 0.0
        for k in weights:
            self._base[k] = running
            running += self._share[k]
        self._high = 0.0

    def step(self, key: str) -> Progress:
        base, share = self._base.get(key, 0.0), self._share.get(key, 0.0)

        def report(frac: float, message: str = "") -> None:
            if self.sink is None:
                return
            value = base + share * max(0.0, min(1.0, frac))
            self._high = max(self._high, value)
            self.sink(self._high, message)

        return report

    def done(self, message: str = "") -> None:
        if self.sink:
            self._high = 1.0
            self.sink(1.0, message)


# --------------------------------------------------------------------------
#  Shared helpers
# --------------------------------------------------------------------------


def _load_labels(path: str) -> tuple[Labelset, list[str]]:
    """The labelset, plus any warning about not having one."""
    if not path:
        return Labelset.empty(), [
            "No labelset chosen, so 'Long Label' falls back to the short code. "
            "Pick labelset_toolbox_zooniverse.json on the Classify page."]
    try:
        labels = Labelset.load(path)
        log.info(f"Labelset: {len(labels)} labels from {Path(path).name}")
        return labels, []
    except (OSError, ValueError) as exc:
        return Labelset.empty(), [f"Could not read the labelset ({exc}). "
                                  "Long labels will fall back to the code."]


def _records_for(points: list[sampling.Point], scale: float) -> list[metadata.PatchRecord]:
    return [
        metadata.PatchRecord(
            filename=patches.patch_filename(p.name, p.row, p.column),
            # Forward slashes, matching the Path column Toolbox writes, so
            # ours and theirs diff cleanly.
            name=p.name, path=p.path.as_posix(), row=p.row, column=p.column,
            patch_size=p.patch_size, scale=scale, done=False,
        )
        for p in points
    ]


def _make_classifier(cfg: AppConfig, labels: Labelset,
                     res: StageResult) -> classify_mod.Classifier:
    classifier = classify_mod.Classifier(
        cfg.classify.weights, cfg.classify.device,
        cfg.classify.imgsz, cfg.classify.batch)
    unknown = labels.unknown_codes(classifier.class_codes) if len(labels) else []
    if unknown:
        res.warnings.append(
            f"{len(unknown)} model class(es) are not in the labelset and will "
            f"keep their short code: {', '.join(unknown[:6])}"
            + (" …" if len(unknown) > 6 else ""))
    res.lines.append(f"model: {classifier.describe()}")
    return classifier


def _process(records: list[metadata.PatchRecord], output_dir: Path,
             jpeg_quality: int, labels: Labelset,
             classifier: classify_mod.Classifier | None,
             write_images: bool,
             progress: Progress | None = None,
             cancel: threading.Event | None = None) -> dict[str, int]:
    """Cut, score and write every patch, one source image at a time.

    The source still is decoded once and everything that needs it happens
    before it is dropped - a still is 4,600 px square and decoding one is
    thousands of times the cost of the crops taken from it.
    """
    by_image: dict[str, list[metadata.PatchRecord]] = {}
    for r in records:
        by_image.setdefault(r.path, []).append(r)

    counts = {"patches": 0, "classified": 0, "skipped": 0}
    done = 0
    total = max(1, len(records))

    for image_path, group in by_image.items():
        if cancel is not None and cancel.is_set():
            break
        image = patches.imread(image_path)
        if image is None:
            log.warning(f"Could not read {Path(image_path).name} - "
                        f"{len(group)} patch(es) skipped.")
            counts["skipped"] += len(group)
            done += len(group)
            if progress:
                progress(done / total, "")
            continue

        if classifier is not None:
            crops = [patches.tight_patch(image, r.row, r.column, r.patch_size)
                     for r in group]
            try:
                # strict: a short result list would otherwise leave the tail of
                # this image silently unclassified.
                for record, suggestions in zip(
                        group, classifier.predict(crops), strict=True):
                    record.suggestions = suggestions
                    counts["classified"] += 1
            except Exception as exc:
                log.error(f"Classification failed on "
                          f"{Path(image_path).name}: {exc}")

        for record in group:
            if cancel is not None and cancel.is_set():
                break
            if write_images:
                label = labels.long_name(record.code) if record.code else ""
                crop, _geom = patches.render(image, record.row, record.column,
                                             record.patch_size, record.scale,
                                             label)
                if crop is None or not patches.imwrite(
                        output_dir / record.filename, crop, jpeg_quality):
                    log.warning(f"Could not write {record.filename}")
                    counts["skipped"] += 1
                    done += 1
                    continue
            record.done = True
            counts["patches"] += 1
            done += 1
            if progress and done % 25 == 0:
                progress(done / total,
                         f"{'Classifying' if classifier else 'Cutting'} "
                         f"patches - {done:,}/{total:,}")

    if progress:
        progress(done / total, "")
    return counts


def _finished(records: list[metadata.PatchRecord],
              stage: StageResult) -> list[metadata.PatchRecord]:
    """Only the patches that were actually cut.

    A stopped run must not leave a metadata.csv naming images that were never
    written -- the upload would report every one of them as missing, and the
    Toolbox sheet would claim annotations nobody made.
    """
    kept = [r for r in records if r.done]
    dropped = len(records) - len(kept)
    if dropped:
        stage.warnings.append(
            f"{dropped:,} planned patch(es) were not reached; the sheets "
            f"describe the {len(kept):,} that were.")
    return kept


def _write_sheets(ctx: Context, records: list[metadata.PatchRecord],
                  labels: Labelset) -> list[Path]:
    """The manifest, both deliverable sheets, and the telemetry join."""
    cfg, out = ctx.cfg, ctx.output_dir
    written = [metadata.write_manifest(out / metadata.MANIFEST_NAME, records)]
    written.append(metadata.write_toolbox_csv(
        out / metadata.toolbox_csv_name(cfg.site_name, cfg.survey_date,
                                        cfg.transect_number),
        records, labels))
    tel = ctx.transect.telemetry if ctx.transect else None
    written.append(metadata.write_zooniverse_csv(
        out / metadata.ZOONIVERSE_NAME, records, labels,
        cfg.site_name, cfg.survey_date, cfg.transect_number,
        cfg.transect_id, tel))
    if tel is not None:
        written.append(metadata.write_joined_csv(
            out / metadata.joined_csv_name(cfg.site_name, cfg.survey_date,
                                           cfg.transect_number),
            records, labels, tel))
    return written


def _guard(stage: str, preview: bool, fn) -> StageResult:
    """Run a stage and turn anything it throws into a reported failure.

    A stage that raises through the worker leaves the GUI with a spinning bar
    and no explanation. Returning the traceback puts it in the log pane, where
    somebody can read it.
    """
    try:
        res = fn()
    except Exception as exc:
        log.error(f"{TITLES.get(stage, stage)} failed: {exc}")
        log.debug(traceback.format_exc())
        res = StageResult(stage=stage, errors=[str(exc)])
    res.stage = stage
    res.preview = preview
    return res


# --------------------------------------------------------------------------
#  1 - Transect
# --------------------------------------------------------------------------


def run_transect(ctx: Context, progress: Progress | None = None,
                 cancel: threading.Event | None = None) -> StageResult:
    """Scan the folder and report what a run would use. Writes nothing."""

    def work() -> StageResult:
        res = StageResult()
        cfg = ctx.cfg
        if not cfg.transect_folder:
            raise ValueError("Choose the transect folder — the folder of "
                             "edited stills for one transect.")
        if progress:
            progress(0.2, "Reading the folder…")
        t = discovery.scan(cfg.transect_folder, ctx.output_dir,
                           cfg.telemetry_csv or None)
        res.advance["transect"] = t
        res.counts = {"images": len(t.images)}
        res.lines.append(t.summary())
        if t.images:
            res.lines.append(f"first: {t.images[0].name}")
            res.lines.append(f"last:  {t.images[-1].name}")
        res.lines += [f"· {n}" for n in t.notes]
        if not t.images:
            res.errors.append(f"No stills found in {t.folder}.")
        if t.placeholders:
            res.warnings.append(
                f"{len(t.placeholders)} image(s) are online-only placeholders "
                "and would stream from the network on every read.")
        if progress:
            progress(1.0, t.summary())
        return res

    # Read-only either way, so the preview flag is recorded rather than obeyed.
    return _guard("transect", ctx.preview, work)


# --------------------------------------------------------------------------
#  2 - Cut patches
# --------------------------------------------------------------------------


def run_patches(ctx: Context, progress: Progress | None = None,
                cancel: threading.Event | None = None) -> StageResult:
    """Sample points and cut a patch around each one.

    Classifies in the same pass when asked and weights are set, because both
    halves need the same decoded still and the finished JPEG carries the label.
    """

    def work() -> StageResult:
        res = StageResult()
        cfg = ctx.cfg
        t = ctx.transect
        if t is None or not t.images:
            raise ValueError("Scan the transect folder first.")
        labels, notes = _load_labels(cfg.classify.labelset)
        res.warnings.extend(notes)

        fuse = bool(cfg.patch.classify_in_same_pass and cfg.classify.weights)
        classifier = None
        if fuse:
            classifier = _make_classifier(cfg, labels, res)
            res.lines.append("Classifying in the same pass, so each patch is "
                             "encoded once.")
        elif cfg.patch.classify_in_same_pass:
            res.warnings.append(
                "No model weights chosen, so the patches go out unlabelled. "
                "Set them on the Classify page, or run that stage next.")

        steps = Steps(progress, sample=5, cut=95)
        points = sampling.sample(t.images, cfg.sample,
                                 steps.step("sample"), cancel)
        records = _records_for(points, cfg.patch.scale)
        res.counts["planned"] = len(records)

        if ctx.preview:
            res.lines.append(
                f"{len(records):,} patches from {len(t.images)} still(s) at "
                f"{int(round(cfg.patch.scale * cfg.sample.patch_size))} px, "
                f"into {ctx.output_dir}")
            existing = len(list(ctx.output_dir.glob("*.jpg"))) \
                if ctx.output_dir.is_dir() else 0
            if existing:
                res.warnings.append(
                    f"{existing:,} patch JPEG(s) are already in that folder "
                    "and would be overwritten where the names match.")
            steps.done("Check finished.")
            return res

        with keep_awake(log.info):
            counts = _process(records, ctx.output_dir, cfg.patch.jpeg_quality,
                              labels, classifier, True,
                              steps.step("cut"), cancel)

        res.counts.update({"images": len(t.images), **counts})
        res.cancelled = bool(cancel and cancel.is_set())
        kept = _finished(records, res)
        res.outputs = [metadata.write_manifest(ctx.manifest, kept)]
        if classifier is not None:
            res.lines.append("Predicted labels:")
            res.lines += metadata.label_summary(kept, labels)
            res.advance["classified"] = True
        steps.done("Patches cut.")
        return res

    return _guard("patches", ctx.preview, work)


# --------------------------------------------------------------------------
#  3 - Classify
# --------------------------------------------------------------------------


def run_classify(ctx: Context, progress: Progress | None = None,
                 cancel: threading.Event | None = None) -> StageResult:
    """Score the patches listed in the manifest with our own model."""

    def work() -> StageResult:
        res = StageResult()
        cfg = ctx.cfg
        if not ctx.manifest.is_file():
            raise FileNotFoundError(
                f"No {metadata.MANIFEST_NAME} in {ctx.output_dir}. Cut the "
                "patches first.")
        records = metadata.read_manifest(ctx.manifest)
        labels, notes = _load_labels(cfg.classify.labelset)
        res.warnings.extend(notes)
        classifier = _make_classifier(cfg, labels, res)
        res.counts["patches"] = len(records)

        if ctx.preview:
            already = sum(1 for r in records if r.classified)
            res.lines.append(
                f"{len(records):,} patches in the manifest"
                + (f", {already:,} already classified — those would be "
                   "re-scored" if already else ""))
            if cfg.classify.burn_label:
                res.lines.append("Patch images would be redrawn with the new "
                                 "label.")
            if progress:
                progress(1.0, "Check finished.")
            return res

        steps = Steps(progress, run=100)
        with keep_awake(log.info):
            counts = _process(records, ctx.output_dir, cfg.patch.jpeg_quality,
                              labels, classifier, cfg.classify.burn_label,
                              steps.step("run"), cancel)

        res.counts.update(counts)
        res.lines.append("Predicted labels:")
        res.lines += metadata.label_summary(records, labels)
        res.outputs = [metadata.write_manifest(ctx.manifest, records)]
        res.cancelled = bool(cancel and cancel.is_set())
        res.advance["classified"] = True
        steps.done("Classification finished.")
        return res

    return _guard("classify", ctx.preview, work)


# --------------------------------------------------------------------------
#  4 - Metadata
# --------------------------------------------------------------------------


def run_metadata(ctx: Context, progress: Progress | None = None,
                 cancel: threading.Event | None = None) -> StageResult:
    """Write the Toolbox and Zooniverse sheets from the manifest."""

    def work() -> StageResult:
        res = StageResult()
        cfg = ctx.cfg
        if not ctx.manifest.is_file():
            raise FileNotFoundError(
                f"No {metadata.MANIFEST_NAME} in {ctx.output_dir}. Cut the "
                "patches first.")
        records = metadata.read_manifest(ctx.manifest)
        labels, notes = _load_labels(cfg.classify.labelset)
        res.warnings.extend(notes)
        res.counts["rows"] = len(records)

        unclassified = sum(1 for r in records if not r.classified)
        if unclassified:
            res.warnings.append(
                f"{unclassified:,} of {len(records):,} patches have no "
                "prediction; their label columns will be empty.")
        if not cfg.transect_id:
            res.warnings.append(
                "No transect id — set the prefix on the Transect page. Every "
                "subject is stamped with it, and it is how a classification is "
                "traced back to a transect.")

        tel = ctx.transect.telemetry if ctx.transect else None
        if tel is None:
            res.warnings.append(
                "No telemetry, so no depth or position columns. Pick the UTC "
                "transect CSV on the Transect page to add them.")

        if ctx.preview:
            for name in _sheet_names(ctx):
                res.lines.append(f"would write {name}")
            res.lines.append("Predicted labels:")
            res.lines += metadata.label_summary(records, labels)
            if progress:
                progress(1.0, "Check finished.")
            return res

        res.outputs = _write_sheets(ctx, records, labels)
        res.lines.append("Predicted labels:")
        res.lines += metadata.label_summary(records, labels)
        res.advance["metadata_csv"] = ctx.output_dir / metadata.ZOONIVERSE_NAME
        if progress:
            progress(1.0, "Sheets written.")
        return res

    return _guard("metadata", ctx.preview, work)


def _sheet_names(ctx: Context) -> list[str]:
    cfg = ctx.cfg
    names = [
        metadata.MANIFEST_NAME,
        metadata.toolbox_csv_name(cfg.site_name, cfg.survey_date,
                                  cfg.transect_number),
        metadata.ZOONIVERSE_NAME,
    ]
    if ctx.transect and ctx.transect.telemetry is not None:
        names.append(metadata.joined_csv_name(
            cfg.site_name, cfg.survey_date, cfg.transect_number))
    return names


# --------------------------------------------------------------------------
#  5 - Upload
# --------------------------------------------------------------------------


def run_upload(ctx: Context, progress: Progress | None = None,
               cancel: threading.Event | None = None) -> StageResult:
    """Create one Zooniverse subject per patch."""

    def work() -> StageResult:
        res = StageResult()
        cfg = ctx.cfg
        csv_path = ctx.output_dir / metadata.ZOONIVERSE_NAME
        if not csv_path.is_file():
            raise FileNotFoundError(
                f"No {metadata.ZOONIVERSE_NAME} in {ctx.output_dir}. Write the "
                "metadata sheets first.")
        if not cfg.transect_id:
            raise ValueError(
                "Set the transect id prefix on the Transect page. Every "
                "subject is stamped with it.")

        outcome = zooniverse.upload(
            ctx.output_dir, csv_path, cfg.transect_id,
            subject_set_name=cfg.upload.subject_set_name,
            subject_set_id=cfg.upload.subject_set_id,
            checkpoint_every=cfg.upload.checkpoint_every,
            sleep=cfg.upload.sleep, limit=cfg.upload.limit,
            dry_run=ctx.preview, progress=progress, cancel=cancel)

        res.counts = {"uploaded": outcome.uploaded, "skipped": outcome.skipped,
                      "failed": outcome.failed}
        res.cancelled = outcome.cancelled
        res.errors.extend(outcome.errors[:5])
        if len(outcome.errors) > 5:
            res.warnings.append(f"{len(outcome.errors) - 5} further upload "
                                "error(s) are in the log.")
        res.lines.append(outcome.summary())
        if outcome.subject_set_id and not ctx.preview:
            res.advance["subject_set_id"] = outcome.subject_set_id
            res.lines.append(
                f"Subject set ID {outcome.subject_set_id} — record this in "
                "tracker.xlsx.")
        return res

    return _guard("upload", ctx.preview, work)


# --------------------------------------------------------------------------
#  6 - Export
# --------------------------------------------------------------------------


def run_export(ctx: Context, progress: Progress | None = None,
               cancel: threading.Event | None = None) -> StageResult:
    """Download the classifications for every subject set the points live in."""

    def work() -> StageResult:
        res = StageResult()
        cfg = ctx.cfg
        ids = export_mod.parse_ids(cfg.export.subject_set_ids)
        if not ids:
            raise ValueError(
                "Give at least one subject set ID — the last number in a "
                "subject set's Zooniverse URL. Separate several with commas.")
        if not cfg.export.output_dir:
            raise ValueError("Choose where to save the exports.")

        outcome = export_mod.export(
            ids, cfg.export.output_dir,
            combined_name=cfg.export.combined_csv,
            dry_run=ctx.preview, progress=progress, cancel=cancel)

        res.counts = {"sets": len(ids), "rows": outcome.rows}
        res.cancelled = outcome.cancelled
        res.lines.append(outcome.summary())
        res.warnings.extend(outcome.warnings)
        res.errors.extend(outcome.errors)
        res.outputs = list(outcome.per_set_csv)
        if outcome.combined_csv:
            res.outputs.append(outcome.combined_csv)
            res.advance["export_csv"] = outcome.combined_csv
        return res

    return _guard("export", ctx.preview, work)


# --------------------------------------------------------------------------
#  7 - Rejoin
# --------------------------------------------------------------------------


def run_rejoin(ctx: Context, progress: Progress | None = None,
               cancel: threading.Event | None = None) -> StageResult:
    """Apply the label rules and report where every point stands."""

    def work() -> StageResult:
        res = StageResult()
        cfg = ctx.cfg
        r = cfg.rejoin
        if not r.toolbox_csv:
            raise ValueError("Choose the Toolbox annotations CSV — the one "
                             "stage 4 wrote for this transect.")
        if not r.classifications_csv:
            raise ValueError("Choose the classifications export — the combined "
                             "CSV stage 6 wrote.")
        if not cfg.classify.labelset:
            raise ValueError("Choose the labelset JSON on the Classify page. "
                             "The rules map a volunteer's answer onto it.")

        outcome = rejoin_mod.rejoin(
            r.toolbox_csv, r.classifications_csv, cfg.classify.labelset,
            r.output_dir or str(Path(r.toolbox_csv).parent),
            annotations_json=r.annotations_json or None,
            use_yn=r.use_yesno, use_yn_exp=r.use_yesno_expert,
            use_multi=r.use_multi, use_multi_exp=r.use_multi_expert,
            dry_run=ctx.preview, progress=progress, cancel=cancel)

        res.counts = {"points": outcome.toolbox_rows, **outcome.counts}
        res.cancelled = outcome.cancelled
        res.lines.append(outcome.summary())
        res.warnings.extend(outcome.warnings)
        res.errors.extend(outcome.errors)
        res.outputs = list(outcome.outputs)
        res.advance["status_counts"] = outcome.counts
        res.advance["status_block"] = outcome.status_block()
        return res

    return _guard("rejoin", ctx.preview, work)


# --------------------------------------------------------------------------
#  8 - Report
# --------------------------------------------------------------------------


def run_report(ctx: Context, progress: Progress | None = None,
               cancel: threading.Event | None = None) -> StageResult:
    """Build the multi-sheet Excel summary from an export."""

    def work() -> StageResult:
        res = StageResult()
        cfg = ctx.cfg
        if not cfg.report.export_csv:
            raise ValueError("Choose the classifications export CSV.")
        out_dir = cfg.report.output_dir or str(
            Path(cfg.report.export_csv).parent / "reports")

        outcome = report_mod.build(
            cfg.report.export_csv, out_dir,
            workflow_id=cfg.report.workflow_id,
            source_image=cfg.report.source_image,
            dry_run=ctx.preview, progress=progress, cancel=cancel)

        res.counts = {"rows": outcome.rows}
        res.cancelled = outcome.cancelled
        res.lines.append(outcome.summary())
        res.warnings.extend(outcome.warnings)
        res.errors.extend(outcome.errors)
        if outcome.output_xlsx:
            res.outputs = [outcome.output_xlsx]
        return res

    return _guard("report", ctx.preview, work)


#: Every stage, by key. The GUI dispatches through this, so adding a stage is
#: one entry here and one panel.
STAGES: dict[str, Callable[..., StageResult]] = {
    "transect": run_transect,
    "patches": run_patches,
    "classify": run_classify,
    "metadata": run_metadata,
    "upload": run_upload,
    "export": run_export,
    "rejoin": run_rejoin,
    "report": run_report,
}


def run(stage: str, ctx: Context, progress: Progress | None = None,
        cancel: threading.Event | None = None) -> StageResult:
    """Run one stage by key."""
    runner = STAGES.get(stage)
    if runner is None:
        return StageResult(stage=stage, errors=[f"Unknown stage '{stage}'."])
    return runner(ctx, progress, cancel)
