"""
Stage 5 - several trained models -> one decision about which to keep.

Retraining produces candidates faster than anyone can judge them, and the
question is never "did the number go up". It is whether it went up *on data
the model has never seen*, whether it went up for the classes that matter, and
whether the run that produced it was still learning or had started memorising.
Those are three different questions and they routinely disagree.

So this stage reads three kinds of evidence and keeps them separate:

**The held-out evaluation** (stage 4's ``*_predictions_detail.csv``) is the
number to trust, and the only one that supports a ranking. Per-image rows mean
precision, recall and F1 can be recomputed here rather than taken on faith,
and the confusions can be named -- "SU_bould is read as SU_cob a fifth of the
time" is actionable in a way that "SU_bould scores 0.43" is not.

**The training curves** (``results.csv``) say how the run behaved. A model can
win on accuracy while its val loss has been climbing for forty epochs, and
that is worth knowing before it becomes the production model.

**The arguments** (``args.yaml``) say what was different, which is what turns
"model B is better" into "raising the cap on KE_sieve helped".

The one rule this stage will not bend: models are only ranked against each
other when they were evaluated on **the same held-out images**. Comparing
accuracy across different evaluation sets is not a comparison, and the
pipeline already exists because a leaked holdout inflated a number by three
points. Differing sets are reported and the ranking is withheld.
"""

from __future__ import annotations

import time
from dataclasses import dataclass, field
from pathlib import Path

import pandas as pd

from ..logging_setup import get_logger
from ..progress import ProgressCB, Stages, check_cancelled
from . import StageResult, guard

log = get_logger("compare")

WORKBOOK_NAME = "model_comparison.xlsx"
HISTORY_MD = "model_history.md"
DECISIONS_MD = "decisions.md"
CURVES_PNG = "model_comparison_curves.png"
PER_CLASS_PNG = "model_comparison_per_class.png"

#: A class scoring below this on F1 is called out as weak. 0.60 is the
#: threshold the existing hand-built comparison used, kept so these numbers
#: stay comparable with the spreadsheets already circulating. It is a
#: convention for triage, not a standard: nothing changes about a class at
#: 0.61 versus 0.59.
WEAK_F1 = 0.60

#: Rough bands for reading a macro F1 at a glance. Orientation only -- what
#: counts as good depends on how many classes there are and how separable they
#: look at this patch size, and two adjacent seafloor points can be genuinely
#: ambiguous to a human expert. The number that matters most is the direction
#: between your own runs.
F1_BANDS = ((0.30, "weak"), (0.50, "fair"), (0.70, "good"), (1.01, "strong"))

#: A class whose reported abundance is within this much of the truth is near
#: enough for percent cover; beyond it the survey figure is visibly skewed.
COVER_OK_PCT = 10

#: The bottleneck codes, in words that do not assume the vocabulary.
BOTTLENECK_WORDS = {
    "recall": "missed",
    "precision": "over-called",
    "balanced": "both equally",
    "-": "-",
}

BOTTLENECK_MEANING = {
    "recall": ("the model does not spot it - where this class really is, it "
               "usually calls it something else"),
    "precision": ("the model claims it too readily - when it says this class, "
                  "it is often something else"),
    "balanced": "it misses it and over-calls it about equally often",
}


def describe_f1(value: float) -> str:
    """A one-word reading of an F1 score."""
    for edge, word in F1_BANDS:
        if value < edge:
            return word
    return "strong"


def chance_level(n_classes: int) -> float:
    """What a model that guessed at random would score, for scale."""
    return round(1 / n_classes, 3) if n_classes else 0.0


def weights_note(curve: dict) -> str:
    """Whether the *saved* weights are affected by what the run did later.

    Worth stating plainly because "this run overfitted" reads as "this model
    is overfitted", and for a run whose best epoch came early those are
    opposite claims. Ultralytics saves best.pt at the best validation epoch,
    so the weights predate the divergence; what the divergence tells you is
    about the recipe, not about the file you would deploy.
    """
    best, total = curve.get("best_epoch"), curve.get("epochs_total")
    verdict = curve.get("verdict")
    if not best or not total:
        return ""
    if verdict in ("overfitting", "mild overfitting"):
        return (
            f"The saved weights are from epoch {best} of {total} - taken "
            f"before the overfitting set in, so the model you would actually "
            f"use is not an overfitted one. What the overfitting says is that "
            f"the last {total - best} epochs were wasted, and that a "
            f"better-regularised recipe would probably reach a higher peak "
            f"than epoch {best} did.")
    if verdict in ("still improving", "underfitting"):
        return (
            f"The saved weights are from epoch {best} of {total}, the last or "
            f"nearly the last - the run was cut off rather than finished, so "
            f"there is very likely more to gain from simply training longer.")
    return (f"The saved weights are from epoch {best} of {total}, and the run "
            f"stayed healthy throughout.")

#: Val-minus-train loss gaps, and what each one means. The bands are coarse on
#: purpose: this is a prompt to look at the curve, not a verdict to quote.
GAP_MILD = 0.10
GAP_SEVERE = 0.35

#: When two evaluations cover different images, they can still be compared on
#: the images they share -- but only if that shared set is most of both, and
#: big enough to mean anything. Below either bar the difference between the
#: runs is more interesting than the comparison, so no ranking is given.
MIN_OVERLAP_FRACTION = 0.80
MIN_OVERLAP_IMAGES = 500

#: Classes with almost no held-out examples have an F1 that is noise -- one
#: image either way swings it completely -- and they crowd the readable
#: classes off the chart. They stay in every table; only the figure filters.
MIN_CHART_SUPPORT = 10


# --------------------------------------------------------------------------
#  Finding what a run left behind
# --------------------------------------------------------------------------

@dataclass
class ModelRun:
    """One candidate model, and the artefacts that describe it."""

    name: str
    root: Path
    results_csv: Path | None = None
    args_yaml: Path | None = None
    weights: Path | None = None
    detail_csv: Path | None = None
    report_csv: Path | None = None

    #: The operator's own record of what this run was trying, if stage 3
    #: wrote one. Empty for runs that predate it.
    notes: dict = field(default_factory=dict)

    #: Filled in by the readers below.
    curve: dict = field(default_factory=dict)
    detail: pd.DataFrame | None = None
    per_class: pd.DataFrame | None = None
    confusions: pd.DataFrame | None = None
    overall: dict = field(default_factory=dict)
    args: dict = field(default_factory=dict)
    holdout_key: frozenset = frozenset()

    @property
    def has_holdout(self) -> bool:
        return self.per_class is not None and bool(self.overall)

    def score(self, subset: frozenset | None = None) -> None:
        """(Re)compute this model's metrics, optionally on a subset of images.

        Evaluations run weeks apart are not necessarily over the same images:
        the held-out folder shrinks whenever stage 2 quarantines a newly found
        leak. Restricting every model to the images they all predicted on is
        what makes those runs comparable again, without re-running inference.
        """
        detail = self.detail
        if subset is not None:
            detail = detail[detail["filepath"].map(
                lambda p: Path(p).name in subset)]
        self.per_class = per_class_metrics(detail)
        self.confusions = top_confusions(detail)
        self.overall = overall_metrics(detail, self.per_class)


def _first(root: Path, patterns: list[str]) -> Path | None:
    """First match for any pattern, searched shallowly.

    A run folder is either the Ultralytics run itself or its parent -- stage 3
    writes ``<output>/<name>/weights`` and stage 4 writes its reports beside
    them in ``<output>`` -- so both levels are checked and neither is assumed.
    """
    for pattern in patterns:
        for candidate in sorted(root.glob(pattern)):
            if candidate.is_file():
                return candidate
    return None


def discover(run_dir: str | Path) -> ModelRun:
    """Locate every artefact a model run can offer, tolerating either layout."""
    root = Path(run_dir)
    name = root.name
    # A folder literally called "train" is Ultralytics' subfolder, not a name
    # anyone would recognise in a comparison table.
    if name.lower() in ("train", "weights") and root.parent.name:
        name = root.parent.name

    return ModelRun(
        name=name,
        root=root,
        notes=read_notes(_first(root, ["run_notes.md", "*/run_notes.md"])),
        results_csv=_first(root, ["results.csv", "*/results.csv"]),
        args_yaml=_first(root, ["args.yaml", "*/args.yaml"]),
        weights=_first(root, ["weights/best.pt", "*/weights/best.pt"]),
        detail_csv=_first(root, ["*_predictions_detail.csv",
                                 "*/*_predictions_detail.csv"]),
        report_csv=_first(root, ["*_accuracy_report.csv",
                                 "*/*_accuracy_report.csv"]),
    )


def discover_all(models_root: str | Path) -> list[str]:
    """Every run folder beneath *models_root*.

    A run is anything holding a results.csv, directly or one level down --
    the two layouts stage 3 and Ultralytics produce between them. Returned as
    the folder the operator would recognise (the parent, when the run sits in
    a ``train/`` subfolder), sorted so the history reads oldest first.
    """
    root = Path(models_root)
    if not root.is_dir():
        return []
    found: dict[str, float] = {}
    for csv in list(root.glob("*/results.csv")) + list(root.glob("*/*/results.csv")):
        run = csv.parent
        if run.name.lower() in ("train", "weights"):
            run = run.parent
        if run == root or "archive" in {p.name.lower() for p in run.relative_to(root).parents} \
                or run.name.lower() == "archive":
            continue
        found[str(run)] = csv.stat().st_mtime
    return [p for p, _ in sorted(found.items(), key=lambda kv: kv[1])]


def read_notes(path: Path | None) -> dict:
    """Pull the operator's words and the recorded settings out of run_notes.md."""
    if not path or not path.is_file():
        return {}
    text = path.read_text(encoding="utf-8", errors="replace")
    out: dict = {"path": str(path)}
    for line in text.splitlines():
        s = line.strip()
        if s.startswith("- **") and ":**" in s:
            key, _, value = s[4:].partition(":**")
            out[key.strip().lower().replace(" ", "_")] = value.strip().strip("`")
    marker = "## What changed in this run, and why"
    if marker in text:
        body = text.split(marker, 1)[1].split("\n## ", 1)[0].strip()
        if not body.startswith("_(no notes"):
            out["why"] = body
    return out


# --------------------------------------------------------------------------
#  Held-out performance, recomputed from the per-image rows
# --------------------------------------------------------------------------

def per_class_metrics(detail: pd.DataFrame) -> pd.DataFrame:
    """Precision, recall, F1 and support for every class, from raw predictions.

    Recomputed rather than read from the accuracy report because that report
    carries recall only. Precision is what says whether a class is *absorbing*
    others -- the failure mode behind a confusion magnet like SU_bould -- and
    without it "43% accurate" does not distinguish a class the model misses
    from one it over-applies.
    """
    truth, pred = detail["true_label"], detail["pred_label"]
    labels = sorted(set(truth) | set(pred))
    total = len(detail)

    rows = []
    for label in labels:
        is_true, is_pred = truth == label, pred == label
        tp = int((is_true & is_pred).sum())
        fp = int((~is_true & is_pred).sum())
        fn = int((is_true & ~is_pred).sum())
        tn = total - tp - fp - fn

        precision = tp / (tp + fp) if tp + fp else 0.0
        recall = tp / (tp + fn) if tp + fn else 0.0
        f1 = (2 * precision * recall / (precision + recall)
              if precision + recall else 0.0)
        specificity = tn / (tn + fp) if tn + fp else 0.0

        rows.append({
            "label": label,
            "support": int(is_true.sum()),
            "predicted_as": int(is_pred.sum()),
            "correct": tp,
            "precision": round(precision, 4),
            "recall": round(recall, 4),
            "f1": round(f1, 4),
            "balanced_accuracy": round((recall + specificity) / 2, 4),
            # What the model would report as this class's cover, against what
            # is really there. Errors that cancel leave cover unbiased even
            # when F1 is poor; errors that do not, bias it directly.
            "cover_bias": int(is_pred.sum() - is_true.sum()),
            "cover_bias_pct": (round(100 * (int(is_pred.sum()) - int(is_true.sum()))
                                     / int(is_true.sum()), 1)
                               if int(is_true.sum()) else None),
            # Which side is holding the class back is the difference between
            # "needs more examples" and "needs a competitor disambiguated".
            # Within a few points of each other the class is not limited by
            # either side, and calling 0.62 against 0.63 "over-called" sends
            # the reader after a problem that is not there.
            "bottleneck": ("-" if not (tp + fp + fn) else
                           "balanced" if abs(precision - recall) < 0.05 else
                           "recall" if recall < precision else "precision"),
        })

    df = pd.DataFrame(rows)
    return df.sort_values("support", ascending=False).reset_index(drop=True)


def overall_metrics(detail: pd.DataFrame, per_class: pd.DataFrame) -> dict:
    """Top-1 plus the macro and weighted averages, in one dict."""
    seen = per_class[per_class["support"] > 0]
    solid = seen[seen["support"] >= MIN_CHART_SUPPORT]
    weights = seen["support"] / seen["support"].sum() if len(seen) else seen["support"]
    return {
        "images": len(detail),
        "top1_accuracy": round(float(detail["correct"].mean()), 4),
        "classes_present": int(len(seen)),
        "macro_precision": round(float(seen["precision"].mean()), 4),
        "macro_recall": round(float(seen["recall"].mean()), 4),
        "macro_f1": round(float(seen["f1"].mean()), 4),
        "weighted_f1": round(float((seen["f1"] * weights).sum()), 4),
        "weak_classes": int((seen["f1"] < WEAK_F1).sum()),
        "classes_total": int(len(seen)),
        # Mean absolute cover error, in percentage points of each class's true
        # count -- the percent-cover deliverable's own accuracy, which F1 does
        # not measure. Restricted to classes with real support: a class with
        # three held-out points predicted nine times reads as +200%, which is
        # arithmetically true and tells nobody anything.
        "mean_abs_cover_bias_pct": round(float(
            solid["cover_bias_pct"].abs().mean()), 1) if len(solid) else None,
        "classes_cover_off_by_25pct": int(
            (solid["cover_bias_pct"].abs() > 25).sum()),
        "cover_classes_counted": int(len(solid)),
        "mean_confidence_correct": round(
            float(detail.loc[detail["correct"], "confidence"].mean()), 4)
        if detail["correct"].any() else 0.0,
        "mean_confidence_wrong": round(
            float(detail.loc[~detail["correct"], "confidence"].mean()), 4)
        if (~detail["correct"]).any() else 0.0,
    }


def top_confusions(detail: pd.DataFrame, limit: int = 12) -> pd.DataFrame:
    """The mistakes that actually cost accuracy, largest first."""
    wrong = detail[~detail["correct"]]
    if wrong.empty:
        return pd.DataFrame(columns=["true_label", "pred_label", "count",
                                     "share_of_class"])
    grouped = (wrong.groupby(["true_label", "pred_label"])
                    .size().reset_index(name="count"))
    support = detail.groupby("true_label").size()
    grouped["share_of_class"] = (
        grouped["count"] / grouped["true_label"].map(support)).round(4)
    return (grouped.sort_values("count", ascending=False)
                   .head(limit).reset_index(drop=True))


def _load_detail(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path)
    if "correct" in df:
        # Written as a Python bool, read back as bool or as the string "True"
        # depending on pandas version -- normalise rather than trust either.
        df["correct"] = df["correct"].astype(str).str.lower().eq("true")
    else:
        df["correct"] = df["true_label"] == df["pred_label"]
    return df


def holdout_identity(detail: pd.DataFrame) -> frozenset:
    """What the model was evaluated *on*, as a comparable key.

    Basenames rather than full paths: the same held-out folder reached through
    a different drive letter or a re-synced Dropbox root is still the same
    evaluation, and refusing to compare those would be pedantry. Two genuinely
    different holdout sets will not share their filenames.
    """
    return frozenset(Path(p).name for p in detail["filepath"])


# --------------------------------------------------------------------------
#  Training behaviour
# --------------------------------------------------------------------------

def curve_summary(results_csv: Path) -> dict:
    """Read one run's learning curve and say what it did.

    ``best_epoch`` is chosen by minimum validation loss, which is what
    Ultralytics saves ``best.pt`` on for classification. The distance from
    there to the last epoch is the part worth reading: a best epoch in the
    first third means the run spent most of its time getting worse.
    """
    df = pd.read_csv(results_csv)
    df.columns = [c.strip() for c in df.columns]
    if "val/loss" not in df or df.empty:
        return {}

    best = df.loc[df["val/loss"].idxmin()]
    last = df.iloc[-1]
    total = int(df["epoch"].max())
    best_epoch = int(best["epoch"])
    gap = float(best["val/loss"]) - float(best.get("train/loss", float("nan")))

    out = {
        "epochs_total": total,
        "best_epoch": best_epoch,
        "best_val_loss": round(float(best["val/loss"]), 5),
        "final_val_loss": round(float(last["val/loss"]), 5),
        "best_train_loss": round(float(best.get("train/loss", float("nan"))), 5),
        "val_minus_train": round(gap, 5),
        "best_top1": round(float(best.get("metrics/accuracy_top1", 0)), 5),
        "best_top5": round(float(best.get("metrics/accuracy_top5", 0)), 5),
        "epochs_past_best": total - best_epoch,
        "val_loss_rise_after_best": round(
            float(last["val/loss"]) - float(best["val/loss"]), 5),
        "final_val_minus_train": round(
            float(last["val/loss"]) - float(last.get("train/loss", float("nan"))), 5),
    }
    out["verdict"], out["suggestion"] = _diagnose(out)
    return out


def _diagnose(c: dict) -> tuple[str, str]:
    """Turn the curve numbers into a verdict and the next thing to try.

    Read at the *end* of the run, not at the best epoch. Those give opposite
    answers on a real run here: one model's validation loss crossed below its
    training loss around epoch 22 and then climbed for the remaining 78 while
    training loss kept falling to 0.08 -- the textbook overfitting picture,
    which a gap measured at the best epoch reports as underfitting.

    ``best.pt`` is unaffected either way: it holds the best epoch regardless of
    what the run did afterwards. What the tail says is whether the *recipe*
    wants changing.
    """
    final_gap = c.get("final_val_minus_train")
    best_gap = c["val_minus_train"]
    rise = c["val_loss_rise_after_best"]
    total, best = c["epochs_total"], c["best_epoch"]
    notes = []

    if final_gap is None or final_gap != final_gap:      # missing or NaN
        final_gap = best_gap

    still_improving = best >= total * 0.9 and total > 5

    if final_gap > GAP_SEVERE:
        verdict = "overfitting"
        notes.append(
            f"By the last epoch validation loss sat {final_gap:.2f} above "
            f"training loss: the model had started memorising the training "
            f"patches. Set Augmentation to 'stronger' on stage 3, or cut "
            f"Epochs so it stops sooner.")
    elif final_gap > GAP_MILD:
        verdict = "mild overfitting"
        notes.append(
            f"A {final_gap:.2f} val-train gap at the end of the run is mild "
            f"and usually acceptable.")
    elif still_improving:
        verdict = "still improving"
        notes.append(
            "Validation loss was still falling when the run ended, so the "
            "model had not finished learning.")
    elif final_gap < 0:
        verdict = "underfitting"
        notes.append(
            "Validation loss stayed below training loss for the whole run, "
            "which usually means the model has not learned enough to overfit "
            "yet -- train longer, or use a larger base model.")
    else:
        verdict = "healthy"
        notes.append("Training and validation loss stayed close together.")

    if best_gap < 0 <= final_gap and verdict.endswith("overfitting"):
        notes.append(
            f"At its best epoch the gap was only {best_gap:.2f}, so the saved "
            f"weights come from the healthy part of the run.")

    if still_improving:
        notes.append(
            f"The best epoch ({best}) is at the very end of {total}. Train "
            f"longer -- this run was cut off, not converged.")
    elif best <= max(1, total * 0.25) and total > 10:
        notes.append(
            f"The best epoch ({best}) came in the first quarter of {total}, so "
            f"{c['epochs_past_best']} epochs were spent getting worse. Cut "
            f"patience so the run stops nearer its best, and lower the "
            f"learning rate if the curve is noisy.")

    if rise > 0.25:
        notes.append(
            f"Validation loss rose {rise:.2f} after the best epoch. best.pt is "
            f"still the right weights, but the schedule wasted "
            f"{c['epochs_past_best']} epochs.")

    return verdict, " ".join(notes)


def read_args(path: Path) -> dict:
    """The handful of training arguments worth putting in a comparison table."""
    try:
        import yaml
        raw = yaml.safe_load(path.read_text(encoding="utf-8")) or {}
    except Exception as ex:
        log.debug(f"Could not read {path}: {ex}")
        return {}
    keep = ("model", "data", "epochs", "patience", "imgsz", "batch",
            "optimizer", "lr0", "weight_decay", "device", "seed",
            # enough to tell which preset a run used, or that it used none
            "hsv_h", "hsv_s", "hsv_v", "degrees", "translate", "shear",
            "scale", "erasing", "auto_augment", "fliplr", "flipud",
            "mixup", "cutmix", "dropout")
    return {k: raw.get(k) for k in keep if k in raw}


def infer_preset(args: dict) -> str:
    """Name the preset a run used, from the values Ultralytics recorded.

    Runs from before run_notes.md existed still have args.yaml, and every
    preset sets a distinctive combination -- so the history can say
    "stronger" for a run nobody labelled at the time. A run matching none of
    them is reported as custom, with the values that make it so.
    """
    from ..config import AUGMENTATION_PRESETS

    if not args:
        return "-"

    def same(a, b) -> bool:
        if a is None or b is None:
            return (a is None or str(a).lower() == "none") and \
                   (b is None or str(b).lower() == "none")
        try:
            return abs(float(a) - float(b)) < 1e-9
        except (TypeError, ValueError):
            return str(a) == str(b)

    for name, preset in AUGMENTATION_PRESETS.items():
        if preset and all(same(args.get(k), v) for k, v in preset.items()):
            return name
    stock = {"scale": 0.5, "erasing": 0.4, "auto_augment": "randaugment",
             "flipud": 0.0, "mixup": 0.0, "dropout": 0.0}
    if all(same(args.get(k), v) for k, v in stock.items()):
        return "ultralytics defaults"
    shown = [f"{k}={args[k]}" for k in ("hsv_s", "degrees", "mixup", "dropout",
                                        "weight_decay", "erasing", "scale")
             if k in args]
    return "custom (" + ", ".join(shown[:4]) + ")"


# --------------------------------------------------------------------------
#  Ranking, and the recommendation
# --------------------------------------------------------------------------

#: Below this difference in held-out top-1, two models are not meaningfully
#: apart on overall accuracy, and the tie is broken on macro F1 instead --
#: which is to say, on the rare classes. On a 13,000-image holdout half a
#: point is roughly 65 images.
TIE_THRESHOLD = 0.005


def recommend(models: list[ModelRun]) -> tuple[ModelRun | None, list[str]]:
    """Pick the model to move forward with, and say why in full.

    Ranked on **macro F1**. F1 balances precision against recall, which is the
    right default when neither error is worse than the other -- and for
    benthic cover neither is: over-calling a class inflates its cover, missing
    it deflates it, and both distort the survey equally. Macro rather than
    weighted, because it scores a class with forty held-out points the same as
    one with four thousand; a weighted average on this taxonomy is dominated
    by the substrate classes and would call a model good while it had stopped
    recognising the rare taxa entirely.

    Top-1 accuracy is reported alongside but does not decide the ranking: it
    is the weighted view under another name, so on an imbalanced set it mostly
    measures how well the common classes are doing.

    Where the two orderings disagree the disagreement is reported rather than
    resolved silently, because which one matters depends on whether the survey
    needs overall cover or rare-taxon detection.
    """
    scored = [m for m in models if m.has_holdout]
    if not scored:
        return None, ["No model has a held-out evaluation, so none can be ranked."]
    if len(scored) == 1:
        return scored[0], [
            f"Only {scored[0].name} has a held-out evaluation, so there is "
            f"nothing to compare it against."]

    by_f1 = sorted(scored, key=lambda m: -m.overall["macro_f1"])
    by_top1 = sorted(scored, key=lambda m: -m.overall["top1_accuracy"])
    winner, runner = by_f1[0], by_f1[1]
    margin = winner.overall["macro_f1"] - runner.overall["macro_f1"]

    lines = [
        f"Recommended: {winner.name} - best macro F1 "
        f"({winner.overall['macro_f1']:.3f}) on "
        f"{winner.overall['images']:,} held-out images."]

    if margin < 0.01:
        lines.append(
            f"It is close: {runner.name} is only {margin:.3f} behind on macro "
            f"F1, which is within the noise of a single evaluation. Treat "
            f"these two as equivalent and choose on the per-class table.")
    if by_top1[0] is not winner:
        lines.append(
            f"{by_top1[0].name} has the higher overall accuracy "
            f"({by_top1[0].overall['top1_accuracy']:.2%} against "
            f"{winner.overall['top1_accuracy']:.2%}), because it does better "
            f"on the common substrate classes. {winner.name} is still the "
            f"recommendation: it is more even across the taxonomy, which is "
            f"what macro F1 measures.")

    # The saved-weights explanation is reported once, in its own section --
    # see build_digest. Repeating it here made the summary say the same
    # paragraph twice.

    bias = winner.overall.get("classes_cover_off_by_25pct")
    if bias:
        lines.append(
            f"For percent cover specifically: {bias} of "
            f"{winner.overall.get('cover_classes_counted')} well-sampled "
            f"class(es) would be reported more than 25% away from their true "
            f"abundance, even where F1 looks acceptable. Errors that cancel "
            f"leave cover unbiased; these do not.")

    return winner, lines


def improvement_notes(m: ModelRun, holdout_shared: bool) -> list[str]:
    """Concrete next steps for one model, drawn from its own numbers."""
    notes = []
    if m.curve.get("suggestion"):
        notes.append(m.curve["suggestion"])
    if not m.has_holdout:
        notes.append("No held-out evaluation: run stage 4 before trusting any "
                     "accuracy figure for this model.")
        return notes

    pc = m.per_class
    seen = pc[pc["support"] > 0]

    # Where the accuracy actually is. A class with 1,300 images at 0.55 F1 is
    # worth more than one with 12 images at 0.10, and sorting by F1 alone hides
    # that completely.
    seen = seen.assign(headroom=(1 - seen["f1"]) * seen["support"])
    worst = seen.sort_values("headroom", ascending=False).head(3)
    if len(worst):
        parts = [f"{r.label} (F1 {r.f1:.2f} on {r.support:,} images, "
                 f"limited by {r.bottleneck})" for r in worst.itertuples()]
        notes.append("Biggest available gains, by images currently misread: "
                     + "; ".join(parts) + ".")

    recall_bound = seen[(seen["bottleneck"] == "recall") & (seen["f1"] < WEAK_F1)]
    if len(recall_bound):
        notes.append(
            f"Missed rather than over-applied: {', '.join(recall_bound['label'].head(5))}"
            f". These need more or more varied training examples -- raising "
            f"the per-class cap or floor in stage 2 is the direct lever.")

    precision_bound = seen[(seen["bottleneck"] == "precision")
                           & (seen["f1"] < WEAK_F1)]
    if len(precision_bound):
        notes.append(
            f"Over-applied rather than missed: "
            f"{', '.join(precision_bound['label'].head(5))}. These are "
            f"absorbing other classes, so more examples of *them* will not "
            f"help; the fix is more examples of whatever they are swallowing.")

    if m.confusions is not None and len(m.confusions):
        top = m.confusions.iloc[0]
        notes.append(
            f"Largest single confusion: {top.true_label} read as "
            f"{top.pred_label} {int(top['count']):,} times "
            f"({top.share_of_class:.0%} of all {top.true_label}). If those two "
            f"are genuinely separable at this patch size, targeted annotation "
            f"there is the highest-value labelling you can do.")

    conf = m.overall
    if conf["mean_confidence_wrong"] > conf["mean_confidence_correct"] - 0.05:
        notes.append(
            f"The model is nearly as confident when wrong ({conf['mean_confidence_wrong']:.2f}) "
            f"as when right ({conf['mean_confidence_correct']:.2f}), so "
            f"confidence cannot be used to filter predictions for review.")

    if not holdout_shared:
        notes.append("This model was evaluated on a different set of images "
                     "from the others, so its numbers are not comparable here.")
    return notes


# --------------------------------------------------------------------------
#  Cross-model tables
# --------------------------------------------------------------------------

def per_class_matrix(models: list[ModelRun], metric: str = "f1") -> pd.DataFrame:
    """One row per class, one column per model, plus who wins each class.

    This is the "how does it compare on discrete labels" question in its most
    direct form: taxa a survey depends on can move in the opposite direction
    to the headline number, and a single accuracy figure hides that entirely.
    """
    scored = [m for m in models if m.has_holdout]
    if not scored:
        return pd.DataFrame()

    frame = None
    for m in scored:
        col = m.per_class[["label", "support", metric]].rename(
            columns={metric: m.name, "support": f"support_{m.name}"})
        frame = col if frame is None else frame.merge(col, on="label", how="outer")

    supports = [c for c in frame.columns if c.startswith("support_")]
    frame["support"] = frame[supports].max(axis=1).fillna(0).astype(int)
    frame = frame.drop(columns=supports)

    names = [m.name for m in scored]
    frame[names] = frame[names].astype(float)
    frame["best_model"] = frame[names].idxmax(axis=1)
    frame["best_minus_worst"] = (frame[names].max(axis=1)
                                 - frame[names].min(axis=1)).round(4)
    order = ["label", "support"] + names + ["best_model", "best_minus_worst"]
    return frame[order].sort_values("support", ascending=False).reset_index(drop=True)


def pairwise_delta(a: ModelRun, b: ModelRun) -> pd.DataFrame:
    """Per-class precision/recall/F1 for two models, with the differences.

    Mirrors the layout of the hand-built comparison sheets so the columns are
    the ones already being read, rather than a new vocabulary to learn.
    """
    cols = ["label", "support", "precision", "recall", "f1", "bottleneck"]
    left = a.per_class[cols].add_suffix(f"_{a.name}").rename(
        columns={f"label_{a.name}": "label", f"support_{a.name}": "support"})
    right = b.per_class[cols].add_suffix(f"_{b.name}").rename(
        columns={f"label_{b.name}": "label", f"support_{b.name}": "support"})
    out = left.merge(right.drop(columns=["support"]), on="label", how="outer")

    for metric in ("precision", "recall", "f1"):
        lhs, rhs = f"{metric}_{a.name}", f"{metric}_{b.name}"
        out[f"delta_{metric}"] = (out[rhs] - out[lhs]).round(4)
    out["verdict"] = out["delta_f1"].apply(
        lambda d: "-" if pd.isna(d) else
        f"{b.name} better" if d > 0.02 else
        f"{a.name} better" if d < -0.02 else "no real difference")
    return out.sort_values("support", ascending=False).reset_index(drop=True)


# --------------------------------------------------------------------------
#  Figures
# --------------------------------------------------------------------------

def plot_curves(models: list[ModelRun], out_path: Path) -> Path | None:
    """Training and validation loss for every run, on shared axes.

    Two panels rather than one: overlaying train and val loss for several runs
    on a single axis is unreadable, and the question being asked of each panel
    is different -- the left one is "did it learn", the right is "did it
    generalise, and when did that stop".
    """
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    runs = [m for m in models if m.results_csv]
    if not runs:
        return None

    fig, (ax_train, ax_val) = plt.subplots(1, 2, figsize=(13, 5), sharey=True)
    for m in runs:
        df = pd.read_csv(m.results_csv)
        df.columns = [c.strip() for c in df.columns]
        if "train/loss" in df:
            ax_train.plot(df["epoch"], df["train/loss"], label=m.name, lw=1.6)
        if "val/loss" in df:
            line, = ax_val.plot(df["epoch"], df["val/loss"], label=m.name, lw=1.6)
            best = m.curve.get("best_epoch")
            if best:
                ax_val.axvline(best, color=line.get_color(), ls=":", lw=1, alpha=0.7)
                ax_val.plot([best], [m.curve["best_val_loss"]], "o",
                            color=line.get_color(), ms=6)

    ax_train.set_title("Training loss")
    ax_val.set_title("Validation loss  (dotted = best epoch, where best.pt is saved)")
    for ax in (ax_train, ax_val):
        ax.set_xlabel("epoch")
        ax.grid(alpha=0.25, lw=0.6)
    ax_train.set_ylabel("loss")
    ax_val.legend(fontsize=8, framealpha=0.9)
    fig.suptitle("Learning curves", fontsize=13)
    fig.tight_layout()
    fig.savefig(out_path, dpi=150)
    plt.close(fig)
    return out_path


def plot_per_class(matrix: pd.DataFrame, models: list[ModelRun],
                   out_path: Path) -> Path | None:
    """Per-class F1 as grouped bars, worst class first.

    Sorted by the best model's weakest classes rather than alphabetically, so
    the left-hand end of the chart is the work queue.
    """
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    import numpy as np

    names = [m.name for m in models if m.has_holdout]
    if matrix.empty or not names:
        return None

    df = matrix[matrix["support"] >= MIN_CHART_SUPPORT].copy()
    omitted = len(matrix) - len(df)
    if df.empty:
        return None
    df["worst"] = df[names].min(axis=1)
    df = df.sort_values("worst").head(30)

    x = np.arange(len(df))
    width = min(0.8 / len(names), 0.35)
    fig, ax = plt.subplots(figsize=(max(9, len(df) * 0.55), 5.2))
    for i, name in enumerate(names):
        ax.bar(x + (i - (len(names) - 1) / 2) * width, df[name], width,
               label=f"{name}")

    ax.axhline(WEAK_F1, color="crimson", ls="--", lw=1,
               label=f"F1 = {WEAK_F1}")
    ax.set_xticks(x)
    ax.set_xticklabels([f"{r.label}\n({r.support:,})" for r in df.itertuples()],
                       rotation=60, ha="right", fontsize=8)
    ax.set_ylabel("F1 on held-out images")
    ax.set_ylim(0, 1)
    title = "Per-class F1, weakest first  (held-out set; support in brackets)"
    if omitted:
        title += (f"\n{omitted} class(es) with fewer than "
                  f"{MIN_CHART_SUPPORT} held-out examples omitted - see the "
                  f"workbook for those")
    ax.set_title(title, fontsize=11)
    ax.legend(fontsize=8)
    ax.grid(axis="y", alpha=0.25, lw=0.6)
    fig.tight_layout()
    fig.savefig(out_path, dpi=150)
    plt.close(fig)
    return out_path


# --------------------------------------------------------------------------
#  The digest -- what someone actually reads
# --------------------------------------------------------------------------

def build_digest(models, matrix, winner, rationale, shared, caveats) -> dict:
    """Everything worth knowing, small enough to read in one sitting.

    The workbook keeps every number; this keeps the ones that change a
    decision. Both the log summary and the tear sheet render from here, so
    they cannot drift apart.
    """
    scored = [m for m in models if m.has_holdout]
    by_f1 = sorted(scored, key=lambda m: -m.overall["macro_f1"])

    table = [{
        "model": m.name,
        "macro_f1": m.overall["macro_f1"],
        "macro_f1_word": describe_f1(m.overall["macro_f1"]),
        "top1": m.overall["top1_accuracy"],
        "weighted_f1": m.overall["weighted_f1"],
        "weak": m.overall["weak_classes"],
        "classes_total": m.overall.get("classes_total"),
        "cover_bias": m.overall.get("mean_abs_cover_bias_pct"),
        "cover_classes": m.overall.get("cover_classes_counted"),
        "verdict": m.curve.get("verdict", "-"),
        "best_epoch": m.curve.get("best_epoch"),
        "epochs": m.curve.get("epochs_total"),
        "weights_note": weights_note(m.curve),
        "is_winner": m is winner,
    } for m in by_f1]

    movers = []
    if len(matrix) and winner is not None and len(by_f1) > 1:
        names = [m.name for m in scored]
        other = [n for n in names if n != winner.name]
        if other:
            runner = other[0]
            frame = matrix[matrix["support"] >= MIN_CHART_SUPPORT].copy()
            frame["delta"] = frame[winner.name] - frame[runner]
            frame = frame.reindex(frame["delta"].abs().sort_values(
                ascending=False).index)
            movers = [{
                "label": r.label, "support": int(r.support),
                "winner_f1": float(frame.loc[r.Index, winner.name]),
                "other_f1": float(frame.loc[r.Index, runner]),
                "delta": float(r.delta),
                "winner_name": winner.name,
                "other_name": runner,
                # Spelled out rather than left to a sign: "0.08 worse" does
                # not say worse than what.
                "better_model": winner.name if r.delta > 0 else runner,
            } for r in frame.head(6).itertuples()]

    weakest = []
    if winner is not None and winner.per_class is not None:
        pc = winner.per_class
        seen = pc[pc["support"] >= MIN_CHART_SUPPORT].copy()
        seen["headroom"] = (1 - seen["f1"]) * seen["support"]
        weakest = [{
            "label": r.label, "support": int(r.support), "f1": float(r.f1),
            "precision": float(r.precision), "recall": float(r.recall),
            "bottleneck": r.bottleneck,
            "problem": BOTTLENECK_WORDS.get(r.bottleneck, r.bottleneck),
            "cover_bias_pct": r.cover_bias_pct,
        } for r in seen.sort_values("headroom", ascending=False).head(5).itertuples()]

    confusions = []
    if winner is not None and winner.confusions is not None:
        confusions = [{
            "true": r.true_label, "pred": r.pred_label,
            "count": int(r.count), "share": float(r.share_of_class),
        } for r in winner.confusions.head(5).itertuples()]

    n_classes = winner.overall.get("classes_total") if winner else 0
    return {
        "recommended": winner.name if winner else None,
        "comparable": shared,
        "images": winner.overall["images"] if winner else 0,
        "classes_total": n_classes,
        "chance_f1": chance_level(n_classes),
        "weak_f1": WEAK_F1,
        "cover_ok_pct": COVER_OK_PCT,
        "weights_note": weights_note(winner.curve) if winner else "",
        "why": list(rationale),
        "caveats": list(caveats),
        "table": table,
        "movers": movers,
        "weakest": weakest,
        "confusions": confusions,
        "next_steps": _next_steps(winner, models, shared),
    }


def _next_steps(winner, models, shared) -> list[str]:
    """The handful of actions worth taking, ordered by expected payoff."""
    if winner is None:
        return ["Run stage 4 on at least two models against the same held-out "
                "folder, then compare again."]
    steps = []
    pc = winner.per_class
    seen = pc[pc["support"] >= MIN_CHART_SUPPORT].copy()
    seen["headroom"] = (1 - seen["f1"]) * seen["support"]
    top = seen.sort_values("headroom", ascending=False).head(2)

    for r in top.itertuples():
        if r.bottleneck == "recall":
            steps.append(
                f"{r.label} is missed more than it is over-called "
                f"(recall {r.recall:.2f} vs precision {r.precision:.2f}) on "
                f"{int(r.support):,} held-out points - the largest single "
                f"pool of errors. Raise its cap or floor in stage 2, or "
                f"annotate more of it.")
        elif r.bottleneck == "precision":
            steps.append(
                f"{r.label} is over-applied rather than missed "
                f"(precision {r.precision:.2f} vs recall {r.recall:.2f}). More "
                f"examples of it will not help; it needs more examples of "
                f"whatever it is absorbing.")
        else:
            steps.append(
                f"{r.label} is missed and over-called about equally "
                f"(precision {r.precision:.2f}, recall {r.recall:.2f}) on "
                f"{int(r.support):,} held-out points. Nothing is one-sided "
                f"here, so this class needs the confusion pairs below "
                f"disambiguated rather than simply more examples.")

    if winner.confusions is not None and len(winner.confusions):
        c = winner.confusions.iloc[0]
        steps.append(
            f"Biggest single confusion: {c.true_label} read as {c.pred_label} "
            f"{int(c['count']):,} times ({c.share_of_class:.0%} of all "
            f"{c.true_label}). If those two are separable at this patch size, "
            f"that is the highest-value annotation available.")

    verdict = winner.curve.get("verdict")
    if verdict in ("overfitting", "mild overfitting"):
        steps.append(
            f"The run overfitted after epoch {winner.curve.get('best_epoch')} "
            f"of {winner.curve.get('epochs_total')}. On stage 3, set "
            f"Augmentation to 'stronger' - more colour variation and full "
            f"rotation, plus mixup, dropout 0.2 and double the weight decay - "
            f"and lower Patience so the run stops nearer its best.")
    elif verdict in ("underfitting", "still improving"):
        steps.append(
            "The run had not finished learning. Raise Epochs, and on stage 3 "
            "set Augmentation to 'lighter' so less of each image is distorted "
            "while the model is still trying to fit what is there.")

    missing = [m.name for m in models if not m.has_holdout]
    if missing:
        steps.append(
            f"Not yet evaluated, so not in this ranking: {', '.join(missing)}. "
            f"Run stage 4 on each against the same held-out folder.")
    if not shared:
        steps.append("Re-evaluate every model against one held-out folder so "
                     "the comparison rests on identical images.")
    return steps


def digest_lines(d: dict) -> list[str]:
    """The digest as plain text, for the log pane."""
    if not d.get("recommended"):
        return ["No ranking could be produced."] + d.get("next_steps", [])

    w = max(len(r["model"]) for r in d["table"])
    best = next(r for r in d["table"] if r["is_winner"])
    out = [
        "=" * 78,
        f"RECOMMENDED:  {d['recommended']}",
        "=" * 78,
        f"Ranked by macro F1 on {d['images']:,} held-out images across "
        f"{d['classes_total']} classes.",
        f"Macro F1 runs 0 (useless) to 1 (perfect). Guessing at random on "
        f"{d['classes_total']} classes scores about {d['chance_f1']:.2f};",
        f"this model scores {best['macro_f1']:.3f} ({best['macro_f1_word']}).",
        "",
        f"  {'model':<{w}}  {'macro F1':>8}  {'top-1':>7}  {'weak':>7}  "
        f"{'cover err':>9}  saved weights",
    ]
    for r in d["table"]:
        mark = "*" if r["is_winner"] else " "
        cover = f"{r['cover_bias']:.0f}%" if r["cover_bias"] is not None else "-"
        weak = f"{r['weak']}/{r['classes_total']}"
        out.append(
            f"{mark} {r['model']:<{w}}  {r['macro_f1']:>8.3f}  "
            f"{r['top1']:>7.1%}  {weak:>7}  {cover:>9}  "
            f"epoch {r['best_epoch']} of {r['epochs']}")
    out += [
        "",
        f"  macro F1   average of each class's precision-and-recall balance, "
        f"every class",
        f"             counting equally - so rare taxa cannot be ignored. "
        f"Higher is better.",
        f"  top-1      share of all points labelled correctly. Dominated by "
        f"the common classes.",
        f"  weak       classes scoring below F1 {d['weak_f1']:.2f}, a triage "
        f"threshold, not a standard.",
        f"  cover err  how far this model's reported abundance per class sits "
        f"from the truth,",
        f"             on average. Under +/-{d['cover_ok_pct']}% is fine for "
        f"percent cover.",
    ]
    if d.get("weights_note"):
        out += ["", "ABOUT THE SAVED WEIGHTS", f"  {d['weights_note']}"]

    if d["why"]:
        out += ["", "WHY"] + [f"  - {line}" for line in d["why"]]
    if d["caveats"]:
        out += ["", "READ THIS FIRST"] + [f"  ! {c}" for c in d["caveats"]]

    if d["movers"]:
        out += ["", "WHERE THE MODELS DISAGREE MOST  (per-class F1)",
                f"  chosen = {d['movers'][0]['winner_name']}",
                f"  other  = {d['movers'][0]['other_name']}",
                "",
                f"  {'class':<12}{'points':>8}  {'chosen':>8}  {'other':>8}   "
                f"better on this class"]
        for m in d["movers"]:
            out.append(
                f"  {m['label']:<12}{m['support']:>8,}  "
                f"{m['winner_f1']:>8.2f}  {m['other_f1']:>8.2f}   "
                f"{'chosen' if m['delta'] > 0 else 'other'}")

    if d["weakest"]:
        out += ["", "THE WORK QUEUE  (weakest classes, by how many points they "
                "get wrong)",
                f"  {'class':<12}{'points':>8}  {'F1':>5}   {'problem':<13}"
                f"{'cover':>7}"]
        for c in d["weakest"]:
            bias = ("-" if c["cover_bias_pct"] is None
                    else f"{c['cover_bias_pct']:+.0f}%")
            out.append(
                f"  {c['label']:<12}{c['support']:>8,}  {c['f1']:>5.2f}   "
                f"{c['problem']:<13}{bias:>7}")
        out += [
            "    missed      = where it really is, the model calls it "
            "something else",
            "    over-called = when the model says it, it is often something "
            "else",
            f"    cover       = reported abundance vs truth. Negative = "
            f"under-reported,",
            "                  positive = over-reported.",
        ]

    if d["next_steps"]:
        out += ["", "WHAT TO TRY NEXT"]
        out += [f"  {i}. {t}" for i, t in enumerate(d["next_steps"], 1)]
    out.append("=" * 72)
    return out


# --------------------------------------------------------------------------
#  The stage
# --------------------------------------------------------------------------

def run(cfg, *, progress: ProgressCB | None = None, cancel=None) -> StageResult:
    started = time.time()
    return guard(lambda: _run(cfg, progress, cancel), started)


def _run(cfg, progress, cancel) -> StageResult:
    res = StageResult(preview=cfg.preview_only)
    st = Stages(progress).plan(read=45, analyse=25, write=30)

    runs = [r for r in cfg.runs if str(r).strip()]
    if cfg.models_root.strip():
        found = discover_all(cfg.models_root)
        added = [f for f in found if not any(_same_run(f, r) for r in runs)]
        if added:
            log.info(f"Models root: {len(added)} run folder(s) found under "
                     f"{Path(cfg.models_root).name}")
        runs = runs + added
    if len(runs) < 2:
        res.errors.append("Select at least two model run folders to compare, "
                          "or a models root with at least two runs beneath it.")
        return res
    for r in runs:
        if not Path(r).is_dir():
            res.errors.append(f"Model folder not found: {r}")
            return res
    if not cfg.preview_only and not cfg.output_dir:
        res.errors.append("No output folder selected.")
        return res

    # ---- read -------------------------------------------------------
    models: list[ModelRun] = []
    for i, r in enumerate(runs, 1):
        check_cancelled(cancel, "reading runs")
        m = discover(r)
        log.info(f"[{i}] {m.name}")
        log.info(f"      curves   : {'found' if m.results_csv else 'MISSING'}")
        log.info(f"      held-out : {'found' if m.detail_csv else 'MISSING'}")

        if m.results_csv:
            m.curve = curve_summary(m.results_csv)
        if m.args_yaml:
            m.args = read_args(m.args_yaml)
        if m.detail_csv:
            m.detail = _load_detail(m.detail_csv)
            m.holdout_key = holdout_identity(m.detail)
            m.score()
            log.info(f"      top-1 {m.overall['top1_accuracy']:.2%} on "
                     f"{m.overall['images']:,} unseen images, "
                     f"macro-F1 {m.overall['macro_f1']:.3f}")
        else:
            res.warnings.append(
                f"{m.name} has no predictions-detail CSV, so it cannot be "
                f"ranked. Run stage 4 on it against the same held-out set.")
        models.append(m)
        st.sub("read")(i / len(runs), f"read {m.name}")
    st.finish("read", "runs read")

    # ---- is this a comparison at all? -------------------------------
    scored = [m for m in models if m.has_holdout]
    shared, subset = _align_holdouts(scored, res)
    if subset is not None:
        for m in scored:
            m.score(subset)
        res.outputs["compared_on_images"] = len(subset)

    # ---- analyse ----------------------------------------------------
    matrix = per_class_matrix(models) if shared else pd.DataFrame()
    winner, rationale = (recommend(models) if shared else (None, []))
    res.outputs["models"] = [m.name for m in models]
    res.outputs["comparable"] = shared
    if winner:
        res.outputs["recommended"] = winner.name
    st.finish("analyse", "compared")

    digest = build_digest(models, matrix, winner, rationale, shared,
                          list(res.warnings))
    res.outputs["digest"] = digest
    for line in digest_lines(digest):
        log.info(line)

    # The summary block above is the report; the run summary stays short so
    # the two do not say the same thing twice.
    if winner:
        best = next(r for r in digest["table"] if r["is_winner"])
        res.say(f"Use {winner.name} - macro F1 {best['macro_f1']:.3f}, "
                f"top-1 {best['top1']:.1%} on {digest['images']:,} held-out "
                f"images.")
        if digest["next_steps"]:
            res.say(f"Next: {digest['next_steps'][0]}")

    if cfg.preview_only:
        res.say("Nothing written - clear 'Preview only' to write the report "
                "and open the summary window.")
        return res

    # ---- write ------------------------------------------------------
    out = Path(cfg.output_dir)
    out.mkdir(parents=True, exist_ok=True)
    written = _write_workbook(models, matrix, winner, rationale, shared, out)
    res.outputs.update(written)
    st.finish("write", "report written")

    # Keyed without the suffix: the tear sheet asks for "curves", not the
    # filename it happens to have been written under.
    digest["figures"] = {k.removesuffix("_png"): written[k]
                         for k in ("curves_png", "per_class_png")
                         if written.get(k)}
    digest["workbook"] = written["workbook"]
    res.outputs["digest"] = digest

    # The record. History is regenerated from the run folders every time, so
    # it cannot drift from them; decisions are appended, never rewritten, so
    # a choice made in June is still there in December with its reasons.
    record_dir = Path(cfg.models_root) if cfg.models_root.strip() else out
    history = write_history(models, winner, record_dir)
    decision = append_decision(models, winner, digest, record_dir)
    res.outputs["history"] = str(history)
    res.outputs["decisions"] = str(decision)
    digest["history"] = str(history)

    res.say(f"Full detail: {written['workbook']}")
    res.say(f"History: {history}")
    res.say(f"Decision logged: {decision}")
    return res


def _align_holdouts(scored: list[ModelRun], res: StageResult):
    """Decide whether these evaluations can be compared, and on which images.

    Returns ``(comparable, subset)``, where *subset* is the set of image names
    every model should be re-scored on, or None to leave them as they are.
    """
    if len(scored) < 2:
        return True, None

    keys = [set(m.holdout_key) for m in scored]
    if len({frozenset(k) for k in keys}) == 1:
        log.info(f"All {len(scored)} scored model(s) were evaluated on the "
                 f"same {len(keys[0]):,} held-out images.")
        return True, None

    common = set.intersection(*keys)
    smallest = min(len(k) for k in keys)
    sizes = ", ".join(f"{m.name} ({len(m.holdout_key):,})" for m in scored)

    if len(common) < MIN_OVERLAP_IMAGES or len(common) < smallest * MIN_OVERLAP_FRACTION:
        log.warning("=" * 66)
        log.warning("Models were evaluated on DIFFERENT held-out images.")
        log.warning("=" * 66)
        res.warnings.append(
            f"These models were evaluated on held-out sets that barely "
            f"overlap: {sizes}, sharing only {len(common):,} images. That is "
            f"too little to compare on, so no ranking is given. Re-run stage 4 "
            f"for each model against one held-out folder.")
        return False, None

    dropped = {m.name: len(m.holdout_key) - len(common) for m in scored}
    detail = ", ".join(f"{k} loses {v:,}" for k, v in dropped.items() if v)
    log.warning(f"Held-out sets differ ({sizes}). Comparing on the "
                f"{len(common):,} images all models were scored on; {detail}.")
    res.warnings.append(
        f"These models were evaluated on different held-out sets ({sizes}), "
        f"which usually means the folder shrank between runs as stage 2 "
        f"quarantined newly found leaks. They are compared here on the "
        f"{len(common):,} images common to all of them, so the figures below "
        f"will not match the ones printed by stage 4 at the time.")
    return True, frozenset(common)


def _write_workbook(models, matrix, winner, rationale, shared, out: Path) -> dict:
    """One workbook, one sheet per question being asked."""
    book = out / WORKBOOK_NAME
    scored = [m for m in models if m.has_holdout]

    summary_rows = [{
        "model": m.name,
        "held_out_images": m.overall.get("images"),
        "top1_accuracy": m.overall.get("top1_accuracy"),
        "macro_f1": m.overall.get("macro_f1"),
        "macro_precision": m.overall.get("macro_precision"),
        "macro_recall": m.overall.get("macro_recall"),
        "weighted_f1": m.overall.get("weighted_f1"),
        "classes_present": m.overall.get("classes_present"),
        "classes_below_weak_f1": m.overall.get("weak_classes"),
        "confidence_when_right": m.overall.get("mean_confidence_correct"),
        "confidence_when_wrong": m.overall.get("mean_confidence_wrong"),
        "recommended": "yes" if winner is m else "",
    } for m in models]

    training_rows = [{
        "model": m.name,
        "epochs_total": m.curve.get("epochs_total"),
        "best_epoch": m.curve.get("best_epoch"),
        "epochs_past_best": m.curve.get("epochs_past_best"),
        "best_val_loss": m.curve.get("best_val_loss"),
        "best_train_loss": m.curve.get("best_train_loss"),
        "val_minus_train_at_best": m.curve.get("val_minus_train"),
        "val_minus_train_at_end": m.curve.get("final_val_minus_train"),
        "val_loss_rise_after_best": m.curve.get("val_loss_rise_after_best"),
        "internal_top1": m.curve.get("best_top1"),
        "verdict": m.curve.get("verdict"),
        "suggestion": m.curve.get("suggestion"),
    } for m in models]

    verdict_rows = [{"recommendation": ln} for ln in (
        rationale or ["No ranking given - see the warnings on this run."])]
    suggestion_rows = [{"model": m.name, "suggestion": n}
                       for m in models for n in improvement_notes(m, shared)]
    # "run" rather than "model": args.yaml has its own "model" key, holding
    # the base weights the run started from, and the two would collide.
    args_rows = [dict(run=m.name, **{k: str(v) for k, v in m.args.items()})
                 for m in models if m.args]

    with pd.ExcelWriter(book, engine="openpyxl") as writer:
        pd.DataFrame(verdict_rows).to_excel(
            writer, sheet_name="recommendation", index=False)
        pd.DataFrame(summary_rows).to_excel(
            writer, sheet_name="holdout_summary", index=False)
        pd.DataFrame(training_rows).to_excel(
            writer, sheet_name="training_summary", index=False)
        if suggestion_rows:
            pd.DataFrame(suggestion_rows).to_excel(
                writer, sheet_name="suggestions", index=False)
        if len(matrix):
            matrix.to_excel(writer, sheet_name="per_class_f1", index=False)
        if len(scored) == 2:
            pairwise_delta(scored[0], scored[1]).to_excel(
                writer, sheet_name="per_class_delta", index=False)
        for m in scored:
            m.per_class.to_excel(writer, sheet_name=_sheet("cls_" + m.name),
                                 index=False)
            if m.confusions is not None and len(m.confusions):
                m.confusions.to_excel(writer, sheet_name=_sheet("conf_" + m.name),
                                      index=False)
        if args_rows:
            pd.DataFrame(args_rows).to_excel(
                writer, sheet_name="training_args", index=False)

    written = {"workbook": str(book)}
    curves = plot_curves(models, out / CURVES_PNG)
    if curves:
        written["curves_png"] = str(curves)
    if len(matrix):
        png = plot_per_class(matrix, models, out / PER_CLASS_PNG)
        if png:
            written["per_class_png"] = str(png)
    return written


def _run_date(m: ModelRun) -> str:
    """When the run was trained: from its notes if it has them, else the file."""
    from datetime import datetime

    if m.notes.get("trained"):
        return m.notes["trained"]
    src = m.results_csv or m.args_yaml
    if src and src.exists():
        return datetime.fromtimestamp(src.stat().st_mtime).strftime("%Y-%m-%d %H:%M")
    return "-"


def _fmt(v, spec: str = ".3f") -> str:
    return "-" if v is None else format(v, spec)


def write_history(models: list[ModelRun], winner, record_dir: Path) -> Path:
    """One document, regenerated: every run, what it tried, how it did.

    Ordered oldest first so it reads as the story of the model. The table is
    for scanning; the sections below it are for the report -- each carries the
    operator's own words from run_notes.md, which is the one thing no file
    Ultralytics writes can supply.
    """
    from datetime import datetime

    record_dir.mkdir(parents=True, exist_ok=True)
    ordered = sorted(models, key=_run_date)

    lines = [
        "# Model history",
        "",
        f"_Regenerated by stage 5 on {datetime.now():%Y-%m-%d %H:%M} from the "
        f"run folders themselves. Oldest first. Held-out figures are macro F1 "
        f"and top-1 on unseen images; a dash means that run has not been "
        f"through stage 4._",
        "",
        "| # | run | trained | base model | preset | best epoch | macro F1 | "
        "top-1 | what it was trying |",
        "|---|---|---|---|---|---|---|---|---|",
    ]
    for i, m in enumerate(ordered, 1):
        why = (m.notes.get("why") or "").replace("\n", " ").replace("|", "/")
        why = why if len(why) <= 90 else why[:87] + "..."
        base = m.notes.get("base_model") or str(m.args.get("model", "-"))
        base = Path(base).name if base and base != "-" else "-"
        preset = (m.notes.get("augmentation_preset", "").split("  (")[0]
                  or infer_preset(m.args))
        best = (f"{m.curve['best_epoch']}/{m.curve['epochs_total']}"
                if m.curve.get("best_epoch") else "-")
        mark = " **(chosen)**" if winner is m else ""
        lines.append(
            f"| {i} | {m.name}{mark} | {_run_date(m)} | {base} | {preset} | "
            f"{best} | {_fmt(m.overall.get('macro_f1'))} | "
            f"{_fmt(m.overall.get('top1_accuracy'), '.1%')} | {why or '_not recorded_'} |")

    lines += ["", "---", "", "## Run by run", ""]
    for m in ordered:
        lines += [f"### {m.name}" + ("  <- current choice" if winner is m else ""),
                  "",
                  f"- **Trained:** {_run_date(m)}",
                  f"- **Base model:** `{m.notes.get('base_model') or m.args.get('model', '-')}`",
                  f"- **Dataset:** `{m.notes.get('dataset') or m.args.get('data', '-')}`"]
        if m.notes.get("classes_left_out"):
            lines.append(f"- **Classes left out:** {m.notes['classes_left_out']}")
        if m.notes.get("augmentation_preset"):
            lines.append(f"- **Augmentation:** {m.notes['augmentation_preset']}")
        elif m.args:
            lines.append(f"- **Augmentation:** {infer_preset(m.args)}  "
                         f"(inferred from args.yaml)")
        if m.notes.get("hand-set_arguments", "none") != "none":
            lines.append(f"- **Hand-set:** {m.notes['hand-set_arguments']}")
        if m.notes.get("schedule"):
            lines.append(f"- **Schedule:** {m.notes['schedule']}")
        lines += ["", "**What changed, and why**", "",
                  m.notes.get("why") or "_Not recorded - this run predates run "
                  "notes, or none were entered._", ""]
        if m.curve:
            lines += ["**Training**", "",
                      f"- Best epoch {m.curve.get('best_epoch')} of "
                      f"{m.curve.get('epochs_total')}; {m.curve.get('verdict', '-')}"
                      + (f" - {weights_note(m.curve)}" if m.curve.get('verdict')
                         in ('overfitting', 'mild overfitting') else ""),
                      ""]
        if m.has_holdout:
            pc = m.per_class[m.per_class["support"] >= MIN_CHART_SUPPORT]
            weak = pc.nsmallest(3, "f1")
            lines += ["**Held-out**", "",
                      f"- Macro F1 {m.overall['macro_f1']:.3f} "
                      f"({describe_f1(m.overall['macro_f1'])}), top-1 "
                      f"{m.overall['top1_accuracy']:.1%} on "
                      f"{m.overall['images']:,} unseen images",
                      f"- Weakest well-sampled classes: "
                      + ", ".join(f"{r.label} ({r.f1:.2f})" for r in weak.itertuples()),
                      ""]
        else:
            lines += ["**Held-out:** not yet evaluated (run stage 4).", ""]

    path = record_dir / HISTORY_MD
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return path


def append_decision(models: list[ModelRun], winner, digest: dict,
                    record_dir: Path) -> Path:
    """One dated entry per comparison, appended. Never rewritten.

    A history document says where things stand; this says what was decided
    and why, at the time, in order. Those are the two questions a report asks.
    """
    from datetime import datetime

    record_dir.mkdir(parents=True, exist_ok=True)
    path = record_dir / DECISIONS_MD
    fresh = not path.exists()

    entry = [f"## {datetime.now():%Y-%m-%d %H:%M} - compared {len(models)} model(s)", ""]
    if winner:
        entry.append(f"**Recommended: {winner.name}**")
        entry.append("")
        for line in digest.get("why", []):
            entry.append(f"- {line}")
        if winner.notes.get("why"):
            entry += ["", f"_What it was trying:_ {winner.notes['why']}"]
    else:
        entry.append("**No recommendation** - see the caveats.")
    if digest.get("caveats"):
        entry += ["", "Caveats:"] + [f"- {c}" for c in digest["caveats"]]
    entry += ["", "Compared: " + ", ".join(m.name for m in models), "", "---", ""]

    with open(path, "a", encoding="utf-8") as fh:
        if fresh:
            fh.write("# Decisions\n\n_Appended by stage 5 each time a comparison "
                     "is committed. Newest at the bottom._\n\n---\n\n")
        fh.write("\n".join(entry) + "\n")
    return path


def _same_run(a: str, b: str) -> bool:
    try:
        return Path(a).resolve() == Path(b).resolve()
    except OSError:
        return str(a).strip() == str(b).strip()


def _sheet(name: str) -> str:
    """Excel caps sheet names at 31 characters and rejects a few others."""
    for ch in ("[", "]", ":", "*", "?", "/", chr(92)):
        name = name.replace(ch, "_")
    return name[:31]
