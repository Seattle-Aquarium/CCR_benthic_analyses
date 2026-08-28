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
CURVES_PNG = "model_comparison_curves.png"
PER_CLASS_PNG = "model_comparison_per_class.png"

#: A class scoring below this on F1 is called out as weak. 0.60 is the
#: threshold the existing hand-built comparison used, kept so the numbers stay
#: comparable with the spreadsheets already circulating.
WEAK_F1 = 0.60

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
        results_csv=_first(root, ["results.csv", "*/results.csv"]),
        args_yaml=_first(root, ["args.yaml", "*/args.yaml"]),
        weights=_first(root, ["weights/best.pt", "*/weights/best.pt"]),
        detail_csv=_first(root, ["*_predictions_detail.csv",
                                 "*/*_predictions_detail.csv"]),
        report_csv=_first(root, ["*_accuracy_report.csv",
                                 "*/*_accuracy_report.csv"]),
    )


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
            # Which side is holding the class back is the difference between
            # "needs more examples" and "needs a competitor disambiguated".
            "bottleneck": ("-" if not (tp + fp + fn) else
                           "recall" if recall < precision else
                           "precision" if precision < recall else "balanced"),
        })

    df = pd.DataFrame(rows)
    return df.sort_values("support", ascending=False).reset_index(drop=True)


def overall_metrics(detail: pd.DataFrame, per_class: pd.DataFrame) -> dict:
    """Top-1 plus the macro and weighted averages, in one dict."""
    seen = per_class[per_class["support"] > 0]
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
            f"patches. Try stronger augmentation, more weight decay, or "
            f"simply stopping sooner.")
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
            "optimizer", "lr0", "weight_decay", "device", "seed")
    return {k: raw.get(k) for k in keep if k in raw}


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

    Ranked on held-out top-1 accuracy, because that is the only number in the
    pipeline measured on images no model has trained on. Macro F1 breaks a
    near-tie: it weights a class with 40 examples the same as one with 4,000,
    so it is the question "does this model still know the rare taxa" asked
    numerically. Where the two disagree the disagreement is reported rather
    than resolved silently -- which of the two matters depends on whether the
    survey needs overall cover or rare-species detection, and that is not a
    decision this code should make on its own.
    """
    scored = [m for m in models if m.has_holdout]
    if not scored:
        return None, ["No model has a held-out evaluation, so none can be ranked."]
    if len(scored) == 1:
        return scored[0], [
            f"Only {scored[0].name} has a held-out evaluation, so there is "
            f"nothing to compare it against."]

    by_top1 = sorted(scored, key=lambda m: -m.overall["top1_accuracy"])
    by_macro = sorted(scored, key=lambda m: -m.overall["macro_f1"])
    best, runner = by_top1[0], by_top1[1]
    margin = best.overall["top1_accuracy"] - runner.overall["top1_accuracy"]

    lines = [
        f"Ranked on held-out top-1 accuracy ({best.overall['images']:,} "
        f"unseen images):"]
    for i, m in enumerate(by_top1, 1):
        lines.append(
            f"   {i}. {m.name}   top-1 {m.overall['top1_accuracy']:.2%}   "
            f"macro-F1 {m.overall['macro_f1']:.3f}   "
            f"{m.overall['weak_classes']} class(es) below F1 {WEAK_F1}")

    winner = best
    if margin < TIE_THRESHOLD and by_macro[0] is not best:
        winner = by_macro[0]
        lines.append(
            f"{best.name} leads on top-1 by only {margin:.2%}, which on this "
            f"holdout is about {round(margin * best.overall['images'])} images "
            f"-- too close to call. {winner.name} has the better macro F1 "
            f"({winner.overall['macro_f1']:.3f} against "
            f"{best.overall['macro_f1']:.3f}), meaning it handles the rare "
            f"classes better, so it is the recommendation.")
    elif by_macro[0] is not best:
        lines.append(
            f"Note the disagreement: {best.name} wins on overall accuracy but "
            f"{by_macro[0].name} has the better macro F1 "
            f"({by_macro[0].overall['macro_f1']:.3f} against "
            f"{best.overall['macro_f1']:.3f}). {best.name} is the better "
            f"choice for overall percent-cover; {by_macro[0].name} is the "
            f"better choice if the rare taxa matter as much as the common "
            f"ones.")
    else:
        lines.append(
            f"{best.name} leads on both overall accuracy and macro F1, so the "
            f"choice is unambiguous.")

    verdict = winner.curve.get("verdict")
    gap = winner.curve.get("final_val_minus_train")
    if verdict in ("overfitting", "mild overfitting"):
        lines.append(
            f"Caveat: {winner.name} shows {verdict} - by the last epoch its "
            f"validation loss was {gap:.2f} above its training loss. It still "
            f"wins on unseen data, so this is not disqualifying, but a "
            f"better-regularised run of the same recipe would probably beat "
            f"it.")
    elif verdict in ("underfitting", "still improving"):
        lines.append(
            f"Caveat: {winner.name} was {verdict} when the run ended, so it is "
            f"winning without having finished learning. Training it longer is "
            f"the cheapest improvement available.")

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
#  The stage
# --------------------------------------------------------------------------

def run(cfg, *, progress: ProgressCB | None = None, cancel=None) -> StageResult:
    started = time.time()
    return guard(lambda: _run(cfg, progress, cancel), started)


def _run(cfg, progress, cancel) -> StageResult:
    res = StageResult(preview=cfg.preview_only)
    st = Stages(progress).plan(read=45, analyse=25, write=30)

    runs = [r for r in cfg.runs if str(r).strip()]
    if len(runs) < 2:
        res.errors.append("Select at least two model run folders to compare.")
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

    for line in rationale:
        res.say(line)
    if shared and len(matrix):
        flipped = matrix[matrix["best_minus_worst"] > 0.10]
        if len(flipped):
            res.say(f"{len(flipped)} class(es) differ by more than 0.10 F1 "
                    f"between models - worth reading the per-class sheet, "
                    f"since a class the survey depends on may have moved the "
                    f"other way from the headline number.")

    for m in models:
        notes = improvement_notes(m, shared)
        if notes:
            log.info(f"--- {m.name} ---")
            for n in notes:
                log.info(f"    {n}")

    if cfg.preview_only:
        res.say("Nothing written - clear 'Preview only' to write the report.")
        return res

    # ---- write ------------------------------------------------------
    out = Path(cfg.output_dir)
    out.mkdir(parents=True, exist_ok=True)
    written = _write_workbook(models, matrix, winner, rationale, shared, out)
    res.outputs.update(written)
    st.finish("write", "report written")

    res.say(f"Comparison written to {written['workbook']}")
    for key in ("curves_png", "per_class_png"):
        if written.get(key):
            res.say(f"   {written[key]}")
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


def _sheet(name: str) -> str:
    """Excel caps sheet names at 31 characters and rejects a few others."""
    for ch in ("[", "]", ":", "*", "?", "/", chr(92)):
        name = name.replace(ch, "_")
    return name[:31]
