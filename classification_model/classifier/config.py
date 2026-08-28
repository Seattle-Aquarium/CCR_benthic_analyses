"""
Stage settings, and the state that carries between stages.

Every stage's inputs are one dataclass, so a stage can be driven identically
from the GUI, from the CLI, or from a test. ``PipelineState`` holds all four
plus the artefacts each stage produced -- that record is what lets stage N+1
prefill itself from stage N instead of the operator copying paths by hand.

Defaults here are the ones this project actually uses. Where a default encodes
a decision rather than a preference, the reasoning is in the comment.
"""

from __future__ import annotations

import json
from dataclasses import asdict, dataclass, field, fields
from pathlib import Path

#: Written next to the package. The leading dot plus ``_config`` suffix matches
#: the .gitignore rule, so per-machine paths never end up in a commit.
STATE_FILENAME = ".classifier_config.json"


def default_state_path() -> Path:
    return Path(__file__).resolve().parent.parent / STATE_FILENAME


# --------------------------------------------------------------------------
#  Stage 1 - extract
# --------------------------------------------------------------------------


@dataclass
class ExtractConfig:
    """Toolbox annotation CSV(s) -> a patch dataset on disk."""

    annotation_csvs: list[str] = field(default_factory=list)
    output_dir: str = ""

    val_frac: float = 0.2
    #: Classes with fewer than this many samples put everything in train. A val
    #: sample held out of a near-empty class measures nothing, and costs that
    #: class one of the few examples it has.
    min_val_count: int = 5
    jpeg_quality: int = 95
    seed: int = 42

    #: "Review" is the Zooniverse consensus outcome for "the volunteers could
    #: not agree", so it is a workflow state rather than a taxon.
    exclude_labels: list[str] = field(default_factory=lambda: ["Review"])

    #: ``OLD=>NEW`` substring rewrites for folders renamed since export.
    path_remaps: list[str] = field(default_factory=list)

    #: Flat ``<Label>/`` output with no train/val split. This is how a held-out
    #: evaluation set is made: it must never be trained or validated on.
    no_split: bool = False

    dry_run: bool = True


# --------------------------------------------------------------------------
#  Stage 2 - merge and balance
# --------------------------------------------------------------------------


@dataclass
class BalanceConfig:
    """Several patch datasets -> one deduplicated, balanced training set."""

    datasets: list[str] = field(default_factory=list)
    output_dir: str = ""

    #: When byte-identical patches are filed under *different* class labels,
    #: the copy from this dataset is the one kept -- and a kept copy carries
    #: its own dataset's label. Blank means no preference, in which case the
    #: winner falls out of train-before-val and path order, which is arbitrary
    #: from the operator's point of view. Set this to the dataset whose
    #: annotations you trust (typically the most recently verified batch).
    label_authority: str = ""

    #: Audited against the merge. Any training patch byte-identical to a
    #: held-out patch is a leak, resolved by dropping the *holdout* copy --
    #: see ``hashing.choose_drops`` for why that direction and not the other.
    holdout_dir: str = ""

    #: Move leaked held-out patches into ``<holdout>/_quarantined_leaked/``.
    #: On by default because the resolution is unambiguous and it moves rather
    #: than deletes; the alternative is discovering the leak three stages later,
    #: which is what happened last time. Only ever acts on a committed run.
    quarantine_holdout_leaks: bool = True

    #: Undersample any train class above ``cap``; augment any below ``floor``.
    #: Left as None, the class is taken as-is.
    cap: int | None = None
    floor: int | None = None

    #: Per-class overrides of ``cap``, for classes where the global cap throws
    #: away information that matters. ``{"KE_sieve": 4000}``.
    class_caps: dict[str, int] = field(default_factory=dict)

    seed: int = 42

    #: Report the combined inventory and duplicate audit, write nothing. On by
    #: default so a destructive merge is always a second, deliberate click.
    inventory_only: bool = True


# --------------------------------------------------------------------------
#  Stage 3 - train
# --------------------------------------------------------------------------

#: Ultralytics' own defaults fight this dataset in four specific ways, all
#: stemming from patches being centred on their annotated point.
#: Augmentation and regularisation, as four named settings.
#:
#: The constraint that shapes all of them: a patch is *centred on the
#: annotated point*, and the label describes what is at that centre. Any
#: augmentation free to crop, shift or erase the middle of the frame can
#: therefore change what the image shows without changing its label, which
#: teaches the model the wrong thing. Ultralytics' defaults do exactly that --
#: ``scale=0.5`` is a random-resized-crop, ``erasing=0.4`` is cutout, and
#: ``auto_augment="randaugment"`` includes both -- so the pipeline has always
#: turned them off. They stay off in every preset except the one that exists
#: to reproduce stock behaviour.
#:
#: What is left to vary is colour and orientation, which is genuinely useful
#: here: underwater colour shifts hard with depth and turbidity, and the
#: seafloor has no canonical "up".
AUGMENTATION_PRESETS: dict[str, dict] = {
    "standard": {
        "hsv_h": 0.015, "hsv_s": 0.7, "hsv_v": 0.4,
        "degrees": 0.0, "translate": 0.1, "shear": 0.0,
        "scale": 0.0, "erasing": 0.0, "auto_augment": None,
        "fliplr": 0.5, "flipud": 0.5,
        "mixup": 0.0, "cutmix": 0.0,
        "dropout": 0.0, "weight_decay": 0.0005,
    },
    "stronger": {
        # For a run that overfits. Every image is seen under more variation,
        # and the head is regularised harder. Rotation is safe because it
        # turns about the centre, so the annotated point stays put -- at the
        # cost of padded corners at angles off the square.
        "hsv_h": 0.03, "hsv_s": 0.8, "hsv_v": 0.5,
        "degrees": 180.0, "translate": 0.05, "shear": 0.0,
        "scale": 0.0, "erasing": 0.0, "auto_augment": None,
        "fliplr": 0.5, "flipud": 0.5,
        "mixup": 0.15, "cutmix": 0.0,
        "dropout": 0.2, "weight_decay": 0.001,
    },
    "lighter": {
        # For a run that underfits -- validation loss below training loss, or
        # still improving when the epochs ran out. Less distortion, so the
        # model can fit what is actually in front of it.
        "hsv_h": 0.01, "hsv_s": 0.4, "hsv_v": 0.3,
        "degrees": 0.0, "translate": 0.0, "shear": 0.0,
        "scale": 0.0, "erasing": 0.0, "auto_augment": None,
        "fliplr": 0.5, "flipud": 0.5,
        "mixup": 0.0, "cutmix": 0.0,
        "dropout": 0.0, "weight_decay": 0.0005,
    },
    # Nothing overridden: whatever Ultralytics ships, crop-and-erase included.
    # Here to measure the pipeline's choices against, not to train with.
    "ultralytics defaults": {},
}

#: What each preset is for, in the words the panel shows.
PRESET_NOTES = {
    "standard": "Colour jitter and flips. What every run so far has used.",
    "stronger": "More colour and full rotation, plus mixup, dropout 0.2 and "
                "double the weight decay. Use when a run overfits.",
    "lighter": "Less distortion. Use when a run underfits or was still "
               "improving when it stopped.",
    "ultralytics defaults": "Stock Ultralytics, including random-resized-crop "
                            "and random erasing - both of which can remove the "
                            "annotated point from its own patch.",
}

TRAIN_AUGMENTATION_OVERRIDES = AUGMENTATION_PRESETS["standard"]



@dataclass
class TrainConfig:
    """A merged dataset -> a trained classification model."""

    data_dir: str = ""

    #: YOLO26 small classification. ``optimizer=auto`` resolves to MuSGD here.
    model: str = "yolo26s-cls.pt"
    epochs: int = 100
    imgsz: int = 256
    seed: int = 42

    batch: int | None = None      # None -> Ultralytics default (16)
    device: str = ""              # "" -> auto, "0" -> first GPU, "cpu"

    #: Epochs without improvement before training stops. Ultralytics' default
    #: is 100, which with ``epochs=100`` can never fire -- so every run went to
    #: the last epoch regardless of when it stopped improving. 20 stops shortly
    #: after the model plateaus while leaving room for a noisy patch of epochs.
    #: ``best.pt`` is the best epoch either way; patience decides how long the
    #: run keeps going after it.
    patience: int = 20

    #: Which entry of ``AUGMENTATION_PRESETS`` to train with.
    augmentation: str = "standard"

    #: Anything else to hand Ultralytics, as ``{key: value}``. Applied last,
    #: so it overrides the preset. Validated against Ultralytics' own argument
    #: list before the run starts, because a typo here is otherwise a crash
    #: forty minutes in.
    extra_args: dict = field(default_factory=dict)

    project: str = ""
    name: str = ""

    #: Ultralytics requires identical class folders in train and val, or it
    #: trains to chance accuracy without ever raising. Classes present in only
    #: one split are moved aside for the run and moved back afterwards.
    exclude_mismatched: bool = True

    #: Report the dataset validation and stop. On by default.
    validate_only: bool = True


# --------------------------------------------------------------------------
#  Stage 4 - evaluate
# --------------------------------------------------------------------------


@dataclass
class EvalConfig:
    """A trained model + a held-out set -> the accuracy report."""

    model_path: str = ""
    eval_dir: str = ""
    output_dir: str = ""
    dataset_label: str = ""

    imgsz: int = 256
    batch: int = 32
    device: str = ""

    #: FP16 inference. Maps to Ultralytics' ``quantize=16``.
    fp16: bool = False

    #: Re-hash the holdout against the training set before reporting anything.
    #: This is the check that would have caught the 12.5% leak, so it defaults
    #: on and the stage refuses to report a number when it fails.
    verify_independence: bool = True
    train_dataset_dir: str = ""

    #: The field-verified accuracy the retrain is trying to beat, shown
    #: alongside the result so the comparison is never done from memory.
    baseline_accuracy: float | None = 47.38

    #: Inventory the held-out set and stop. On by default.
    preview_only: bool = True


# --------------------------------------------------------------------------
#  Whole-pipeline state
# --------------------------------------------------------------------------


@dataclass
class CompareConfig:
    """Several finished model runs -> one decision about which to keep."""

    #: Each entry is a model's output folder -- either the Ultralytics run
    #: itself or the folder stage 4 wrote its reports into. Both layouts are
    #: recognised, so this is whatever was typed into stage 3's Output folder.
    runs: list[str] = field(default_factory=list)
    output_dir: str = ""

    #: Reads and reports without writing the workbook. On by default like
    #: every other stage, though this one only ever writes a report.
    preview_only: bool = True


@dataclass
class PipelineState:
    """All four stage configs plus what each stage last produced.

    The ``last_*`` fields are the wiring between stages. They are written by a
    stage on success and read by ``advance()`` to prefill the next one, which
    is the whole reason the operator no longer has to remember which folder the
    previous step wrote to.
    """

    extract: ExtractConfig = field(default_factory=ExtractConfig)
    balance: BalanceConfig = field(default_factory=BalanceConfig)
    train: TrainConfig = field(default_factory=TrainConfig)
    evaluate: EvalConfig = field(default_factory=EvalConfig)
    compare: CompareConfig = field(default_factory=CompareConfig)

    last_extract_dir: str = ""
    last_holdout_dir: str = ""
    last_dataset_dir: str = ""
    last_weights: str = ""

    theme: str = "dark"

    # ---- feeding forward ---------------------------------------------

    def advance_from_extract(self, output_dir: str, no_split: bool) -> str:
        """Route an extraction to whichever stage consumes that kind of output.

        A no-split extraction is a held-out evaluation set, so it goes to stage
        4. A split extraction is training data, so it goes to stage 2's merge
        list. Getting this backwards is exactly the mistake that contaminates a
        holdout, so the routing is derived from ``no_split`` rather than chosen.
        """
        if no_split:
            self.last_holdout_dir = output_dir
            self.evaluate.eval_dir = output_dir
            self.balance.holdout_dir = output_dir
            if not self.evaluate.dataset_label:
                self.evaluate.dataset_label = Path(output_dir).name
            return "evaluate"

        self.last_extract_dir = output_dir
        if output_dir not in self.balance.datasets:
            self.balance.datasets.append(output_dir)
        return "balance"

    def advance_from_balance(self, output_dir: str) -> str:
        self.last_dataset_dir = output_dir
        self.train.data_dir = output_dir
        self.evaluate.train_dataset_dir = output_dir
        return "train"

    def advance_from_evaluate(self, report_dir: str) -> str:
        """Add a freshly evaluated model to the comparison list.

        Stage 5 reads the folder stage 4 wrote its reports into, so that is
        what gets handed on -- not the weights. Appended rather than replacing
        the list, because comparing means keeping the previous candidates.
        """
        if report_dir and report_dir not in self.compare.runs:
            self.compare.runs.append(report_dir)
        if not self.compare.output_dir:
            self.compare.output_dir = report_dir
        return "compare"

    def advance_from_train(self, weights: str) -> str:
        self.last_weights = weights
        self.evaluate.model_path = weights
        if not self.evaluate.output_dir and weights:
            self.evaluate.output_dir = str(Path(weights).parent.parent)
        return "evaluate"

    # ---- persistence -------------------------------------------------

    def save(self, path: str | Path | None = None) -> Path:
        path = Path(path or default_state_path())
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(asdict(self), indent=2), encoding="utf-8")
        return path

    @classmethod
    def load(cls, path: str | Path | None = None) -> "PipelineState":
        """Load saved state, tolerating a file written by an older version.

        Unknown keys are dropped and missing ones keep their default, so adding
        a setting never strands someone on an unreadable config.
        """
        path = Path(path or default_state_path())
        if not path.is_file():
            return cls()
        try:
            raw = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, ValueError):
            return cls()
        if not isinstance(raw, dict):
            return cls()

        sections = {"extract": ExtractConfig, "balance": BalanceConfig,
                    "train": TrainConfig, "evaluate": EvalConfig,
                    "compare": CompareConfig}
        kwargs = {}
        for name, klass in sections.items():
            kwargs[name] = _coerce(klass, raw.get(name))
        for f in fields(cls):
            if f.name not in sections and f.name in raw:
                kwargs[f.name] = raw[f.name]
        return cls(**kwargs)


def _coerce(klass, raw):
    """Build a config dataclass from a dict, ignoring keys it does not have."""
    if not isinstance(raw, dict):
        return klass()
    known = {f.name for f in fields(klass)}
    return klass(**{k: v for k, v in raw.items() if k in known})
