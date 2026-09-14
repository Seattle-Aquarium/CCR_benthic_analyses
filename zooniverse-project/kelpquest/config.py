"""
Run settings, and where they are remembered between sessions.

Defaults here are the ones our finished transects were actually built with, so
a first run on a new transect matches the existing ones without anybody having
to remember the numbers. Anything a person would plausibly want to change is
reachable from the GUI rather than only from this file.
"""

from __future__ import annotations

import json
import os
from dataclasses import asdict, dataclass, field, fields
from pathlib import Path

#: Settings live beside the project, in the same dotfile style the classifier
#: and the older scripts already use.
CONFIG_PATH = Path(__file__).resolve().parent.parent / ".kelpquest_config.json"


# --------------------------------------------------------------------------
#  Stage settings
# --------------------------------------------------------------------------


@dataclass
class SampleSettings:
    """How annotation points are scattered over each source image.

    50 points an image on a 224 px patch, and a 224 px band around the edge of
    the frame that no point lands in -- one whole patch width, so the patch
    itself is always fully inside the image. The Zooniverse crop is wider than
    that (3.5x the patch, so 392 px either side), and the handful of crops that
    would overhang are slid back inside rather than clipped.
    """

    points_per_image: int = 50
    patch_size: int = 224
    #: One patch width. Kept separate from patch_size so it can be widened
    #: without changing what the classifier sees.
    margin: int = 224
    #: Fixed so a re-run of the same transect lands on the same points. Change
    #: it to draw a genuinely different sample.
    seed: int = 42


@dataclass
class PatchSettings:
    """The crop that a volunteer actually sees."""

    #: Crop side as a multiple of the patch size. 3.5 gives enough surrounding
    #: context to judge what the point is sitting on.
    scale: float = 3.5
    jpeg_quality: int = 100
    #: Classify inside the cutting pass. Both halves need the same decoded
    #: still and the finished JPEG carries the label, so doing them together
    #: encodes each patch once instead of twice. Untick to cut unlabelled
    #: patches and score them as a separate step.
    classify_in_same_pass: bool = True


@dataclass
class ClassifySettings:
    """Our own classifier, run over the tight patch at each point."""

    weights: str = ""
    labelset: str = ""
    imgsz: int = 256
    batch: int = 32
    #: "cpu", or "0" for the first CUDA device. Left on CPU by default because
    #: that is what every laptop here has.
    device: str = "cpu"
    #: Burn the predicted label onto the patch image, as the Toolbox-sourced
    #: patches did. Volunteers are asked to agree or disagree with it.
    burn_label: bool = True


@dataclass
class UploadSettings:
    """Zooniverse subject creation."""

    subject_set_name: str = ""
    subject_set_id: str = ""
    checkpoint_every: int = 100
    #: Seconds between subject saves, to keep API pressure down.
    sleep: float = 0.1
    #: 0 = no limit. Small numbers are for testing against a scratch set.
    limit: int = 0


@dataclass
class ExportSettings:
    """Downloading the volunteers' classifications back out again.

    Several subject sets, not one: a subject moves as it retires -- yes/no,
    then multiple choice, then an expert set -- and its classifications stay
    with whichever set it was in at the time. Exporting only the set a
    transect was uploaded into makes every subject that has moved on look
    like one nobody has classified.
    """

    #: Free text, so ids can be pasted from a spreadsheet column. Commas,
    #: spaces and newlines all separate; see export.parse_ids.
    subject_set_ids: str = ""
    output_dir: str = ""
    combined_csv: str = ""


@dataclass
class RejoinSettings:
    """Bringing the answers back to the Toolbox annotations."""

    toolbox_csv: str = ""
    classifications_csv: str = ""
    annotations_json: str = ""
    output_dir: str = ""
    #: Which of the four logical workflows to count. All four by default --
    #: leaving one out is for diagnosing a disagreement, not for normal runs.
    use_yesno: bool = True
    use_yesno_expert: bool = True
    use_multi: bool = True
    use_multi_expert: bool = True


@dataclass
class ReportSettings:
    """The Excel summary built from an export."""

    export_csv: str = ""
    output_dir: str = ""
    workflow_id: str = ""
    source_image: str = ""


@dataclass
class AppConfig:
    transect_folder: str = ""
    telemetry_csv: str = ""
    output_dir: str = ""
    site_name: str = ""
    survey_date: str = ""
    transect_number: str = ""
    #: Everything but the transect number -- "EBM_W25". Asked for once; the
    #: number is appended, so transect 6 cannot be uploaded as transect 1
    #: because somebody edited half the id.
    transect_id_prefix: str = ""

    sample: SampleSettings = field(default_factory=SampleSettings)
    patch: PatchSettings = field(default_factory=PatchSettings)
    classify: ClassifySettings = field(default_factory=ClassifySettings)
    upload: UploadSettings = field(default_factory=UploadSettings)
    export: ExportSettings = field(default_factory=ExportSettings)
    report: ReportSettings = field(default_factory=ReportSettings)
    rejoin: RejoinSettings = field(default_factory=RejoinSettings)

    theme: str = "dark"

    @property
    def transect_id(self) -> str:
        """`EBM_W25` + `T6` -> `EBM_W25_T6`, the id stamped on every subject."""
        prefix = (self.transect_id_prefix or "").strip().rstrip("_")
        number = (self.transect_number or "").strip()
        if prefix and number:
            return f"{prefix}_{number}"
        return prefix or number

    # ---- persistence -------------------------------------------------

    def save(self, path: Path | None = None) -> None:
        """Best effort: a settings file we cannot write is not a failed run."""
        target = Path(path or CONFIG_PATH)
        try:
            target.write_text(json.dumps(asdict(self), indent=2), encoding="utf-8")
        except OSError:
            pass

    @classmethod
    def load(cls, path: Path | None = None) -> AppConfig:
        source = Path(path or CONFIG_PATH)
        try:
            raw = json.loads(source.read_text(encoding="utf-8"))
        except (OSError, ValueError):
            return cls()
        return _from_dict(cls, raw)


#: Which AppConfig fields hold a nested settings dataclass. `field.type` is a
#: string here (the module uses postponed annotations), so the mapping is
#: written out rather than inferred.
_NESTED = {
    "sample": SampleSettings,
    "patch": PatchSettings,
    "classify": ClassifySettings,
    "upload": UploadSettings,
    "export": ExportSettings,
    "report": ReportSettings,
    "rejoin": RejoinSettings,
}


def _from_dict(cls, raw: dict):
    """Rebuild a dataclass from JSON, ignoring keys it no longer has.

    Settings files outlive the code that wrote them. A field that was renamed
    or dropped should cost the stale value, not the whole file -- otherwise one
    old key resets every other setting the user had tuned.
    """
    if not isinstance(raw, dict):
        return cls()
    kwargs = {}
    for f in fields(cls):
        if f.name not in raw:
            continue
        value = raw[f.name]
        sub = _NESTED.get(f.name) if cls is AppConfig else None
        kwargs[f.name] = _from_dict(sub, value) if sub and isinstance(value, dict) else value
    try:
        return cls(**kwargs)
    except TypeError:
        return cls()


# --------------------------------------------------------------------------
#  Paths
# --------------------------------------------------------------------------


def cache_root() -> Path:
    """Scratch space, kept out of the Dropbox-synced transect folders.

    Finished patches belong beside the transect they came from -- they are the
    product, and the upload log has to travel with them. Everything disposable
    goes here instead, so nothing temporary is pushed to the whole team.
    """
    base = Path(os.environ.get("LOCALAPPDATA") or os.path.expanduser("~"))
    return base / "kelpquest_cache"


def log_path() -> Path:
    return cache_root() / "kelpquest_log.txt"


def default_output_dir(transect_folder: str | Path) -> Path:
    """Where patches land when the user has not chosen somewhere else.

    A subfolder of the transect, so patches stay with the stills they were cut
    from and a second transect cannot overwrite the first one's upload log.
    """
    return Path(transect_folder) / "zooniverse_patches"
