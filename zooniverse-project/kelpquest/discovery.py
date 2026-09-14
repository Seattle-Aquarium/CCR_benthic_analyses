"""
Reading a transect folder.

The tool is pointed at the folder of edited stills for one transect -- in
practice something like
``.../2025_01_28_EBM/downward/HERO1/transects/T6_shallow/edited`` -- and works
out what is in it and what the transect is called, so the run can be confirmed
before any of the expensive work starts.

Identity comes from the UTC transect CSV where one can be found: it carries
``Transect_ID``, ``Site_name``, ``Transect_number`` and ``Date`` as recorded on
the dive, which beats anything inferred from a folder name. The path-reading
below is the fallback for a transect that has not been through UTC.

Nothing here guesses silently: every inference is reported alongside where it
came from, and everything it produces is editable in the GUI.
"""

from __future__ import annotations

import os
import re
from dataclasses import dataclass, field
from pathlib import Path

from . import telemetry as telemetry_mod
from .logging_setup import get_logger

log = get_logger("discovery")

IMAGE_SUFFIXES = (".jpg", ".jpeg", ".png", ".tif", ".tiff")

#: Windows marks a file that is not really on disk -- OneDrive or Dropbox
#: online-only. It has the right name and the right size, but every read
#: streams from the network, which turns a ten-minute run into an afternoon and
#: looks exactly like a hang.
_FILE_ATTRIBUTE_OFFLINE = 0x1000
_FILE_ATTRIBUTE_RECALL_ON_OPEN = 0x40000
_FILE_ATTRIBUTE_RECALL_ON_DATA_ACCESS = 0x400000
_PLACEHOLDER_MASK = (
    _FILE_ATTRIBUTE_OFFLINE
    | _FILE_ATTRIBUTE_RECALL_ON_OPEN
    | _FILE_ATTRIBUTE_RECALL_ON_DATA_ACCESS
)

_DATE_RE = re.compile(r"(20\d{2})[_-](\d{2})[_-](\d{2})")
_TRANSECT_RE = re.compile(r"^T(\d+)", re.IGNORECASE)


@dataclass
class Transect:
    """What a transect folder turned out to contain."""

    folder: Path
    images: list[Path] = field(default_factory=list)
    site_name: str = ""
    survey_date: str = ""          # YYYY-MM-DD
    transect_number: str = ""      # "T6"
    transect_id_prefix: str = ""   # "EBM_W25"
    telemetry: telemetry_mod.Telemetry | None = None
    telemetry_matched: int = 0
    placeholders: list[Path] = field(default_factory=list)
    notes: list[str] = field(default_factory=list)

    @property
    def ok(self) -> bool:
        return bool(self.images) and not self.placeholders

    @property
    def transect_id(self) -> str:
        if self.transect_id_prefix and self.transect_number:
            return f"{self.transect_id_prefix}_{self.transect_number}"
        return self.transect_id_prefix or self.transect_number

    def summary(self) -> str:
        if not self.images:
            return f"No images found in {self.folder}"
        bits = [f"{len(self.images)} image(s)"]
        if self.site_name:
            bits.append(f"site {self.site_name}")
        if self.survey_date:
            bits.append(self.survey_date)
        if self.transect_id:
            bits.append(self.transect_id)
        if self.telemetry is not None:
            bits.append(f"{self.telemetry_matched}/{len(self.images)} with telemetry")
        return " · ".join(bits)


def is_placeholder(path: Path) -> bool:
    """True when the file is online-only and would stream on every read."""
    if os.name != "nt":
        return False
    try:
        attrs = os.stat(path, follow_symlinks=False).st_file_attributes
    except (OSError, AttributeError):
        return False
    return bool(attrs & _PLACEHOLDER_MASK)


def find_images(folder: Path, exclude: Path | None = None) -> list[Path]:
    """Stills directly in the folder, in name order.

    Deliberately not recursive: the patches this tool writes go in a subfolder,
    and a recursive scan would find them on the second run and cut patches out
    of patches.
    """
    exclude = exclude.resolve() if exclude else None
    out = []
    for p in sorted(folder.iterdir()):
        if not p.is_file() or p.suffix.lower() not in IMAGE_SUFFIXES:
            continue
        if exclude and p.parent.resolve() == exclude:
            continue
        out.append(p)
    return out


def guess_identity(folder: Path, images: list[Path]) -> tuple[str, str, str, list[str]]:
    """Site, date and transect number from the path, with a note for each.

    Our folder layout carries all three: a dated flight folder like
    ``2025_01_28_EBM``, and a transect folder like ``T6_shallow``. The image
    filenames repeat the date, which is the fallback when the flight folder has
    been renamed.
    """
    notes: list[str] = []
    site = date_s = number = ""

    for parent in [folder, *folder.parents][:6]:
        m = _TRANSECT_RE.match(parent.name)
        if m and not number:
            number = f"T{int(m.group(1))}"
            notes.append(f"transect {number} from folder '{parent.name}'")
        m = _DATE_RE.search(parent.name)
        if m and not date_s:
            date_s = f"{m.group(1)}-{m.group(2)}-{m.group(3)}"
            # "2025_01_28_EBM" -> site is whatever follows the date.
            tail = _DATE_RE.sub("", parent.name).strip("_- ")
            # Drop the descriptive half of names like "diver-ROV_EBM".
            if tail:
                site = tail.split("_")[-1]
            notes.append(f"date {date_s} from folder '{parent.name}'")

    if not date_s and images:
        m = _DATE_RE.search(images[0].name)
        if m:
            date_s = f"{m.group(1)}-{m.group(2)}-{m.group(3)}"
            notes.append(f"date {date_s} from the first image name")

    return site, date_s, number, notes


def scan(folder: str | Path, output_dir: Path | None = None,
         telemetry_csv: str | Path | None = None) -> Transect:
    """Look at a transect folder and report what a run would use.

    Never raises for a folder that is simply empty or wrong -- an empty result
    with a note is more useful on screen than a traceback.

    Pass `telemetry_csv` to use a particular UTC transect CSV; leave it out and
    one is looked for up the tree.
    """
    folder = Path(folder)
    t = Transect(folder=folder)
    if not folder.is_dir():
        t.notes.append(f"{folder} is not a folder")
        return t

    t.images = find_images(folder, exclude=output_dir)
    if not t.images:
        t.notes.append("No .jpg/.png stills directly in this folder — is this "
                       "the 'edited' folder for one transect?")
        return t

    t.placeholders = [p for p in t.images if is_placeholder(p)]
    if t.placeholders:
        t.notes.append(
            f"{len(t.placeholders)} of {len(t.images)} images are online-only "
            "placeholders. Right-click the folder and choose 'Always keep on "
            "this device', wait for it to download, then scan again."
        )

    t.site_name, t.survey_date, t.transect_number, notes = guess_identity(
        folder, t.images)
    t.notes.extend(notes)

    _attach_telemetry(t, telemetry_csv)
    if not t.transect_id_prefix:
        t.notes.append(
            "No transect id prefix — type the site and season part (like "
            "'EBM_W25') on the Transect page; the transect number is added "
            "for you.")
    return t


def _attach_telemetry(t: Transect, telemetry_csv: str | Path | None) -> None:
    """Overlay the UTC transect CSV's identity onto a scanned transect.

    The CSV wins over the path for every field it carries. It was written from
    the dive log, whereas a folder name is whatever somebody typed -- and when
    they disagree, the disagreement itself is worth saying out loud.
    """
    path = Path(telemetry_csv) if telemetry_csv else telemetry_mod.find_for(
        t.folder, t.transect_number)
    if not path:
        t.notes.append(
            "No UTC transect CSV found nearby. Site, date and transect are "
            "read off the folder names instead; pick a CSV to use the dive log.")
        return
    if not Path(path).is_file():
        t.notes.append(f"Telemetry CSV not found: {path}")
        return

    try:
        tel = telemetry_mod.load(path)
    except (OSError, ValueError) as exc:
        t.notes.append(f"Could not read {Path(path).name}: {exc}")
        return

    t.telemetry = tel
    t.notes.extend(tel.notes)
    t.notes.append(f"telemetry: {tel.summary()}")

    if tel.transect_number and t.transect_number and \
            tel.transect_number != t.transect_number:
        t.notes.append(
            f"the folder says {t.transect_number} but {Path(path).name} says "
            f"{tel.transect_number} — check you have the right CSV")
    if tel.transect_number:
        t.transect_number = tel.transect_number
    if tel.prefix:
        t.transect_id_prefix = tel.prefix
        t.notes.append(f"transect id '{t.transect_id}' from {Path(path).name}")
    if tel.site_code:
        t.site_name = tel.site_code
    if tel.survey_date:
        t.survey_date = tel.survey_date

    t.telemetry_matched = tel.match_count(p.name for p in t.images)
    if t.telemetry_matched < len(t.images):
        missing = len(t.images) - t.telemetry_matched
        t.notes.append(
            f"{missing} of {len(t.images)} stills have no telemetry row at "
            "their timestamp; those columns will be blank for them.")
