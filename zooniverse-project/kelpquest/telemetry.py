"""
The UTC transect CSV, as the source of truth for what a transect is.

`CCR_ROV_survey_methods/UTC` writes one CSV per transect with a row per second:

    Date, Time, Site_name, Transect_number, Transect_ID, Mode, Battery_*,
    Latitude, Longitude, EKFlat, EKFlon, DVL*, Altitude, Depth, Depth_std,
    Depth_Source, Heading, Velocity_mps, Width, Area_m2, Distance, NEDz, VFR_alt

Two things follow from that, and both matter here.

**It already knows the transect's identity.** `Transect_ID` is exactly the
`EBM_W25_T6` form we stamp on subjects, and `Site_name`, `Transect_number` and
`Date` come with it. Read from the CSV, none of that is guessed from a folder
name or typed in twice.

**It can be joined to the stills.** A still is named `YYYY_MM_DD_HH-MM-SS.jpg`
and the telemetry ticks once a second, so an exact timestamp match gives every
image its depth, altitude, position and heading. On the transect this was
written against, all 58 stills matched.
"""

from __future__ import annotations

import csv
import re
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path

from .logging_setup import get_logger

log = get_logger("telemetry")

#: `2025_01_28_11-34-11.jpg`, the name a UTC-processed still carries.
_STILL_TS = re.compile(r"(\d{4})_(\d{2})_(\d{2})[_-](\d{2})-(\d{2})-(\d{2})")

#: Trailing transect number on an id like `EBM_W25_T6`.
_ID_TAIL = re.compile(r"_T\d+$", re.IGNORECASE)

#: Where UTC puts them, relative to the flight folder. Tried in order.
_SEARCH_DIRS = (
    Path("logs") / "transects" / "transects",
    Path("logs") / "transects",
    Path("logs"),
)

#: Telemetry column -> (published name, decimal places). A curated subset: the
#: whole CSV is 31 columns, most of them about the vehicle rather than the
#: seafloor, and every one of them would land on every subject.
#:
#: The rounding is not cosmetic. The CSV carries full float repr -- a depth of
#: -5.179999828338623, a heading of 89.5193130767966 -- and a pressure sensor
#: good to a centimetre reported to the femtometre invites somebody downstream
#: to believe it. Latitude keeps 7 places, which is about 1 cm.
COLUMNS = {
    "Depth": ("depth_m", 2),
    "Altitude": ("altitude_m", 2),
    "Latitude": ("latitude", 7),
    "Longitude": ("longitude", 7),
    "Heading": ("heading_deg", 1),
    "Velocity_mps": ("velocity_mps", 3),
    "Width": ("image_width_m", 2),
    "Area_m2": ("area_m2", 2),
}

#: The names, in publication order, for whoever writes the header.
FIELDS = tuple(name for name, _dp in COLUMNS.values())


def still_timestamp(name: str) -> datetime | None:
    """The moment a still was taken, from its filename."""
    m = _STILL_TS.search(str(name))
    if not m:
        return None
    try:
        return datetime(*(int(g) for g in m.groups()))
    except ValueError:
        return None


@dataclass
class Telemetry:
    """One transect's telemetry, indexed by the second it was recorded."""

    path: Path
    transect_id: str = ""
    site_name: str = ""          # "Elliott Bay Marina"
    site_code: str = ""          # "EBM", taken from the transect id
    transect_number: str = ""    # "T6"
    survey_date: str = ""        # YYYY-MM-DD
    rows: dict[datetime, dict[str, str]] = field(default_factory=dict)
    notes: list[str] = field(default_factory=list)

    @property
    def prefix(self) -> str:
        """`EBM_W25` — the part of the id that is not the transect number."""
        return _ID_TAIL.sub("", self.transect_id)

    @property
    def span(self) -> tuple[datetime, datetime] | None:
        if not self.rows:
            return None
        keys = sorted(self.rows)
        return keys[0], keys[-1]

    def for_still(self, name: str) -> dict[str, str]:
        """Published telemetry for one still, or {} when it does not match.

        An exact second match only. Interpolating would invent a position for
        an image that the telemetry never covered, and a blank column is easier
        to notice than a plausible wrong number.
        """
        ts = still_timestamp(name)
        if ts is None:
            return {}
        row = self.rows.get(ts)
        if row is None:
            return {}
        out = {}
        for source, (published, places) in COLUMNS.items():
            value = (row.get(source) or "").strip()
            if not value:
                continue
            try:
                out[published] = f"{round(float(value), places):.{places}f}"
            except ValueError:
                out[published] = value      # a non-numeric column, kept as-is
        return out

    def match_count(self, names) -> int:
        return sum(1 for n in names if self.for_still(n))

    def summary(self) -> str:
        bits = [self.path.name]
        if self.transect_id:
            bits.append(self.transect_id)
        if self.site_name:
            bits.append(self.site_name)
        bits.append(f"{len(self.rows):,} rows")
        span = self.span
        if span:
            bits.append(f"{span[0]:%H:%M:%S}–{span[1]:%H:%M:%S}")
        return " · ".join(bits)


def load(path: str | Path) -> Telemetry:
    """Read a UTC transect CSV. Raises only if the file cannot be read."""
    path = Path(path)
    tel = Telemetry(path=path)
    with path.open(newline="", encoding="utf-8-sig") as fh:
        reader = csv.DictReader(fh)
        if not reader.fieldnames or "Time" not in reader.fieldnames:
            raise ValueError(
                f"{path.name} does not look like a UTC transect CSV — it has no "
                "Time column.")
        for row in reader:
            stamp = _row_timestamp(row)
            if stamp is not None:
                tel.rows.setdefault(stamp, row)
            if not tel.transect_id:
                tel.transect_id = (row.get("Transect_ID") or "").strip()
                tel.site_name = (row.get("Site_name") or "").strip()
                number = (row.get("Transect_number") or "").strip()
                if number:
                    # Written as "6" in the CSV; we use "T6" everywhere else.
                    tel.transect_number = f"T{int(float(number))}"
                tel.survey_date = (row.get("Date") or "").strip()

    if tel.transect_id:
        tel.site_code = tel.transect_id.split("_")[0]
    if not tel.transect_number and tel.transect_id:
        m = re.search(r"_T(\d+)$", tel.transect_id, re.IGNORECASE)
        if m:
            tel.transect_number = f"T{int(m.group(1))}"
    if not tel.rows:
        tel.notes.append(f"{path.name} has no rows with a readable Date/Time.")
    log.info(f"Telemetry: {tel.summary()}")
    return tel


def _row_timestamp(row: dict) -> datetime | None:
    date_s = (row.get("Date") or "").strip()
    time_s = (row.get("Time") or "").strip()
    if not (date_s and time_s):
        return None
    for fmt in ("%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S",
                "%Y-%m-%d %H:%M:%S.%f"):
        try:
            return datetime.strptime(f"{date_s} {time_s}", fmt)
        except ValueError:
            continue
    return None


# --------------------------------------------------------------------------
#  Finding the file
# --------------------------------------------------------------------------


def candidates(transect_folder: str | Path) -> list[Path]:
    """Transect CSVs anywhere up the tree from a transect folder.

    UTC writes them under the *flight* folder, several levels above the stills
    (`2025_01_28_EBM/logs/transects/transects/EBM_W25_T6.csv` against
    `.../transects/T6_shallow/edited`), so this walks up rather than expecting
    a fixed relative path.
    """
    folder = Path(transect_folder)
    found: list[Path] = []
    for parent in [folder, *folder.parents][:8]:
        for rel in _SEARCH_DIRS:
            directory = parent / rel
            if not directory.is_dir():
                continue
            for csv_path in sorted(directory.glob("*.csv")):
                if csv_path not in found:
                    found.append(csv_path)
            if found:
                return found
    return found


def find_for(transect_folder: str | Path,
             transect_number: str = "") -> Path | None:
    """The transect CSV for this transect, if one is sitting up the tree.

    With a transect number in hand the match is unambiguous: a file whose name
    ends `_T6`. Without one, a lone candidate is taken and anything more is
    left for the operator to pick, because uploading transect 6's stills under
    transect 1's identity is not a mistake that shows up later.
    """
    found = candidates(transect_folder)
    if not found:
        return None
    number = re.sub(r"[^0-9]", "", transect_number or "")
    if number:
        tail = re.compile(rf"_T0*{number}$", re.IGNORECASE)
        exact = [p for p in found if tail.search(p.stem)]
        if len(exact) == 1:
            return exact[0]
        if exact:
            log.warning(f"{len(exact)} transect CSVs match T{number}; "
                        "choose one on the Transect page.")
            return None
    if len(found) == 1:
        return found[0]
    log.info(f"{len(found)} transect CSVs found near this folder; "
             "choose one on the Transect page.")
    return None
