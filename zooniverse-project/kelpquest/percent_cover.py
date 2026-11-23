"""
Finalized annotations into percent cover per image, with the telemetry.

This is what the whole pipeline is for. A point annotation says "this one
pixel is sugar kelp"; an ecologist needs "this image is 14% sugar kelp, at
8.2 m depth, 47.6195 N". That is a grouping and a join, and
``scripts/percent_cover_telemetry.py`` already does both -- so, as with the
rejoin and the report, this loads that module and calls its functions rather
than keeping a second copy of them.

The two steps it performs:

1. **Percent cover.** Group the annotation points by image, count each label,
   divide by *that image's own* point total. Not by 50: an image that lost a
   point to a failed patch would otherwise read as 2% short on everything.
2. **Telemetry join.** Parse the ``YYYY_MM_DD_HH-MM-SS`` timestamp out of each
   image name and left-join the dive telemetry row for that second. Left, so
   an image with no telemetry still appears, with empty columns and a count in
   the report, rather than vanishing from the table.

Several files of each are accepted and concatenated first. A site-season
table is one row per image across a whole site -- the finalized ``CNL_S24``
table is Centennial's six 2024 transects together -- and concatenating before
grouping is what makes that one table rather than six.

**The annotations wanted here are the finalized ones**: exported from Toolbox
after the Zooniverse answers have been brought back in (stage 8) and a person
has settled whatever was left. Run on an unfinished set, every point still
labelled ``Review`` counts in the denominator and quietly deflates every real
class, which is why the result reports how many of those there were.
"""

from __future__ import annotations

import importlib.util
import sys
import threading
from collections.abc import Callable, Sequence
from dataclasses import dataclass, field
from pathlib import Path

from .logging_setup import get_logger

log = get_logger("cover")

#: The tool holding the grouping and the join.
TOOL = (Path(__file__).resolve().parent.parent / "scripts"
        / "percent_cover_telemetry.py")

#: The one label that is not an observation. `Review` is what the rules leave
#: on a point nobody has settled yet, so a point carrying it is unfinished
#: work rather than a thing on the seafloor.
#:
#: `unknown` is NOT in this list. It is a real category -- "I looked, and it
#: cannot be determined from the image" is a finding about the seafloor, and
#: dropping it would quietly inflate every other class.
REVIEW_LABEL = "Review"

#: Where the unfinished points are listed, for working through in Toolbox.
REVIEW_NAME = "points_needing_review.csv"


@dataclass
class CoverResult:
    output_csv: Path | None = None
    images: int = 0
    points: int = 0
    matched: int = 0
    unmatched: int = 0
    unparsed_names: int = 0
    telemetry_rows: int = 0
    duplicate_telemetry: int = 0
    labels: list[str] = field(default_factory=list)
    #: label -> share of all points, for the summary.
    label_share: dict[str, float] = field(default_factory=dict)
    #: How many points are still `Review`, and which images hold them.
    review_points: int = 0
    review_images: dict[str, int] = field(default_factory=dict)
    #: Where those points were listed, so they can be worked through.
    review_csv: Path | None = None
    excluded: list[str] = field(default_factory=list)
    dry_run: bool = False
    cancelled: bool = False
    lines: list[str] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)
    errors: list[str] = field(default_factory=list)

    @property
    def ok(self) -> bool:
        return not self.errors and not self.cancelled

    def summary(self) -> str:
        head = ("Checked only - nothing was written." if self.dry_run
                else f"{self.images:,} image(s) of percent cover")
        out = [head]
        out += self.lines
        if self.label_share:
            out += ["", "Cover across every point:"]
            for code, share in sorted(self.label_share.items(),
                                      key=lambda kv: -kv[1])[:12]:
                out.append(f"  {code:<14} {share:7.2%}")
            if len(self.label_share) > 12:
                out.append(f"  … and {len(self.label_share) - 12} more")
        return "\n".join(out)


def available() -> bool:
    return TOOL.is_file()


def _load_tool():
    """Import scripts/percent_cover_telemetry.py as a module."""
    if not TOOL.is_file():
        raise FileNotFoundError(
            f"The percent-cover tool is missing: {TOOL}. It ships alongside "
            "this app in the scripts folder.")
    name = "kelpquest_percent_cover_telemetry"
    if name in sys.modules:
        return sys.modules[name]
    spec = importlib.util.spec_from_file_location(name, TOOL)
    if spec is None or spec.loader is None:
        raise ImportError(f"Could not load {TOOL}")
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    # It configures the root logger to stdout on import, and a windowed build
    # has no stdout. Ours reaches the GUI's log pane.
    module.log = get_logger("cover")
    return module


def default_csv_name(transect_id: str = "") -> str:
    """`CNL_S24_T1_percent_cover_telemetry.csv`, matching the existing files."""
    stem = (transect_id or "").strip().strip("_")
    return f"{stem}_percent_cover_telemetry.csv" if stem \
        else "percent_cover_telemetry.csv"


def _shared_stem(paths: Sequence[str | Path]) -> str:
    """What the chosen files have in common: `CNL_S24_T1..T6` -> `CNL_S24`.

    Whole underscore-separated parts only, so six transects of one site-season
    collapse to the site-season and a single file keeps its own name.
    """
    stems = [Path(q).stem for q in paths if str(q).strip()]
    if not stems:
        return ""
    if len(stems) == 1:
        return stems[0]
    shared: list[str] = []
    for bits in zip(*(stem.split("_") for stem in stems)):
        if len(set(bits)) != 1:
            break
        shared.append(bits[0])
    return "_".join(shared)


def suggest_name(annotation_csvs: Sequence[str | Path] = (),
                 telemetry_csvs: Sequence[str | Path] = (),
                 fallback: str = "") -> tuple[str, str]:
    """The file name to offer, and a phrase saying what it was taken from.

    Named after *this stage's own inputs*, not after whatever transect stage 1
    was last pointed at. The stages do not have to be run in order -- percent
    cover is usually built long after, for a transect that may have been
    patched and uploaded on somebody else's machine -- and a name carried over
    from stage 1 would label the table with the wrong transect while looking
    entirely deliberate.

    So the transect id is the fallback rather than the source, and what comes
    back is only a suggestion: the name is an editable field, because no file
    naming convention survives contact with six field seasons.
    """
    stem = _shared_stem(annotation_csvs)
    if stem:
        return default_csv_name(stem), "the annotation file(s)"
    stem = _shared_stem(telemetry_csvs)
    if stem:
        return default_csv_name(stem), "the telemetry file(s)"
    stem = (fallback or "").strip().strip("_")
    if stem:
        return default_csv_name(stem), "the transect id from stage 1"
    return default_csv_name(), "nothing yet - choose the annotations"


def build(annotation_csvs: Sequence[str | Path],
          telemetry_csvs: Sequence[str | Path],
          output_csv: str | Path,
          exclude_labels: Sequence[str] = (),
          dry_run: bool = True,
          progress: Callable[[float, str], None] | None = None,
          cancel: threading.Event | None = None) -> CoverResult:
    """Percent cover per image, joined to the telemetry for that second."""
    tool = _load_tool()
    result = CoverResult(dry_run=dry_run)

    annotations = [Path(p) for p in annotation_csvs]
    telemetry_paths = [Path(p) for p in telemetry_csvs]
    if not annotations:
        raise ValueError("Choose at least one Toolbox annotation CSV.")
    if not telemetry_paths:
        raise ValueError("Choose at least one telemetry CSV.")
    for path in annotations + telemetry_paths:
        if not path.is_file():
            raise FileNotFoundError(f"No file at {path}")

    if progress:
        progress(0.1, "Reading the annotations…")
    points = tool._load_and_concat_csvs([str(p) for p in annotations],
                                        "annotations")
    missing = {"Name", "Label"} - set(points.columns)
    if missing:
        raise ValueError(
            f"The annotation CSV is missing {', '.join(sorted(missing))}. "
            "This stage wants a Toolbox annotation export — one row per "
            "point — not a percent-cover table.")
    result.points = len(points)
    result.lines.append(
        f"{len(points):,} annotation point(s) from {len(annotations)} file(s).")

    # Counted before anything is excluded, so the number describes the data
    # rather than the options. This is the cleaning step: a point still
    # marked Review is unfinished work, and the two ways out of it -- label
    # it in Toolbox, or decide to leave it out -- both need to know which
    # images are affected.
    still_review = points[points["Label"] == REVIEW_LABEL]
    result.review_points = len(still_review)
    if result.review_points:
        per_image = still_review["Name"].value_counts()
        result.review_images = {str(k): int(v) for k, v in per_image.items()}
        result.warnings.append(
            f"{result.review_points:,} point(s) across "
            f"{len(result.review_images):,} image(s) are still marked "
            f"'{REVIEW_LABEL}' — nobody has settled them. Either label them "
            f"in Toolbox and export again, or tick 'leave out {REVIEW_LABEL}' "
            "to share those images' remaining points over what is left. "
            "Counted as they are, they sit in the denominator and make every "
            "real class on those images read lower.")

    excluded = [c for c in exclude_labels if str(c).strip()]
    if excluded:
        before = len(points)
        points = points[~points["Label"].isin(excluded)]
        result.excluded = excluded
        result.lines.append(
            f"{before - len(points):,} point(s) excluded "
            f"({', '.join(excluded)}); the rest are shared over what is left.")
        if points.empty:
            raise ValueError("Excluding those labels leaves no points at all.")

    if cancel is not None and cancel.is_set():
        result.cancelled = True
        return result

    if progress:
        progress(0.4, "Counting points per image…")
    cover = tool.annotations_to_percent_cover(points)
    result.labels = [c for c in cover.columns if c != "Name"]

    if progress:
        progress(0.6, "Reading the telemetry…")
    telemetry = tool._load_and_concat_csvs(
        [str(p) for p in telemetry_paths], "telemetry")

    if progress:
        progress(0.8, "Joining the telemetry…")
    merged, stats = tool.join_with_telemetry(cover, telemetry)
    result.images = int(stats["n_images"])
    result.matched = int(stats["n_matched"])
    result.unmatched = int(stats["n_unmatched"])
    result.unparsed_names = int(stats["n_unparsed_names"])
    result.telemetry_rows = int(stats["n_telemetry_rows"])
    result.duplicate_telemetry = int(stats["n_duplicate_telemetry_dropped"])

    result.lines.append(
        f"{result.images:,} image(s); {result.matched:,} matched a telemetry "
        f"row, {result.unmatched:,} did not.")
    if result.unmatched:
        result.warnings.append(
            f"{result.unmatched:,} image(s) have no telemetry row for their "
            "second. They are kept, with the telemetry columns empty — check "
            "the telemetry covers this transect's whole run.")
    if result.unparsed_names:
        result.warnings.append(
            f"{result.unparsed_names:,} image name(s) carry no "
            "YYYY_MM_DD_HH-MM-SS timestamp, so they cannot be matched at all.")
    if result.duplicate_telemetry:
        result.lines.append(
            f"{result.duplicate_telemetry:,} duplicate telemetry second(s) "
            "dropped before the join.")

    # The share each label holds over every point, which is the number an
    # ecologist reads first. Computed from the table rather than the counts
    # so it reflects whatever was excluded.
    weights = cover[result.labels].mul(
        points.groupby("Name").size().reindex(cover["Name"]).to_numpy(),
        axis=0)
    totals = weights.sum()
    grand = float(totals.sum()) or 1.0
    result.label_share = {str(k): float(v) / grand
                          for k, v in totals.items() if v > 0}

    target = Path(output_csv)
    if dry_run:
        result.lines.append(f"would write {target}")
        if result.review_points:
            result.lines.append(f"would write {REVIEW_NAME}")
        if progress:
            progress(1.0, "Check finished.")
        return result

    target.parent.mkdir(parents=True, exist_ok=True)
    merged.to_csv(target, index=False)
    result.output_csv = target

    if result.review_points:
        # "Label them in Toolbox" needs a list to work through, with the
        # coordinates -- a count in a warning is not actionable.
        columns = [c for c in ("Name", "Row", "Column", "Label", "Long Label",
                               "Path") if c in still_review.columns]
        review_path = target.parent / REVIEW_NAME
        still_review[columns].sort_values(
            [c for c in ("Name", "Row", "Column") if c in columns]
        ).to_csv(review_path, index=False)
        result.review_csv = review_path
        log.info(f"Wrote the points still needing review: {review_path} "
                 f"({result.review_points:,} point(s))")
    log.info(f"Wrote percent cover: {target}  "
             f"({len(merged):,} rows, {len(result.labels)} label columns)")

    # A row that does not sum to 1 means the grouping went wrong, and it is
    # cheap to notice here rather than in an analysis months later.
    sums = merged[result.labels].sum(axis=1)
    off = int((sums.sub(1.0).abs() > 1e-6).sum())
    if off:
        result.warnings.append(
            f"{off:,} row(s) do not sum to 1. That should not happen; the "
            "cover proportions for those images are suspect.")
    if progress:
        progress(1.0, "Percent cover written.")
    return result
