"""
Take a transect's subjects out of circulation on Zooniverse.

The case this exists for: the classifications for a transect have been
exported and the rest of the work is going to be finished by hand in Toolbox.
Whatever is still sitting in a workflow is now work that will be done twice --
volunteers classifying points whose answer is already settled elsewhere.

**It retires; it does not delete.** Retiring a subject in a workflow tells
Zooniverse to stop serving it to volunteers. Everything else stays: the
classifications already collected, the subject itself, its metadata, its
membership of every subject set it belongs to. Nothing is destroyed and
nothing stops a later export working -- so a mistake here costs a second run
of the same page with the box ticked the other way, not a re-upload.

Three things decide what the page has to do.

*Which subjects.* The upload log, not the classifications export. The export
only knows the subjects volunteers actually reached, and those are the least
of the problem -- an untouched subject is the one that will be served
tomorrow. The log is this app's own record of what it created, one row per
subject, carrying the subject id and the transect it belongs to.

*Which workflows.* Retirement is recorded per workflow, not per subject. A
subject moves between subject sets as it retires -- yes/no, then multiple
choice, then an expert set -- and those sets hang off different workflows, so
retiring in one leaves it servable in the next.

*Which subjects in which workflow.* These are not the same question. A
workflow can only serve a subject that is in one of **its** subject sets, and
a project accumulates workflows for other transects and earlier seasons
entirely. Zooniverse will cheerfully accept a retirement for a subject a
workflow has never held -- it writes the row and returns success -- so asking
it to retire everything everywhere produces a page of identical numbers that
say nothing about what changed. Hence every run works out the membership
first and reports per workflow how many of *these* subjects it actually
holds.
"""

from __future__ import annotations

import csv
import threading
import time
from collections.abc import Callable, Iterable, Sequence
from dataclasses import dataclass, field
from pathlib import Path

from .logging_setup import get_logger
from .zooniverse import UPLOAD_LOG_NAME, connect

log = get_logger("retire")

#: What Zooniverse is told the reason was. Its vocabulary is fixed --
#: classification_count, consensus, blank, flagged, other -- and none of the
#: first four is true here. The work moved to Toolbox; that is `other`, and
#: claiming consensus would put a number into the project's own statistics
#: that no volunteer ever produced.
REASON = "other"

#: Subject ids per retirement request. The Zooniverse client batches at 100,
#: and matching it means one HTTP call per chunk rather than a hidden second
#: round of splitting -- which matters because the progress bar and the Stop
#: button only move between chunks.
CHUNK = 100


#: Written next to the upload log. The record of what was taken out of
#: circulation, and what the undo would have to put back.
RETIRE_LOG_NAME = "retire_log.csv"
LOG_HEAD = ["at", "action", "workflow_id", "workflow_name", "subject_id",
            "transect_id"]


@dataclass
class WorkflowPlan:
    """What one workflow holds, and what this run would do about it."""

    workflow_id: str
    name: str = ""
    #: How many of the chosen subjects are in this workflow's subject sets.
    members: int = 0
    #: How many of those it has already retired. None when Zooniverse would
    #: not say -- reported as "?" rather than guessed at.
    already: int | None = None
    #: How many this run would send, or did send.
    todo: int = 0
    sent: int = 0
    #: False when Zooniverse would not say what this workflow holds, in
    #: which case `members` is an assumption and not a reading.
    status_read: bool = True

    @property
    def holds_nothing(self) -> bool:
        return self.status_read and self.members == 0


@dataclass
class RetireResult:
    subject_ids: list[str] = field(default_factory=list)
    transects: dict[str, int] = field(default_factory=dict)
    plans: list[WorkflowPlan] = field(default_factory=list)
    include_future: bool = False
    unretire: bool = False
    log_csv: Path | None = None
    dry_run: bool = False
    cancelled: bool = False
    lines: list[str] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)
    errors: list[str] = field(default_factory=list)

    @property
    def ok(self) -> bool:
        return not self.errors and not self.cancelled

    @property
    def verb(self) -> str:
        return "un-retire" if self.unretire else "retire"

    @property
    def total_sent(self) -> int:
        return sum(p.sent for p in self.plans)

    def summary(self) -> str:
        head = ("Checked only - nothing on Zooniverse was changed."
                if self.dry_run else
                f"{self.total_sent:,} subject-workflow pair(s) {self.verb}d")
        out = [head]
        out += self.lines
        if self.plans:
            last = "would send" if self.dry_run else "sent"
            out += ["", f"{'workflow':<36} {'holds':>7} {'retired':>8} "
                        f"{last:>10}"]
            for p in self.plans:
                shown = f"{p.name[:28]} ({p.workflow_id})"
                already = "-" if p.already is None else f"{p.already:,}"
                count = p.todo if self.dry_run else p.sent
                out.append(f"{shown:<36} {p.members:>7,} {already:>8} "
                           f"{count:>10,}")
            idle = [p for p in self.plans if p.holds_nothing]
            if idle:
                out += ["", f"{len(idle)} workflow(s) hold none of these "
                            "subjects and were left alone: "
                        + ", ".join(p.name or p.workflow_id for p in idle)]
        return "\n".join(out)


def available() -> bool:
    """Whether the Zooniverse client is importable. Cheap; no network."""
    try:
        import panoptes_client  # noqa: F401
    except Exception:
        return False
    return True


# --------------------------------------------------------------------------
#  Which subjects
# --------------------------------------------------------------------------


def default_log(patches_dir: str | Path) -> Path:
    return Path(patches_dir) / UPLOAD_LOG_NAME


def read_log(upload_log: str | Path) -> list[dict]:
    path = Path(upload_log)
    if not path.is_file():
        raise FileNotFoundError(
            f"No upload log at {path}. It is written into the patches folder "
            "by stage 5, one row per subject created.")
    with path.open(newline="", encoding="utf-8") as fh:
        rows = list(csv.DictReader(fh))
    if rows and "subject_id" not in rows[0]:
        raise ValueError(
            f"{path.name} has no subject_id column, so there is nothing to "
            "retire by. Logs written by stage 5 and by import_subjects.py "
            "both have one.")
    return rows


def origin_sets(upload_log: str | Path, transect_id: str = "") -> set:
    """The subject sets these subjects were uploaded into.

    The safety net under `holdings`. A status row appears once a workflow
    holds a subject, but a set that has just been created and linked, whose
    subjects nobody has been served yet, is the one case where that could in
    principle read as empty -- and it is exactly the case where retiring
    matters most. The upload log knows which set each subject was created
    in, so a workflow linked to that set is treated as holding all of them
    whatever the status endpoint says.
    """
    wanted = (transect_id or "").strip()
    out: set = set()
    for row in read_log(upload_log):
        if wanted and (row.get("transect_id") or "").strip() != wanted:
            continue
        set_id = (row.get("subject_set_id") or "").strip()
        if set_id:
            out.add(set_id)
    return out


def transects_in(upload_log: str | Path) -> dict[str, int]:
    """transect id -> how many subjects the log has for it."""
    counts: dict[str, int] = {}
    for row in read_log(upload_log):
        key = (row.get("transect_id") or "").strip() or "(none)"
        counts[key] = counts.get(key, 0) + 1
    return counts


def subjects_in(upload_log: str | Path,
                transect_id: str = "") -> tuple[list[str], dict[str, int]]:
    """(subject ids, transect -> count) for one transect, or for all of them.

    Deduplicated and ordered: a log appended to across several upload runs can
    carry the same subject twice, and retiring it twice is a wasted request.
    """
    wanted = (transect_id or "").strip()
    ids: list[str] = []
    seen: set[str] = set()
    counts: dict[str, int] = {}
    for row in read_log(upload_log):
        got = (row.get("transect_id") or "").strip()
        if wanted and got != wanted:
            continue
        subject_id = (row.get("subject_id") or "").strip()
        if not subject_id or subject_id in seen:
            continue
        seen.add(subject_id)
        ids.append(subject_id)
        counts[got or "(none)"] = counts.get(got or "(none)", 0) + 1
    return ids, counts


# --------------------------------------------------------------------------
#  Which workflows, and what they hold
# --------------------------------------------------------------------------


def list_workflows() -> list[tuple[str, str, bool]]:
    """(id, display name, active) for every workflow in the project."""
    from panoptes_client import Project, Workflow

    project_id = connect()
    project = Project.find(project_id)
    out = []
    for workflow in Workflow.where(project_id=project.id):
        active = bool(getattr(workflow, "active", True))
        out.append((str(workflow.id), str(workflow.display_name), active))
    out.sort(key=lambda row: int(row[0]))
    log.info(f"Project {project_id} has {len(out)} workflow(s).")
    return out


def parse_ids(text: str) -> list[str]:
    """Workflow ids out of whatever is in the box. Same rules as stage 6."""
    seen: list[str] = []
    for chunk in str(text or "").replace(",", " ").replace(";", " ").split():
        digits = chunk.strip()
        if digits.isdigit() and digits not in seen:
            seen.append(digits)
    return seen


def _chunks(items: Sequence[str], size: int = CHUNK) -> Iterable[list[str]]:
    for start in range(0, len(items), size):
        yield list(items[start:start + size])


def workflow_sets(workflow_ids: Sequence[str]) -> dict[str, tuple[str, set]]:
    """workflow id -> (display name, the subject set ids linked to it)."""
    from panoptes_client import Workflow

    out: dict[str, tuple[str, set]] = {}
    for workflow_id in workflow_ids:
        workflow = Workflow.find(str(workflow_id))
        sets = {str(s.id) for s in workflow.links.subject_sets}
        name = str(getattr(workflow, "display_name", "") or workflow_id)
        out[str(workflow_id)] = (name, sets)
        log.info(f"Workflow {workflow_id} ('{name}') serves "
                 f"{len(sets)} subject set(s).")
    return out


def holdings(workflow_id: str, subject_ids: Sequence[str],
             progress: Callable[[float, str], None] | None = None,
             cancel: threading.Event | None = None,
             base: float = 0.0, span: float = 1.0
             ) -> tuple[set, set] | None:
    """(the subjects this workflow holds, the ones it has already retired).

    Both from one pass over ``SubjectWorkflowStatus``, which carries a row
    per subject *per workflow that holds it*. Measured against an untouched
    transect on this project, the rows match subject-set membership exactly
    -- 100 of 100 in the workflow holding its own set, 10 of 10, 12 of 12 and
    3 of 3 in the three it had moved into -- and none at all in the
    workflows belonging to other transects. Asking this way costs about half
    a minute for a transect; asking the subject-set endpoint the same
    question costs about forty minutes, and silently drops rows when more
    than fifty subjects are named at once.

    *Which* subjects, not how many of them. With only a count a run has to
    send every subject and can then report only how many it sent -- which is
    how six workflows came to report the same number regardless of what was
    true of them. With the ids it sends the difference, so what it reports is
    the change.

    None when Zooniverse would not say -- an older client, a changed
    endpoint. A check that cannot read the status is still worth running, so
    this degrades to a question mark rather than failing the stage.
    """
    from panoptes_client.subject_workflow_status import SubjectWorkflowStatus

    held: set = set()
    retired: set = set()
    done = 0
    for chunk in _chunks(subject_ids):
        if cancel is not None and cancel.is_set():
            return held, retired
        try:
            for status in SubjectWorkflowStatus.where(
                    workflow_id=str(workflow_id),
                    subject_ids=",".join(chunk)):
                subject_id = str(status.raw["links"]["subject"])
                held.add(subject_id)
                if (getattr(status, "retirement_reason", None)
                        or getattr(status, "retired_at", None)):
                    retired.add(subject_id)
        except Exception as exc:
            log.warning(f"Could not read what workflow {workflow_id} holds: "
                        f"{exc}")
            return None
        done += len(chunk)
        if progress:
            progress(base + span * done / max(1, len(subject_ids)),
                     f"Reading workflow {workflow_id} - "
                     f"{done:,}/{len(subject_ids):,}")
    return held, retired


def _append_log(path: Path, rows: list[dict]) -> None:
    if not rows:
        return
    new = not path.exists()
    with path.open("a", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(fh, fieldnames=LOG_HEAD)
        if new:
            writer.writeheader()
        writer.writerows(rows)


# --------------------------------------------------------------------------
#  Working out what to do
# --------------------------------------------------------------------------


def plan(subject_ids: Sequence[str],
         workflow_ids: Sequence[str],
         unretire: bool = False,
         include_future: bool = False,
         origin_sets: Sequence[str] = (),
         progress: Callable[[float, str], None] | None = None,
         cancel: threading.Event | None = None
         ) -> tuple[list[WorkflowPlan], dict[str, list[str]]]:
    """Per workflow: what it holds, what it has retired, what to send."""
    ids = [str(s) for s in subject_ids]
    origins = {str(s) for s in origin_sets if str(s).strip()}
    linked = workflow_sets(workflow_ids)

    plans: list[WorkflowPlan] = []
    targets: dict[str, list[str]] = {}
    span = 1.0 / max(1, len(workflow_ids))
    for i, workflow_id in enumerate(workflow_ids):
        workflow_id = str(workflow_id)
        name, sets = linked.get(workflow_id, (workflow_id, set()))
        if cancel is not None and cancel.is_set():
            break

        found = holdings(workflow_id, ids, progress, cancel,
                         base=i * span, span=span)
        if found is None:
            # Unknown, so assume the worst and send everything rather than
            # skip a subject that is still being served.
            held, retired, read = set(ids), set(), False
        else:
            held, retired = found
            read = True
        # A workflow linked to the subject set these were uploaded into
        # holds every one of them, whether or not a volunteer has reached
        # any yet. That is the one case a status row would not cover.
        if sets & origins:
            held = set(ids)

        item = WorkflowPlan(workflow_id=workflow_id, name=name,
                            members=len(held),
                            already=len(retired) if read else None,
                            status_read=read)
        pool = ids if include_future else [s for s in ids if s in held]
        send = ([s for s in pool if s in retired] if unretire
                else [s for s in pool if s not in retired])
        item.todo = len(send)
        targets[workflow_id] = send
        plans.append(item)
    return plans, targets


# --------------------------------------------------------------------------
#  The act
# --------------------------------------------------------------------------


def retire(subject_ids: Sequence[str],
           workflow_ids: Sequence[str],
           transect_id: str = "",
           unretire: bool = False,
           include_future: bool = False,
           origin_sets: Sequence[str] = (),
           dry_run: bool = True,
           log_dir: str | Path | None = None,
           progress: Callable[[float, str], None] | None = None,
           cancel: threading.Event | None = None) -> RetireResult:
    """Stop (or resume) these subjects being served in these workflows."""
    ids = [str(s).strip() for s in subject_ids if str(s).strip()]
    workflows = [str(w).strip() for w in workflow_ids if str(w).strip()]
    result = RetireResult(subject_ids=ids, dry_run=dry_run, unretire=unretire,
                          include_future=include_future)

    if not ids:
        raise ValueError(
            "No subjects to act on. Point at the transect's upload_log.csv "
            "and pick the transect.")
    if not workflows:
        raise ValueError(
            "Tick at least one workflow. Retirement is recorded per "
            "workflow, so a subject left servable by another one carries on "
            "being served.")

    connect()
    plans, targets = plan(ids, workflows, unretire=unretire,
                          include_future=include_future,
                          origin_sets=origin_sets,
                          progress=progress, cancel=cancel)
    result.plans = plans
    result.lines.append(
        f"{len(ids):,} subject(s), {len(workflows)} workflow(s) asked.")
    if any(not p.status_read for p in plans):
        result.warnings.append(
            "Zooniverse would not say what some workflows hold, so those "
            "rows show '-' and every subject is sent to them.")
    if include_future:
        result.warnings.append(
            "'Include subjects a workflow does not hold yet' is ticked, so "
            "every subject is sent to every workflow asked — including ones "
            "that hold none of them. Those requests are accepted and write "
            "a row, but there was nothing there to stop.")

    if cancel is not None and cancel.is_set():
        result.cancelled = True
        return result

    if dry_run:
        if progress:
            progress(1.0, "Check finished.")
        return result

    from panoptes_client import Workflow

    rows: list[dict] = []
    stamp = time.strftime("%Y-%m-%dT%H:%M:%S")
    action = "unretire" if unretire else "retire"
    total = sum(len(t) for t in targets.values()) or 1
    done = 0

    for item in plans:
        if cancel is not None and cancel.is_set():
            result.cancelled = True
            break
        send = targets.get(item.workflow_id, [])
        if not send:
            log.info(f"Workflow {item.workflow_id} ('{item.name}') holds none "
                     "of these subjects — nothing sent.")
            continue
        workflow = Workflow.find(item.workflow_id)
        for chunk in _chunks(send):
            if cancel is not None and cancel.is_set():
                result.cancelled = True
                log.warning("Stopped - the chunks already sent stay as they "
                            "are; run again to finish.")
                break
            try:
                if unretire:
                    workflow.unretire_subjects(chunk)
                else:
                    workflow.retire_subjects(chunk, reason=REASON)
            except Exception as exc:
                result.errors.append(
                    f"workflow {item.workflow_id} ({item.name}), subjects "
                    f"{chunk[0]}-{chunk[-1]}: {exc}")
                log.error(f"[fail] {action} {len(chunk)} subject(s) in "
                          f"workflow {item.workflow_id}: {exc}")
                done += len(chunk)
                continue
            item.sent += len(chunk)
            done += len(chunk)
            rows += [{"at": stamp, "action": action,
                      "workflow_id": item.workflow_id,
                      "workflow_name": item.name,
                      "subject_id": s, "transect_id": transect_id}
                     for s in chunk]
            if progress:
                progress(done / total,
                         f"{action.title()} - {item.sent:,}/{len(send):,} in "
                         f"{item.name[:24]}")
        log.info(f"{action.title()}d {item.sent:,} subject(s) in workflow "
                 f"{item.workflow_id} ('{item.name}').")

    if log_dir and rows:
        target = Path(log_dir) / RETIRE_LOG_NAME
        try:
            target.parent.mkdir(parents=True, exist_ok=True)
            _append_log(target, rows)
            result.log_csv = target
        except OSError as exc:
            result.warnings.append(f"Could not write {RETIRE_LOG_NAME}: {exc}")

    if not unretire and result.total_sent:
        result.lines.append(
            "They keep their classifications and their subject sets; "
            "Zooniverse simply stops serving them. Tick 'put them back' on "
            "this page to undo it.")
    if progress:
        progress(1.0, f"{action.title()} finished.")
    return result
