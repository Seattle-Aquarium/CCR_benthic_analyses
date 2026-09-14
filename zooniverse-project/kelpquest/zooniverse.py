"""
Creating Zooniverse subjects from a folder of patches.

Follows ``scripts/import_subjects.py``, which has survived enough interrupted
uploads to be worth copying rather than reinventing:

* every successful subject is appended to ``upload_log.csv``, and a re-run
  skips anything already in it, so a dropped connection costs the current
  batch and nothing else;
* subjects are flushed into the subject set every ``checkpoint_every`` rows
  rather than all at the end, so an interrupted run leaves a usable set;
* anything skipped or refused lands in ``fail_log.csv`` with the reason.

``panoptes_client`` is imported lazily so the rest of the app runs on a machine
that has never had it installed.
"""

from __future__ import annotations

import csv
import os
import subprocess
import sys
import threading
import time
from collections.abc import Callable
from contextlib import contextmanager
from dataclasses import dataclass, field
from pathlib import Path

from .logging_setup import get_logger

log = get_logger("zooniverse")

#: The metadata key duplicate detection is keyed on. Same value as the older
#: script, so an upload log written by either tool is understood by both.
UNIQUE_ID_FIELD = "source_id"

UPLOAD_LOG_NAME = "upload_log.csv"
FAIL_LOG_NAME = "fail_log.csv"

_PROJECT_ROOT = Path(__file__).resolve().parent.parent

#: Searched in order; the first file that exists wins for each key. Kept the
#: same as the older script so credentials already set up keep working.
ENV_CANDIDATES = (
    _PROJECT_ROOT / "scripts" / ".env",
    _PROJECT_ROOT / "scripts" / "config.env",
    _PROJECT_ROOT / ".env",
)


@dataclass
class UploadResult:
    uploaded: int = 0
    skipped: int = 0
    failed: int = 0
    subject_set_id: str = ""
    subject_set_name: str = ""
    cancelled: bool = False
    dry_run: bool = False
    errors: list[str] = field(default_factory=list)

    def summary(self) -> str:
        head = "Dry run - nothing was uploaded." if self.dry_run else (
            f"Subject set '{self.subject_set_name}' (ID {self.subject_set_id})")
        bits = [head, f"{self.uploaded:,} uploaded",
                f"{self.skipped:,} skipped", f"{self.failed:,} failed"]
        if self.cancelled:
            bits.append("stopped early")
        return "  ·  ".join(bits)


# --------------------------------------------------------------------------
#  Credentials
# --------------------------------------------------------------------------


def load_env() -> tuple[str, list[Path]]:
    """Read the credential files; return the project id and what was read.

    Never returns or logs the password - the GUI only ever needs to say
    whether credentials were found and which file they came from.
    """
    read: list[Path] = []
    try:
        from dotenv import load_dotenv
    except ImportError:
        return os.getenv("ZOONIVERSE_PROJECT_ID", ""), read

    for path in ENV_CANDIDATES:
        if path.is_file():
            load_dotenv(path, override=False)
            read.append(path)
    return os.getenv("ZOONIVERSE_PROJECT_ID", "").strip(), read


MAGIC_ADVICE = (
    "panoptes-client crashes this Python when it is imported.\n\n"
    "The cause is python-magic: it loads a native libmagic library, and on "
    "Windows the plain package finds no magic database and dies with an "
    "access violation. Uploading and exporting cannot run until it is fixed.\n\n"
    "Fix it with:\n\n"
    "    pip install python-magic-bin\n\n"
    "That wheel bundles both the library and its database. Everything else in "
    "this app works without it."
)


#: Cached, because the probe below costs a process launch.
_client_probe: tuple[bool, str] | None = None


def client_available() -> tuple[bool, str]:
    """Whether ``panoptes_client`` can be imported without killing us.

    Probed in a subprocess, and that is the whole point. panoptes_client
    imports ``python-magic`` for MIME sniffing, which loads a native libmagic;
    on Windows without ``python-magic-bin`` the load reaches for a magic
    database that is not there and dies with an access violation. That is a
    segfault, not an exception -- ``try: import`` cannot catch it, and the app
    simply vanishes the moment somebody presses Upload, with nothing in the log
    to say why. Asking a child process to try it instead costs a few hundred
    milliseconds once and turns a disappearing window into a sentence.
    """
    global _client_probe
    if _client_probe is not None:
        return _client_probe

    if getattr(sys, "frozen", False):
        # A packaged build has no interpreter to hand `-c` to, and its
        # dependencies were fixed when it was built.
        _client_probe = (True, "")
        return _client_probe

    kwargs = {}
    if os.name == "nt":
        kwargs["creationflags"] = 0x08000000      # CREATE_NO_WINDOW
    # A healthy import takes a second or two. The unhealthy one crashes, and
    # Windows Error Reporting then holds the dead process open for as long as
    # it is allowed to -- so the error box is suppressed for the child (it
    # inherits our error mode) and the wait is short.
    with _no_crash_dialog():
        try:
            done = subprocess.run(
                [sys.executable, "-c", "import panoptes_client"],
                capture_output=True, timeout=PROBE_TIMEOUT_S, **kwargs)
        except subprocess.TimeoutExpired:
            # It neither imported nor returned. For our purposes that is the
            # same as crashing: the stage cannot run, and the advice is the
            # same.
            _client_probe = (False, MAGIC_ADVICE)
            return _client_probe
        except (OSError, subprocess.SubprocessError) as exc:
            _client_probe = (False, f"Could not check panoptes-client: {exc}")
            return _client_probe

    stderr = done.stderr.decode("utf-8", "replace")
    if done.returncode == 0:
        _client_probe = (True, "")
    elif "magic" in stderr.lower() or not stderr.strip():
        # Named the culprit, or died with nothing to say at all -- an access
        # violation prints no traceback.
        _client_probe = (False, MAGIC_ADVICE)
    else:
        tail = stderr.strip().splitlines()
        _client_probe = (
            False,
            "panoptes-client could not be imported, so nothing can be "
            f"uploaded or exported:\n\n{tail[-1]}\n\nInstall it with:\n\n"
            "pip install -r requirements-app.txt")
    return _client_probe


#: Long enough for a healthy import on a cold cache, short enough that a
#: crashing one does not look like a hang.
PROBE_TIMEOUT_S = 45


@contextmanager
def _no_crash_dialog():
    """Stop Windows popping an error box for a child that crashes.

    The error mode is inherited, and a child that faults with reporting
    enabled sits there being reported instead of exiting. Restored afterwards
    so this app's own crashes are still handled normally.
    """
    if os.name != "nt":
        yield
        return
    SEM_FAILCRITICALERRORS, SEM_NOGPFAULTERRORBOX = 0x0001, 0x0002
    try:
        import ctypes

        kernel32 = ctypes.windll.kernel32
        previous = kernel32.SetErrorMode(
            SEM_FAILCRITICALERRORS | SEM_NOGPFAULTERRORBOX)
    except Exception:
        yield
        return
    try:
        yield
    finally:
        try:
            kernel32.SetErrorMode(previous)
        except Exception:
            pass


def credentials_status() -> tuple[bool, str]:
    """(ready, human-readable status) for the GUI to show before a run."""
    project_id, read = load_env()
    user = os.getenv("ZOONIVERSE_USERNAME", "").strip()
    password = os.getenv("ZOONIVERSE_PASSWORD", "").strip()
    if not read:
        return False, ("No credentials file found. Copy scripts/config.env to "
                       "scripts/.env and fill in your Zooniverse details.")
    where = read[0].name
    missing = [n for n, v in (("username", user), ("password", password),
                              ("project id", project_id)) if not v]
    if missing:
        return False, f"{where} is missing: {', '.join(missing)}"
    return True, f"Signed in as {user} · project {project_id} · from {where}"


def connect() -> str:
    """Authenticate and return the project id."""
    from panoptes_client import Panoptes

    project_id, _read = load_env()
    user = os.getenv("ZOONIVERSE_USERNAME", "").strip()
    password = os.getenv("ZOONIVERSE_PASSWORD", "").strip()
    if not (user and password and project_id):
        raise RuntimeError(
            "Zooniverse credentials are incomplete. Fill in "
            "ZOONIVERSE_USERNAME, ZOONIVERSE_PASSWORD and "
            "ZOONIVERSE_PROJECT_ID in scripts/.env.")
    Panoptes.connect(username=user, password=password)
    log.info(f"Connected to Zooniverse as {user} (project {project_id}).")
    return project_id


# --------------------------------------------------------------------------
#  Logs
# --------------------------------------------------------------------------


def load_upload_log(path: Path) -> set[str]:
    """Which subjects a previous run already created."""
    if not path.is_file():
        return set()
    try:
        with path.open(newline="", encoding="utf-8") as fh:
            done = {(r.get(UNIQUE_ID_FIELD) or "").strip()
                    for r in csv.DictReader(fh)}
        done.discard("")
        if done:
            log.info(f"Upload log has {len(done):,} subject(s) already "
                     "uploaded; those will be skipped.")
        return done
    except OSError as exc:
        log.warning(f"Could not read {path.name}: {exc}")
        return set()


def _append(path: Path, rows: list[dict]) -> None:
    if not rows:
        return
    new = not path.exists()
    with path.open("a", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0]))
        if new:
            w.writeheader()
        w.writerows(rows)


def read_metadata(csv_path: Path) -> list[dict]:
    with csv_path.open(newline="", encoding="utf-8") as fh:
        return list(csv.DictReader(fh))


# --------------------------------------------------------------------------
#  Upload
# --------------------------------------------------------------------------


def upload(image_dir: str | Path,
           metadata_csv: str | Path,
           transect_id: str,
           subject_set_name: str = "",
           subject_set_id: str = "",
           checkpoint_every: int = 100,
           sleep: float = 0.1,
           limit: int = 0,
           dry_run: bool = True,
           progress: Callable[[float, str], None] | None = None,
           cancel: threading.Event | None = None) -> UploadResult:
    """Create one subject per row of `metadata_csv`.

    Give exactly one of `subject_set_name` (create a new set) or
    `subject_set_id` (add to an existing one).
    """
    image_dir = Path(image_dir)
    metadata_csv = Path(metadata_csv)
    result = UploadResult(dry_run=dry_run, subject_set_name=subject_set_name,
                          subject_set_id=subject_set_id)

    if not metadata_csv.is_file():
        raise FileNotFoundError(
            f"No {metadata_csv.name} in {image_dir}. Write the metadata sheets "
            "before uploading.")
    if bool(subject_set_name) == bool(subject_set_id):
        raise ValueError("Give a name for a new subject set, or the ID of an "
                         "existing one - not both, and not neither.")

    rows = read_metadata(metadata_csv)
    if limit:
        rows = rows[:limit]
    if not rows:
        raise ValueError(f"{metadata_csv.name} has no rows.")

    upload_log = image_dir / UPLOAD_LOG_NAME
    fail_log = image_dir / FAIL_LOG_NAME
    already = load_upload_log(upload_log)

    # ---- dry run: check the files, touch nothing ---------------------
    if dry_run:
        for i, row in enumerate(rows):
            name = (row.get("filename") or "").strip()
            path = image_dir / name
            if not path.is_file():
                result.failed += 1
                log.warning(f"  MISSING  {name}")
            elif name in already:
                result.skipped += 1
            else:
                result.uploaded += 1
            if progress and i % 50 == 0:
                progress(i / len(rows), f"Checking - {i:,}/{len(rows):,}")
        log.info(f"Dry run: {result.uploaded:,} would upload, "
                 f"{result.skipped:,} already in the upload log, "
                 f"{result.failed:,} missing from disk.")
        if progress:
            progress(1.0, "Dry run finished.")
        return result

    # ---- the real thing ----------------------------------------------
    from panoptes_client import Project, Subject, SubjectSet

    project_id = connect()
    project = Project.find(project_id)

    if subject_set_id:
        subject_set = SubjectSet.find(subject_set_id)
        log.info(f"Adding to existing subject set '{subject_set.display_name}' "
                 f"(ID {subject_set.id}).")
    else:
        subject_set = SubjectSet()
        subject_set.links.project = project
        subject_set.display_name = subject_set_name
        subject_set.save()
        log.info(f"Created subject set '{subject_set_name}' - "
                 f"ID {subject_set.id}")
        log.info("Record this Subject Set ID in tracker.xlsx.")
    result.subject_set_id = str(subject_set.id)
    result.subject_set_name = subject_set.display_name

    pending: list = []
    upload_rows: list[dict] = []
    fail_rows: list[dict] = []

    def flush() -> None:
        if pending:
            log.info(f"[checkpoint] adding {len(pending)} subject(s) to the set")
            subject_set.add(pending)
            pending.clear()
        _append(upload_log, upload_rows)
        _append(fail_log, fail_rows)
        upload_rows.clear()
        fail_rows.clear()

    try:
        for i, row in enumerate(rows, start=1):
            if cancel is not None and cancel.is_set():
                result.cancelled = True
                log.warning("Stopped - saving what has been uploaded so far.")
                break

            filename = (row.get("filename") or "").strip()
            source_id = filename
            image_path = image_dir / filename

            if source_id in already:
                result.skipped += 1
                fail_rows.append({"filename": filename,
                                  UNIQUE_ID_FIELD: source_id,
                                  "reason": "Already in upload log"})
            elif not image_path.is_file():
                result.failed += 1
                log.warning(f"[skip] image not found: {filename}")
                fail_rows.append({"filename": filename,
                                  UNIQUE_ID_FIELD: source_id,
                                  "reason": "Image file not found"})
            else:
                try:
                    subject = Subject()
                    subject.links.project = project
                    subject.add_location(str(image_path))
                    for key, value in row.items():
                        subject.metadata[key] = "" if value is None else str(value)
                    subject.metadata[UNIQUE_ID_FIELD] = source_id
                    subject.metadata["transect_id"] = transect_id
                    subject.metadata["filename"] = filename
                    subject.save()
                    # Stamped back so the subject carries its own id, which is
                    # what the classification export is later joined on.
                    subject.metadata["subject_id"] = str(subject.id)
                    subject.save()

                    pending.append(subject)
                    already.add(source_id)
                    result.uploaded += 1
                    upload_rows.append({
                        "subject_id": subject.id,
                        "filename": filename,
                        UNIQUE_ID_FIELD: source_id,
                        "transect_id": transect_id,
                        "subject_set_id": subject_set.id,
                        "uploaded_at": time.strftime("%Y-%m-%dT%H:%M:%S"),
                    })
                    if sleep > 0:
                        time.sleep(sleep)
                except Exception as exc:
                    result.failed += 1
                    result.errors.append(f"{filename}: {exc}")
                    log.error(f"[fail] {filename}: {exc}")
                    fail_rows.append({"filename": filename,
                                      UNIQUE_ID_FIELD: source_id,
                                      "reason": str(exc)})

            if progress:
                progress(i / len(rows),
                         f"Uploading - {i:,}/{len(rows):,}  "
                         f"({result.uploaded:,} created)")
            if i % max(1, checkpoint_every) == 0:
                flush()
    finally:
        # Whatever happened, the subjects already created must reach the set
        # and the log, or the next run will make them all over again.
        try:
            flush()
        except Exception as exc:
            log.error(f"Final checkpoint failed: {exc}")
            result.errors.append(f"Final checkpoint failed: {exc}")

    log.info(result.summary())
    return result
