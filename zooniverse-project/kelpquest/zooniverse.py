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


USER_KEY = "ZOONIVERSE_USERNAME"
PASS_KEY = "ZOONIVERSE_PASSWORD"
PROJECT_KEY = "ZOONIVERSE_PROJECT_ID"

#: Where a password saved from the GUI is kept. On Windows `keyring` stores it
#: in the Credential Manager, which encrypts it with the signed-in account's
#: own key -- so it is readable by this user on this machine and by nothing
#: else. The alternative, a line in a file, is readable by anything that can
#: read the folder, and this project's folder is inside OneDrive.
KEYRING_SERVICE = "KelpQuest Zooniverse"

KEYRING_ADVICE = (
    "Saving a password needs the `keyring` package, which stores it in the "
    "Windows Credential Manager.\n\nInstall it with:\n\n    pip install keyring"
    "\n\nWithout it the app can still read credentials from scripts/.env.")


@dataclass
class Credentials:
    """Who to sign in as, and where each piece came from.

    The password is deliberately absent from ``__repr__``: a dataclass prints
    every field, and this object ends up in tracebacks and debugger frames.
    """

    username: str = ""
    password: str = ""
    project_id: str = ""
    #: Human-readable origin of the password, for the GUI to show.
    source: str = ""
    files_read: list[Path] = field(default_factory=list)

    def __repr__(self) -> str:                      # noqa: D105
        return (f"Credentials(username={self.username!r}, "
                f"project_id={self.project_id!r}, source={self.source!r}, "
                f"password={'set' if self.password else 'unset'})")

    @property
    def complete(self) -> bool:
        return bool(self.username and self.password and self.project_id)


def keyring_available() -> tuple[bool, str]:
    """Whether a password can be saved to the OS credential store."""
    try:
        import keyring
    except ImportError:
        return False, KEYRING_ADVICE
    try:
        backend = keyring.get_keyring()
    except Exception as exc:
        return False, f"keyring is installed but unusable here: {exc}"
    name = type(backend).__name__
    if "fail" in name.lower():
        return False, ("keyring found no credential store on this machine, so "
                       "a password cannot be saved securely.")
    return True, name


def _from_files() -> tuple[dict[str, str], list[Path]]:
    """Parse the credential files, without touching ``os.environ``.

    Reading them into the process environment -- which is what this used to do
    -- makes "came from the environment" and "came from a file" impossible to
    tell apart afterwards, and the GUI has to be able to say which.
    """
    values: dict[str, str] = {}
    read: list[Path] = []
    try:
        from dotenv import dotenv_values
    except ImportError:
        def dotenv_values(path):                      # minimal stand-in
            out = {}
            for line in Path(path).read_text(encoding="utf-8").splitlines():
                line = line.strip()
                if not line or line.startswith("#") or "=" not in line:
                    continue
                key, _, raw = line.partition("=")
                out[key.strip()] = raw.strip().strip("'\"")
            return out

    for path in ENV_CANDIDATES:
        if not path.is_file():
            continue
        try:
            found = dotenv_values(path) or {}
        except OSError as exc:
            log.warning(f"Could not read {path.name}: {exc}")
            continue
        read.append(path)
        for key in (USER_KEY, PASS_KEY, PROJECT_KEY):
            # First file wins, to match the old load_dotenv(override=False).
            if key not in values and (found.get(key) or "").strip():
                values[key] = str(found[key]).strip()
    return values, read


def stored_password(username: str) -> str:
    """The saved password for a username, or "". Never logged."""
    if not username:
        return ""
    ok, _why = keyring_available()
    if not ok:
        return ""
    import keyring

    try:
        return (keyring.get_password(KEYRING_SERVICE, username) or "").strip()
    except Exception as exc:
        log.warning(f"Could not read the saved password: {exc}")
        return ""


def save_credentials(username: str, password: str) -> str:
    """Save the password to the OS credential store. Returns where it went."""
    ok, why = keyring_available()
    if not ok:
        raise RuntimeError(why)
    import keyring

    keyring.set_password(KEYRING_SERVICE, username, password)
    backend = type(keyring.get_keyring()).__name__
    # The username is fine to log. The password is not, and is not passed on
    # to the logger anywhere in this module.
    log.info(f"Saved the Zooniverse password for {username} to the OS "
             f"credential store ({backend}).")
    return backend


def forget_credentials(username: str) -> bool:
    """Remove a saved password. False when there was nothing to remove."""
    ok, _why = keyring_available()
    if not ok or not username:
        return False
    import keyring

    try:
        keyring.delete_password(KEYRING_SERVICE, username)
    except Exception:
        return False
    log.info(f"Removed the saved Zooniverse password for {username}.")
    return True


#: The account the app is configured with: (username, project id). Neither is
#: secret. Module state because `connect` is reached from `export.inspect` and
#: `export.list_project_sets` as well as from here, and threading an account
#: argument through all of those to describe one signed-in user would be a lot
#: of plumbing for one fact that does not change during a run.
_account: tuple[str, str] = ("", "")


def set_account(username: str, project_id: str) -> None:
    """Tell this module which account the app has remembered."""
    global _account
    _account = ((username or "").strip(), (project_id or "").strip())


def resolve(username: str = "", project_id: str = "") -> Credentials:
    """Work out which credentials a run would use, and from where.

    Order, most deliberate first: the process environment, then the password
    saved from the GUI, then the credential files. The files come last so that
    signing in through the app takes effect immediately rather than being
    shadowed by a stale `config.env` -- and ``source`` always says which won,
    because a silent order is the thing that makes either order dangerous.

    ``username`` and ``project_id`` are what the app has remembered; they are
    not secret and live in the ordinary settings file.
    """
    files, read = _from_files()
    env = {k: (os.getenv(k) or "").strip()
           for k in (USER_KEY, PASS_KEY, PROJECT_KEY)}
    remembered_user, remembered_project = _account

    creds = Credentials(files_read=read)
    creds.username = (env[USER_KEY] or (username or "").strip()
                      or remembered_user or files.get(USER_KEY, ""))
    creds.project_id = (env[PROJECT_KEY] or (project_id or "").strip()
                        or remembered_project or files.get(PROJECT_KEY, ""))

    if env[PASS_KEY]:
        creds.password, creds.source = env[PASS_KEY], "the environment"
    else:
        saved = stored_password(creds.username)
        if saved:
            creds.password, creds.source = saved, "the OS credential store"
        elif files.get(PASS_KEY):
            creds.password = files[PASS_KEY]
            creds.source = read[0].name if read else "a credentials file"
    return creds


def verify(username: str, password: str) -> str:
    """Sign in to check a username and password actually work.

    Done before saving, so a typo is caught at the moment it can still be
    corrected rather than at the start of a two-hour upload.
    """
    from panoptes_client import Panoptes

    Panoptes.connect(username=username, password=password)
    log.info(f"Signed in to Zooniverse as {username}.")
    return username


def load_env() -> tuple[str, list[Path]]:
    """Back-compatible shim: (project id, files read).

    Kept because the older call sites read it this way. New code should use
    `resolve`, which also says where the password came from.
    """
    creds = resolve()
    return creds.project_id, creds.files_read


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


def credentials_status(username: str = "",
                       project_id: str = "") -> tuple[bool, str]:
    """(ready, human-readable status) for the GUI to show before a run.

    Says the username, the project and where the password came from -- never
    the password itself, and never whether a guess at it would be close.
    """
    creds = resolve(username, project_id)
    missing = [n for n, v in (("username", creds.username),
                              ("password", creds.password),
                              ("project id", creds.project_id)) if not v]
    if missing:
        return False, ("Not signed in — no " + ", ".join(missing)
                       + ". Fill in the account card above, or put them in "
                         "scripts/.env.")
    return True, (f"Signed in as {creds.username} · project "
                  f"{creds.project_id} · password from {creds.source}")


def connect(username: str = "", project_id: str = "") -> str:
    """Authenticate and return the project id."""
    from panoptes_client import Panoptes

    creds = resolve(username, project_id)
    if not creds.complete:
        raise RuntimeError(
            "Zooniverse credentials are incomplete. Sign in on the Upload "
            "page, or fill in ZOONIVERSE_USERNAME, ZOONIVERSE_PASSWORD and "
            "ZOONIVERSE_PROJECT_ID in scripts/.env.")
    Panoptes.connect(username=creds.username, password=creds.password)
    log.info(f"Connected to Zooniverse as {creds.username} "
             f"(project {creds.project_id}).")
    return creds.project_id


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
