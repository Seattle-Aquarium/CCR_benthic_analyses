"""
One logging configuration for the whole pipeline.

Previously each script configured its own, and six of the seven opened their
log file without an explicit encoding. On Windows that means cp1252, so the
first check mark or warning sign in a message raised a UnicodeEncodeError
inside the logging machinery -- non-fatal, but it printed a stack trace into
the middle of every run. Configured in one place, with ``encoding="utf-8"``,
it cannot regress in six files independently.
"""

from __future__ import annotations

import io
import logging
import queue
import re
import sys
import time
from pathlib import Path

LOGGER_NAME = "classifier"

_configured = False

#: Ultralytics colours its output for a terminal. A Tk text widget renders the
#: escape sequences literally, so "train:" arrives as "<ESC>[34m<ESC>[1mtrain:"
#: -- stripped rather than passed through.
_ANSI = re.compile(r"\x1b\[[0-9;]*[A-Za-z]")


class _LineSink(io.TextIOBase):
    """A writable stand-in for a ``sys.stdout`` that does not exist.

    A windowed process -- which is what ``pythonw.exe`` gives us, and what the
    launcher uses so double-clicking does not leave a console behind -- has no
    standard streams at all: ``sys.stdout`` and ``sys.stderr`` are literally
    ``None``. ``print()`` special-cases that and quietly does nothing, so most
    code survives. Anything that writes to the stream *object* does not, and
    Ultralytics writes its progress table straight to ``sys.stdout``:

        AttributeError: 'NoneType' object has no attribute 'write'

    which killed training a tenth of a minute into the run, after the base
    model had loaded. Substituting this sink turns those writes into log
    records, so the per-epoch table reaches the app's log pane and the log
    file rather than ending the run.
    """

    #: Marks this as a substitute, so ``configure`` does not attach a console
    #: handler pointed back at it and build a write -> log -> write loop.
    _is_shim = True

    #: tqdm redraws its bar many times a second with a bare carriage return.
    #: Forwarding every redraw would bury the real output, so partial lines
    #: are throttled to one every couple of seconds.
    _PARTIAL_INTERVAL_S = 2.0

    def __init__(self, logger: logging.Logger, level: int = logging.INFO):
        self._log = logger
        self._level = level
        self._buf = ""
        self._last_partial = 0.0

    # Both tqdm and Ultralytics branch on these before writing.
    def isatty(self) -> bool:
        return False

    def writable(self) -> bool:
        return True

    @property
    def encoding(self) -> str:
        return "utf-8"

    def write(self, text: str) -> int:
        if not text:
            return 0
        self._buf += text
        while True:
            nl, cr = self._buf.find("\n"), self._buf.find("\r")
            cut = min(i for i in (nl, cr) if i >= 0) if (nl >= 0 or cr >= 0) else -1
            if cut < 0:
                break
            line, self._buf = self._buf[:cut], self._buf[cut + 1:]
            line = _ANSI.sub("", line).strip()
            if not line:
                continue
            if cut == cr and cut != nl:
                # A progress redraw: keep the latest, but not all of them.
                now = time.monotonic()
                if now - self._last_partial < self._PARTIAL_INTERVAL_S:
                    continue
                self._last_partial = now
            self._log.log(self._level, line)
        return len(text)

    def flush(self) -> None:
        line = _ANSI.sub("", self._buf).strip()
        self._buf = ""
        if line:
            self._log.log(self._level, line)


def ensure_streams() -> bool:
    """Guarantee ``sys.stdout``/``sys.stderr`` are writable objects.

    Call this before anything else in a windowed entry point. Returns True if
    a substitution was made, which is only the case under ``pythonw``.
    """
    sink = None
    for name in ("stdout", "stderr"):
        if getattr(sys, name, None) is None:
            sink = sink or _LineSink(get_logger("console"))
            setattr(sys, name, sink)
    return sink is not None


def get_logger(name: str | None = None) -> logging.Logger:
    """The pipeline logger, or a child of it."""
    return logging.getLogger(f"{LOGGER_NAME}.{name}" if name else LOGGER_NAME)


def configure(log_path: str | Path | None = None, *, level: int = logging.INFO,
              console: bool = True) -> logging.Logger:
    """Attach the file and console handlers. Safe to call more than once."""
    global _configured
    log = logging.getLogger(LOGGER_NAME)
    log.setLevel(level)
    log.propagate = False

    if _configured:
        return log

    fmt = logging.Formatter("%(asctime)s [%(levelname)s] %(message)s",
                            datefmt="%H:%M:%S")

    if log_path:
        path = Path(log_path)
        path.parent.mkdir(parents=True, exist_ok=True)
        # The whole point of this module: without encoding="utf-8" this handler
        # takes the system codepage and dies on any non-ASCII log message.
        fh = logging.FileHandler(path, encoding="utf-8")
        fh.setFormatter(fmt)
        log.addHandler(fh)

    if console:
        # The file handler is UTF-8 above, but the console is a separate
        # problem: a Windows terminal defaults to cp1252, which turns every
        # non-ASCII character in a message into a replacement glyph. Ask for
        # UTF-8, and fall back to replacing rather than raising if the stream
        # cannot be reconfigured (a pythonw build has no real stdout at all).
        stream = sys.stdout
        try:
            stream.reconfigure(encoding="utf-8", errors="replace")
        except (AttributeError, ValueError, OSError):
            pass
        if stream is not None and not getattr(stream, "_is_shim", False):
            sh = logging.StreamHandler(stream)
            sh.setFormatter(fmt)
            log.addHandler(sh)

    _configured = True
    return log


class QueueHandler(logging.Handler):
    """Forward log records to a queue for the GUI to drain.

    Tk is not thread-safe, so a worker thread must never touch a widget. The
    worker logs as normal, this handler puts formatted lines on a queue, and
    the Tk main loop drains it on a timer.
    """

    def __init__(self, q: "queue.Queue", kind: str = "log"):
        super().__init__()
        self.queue = q
        self.kind = kind
        self.setFormatter(logging.Formatter("%(message)s"))

    def emit(self, record: logging.LogRecord) -> None:
        try:
            self.queue.put((self.kind, record.levelno, self.format(record)))
        except Exception:  # a broken log sink must never break the run
            pass


def attach(handler: logging.Handler) -> logging.Handler:
    logging.getLogger(LOGGER_NAME).addHandler(handler)
    return handler


def detach(handler: logging.Handler) -> None:
    logging.getLogger(LOGGER_NAME).removeHandler(handler)
