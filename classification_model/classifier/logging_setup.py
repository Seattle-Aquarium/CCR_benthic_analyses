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

import logging
import queue
import sys
from pathlib import Path

LOGGER_NAME = "classifier"

_configured = False


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
        if stream is not None:
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
