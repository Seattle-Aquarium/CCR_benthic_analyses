"""
One logger for the whole app.

Every stage logs through ``kelpquest``; the GUI attaches a handler that feeds
its log pane, and a rotating file keeps the last few runs for when somebody
asks what happened yesterday. The file lives in the local cache rather than in
the transect folder, so a run does not push a log file to the whole team.
"""

from __future__ import annotations

import logging
from collections.abc import Callable
from logging.handlers import RotatingFileHandler

from .config import cache_root, log_path

LOGGER_NAME = "kelpquest"

_FORMAT = "%(asctime)s [%(levelname)s] %(message)s"
_DATEFMT = "%H:%M:%S"

_file_handler: logging.Handler | None = None


def get_logger(name: str | None = None) -> logging.Logger:
    """The app logger, or a child of it for one module."""
    return logging.getLogger(LOGGER_NAME if not name else f"{LOGGER_NAME}.{name}")


def setup(level: int = logging.INFO) -> logging.Logger:
    """Attach the file handler. Safe to call more than once."""
    global _file_handler
    log = logging.getLogger(LOGGER_NAME)
    log.setLevel(level)
    # Our own handlers only: propagating to the root would double every line
    # if some other module has called basicConfig.
    log.propagate = False

    if _file_handler is None:
        try:
            cache_root().mkdir(parents=True, exist_ok=True)
            _file_handler = RotatingFileHandler(
                log_path(), maxBytes=2_000_000, backupCount=3, encoding="utf-8"
            )
        except OSError:
            # A read-only or missing cache directory is not worth failing over;
            # the GUI pane still shows everything.
            _file_handler = logging.NullHandler()
        _file_handler.setFormatter(logging.Formatter(_FORMAT, _DATEFMT))
        log.addHandler(_file_handler)
    return log


class CallbackHandler(logging.Handler):
    """Push formatted lines at a callable -- the GUI's log pane.

    The callable is invoked on whatever thread logged, which for us is the
    worker. The GUI therefore does not touch a widget here; it queues the line
    and lets the Tk thread render it.
    """

    def __init__(self, sink: Callable[[str], None], level: int = logging.INFO):
        super().__init__(level)
        self.sink = sink
        self.setFormatter(logging.Formatter(_FORMAT, _DATEFMT))

    def emit(self, record: logging.LogRecord) -> None:
        try:
            self.sink(self.format(record))
        except Exception:      # a failing log sink must never break a run
            pass


def add_sink(sink: Callable[[str], None], level: int = logging.INFO) -> logging.Handler:
    handler = CallbackHandler(sink, level)
    logging.getLogger(LOGGER_NAME).addHandler(handler)
    return handler
