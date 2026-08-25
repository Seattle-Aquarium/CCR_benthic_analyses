"""
Progress reporting and cancellation, shared by every stage.

A stage reports a single 0..1 fraction with a message, so the GUI needs no
knowledge of what the stage is doing internally. Long stages compose several
weighted sub-steps into that one fraction via ``Stages``.
"""

from __future__ import annotations

from typing import Callable, Protocol

#: ``(fraction_0_to_1, message) -> None``. The message may be empty.
ProgressCB = Callable[[float, str], None]


class CancelToken(Protocol):
    """Anything with ``is_set()`` -- in practice a ``threading.Event``."""

    def is_set(self) -> bool: ...


class CancelledError(Exception):
    """Raised inside a stage when the user asked it to stop.

    Distinct from a failure: the pipeline reports it as cancelled rather than
    as an error, and no partial output is presented as a result.
    """


def is_cancelled(cancel: CancelToken | None) -> bool:
    return cancel is not None and cancel.is_set()


def check_cancelled(cancel: CancelToken | None, where: str = "") -> None:
    """Raise if cancellation was requested. Call from inside long loops."""
    if is_cancelled(cancel):
        raise CancelledError(f"cancelled{f' during {where}' if where else ''}")


def noop_progress(fraction: float, message: str = "") -> None:
    """A ProgressCB that does nothing -- the default for headless runs."""


class Stages:
    """Weighted progress across sub-steps of one stage.

    Give each sub-step a weight roughly proportional to how long it takes, then
    report into it with ``sub(name)``; the combined fraction comes out of the
    single callback the GUI is watching.
    """

    def __init__(self, cb: ProgressCB | None):
        self.cb = cb
        self.weights: dict[str, float] = {}
        self.done: dict[str, float] = {}

    def plan(self, **weights: float) -> "Stages":
        self.weights = dict(weights)
        self.done = {k: 0.0 for k in weights}
        return self

    def sub(self, name: str) -> ProgressCB:
        def cb(fraction: float, message: str = "") -> None:
            # Never let a stage's progress go backwards. Sub-steps that each
            # count 0..1 would otherwise rewind the bar, which reads as a hang.
            self.done[name] = max(self.done.get(name, 0.0),
                                  max(0.0, min(1.0, fraction)))
            self._emit(message)
        return cb

    def finish(self, name: str, message: str = "") -> None:
        self.done[name] = 1.0
        self._emit(message)

    def _emit(self, message: str) -> None:
        if not self.cb:
            return
        total = sum(self.weights.values()) or 1.0
        acc = sum(self.weights[k] * self.done.get(k, 0.0) for k in self.weights)
        self.cb(acc / total, message)
