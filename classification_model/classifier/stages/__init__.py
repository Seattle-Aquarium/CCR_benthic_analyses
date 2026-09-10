"""
The four pipeline stages, and the result contract they share.

Each stage exposes ``run(cfg, *, progress=None, cancel=None) -> StageResult``
and never raises for an expected problem -- inspect the result instead. That
keeps the GUI's worker thread simple: call, get a result, report it.
"""

from __future__ import annotations

import time
from dataclasses import dataclass, field
from typing import Callable

from ..progress import CancelledError

#: Stage order, and the keys used everywhere else to name a stage.
ORDER = ("extract", "balance", "train", "evaluate", "compare")

TITLES = {
    "extract": "Extract patches",
    "balance": "Merge & balance",
    "train": "Train model",
    "evaluate": "Evaluate on holdout",
    "compare": "Compare models",
}


@dataclass
class StageResult:
    """What a stage produced, and what went wrong.

    ``blocked`` is deliberately distinct from ``errors``: a blocked stage did
    not fail, it correctly refused. Leaking patches into a held-out set is not
    an error condition to be retried -- it is a finding, and the operator has
    to decide what to do about it.
    """

    lines: list[str] = field(default_factory=list)
    outputs: dict = field(default_factory=dict)
    warnings: list[str] = field(default_factory=list)
    errors: list[str] = field(default_factory=list)
    blocked: list[str] = field(default_factory=list)
    cancelled: bool = False
    preview: bool = False
    elapsed_s: float = 0.0

    @property
    def ok(self) -> bool:
        return not self.errors and not self.blocked and not self.cancelled

    @property
    def committed(self) -> bool:
        """True when this run actually wrote its output, rather than previewing."""
        return self.ok and not self.preview

    def say(self, line: str) -> None:
        self.lines.append(line)

    def summary(self) -> str:
        out = list(self.lines)
        if self.cancelled:
            out.append("Cancelled - nothing further was written.")
        for b in self.blocked:
            out.append(f"BLOCKED: {b}")
        for w in self.warnings:
            out.append(f"WARNING: {w}")
        for e in self.errors:
            out.append(f"ERROR: {e}")
        return "\n".join(out)


def guard(fn: Callable[[], StageResult], started: float | None = None) -> StageResult:
    """Run a stage body, turning cancellation and crashes into a StageResult."""
    started = started if started is not None else time.time()
    try:
        res = fn()
    except CancelledError:
        res = StageResult(cancelled=True)
    except Exception as ex:                     # unexpected: report, don't crash
        res = StageResult(errors=[f"{type(ex).__name__}: {ex}"])
    res.elapsed_s = time.time() - started
    return res
