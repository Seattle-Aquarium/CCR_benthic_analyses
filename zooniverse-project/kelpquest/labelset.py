"""
The CoralNet-Toolbox labelset.

Toolbox stores its labels as a JSON list of ``{id, short_label_code,
long_label_code, color}``. Our classifier's class names are the short codes, so
this is what turns a prediction into the pair of columns Toolbox expects --
``Label`` and ``Long Label`` -- and into the name a volunteer reads on the
patch.
"""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path

#: Where the shared labelset normally lives. Offered as the default in the GUI;
#: a person on a different machine browses to their own copy.
DEFAULT_PATHS = (
    Path.home() / "Seattle Aquarium Dropbox" / "Coastal_Climate_Resilience"
    / "machine_learning" / "Toolbox" / "labelset"
    / "labelset_toolbox_zooniverse.json",
)


@dataclass(frozen=True)
class Label:
    code: str
    long_name: str


class Labelset:
    """Short code to long name, with the lookup a prediction needs."""

    def __init__(self, labels: list[Label], source: Path | None = None):
        self.labels = labels
        self.source = source
        self._by_code = {lab.code: lab for lab in labels}

    def __len__(self) -> int:
        return len(self.labels)

    def __contains__(self, code: str) -> bool:
        return code in self._by_code

    def long_name(self, code: str) -> str:
        """The long label, or the code itself when the labelset does not have it.

        A model trained before a label was added will happily predict a class
        the labelset has never heard of. Falling back to the code keeps the run
        going and leaves the mismatch visible in the sheet, which is a better
        outcome than a blank column or a crash 2,000 patches in.
        """
        lab = self._by_code.get(code)
        return lab.long_name if lab else code

    def unknown_codes(self, codes) -> list[str]:
        """Which of `codes` this labelset cannot name. Reported before a run."""
        return sorted({c for c in codes if c and c not in self._by_code})

    # ---- loading -----------------------------------------------------

    @classmethod
    def load(cls, path: str | Path) -> Labelset:
        p = Path(path)
        raw = json.loads(p.read_text(encoding="utf-8"))
        if isinstance(raw, dict):          # some exports wrap the list
            raw = raw.get("labels", raw.get("labelset", []))
        labels = [
            Label(str(e.get("short_label_code", "")).strip(),
                  str(e.get("long_label_code", "")).strip())
            for e in raw
            if isinstance(e, dict) and e.get("short_label_code")
        ]
        if not labels:
            raise ValueError(f"No labels found in {p}")
        return cls(labels, p)

    @classmethod
    def empty(cls) -> Labelset:
        """Used when no labelset is chosen: long names fall back to the code."""
        return cls([])


def find_default() -> Path | None:
    for p in DEFAULT_PATHS:
        if p.is_file():
            return p
    return None
