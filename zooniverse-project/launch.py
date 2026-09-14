"""
PyInstaller entry point.

Deliberately a shim outside the package. PyInstaller runs its target as
``__main__``, which breaks relative imports inside a package -- pointing the
spec at ``kelpquest/gui/app.py`` gives "attempted relative import with no known
parent package". Importing the package from here keeps the package context
intact.

A windowed build also discards stdout and stderr, so a crash before the window
appears leaves nothing behind. KELPQUEST_DEBUG=1 writes the traceback to a file
beside the executable.
"""

from __future__ import annotations

import os
import sys
import traceback
from pathlib import Path


def main() -> int:
    try:
        from kelpquest.gui.app import main as run

        run()
        return 0
    except Exception:
        report = traceback.format_exc()
        sys.stderr.write(report)
        if os.environ.get("KELPQUEST_DEBUG"):
            target = Path(sys.executable).parent / "kelpquest_crash.txt"
            try:
                target.write_text(report, encoding="utf-8")
            except OSError:
                pass
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
