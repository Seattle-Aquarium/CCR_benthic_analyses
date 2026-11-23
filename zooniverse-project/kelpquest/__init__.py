"""
Kelp Quest — transect folder to Zooniverse subject set.

One run takes a folder of edited transect stills and carries it through the
whole chain: sample annotation points, cut a patch around each one, classify
the patch with our own model, write the two metadata sheets (CoralNet-Toolbox
and Zooniverse), and upload the patches as subjects.

The stages are separate modules with no GUI imports, so the same code runs
under `kelpquest.gui` and under `python -m kelpquest`.
"""

from __future__ import annotations

APP_NAME = "Kelp Quest"
APP_ABBREV = "KelpQuest"
__version__ = "1.0.0"
