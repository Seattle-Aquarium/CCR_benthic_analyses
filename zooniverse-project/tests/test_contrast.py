"""
Every colour pair, on the surface it actually sits on, against WCAG AA.

This is the check the eye cannot do. The bright secondary and accent colours
look correct in the palette and fail only in place -- Algae on Pumice is
1.97:1 -- so the palette being right is not the same as the theme being right.

Three of the light-theme pairs clear 4.5:1 with little to spare. If `surface`
on light is ever darkened, those are the ones that fail first.
"""

from __future__ import annotations

import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from kelpquest import brand

#: (label, foreground attribute, background attribute, ratio needed).
#: 3.0 is the large-text allowance, which headings qualify for.
PAIRS = (
    ("body on bg", "text", "bg", 4.5),
    ("body on surface", "text", "surface", 4.5),
    ("body on surface_alt", "text", "surface_alt", 4.5),
    ("muted on surface", "text_muted", "surface", 4.5),
    ("heading on bg", "heading", "bg", 3.0),
    ("heading on surface", "heading", "surface", 3.0),
    ("text on accent", "accent_text", "accent", 4.5),
    ("ok on surface", "ok", "surface", 4.5),
    ("warn on surface", "warn", "surface", 4.5),
    ("error on surface", "error", "surface", 4.5),
)


def _luminance(hex_colour: str) -> float:
    def channel(c: float) -> float:
        c /= 255
        return c / 12.92 if c <= 0.03928 else ((c + 0.055) / 1.055) ** 2.4

    r, g, b = brand.hex_to_rgb(hex_colour)
    return 0.2126 * channel(r) + 0.7152 * channel(g) + 0.0722 * channel(b)


def contrast(a: str, b: str) -> float:
    la, lb = _luminance(a), _luminance(b)
    hi, lo = max(la, lb), min(la, lb)
    return (hi + 0.05) / (lo + 0.05)


def test_contrast_of_a_known_pair():
    """Guards the maths itself, so a failure below is about the theme."""
    assert contrast("#FFFFFF", "#000000") == pytest.approx(21.0)
    assert contrast("#FFFFFF", "#FFFFFF") == pytest.approx(1.0)


@pytest.mark.parametrize("theme_name", ["dark", "light"])
@pytest.mark.parametrize("label,fg,bg,need", PAIRS,
                         ids=[p[0].replace(" ", "_") for p in PAIRS])
def test_pair_passes_aa(theme_name: str, label: str, fg: str, bg: str,
                        need: float):
    """Both themes, not just the one it was developed in."""
    theme = brand.THEMES[theme_name]
    ratio = contrast(getattr(theme, fg), getattr(theme, bg))
    assert ratio >= need, (
        f"{theme_name}: {label} is {ratio:.2f}:1, needs {need}")


def test_the_bright_colours_are_never_light_theme_text():
    """Algae, Seafoam and Coral fail as text on a light ground at any size.

    On light grounds they are fill and graphic colours only. The light theme
    therefore picks its own ok/warn, which is the one place we knowingly step
    outside the palette -- flag it in review rather than quietly reverting it.
    """
    light = brand.LIGHT
    for bright in (brand.ALGAE, brand.SEAFOAM, brand.CORAL):
        assert contrast(bright, light.surface) < 3.0      # unusable, as expected
        assert bright not in (light.text, light.text_muted, light.ok,
                              light.warn, light.error, light.heading)
