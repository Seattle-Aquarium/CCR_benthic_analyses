"""
One panel per stage.

Each panel is a view onto the run config: ``load()`` fills the widgets from it,
``collect()`` writes them back. Nothing else in the GUI knows what a stage's
settings are, and no stage knows a GUI exists.

Every panel's action is guarded by the footer's check toggle, which is on by
default, so the first click on a new set of paths always reports what *would*
happen. Given that one stage writes thousands of JPEGs and another creates
subjects that volunteers immediately start work on, the default is worth the
extra click.
"""

from __future__ import annotations

from pathlib import Path

import customtkinter as ctk

from .. import (
    classify,
    export,
    labelset,
    metadata,
    rejoin,
    report,
    telemetry,
    zooniverse,
)
from ..config import default_output_dir
from . import theme as T
from .widgets import (
    Card,
    Field,
    PathField,
    button,
    checkbox,
    combobox,
    hint,
    label,
    set_text,
    textbox,
)


class StagePanel(ctk.CTkScrollableFrame):
    """Base class: a scrolling column of cards for one stage.

    Scrolling rather than fixed, because a laptop lid at 1366x768 is the
    smallest screen these run on and a card should never be unreachable.
    """

    key = ""
    title = ""
    subtitle = ""
    run_text = "Run"
    check_text = "Check only — write nothing"
    #: Some stages only ever read. Their check box is meaningless and is hidden.
    can_check = True

    def __init__(self, master, app):
        super().__init__(master, fg_color=T.BG)
        self.grid_columnconfigure(0, weight=1)
        self.app = app
        self.cfg = app.cfg
        self._row = 0
        self.build()
        self.load()

    # ---- subclass hooks ------------------------------------------

    def build(self) -> None: ...
    def load(self) -> None: ...
    def collect(self) -> None: ...

    def validate(self) -> str | None:
        """A message to show instead of running, or None to proceed."""
        return None

    def confirm(self) -> str | None:
        """A yes/no question to ask before committing, or None to just go."""
        return None

    def on_result(self, result) -> None:
        """Called on the Tk thread once a run of this stage lands."""

    # ---- helpers -------------------------------------------------

    def card(self, title: str, subtitle: str = "") -> Card:
        c = Card(self, title, subtitle)
        c.grid(row=self._row, column=0, sticky="ew", pady=(0, T.PAD), padx=(0, 4))
        self._row += 1
        return c

    def stack(self, card: Card, *widgets) -> None:
        for i, w in enumerate(widgets):
            w.grid(row=i, column=0, sticky="ew", pady=3)


# --------------------------------------------------------------------------
#  1 - Transect
# --------------------------------------------------------------------------


class TransectPanel(StagePanel):
    key = "transect"
    title = "1.  Transect folder"
    subtitle = ("Point at the folder of edited stills for one transect, and "
                "confirm what was found. These values are stamped on every "
                "subject and written into every sheet, so a wrong one is only "
                "discovered much later.")
    run_text = "Scan folder"
    can_check = False

    def build(self) -> None:
        c1 = self.card("Where the stills are",
                       "Usually the 'edited' folder under transects/T#.")
        self.folder = PathField(c1.body, "Transect folder", kind="folder",
                                title="Select the transect folder",
                                on_change=self._folder_changed)
        self.folder.grid(row=0, column=0, sticky="ew")
        self.output = PathField(c1.body, "Output folder", kind="folder",
                                title="Where should the patches be written?")
        self.output.grid(row=1, column=0, sticky="ew", pady=(8, 0))
        hint(c1.body, "Patch images, both sheets and the upload log all land in "
                      "the output folder. Keeping it inside the transect means "
                      "a second transect cannot overwrite this one's upload log."
             ).grid(row=2, column=0, sticky="ew", pady=(6, 0))

        c2 = self.card("Telemetry", "The UTC transect CSV for this transect. It "
                                    "carries the transect ID, site, date and a "
                                    "row per second, so each still gets its "
                                    "depth and position.")
        self.telemetry = PathField(
            c2.body, "Transect CSV", kind="file",
            title="Select the UTC transect CSV",
            filetypes=(("CSV", "*.csv"), ("All files", "*.*")),
            on_change=self._telemetry_changed)
        self.telemetry.grid(row=0, column=0, sticky="ew")
        button(c2.body, "Find it for me", self._find_telemetry, "ghost",
               width=130).grid(row=1, column=0, sticky="w", pady=(8, 0))

        c3 = self.card("What was found")
        self.found = textbox(c3.body, height=132)
        self.found.grid(row=0, column=0, sticky="ew")

        c4 = self.card("Identity",
                       "Read from the telemetry CSV where there is one. The "
                       "transect ID prefix is the site and season — the "
                       "transect number is added to it.")
        self.prefix = Field(c4.body, "Transect ID prefix", width=140,
                            hint_text="EBM_W25", caption_width=150,
                            validate=self._check_prefix,
                            on_change=self._compose)
        self.site = Field(c4.body, "Site", width=110, hint_text="EBM",
                          caption_width=150)
        self.date = Field(c4.body, "Survey date", width=130,
                          hint_text="YYYY-MM-DD", caption_width=150,
                          validate=_check_date)
        self.number = Field(c4.body, "Transect number", width=80,
                            hint_text="T6", caption_width=150,
                            validate=self._check_number, on_change=self._compose)
        self.stack(c4, self.prefix, self.site, self.date, self.number)

    # ---- reactions -------------------------------------------------

    def _folder_changed(self, text: str) -> None:
        """Offer an output folder as soon as a transect folder is chosen."""
        if text and not self.output.get():
            self.output.set(default_output_dir(text))

    def _telemetry_changed(self, path: str) -> None:
        if not path:
            self.telemetry.note("without one, site and date come from the "
                                "folder names")
            return
        if not Path(path).is_file():
            self.telemetry.note("no file at that path", ok=False)
            return
        try:
            tel = telemetry.load(path)
        except (OSError, ValueError) as exc:
            self.telemetry.note(str(exc), ok=False)
            return
        self.telemetry.note(tel.summary(), ok=True)

    def _find_telemetry(self) -> None:
        folder = self.folder.get()
        if not folder:
            self.app.warn("Choose the transect folder first.")
            return
        found = telemetry.find_for(folder, self.number.get())
        if found:
            self.telemetry.set(found)
            return
        options = telemetry.candidates(folder)
        if options:
            self.app.warn(
                f"{len(options)} transect CSVs sit near that folder and none "
                "matches the transect number on its own:\n\n"
                + "\n".join(p.name for p in options[:8])
                + "\n\nPick one with Browse, or set the transect number first.")
        else:
            self.app.warn("No UTC transect CSVs found near that folder. They "
                          "normally live under the flight folder in "
                          "logs/transects/transects.")

    def _compose(self) -> None:
        """Show the composed transect id whenever either half changes.

        notify=False matters: the prefix field's own change handler is this
        method, so refreshing it the normal way would call straight back in.
        """
        if hasattr(self, "prefix"):
            self.prefix.refresh(notify=False)

    def _check_prefix(self, text: str) -> tuple[bool, str]:
        prefix = text.rstrip("_")
        number = self.number.get() if hasattr(self, "number") else ""
        composed = f"{prefix}_{number}" if number else prefix
        if number and prefix.upper().endswith(number.upper()):
            return False, ("the prefix already ends in the transect number — "
                           f"drop the '_{number}'")
        return True, f"subjects stamped '{composed}'"

    def _check_number(self, text: str) -> tuple[bool, str]:
        if not text.upper().startswith("T") or not text[1:].isdigit():
            return False, "like T6"
        return True, "appended to the prefix"

    # ---- config ----------------------------------------------------

    def load(self) -> None:
        cfg = self.cfg
        self.folder.set(cfg.transect_folder)
        self.output.set(cfg.output_dir)
        self.telemetry.set(cfg.telemetry_csv)
        self.prefix.set(cfg.transect_id_prefix)
        self.site.set(cfg.site_name)
        self.date.set(cfg.survey_date)
        self.number.set(cfg.transect_number)

    def collect(self) -> None:
        cfg = self.cfg
        cfg.transect_folder = self.folder.get()
        cfg.output_dir = self.output.get() or (
            str(default_output_dir(cfg.transect_folder))
            if cfg.transect_folder else "")
        cfg.telemetry_csv = self.telemetry.get()
        cfg.transect_id_prefix = self.prefix.get()
        cfg.site_name = self.site.get()
        cfg.survey_date = self.date.get()
        cfg.transect_number = self.number.get()

    def validate(self) -> str | None:
        if not self.folder.get():
            return "Choose the transect folder — the folder of edited stills."
        if not Path(self.folder.get()).is_dir():
            return f"There is no folder at {self.folder.get()}."
        return None

    def on_result(self, result) -> None:
        set_text(self.found, "\n".join(result.lines) or result.summary())
        t = result.advance.get("transect")
        if t is None:
            return
        # The scan is authoritative for what it found; a value the operator has
        # deliberately corrected is left alone.
        if t.transect_number:
            self.number.set(t.transect_number)
        if t.transect_id_prefix:
            self.prefix.set(t.transect_id_prefix)
        if t.site_name and not self.site.get():
            self.site.set(t.site_name)
        if t.survey_date and not self.date.get():
            self.date.set(t.survey_date)
        if t.telemetry is not None and not self.telemetry.get():
            self.telemetry.set(t.telemetry.path)
        self.folder.note(t.summary(), ok=t.ok or None)


# --------------------------------------------------------------------------
#  2 - Cut patches
# --------------------------------------------------------------------------


class PatchPanel(StagePanel):
    key = "patches"
    title = "2.  Cut patches"
    subtitle = ("Scatter random points over every still and cut the crop a "
                "volunteer will see. Every number here shows its consequence, "
                "because none of them mean much on their own.")
    run_text = "Cut patches"

    def build(self) -> None:
        c1 = self.card("Sampling",
                       "Random points, the way CoralNet-Toolbox scatters them. "
                       "The same seed on the same transect gives the same "
                       "points, so a re-run does not renumber everything.")
        self.points = Field(c1.body, "Points per image", width=80,
                            caption_width=150, validate=self._check_points,
                            on_change=self._update)
        self.patch_size = Field(c1.body, "Patch size (px)", width=80,
                                caption_width=150, validate=self._check_patch,
                                on_change=self._update)
        self.margin = Field(c1.body, "Edge margin (px)", width=80,
                            caption_width=150, validate=self._check_margin)
        self.seed = Field(c1.body, "Seed", width=80, caption_width=150,
                          validate=_check_int("same seed, same points"))
        self.stack(c1, self.points, self.patch_size, self.margin, self.seed)

        c2 = self.card("The crop a volunteer sees",
                       "A square crop centred on the point, with the patch "
                       "outlined in green and the point marked in red.")
        self.scale = Field(c2.body, "Crop scale", width=80, caption_width=150,
                           validate=self._check_scale, on_change=self._update)
        self.quality = Field(c2.body, "JPEG quality", width=80,
                             caption_width=150, validate=self._check_quality)
        self.stack(c2, self.scale, self.quality)

        c3 = self.card("Classification",
                       "Cutting a patch and classifying it both need the same "
                       "still decoded, and the finished JPEG carries the "
                       "label, so doing both here encodes each patch once "
                       "instead of twice.")
        self.fuse = ctk.BooleanVar(value=True)
        checkbox(c3.body, "Classify in the same pass", variable=self.fuse,
                 command=self._update).grid(row=0, column=0, sticky="w")
        self.fuse_note = hint(c3.body, "")
        self.fuse_note.grid(row=1, column=0, sticky="ew", pady=(6, 0))

        c4 = self.card("This run")
        self.plan = label(c4.body, "", muted=True)
        self.plan.grid(row=0, column=0, sticky="ew")

    # ---- inline validation -----------------------------------------

    def _check_points(self, text: str) -> tuple[bool, str]:
        try:
            n = int(text)
        except ValueError:
            return False, "whole number"
        if n < 1:
            return False, "at least 1"
        t = self.app.transect
        if t and t.images:
            return True, f"{n * len(t.images):,} patches over {len(t.images)} stills"
        return True, "points per still"

    def _check_patch(self, text: str) -> tuple[bool, str]:
        try:
            n = int(text)
        except ValueError:
            return False, "whole number"
        if n < 16:
            return False, "too small to classify"
        return True, "the size the model was trained on"

    def _check_margin(self, text: str) -> tuple[bool, str]:
        try:
            n = int(text)
        except ValueError:
            return False, "whole number"
        if n < 0:
            return False, "cannot be negative"
        patch = self.patch_size.int_value(224)
        if n < patch // 2:
            return False, f"below {patch // 2} the patch itself runs off the edge"
        # The crop field is built after this one, so on the first pass the
        # scale comes from the config rather than a widget that does not exist.
        scale = (self.scale.float_value(3.5) if hasattr(self, "scale")
                 else self.cfg.patch.scale)
        half = int(scale * patch / 2)
        if n < half:
            return True, (f"the patch clears the edge; crops within "
                          f"{half - n} px of it are slid inward")
        return True, "both the patch and the whole crop clear the edge"

    def _check_scale(self, text: str) -> tuple[bool, str]:
        try:
            v = float(text)
        except ValueError:
            return False, "a number like 3.5"
        if v < 1:
            return False, "at least 1"
        return True, f"{int(round(v * self.patch_size.int_value(224)))} px crop"

    def _check_quality(self, text: str) -> tuple[bool, str]:
        try:
            n = int(text)
        except ValueError:
            return False, "whole number"
        if not 1 <= n <= 100:
            return False, "between 1 and 100"
        return True, ("largest files, no visible loss" if n >= 95
                      else "smaller files, faster upload")

    def _update(self) -> None:
        if not hasattr(self, "plan"):
            return
        weights = self.cfg.classify.weights
        if not self.fuse.get():
            self.fuse_note.configure(
                text="Patches go out unlabelled; run stage 3 to score them.")
        elif weights:
            self.fuse_note.configure(text=f"Using {Path(weights).name}.")
        else:
            self.fuse_note.configure(
                text="No weights chosen yet — set them on stage 3, or the "
                     "patches go out unlabelled.")
        t = self.app.transect
        if not t or not t.images:
            self.plan.configure(text="No transect scanned yet.")
            return
        n = len(t.images) * self.points.int_value(50)
        crop = int(round(self.scale.float_value(3.5)
                         * self.patch_size.int_value(224)))
        self.plan.configure(text=f"{n:,} patches at {crop}×{crop} px "
                                 f"into {self.cfg.output_dir or '(not set)'}")

    # ---- config ----------------------------------------------------

    def load(self) -> None:
        s, p = self.cfg.sample, self.cfg.patch
        self.points.set(s.points_per_image)
        self.patch_size.set(s.patch_size)
        self.margin.set(s.margin)
        self.seed.set(s.seed)
        self.scale.set(p.scale)
        self.quality.set(p.jpeg_quality)
        self.fuse.set(p.classify_in_same_pass)
        self._update()

    def refresh(self) -> None:
        """Re-validate against the transect the scan just found.

        The point count's note is "2,900 patches over 58 stills" only once
        there is a transect to count; built before the scan it can only say
        "points per still", and nothing would ever correct it.
        """
        self.points.refresh(notify=False)
        self.margin.refresh(notify=False)
        self._update()

    def collect(self) -> None:
        s, p = self.cfg.sample, self.cfg.patch
        s.points_per_image = self.points.int_value(s.points_per_image)
        s.patch_size = self.patch_size.int_value(s.patch_size)
        s.margin = self.margin.int_value(s.margin)
        s.seed = self.seed.int_value(s.seed)
        p.scale = self.scale.float_value(p.scale)
        p.jpeg_quality = self.quality.int_value(p.jpeg_quality)
        p.classify_in_same_pass = bool(self.fuse.get())

    def validate(self) -> str | None:
        t = self.app.transect
        if t is None or not t.images:
            return "Scan the transect folder first — stage 1."
        if self.margin.int_value(0) < self.patch_size.int_value(224) // 2:
            return ("The edge margin is smaller than half the patch, so some "
                    "patches would run off the edge of the frame.")
        return None

    def confirm(self) -> str | None:
        t = self.app.transect
        count = len(t.images) * self.points.int_value(50) if t else 0
        return (f"Cut {count:,} patch images into "
                f"{self.cfg.output_dir}?\n\nExisting patches with the same "
                "names are overwritten.")


# --------------------------------------------------------------------------
#  3 - Classify
# --------------------------------------------------------------------------


class ClassifyPanel(StagePanel):
    key = "classify"
    title = "3.  Classify patches"
    subtitle = ("Run our own model over the patches. The prediction fills the "
                "label columns in both sheets and is burned onto the patch as "
                "the claim a volunteer is asked to agree or disagree with, so "
                "the weights chosen here decide what the subject set asks.")
    run_text = "Run classification"

    def build(self) -> None:
        c1 = self.card("Model", "A YOLO classification checkpoint — best.pt "
                                "from a run in classification_model.")
        self.weights = PathField(
            c1.body, "Weights", kind="file", title="Select the model weights",
            filetypes=(("PyTorch weights", "*.pt"), ("All files", "*.*")),
            on_change=self._weights_changed)
        self.weights.grid(row=0, column=0, sticky="ew")
        self.labelset = PathField(
            c1.body, "Labelset", kind="file",
            title="Select the Toolbox labelset",
            filetypes=(("Labelset JSON", "*.json"), ("All files", "*.*")),
            on_change=self._labelset_changed)
        self.labelset.grid(row=1, column=0, sticky="ew", pady=(8, 0))

        c2 = self.card("How it runs",
                       "Patches are cut from the source still at full "
                       "resolution, not from the saved JPEG, so the model sees "
                       "what it was trained on.")
        self.device = Field(c2.body, "Device", width=80, hint_text="cpu",
                            caption_width=150, validate=self._check_device)
        self.imgsz = Field(c2.body, "Image size", width=80, caption_width=150,
                           validate=_check_int("match the training size"))
        self.batch = Field(c2.body, "Batch", width=80, caption_width=150,
                           validate=_check_int("patches scored at a time"))
        self.stack(c2, self.device, self.imgsz, self.batch)
        self.burn = ctk.BooleanVar(value=True)
        checkbox(c2.body, "Write the predicted label onto each patch image",
                 variable=self.burn).grid(row=3, column=0, sticky="w",
                                          pady=(10, 0))

    def _check_device(self, text: str) -> tuple[bool, str]:
        if text.strip().lower() == "cpu":
            return True, "slower, but works everywhere"
        return True, "a CUDA device index — falls back to CPU if absent"

    def _weights_changed(self, path: str) -> None:
        if not classify.available():
            self.weights.note("ultralytics is not installed in this "
                              "environment", ok=False)
            return
        if not path:
            self.weights.note("without weights the patches go out unlabelled",
                              ok=False)
            return
        p = Path(path)
        if not p.is_file():
            self.weights.note("no file at that path", ok=False)
            return
        self.weights.note(f"{p.name} · {p.stat().st_size / 1e6:.0f} MB", ok=True)

    def _labelset_changed(self, path: str) -> None:
        if not path:
            self.labelset.note("without one, long labels fall back to the "
                               "short code", ok=False)
            return
        try:
            ls = labelset.Labelset.load(path)
        except (OSError, ValueError) as exc:
            self.labelset.note(str(exc), ok=False)
            return
        self.labelset.note(f"{len(ls)} labels", ok=True)

    def load(self) -> None:
        c = self.cfg.classify
        self.weights.set(c.weights)
        found = labelset.find_default()
        self.labelset.set(c.labelset or (str(found) if found else ""))
        self.device.set(c.device)
        self.imgsz.set(c.imgsz)
        self.batch.set(c.batch)
        self.burn.set(c.burn_label)

    def refresh(self) -> None:
        self._weights_changed(self.weights.get())
        self._labelset_changed(self.labelset.get())

    def collect(self) -> None:
        c = self.cfg.classify
        c.weights = self.weights.get()
        c.labelset = self.labelset.get()
        c.device = self.device.get() or "cpu"
        c.imgsz = self.imgsz.int_value(c.imgsz)
        c.batch = self.batch.int_value(c.batch)
        c.burn_label = bool(self.burn.get())

    def validate(self) -> str | None:
        if not classify.available():
            return ("ultralytics is not installed in this environment, so "
                    "nothing can be classified. Install it with:\n\n"
                    "pip install -r requirements-app.txt")
        if not self.weights.get():
            return "Choose the model weights — the Weights box on this page."
        if not Path(self.weights.get()).is_file():
            return f"There is no file at {self.weights.get()}."
        return None

    def confirm(self) -> str | None:
        extra = ("\n\nEach patch image will be redrawn with its new label."
                 if self.burn.get() else "")
        return f"Classify every patch in the manifest?{extra}"


# --------------------------------------------------------------------------
#  4 - Metadata
# --------------------------------------------------------------------------


class MetadataPanel(StagePanel):
    key = "metadata"
    title = "4.  Write metadata"
    subtitle = ("Both sheets come from patches.csv, so this can be re-run on "
                "its own after correcting the site or the date without "
                "touching a single image.")
    run_text = "Write metadata sheets"

    def build(self) -> None:
        c1 = self.card(
            "What gets written",
            "The Toolbox sheet is for importing the whole transect and "
            "verifying it point by point. metadata.csv is what the upload "
            "reads. The telemetry join is written whenever a transect CSV is "
            "in play.")
        self.preview_box = textbox(c1.body, height=190)
        self.preview_box.grid(row=0, column=0, sticky="ew")

    def load(self) -> None:
        self.refresh()

    def refresh(self) -> None:
        cfg = self.cfg
        out = Path(cfg.output_dir) if cfg.output_dir else None
        if out is None:
            set_text(self.preview_box, "Scan a transect folder first.")
            return
        t = self.app.transect
        tel = t.telemetry if t else None
        lines = [
            f"folder      {out}",
            "",
            f"manifest    {metadata.MANIFEST_NAME}",
            f"Toolbox     {metadata.toolbox_csv_name(cfg.site_name, cfg.survey_date, cfg.transect_number)}",
            f"Zooniverse  {metadata.ZOONIVERSE_NAME}",
        ]
        if tel is not None:
            lines.append(
                "telemetry   " + metadata.joined_csv_name(
                    cfg.site_name, cfg.survey_date, cfg.transect_number))
        lines += [
            "",
            f"site        {cfg.site_name or '(not set)'}",
            f"date        {cfg.survey_date or '(not set)'}",
            f"transect    {cfg.transect_number or '(not set)'}",
            f"transect id {cfg.transect_id or '(not set)'}",
        ]
        if tel is not None:
            lines.append(f"telemetry   {tel.summary()}")
        else:
            lines.append("telemetry   none — no depth or position columns")

        manifest = out / metadata.MANIFEST_NAME
        if manifest.is_file():
            try:
                records = metadata.read_manifest(manifest)
                done = sum(1 for r in records if r.classified)
                lines += ["", f"{len(records):,} patches in the manifest, "
                              f"{done:,} classified"]
            except (OSError, ValueError, KeyError) as exc:
                lines += ["", f"manifest unreadable: {exc}"]
        else:
            lines += ["", "No patches.csv yet — cut the patches first."]
        set_text(self.preview_box, "\n".join(lines))

    def validate(self) -> str | None:
        out = Path(self.cfg.output_dir) if self.cfg.output_dir else None
        if out is None:
            return "Scan a transect folder first — stage 1."
        if not (out / metadata.MANIFEST_NAME).is_file():
            return (f"There is no {metadata.MANIFEST_NAME} in {out}. Cut the "
                    "patches first — stage 2.")
        return None

    def on_result(self, result) -> None:
        self.refresh()


# --------------------------------------------------------------------------
#  5 - Upload
# --------------------------------------------------------------------------


class UploadPanel(StagePanel):
    key = "upload"
    title = "5.  Upload to Zooniverse"
    subtitle = ("The one step that leaves this machine. A check lists what "
                "would go up without contacting Zooniverse; committing names "
                "the subject set and the count first, because a subject set "
                "cannot be un-created and volunteers start on it immediately.")
    run_text = "Upload to Zooniverse"
    check_text = "Check only — do not upload"

    def build(self) -> None:
        c1 = self.card("Zooniverse account",
                       "Read from scripts/.env — never from anything this app "
                       "writes.")
        c1.body.grid_columnconfigure(1, weight=1)
        button(c1.body, "Check", self._check_credentials, "ghost", width=90
               ).grid(row=0, column=0, sticky="w")
        self.creds = label(c1.body, "", muted=True)
        self.creds.grid(row=0, column=1, sticky="ew", padx=(12, 0))

        c2 = self.card("Subject set",
                       "Create a new set for a transect, or add into one that "
                       "already exists. The ID is the last number in the "
                       "subject set's Zooniverse URL.")
        self.mode = ctk.StringVar(value="new")
        radios = ctk.CTkFrame(c2.body, fg_color="transparent")
        radios.grid(row=0, column=0, sticky="w")
        for i, (value, text) in enumerate((("new", "Create a new set"),
                                           ("existing", "Add to an existing set"))):
            ctk.CTkRadioButton(
                radios, text=text, variable=self.mode, value=value,
                command=self._mode_changed, font=T.FONT_BODY,
                text_color=T.TEXT, fg_color=T.ACCENT,
                hover_color=T.ACCENT_HOVER, border_color=T.FIELD_BORDER,
            ).grid(row=0, column=i, sticky="w", padx=(0, 18))

        self.set_name = Field(c2.body, "New set name", width=230,
                              hint_text="2025_01_28_EBM_T6", caption_width=150)
        self.set_id = Field(c2.body, "Existing set ID", width=120,
                            hint_text="135054", caption_width=150,
                            validate=_check_digits("subject set id"))
        self.transect_id = label(c2.body, "", muted=True)
        for i, w in enumerate((self.set_name, self.set_id, self.transect_id)):
            w.grid(row=i + 1, column=0, sticky="ew", pady=(8 if i == 0 else 3, 0))

        c3 = self.card("How it runs",
                       "Subjects are flushed into the set every checkpoint, so "
                       "a dropped connection costs the current batch and "
                       "nothing more. Re-running skips anything already in "
                       "upload_log.csv.")
        self.limit = Field(c3.body, "Limit", width=80, hint_text="all",
                           caption_width=150, validate=self._check_limit)
        self.checkpoint = Field(c3.body, "Checkpoint every", width=80,
                                caption_width=150,
                                validate=_check_int("subjects between flushes"))
        self.pause = Field(c3.body, "Pause between (s)", width=80,
                           caption_width=150, validate=_check_float)
        self.stack(c3, self.limit, self.checkpoint, self.pause)

        c4 = self.card("Ready to upload")
        self.status = label(c4.body, "", muted=True)
        self.status.grid(row=0, column=0, sticky="ew")

    def _mode_changed(self) -> None:
        new = self.mode.get() == "new"
        self.set_name.enable(new)
        self.set_id.enable(not new)

    def _check_limit(self, text: str) -> tuple[bool, str]:
        if not text.isdigit() or int(text) < 1:
            return False, "leave empty for all"
        return True, f"first {int(text):,} rows only"

    def _check_credentials(self) -> None:
        usable, why = zooniverse.client_available()
        if not usable:
            self.creds.configure(text=why.splitlines()[0], text_color=T.WARN)
            return
        ready, message = zooniverse.credentials_status()
        self.creds.configure(text=message,
                             text_color=T.OK if ready else T.WARN)

    def _metadata_csv(self) -> Path | None:
        out = self.cfg.output_dir
        return Path(out) / metadata.ZOONIVERSE_NAME if out else None

    def load(self) -> None:
        u = self.cfg.upload
        self.mode.set("existing" if u.subject_set_id else "new")
        self.set_name.set(u.subject_set_name)
        self.set_id.set(u.subject_set_id)
        self.limit.set(u.limit or "")
        self.checkpoint.set(u.checkpoint_every)
        self.pause.set(u.sleep)
        self._mode_changed()
        self._check_credentials()
        self.refresh()

    def refresh(self) -> None:
        cfg = self.cfg
        if not self.set_name.get():
            stem = metadata.sheet_stem(cfg.site_name, cfg.survey_date,
                                       cfg.transect_number)
            if stem != "transect":
                self.set_name.set(stem)
        self.transect_id.configure(
            text=f"Every subject is stamped transect_id "
                 f"'{cfg.transect_id or '(not set)'}' — set the prefix on "
                 f"stage 1.")
        csv_path = self._metadata_csv()
        if csv_path and csv_path.is_file():
            try:
                rows = len(zooniverse.read_metadata(csv_path))
            except OSError:
                rows = 0
            done = len(zooniverse.load_upload_log(
                csv_path.parent / zooniverse.UPLOAD_LOG_NAME))
            self.status.configure(
                text=f"{rows:,} subjects in metadata.csv"
                     + (f", {done:,} already uploaded and would be skipped"
                        if done else ""))
        else:
            self.status.configure(text="No metadata.csv yet — write the sheets "
                                       "first, stage 4.")

    def collect(self) -> None:
        u = self.cfg.upload
        new = self.mode.get() == "new"
        u.subject_set_name = self.set_name.get() if new else ""
        u.subject_set_id = "" if new else self.set_id.get()
        u.limit = self.limit.int_value(0)
        u.checkpoint_every = self.checkpoint.int_value(u.checkpoint_every)
        u.sleep = self.pause.float_value(u.sleep)

    def validate(self) -> str | None:
        csv_path = self._metadata_csv()
        if not csv_path or not csv_path.is_file():
            return ("There is no metadata.csv in the output folder. Write the "
                    "metadata sheets first — stage 4.")
        if not self.cfg.transect_id:
            return ("Set the transect ID prefix on stage 1. Every subject is "
                    "stamped with it, and it is how a classification is traced "
                    "back to a transect.")
        if self.mode.get() == "new" and not self.set_name.get():
            return "Name the new subject set."
        if self.mode.get() == "existing" and not self.set_id.get().isdigit():
            return ("Give the ID of the subject set to add to — the last "
                    "number in its Zooniverse URL.")
        usable, why = zooniverse.client_available()
        if not usable:
            return why
        ready, message = zooniverse.credentials_status()
        if not ready:
            return message
        return None

    def confirm(self) -> str | None:
        csv_path = self._metadata_csv()
        try:
            rows = len(zooniverse.read_metadata(csv_path))
        except OSError:
            rows = 0
        if self.cfg.upload.limit:
            rows = min(rows, self.cfg.upload.limit)
        target = (f"a new subject set called '{self.cfg.upload.subject_set_name}'"
                  if self.mode.get() == "new"
                  else f"subject set {self.cfg.upload.subject_set_id}")
        return (f"Upload up to {rows:,} subjects into {target}?\n\n"
                "Volunteers can start classifying as soon as the set is "
                "attached to a workflow, and subjects cannot be un-uploaded.")

    def on_result(self, result) -> None:
        self.refresh()
        set_id = result.advance.get("subject_set_id")
        if set_id:
            # The ID has to be recorded by hand in tracker.xlsx, so put it
            # somewhere it cannot scroll out of the log.
            self.status.configure(
                text=f"Subject set ID {set_id} — record this in tracker.xlsx",
                text_color=T.OK)


# --------------------------------------------------------------------------
#  6 - Export
# --------------------------------------------------------------------------


class ExportPanel(StagePanel):
    key = "export"
    title = "6.  Export classifications"
    subtitle = ("Once volunteers have worked on the subjects, ask Zooniverse "
                "for the classifications. Give every subject set the points "
                "have passed through — a subject moves as it retires, and its "
                "classifications stay with whichever set it was in at the "
                "time.")
    run_text = "Export classifications"
    check_text = "Check only — do not generate exports"

    def build(self) -> None:
        c1 = self.card(
            "Which subject sets",
            "The last number in a subject set's Zooniverse URL — "
            "zooniverse.org/lab/24397/subject-sets/135054. Separate several "
            "with commas; paste a column straight from tracker.xlsx if that "
            "is easier.")
        self.set_ids = Field(c1.body, "Subject set IDs", width=300,
                             hint_text="135009, 135054", caption_width=150,
                             validate=self._check_ids)
        self.set_ids.grid(row=0, column=0, sticky="ew")
        button(c1.body, "List the project's sets", self._list_sets, "ghost",
               width=180).grid(row=1, column=0, sticky="w", pady=(8, 0))
        self.sets_box = textbox(c1.body, height=132)
        self.sets_box.grid(row=2, column=0, sticky="ew", pady=(8, 0))
        hint(c1.body, "Which sets a transect's subjects are in is not knowable "
                      "from the upload log — they move. Listing them is a few "
                      "dozen rows, so pick from the list rather than "
                      "remembering."
             ).grid(row=3, column=0, sticky="ew", pady=(6, 0))

        c2 = self.card("Where to save them",
                       "One CSV per subject set, plus a combined one with "
                       "duplicate classifications dropped. The combined file "
                       "is what stage 7 reads.")
        self.output = PathField(
            c2.body, "Export folder", kind="folder",
            title="Where should the exports go?")
        self.output.grid(row=0, column=0, sticky="ew")
        self.combined = Field(c2.body, "Combined CSV", width=260,
                              hint_text="all_classifications.csv",
                              caption_width=150,
                              validate=lambda s: (True, "stage 7 reads this"))
        self.combined.grid(row=1, column=0, sticky="ew", pady=(8, 0))
        hint(c2.body, "Add a row to the Export Log sheet in tracker.xlsx "
                      "afterwards.").grid(row=2, column=0, sticky="ew",
                                          pady=(6, 0))

    def _check_ids(self, text: str) -> tuple[bool, str]:
        ids = export.parse_ids(text)
        if not ids:
            return False, "digits, separated by commas"
        return True, f"{len(ids)} subject set(s)"

    def _list_sets(self) -> None:
        """Fetch the project's subject sets so the operator can pick.

        Which sets hold a transect's subjects cannot be read off the upload
        log: retirement moves them. Listing is one cheap call and turns a
        memory exercise into a choice.
        """
        usable, why = zooniverse.client_available()
        if not usable:
            self.app.warn(why)
            return
        ready, message = zooniverse.credentials_status()
        if not ready:
            self.app.warn(message)
            return
        set_text(self.sets_box, "Asking Zooniverse…")
        self.update_idletasks()
        try:
            sets = export.list_project_sets()
        except Exception as exc:
            set_text(self.sets_box, f"Could not list the subject sets: {exc}")
            return
        if not sets:
            set_text(self.sets_box, "The project has no subject sets.")
            return
        lines = [f"{'ID':<9} {'subjects':>9}  name"]
        lines += [f"{sid:<9} {count:>9,}  {name}" for sid, name, count in sets]
        lines += ["", "Copy the IDs you want into the box above."]
        set_text(self.sets_box, "\n".join(lines))

    def load(self) -> None:
        e = self.cfg.export
        self.set_ids.set(e.subject_set_ids or self.cfg.upload.subject_set_id)
        root = Path(__file__).resolve().parent.parent.parent / "exports"
        self.output.set(e.output_dir or str(root))
        self.combined.set(e.combined_csv or self._suggest_combined())

    def _suggest_combined(self) -> str:
        stem = metadata.sheet_stem(self.cfg.site_name, self.cfg.survey_date,
                                   self.cfg.transect_number)
        return (f"{stem}_classifications.csv" if stem != "transect"
                else "all_classifications.csv")

    def collect(self) -> None:
        e = self.cfg.export
        e.subject_set_ids = self.set_ids.get()
        e.output_dir = self.output.get()
        e.combined_csv = self.combined.get()

    def validate(self) -> str | None:
        if not export.parse_ids(self.set_ids.get()):
            return ("Give at least one subject set ID — the last number in a "
                    "subject set's Zooniverse URL.")
        if not self.output.get():
            return "Choose where to save the exports."
        usable, why = zooniverse.client_available()
        if not usable:
            return why
        ready, message = zooniverse.credentials_status()
        if not ready:
            return message
        return None

    def confirm(self) -> str | None:
        ids = export.parse_ids(self.set_ids.get())
        return (f"Ask Zooniverse to generate a fresh classifications export "
                f"for {len(ids)} subject set(s)?\n\n" + ", ".join(ids)
                + "\n\nEach one can take several minutes while their servers "
                  "build it.")


# --------------------------------------------------------------------------
#  7 - Rejoin
# --------------------------------------------------------------------------


class RejoinPanel(StagePanel):
    key = "rejoin"
    title = "7.  Rejoin to Toolbox"
    subtitle = ("Apply the label rules to everything the volunteers said, "
                "write the Toolbox import, and report where every point "
                "stands: settled, still being classified, or waiting for a "
                "person in Toolbox.")
    run_text = "Rejoin to Toolbox"
    check_text = "Check only — write no sheets"

    def build(self) -> None:
        # The counts go first, before the inputs. This is the page somebody
        # opens to ask "how much is done?", and burying the answer three cards
        # down means scrolling for it every time.
        c0 = self.card(
            "Where every point stands",
            "'Needs Toolbox' means Zooniverse is finished with the point and "
            "it is still unresolved — no more votes are coming, so a person "
            "has to label it.")
        self.status = textbox(c0.body, height=170)
        self.status.grid(row=0, column=0, sticky="ew")

        # Right under the counts, because it is the one thing on this page a
        # person can act on immediately: these points are unresolved only
        # because nothing knows which label the volunteers' answer means.
        # Hidden until a run finds some -- an empty card is a puzzle.
        self.unmapped_card = self.card(
            "Labels the labelset could not name",
            "The volunteers agreed, but their answer does not match a label "
            "in the labelset, so those points were left as Review. Say which "
            "label each one means and they will resolve on the next run.")
        self.unmapped_rows = ctk.CTkFrame(self.unmapped_card.body,
                                          fg_color="transparent")
        self.unmapped_rows.grid(row=0, column=0, sticky="ew")
        self.unmapped_rows.grid_columnconfigure(0, weight=1)
        self.map_btn = button(self.unmapped_card.body, "Map these labels",
                              self._add_expansions, "ghost", width=160)
        self.map_btn.grid(row=1, column=0, sticky="w", pady=(10, 0))
        self.map_note = hint(self.unmapped_card.body, "")
        self.map_note.grid(row=2, column=0, sticky="ew", pady=(6, 0))
        self._unmapped: list[tuple[str, object]] = []
        self._code_for: dict[str, str] = {}
        self.unmapped_card.grid_remove()

        c1 = self.card("What to join",
                       "The Toolbox sheet stage 4 wrote for this transect, "
                       "and the combined export from stage 6.")
        self.toolbox = PathField(
            c1.body, "Toolbox CSV", kind="file",
            title="Select the Toolbox annotations CSV",
            filetypes=(("CSV", "*.csv"), ("All files", "*.*")))
        self.classifications = PathField(
            c1.body, "Classifications", kind="file",
            title="Select the combined classifications export",
            filetypes=(("CSV", "*.csv"), ("All files", "*.*")),
            on_change=self._csv_changed)
        self.annotations = PathField(
            c1.body, "Annotation JSON", kind="file",
            title="Select the Toolbox annotation JSON (optional)",
            filetypes=(("JSON", "*.json"), ("All files", "*.*")))
        self.output = PathField(
            c1.body, "Output folder", kind="folder",
            title="Where should the Toolbox import go?")
        for i, w in enumerate((self.toolbox, self.classifications,
                               self.annotations, self.output)):
            w.grid(row=i, column=0, sticky="ew", pady=(0 if i == 0 else 8, 0))
        hint(c1.body, "The annotation JSON is optional but wanted: Toolbox "
                      "needs it to keep the pixel-level fields, so without it "
                      "only the CSV is written."
             ).grid(row=4, column=0, sticky="ew", pady=(6, 0))

        c2 = self.card("The rules",
                       "As configured in scripts/zooni_to_toolbox_annot.py — "
                       "the same rules Caesar is set up with. An expert answer "
                       "beats a crowd one, and a multiple-choice consensus "
                       "beats a yes/no denial.")
        self.rules = textbox(c2.body, height=104)
        self.rules.grid(row=0, column=0, sticky="ew")
        flags = ctk.CTkFrame(c2.body, fg_color="transparent")
        flags.grid(row=1, column=0, sticky="ew", pady=(10, 0))
        self.use_yn = ctk.BooleanVar(value=True)
        self.use_yn_exp = ctk.BooleanVar(value=True)
        self.use_multi = ctk.BooleanVar(value=True)
        self.use_multi_exp = ctk.BooleanVar(value=True)
        for i, (text, var) in enumerate((
                ("Yes/No crowd", self.use_yn),
                ("Yes/No expert", self.use_yn_exp),
                ("Multi-choice crowd", self.use_multi),
                ("Multi-choice expert", self.use_multi_exp))):
            checkbox(flags, text, variable=var).grid(
                row=i // 2, column=i % 2, sticky="w", padx=(0, 24), pady=2)
        hint(c2.body, "Leave all four on for a normal run. Turning one off is "
                      "for working out why a point resolved the way it did."
             ).grid(row=2, column=0, sticky="ew", pady=(8, 0))

    def _csv_changed(self, path: str) -> None:
        if not path:
            self.classifications.note("")
            return
        p = Path(path)
        if not p.is_file():
            self.classifications.note("no file at that path", ok=False)
            return
        self.classifications.note(f"{p.stat().st_size / 1e6:.1f} MB", ok=True)

    def load(self) -> None:
        r = self.cfg.rejoin
        cfg = self.cfg
        out = Path(cfg.output_dir) if cfg.output_dir else None
        default_toolbox = ""
        if out is not None:
            candidate = out / metadata.toolbox_csv_name(
                cfg.site_name, cfg.survey_date, cfg.transect_number)
            if candidate.is_file():
                default_toolbox = str(candidate)
        self.toolbox.set(r.toolbox_csv or default_toolbox)
        self.classifications.set(r.classifications_csv or cfg.report.export_csv)
        self.annotations.set(r.annotations_json)
        self.output.set(r.output_dir or (str(out) if out else ""))
        self.use_yn.set(r.use_yesno)
        self.use_yn_exp.set(r.use_yesno_expert)
        self.use_multi.set(r.use_multi)
        self.use_multi_exp.set(r.use_multi_expert)
        self._show_rules()
        if not self.status.get("1.0", "end").strip():
            set_text(self.status,
                     "Not run yet.\n\n"
                     "Run this stage to see how many points are settled, how "
                     "many are still\nbeing classified on Zooniverse, and how "
                     "many are waiting for a person\nin Toolbox. A check "
                     "reports the counts without writing anything.")

    def _show_rules(self) -> None:
        if not rejoin.available():
            set_text(self.rules,
                     f"{rejoin.LINKER} is missing — it ships alongside this "
                     "app in the scripts folder.")
            return
        try:
            set_text(self.rules, "\n".join(rejoin.workflow_summary()))
        except Exception as exc:
            set_text(self.rules, f"Could not read the rules: {exc}")

    def refresh(self) -> None:
        self.load()

    def collect(self) -> None:
        r = self.cfg.rejoin
        r.toolbox_csv = self.toolbox.get()
        r.classifications_csv = self.classifications.get()
        r.annotations_json = self.annotations.get()
        r.output_dir = self.output.get()
        r.use_yesno = bool(self.use_yn.get())
        r.use_yesno_expert = bool(self.use_yn_exp.get())
        r.use_multi = bool(self.use_multi.get())
        r.use_multi_expert = bool(self.use_multi_exp.get())

    def validate(self) -> str | None:
        if not rejoin.available():
            return (f"The linker is missing: {rejoin.LINKER}. It ships "
                    "alongside this app in the scripts folder.")
        for widget, what in ((self.toolbox, "Toolbox annotations CSV"),
                             (self.classifications, "classifications export")):
            path = widget.get()
            if not path:
                return f"Choose the {what}."
            if not Path(path).is_file():
                return f"There is no file at {path}."
        if self.annotations.get() and not Path(self.annotations.get()).is_file():
            return f"There is no file at {self.annotations.get()}."
        if not self.cfg.classify.labelset:
            return ("Choose the labelset JSON on stage 3. The rules map a "
                    "volunteer's answer onto it.")
        if not any((self.use_yn.get(), self.use_yn_exp.get(),
                    self.use_multi.get(), self.use_multi_exp.get())):
            return "Turn on at least one workflow."
        if not self.output.get():
            return "Choose where the Toolbox import should go."
        return None

    def confirm(self) -> str | None:
        return (f"Write the Toolbox import into {self.output.get()}?\n\n"
                "Existing toolbox_import.csv and qaqc_classifications.csv "
                "there are overwritten.")

    def on_result(self, result) -> None:
        # The counts only. Which files were written is in the log; leading the
        # card with that pushes the numbers out of view.
        block = result.advance.get("status_block")
        set_text(self.status, block or result.summary())
        self._show_unmapped(result.advance.get("unmapped") or [])

    # ---- mapping a label the labelset does not have --------------

    def _show_unmapped(self, pairs) -> None:
        """One row per label the rules could not map, with a code to pick."""
        for child in self.unmapped_rows.winfo_children():
            child.destroy()
        self._unmapped = []
        if not pairs:
            self.unmapped_card.grid_remove()
            return

        try:
            choices = rejoin.label_choices(self.cfg.classify.labelset)
        except (OSError, ValueError) as exc:
            self.unmapped_card.grid()
            label(self.unmapped_rows,
                  f"Could not read the labelset: {exc}").grid(
                      row=0, column=0, sticky="ew")
            self.map_btn.configure(state="disabled")
            return

        # "CODE — long name" reads better in a dropdown than a bare code, and
        # the code is recovered from this map rather than by splitting the
        # string back apart on a dash the long names also contain.
        self._code_for = {f"{code} — {long_name}": code
                          for code, long_name in choices}
        values = list(self._code_for)
        self.map_btn.configure(state="normal")
        for i, (raw, count) in enumerate(pairs):
            row = ctk.CTkFrame(self.unmapped_rows, fg_color="transparent")
            row.grid(row=i, column=0, sticky="ew", pady=3)
            # The gap goes between the name and the count, so the count sits
            # next to the dropdown it belongs with rather than stranded beside
            # the label with a hand's width of nothing after it.
            row.grid_columnconfigure(0, weight=1)
            # The raw choice text can be a whole line of image markdown, so it
            # is shown cleaned -- the same text the rules look up.
            shown = rejoin.clean_choice(raw)
            label(row, f"{shown}", font=T.FONT_BODY).grid(
                row=0, column=0, sticky="w")
            label(row, f"{count:,} point(s)", muted=True).grid(
                row=0, column=1, sticky="e", padx=(10, 12))
            picker = combobox(row, values, width=260)
            picker.set("")
            picker.grid(row=0, column=2, sticky="e")
            self._unmapped.append((raw, picker))
        self.map_note.configure(
            text="Written to label_expansions.json beside "
                 f"{rejoin.LINKER.name}, so the command-line script uses the "
                 "same mappings. Leave a row blank to skip it.",
            text_color=T.TEXT_MUTED)
        self.unmapped_card.grid()

    def _add_expansions(self) -> None:
        mapping = {}
        for raw, picker in self._unmapped:
            chosen = picker.get().strip()
            if not chosen:
                continue
            # Typed as well as picked: accept a bare code too.
            mapping[raw] = self._code_for.get(chosen, chosen)
        if not mapping:
            self.map_note.configure(
                text="Pick a label for at least one row first.",
                text_color=T.WARN)
            return
        try:
            outcome = rejoin.add_expansions(mapping, self.cfg.classify.labelset)
        except Exception as exc:
            self.map_note.configure(text=f"Could not save the mapping: {exc}",
                                    text_color=T.WARN)
            return
        self.app.log_lines(outcome.summary(), warn=not outcome.ok)
        self.map_note.configure(
            text=" · ".join(line.strip()
                            for line in outcome.summary().splitlines()
                            if line.strip()),
            text_color=T.TEXT_MUTED if outcome.ok else T.WARN)
        # A row that took is settled; freezing its picker stops it being
        # mapped twice and shows at a glance which ones still want an answer.
        left = 0
        for raw, picker in self._unmapped:
            if rejoin.clean_choice(raw) in outcome.added:
                picker.configure(state="disabled")
            else:
                left += 1
        self.map_btn.configure(state="normal" if left else "disabled")


# --------------------------------------------------------------------------
#  8 - Report
# --------------------------------------------------------------------------


class ReportPanel(StagePanel):
    key = "report"
    title = "8.  Build the report"
    subtitle = ("A multi-sheet Excel summary of an export: overview, how far "
                "along each transect is, per workflow, per subject, per "
                "volunteer, the answer breakdown, per source still, and "
                "classification times.")
    run_text = "Build the report"
    check_text = "Check only — do not write the workbook"

    def build(self) -> None:
        c1 = self.card("The export to summarise")
        self.export_csv = PathField(
            c1.body, "Export CSV", kind="file",
            title="Select the classifications export",
            filetypes=(("CSV", "*.csv"), ("All files", "*.*")),
            on_change=self._csv_changed)
        self.export_csv.grid(row=0, column=0, sticky="ew")
        self.output = PathField(c1.body, "Report folder", kind="folder",
                                title="Where should the workbook go?")
        self.output.grid(row=1, column=0, sticky="ew", pady=(8, 0))

        c2 = self.card(
            "Filters",
            "Transect first. The multiple-choice and expert subject sets are "
            "shared by the whole project, so an export of them carries every "
            "transect's subjects — without this filter a report meant for one "
            "transect summarises all of them.")
        self.transect = Field(c2.body, "Transect ID", width=230,
                              hint_text="all transects", caption_width=150,
                              validate=self._check_transects)
        self.workflow = Field(c2.body, "Workflow ID", width=120,
                              hint_text="all", caption_width=150,
                              validate=_check_digits("one workflow only"))
        self.source = Field(c2.body, "Source image", width=230,
                            hint_text="all", caption_width=150,
                            validate=lambda s: (True, "that still only"))
        self.stack(c2, self.transect, self.workflow, self.source)
        hint(c2.body, "Matched against the transect_id stamped on each "
                      "subject, so it has to be exact — EBM_W25_T6, not "
                      "EBM_T6. Several can be separated by commas. All three "
                      "filters are optional."
             ).grid(row=3, column=0, sticky="ew", pady=(8, 0))

        c3 = self.card("Analysis tool")
        self.tool = label(c3.body, "", muted=True)
        self.tool.grid(row=0, column=0, sticky="ew")

    @staticmethod
    def _check_transects(text: str) -> tuple[bool, str]:
        ids = [t.strip() for t in text.split(",") if t.strip()]
        if not ids:
            return False, "e.g. EBM_W25_T6"
        return True, f"{len(ids)} transect(s)" if len(ids) > 1 else "that one"

    def _csv_changed(self, path: str) -> None:
        if not path:
            self.export_csv.note("")
            return
        p = Path(path)
        if not p.is_file():
            self.export_csv.note("no file at that path", ok=False)
            return
        self.export_csv.note(f"{p.stat().st_size / 1e6:.1f} MB", ok=True)
        if not self.output.get():
            self.output.set(p.parent.parent / "reports"
                            if p.parent.name == "exports" else p.parent / "reports")

    def load(self) -> None:
        r = self.cfg.report
        self.export_csv.set(r.export_csv
                            or self.cfg.rejoin.classifications_csv)
        self.output.set(r.output_dir)
        self.workflow.set(r.workflow_id)
        self.source.set(r.source_image)
        # Defaults to the transect stage 1 was pointed at, which is almost
        # always the one being reported on. Typed once, not twice.
        self.transect.set(r.transect_id or self.cfg.transect_id)
        tool = report.analyser_path()
        self.tool.configure(
            text=(f"Delegates to {tool.name} in scripts/."
                  if report.available()
                  else f"{tool} is missing — it ships alongside "
                       "this app in the scripts folder."),
            text_color=T.TEXT_MUTED if report.available() else T.WARN)

    def collect(self) -> None:
        r = self.cfg.report
        r.export_csv = self.export_csv.get()
        r.output_dir = self.output.get()
        r.workflow_id = self.workflow.get()
        r.source_image = self.source.get()
        r.transect_id = self.transect.get()

    def validate(self) -> str | None:
        if not report.available():
            return (f"The analysis tool is missing: {report.analyser_path()}. "
                    "It ships alongside this app in the scripts folder.")
        if not self.export_csv.get():
            return "Choose the classifications export CSV."
        if not Path(self.export_csv.get()).is_file():
            return f"There is no file at {self.export_csv.get()}."
        if not self.output.get():
            return "Choose where the workbook should go."
        return None


# --------------------------------------------------------------------------
#  Validators shared by several panels
# --------------------------------------------------------------------------


def _check_date(text: str) -> tuple[bool, str]:
    from datetime import date

    try:
        y, m, d = (int(p) for p in text.split("-"))
        date(y, m, d)
    except ValueError:
        return False, "use YYYY-MM-DD"
    return True, "ok"


def _check_int(ok_note: str):
    def check(text: str) -> tuple[bool, str]:
        stripped = text.strip().lstrip("-")
        if not stripped.isdigit():
            return False, "whole number"
        return True, ok_note
    return check


def _check_digits(ok_note: str):
    def check(text: str) -> tuple[bool, str]:
        if not text.strip().isdigit():
            return False, "digits only"
        return True, ok_note
    return check


def _check_float(text: str) -> tuple[bool, str]:
    try:
        value = float(text)
    except ValueError:
        return False, "a number of seconds"
    if value < 0:
        return False, "cannot be negative"
    return True, "between subject saves"


#: In rail order. The app builds one of each.
PANELS = (TransectPanel, PatchPanel, ClassifyPanel, MetadataPanel,
          UploadPanel, ExportPanel, RejoinPanel, ReportPanel)
