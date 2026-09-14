# Zooniverse Kelp Quest Project

This repository organizes everything for our Zooniverse citizen science project — from extracting annotated patches out of CoralNet-Toolbox through to uploading them to Zooniverse and downloading the volunteer classifications.

---

## Repository Structure

```
zooniverse-project/
├── README.md
├── tracker.xlsx                        ← master tracking spreadsheet
├── run_KelpQuest.bat                   ← double-click to start the desktop app
├── launch.py                           ← PyInstaller entry shim
├── kelpquest/                          ← the desktop app
│   ├── discovery.py                    ← read a transect folder
│   ├── telemetry.py                    ← the UTC transect CSV: identity + per-still depth
│   ├── sampling.py                     ← scatter annotation points
│   ├── patches.py                      ← cut and draw the patch images
│   ├── classify.py                     ← run our YOLO classifier
│   ├── metadata.py                     ← write the Toolbox, Zooniverse and joined sheets
│   ├── zooniverse.py                   ← create subjects
│   ├── export.py                       ← download the classifications
│   ├── report.py                       ← the Excel summary
│   ├── pipeline.py                     ← the seven stages, and their contract
│   └── gui/                            ← window, stage rail and panels
├── scripts/                            ← the original command-line tools
│   ├── toolbox_to_subjects.py          ← extract patches from Toolbox annotations
│   ├── import_subjects.py              ← upload patches to Zooniverse
│   ├── export_subjectset.py            ← export classifications for one subject set
│   ├── analyze_classifications.py      ← generate Excel summary report
│   └── config.example.env              ← credentials template
├── tests/
└── exports/                            ← downloaded classification CSVs (git-ignored)
```

---

## Kelp Quest — the desktop app

`run_KelpQuest.bat` opens a window that carries one transect from a folder of
edited stills all the way to an Excel summary of what volunteers said about it.
It does not need CoralNet-Toolbox upstream: it scatters its own annotation
points, so the Toolbox sheet becomes something the run *produces*, ready to
import and verify.

Eight stages down the left, the selected stage's settings in the middle, one
shared log and progress bar along the bottom. Finishing a stage fills in the
next stage's inputs and moves the selection there, so paths are never retyped.

```
UTC transect CSV ──┐
                   ▼
transect folder ─► 1 · Transect folder    scan; identity read from the telemetry
                   │
                   ▼
                   2 · Cut patches        50 points a still, 224 px patch, 784 px crop
                   │                      (classifies in the same pass by default)
                   ▼
                   3 · Classify patches   our YOLO model, top 5 suggestions
                   │
                   ▼
                   4 · Write metadata     Toolbox sheet · metadata.csv · telemetry join
                   │
                   ▼
                   5 · Upload to Zooniverse   one subject per patch, resumable
                   │
                   ▼       (volunteers classify; subjects move between sets)
                   6 · Export classifications  every set they passed through
                   │
                   ▼
                   7 · Rejoin to Toolbox  the label rules + where every point stands
                   │
                   ▼
                   8 · Build the report   multi-sheet Excel summary
```

Every stage runs on its own, so a step can be redone without redoing the
transect — re-classify with different weights, or rewrite a sheet after
correcting the site name.

### The check toggle

**"Check only" is ticked every time the app opens, and again after each stage
advances.** A check reports what *would* happen and writes nothing: how many
patches would be cut, which sheets would be written, what would be uploaded,
how big the subject set is. One stage writes thousands of JPEGs and another
creates subjects volunteers immediately start work on, so the first click on a
new set of paths is always the safe one. Untick it and the app names what it is
about to do before doing it.

Stage 1 only ever reads, so it has no check box.

### Telemetry does the identity work

Point the Transect page at the UTC transect CSV for the transect — "Find it for
me" looks for it under the flight folder — and it supplies:

| From the CSV | Used for |
|---|---|
| `Transect_ID` | the transect ID prefix, e.g. `EBM_W25` |
| `Site_name`, `Transect_number`, `Date` | site, transect, survey date |
| `Depth`, `Altitude`, `Latitude`, `Longitude`, `Heading`, `Velocity_mps`, `Width`, `Area_m2` | per-still columns, joined on the timestamp in the filename |

The join is an exact second match between the still's name
(`2025_01_28_11-34-11.jpg`) and the CSV's `Date`/`Time` — the same join
`scripts/join_percent_cover_telemetry.py` does after the fact. On the transect
this was built against, 58 of 58 stills matched. Interpolating was deliberately
not attempted: a blank column is easier to notice than a plausible wrong
position.

Without a CSV, site, date and transect fall back to reading the folder names
and no telemetry columns are written.

### The transect ID

You give the **prefix** — the site and season, `EBM_W25` — and the transect
number is appended, giving `EBM_W25_T6`. The prefix comes from the telemetry
CSV when there is one; type it once otherwise. It is asked for as two halves on
purpose: a single free-text field is how transect 6's stills end up uploaded
under transect 1's identity.

### Starting it

Double-click `run_KelpQuest.bat`. The first run builds a private Python
environment in `%LOCALAPPDATA%\KelpQuest` and installs what it needs, which
takes several minutes; after that it starts straight away. From an environment
that already has the dependencies:

```bash
python -m kelpquest
```

### What a run writes

Everything lands in the output folder, `<transect>/zooniverse_patches` by
default:

| File | What it is |
|---|---|
| `<still>_r<row>_c<col>.jpg` | one patch per point — green box on the patch, red crosshair on the point, predicted label above |
| `patches.csv` | the manifest: every point, its top-5 suggestions and confidences |
| `<date>_<site>_<T#>_toolbox_annotations.csv` | import into CoralNet-Toolbox and verify point by point |
| `<date>_<site>_<T#>_annotations_telemetry.csv` | every point with the depth and position of its still — what a cover-versus-depth analysis needs |
| `metadata.csv` | what the upload reads; becomes each subject's metadata |
| `upload_log.csv` | every subject created, so a re-run skips it |
| `fail_log.csv` | anything skipped or refused, with the reason |

Keys prefixed `#` in `metadata.csv` — the model's confidence and all the
telemetry — are hidden from volunteers by Zooniverse. They are for the analysis
afterwards; a depth or a confidence score on screen is context nobody was asked
to weigh while deciding whether they agree with the model.

### Subjects move, so the export takes several sets

A subject does not sit still. It starts in the yes/no set; on retirement it may
move to the multiple-choice set, and from there to an expert set. Its
classifications stay with whichever set it was in at the time, so **exporting
only the set a transect was uploaded into makes every subject that has moved on
look like one nobody has classified.**

Stage 6 therefore takes a list of subject set IDs. **List the project's sets**
fetches them with names and subject counts, because which sets hold a
transect's subjects is not knowable from the upload log. It writes one CSV per
set plus a combined one, deduplicated on `classification_id` — a subject that
belongs to two sets comes back in both exports, and counting its votes twice
would push points over thresholds they had not earned.

### Don't download the same 200 MB twice

The multiple-choice and expert sets are shared by the whole project, so working
through transects one at a time **their exports are the same file every time**
— and the multiple-choice one runs to a couple of hundred megabytes that
Zooniverse takes minutes to build. Only the transect's own yes/no set is
genuinely new.

Tick **Reuse exports already in the folder** and a set whose export is already
there is taken from disk; the rest are generated. The combine still runs across
the mixture, so stage 7 gets the same complete combined CSV either way.

A set is matched to a file by `export_log.csv`, which stage 6 writes as it
downloads, and failing that by the sanitised subject-set name in the filename —
so exports downloaded before the log existed are reused too rather than being
fetched again once.

**Reuse trades a correct answer for a fast one.** A reused export has none of
the votes cast since it was downloaded, so a point that has reached consensus
in the meantime reads as still being classified. That is why it is off by
default, why every reused file is listed with its age, and why the run warns
with the age of the oldest. For a set that has collected votes since, name it
under **…but re-download** and it is fetched even so.

Give each transect's combined CSV its own name. It is the file that transect's
stage 7 reads, and the panel flags it when the stored name no longer matches
the transect stage 1 is pointed at.

### Where every point stands

Stage 7 applies the label rules and then answers the question the rules do not:
how much is actually done. Every annotation point lands in exactly one of four
buckets, and the split that matters is the middle two.

| | Meaning |
|---|---|
| **Verified** | a rule resolved it; the label is settled. The card also says how many of these are retired on Zooniverse. |
| **Needs review in Toolbox** | unresolved *and* Zooniverse is finished with it — denied, voted "not sure", a consensus label the labelset does not have, or retired on count without agreement. No more votes are coming. |
| **Still being classified on Zooniverse** | unresolved, but not retired everywhere it lives. More votes are still arriving. |
| **No classifications found** | nothing in the exports given. Either never uploaded, or the set it moved to was not exported. |

"Retired but unresolved" versus "not retired and unresolved" is the whole point
of the column: the first is a job for a person, the second is a waiting game.

On the transect this was built against — Centennial T1, with only its original
subject set exported:

```
3,100 annotation point(s) in the transect
3,043 matched a classification

  Verified — label settled                   648   20.9%
    of which retired on Zooniverse           509
  Needs review in Toolbox                    151    4.9%
  Still being classified on Zooniverse     2,244   72.4%
  No classifications found                    57    1.8%

Why the unresolved ones are unresolved:
  waiting for more votes                   2,244
  denied in yes/no                           125
  retired without consensus                   26
```

Adding the multiple-choice and expert sets to stage 6 moves a large share of
those 2,244 into Verified or Needs Toolbox — they are not waiting, they are
waiting *somewhere that export could not see*.

### The rules

Stage 7 does not decide labels itself. It calls `link_annotations` in
`scripts/zooni_to_toolbox_annot.py`, which holds the determination rules that
mirror the Caesar configuration, so there is one copy of them rather than two
to keep in step. The panel shows them as configured:

| Workflow | Threshold | Effect |
|---|---|---|
| Multi-choice expert | n ≥ 1, ≥ 67% agree | label mapped from the labelset, Verified |
| Multi-choice crowd | n ≥ 3, ≥ 67% agree | same; expert wins if both reached consensus |
| Yes/No expert | n ≥ 1, ≥ 75% Yes | keeps the model's label, Verified |
| Yes/No crowd | n ≥ 5, ≥ 75% Yes | same, unless an expert denied |

Both multi-choice results override a yes/no denial. Anything else stays
`Review` / unverified. The four checkboxes let you drop a workflow — useful for
working out why a point resolved the way it did, not for a normal run.

Outputs land in the folder you choose:

| File | What it is |
|---|---|
| `toolbox_import.csv` | Toolbox's own columns, ready to import |
| `toolbox_import_annotations.json` | written when you supply the annotation JSON, so the pixel-level fields survive |
| `qaqc_classifications.csv` | every point with its vote counts and why it resolved as it did |
| `subject_status.csv` | every point with its status, reason, subject IDs and retirement |
| `unmapped_multi_consensus_labels.csv` | consensus labels the labelset could not name, with counts |

### Labels the labelset could not name

Sometimes the volunteers agree and the point still will not resolve, because
their answer is not a labelset code. A Zooniverse choice is written to be read
on a button — `Wood`, `Sugar`, `5-rib` — while the labelset calls those
`SU_wood`, `KE_sugar`, `KE_5rib`. An **expansions table** maps one to the
other, and a choice it does not cover comes back as
`zoon_status = multi_consensus_unmapped` and `Label = Review`.

Stage 7 lists those on the page, one row each with its point count and a
dropdown of the labelset's codes. Pick a label, press **Map these labels**, and
run the stage again — those points resolve. It writes
`scripts/label_expansions.json`, which is the same file
`zooni_to_toolbox_annot.py` reads, so a mapping made in the app is in force on
the command line too. Built-in defaults stay in `DEFAULT_EXPANSIONS` in that
script and the file holds only the additions, so a later correction to a
default is not shadowed by a stale copy of it.

A code the labelset does not have is refused rather than written. An expansion
pointing at a missing code resolves to nothing, so the next run would report
the very same label as unmapped with no hint as to why.

A check run lists the unmapped labels too, so they can be mapped before the run
that writes anything.

### Settings that matter

| Setting | Default | Why |
|---|---|---|
| Points per image | 50 | matches the finalized 2024–2025 transects |
| Patch size | 224 px | what the classifier was trained on |
| Edge margin | 224 px | one patch width, so the patch is always wholly inside the frame |
| Seed | 42 | the same seed on the same transect gives the same points |
| Crop scale | 3.5 | 784 px of context around a 224 px patch |
| Classify in the same pass | on | cutting and classifying both need the still decoded, so doing both encodes each patch once instead of twice |
| Weights | — | `best.pt` from a run in `classification_model` |
| Labelset | shared Dropbox copy | turns a short code into a long label |

### Things worth knowing

- **Stopping is safe.** Press Stop and the sheets are written for the patches
  that were actually cut, so what is on disk is uploadable rather than
  orphaned.
- **An interrupted upload resumes.** Every subject created is in
  `upload_log.csv`; run it again and those are skipped.
- **The export is generated fresh.** Zooniverse builds it server-side, which
  takes minutes, and the app waits. Asking without waiting hands back whatever
  export was generated last — quietly the wrong file on a set that has had new
  classifications since.
- **Online-only files are refused.** A OneDrive or Dropbox placeholder reads at
  network speed, which looks exactly like a hang. The app counts them and asks
  before starting.
- **Record the Subject Set ID.** It is shown on the Upload page after a run and
  goes in `tracker.xlsx`.
- **Credentials** come from `scripts/.env`, the same file the scripts use. The
  app never writes them anywhere.
- **The report** delegates to `scripts/analyze_classifications.py`, and the
  rejoin to `scripts/zooni_to_toolbox_annot.py`, so the sheets and the rules
  each have one implementation rather than two to keep in step.

### One transect, several, or all of them

Stage 8's **Transect IDs** box takes a comma-separated list, and the workbook's
per-transect sheet then carries a row each — which is how two transects get
compared. **List the transects in this export** says what is actually in the
file, with classification and subject counts, because an export of the shared
sets holds whichever transects happened to be in them and that is not something
anybody can recall. It streams the CSV rather than flattening it: three seconds
against twenty on a 250 MB export.

Left empty, every transect in the export is summarised. The filter matches the
`transect_id` stamped on each subject, so it has to be exact — `EBM_W25_T6`,
not `EBM_T6` — and subjects uploaded before that field existed carry none, so a
filtered report cannot see them. An unfiltered run says how many those are.

### If uploading and exporting refuse to start

The app checks whether `panoptes-client` can be imported *before* using it,
because on Windows it can crash the interpreter rather than raise: it pulls in
`python-magic`, which loads a native libmagic, and the plain wheel ships no
magic database. That is a segfault, so `try: import` cannot catch it and the
window would simply vanish the moment you pressed Upload.

The fix, once per environment:

```bash
pip install python-magic-bin
```

Everything except stages 5 and 6 works without it.

### Tests

```bash
python -m pytest tests/
```

Nothing in the suite needs Dropbox, a model checkpoint or a network — stills
and telemetry are synthesised.

---

## The command-line scripts

The original per-step scripts still work and are documented below. Use them for
a transect that already has Toolbox annotations you want to keep, which is the
one thing the app does not do.

### Full Workflow

```
CoralNet-Toolbox
    annotations.csv
        │
        ▼
 toolbox_to_subjects.py
        │  creates patch images + metadata.csv
        ▼
 import_subjects.py
        │  uploads to Zooniverse subject set
        ▼
   Zooniverse
   (volunteers classify)
        │
        ▼
 export_subjectset.py
     │  downloads classifications export for one subject set
        ▼
   exports/  *.csv
     │
     ▼
 analyze_classifications.py
     │  builds multi-sheet Excel summary report
     ▼
   reports/  *.xlsx
     │
     ▼
 zooni_to_toolbox_annot.py
     │  merges volunteer classifications back into Toolbox annotation files
     ▼
   toolbox_import.csv / toolbox_import_annotations.json
```

---

## Setup

Requires **Python 3.10+** 

### 1. Install dependencies

For the scripts:

```bash
pip install -r requirements.txt
```

For the desktop app as well (adds customtkinter, Pillow and ultralytics):

```bash
pip install -r requirements-app.txt
```

`run_KelpQuest.bat` does this for you into its own environment, and installs
the CPU build of PyTorch rather than the multi-gigabyte CUDA one.

### 2. Configure credentials

Copy the template and fill in your Zooniverse details:

```bash
cp scripts/config.example.env scripts/.env
```

Edit `scripts/.env`:

```
ZOONIVERSE_USERNAME=your_username
ZOONIVERSE_PASSWORD=your_password
ZOONIVERSE_PROJECT_ID=24397
```

> **Never commit `.env` to Git.** It is already listed in `.gitignore`.

---

## Scripts

### `toolbox_to_subjects.py` — Extract patches from CoralNet-Toolbox

Reads a `annotations.csv` exported from CoralNet-Toolbox and produces a folder of cropped patch images ready for Zooniverse, plus a `metadata.csv` for the import script.

Each output image is a larger crop centred on the annotation point with three overlays burned in:
- A **green rectangle** marking the original Toolbox patch boundary
- A **red crosshair** at the exact annotation centre point
- The **Toolbox model prediction label** as text

**Arguments**

| Argument | Required | Default | Description |
|---|---|---|---|
| `--annotations-csv` | ✅ | — | Path to `annotation.csv` from CoralNet-Toolbox |
| `--output-dir` | ✅ | — | Folder where patch images will be saved |
| `--metadata-csv` | ✅ | — | Output path for `metadata.csv` |
| `--scale` | | `3.5` | Crop size multiplier relative to Toolbox patch size |
| `--jpeg-quality` | | `100` | JPEG output quality (1–100) |
| `--dry-run` | | off | Validate inputs and count rows without writing files |

**Example**

```bash
# Standard run
python scripts/toolbox_to_subjects.py \
    --annotations-csv  /transects/T1/annotations.csv \
    --output-dir   /transects/T1/patches/ \
    --metadata-csv /transects/T1/patches/metadata.csv

# Larger crops and smaller file size
python scripts/toolbox_to_subjects.py \
    --annotations-csv  /transects/T1/annotations.csv \
    --output-dir   /transects/T1/patches/ \
    --metadata-csv /transects/T1/patches/metadata.csv \
    --scale 4 --jpeg-quality 80

# Check everything looks right before committing
python scripts/toolbox_to_subjects.py \
    --annotations-csv /transects/T1/annotations.csv \
    --output-dir  /transects/T1/patches/ \
    --metadata-csv /transects/T1/patches/metadata.csv \
    --dry-run
```

**Output files**

- `<output-dir>/<source_image>_r<row>_c<col>.jpg` — one image per annotation
- `<metadata-csv>` — columns: `filename`, `source_image`, `row`, `column`, `model_pred_code`, `model_pred_name`
- `toolbox_extract_log.txt` — run log

---

### `import_subjects.py` — Upload patches to Zooniverse

Reads the patch images and `metadata.csv` produced by the previous step and uploads them to a Zooniverse subject set. Keeps a local `upload_log.csv` so re-runs safely skip anything already uploaded.

**Arguments**

| Argument | Required | Default | Description |
|---|---|---|---|
| `--transect-id` | ✅ | — | Internal transect ID, e.g. `EBM_W25_T1` |
| `--image-dir` | ✅ | — | Folder containing patch images and `metadata.csv` |
| `--subject-set-name` | ✅ * | — | Name for a **new** subject set to create |
| `--subject-set-id` | ✅ * | — | ID of an **existing** subject set to add into |
| `--filename-column` | | `filename` | Column in `metadata.csv` that holds the image filename |
| `--upload-log` | | `<image-dir>/upload_log.csv` | CSV recording successfully uploaded subjects |
| `--fail-log` | | `<image-dir>/fail_log.csv` | CSV recording skipped or failed subjects |
| `--checkpoint-every` | | `100` | Flush subjects to the set every N uploads |
| `--sleep` | | `0.1` | Seconds between subject saves (reduces API pressure) |
| `--limit` | | — | Only process the first N rows (useful for testing) |
| `--check-server-duplicates` | | off | Check Zooniverse server for duplicates (slow) |
| `--skip-missing` | | on | Skip rows where the image file is not found |
| `--dry-run` | | off | List images with ✓/✗ MISSING without uploading |

\* Provide exactly one of `--subject-set-name` (creates new) or `--subject-set-id` (adds to existing).

**Example**

```bash
# New subject set
python scripts/import_subjects.py \
    --transect-id EBM_W25_T1 \
    --subject-set-name "2025_01_28_EBM_T1" \
    --image-dir /transects/T1/patches/

# Add to an existing subject set
python scripts/import_subjects.py \
    --transect-id EBM_W25_T1 \
    --subject-set-id 135009 \
    --image-dir /transects/T1/patches/

# Test with first 10 images only
python scripts/import_subjects.py \
    --transect-id  \
    --subject-set-name "Test" \
    --image-dir /transects/T1/patches/ \
    --limit 10 --dry-run
```

**After upload:** the script prints the Subject Set ID — record this in `tracker.xlsx`.

**Output files**

- `<image-dir>/upload_log.csv` — `subject_id`, `filename`, `source_id`, `transect_id`, `subject_set_id`, `uploaded_at`
- `<image-dir>/fail_log.csv` — skipped or failed rows with reason
- `import_log.txt` — run log

**Re-running safely:** if a run is interrupted, just run the same command again. The script reads `upload_log.csv` and skips anything already uploaded.

---

### `export_subjectset.py` — Export classifications for one subject set

Requests a classifications export for a single subject set and saves the CSV to the path you choose.

Current script behavior uses a GUI form for inputs.

**GUI inputs**

- Subject set ID (required)
- Save CSV path (required)

**Example**

```bash
python scripts/export_subjectset.py
```

**Output files**

- CSV at the path selected in the GUI (subject set classifications export)

After export: add a row to the **Export Log** sheet in `tracker.xlsx` and set the transect status to `Complete`.

---

### `analyze_classifications.py` — Build an Excel summary report

Reads a raw or flattened Zooniverse classification export CSV and produces a formatted multi-sheet Excel report (overview, transect completion, workflow summary, subject summary, user summary, answer breakdown, source image summary, and time stats).

Current script behavior uses a GUI form for inputs.

**GUI inputs**

- Export CSV (required)
- Output folder
- Optional transect ID filter — **use this.** The multiple-choice and expert
  subject sets are shared by the whole project, so an export of them carries
  every transect's subjects. Without the filter a report meant for one
  transect summarises all of them.
- Optional workflow ID filter
- Optional source image filter
- Optional date range

**Output files**

- `reports/classification_report_<timestamp>.xlsx` (filename includes selected filters)

Use this as the analysis step after running `export_subjectset.py`.

---

### `zooni_to_toolbox_annot.py` — Merge Zooniverse classifications back into Toolbox

Reads the raw Zooniverse classifications export alongside a CoralNet-Toolbox `annotation.csv` (and optionally the Toolbox annotation JSON) and writes updated annotation files with labels and verification status assigned from the volunteer and expert votes.

Launches a GUI — no command-line arguments required. The GUI remembers the last-used file paths between runs.

**GUI inputs**

| Field | Required | Description |
|---|---|---|
| Toolbox annotation.csv | ✅ | Exported from CoralNet-Toolbox |
| Zooniverse export CSV | ✅ | Full classifications export downloaded from Zooniverse |
| Labelset JSON | ✅ | Toolbox labelset JSON (list of label objects with `short_label_code` / `long_label_code`) |
| Toolbox annotation JSON | | Optional — if provided, an updated annotation JSON is also written |
| Output directory | ✅ | Folder where output files will be saved |
| Workflows to include | ✅ | Checkboxes for each of the four workflows (all on by default) |

**Workflows**

| ID | Name | Type |
|---|---|---|
| 30787 | Yes/No | Crowd confirm/deny |
| 31534 | Yes/No Expert | Expert confirm/deny |
| 30752 | Multi-choice | Crowd species classification |
| 31535 | Multi-choice Expert | Expert species classification |

**Label determination rules**

All points start as `Label = "Review"`, `Verified = FALSE`. Rules are applied in priority order — the first rule that fires wins.

1. **Multi expert consensus** (workflow 31535)
   Threshold: n ≥ 1 classification, top label agreement ≥ 67%
   → Label mapped from labelset. `Verified = TRUE`.
   A single expert classification is sufficient.

2. **Multi crowd consensus** (workflow 30752)
   Threshold: n ≥ 3 classifications, top label agreement ≥ 67%
   → Label mapped from labelset. `Verified = TRUE`.
   Expert result (step 1) takes precedence if both reached consensus.
   Both steps 1–2 override an expert yes/no denial.

   *Label mapping for steps 1–2:*
   - Zooniverse choice text is cleaned: image markdown (`![img](url) Label`) is reduced to the trailing text; if no text follows the image, the alt text is used instead.
   - Cleaned text is looked up in the labelset JSON, then in a built-in expansions table (common alt-text shortcuts → short label codes).
   - Ambiguous or project-specific mappings can be added to `manual_overrides` in `main()`.
   - If the label cannot be mapped → `Review`, `Verified = FALSE`, `zoon_status = multi_consensus_unmapped` (reported in `unmapped_multi_consensus_labels.csv`).
   - If the consensus label is a known "not sure" response (e.g. `"Not sure (needs expert review)"`) → `Review`, `Verified = FALSE`, `zoon_status = voted_review`.

3. **Expert yes/no confirmed** (workflow 31534)
   Threshold: n ≥ 1 classification, ≥ 75% voted Yes
   → Original Toolbox label kept unchanged. `Verified = TRUE`.
   Only applies when no multi consensus exists.

4. **Crowd yes/no confirmed** (workflow 30787)
   Threshold: n ≥ 5 classifications, ≥ 75% voted Yes
   → Original Toolbox label kept unchanged. `Verified = TRUE`.
   Only applies when no multi consensus exists AND expert did not deny.

5. **Review (fallback)**
   Point remains `Label = "Review"`, `Verified = FALSE` if:
   - Expert denied (≥ 75% voted No, n ≥ 1), or
   - Crowd denied (≥ 75% voted No, n ≥ 5), or
   - Insufficient votes in all workflows, or
   - Multi consensus label could not be mapped to the labelset, or
   - Multi consensus label is a known "not sure" response.

**Output files**

| File | Description |
|---|---|
| `toolbox_import.csv` | Updated annotation CSV ready to import into CoralNet-Toolbox |
| `toolbox_import_annotations.json` | Updated annotation JSON (only if input JSON was provided) |
| `qaqc_classifications.csv` | Full QA/QC report with vote counts, agreement, status, and review reasons for every point |
| `unmapped_multi_consensus_labels.csv` | Labels that reached multi consensus but could not be mapped to the labelset — add these to `expansions` or `manual_overrides` in the script |

**Adding new label mappings**

If the unmapped labels report shows labels that should map to a known Toolbox code, add them to the `expansions` dict inside `map_to_toolbox_codes()`. Keys must be the normalized form of the cleaned label text (lowercase, spaces and punctuation replaced by underscores):

```python
expansions = {
    "brown_algae": "BR_sarg",   # "![img](url) Brown algae" → BR_sarg
    "silt":        "SU_silt",   # "![img](url) Silt"        → SU_silt
    # ...
}
```

For labels that should always map to `"Review"` (e.g. volunteer "not sure" options), add them to `manual_overrides` in `main()`:

```python
manual_overrides: dict = {
    "Not sure (needs expert review)": ("Review", "Review"),
}
```

---

## Tracking Spreadsheet (`tracker.xlsx`)

| Sheet | Purpose |
|---|---|
| **Subject Set Tracker** | One row per transect — subject set name & ID, image counts, upload/export dates, script versions, status |
| **Script Version Log** | Every script version with Git commit/tag reference |
| **Export Log** | Every classification export download |
| **Summary Dashboard** | Auto-calculated totals and status breakdown |

### Finding your Subject Set ID

After a subject set is created, the ID appears in the Zooniverse URL:

```
https://www.zooniverse.org/lab/24397/subject-sets/135054
                                                   ^^^^^^
                                             Subject Set ID
```

### Status values

| Status | Meaning |
|---|---|
| `Not Uploaded` | Images exist locally, not yet sent to Zooniverse |
| `Uploaded` | Images uploaded, subject set active |
| `In Progress` | Being classified by volunteers |
| `Awaiting Export` | Classification complete, export not yet downloaded |
| `Complete` | Exported and saved |
| `On Hold` | Paused |

