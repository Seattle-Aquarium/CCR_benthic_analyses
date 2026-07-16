# Zooniverse Kelp Quest Project

This repository organizes everything for our Zooniverse citizen science project — from extracting annotated patches out of CoralNet-Toolbox through to uploading them to Zooniverse and downloading the volunteer classifications.

---

## Repository Structure

```
zooniverse-project/
├── README.md
├── tracker.xlsx                        ← master tracking spreadsheet
├── scripts/
│   ├── toolbox_to_subjects.py          ← Step 1: extract patches from Toolbox annotations
│   ├── import_subjects.py              ← Step 2: upload patches to Zooniverse
│   ├── export_subjectset.py            ← Step 3: export classifications for one subject set
│   ├── analyse_classifications.py      ← Step 4: generate Excel summary report
│   ├── zooni_to_toolbox_annot.py       ← Step 5: merge Zooniverse results back into Toolbox
│   └── config.example.env             ← credentials template
└── exports/                            ← downloaded classification CSVs (git-ignored)
```

---

## Full Workflow

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
 analyse_classifications.py
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

All Python dependencies for every script in `scripts/` are pinned in `requirements.txt`:

```bash
pip install -r requirements.txt
```


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

### `analyse_classifications.py` — Build an Excel summary report

Reads a raw or flattened Zooniverse classification export CSV and produces a formatted multi-sheet Excel report (overview, workflow summary, subject summary, user summary, answer breakdown, source image summary, and time stats).

Current script behavior uses a GUI form for inputs.

**GUI inputs**

- Export CSV (required)
- Output folder
- Optional workflow ID filter
- Optional source image filter

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

