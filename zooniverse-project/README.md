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
│   ├── export_classifications.py       ← Step 3: download volunteer classifications
│   └── config.example.env             ← credentials template
└── exports/                            ← downloaded classification CSVs (git-ignored)
```

---

## Full Workflow

```
CoralNet-Toolbox
    dataset.csv
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
 export_classifications.py
        │  downloads & flattens results
        ▼
   exports/  *.csv
```

---

## Setup

### 1. Install dependencies

```bash
pip install panoptes-client python-dotenv pandas tqdm opencv-python-headless
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

Reads a `dataset.csv` exported from CoralNet-Toolbox and produces a folder of cropped patch images ready for Zooniverse, plus a `metadata.csv` for the import script.

Each output image is a larger crop centred on the annotation point with three overlays burned in:
- A **green rectangle** marking the original Toolbox patch boundary
- A **red crosshair** at the exact annotation centre point
- The **Toolbox model prediction label** as text

**Arguments**

| Argument | Required | Default | Description |
|---|---|---|---|
| `--dataset-csv` | ✅ | — | Path to `dataset.csv` from CoralNet-Toolbox |
| `--output-dir` | ✅ | — | Folder where patch images will be saved |
| `--metadata-csv` | ✅ | — | Output path for `metadata.csv` |
| `--scale` | | `3.5` | Crop size multiplier relative to Toolbox patch size |
| `--jpeg-quality` | | `100` | JPEG output quality (1–100) |
| `--dry-run` | | off | Validate inputs and count rows without writing files |

**Example**

```bash
# Standard run
python scripts/toolbox_to_subjects.py \
    --dataset-csv  /data/T-006/dataset.csv \
    --output-dir   /data/T-006/patches/ \
    --metadata-csv /data/T-006/patches/metadata.csv

# Larger crops and smaller file size
python scripts/toolbox_to_subjects.py \
    --dataset-csv  /data/T-006/dataset.csv \
    --output-dir   /data/T-006/patches/ \
    --metadata-csv /data/T-006/patches/metadata.csv \
    --scale 3.5 --jpeg-quality 80

# Check everything looks right before committing
python scripts/toolbox_to_subjects.py \
    --dataset-csv /data/T-006/dataset.csv \
    --output-dir  /data/T-006/patches/ \
    --metadata-csv /data/T-006/patches/metadata.csv \
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
| `--transect-id` | ✅ | — | Internal transect ID, e.g. `T-006` |
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

### `export_classifications.py` — Download volunteer classifications

Downloads the classification export for a workflow and saves a flattened CSV.

**Arguments**

| Argument | Required | Default | Description |
|---|---|---|---|
| `--workflow-id` | ✅ | — | Zooniverse workflow ID |
| `--subject-set-id` | | — | Filter export to a specific subject set |
| `--output-dir` | | `exports/` | Folder to save the output CSV |
| `--generate-new` | | off | Request a fresh export from Zooniverse (can take several minutes) |

**Example**

```bash
# Download the most recent export for a workflow
python scripts/export_classifications.py \
    --workflow-id 9876 \
    --output-dir  exports/

# Filter to one subject set and request a fresh export
python scripts/export_classifications.py \
    --workflow-id    9876 \
    --subject-set-id 135054 \
    --output-dir     exports/ \
    --generate-new
```

**Output files**

- `exports/workflow<id>_ss<id>_<YYYYMMDD>_classifications.csv` — flattened classifications
- `export_log.txt` — run log

> **Customise the flattening:** `flatten_annotations()` in the script extracts task answers into columns. Edit it to match your workflow's specific question/task structure.

After export: add a row to the **Export Log** sheet in `tracker.xlsx` and set the transect status to `Complete`.

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

---

## Git Tips

Tag script releases so the version column in `tracker.xlsx` links to real code:

```bash
git tag toolbox-v1.0
git tag import-v2.0
git tag export-v1.0
git push --tags
```

Update the tracker after every upload or export, then commit:

```bash
git add tracker.xlsx
git commit -m "Tracker: T-006 uploaded, subject set 135054"
git push
```

Export CSVs are git-ignored (large, re-downloadable from Zooniverse). Only `tracker.xlsx` and scripts are tracked.
