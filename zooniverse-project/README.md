# 🔭 Zooniverse Project Tracker

This repository organizes everything related to our Zooniverse citizen science project — tracking transect uploads, subject sets, classification exports, and the scripts used for each.

---

## 📁 Repository Structure

```
zooniverse-project/
├── README.md                    ← You are here
├── tracker.xlsx                 ← Master tracking spreadsheet (see below)
├── scripts/
│   ├── import_subjects.py       ← Upload images to Zooniverse subject sets
│   ├── export_classifications.py← Download & flatten classification exports
│   └── config.example.env       ← Environment variable template (copy → .env)
├── exports/                     ← Downloaded classification CSVs (git-ignored)
│   └── .gitkeep
└── docs/
    └── workflow.md              ← Step-by-step upload/export workflow guide
```

---

## 📊 Tracking Spreadsheet (`tracker.xlsx`)

The spreadsheet has four sheets:

| Sheet | Purpose |
|---|---|
| **Subject Set Tracker** | One row per transect — links transect ID → subject set name → subject set ID, image count, upload date, script version used |
| **Script Version Log** | Records every version of import & export scripts with Git commit/tag references |
| **Export Log** | Logs every classification export download (date, workflow, # classifications, file location) |
| **Summary Dashboard** | Auto-calculated KPIs — total transects, images uploaded, status breakdown |

### Subject Set Tracker columns

| Column | Description |
|---|---|
| Transect ID | Your internal transect identifier (e.g. `T-001`) |
| Site / Survey Area | Human-readable location name |
| Date Surveyed | When the transect was conducted |
| Subject Set Name | Name used in Zooniverse |
| Subject Set ID | Numeric ID from Zooniverse (visible in the URL) |
| # Images Uploaded | Count of image files sent to Zooniverse |
| # Subjects Created | Count confirmed in Zooniverse after upload |
| Upload Date | When the upload script was run |
| Upload Script Ver. | Version of `import_subjects.py` used (e.g. `v1.3`) |
| Export Date | When classifications were exported |
| Export Script Ver. | Version of `export_classifications.py` used |
| Export File Name | Filename of the saved export CSV |
| Status | Dropdown: Not Uploaded / Uploaded / In Progress / Awaiting Export / Complete / On Hold |
| Notes | Any relevant notes |

---

## 🚀 Workflow

### 1. Upload a new transect

```bash
# Copy and fill in your credentials
cp scripts/config.example.env scripts/.env

# Run the import script
python scripts/import_subjects.py \
  --transect-id T-006 \
  --subject-set-name "WestLagoon_Jan2026" \
  --image-dir /path/to/images/T-006/
```

Then update `tracker.xlsx`:
- Fill in the Subject Set ID (from Zooniverse URL after upload)
- Record the image count, upload date, and script version used
- Set Status → `Uploaded` or `In Progress`

### 2. Export classifications

```bash
python scripts/export_classifications.py \
  --workflow-id 9876 \
  --subject-set-id 12349 \
  --output-dir exports/
```

Then update `tracker.xlsx`:
- Fill in export date, script version, and export file name
- Add a row to the **Export Log** sheet
- Set Status → `Complete`

---

## 🔑 Credentials / API Keys

**Never commit credentials to this repo.**

Use the `scripts/.env` file (which is git-ignored). See `scripts/config.example.env` for the required variables:

```
ZOONIVERSE_USERNAME=your_username
ZOONIVERSE_PASSWORD=your_password
ZOONIVERSE_PROJECT_ID=12345
```

---

## 📌 Finding Your Subject Set ID

After creating or uploading to a subject set in Zooniverse, the ID appears in the URL:

```
https://www.zooniverse.org/lab/12345/subject-sets/67890
                                                   ^^^^^
                                              Subject Set ID
```

Record this in the **Subject Set ID** column of the tracker.

---

## 🤝 Contributing

1. Pull latest before making changes: `git pull`
2. Update `tracker.xlsx` after every upload or export
3. When releasing a new script version, add a row to the **Script Version Log** sheet and tag the commit: `git tag import-v1.4`
4. Push changes: `git add tracker.xlsx && git commit -m "Update tracker: T-006 uploaded" && git push`

---

## 📋 Status Key

| Status | Meaning |
|---|---|
| 🔴 Not Uploaded | Images exist locally, not yet sent to Zooniverse |
| 🟡 Uploaded | Images uploaded, subject set active, awaiting volunteers |
| 🔵 In Progress | Being classified by volunteers |
| 🟠 Awaiting Export | Classification complete, export not yet downloaded |
| 🟢 Complete | Exported and saved locally |
| ⚪ On Hold | Paused for any reason |
