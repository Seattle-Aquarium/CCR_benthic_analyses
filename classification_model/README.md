# CCR Benthic Classifier

Turns Zooniverse-verified point annotations into a retrained benthic
classification model, and then measures that model against imagery it has never
seen.

Four stages, each feeding the next:

```
annotation CSVs ──▶ 1. extract ──▶ patch dataset ──┐
                                                    ├─▶ 2. balance ──▶ merged dataset
                    (held-out transects) ───────────┘                        │
                                                                             ▼
        accuracy report ◀── 4. evaluate ◀── best.pt ◀── 3. train ────────────┘
                                  ▲
                                  └── the held-out set, verified unseen
```

Finishing a stage fills in the next one's inputs and moves you there, so paths
are never retyped and a dataset cannot be handed to the wrong stage by accident.

---

## Running it

Double-click **`run_classifier.bat`**, or from a terminal with `rov_env` active:

```
python -m classifier
```

First time only:

```
python -m pip install customtkinter
```

Everything else — ultralytics, torch, opencv, pandas, matplotlib — is already in
`rov_env`. `requirements.txt` lists the full set for a fresh machine.

Every stage also runs headless. Same settings file, same behaviour:

```
python -m classifier.cli train --data merged_dataset --commit
```

`python -m classifier.cli --help` lists the subcommands. **Every stage previews
by default; `--commit` is what makes it write.**

---

## The workflow

### 1. Extract patches

Point at one or more CoralNet-Toolbox annotation CSVs (the output of
`zooni_to_toolbox_annot.py`). Every row with `Verified == True` becomes one
`Patch Size` × `Patch Size` crop centred on its `(Row, Column)`.

Two settings decide what the output *is*:

* **leave "Held-out set" unticked** → `train/` and `val/` subfolders. This is
  training data, and it feeds stage 2.
* **tick "Held-out set"** → flat `<Label>/` folders, no split at all. This is a
  held-out evaluation set, and it feeds stage 4. It must never be trained on.

The `Path` column in an annotation CSV points at whoever exported it
(`C:\Users\someone-else\...`). The username is corrected automatically. For
folders that have been *renamed* since export, add a remap: `OLD=>NEW`, one per
line.

On a real run this also writes `annotations_updated_paths.csv` next to the
patches, with every path resolved — so a later run over the same data needs no
remaps at all.

### 2. Merge & balance

Combines patch datasets into one training set. Three things happen, in order:

**Deduplicate by content.** Every patch is hashed. Byte-identical copies are
collapsed to one. Anything appearing in both `train/` and `val/` is resolved in
favour of `train`, because a val image the model trained on measures nothing.

**Audit the held-out set.** Point the "Held-out folder" field at your evaluation
set. Any training patch identical to a held-out one is a leak, and it is the
*held-out* copy that gets moved aside — into `<held-out>/_quarantined_leaked/`,
moved rather than deleted. Dropping the training copy instead would shrink the
training set in a way that flatters the evaluation.

**Balance.** Classes above **Cap** are randomly undersampled; classes below
**Floor** are topped up with augmented copies (flip, ±15° rotation, 0.8–1.2×
zoom, hue/saturation and brightness/contrast jitter — never an exact duplicate).
Use **per-class cap overrides** (`KE_sieve=4000`) where the global cap throws
away information that matters.

`val/` is merged exactly as it is — never capped, never augmented — so each
round's validation number stays comparable with the last.

Run an **inventory** first. It prints the combined per-class counts plus the min,
median and max train class size, which is what Cap and Floor should be chosen
from.

### 3. Train

A thin wrapper around Ultralytics. A field left blank is omitted from the call,
so the library's own default applies rather than this app inventing one.

**Patience** is the setting worth understanding. Ultralytics defaults it to 100,
which with 100 epochs can never fire — so every previous run trained to the last
epoch whether or not it had stopped improving hours earlier. The default here is
20. `best.pt` is the best epoch either way; patience only decides how long the
run continues past it. Afterwards the app reports the best epoch against the
epochs actually run, which is what overfitting looks like from the outside.

**"Set aside classes missing from either split"** should stay ticked. Ultralytics
requires identical class folders in `train/` and `val/` and does not enforce it:
given a mismatch it trains to chance accuracy without ever raising. A real run
was lost to this — val loss climbing every epoch, top-1 stuck at 2–3%, exactly
1/n for the class count. Mismatched folders are moved aside for the run and
restored afterwards.

Online augmentation is retuned for these patches: no cutout, no random
off-centre crop, no RandAugment, and vertical flip enabled. All four follow from
patches being centred on their annotated point — the label describes the centre
pixel, so an augmentation that can move or erase the centre is destroying the
label.

On CUDA out-of-memory, **reduce Image size before Batch.** On this hardware the
allocation tracked `imgsz` far more closely than batch size or precision;
256 → 128 fixed it where FP16 and smaller batches did not. `Device: cpu` always
works, slowly.

### 4. Evaluate on held-out data

Scores `best.pt` against transects it has never seen, using the same columns as
the original field-verified accuracy report so the two are directly comparable.

**Independence is verified, not assumed.** Before reporting anything, the
held-out patches are hashed against the training set; if any are byte-identical,
the stage reports the overlap and refuses to produce a number. Leave "Verify the
held-out set is unseen" ticked and fill in the training dataset path.

Outputs, prefixed with the label you give:

* `*_accuracy_report.csv` — per class: totals, accuracy, mean confidence when
  right and when wrong
* `*_confusion_matrix.png` — rows are held-out classes, columns the full model
  taxonomy, normalised per row
* `*_predictions_detail.csv` — one row per image, for auditing individual
  misclassifications

---

## Why internal accuracy is not the number

The previous model reported **97.86%** on its own test split and **47.38%**
against field-verified annotations. The split was not the problem; the
provenance was. Train, val and test all came out of the same curated pipeline,
so the test set measured how well the model had learned that pipeline.

Stage 4 exists because of that gap, and the independence check exists because
skipping it once already cost an evaluation: 12.5% of the held-out set had
leaked, and a reported 65.85% was really 62.81%.

Treat stage 3's validation accuracy as a training diagnostic. Stage 4's number
is the one to report.

---

## Layout

```
classification_model/
    classifier/
        brand.py            Seattle Aquarium palette and type
        config.py           stage settings + the state that carries between them
        fsutil.py           paths, dataset listing, safe filenames
        hashing.py          byte-identical duplicate and leak detection
        imaging.py          cropping and augmentation
        logging_setup.py    one UTF-8 logging config
        progress.py         progress reporting and cancellation
        stages/             the four stages, GUI-free
        gui/                the app
        cli.py              headless entry point
    assets/                 logo and fonts
    legacy/                 the seven scripts this replaced (see legacy/README.md)
    run_classifier.bat      double-click launcher
```

Settings persist to `.classifier_config.json` next to the package. It holds local
paths, so it is git-ignored.

## Known limitations

**Duplicate detection is byte-exact.** It does not find near-duplicates, and that
is correct in both directions: augmented copies are deliberately not identical
and must not be flagged, and two annotation points a few pixels apart are
genuinely different samples even though they look alike.

**Classes with no imagery are excluded from the model entirely** — `BR_acid`,
`KE_ribbon`, `RE_encrust`, `SG`. A deliberate decision; revisit once real field
imagery exists.

**Known weak classes**, reproducible across independent evaluation rounds:
`SU_bould` (~43%, a diffuse confusion magnet), `KE_sieve` (~21%, possibly an
over-aggressive cap — try a per-class override), `RE_filam` (~52%).
