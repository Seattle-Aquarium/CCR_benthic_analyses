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

#### Augmentation

Augmentation is how a model sees more than it was given: every epoch, each
training image is shown slightly altered, so the model learns the class rather
than the file. Stage 3 offers it as four presets, chosen from one dropdown.

**The constraint that shapes all of them.** A patch is cut *centred on its
annotated point*, and the label describes what is at that centre. An
augmentation that is free to move, crop or erase the middle of the frame can
therefore change what the image shows without changing its label — and the
model is then taught, with full confidence, that a patch of bare sand is
`KE_sieve`. Ultralytics' stock classification recipe does exactly this, three
ways:

| stock setting | what it does | why it is off here |
|---|---|---|
| `scale=0.5` — random-resized crop | crops a random 50–100% region and resizes it | the crop need not contain the centre; the annotated organism can leave the frame entirely |
| `erasing=0.4` — random erasing (cutout) | blanks a random rectangle, 40% of the time | the rectangle can land on the annotated point, leaving a label with nothing under it |
| `auto_augment=randaugment` | a random policy of geometric and colour ops | includes translate, shear and cutout — all of the above, chosen at random |

All three are set to off in every preset except the one that exists to
reproduce stock behaviour. The pipeline also enables **vertical flip**, which
stock leaves off: the seafloor has no canonical "up", so a patch flipped
top-to-bottom is as valid a training example as the original.

**What Ultralytics itself can vary** is colour and flips. Colour is genuinely
useful underwater, where hue and saturation shift hard with depth and
turbidity; `hsv_v` drives both brightness and contrast. Beyond augmentation
proper, two regularisers: **dropout** on the classifier head, and **weight
decay** in the optimiser. Both make the model work harder to memorise.

**What is added from outside.** Ultralytics builds its classification
transforms from torchvision, and that path reads only `hsv_*`, `fliplr`,
`flipud`, `scale`, `erasing` and `auto_augment`. Its geometric arguments —
`degrees`, `translate`, `shear` — and mixing ones — `mixup`, `cutmix` — belong
to the detection augmenter and are **silently ignored** for classification:
they can be passed, they appear in `args.yaml`, and nothing happens. (An early
version of `stronger` set `degrees=180` and `mixup=0.15` believing otherwise;
its gain came from the colour jitter and the regularisers.) So the pipeline
adds three transforms of its own, spliced into Ultralytics' chain by a trainer
subclass (`classifier/augment.py`). All three keep the annotated point exactly
where it is:

| transform | what it does | why it is safe, and useful |
|---|---|---|
| **rot90** | rotates by 0°, 90°, 180° or 270°, chosen uniformly | a pixel permutation — no interpolation, no padded corners. With the two flips it gives all eight orientations a seafloor patch can legitimately take; the seafloor has no "up" and the model should not learn one |
| **center_zoom** | half the time, zooms in on the centre by up to a set fraction and resizes back | ROV altitude varies, so the same taxon is seen at slightly different scales. This is what random-resized crop would be if it were pinned to the centre: nothing enters from outside the frame, and the point stays put |
| **blur** | some of the time, a mild Gaussian blur (σ up to 1.5 px at training size) | turbidity and focus vary shot to shot; a model that needs every edge crisp is memorising the good frames |

These never appear in `args.yaml`, because there is no argument for them.
`run_notes.md` in each run folder records them, and the training log prints
what was added.

| preset | colour jitter (h / s / v) | flips | rot90 | zoom | blur | dropout | weight decay | use it when |
|---|---|---|---|---|---|---|---|---|
| **standard** | 0.015 / 0.7 / 0.4 | both | yes | — | — | 0 | 0.0005 | the default |
| **stronger** | 0.03 / 0.8 / 0.5 | both | yes | to 1.15× | 25% | 0.2 | 0.001 | a run overfits — validation loss climbing while training loss falls, best epoch early |
| **lighter** | 0.01 / 0.4 / 0.3 | both | yes | — | — | 0 | 0.0005 | a run underfits — validation loss *below* training loss, or still improving at the last epoch |
| **ultralytics defaults** | stock | horizontal | — | — | — | 0 | 0.0005 | never for a real model; here to measure the pipeline's choices against stock |

Crop, erasing and RandAugment are off in the first three and stock in the
fourth. Rotation is on for every real preset because it costs nothing and
teaches orientation invariance; zoom and blur are the strength knobs. The
panel shows the exact values under whichever preset is selected.

Runs from before rotation was added — `2026_09_09_HSIL_EBpt3_strong_aug` and
earlier — used the same colour, flip and regulariser settings without the
three extras. Stage 5 infers the preset of such runs from their `args.yaml`,
which cannot see the extras, so the history labels them by the Ultralytics
values and their `run_notes.md`, where present, says what else applied.

**Anything else** takes `KEY=VALUE` lines for arguments the presets do not
cover, applied after the preset. Names are checked against Ultralytics' own
argument list before the run starts — a typo would otherwise cost the dataset
scan before failing — but a *valid* argument the classification pipeline
ignores (see above) passes the check and does nothing. `label_smoothing` is not
an argument this Ultralytics version accepts at all.

Not to be confused with stage 2's **offline** augmentation, which writes
flipped, rotated and brightness-shifted copies to disk to lift undersized
classes to the floor. Those are ordinary training files by the time stage 3
sees them; the presets here act on every image, every epoch, in memory.

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
* `*_untrained_classes.csv` — held-out images whose class the model was never
  trained on. Set aside from every accuracy figure, since the model cannot be
  right about a class it has not seen and its guesses would otherwise land as
  false positives against classes it does know. Reported on their own terms:
  what did the model make of them?

### 5. Compare models

Reads finished runs and says which to keep. Point it at two or more run
folders, or set a **models root** and every run beneath it is compared
automatically (an `archive/` folder is skipped).

Ranked on **macro F1** measured on the held-out set — see *Why internal
accuracy is not the number*. Models are only ranked against each other when
they were evaluated on the **same held-out images**; where the sets differ,
the comparison falls back to the images they share and says so, and where the
overlap is too small it withholds the ranking rather than compare numbers
that do not mean the same thing.

A window opens with the result, and the same page is written to disk:

* **`model_comparison_summary.md`** — the tear sheet as a document: the
  recommendation and why, every model side by side, what to try next, where
  the models disagree by class, the work queue, and both figures. This is the
  one to paste into a report.
* `model_comparison.xlsx` — all the detail behind it. The per-class sheet
  carries, for every class: `reliability` (how far to trust the row, given
  how many held-out examples decide it), `problem` (good / missed /
  over-called / weak both ways / never right / unmeasured), `diagnosis` and
  `suggestion` in plain sentences, and `missed_as` / `absorbs` naming the
  class it is confused with in each direction.
* `model_comparison_per_class.png`, `model_comparison_curves.png`

Two files live at the models root rather than with any one comparison:

* **`model_history.md`** — regenerated from the run folders every time, so it
  cannot drift from them. Every run oldest first, with what it was trying (from
  `run_notes.md`), how it trained, and how it scored. The current choice is
  called out at the top.
* **`decisions.md`** — appended, never rewritten: one dated entry per
  comparison with the recommendation and its reasons.

Stage 3's **Notes** box is what makes the history worth reading. It asks what
changed in this run and why, at the one moment the answer is known, and writes
it to `run_notes.md` beside the weights along with the settings actually used.
Runs from before it exists say so rather than guessing.

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
        augment.py          centre-preserving transforms Ultralytics lacks
        brand.py            Seattle Aquarium palette and type
        config.py           stage settings, augmentation presets, and the
                            state that carries between stages
        fsutil.py           paths, dataset listing, safe filenames
        hashing.py          byte-identical duplicate and leak detection
        imaging.py          cropping and offline augmentation
        logging_setup.py    one UTF-8 logging config; also the stand-in for a
                            missing sys.stdout under pythonw
        progress.py         progress reporting and cancellation
        provenance.py       which survey photo a patch was cut from
        stages/             the five stages, GUI-free
        gui/                the app, including tearsheet.py (the comparison
                            summary window)
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
