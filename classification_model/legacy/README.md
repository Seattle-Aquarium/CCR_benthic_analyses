# Superseded scripts

These are the seven standalone tkinter scripts the `classifier` package replaced.
They are kept for reference only — **they are not maintained, and running them
will not produce the same result as the pipeline.**

| Old script | Replaced by |
|---|---|
| `extract_training_patches.py` | stage 1 — `classifier/stages/extract.py` |
| `merge_and_balance_dataset.py` | stage 2 — `classifier/stages/balance.py` |
| `train_classification_model.py` | stage 3 — `classifier/stages/train.py` |
| `evaluate_holdout.py` | stage 4 — `classifier/stages/evaluate.py` |
| `find_duplicate_patches.py` | `classifier/hashing.py`, now run automatically inside stages 2 and 4 |
| `remove_duplicate_images.py` | `classifier/hashing.py` (`python -m classifier.cli audit`) |
| `rebalance_train_val_split.py` | stage 2's cap/floor balancing |

## Why they were replaced

Beyond the duplication — config load/save was copy-pasted six times, logging
setup seven times, `augment_image` twice, `list_class_images` three times — three
behaviours differ in ways that mattered:

**Duplicate detection compared filenames, not content.** `merge_and_balance_dataset.py`
indexed patches by `os.path.basename`, so the same annotation point extracted in
two different batches produced two identical files under two different names and
sailed through. That is how 1,929 of 15,380 held-out patches (12.5%) came to be
byte-identical to training data, inflating a reported 65.85% that was really
62.81%. Stage 2 hashes contents, and stage 4 refuses to report a number until the
held-out set is verified disjoint.

**Early stopping never fired.** `train_classification_model.py` left `patience`
blank, so Ultralytics' default of 100 applied — and with `epochs=100` that can
never trigger. Every run trained to the last epoch regardless of when it stopped
improving. Stage 3 defaults `patience=20` and reports the best epoch against the
epochs actually run.

**Source images were decoded once per annotation point.** `extract()` called
`cv2.imread()` inside the per-row loop, so an image carrying 100 points was
decoded 100 times. Stage 1 groups by source image first.

Two of these scripts (`remove_duplicate_images.py`, `rebalance_train_val_split.py`)
were never part of the documented workflow and appear to have been one-off tools.

## If you need something from here

Prefer porting it into the package over running these directly. The pipeline
state file (`.classifier_config.json`) is not compatible with the old
per-script `.{script_name}_config.json` files.
