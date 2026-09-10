"""CCR Benthic Classifier — the retraining pipeline, end to end.

Four stages, run in order, each feeding the next:

    1. extract   annotation CSV(s)        -> patch dataset (train/val, or flat holdout)
    2. balance   patch dataset(s)         -> one merged, deduplicated, balanced dataset
    3. train     merged dataset           -> a YOLO classification model
    4. evaluate  model + holdout patches  -> accuracy report + confusion matrix

``python -m classifier`` opens the app; ``python -m classifier.cli`` is headless.
"""

__version__ = "1.0"
