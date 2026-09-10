"""
Convert a point-annotation CSV (one row per annotated point) into a
percent-cover table (one row per image, one column per label).

Usage:
    python annotations_to_percent_cover.py input.csv output.csv

Logic:
    - Groups rows by 'Name' (the image).
    - For each image, counts how many points fall under each 'Label'.
    - Divides those counts by the TOTAL number of annotated points for
      that specific image (not a hardcoded 50), so results are correct
      even if an image has a different number of points.
    - Output columns are the label short codes found in the 'Label'
      column. Values are proportions from 0 to 1, and each row sums to 1
      (barring floating point rounding).
"""

import sys
import pandas as pd


def annotations_to_percent_cover(input_csv: str, output_csv: str) -> pd.DataFrame:
    df = pd.read_csv(input_csv)

    required_cols = {"Name", "Label"}
    missing = required_cols - set(df.columns)
    if missing:
        raise ValueError(f"Input CSV is missing expected column(s): {missing}")

    # Count points per (Name, Label)
    counts = df.groupby(["Name", "Label"]).size().unstack(fill_value=0)

    # Divide by each image's actual total point count (robust to images
    # that don't have exactly 50 points)
    totals = df.groupby("Name").size()
    percent_cover = counts.div(totals, axis=0)

    percent_cover = percent_cover.reset_index()
    percent_cover.to_csv(output_csv, index=False)
    return percent_cover


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python annotations_to_percent_cover.py input.csv output.csv")
        sys.exit(1)
    result = annotations_to_percent_cover(sys.argv[1], sys.argv[2])
    print(f"Wrote {len(result)} rows and {len(result.columns) - 1} label columns to {sys.argv[2]}")