"""
Join a percent-cover table (one row per image) with dive telemetry
(one row per second) using the timestamp encoded in the image filename.

Usage:
    python join_percent_cover_telemetry.py percent_cover.csv telemetry.csv output.csv

Logic:
    - Percent-cover 'Name' looks like '2024_10_08_10-07-41.jpg':
      YYYY_MM_DD_HH-MM-SS. That's parsed into a timestamp.
    - Telemetry has separate 'Date' (e.g. 10/8/2024) and 'Time'
      (e.g. 10:07:41) columns, parsed into the same timestamp type.
    - Rows are joined on matching timestamp (left join, keeping every
      percent-cover row). Any image whose timestamp has no telemetry
      match keeps NaN telemetry columns and is reported to stderr.
"""

import sys
import re
import pandas as pd

NAME_TIMESTAMP_RE = re.compile(
    r"(\d{4})_(\d{2})_(\d{2})_(\d{2})-(\d{2})-(\d{2})"
)


def extract_timestamp(name: str) -> pd.Timestamp:
    match = NAME_TIMESTAMP_RE.search(name)
    if not match:
        return pd.NaT
    year, month, day, hour, minute, second = match.groups()
    try:
        return pd.Timestamp(
            year=int(year), month=int(month), day=int(day),
            hour=int(hour), minute=int(minute), second=int(second),
        )
    except ValueError:
        return pd.NaT


def join_percent_cover_with_telemetry(
    percent_cover_csv: str, telemetry_csv: str, output_csv: str
) -> pd.DataFrame:
    percent_cover = pd.read_csv(percent_cover_csv)
    telemetry = pd.read_csv(telemetry_csv)

    if "Name" not in percent_cover.columns:
        raise ValueError("Percent-cover CSV is missing expected column: Name")
    required_telemetry_cols = {"Date", "Time"}
    missing = required_telemetry_cols - set(telemetry.columns)
    if missing:
        raise ValueError(f"Telemetry CSV is missing expected column(s): {missing}")

    percent_cover["timestamp"] = percent_cover["Name"].apply(extract_timestamp)
    telemetry["timestamp"] = pd.to_datetime(
        telemetry["Date"] + " " + telemetry["Time"]
    )

    unparsed = percent_cover["timestamp"].isna().sum()
    if unparsed:
        print(
            f"Warning: {unparsed} image name(s) did not match the expected "
            "timestamp pattern and will have no telemetry data.",
            file=sys.stderr,
        )

    telemetry = telemetry.drop_duplicates(subset="timestamp")
    merged = percent_cover.merge(telemetry, on="timestamp", how="left")

    unmatched = merged["Date"].isna().sum()
    if unmatched:
        print(
            f"Warning: {unmatched} image(s) had no matching telemetry row.",
            file=sys.stderr,
        )

    merged = merged.drop(columns="timestamp")
    merged.to_csv(output_csv, index=False)
    return merged


if __name__ == "__main__":
    if len(sys.argv) != 4:
        print(
            "Usage: python join_percent_cover_telemetry.py "
            "percent_cover.csv telemetry.csv output.csv"
        )
        sys.exit(1)
    result = join_percent_cover_with_telemetry(sys.argv[1], sys.argv[2], sys.argv[3])
    print(f"Wrote {len(result)} rows to {sys.argv[3]}")
