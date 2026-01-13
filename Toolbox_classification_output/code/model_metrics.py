# Takes original csv file located in *data* folder as input -- does not operate with nested or "-cleaned.csv" files

from pathlib import Path
import pandas as pd
import numpy as np

def calculate_accuracy(input_csv, output_csv):
    # Load CSV
    df = pd.read_csv(input_csv)

    # Standardize column names
    df.columns = [col.lower().replace(" ", "_") for col in df.columns]

    # Check for match (True/False)
    df["is_correct"] = df["label"] == df["machine_suggestion_1"]

    # Model Accuracy
    output = (
        df.groupby("label")
          .agg(
              number_accurate=("is_correct", "sum"),
              number_incorrect=("is_correct", lambda x: (~x).sum())
          )
          .reset_index()
    )

    output["total_annotations"] = output["number_accurate"] + output["number_incorrect"]
    output["percent_accuracy"] = (
        output["number_accurate"] / output["total_annotations"] * 100
    ).round(2)

    # Model Confidence
    def mean_or_nan(x):
        return np.nan if len(x) == 0 else np.mean(x)

    def se_or_nan(x):
        return np.nan if len(x) == 0 else (np.std(x, ddof=1) / np.sqrt(len(x)))

    conf_stats = (
        df.groupby(["label", "is_correct"])["machine_confidence_1"]
          .agg(mean_conf=mean_or_nan, se_conf=se_or_nan)
          .reset_index()
    ).round(2)

    # Pivot so correct/incorrect split into columns
    conf_pivot = conf_stats.pivot(index="label", columns="is_correct")
    conf_pivot.columns = [
        "avg_confidence_incorrect", "avg_confidence_correct",
        "std_error_incorrect", "std_error_correct"
    ]
    conf_pivot = conf_pivot.reset_index()

    # Merge back with output
    output = output.merge(conf_pivot, on="label", how="left")

    # Reorder columns
    col_order = [
        "label",
        "total_annotations",
        "number_accurate",
        "number_incorrect",
        "percent_accuracy",
        "avg_confidence_correct",
        "std_error_correct",
        "avg_confidence_incorrect",
        "std_error_incorrect"
    ]
    output = output[col_order]

    # Save
    output.to_csv(output_csv, na_rep='NULL', index=False)
    print(f"model_metrics_report.csv saved to {output_csv}")

if __name__ == "__main__":
    # Define relative paths
    cwd = Path.cwd()
    root = cwd.parent

    data = root / "data"
    results = root / "results"
    
    # Prompt for CSV file
    csv_name = (input("Enter name of CSV file: ").strip())
    csv_path = data / csv_name
    if not csv_path.is_file():
        print(f"Error: File '{csv_path}' not found.")
    else:
        # Output in the same folder as input CSV, named after parent folder
        output_path = results / f"{csv_name.split('.')[0]}-metrics_report.csv"
        calculate_accuracy(csv_path, output_path)
