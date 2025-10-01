from pathlib import Path
import pandas as pd

# Converts column headers to a more manipulable format
def format_header(column_name):
    return column_name.strip().lower().replace(" ", "_")

# Builds short-form rows from Toolbox output file
def reformat_csv(file_path, site, transect):
    df = pd.read_csv(file_path)

    # Call function to clean header names
    df.columns = [format_header(col) for col in df.columns]

    # Build label frequency table per image
    counts = (
        df.groupby(["name", "label"])
        .size()
        .reset_index(name="count")
    )

    pivot = counts.pivot_table(
        index="name", 
        columns="label", 
        values="count", 
        fill_value=0
    )

    # Convert label frequency to proportion
    pivot = pivot.div(pivot.sum(axis=1), axis=0).round(2)

    # Clean header names in new table
    pivot.columns = [format_header(col) for col in pivot.columns]

    # Add metadata
    pivot["site"] = site
    pivot["transect"] = transect

    # Reorder so site/transect/name come first
    cols = ["name", "site", "transect"] + [col for col in pivot.columns if col not in ["site", "transect", "name"]]
    pivot = pivot.reset_index()[cols]

    return pivot

def main():
    # Define relative paths
    cwd = Path.cwd()
    root = cwd.parent

    data = root / "data"
    results = root / "results"
    
    # Takes multiple inputs from an input folder
    folder_name = input("Enter name to folder containing CSV files: ").strip()
    folder_path = data / folder_name
    if not folder_path.is_dir():
        print(f"Error: Folder '{folder_path}' not found.")
        return

    all_dataframes = []

    for filename in folder_path.iterdir():
        if filename.suffix.lower() == ".csv":

            site = input("Enter site name for this dataset: ").strip()
            transect = input("Enter transect ID for this dataset: ").strip()

            df = reformat_csv(filename, site, transect)
            all_dataframes.append(df)

    if all_dataframes:
        save_path = results / f"{folder_path.name}-cleaned.csv"
        combined_df = pd.concat(all_dataframes, ignore_index=True)
        combined_df.to_csv(save_path, index=False)
        print(f"\nCombined CSV saved to: {save_path}")
    else:
        print("No CSV files found in the given folder.")

if __name__ == "__main__":
    main()
