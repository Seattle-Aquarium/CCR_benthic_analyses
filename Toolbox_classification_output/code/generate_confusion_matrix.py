# Takes original Toolbox dataset csv file located in 'data' as input

import pandas as pd
import matplotlib.pyplot as plt
from sklearn.metrics import confusion_matrix, ConfusionMatrixDisplay
from pathlib import Path

def main():
    # Define relative paths
    cwd = Path.cwd()
    root = cwd.parent

    data = root / "data"
    figs = root / "figs"
    
    # Prompt for CSV file name
    csv_filename = input("Enter CSV file name: ").strip()
    csv_path = data / csv_filename

    if not csv_path.is_file():
        print(f"Error: File '{csv_path}' not found.")
        return

    # Load CSV
    df = pd.read_csv(csv_path)

    # Normalize column headers
    df.columns = [col.strip().lower().replace(" ", "_") for col in df.columns]

    # Extract true labels and predictions
    y_true = df["label"]
    y_pred = df["machine_suggestion_1"]

    # Create list of all labels
    labels = sorted(set(y_true) | set(y_pred))

    # Compute normalized confusion matrix
    cm_normalized = confusion_matrix(y_true, y_pred, labels=labels, normalize="true")

    # Plot
    fig, ax = plt.subplots(figsize=(18, 14))
    disp = ConfusionMatrixDisplay(confusion_matrix=cm_normalized, display_labels=labels)
    disp.plot(include_values=True, cmap="Blues", ax=ax, xticks_rotation=90, values_format=".2f")

    ax.set_xlabel("Predicted Label", fontsize=14)
    ax.set_ylabel("True Label", fontsize=14)
    ax.set_title(f"Normalized Confusion Matrix ({csv_path.stem})", fontsize=16)
    plt.tight_layout()

    # Save PNG
    output_path = figs / f"{csv_path.stem}-confusion_matrix.png"
    plt.savefig(output_path, dpi=300)
    plt.close()

    print(f"Confusion matrix saved to: {output_path}")

if __name__ == "__main__":
    main()
