import os
import csv
from pathlib import Path

from dotenv import load_dotenv
from panoptes_client import Panoptes, SubjectSet

###################################
subjset_id = 138298
csv_path = r"C:\Users\williamsm\Seattle Aquarium Dropbox\Coastal_Climate_Resilience\machine_learning\Zooniverse\training_set\training_subjects_metadata.csv"
###################################

env_path = Path(__file__).parent / "config.env"
load_dotenv(env_path)

puser = os.environ.get("ZOONIVERSE_USERNAME", "").strip()
ppswd = os.environ.get("ZOONIVERSE_PASSWORD", "").strip()

Panoptes.connect(username=puser, password=ppswd)

# Build a lookup: filename -> feedback metadata fields from the manifest
feedback_by_filename = {}
with open(csv_path, newline="") as f:
    reader = csv.DictReader(f)
    for row in reader:
        feedback_by_filename[row["filename"]] = {
            "#training_subject": row["#training_subject"],
            "#feedback_1_id": row["#feedback_1_id"],
            "#feedback_1_answer": row["#feedback_1_answer"],
            "#feedback_1_successMessage": row["#feedback_1_successMessage"],
            "#feedback_1_failureMessage": row["#feedback_1_failureMessage"],
        }

subject_set = SubjectSet.find(subjset_id)
updated, skipped = 0, 0

for subject in subject_set.subjects:
    filename = subject.metadata.get("filename")
    if filename in feedback_by_filename:
        subject.metadata.update(feedback_by_filename[filename])
        subject.save()
        updated += 1
    else:
        skipped += 1

print(f"Updated: {updated}, skipped (no filename match): {skipped}")