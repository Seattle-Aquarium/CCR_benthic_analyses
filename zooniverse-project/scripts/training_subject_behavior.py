import os
from pathlib import Path

from dotenv import load_dotenv
from panoptes_client import Panoptes, Workflow, SubjectSet

###################################
### Training Subject Configuration Script
# Note: in addition to running this script, a Zoo team member must
# also configure Caesar and set workflow retirement to `never_retire`

# Input Parameters
workflowid = 32022
subjsetids = [138298]

# Define training chances according to your needs.
chances = (10 * [0.5]) + (40 * [0.2]) + (50 * [0.1])
default_chance = 0.05

# If training subject metadata needs editing, then enable.
edit_metadata = False
###################################

# Load Zooniverse credentials from scripts/config.env
env_path = Path(__file__).parent / "config.env"
load_dotenv(env_path)

puser = os.environ.get("ZOONIVERSE_USERNAME", "").strip()
ppswd = os.environ.get("ZOONIVERSE_PASSWORD", "").strip()

missing = []
if not puser:
    missing.append("ZOONIVERSE_USERNAME")
if not ppswd:
    missing.append("ZOONIVERSE_PASSWORD")
if missing:
    raise ValueError(
        "Missing required credentials in scripts/config.env: " + ", ".join(missing)
    )

Panoptes.connect(username=puser, password=ppswd)

w = Workflow.find(workflowid)
w.configuration['training_set_ids'] = subjsetids
w.configuration['training_default_chance'] = default_chance
w.configuration['training_chances'] = chances
w.configuration['subject_queue_page_size'] = 4
w.save()

if edit_metadata:
    new_metadata = {"#training_subject": "true"}
    for ssid in subjsetids:
        subject_set = SubjectSet.find(ssid)
        print('Editing metadata for Subject Set #{0}: {1}'.format(subject_set.raw['id'], subject_set.raw['display_name']))
        for subject in subject_set.subjects:
            for key, value in new_metadata.items():
                subject.metadata[key] = value
                subject.save()