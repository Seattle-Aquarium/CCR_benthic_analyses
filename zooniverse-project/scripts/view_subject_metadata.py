from panoptes_client import Panoptes, SubjectSet

Panoptes.connect(username="m-williams", password="Aquarium2")

subject_set = SubjectSet.find(138298)
for subject in subject_set.subjects:
    print(subject.id, subject.metadata)
    break  # just check the first one first