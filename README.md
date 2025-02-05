# CCR benthic taxa analyses
A (temporarily) private repo to work on code development analyzing CoralNet percent-cover data, VIAME abundance data, and bull kelp stipe and bundle counts

### data_input
- `2022_multiple_transects.csv` contains the raw output from CoralNet merged with our ROV telemetry file, containing 1479 images and 118,000 annotations in CoralNet.
- `bull_kelp_stipes.csv` contains bull kelp stipe and bundle counts from the forward-facing video.
- `to-do_file.csv` contains our CoralNet annotations, including all XX categories, and the revised YY categories. 

### data_output
- `2022_all.csv` contains all 1479 rows transformed to 0-1 scale for percent-coverage of the revised categories.
- `summary/2022_all_photos.csv` a list of images per transect.
- `summary/T1_T2_T3_photos.csv` a list of rarified images per transect.
- `summary/sum_of_all_categories` a list of # of annotations per label.
- `summary/sum_2022_revised` a list of # of annotations per label for the revised label set.

### code
- ...
- ...
