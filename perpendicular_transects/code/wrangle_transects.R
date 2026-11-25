## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## wrangle Fishing Pier perpendicular transects (T1, T2; 8 Oct 2025) ~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## NOTE: run via RunMe.R -- packages, working directory, function sourcing, and
## the path variables used below are all set up there.




## photo-level percent cover + telemetry ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## both transects share the full label set (zero-filled), are stacked, and get
## cumulative distance, tide-corrected depth, and the nine cover groups
photos <- bind_rows(
  read.transect(file.path(data_dir, "FPR_S25_T1.csv")),
  read.transect(file.path(data_dir, "FPR_S25_T2.csv"))
) %>%
  rename(transect = Transect_number)

## every photo's 50 points must account for 100% of its cover
stopifnot(all(abs(rowSums(photos[names(label_set)]) - 1) < 1e-9))

tide <- read.tide("2025-10-08", file.path(data_dir, "NOAA_9447130_water_level_2025-10-08.csv"))

photos <- photos %>%
  add.cumulative.distance() %>%
  add.tide.corrected.depth(tide) %>%
  add.cover.groups()

photos %>%
  group_by(transect) %>%
  summarise(n_photos = n(),
            length_m = max(cumulative_distance_m),
            depth_mllw_range = paste(round(range(depth_mllw_m), 1), collapse = " to "),
            water_level_mllw = paste(round(range(water_level_mllw_m), 2), collapse = " to ")) %>%
  print()
## END photo-level ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## point-level annotations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## image row/column + label for all 50 points of every photo -- used for the
## point-overlay figure. These live alongside the photos on Dropbox; the two
## files below are the final (cleaned / QA-QC'd) versions, and are checked here
## against the percent-cover CSVs point for point.
points <- bind_rows(
  read.points(file.path(flights_root, "T1", "edited", "FPR_S25_T1_toolbox_annotations_cleaned.csv"), 1),
  read.points(file.path(flights_root, "T2", "edited", "zooniverse_gui", "FPR_S25_T2_toolbox_annotations_cleaned.json"), 2)
) %>%
  mutate(group = code.to.group(code))

check.points.match.cover(points, photos)
## END point-level ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## save ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## photo-level file keeps the short label codes as column names (matching the
## raw CSVs) followed by the nine cover-group columns
save.csv(photos, results_dir, "FPR_S25_photo-level.csv")
save.csv(points, results_dir, "FPR_S25_point-level.csv")
## END save ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
