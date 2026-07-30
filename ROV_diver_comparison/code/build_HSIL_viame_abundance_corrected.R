## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## build corrected VIAME abundance data across all 24 transects ~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## For each of the 24 site x season x transect combinations: writes an
## annotated_qaqc/ folder (see annotate_viame_detections_functions.R for the
## drawing style and the QA rationale) AND tallies per-photo species counts
## straight from that same JSON parse -- so the corrected CSVs below are
## built from the same ground-truth-verified data as the QA imagery, not
## re-derived separately.
##
## Per-photo species counts come from the raw VIAME_raw_export/*.json tracks,
## not from HSIL_viame_abundance.csv's own count columns -- since that file's
## Transect ID is already known to be wrong for some rows (see functions file
## header), re-deriving counts directly from the JSON (resolved to real
## filenames via the transect's own folder) avoids inheriting any related
## bug in how that file's per-photo counts were assembled. HSIL_viame_
## abundance.csv is still used for one thing only: which photos count as the
## official ~1-photo-per-meter set (get.official.photos()).
##
## Outputs:
##   data/ROV/HSIL_viame_abundance_corrected.csv -- one row per official
##     photo (site, transect, season, depth, photo_name, species columns)
##   results/HSIL_viame_abundance_corrected_summed.csv -- one row per
##     transect (24 total), summed species counts + n_photos




## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls())

library(tidyverse)
library(magick)
library(jsonlite)


## set working directory one level up and verify
setwd("../")
getwd()


ROV_input <- "data/ROV"
results <- "results"
code <- "code"

source(file.path(code, "wrangle_data_functions.R"))
source(file.path(code, "annotate_viame_detections_functions.R"))

abundance_csv_path <- file.path(ROV_input, "HSIL_viame_abundance.csv")
ground_truth_path <- file.path(ROV_input, "HSIL_viame_transect_ground_truth.csv")
json_dir <- file.path(ROV_input, "VIAME_raw_export")
flights_root <- "C:/Users/randellz/Seattle Aquarium Dropbox/Coastal_Climate_Resilience/flights/HSIL"
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## registry of the 4 surveys (site code as used in HSIL_viame_abundance.csv's
## Site ID / the JSON filenames, survey date, season, and the transects/
## folder for that survey) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
surveys <- tribble(
  ~site,        ~date_prefix,  ~season,   ~transects_dir,
  "Centennial", "2024_10_08",  "summer",  file.path(flights_root, "2024/2024_10_08_diver-ROV_Centennial_Park/downward/photos/transects"),
  "EBM",        "2024_10_09",  "summer",  file.path(flights_root, "2024/2024_10_09_diver-ROV_EBM/downward/photos/transects"),
  "Centennial", "2025_01_27",  "winter",  file.path(flights_root, "2025/2025_01_27_Centennial/downward/HERO1/transects"),
  "EBM",        "2025_01_28",  "winter",  file.path(flights_root, "2025/2025_01_28_EBM/downward/HERO1/transects")
)

registry <- surveys %>%
  tidyr::crossing(transect_number = 1:6) %>%
  mutate(
    depth = if_else(transect_number <= 3, "deep", "shallow"),
    transect_dir = file.path(transects_dir, sprintf("T%d_%s", transect_number, depth))
  )
stopifnot(nrow(registry) == 24)


## find the one JSON file matching {date}_{site}_T{n}[_cropped].json
find.json.path <- function(date_prefix, site, transect_number) {
  pattern <- sprintf("^%s_%s_T%d(_cropped)?\\.json$", date_prefix, site, transect_number)
  hits <- list.files(json_dir, pattern = pattern)
  stopifnot(length(hits) == 1)
  file.path(json_dir, hits)
}

registry$json_path <- pmap_chr(registry[c("date_prefix", "site", "transect_number")], find.json.path)
## END registry ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## process all 24 transects ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
photo_meta_list <- vector("list", nrow(registry))
detections_list <- vector("list", nrow(registry))
diagnostics_list <- vector("list", nrow(registry))

## how many rows HSIL_viame_abundance.csv itself tags with this transect,
## for the before/after mislabeling comparison in the diagnostics report
abundance_raw <- read_csv(abundance_csv_path, show_col_types = FALSE)

for (i in seq_len(nrow(registry))) {
  r <- registry[i, ]
  cat(sprintf("\n[%d/24] %s %s T%d (%s, %s)\n", i, r$site, r$date_prefix, r$transect_number, r$season, r$depth))

  official <- get.official.photos(r$site, r$transect_number, r$date_prefix,
                                  abundance_csv_path = abundance_csv_path,
                                  ground_truth_path = ground_truth_path)

  originally_tagged <- sum(abundance_raw$`Site ID` == r$site &
                           abundance_raw$`Transect ID` == sprintf("T%d", r$transect_number) &
                           startsWith(abundance_raw$Name, r$date_prefix))
  diagnostics_list[[i]] <- tibble(
    site = r$site, date_prefix = r$date_prefix, transect_number = r$transect_number,
    originally_tagged = originally_tagged, corrected_count = nrow(official)
  )

  result <- process.transect(
    json_path = r$json_path,
    transect_dir = r$transect_dir,
    official_photos = official$Name
  )

  site_full <- unique(official$site)
  stopifnot(length(site_full) == 1)

  photo_meta_list[[i]] <- tibble(
    site = site_full, transect = r$transect_number, season = r$season,
    depth = r$depth, photo_name = official$Name
  )
  detections_list[[i]] <- result$detections %>%
    rename(photo_name = image)
}

cat("\nAll 24 transects processed.\n")
## END processing loop ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## build the corrected per-photo CSV ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
photo_meta <- bind_rows(photo_meta_list)
stopifnot(!anyDuplicated(photo_meta$photo_name))

detections_long <- bind_rows(detections_list)
species_cols <- sort(unique(detections_long$species_name))

species_wide <- detections_long %>%
  count(photo_name, species_name) %>%
  pivot_wider(names_from = species_name, values_from = n, values_fill = 0)

corrected <- photo_meta %>%
  left_join(species_wide, by = "photo_name") %>%
  mutate(across(all_of(species_cols), ~ replace_na(.x, 0))) %>%
  mutate(
    site = factor(site, levels = c("Centennial_Park", "Elliott_Bay_Marina")),
    season = factor(season, levels = c("summer", "winter"))
  ) %>%
  arrange(site, season, transect, photo_name) %>%
  mutate(site = as.character(site), season = as.character(season))

stopifnot(nrow(corrected) == nrow(photo_meta))

save.csv(corrected, ROV_input, "HSIL_viame_abundance_corrected.csv")
## END corrected per-photo CSV ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## build the 24-row transect-level sums ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
corrected_sums <- sum.by.group(
  corrected,
  group_cols = c("site", "transect", "season", "depth"),
  cols = species_cols
)
stopifnot(nrow(corrected_sums) == 24)

save.csv(corrected_sums, results, "HSIL_viame_abundance_corrected_summed.csv")
## END transect-level sums ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## diagnostics: HSIL_viame_abundance.csv's own Transect ID vs the corrected
## (ground-truth) transect assignment, per transect ~~~~~~~~~~~~~~~~~~~~~~~~~~
diagnostics <- bind_rows(diagnostics_list) %>%
  mutate(net_change = corrected_count - originally_tagged)

cat("\noriginally-tagged vs. ground-truth-corrected photo counts, per transect:\n")
print(diagnostics, n = Inf)

cat(sprintf("\ntotal originally tagged: %d | total corrected: %d\n",
           sum(diagnostics$originally_tagged), sum(diagnostics$corrected_count)))
## END diagnostics ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
