## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## wrangle HSIL ROV percent-cover data for ROV-diver comparison ~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Source: data/ROV/HSIL_percent_cover.csv -- one row per ROV photo, with
## x33 metadata columns (navigation/telemetry + survey ID) followed by x30
## percent-cover category columns (x2 sites, x6 transects, x2 seasons).
## This supersedes the earlier short_percent_t4/t6.csv exports (see
## wrangle_ROV_percent-cover_data.R), which only covered a single season.
##
## Category code -> descriptive name mappings (see consistent.labels() in
## wrangle_data_functions.R) were cross-checked against the Label/Long Label
## pairs in the raw Toolbox exports at ../Toolbox_classification_output/data/
## (the ground truth for these codes, one level up in the parent repo).





## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())


## add libraries
library(tidyverse)
library(stringr)


## set working directory one level up and verify
setwd("../")
getwd()


## relative file paths
ROV_input <- "data/ROV"
ROV_output <- "results/ROV"
code <- "code"
figs <- "figs"


## source functions
source(file.path(code, "wrangle_data_functions.R"))
source(file.path(code, "analyze_functions.R"))


## read ROV data
HSIL <- read.csv(file.path(ROV_input, "HSIL_percent_cover.csv"))
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## prep metadata ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## drop navigation/telemetry columns not needed for percent-cover analyses
## (battery columns are retained per request)
HSIL <- delete.cols(HSIL, c("Mode_num", "Mode",
                            "EKFlat", "EKFlon",
                            "DVLx", "DVLy", "DVLlat", "DVLlon",
                            "Depth_std", "Depth_Source",
                            "Heading", "Velocity_mps", "Width",
                            "NEDz", "VFR_alt"))


## rename instrument depth (m) to avoid colliding w/ the shallow/deep transect
## grouping added below (also called "depth" throughout the rest of the pipeline)
HSIL <- rename.columns(HSIL, "Depth", "ROV_depth_m")


## rename metadata to match site/transect convention used by the diver
## and ROV-abundance pipelines
HSIL <- rename.columns(HSIL, c("Site_name", "Transect_number"), c("site", "transect"))
HSIL$transect <- as.integer(HSIL$transect)


## parse survey date (source format is M/D/YYYY)
HSIL$Date <- as.Date(HSIL$Date, format = "%m/%d/%Y")


## standardize site names to match diver data
HSIL <- rename.cells(HSIL, "site", old_vals, new_vals)


## add shallow/deep (by transect number) and season (by date) columns
HSIL <- add.depth(HSIL)
HSIL <- add.season(HSIL)
## END metadata prep ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## prep percent-cover columns ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## rename raw category codes (e.g. "BR_filam") to descriptive names
HSIL <- consistent.labels(HSIL)


## explicit list of the 30 raw percent-cover category columns, captured before
## reorder.by.total() below (which sorts these columns by total cover, so a
## start/end name-range lookup would no longer reliably span the full block)
category_cols <- c("brown_algae_encrusting", "brown_algae_filamentous",
                   "brown_algae_fucus", "brown_algae_sargassum",
                   "green_algae_filamentous", "green_algae_ulva",
                   "kelp_five_rib", "kelp_bull_blade", "kelp_holdfast", "kelp_sieve",
                   "kelp_stipe", "kelp_sugar",
                   "mobile_species",
                   "red_algae_cca", "red_algae_branching", "red_algae_bushy",
                   "red_algae_encrusting", "red_algae_filamentous", "red_algae_flat_leaf",
                   "sessile_invertebrates", "kelp_bryozoan",
                   "anthropogenic", "boulder", "cobble", "pebble",
                   "sand_fine_shell", "shell_hash", "silt", "wood_debris",
                   "unknown_area")
stopifnot(all(category_cols %in% names(HSIL)))


## order percent-cover categories by total cover (most dominant first); purely
## cosmetic for the saved photo-level CSV, downstream steps use category_cols
HSIL <- reorder.by.total(HSIL, "brown_algae_encrusting", "unknown_area")


## combine categories to match Reef Check's single "red algae" / "green algae"
## categories (per README: ROV categories are summed to enable comparison)
HSIL <- combine.cols(HSIL, c("red_algae_branching",
                             "red_algae_bushy",
                             "red_algae_filamentous",
                             "red_algae_flat_leaf"),
                     "combined_red_algae")

HSIL <- combine.cols(HSIL, c("red_algae_cca",
                             "red_algae_encrusting"),
                     "combined_encrusting_algae")
                     

HSIL <- combine.cols(HSIL, c("green_algae_filamentous",
                             "green_algae_ulva"),
                     "combined_green_algae")


## capture whatever "combined_*" columns were just created above, so
## downstream steps stay correct automatically as combined categories are
## added/removed/renamed, rather than relying on separately-maintained lists
## (each combined column is a sum of raw categories already counted elsewhere,
## so it must be excluded from any full-category sum/points accounting below,
## e.g. the sanity check and the points-based files, to avoid double-counting)
combined_cols <- names(HSIL)[startsWith(names(HSIL), "combined_")]


## sanity check: per-photo percent-cover categories should sum to ~1
row_totals <- rowSums(HSIL[category_cols], na.rm = TRUE)
range(row_totals)
## END percent-cover prep ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## build long-form + transect-averaged versions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## long form: one row per photo x category (useful for faceted plots / models)
HSIL_long <- HSIL %>%
  pivot_longer(cols = all_of(category_cols),
              names_to = "category",
              values_to = "percent_cover")


## transect-level means (grouped by site/transect/depth/season, since each
## site x transect was surveyed in both S24 and W25)
HSIL_avg <- average.by.group(HSIL,
                             group_cols = c("site", "transect", "depth", "season"),
                             cols = c(category_cols, combined_cols))
## END long-form + averaging ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## point-count versions of the 30 raw percent-cover columns ~~~~~~~~~~~~~~~~~~~
## each photo's percent-cover values were generated from 50 randomly
## distributed points (e.g. 0.38 cover == 19/50 points), so multiplying by 50
## recovers the underlying point counts, which can be summed (unlike percentages)
## step 1: per-photo point counts (drop the percent-scale combined_* cols,
## since those aren't part of the raw 30-category point-count accounting --
## and, being sums of other categories, would double-count if left in)
HSIL_points_photo <- HSIL %>%
  select(-all_of(combined_cols)) %>%
  mutate(across(all_of(category_cols), ~ round(.x * 50)))


## step 2: sum point counts within each transect, and add total_points
## (n_photos x 50 points/photo) -- the denominator for step 3
HSIL_points_sum <- sum.by.group(HSIL_points_photo,
                                group_cols = c("site", "transect", "depth", "season"),
                                cols = category_cols) %>%
  mutate(total_points = n_photos * 50, .after = n_photos)


## step 3: each category's transect sum divided by total_points -- recovers
## the transect-average percent cover (equivalent to HSIL_avg's raw category
## means above, since every photo contributes exactly 50 points)
HSIL_points_avg <- HSIL_points_sum %>%
  mutate(across(all_of(category_cols), ~ round(.x / total_points, 3)))
## END point-count versions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## save the new dataframes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
save.csv(HSIL, ROV_output, "HSIL_percent-cover_photo-level.csv")
save.csv(HSIL_long, ROV_output, "HSIL_percent-cover_long.csv")
save.csv(HSIL_avg, ROV_output, "HSIL_percent-cover_transect-averaged.csv")

save.csv(HSIL_points_photo, ROV_output, "HSIL_points_photo-level.csv")
save.csv(HSIL_points_sum, ROV_output, "HSIL_points_transect-sums.csv")
save.csv(HSIL_points_avg, ROV_output, "HSIL_points_transect-average.csv")
## END save ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
