## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## build combined ROV-diver percent-cover data, ready for percent-cover_models.R
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Combines diver_UPC_percentage.csv and HSIL_percent-cover_transect-averaged.csv
## (the photo-averaged ROV percent-cover -- one row per transect, matching the
## diver's one-UPC-summary-per-transect grain) into one long-format table
## (site, transect, season, depth, type), restricted to the 8 cover categories
## percent-cover_models.R crosswalks between the two protocols. This replaces
## the percent-cover half of the data-assembly steps that used to live inline
## in that script's "Set up data" step.
##
## Category crosswalk (diver name = ROV name), ported as-is from percent-cover_models.R:
##   cover_red_algae             = combined_red_algae
##   combined_green_algae        = green_algae_ulva
##   cover_crustose_coralline    = red_algae_cca
##   combined_substrate_boulder  = boulder
##   substrate_rock_.15.25cm.wa. = cobble
##   combined_substrate_pebble   = pebble
##   substrate_sand              = sand_fine_shell
##   substrate_shell_hash        = shell_hash
##
## Diver UPC values are recorded on a 0-100 scale and are divided by 100 here;
## ROV transect-averaged values are already on a 0-1 proportion scale (see
## HSIL_percent-cover_transect-averaged.csv) and are left as-is.
##
## "n" (the percent-cover sample-size denominator, needed for binomial/beta-
## binomial models in percent-cover_models.R): divers record a fixed n = 30
## (one point every meter along the 30m tape). ROV n is the total number of
## classified percent-cover points across all photos in a transect (up to 50
## points per photo, per HSIL_points_photo-level.csv), summed here from that
## file -- this replaces percent-cover_models.R's equivalent rowwise()/
## list()/unlist() step with a plain group_by() %>% summarise(sum(...)), and
## sums by explicit point-category column name rather than column position
## (22:51), since position-based indexing silently breaks if that file's
## column order ever changes.
##
## Output:
##   results/combined/ROV_diver_percent_cover_combined.csv -- one row per
##     transect x method (48 rows: 24 transects x 2 methods), with
##     transect_id (site x transect, collapsed across season) and key (fully
##     unique site/transect/season identifier, e.g. "CP_1_summer") -- see
##     build_combined_abundance.R and add.key() in wrangle_data_functions.R
##     for the same pattern applied to the abundance data.




## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls())


## add libraries
library(tidyverse)


## set working directory one level up and verify
setwd("../")
getwd()


## relative file paths
diver_input <- "results/diver"
ROV_input <- "results/ROV/percent_cover"
combined_output <- "results/combined"
code <- "code"


## source functions
source(file.path(code, "wrangle_data_functions.R"))
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## the 30 percent-cover point categories in HSIL_points_photo-level.csv
## (used to total classified points per photo -> n per transect)
point_categories <- c(
  "pebble", "boulder", "kelp_sieve", "silt", "green_algae_ulva",
  "red_algae_branching", "red_algae_cca", "sand_fine_shell", "kelp_sugar",
  "red_algae_flat_leaf", "unknown_area", "red_algae_filamentous", "cobble",
  "brown_algae_filamentous", "red_algae_bushy", "brown_algae_sargassum",
  "shell_hash", "mobile_species", "kelp_five_rib", "anthropogenic",
  "brown_algae_encrusting", "kelp_stipe", "kelp_holdfast", "kelp_bryozoan",
  "wood_debris", "sessile_invertebrates", "kelp_bull_blade",
  "brown_algae_fucus", "green_algae_filamentous", "red_algae_encrusting"
)




## read + tag diver percent-cover ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
diver <- read.csv(file.path(diver_input, "diver_UPC_percentage.csv")) %>%
  mutate(type = "diver", n = 30) %>%
  select(site, transect, season, depth, type, n,
         cover_red_algae, combined_green_algae, cover_crustose_coralline,
         combined_substrate_boulder, substrate_rock_.15.25cm.wa.,
         combined_substrate_pebble, substrate_sand, substrate_shell_hash) %>%
  mutate(across(cover_red_algae:substrate_shell_hash, ~ .x / 100))
## END diver ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## ROV: total classified points per transect (n) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rov_n <- read.csv(file.path(ROV_input, "HSIL_points_photo-level.csv")) %>%
  mutate(npoints = rowSums(across(all_of(point_categories)))) %>%
  group_by(site, transect, season) %>%
  summarise(n = sum(npoints), .groups = "drop")
## END ROV n ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## read + tag ROV percent-cover ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rov <- read.csv(file.path(ROV_input, "HSIL_percent-cover_transect-averaged.csv")) %>%
  mutate(type = "ROV") %>%
  left_join(rov_n, by = c("site", "transect", "season")) %>%
  select(site, transect, season, depth, type, n,
         cover_red_algae = combined_red_algae,
         combined_green_algae = green_algae_ulva,
         cover_crustose_coralline = red_algae_cca,
         combined_substrate_boulder = boulder,
         substrate_rock_.15.25cm.wa. = cobble,
         combined_substrate_pebble = pebble,
         substrate_sand = sand_fine_shell,
         substrate_shell_hash = shell_hash)
## END ROV ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## combine + derive transect_id and key ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
combined <- bind_rows(diver, rov) %>%
  mutate(transect_id = paste(transect, site)) %>%
  add.key() %>%
  select(site, transect, transect_id, key, season, depth, type, n, everything())

stopifnot(nrow(combined) == 48)                          ## 24 transects x 2 methods
stopifnot(length(unique(combined$transect_id)) == 12)    ## 6 transects x 2 sites
stopifnot(length(unique(combined$key)) == 24)            ## 6 transects x 2 sites x 2 seasons
stopifnot(!anyNA(combined$n))                            ## every transect got a point-count total
## END combine ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## save ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!dir.exists(combined_output)) dir.create(combined_output, recursive = TRUE)
save.csv(combined, combined_output, "ROV_diver_percent_cover_combined.csv")
## END save ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
