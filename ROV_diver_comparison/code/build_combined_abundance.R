## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## build combined ROV-diver abundance data, ready for models.Rmd ~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Combines diver_invert_abundance.csv and HSIL_viame_abundance_corrected_
## summed.csv into one long-format table (site, transect, season, depth, type)
## restricted to the 10 taxa recorded by BOTH methods (see tab:overlap_taxa --
## clam_siphon excluded, since diver count = 0 across all 24 transects). This
## replaces the abundance half of the data-assembly steps that used to live
## inline in models.Rmd's "Set up data" chunk, which pointed at
## results/HSIL_abundances_averaged.csv -- a file that no longer exists
## (superseded by HSIL_viame_abundance_corrected_summed.csv, see
## build_HSIL_viame_abundance_corrected.R).
##
## Output:
##   results/combined/ROV_diver_abundance_combined.csv -- one row per
##     transect x method (48 rows: 24 transects x 2 methods), with
##     transect_id identifying the 12 physical transect locations (site x
##     transect number, collapsed across season) for use as the (1|transect_id)
##     random-effect grouping variable in models.Rmd, and key -- a fully
##     unique site/transect/season identifier (e.g. "CP_1_summer") shared by
##     the diver and ROV rows for that same sampling event, distinguishing
##     e.g. Centennial Park T1 summer from Centennial Park T1 winter (see
##     add.key() in wrangle_data_functions.R).




## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls())


## add libraries
library(tidyverse)


## set working directory one level up and verify
setwd("../")
getwd()


## relative file paths
diver_input <- "results/diver"
ROV_input <- "results/ROV/abundance"
combined_output <- "results/combined"
code <- "code"


## source functions
source(file.path(code, "wrangle_data_functions.R"))
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## the 10 invertebrate taxa recorded by BOTH diver and ROV methods (see
## tab:overlap_taxa) -- clam_siphon excluded, since divers never recorded one
## across all 24 transects
overlap_taxa <- c("ochre_mottled_star", "cancer_crab", "burrowing_sea_cucumber",
                  "kelp_crab", "leather_star", "plumose_anemone",
                  "green_white_urchin", "california_sea_cucumber",
                  "blood_star", "large_anemone")




## read + tag diver abundance ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
diver <- read.csv(file.path(diver_input, "diver_invert_abundance.csv")) %>%
  mutate(type = "diver") %>%
  select(site, transect, season, depth, type, all_of(overlap_taxa))
## END diver ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## read + tag ROV abundance ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## n_photos is carried through as a possible search-effort covariate (e.g. an
## offset term) -- there's no diver equivalent (diver abundance is a single
## swath count per transect, not a photo-by-photo tally), so it's left NA on
## diver rows below rather than inventing a value
rov <- read.csv(file.path(ROV_input, "HSIL_viame_abundance_corrected_summed.csv")) %>%
  mutate(type = "ROV") %>%
  select(site, transect, season, depth, type, n_photos, all_of(overlap_taxa))
## END ROV ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## combine + derive transect_id and key ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
combined <- bind_rows(diver, rov) %>%
  mutate(transect_id = paste(transect, site)) %>%
  add.key() %>%
  select(site, transect, transect_id, key, season, depth, type, n_photos, everything())

stopifnot(nrow(combined) == 48)                          ## 24 transects x 2 methods
stopifnot(length(unique(combined$transect_id)) == 12)    ## 6 transects x 2 sites
stopifnot(length(unique(combined$key)) == 24)            ## 6 transects x 2 sites x 2 seasons
## END combine ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## save ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!dir.exists(combined_output)) dir.create(combined_output, recursive = TRUE)
save.csv(combined, combined_output, "ROV_diver_abundance_combined.csv")
## END save ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
