## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## wrange data for ROV-diver comparison ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





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
diver_input <- "data/diver"
diver_output <- "results/diver"
code <- "code"


## source functions 
source(file.path(code, "wrangle_data_functions.R"))


## read DIVER data 
original_diver_algae <- read.csv(file.path(diver_input, "Algae_Washington_raw_2025.csv"))
original_diver_invert <- read.csv(file.path(diver_input, "Invert_Washington_raw_2025.csv"))
original_diver_UPC <- read.csv(file.path(diver_input, "UPC_Washington_raw_2025.csv"))
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## wrangle diver data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## create new data frames
algae <- filter.and.sort(original_diver_algae, sites_to_retain)
invert <- filter.and.sort(original_diver_invert, sites_to_retain)
UPC <- filter.and.sort(original_diver_UPC, sites_to_retain)


## isolate to the core survey dates; remove summer 2025 
algae <- remove_summer_2025(algae)
invert <- remove_summer_2025(invert)
UPC <- remove_summer_2025(UPC)


## remove the original dataframes
remove(original_diver_algae, original_diver_invert, original_diver_UPC)


## rename site cols within all Reef Check dataframes
algae <- rename.cells(algae, "Site", old_vals, new_vals)
invert <- rename.cells(invert, "Site", old_vals, new_vals)
UPC <- rename.cells(UPC, "Site", old_vals, new_vals)


## rename site / transect to standardize with ROV data
algae <- rename.metadata(algae)
invert <- rename.metadata(invert)
UPC <- rename.metadata(UPC)


## extrapolate abundances out to 30m in instances of subsampled data 
algae <- extrapolate.abundance(algae, 
                               amount_col = "Amount", 
                               distance_col = "Distance")


## slim down the UPC column names
UPC <- remove.chars(UPC, Category, 4)


## combine UPC names
UPC <- combine.UPC.names(UPC)
UPC$Percentage <- round(UPC$Percentage, 1)
## END initial wrangling ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## convert long -> short form and further transform ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## convert to short/wide form for algae, invert, and UPC data
algae <- compress.to.wide(algae, class_col = "Classcode", value_col = "extrapolated")
invert <- compress.to.wide(invert, class_col = "Classcode", value_col = "Amount")
UPC <- compress.to.wide(UPC, class_col = "combined_name", value_col = "Percentage")


## add depth column
algae <- add.depth(algae) 
invert <- add.depth(invert)
UPC <- add.depth(UPC)


## add season column
algae <- add.season(algae)
invert <- add.season(invert)
UPC <- add.season(UPC)


## combine mottled and ochre stars for diver invert data; delete old cols
invert$ochre_mottled_star <- invert$`Ochre Star` + invert$`Mottled Star`
invert <- delete.cols(invert, c("Ochre Star", "Mottled Star"))


## standarize cols
algae <- rename_columns(algae, algae_name_map)
invert <- standardize.invert.cols(invert)


## explicit species-column lists, captured now (before reorder.by.total()
## below re-sorts columns by total value). calculate.density() is called
## further down with these, rather than a start_col/end_col name-range,
## since a range is looked up by *current* column position and would only
## "happen" to be correct if the intended start/end species still ranked
## highest/lowest by total after reordering
meta_cols <- c("Date", "season", "site", "transect", "depth")
algae_cols <- setdiff(names(algae), meta_cols)
invert_cols <- setdiff(names(invert), meta_cols)


## combine red algae categories
UPC <- combine.cols(UPC, c("cover_red_algae",
                           "superlayer_red_algae"),
                    "combined_red_algae")


## combine green categories -- NOT a simple sum, unlike the other three
## combine.cols() calls below. Cover and superlayer are two independent
## UPC "mini-surveys" recorded at the same 30 points (cover = primary
## substrate-level layer; superlayer = whether a >30cm canopy is ALSO
## present, of a given algae type), not two mutually exclusive outcomes of
## one draw -- so a point can in principle register green algae in BOTH,
## and cover_green_algae + superlayer_green_algae can double-count it.
## Superlayer's contribution is small for red algae (5% of the combined
## total across all 24 core transects) but substantial for green algae
## (38.5% of the combined total; superlayer > cover outright at Centennial
## Park transect 5) -- so this matters in practice here, unlike for red
## algae.
##
## No file in this pipeline (raw or wrangled) records which of the 30
## points contributed to each tally, so a true per-point union (count of
## points with green algae in cover OR superlayer, no double-counting) is
## not reconstructable. As the best available approximation, this treats
## cover and superlayer as independent per-point events and computes the
## expected fraction of points registering in at least one:
## P(cover or superlayer) = 1 - P(not cover) * P(not superlayer)
## -- i.e. inclusion-exclusion under an assumed independence between the
## two axes. This is a labeled statistical approximation, not a measured
## quantity: it is bounded correctly at 100 (unlike a raw sum, which
## cannot exceed 100 in this dataset but has no structural reason not to),
## and reduces to approximately cover + superlayer when both are small
## (negligible double-counting) while discounting the naive sum as either
## grows large (more room for the same points to double-count).
UPC$combined_green_algae <- 100 * (1 - (1 - UPC$cover_green_algae / 100) * (1 - UPC$superlayer_green_algae / 100))


## combine substrate boulder categories
UPC <- combine.cols(UPC, c("substrate_large_boulder_(50cm-1m-wa)",
                           "substrate_reef"),
                    "combined_substrate_boulder")


## combine substrate pebble/cobble categories
UPC <- combine.cols(UPC, c("substrate_pebble_(0.5-5cm-wa)",
                           "substrate_cobble_(5-15cm-wa)"),
                    "combined_substrate_pebble")



## re-order algae and invert columns with the most dense at the beginning of the data frame
algae <- reorder.by.total(algae, "acid_weed", "woody_kelp")
invert <- reorder.by.total(invert, "bat_star", "ochre_mottled_star")



## calculate density for algae abundances
algae_density <- calculate.density(df = algae,
                                   cols = algae_cols,
                                   divisor = 60)


## calculate density for invert abundances
invert_density <- calculate.density(df = invert,
                                    cols = invert_cols,
                                    divisor = 60)
## END diver data wrangling ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## save files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## save csvs of diver abundances 
save.csv(invert, diver_output, "diver_invert_abundance.csv")
save.csv(algae, diver_output, "diver_algae_abundance.csv")


## save csvs of diver densities
save.csv(invert_density, diver_output, "diver_invert_density.csv")
save.csv(algae_density, diver_output, "diver_algae_density.csv")


## save csvs of UPC 
save.csv(UPC, diver_output, "diver_UPC_percentage.csv")
## END file save ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
