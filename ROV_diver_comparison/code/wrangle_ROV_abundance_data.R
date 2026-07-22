## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## wrangle data for ROV-diver comparison ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
ROV_input <- "data/ROV"
ROV_output <- "results/ROV"
code <- "code"
diver_output <- "results/diver"


## source functions 
source(file.path(code, "wrangle_data_functions.R"))


## read ROV data
ROV_objects <- read.csv(file.path(ROV_input, "ROV_VIAME_abundance_data.csv"))
invert <- read.csv(file.path(diver_output, "diver_invert_abundance.csv"))
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ROV objects (abundances) data wrangling ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ROV_objects <- remove.chars(ROV_objects, Transect, 1)


## rename ROV cols (species codes -> descriptive names)
ROV_objects <- rename.columns(ROV_objects, names(rov_invert_name_map), unname(rov_invert_name_map))


## trim down the 1s interval data to every 3s to avoid double counting 
ROV_objects <- nth.row(ROV_objects, n=3)


## rename metadata Transect --> transect; Site --> site 
ROV_objects <- rename.metadata(ROV_objects)


## add Reef Check inverts not observed by ROV to ROV dataset
ROV_objects <- add.Reef.Check.inverts(source_df = invert, 
                                      receiver_df = ROV_objects)


## summarize by site and transect 
ROV_objects <- summarize.by.site.transect(ROV_objects,
                                          "ochre_mottled_star", 
                                          "sunflower_star")

## remame site factors 
ROV_objects <- rename.cells(ROV_objects, "site", old_vals, new_vals)
## END ROV abundance data wrangling ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## save .csv of ROV abundances ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
save.csv(ROV_objects, ROV_output, "ROV_invert_abundance.csv")
## END save ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## combine ROV and diver abundances into single dataframe ~~~~~~~~~~~~~~~~~~~~~~
## reuse `invert` (already read at startup) rather than re-reading the same
## file; the old version also force-reordered rows to [7:12, 1:6], which
## assumed exactly 12 rows (one season) -- now that the diver data covers two
## seasons (24 rows), that would have silently dropped every winter row before
## combine_with_zero_fill() ever ran. bind_rows() (used inside
## combine_with_zero_fill()) matches by column name, not row position, so no
## row reordering is needed here at all.
diver_invert_abundance <- add_column(invert, "observer", "diver", 3)
diver_invert_abundance <- add_depth_column(diver_invert_abundance, 4)


ROV_abundance <- read.csv(file.path(ROV_output, "ROV_invert_abundance.csv"))
ROV_abundance <- add_column(ROV_abundance, "observer", "ROV", 3)
ROV_abundance <- add_depth_column(ROV_abundance, 4)

invert_abundances <- combine_with_zero_fill(ROV_abundance, diver_invert_abundance)

save.csv(invert_abundances, ROV_output, "invert_abundances.csv")

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
