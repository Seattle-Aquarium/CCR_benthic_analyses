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
diver_input <- "data_input/diver"
diver_output <- "data_output/diver"
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


## combine red algae categories
UPC <- combine.cols(UPC, c("Cover_Red Algae", 
                           "Superlayer Red Algae"), 
                    "combined_red_algae")


## combine green categories
UPC <- combine.cols(UPC, c("Cover_Green Algae", 
                           "Superlayer Green Algae"), 
                    "combined_green_algae")



## re-order algae and invert columns with the most dense at the beginning of the data frame
algae <- reorder.by.total(algae, "acid_weed", "woody_kelp")
invert <- reorder.by.total(invert, "bat_star", "ochre_mottled_star")



## calculate density for algae abundances
algae_density <- calculate.density(df = algae, 
                                   start_col = "kelp_sieve", 
                                   end_col = "woody_kelp", 
                                   divisor = 60)


## calculate density for invert abundances
invert_density <- calculate.density(df = invert, 
                                    start_col = "ochre_mottled_star",
                                    end_col = "sunflower_star",
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
