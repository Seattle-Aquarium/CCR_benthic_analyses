## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## CoralNet data processing and category editing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())
options(error = NULL)


## add libraries
library(tidyverse)
library(reshape)


## set working directory to home folder
setwd("../")
getwd()


## relative file paths
code <- "code"
data_input <- "data_input"
data_output <- "data_output"
figs <- "figs"
urban_kelp <- "data_output/Port_of_Seattle"
annotation_sum <- "data_output/annotation_summary"

## source functions 
source(file.path(code, "revise_categories_functions.R"))


#dat <- read.csv("2022_multiple_transects.csv")
#dat <- read.csv(file.path(urban_kelp, "2022_T1_T2.csv"))
dat <- read.csv(file.path(data_input, "2022_multiple_transects.csv"))
#dat <- read.csv(file.path(urban_kelp, "2022_all_current_photos.csv"))
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## invoke functions - data cleaning / processing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## remove "S" and "T" labels from site columns 
#dat <- remove_characters(dat, "site", "left", 1)
#dat <- remove_characters(dat, "transect", "left", 1)


## create key and SU columns, sort data appropriately by site
#dat <- create.key(dat)
#dat <- arrange(dat, site)
dat <- create.SU(dat)


## split off metadata and save, if desired
#metadata <- split_dataframe(dat, 27)


## Columns to preserve
cols_to_preserve <- c("SU", "key", "site", "transect", "img_name") # Columns to preserve


## Invoke the function to delete columns between "SU" and "Points", but preserve certain columns
dat <- delete_columns(dat, "SU", "Points", cols_to_preserve)
## END data processing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## revise CoralNet categories ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
revised <- dat

## smooth brown algae categories
revised <- revise_categories(revised, c("coland_KE", "X5rib_KE", "cabb_KE"), "textured_kelp")

## smooth brown algae categories
revised <- revise_categories(revised, c("X3rib_KE", "broad_KE", "split_KE", "strap_KE"), "smooth_kelp")

## other brown kelp categories
revised <- revise_categories(revised, c("cord_KE", "DiRet_KE", "feath_KE", "LaEph_KE", 
                                "LaLoSi_KE", "palm_KE", "ribbn_KE", "stalk_KE"),"other_brown_kelp")

## revise red algae categories
revised <- revise_categories(revised, c("filam_RE", "bushy_RE", "branch_RE", "leaf_RE", "unk_RE"), "red_algae")

## revise shell categories
revised <- revise_categories(revised, c("shell_SU", "lgshell_SU"), "shell")

## revise hard substrate categories
revised <- revise_categories(revised, c("reef_SU", "bould_SU", "concr_SU"), "hard_substrate")

## revise hard substrate categories
revised <- revise_categories(revised, c("glass_SU", "metal_SU", "poly_SU", "wood_SU", "anth_SU"), "anthro_substrate")

## revise eelgrass, surfgrass categories
revised <- revise_categories(revised, c("eel_SG", "surf_SG"), "seagrass")

## revise sessile invert categories
revised <- revise_categories(revised, c("HydBry_SI", "cup_SI", "CucEmb_SI", #"mussel_SI", 
                                "scallop_SI", "SponSol_SI", "TunSol_SI", "anem_SI", "encru_SI"), "sessile_invert")



## revise mobile invert categories
revised <- revise_categories(revised, c("CaCuc_MS", "crab_MS", "gastro_MS", 
                                "fish_MS", "SStar_MS", "urchin_MS"), "mobile_invert")

## revise eelgrass, surfgrass categories
revised <- revise_categories(revised, c("art_CA", "crust_CA"), "coralline_algae")
## end category revision ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## renmame columns and double-check ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## print df with names to check against renamed columns
#original_names <- as.data.frame(print(colnames(revised)))


## rename cols
revised <- rename_columns(revised, "SS_SU", "soft_sediment")
revised <- rename_columns(revised, "cob_SU", "cobble")
revised <- rename_columns(revised, "peb_SU", "pebble")
revised <- rename_columns(revised, "KelpBry_SI", "kelp_bryozoan")
revised <- rename_columns(revised, "UNIdent", "unknown")
revised <- rename_columns(revised, "filam_BR", "filamentous_brown")
revised <- rename_columns(revised, "FlatAci_BR", "acid_weed")
revised <- rename_columns(revised, "fucus_BR", "rock_weed")
revised <- rename_columns(revised, "holdfas_BR", "kelp_holdfast")
revised <- rename_columns(revised, "sargass_BR", "sargassum")
revised <- rename_columns(revised, "sugar_KE", "sugar_kelp")
revised <- rename_columns(revised, "shell", "shell_debris")
revised <- rename_columns(revised, "ulva_GR", "green_algae")


## print revised names
#revised_names <- as.data.frame(print(colnames(revised)))
#combined <- cbind(original_names, revised_names)
#remove(original_names, revised_names, combined)


## delete extraneous columns
revised <- remove_columns(revised, c("StriAci_BR", "undaria_BR", "unk_BR", "giant_KE", 
                             "bullBL_KE", "bullST_KE", "senes_RE", "senes_AL"))


## remove columns with very few observations
revised <- remove_columns(revised, c("rock_weed", "acid_weed", "smooth_kelp", "seagrass"))


## remove extra telemetry log columns
revised <- remove_columns(revised, c("alt_smooth", "SimDIS"))


## new ordering of columns
new.order <- c("key", "site", "transect", "img_name",
               "soft_sediment",
               "shell_debris",
               "cobble",
               "pebble",
               "hard_substrate",
               "anthro_substrate",
               "unknown",
               "filamentous_brown",
               "kelp_holdfast",
               "sargassum",
               "sugar_kelp",
               "green_algae",
               "textured_kelp",
               "other_brown_kelp",
               "red_algae",
               "coralline_algae",
               "kelp_bryozoan",
               "sessile_invert",
               "mobile_invert")


## Reorder the columns
revised <- revised %>% select(all_of(new.order))


## as.factor for future plotting
revised$site <- as.factor(revised$site)
revised$transect <- as.factor(revised$transect)
revised$key <- as.factor(revised$key)


## category summary
# sums_all_categories <- annotation.sum(revised, "soft_sediment", "mobile_invert")
# write.csv(sums_all_categories, file.path(annotation_sum, "sum_2022_all.csv"))


## save data
write.csv(revised, file.path(urban_kelp, "2022_revised_categories.csv"), row.names = FALSE)
## END column revision ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
