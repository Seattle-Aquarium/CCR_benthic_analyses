## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## CoralNet data processing and category editing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())


## add libraries
library(tidyverse)
library(reshape)


## list out current path
getwd()


## hardcode relative file paths
code <- "../code"
data_input <- "../data_input"
data_output <- "../data_output"
figs <- "../figs"
urban_kelp <- "../data_output/Port_of_Seattle"


## source functions - remove unnecessary functions
setwd(code)
source("revise_categories_functions.R")


## invoke relative file path 
setwd(data_input)


## read in csv file 
dat <- read.csv("combined_csv.csv")
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## invoke functions - data cleaning / processing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## remove "S" and "T" labels from site columns 
dat <- remove_characters(dat, "site", "left", 1)
dat <- remove_characters(dat, "transect", "left", 1)


## create key and SU columns, sort data appropriately by site
dat <- create.key(dat)
dat <- arrange(dat, site)
dat <- create.SU(dat)


## split off metadata
metadata <- split_dataframe(dat, 31)


## columns to delete (most metadata)
cols_to_delete <- 1:31 


## but preserve these specific columns 
cols_to_preserve <- c("SU", "key", "site", "transect", "img_name") # Columns to preserve


## invoke function
dat <- delete_columns(dat, cols_to_delete, cols_to_preserve) # Delete columns and get the updated dataframe
## END data processing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## revise CoralNet categories ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## smooth brown algae categories
dat <- revise_categories(dat, c("coland_KE", "X5rib_KE", "cabb_KE"), "textured_kelp")

## smooth brown algae categories
dat <- revise_categories(dat, c("X3rib_KE", "broad_KE", "split_KE", "strap_KE"), "smooth_kelp")

## other brown kelp categories
dat <- revise_categories(dat, c("cord_KE", "DiRet_KE", "feath_KE", "LaEph_KE", 
                                "LaLoSi_KE", "palm_KE", "ribbn_KE", "stalk_KE"),"other_brown_kelp")

## revise red algae categories
dat <- revise_categories(dat, c("filam_RE", "bushy_RE", "branch_RE", "leaf_RE", "unk_RE"), "red_algae")

## revise shell categories
dat <- revise_categories(dat, c("Shell_SU", "lgshell_SU"), "shell_SU")

## revise hard substrate categories
dat <- revise_categories(dat, c("reef_SU", "bould_SU", "concr_SU"), "hard_substrate")

## revise hard substrate categories
dat <- revise_categories(dat, c("glass_SU", "metal_SU", "poly_SU", "wood_SU", "anth_SU"), "anthro_substrate")

## revise eelgrass, surfgrass categories
dat <- revise_categories(dat, c("eel_SG", "surf_SG"), "seagrass")

## revise sessile invert categories
dat <- revise_categories(dat, c("HydBry_SI", "cup_SI", "CucEmb_SI", "mussel_SI", 
                                "scallop_SI", "SponSol_SI", "TunSol_SI", "anem_SI", "encru_SI"), "sessile_invert")

## revise mobile invert categories
dat <- revise_categories(dat, c("CaCuc_MS", "crab_MS", "gastro_MS", 
                                "fish_MS", "SStar_MS", "urchin_MS"), "mobile_invert")

## revise eelgrass, surfgrass categories
dat <- revise_categories(dat, c("art_CA", "crust_CA"), "coralline_algae")
## end category revision ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## renmame columns and double-check ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## print df with names to check against renamed columns
original_names <- as.data.frame(print(colnames(dat)))


## rename cols
dat <- rename_columns(dat, "SS_SU", "soft_sediment")
dat <- rename_columns(dat, "cob_SU", "cobble")
dat <- rename_columns(dat, "peb_SU", "pebble")
dat <- rename_columns(dat, "KelpBry_SI", "kelp_bryozoan")
dat <- rename_columns(dat, "UNIdent", "unknown")
dat <- rename_columns(dat, "filam_BR", "filamentous_brown")
dat <- rename_columns(dat, "FlatAci_BR", "acid_weed")
dat <- rename_columns(dat, "fucus_BR", "rock_weed")
dat <- rename_columns(dat, "holdfas_BR", "kelp_holdfast")
dat <- rename_columns(dat, "sargass_BR", "sargassum")
dat <- rename_columns(dat, "sugar_KE", "sugar_kelp")
dat <- rename_columns(dat, "shell_SU", "shell_debris")
dat <- rename_columns(dat, "ulva_GR", "green_algae")


## print revised names
revised_names <- as.data.frame(print(colnames(dat)))
combined <- cbind(original_names, revised_names)


## delete extraneous columns
dat <- remove_columns(dat, c("StriAci_BR", "undaria_BR", "unk_BR", "giant_KE", 
                             "bullBL_KE", "bullST_KE", "senes_RE", "senes_AL"))


## remove columns with very few observations
dat <- remove_columns(dat, c("rock_weed", "acid_weed", "smooth_kelp", "seagrass"))
## END CoralNet category editing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## export ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd(urban_kelp)
write.csv(dat, "2022_T1_revised_categories.csv", row.names=FALSE)
## END export ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~






## sample down to 50pts per image ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## run function - sample 50 data points without replacement
dat2 <- sample.data(dat, 50, FALSE)


## delete the SU column, no longer needed
dat2 <- dat2[,-1]


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
dat2 <- dat2 %>% select(all_of(new.order))


## as.factor for future plotting
dat2$site <- as.factor(dat2$site)
dat2$transect <- as.factor(dat2$transect)
dat2$key <- as.factor(dat2$key)


## save the data frame 
setwd(urban_kelp)
write.csv(dat2, "2022_T1_50-photos.csv", row.names=FALSE)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
