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
annotations_sum <- "../data_output/annotations_summary"


## source functions - remove unnecessary functions
setwd(code)
source("revise_categories_functions.R")
remove(create.key, create.SU, remove_characters, split_dataframe)


## read in csv file 
setwd(data_input)
dat <- read.csv("annotations.csv")
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## calculate sum total annotations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## delete x, y data from photo as it's not needed for these summary data
dat <- dat[, -c(2,3)]


## summarize data
image.sum <- dat %>%
  group_by(name, label) %>%
  summarise(count = n()) %>%
  spread(key = label, value = count, fill = 0)


## Calculate the sum of each column from 2 to 54
column_sums <- as.data.frame(t(apply(image.sum[, 2:54], 2, sum)))
rownames(column_sums)[1] <- "total annotations"


## order data from greatest to least
ordered <- column_sums[, order(-as.numeric(column_sums[1, ]))]


## calculate column with 
ordered$total_annotations <- sum(ordered[1, ])
ordered_cols <- front.ofthe.line(ordered)
remove(ordered, image.sum, column_sums)


## save as list for pasting elsewhere (e.g., Git)
ordered_list <- as.data.frame(t(ordered_cols))


## save raw data
#setwd(annotations_sum)
#write.csv(ordered_cols, "sum_of_all_categories_by_column.csv")
#write.csv(ordered_list, "sum_of_all_categories_by_list.csv")
## END sum calculation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## revise categories ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
revised_cols <- ordered_cols

## smooth brown algae categories
revised_cols <- revise_categories(revised_cols, c("coland_KE", "5rib_KE", "cabb_KE"), "textured_kelp")

## smooth brown algae categories
revised_cols <- revise_categories(revised_cols, c("broad_KE"), "smooth_kelp")

## other brown kelp categories
revised_cols <- revise_categories(revised_cols, c("cord_KE", "stalk_KE", "ribbn_KE"),"other_brown_kelp")

## revise red algae categories
revised_cols <- revise_categories(revised_cols, c("filam_RE", "bushy_RE", "branch_RE", "leaf_RE", "unk_RE"), "red_algae")

## revise shell categories
revised_cols <- revise_categories(revised_cols, c("shell_SU", "lgshell_SU"), "Shell_SU")

## revise hard substrate categories
revised_cols <- revise_categories(revised_cols, c("reef_SU", "bould_SU", "concr_SU"), "hard_substrate")

## revise hard substrate categories
revised_cols <- revise_categories(revised_cols, c("metal_SU", "wood_SU", "anth_SU"), "anthro_substrate")

## revise eelgrass, surfgrass categories
revised_cols <- revise_categories(revised_cols, c("eel_SG", "surf_SG"), "seagrass")

## revise sessile invert categories
revised_cols <- revise_categories(revised_cols, c("HydBry_SI", "CucEmb_SI", "scallop_SI", "SponSol_SI", "anem_SI", "encru_SI"), "sessile_invert")

## revise mobile invert categories
revised_cols <- revise_categories(revised_cols, c("CaCuc_MS", "crab_MS", "gastro_MS", "fish_MS", "SStar_MS"), "mobile_invert")

## revise eelgrass, surfgrass categories
revised_cols <- revise_categories(revised_cols, c("art_CA", "crust_CA"), "coralline_algae")
## end column combination ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## sanity check on column combination ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ordered_cols$leaf_RE + ordered_cols$filam_RE + ordered_cols$branch_RE + ordered_cols$bushy_RE + ordered_cols$unk_RE
revised_cols$red_algae
## END sanity check on column combination ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## rename columns and double check ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## print df with names to check against renamed columns
original_names <- as.data.frame(print(colnames(revised_cols)))


## rename columns
revised_cols <- rename_columns(revised_cols, "SS_SU", "soft_sediment")
revised_cols <- rename_columns(revised_cols, "cob_SU", "cobble")
revised_cols <- rename_columns(revised_cols, "peb_SU", "pebble")
revised_cols <- rename_columns(revised_cols, "KelpBry_SI", "kelp_bryozoan")
revised_cols <- rename_columns(revised_cols, "UNIdent", "unknown")
revised_cols <- rename_columns(revised_cols, "filam_BR", "filamentous_brown")
revised_cols <- rename_columns(revised_cols, "holdfas_BR", "kelp_holdfast")
revised_cols <- rename_columns(revised_cols, "sargass_BR", "sargassum")
revised_cols <- rename_columns(revised_cols, "sugar_KE", "sugar_kelp")
revised_cols <- rename_columns(revised_cols, "Shell_SU", "shell_debris")
revised_cols <- rename_columns(revised_cols, "ulva_GR", "green_algae")


## print revised names
revised_names <- as.data.frame(print(colnames(revised_cols)))
combined <- cbind(original_names, revised_names)

      
## delete extraneous columns
revised_cols <- remove_columns(revised_cols, c("StriAci_BR", 
                                               "undaria_BR", 
                                               "unk_BR", 
                                               "bullBL_KE", 
                                               "bullST_KE", 
                                               "senes_RE", 
                                               "senes_AL", 
                                               "fucus_BR", 
                                               "total_annotations",
                                               "seagrass"))
## END CoralNet category editing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~






## order and export ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## order data from greatest to least
revised_cols <- revised_cols[, order(-as.numeric(revised_cols[1, ]))]


## calculate column with 
revised_cols$total_annotations <- sum(revised_cols[1, ])
revised_cols <- front.ofthe.line(revised_cols)


## save as list for pasting elsewhere (e.g., Git)
revised_list <- as.data.frame(t(revised_cols))


## save raw data
#setwd(annotations_sum)
#write.csv(revised_cols, "sum_revised_categories_by_column.csv")
#write.csv(revised_list, "sum_revised_categories_by_list.csv")
## END ordering and export ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
