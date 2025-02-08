## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## CoralNet data processing and category editing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## wrangle data to key columns, calculate percent-cover, reduce to key columns ~
## 2025_02_06, zhr  
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())
options(error = NULL)


## add libraries
library(tidyverse)


## set working directory to home folder
setwd("../")
getwd()


## relative file paths
code <- "code"
data_input <- "data_input"
data_output <- "data_output"
figs <- "figs"
#urban_kelp <- "data_output/urban_kelp"
active <- "data_output/active"


## source functions 
source(file.path(code, "wrangle_and_revise_functions.R"))


dat <- read.csv(file.path(data_input, "original_CoralNet_2022_dataset.csv"))
dat <- create.SU(dat)
#dat <- read.csv(file.path(urban_kelp, "2022_revised_categories.csv"))
#dat <- read.csv(file.path(urban_kelp, "2022_all_current_photos.csv"))
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## clean up metadata ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## split off metadata and save, if desired
#metadata <- split_dataframe(dat, 27)


## Columns to preserve
cols_to_preserve <- c("SU", "key", "site", "transect", "img_name") # Columns to preserve
cols_to_remove <- c("alt_smooth", "SimDIS")


## Invoke the function to delete columns between "SU" and "Points", but preserve certain columns
dat <- delete_columns(dat, "SU", "Points", cols_to_preserve)


## specific columns to remove, if present
dat <- dat[, !names(dat) %in% cols_to_remove]
## END metadata cleanup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## invoke function to calculate percent-cover ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#dat <- calculate.percent(dat, soft_sediment, mobile_invert, 2)
dat <- calculate.percent(dat, cup_SI, surf_SG, 2)


## add total percent column at front of data.frame - check that sums to 1 
dat$sum <- rowSums(select(dat, cup_SI:surf_SG))
dat <- front.ofthe.line(dat)
## END percent-cover calculation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## reduce to key transects of interest ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
T3.1 <- dat %>% filter(transect %in% c("1", "2", "3"))
count_rows(T3.1, "key")


## sample down to 50 per transect, or retain all rows if nrow <50
T3.2 <- sample.down(T3.1, "key", 50)
count_rows(test.T3, "key")


## only retain transects with at least 50 photos. 
T3.3 <- sample.down.filtered(T3, "key", 50)
count_rows(second.test.T3, "key")


## test to make sure the sampling didn't generate any repeat rows
duplicates <- T3.3 %>% count(SU) %>% filter(n > 1)


## save relevant files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#write.csv(T3.1, file.path(active, "T3-1_69_labels.csv"), row.names=FALSE)
#write.csv(T3.2, file.path(active, "T3-2_69_labels.csv"), row.names=FALSE)
#write.csv(T3.3, file.path(active, "T3-3_69_labels.csv"), row.names=FALSE)
## END row sampling and file save ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## calculate category sums and save as separate files ~~~~~~~~~~~~~~~~~~~~~~~ ##
T3.1_totals <- annotation.sum(T3.1, "cup_SI", "surf_SG")
T3.2_totals <- annotation.sum(T3.2, "cup_SI", "surf_SG")
T3.3_totals <- annotation.sum(T3.3, "cup_SI", "surf_SG")


## save csvs 
#write.csv(T3.1_totals, file.path(active, "T3-1_69_labels_totals.csv"), row.names=FALSE)
#write.csv(T3.2_totals, file.path(active, "T3-2_69_labels_totals.csv"), row.names=FALSE)
#write.csv(T3.3_totals, file.path(active, "T3-3_69_labels_totals.csv"), row.names=FALSE)
## END category sums ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
