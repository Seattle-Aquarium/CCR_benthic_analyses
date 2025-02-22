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
label_69 <- "data_output/69_labels"
label_19 <- "data_output/19_labels"

## source functions 
source(file.path(code, "wrangle_data_functions.R"))


dat <- read.csv(file.path(data_input, "original_CoralNet_2022_dataset.csv"))
dat <- read.csv(file.path(label_19, "diversity_19_labels_VIAME.csv"))
dat <- create.SU(dat)
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## clean up metadata ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## split off metadata and save, if desired
#metadata <- split_dataframe(dat, 27)


## Columns to preserve
cols_to_preserve <- c("SU", 
                      "key", 
                      "site", 
                      "transect", 
                      "depth", 
                      "avg_dist", 
                      "sim_x", 
                      "sim_y", 
                      "img_name")

cols_to_remove <- c("alt_smooth", 
                    "SimDIS")


## Invoke the function to delete columns between "SU" and "Points", but preserve certain columns
dat <- delete.columns(dat, "SU", "Points", cols_to_preserve)


## specific columns to remove, if present
dat <- dat[, !names(dat) %in% cols_to_remove]


## calculate range of depth spans
dat <- calculate.depth.spans(dat)


## list of column names to change 
old.names <- c("avg_dist", "sim_x", "sim_y")
new.names <- c("altitude", "GPS_x", "GPS_y")
dat <- rename.columns(dat, old.names, new.names)
## END metadata cleanup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## invoke function to calculate percent-cover ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#dat <- calculate.percent(dat, soft_sediment, mobile_invert, 2)
dat <- calculate.percent(dat, cup_SI, surf_SG, 2)


## add total percent column at front of data.frame - check that sums to 1 
dat$sum <- rowSums(select(dat, cup_SI:surf_SG))
dat <- front.ofthe.line(dat)


## list of cols to re-order
cols.list <- c("SU", 
               "key",
               "site", 
               "transect", 
               "depth", 
               "depth_site_span", 
               "depth_transect_span",
               "altitude",
               "GPS_x",
               "GPS_y",
               "img_name")


## reorder columns of metadata
dat <- reorder.cols(dat, cols.list)
## END percent-cover calculation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## reduce to key transects of interest ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
T3.1 <- dat %>% filter(transect %in% c("1", "2", "3"))
count.rows(T3.1, "key")


## sample down to 50 per transect, or retain all rows if nrow <50
T3.2 <- sample.down(T3.1, "key", 50)
count.rows(T3.2, "key")


## only retain transects with at least 50 photos. 
T3.3 <- sample.down.filtered(T3.2, "key", 50)
count.rows(T3.3, "key")


## test to make sure the sampling didn't generate any repeat rows
duplicates <- T3.2 %>% count(SU) %>% filter(n > 1)


## save relevant files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write.csv(T3.1, file.path(label_69, "T3-1_69_labels.csv"), row.names=FALSE)
write.csv(T3.2, file.path(label_69, "T3-2_69_labels.csv"), row.names=FALSE)
write.csv(T3.3, file.path(label_69, "T3-3_69_labels.csv"), row.names=FALSE)
## END row sampling and file save ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## calculate category sums and save as separate files ~~~~~~~~~~~~~~~~~~~~~~~ ##
T3.1_totals <- annotation.sum(T3.1, "cup_SI", "surf_SG")
T3.2_totals <- annotation.sum(T3.2, "cup_SI", "surf_SG")
T3.3_totals <- annotation.sum(T3.3, "cup_SI", "surf_SG")


## save csvs 
write.csv(T3.1_totals, file.path(label_69, "T3-1_69_labels_totals.csv"), row.names=FALSE)
write.csv(T3.2_totals, file.path(label_69, "T3-2_69_labels_totals.csv"), row.names=FALSE)
write.csv(T3.3_totals, file.path(label_69, "T3-3_69_labels_totals.csv"), row.names=FALSE)
## END category sums ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
