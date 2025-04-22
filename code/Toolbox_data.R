## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clean up and work with Toolbox data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())
options(error = NULL)


## add libraries
library(tidyverse)
library(jpeg)
library(ggplot2)
library(grid)


## set working directory to home folder
setwd("../")
getwd()


## relative file paths
code <- "code"
data_input <- "data_input"
data_output <- "data_output"
figs <- "figs"


## source functions 
source(file.path(code, "Toolbox_data_functions.R"))


## read in data
#dat <- read.csv(file.path(data_input, "Centennial_Park_t6.csv"))
dat <- read.csv(file.path(data_input, "example_percent-cover_data.csv"))
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## clean up data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## list of columns to rename
dat <- rename.columns(dat, c("Name", 
                             "Row", 
                             "Column", 
                             "Label", 
                             "Long.Label", 
                             "Patch.Size",
                             "Machine.confidence.1",
                             "Machine.suggestion.1"), 
                      c("image_name", 
                        "midpoint_row",
                        "midpoint_column",
                        "label_code",
                        "label_name",
                        "patch_size",
                        "confidence",
                        "machine_prediction"))


## list of columns to delete
cols_to_remove <- c("Machine.confidence.2", 
                    "Machine.suggestion.2",
                    "Machine.confidence.3", 
                    "Machine.suggestion.3",
                    "Machine.confidence.4", 
                    "Machine.suggestion.4",
                    "Machine.confidence.5", 
                    "Machine.suggestion.5")


## delete columns
dat <- dat[, !names(dat) %in% cols_to_remove]


## invokes function to add a "1" in col "confidence" for manually reviewed data
dat <- replace.with.1(dat, "confidence")


## invokes function to add "manual_update" to column "machine_prediction"
dat <- add.text.to.cell(dat, "machine_prediction")


## invokes function to calculate the x4 pixel corners of each image patch
dat <- calculate_patch_bounds(dat)


## new column order ordering 
new_order <- c("image_name", 
               "midpoint_row", 
               "midpoint_column", 
               "left_column", 
               "right_column", 
               "top_row", 
               "bottom_row", 
               "patch_size", 
               "label_code", 
               "label_name",
               "machine_prediction", 
               "confidence")


## Reorder columns
dat <- reorder_columns(dat, new_order)


## double check bounding boxes by plotting on a single image
plot_boxes_on_image(dat, data_input)


## write a .csv and save the output
write.csv(dat, "example_percent-cover_output.csv")
## END data cleanup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~






## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
