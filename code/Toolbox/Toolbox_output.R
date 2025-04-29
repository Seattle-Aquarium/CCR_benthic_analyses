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
library(grid)
library(cowplot)


## set working directory to home folder
setwd("../")
getwd()


## relative file paths
code <- "code/Toolbox"
data_input <- "data_input"
data_output <- "data_output/Toolbox"
figs <- "figs"


## source functions 
source(file.path(code, "Toolbox_output_functions.R"))


## read in data
dat <- read.csv(file.path(data_output, "CP_t6_percent-cover.csv"))
#dat <- read.csv(file.path(data_input, "percent_cover_25_images.csv"))
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
dat <- calculate.patch.bounds(dat)


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
dat <- reorder.columns(dat, new_order)


## double check bounding boxes by plotting on a single image
plot.boxes.on.image(dat, data_input)


## transform data to short form for abundance counts
short_counts <- short.form.counts(dat)


## tranform data to short form for percent-cover
short_percent <- short.form.percent(dat)


## write a .csv and save the output
write.csv(short_percent, "short_percent.csv")


## save .csv 
save.csv(short_percent, "data_output/CP_t6_percent-cover.csv")
## END data cleanup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## calculate Toolbox efficacy ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary_output <- summarize.predictions(dat)
write.csv(summary_output, "CP_t6_summary.csv")
## END Toolbox efficacy calculation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## visualizations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## plot kernel densities for all groups
plot.kernels.by.column(dat, presence_cutoff = 0.10, max_cutoff = 0.10)


## filter group to remove "manual_update" 
new_dat <- filter.out.group(dat, "machine_prediction", "manual_update")


## plot kernel densities 
plot.kernels.by.group(short_percent, "confidence", 0)


## plot a frequency histogram tallying # of annotations per label
plot.freq.hist(summary_output, 
               group = "label_name", 
               count = "total_count",
               plot_title = "2500 percent-cover data points across 25 images")
## END visualization ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
