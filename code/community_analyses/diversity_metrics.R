## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## script to calculate spp diversity metrics and save csv ~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())


## add libraries
library(tidyverse)
library(vegan)


## set working directory to home folder
setwd("../")
getwd()


## relative file paths
code <- "code"
figs <- "figs"
input <- "data_input"
label_19 <- "data_output/19_labels"
label_69 <- "data_output/69_labels"


## invoke relative file path 
dat <- read.csv(file.path(label_19, "ratios.csv"))
#dat <- read.csv(file.path(label_69, "T3-2_69_labels.csv"))
#spp_scores <- read.csv(file.path(label_19, "spp_scores_T3-2_19_natural_scale.csv"))


## classify as factor for color plotting
dat$transect <- as.factor(dat$transect)
dat$site <- as.factor(dat$site)
dat$key <- as.factor(dat$key)


## analyses functions  
source(file.path(code, "diversity_metrics_functions.R"))
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## diversity metrics ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## partition data for diversity metrics
metadata <- dat[, c(1:16)]
community <- dat[, c(15:105)]


## calculate species diversity metrics
diversity <- calculate.diversity(community)


## combine diversity and dat to create new dat
dat <- insert.df(diversity, dat, 11)


## list of cols to remove
cols_to_remove <- c("Site", "Shannon", "Simpson", "richness", "Pielou")


## thin out columns
dat <- delete.cols(dat, cols_to_remove)


## list of cols to name 
cols_to_rename <- c(
  "Species_Richness" = "richness",
  "Shannon_Index" = "Shannon",
  "Simpson_Index" = "Simpson",
  "Pielou_Evenness" = "Pielou"
)


## rename cols 
dat <- rename.cols(dat, cols_to_rename)


## add col for location
dat <- specify.location(dat)


## Compute median for "Shannon" and "evenness" per site and transect
median_results <- as.data.frame(calculate_median(dat, measure_cols = c("Shannon", "Pielou")))


## View the results
print(median_results)


## calculate variance
var <- calculate_variance(dat, measure_cols = c("Shannon"))
## END diversity calculations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## calculate ratio of taxa ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## combine relevant substrate categories
## pebble + shell_debris
dat$pebble_shell <- dat$pebble + dat$shell_debris


## substrate + CCA
dat$hard_substrate_CCA <- dat$hard_substrate + dat$coralline_algae

## taxa ratios 
## calculate kelp ratio
dat$kelp_ratio <- (dat$sugar_kelp+1) / (dat$textured_kelp+1)


## calculate flipped kelp ratio 
dat$kelp_ratio_flipped <- (dat$textured_kelp+1) / (dat$sugar_kelp+1)


## calculate substrate ratio #1 
dat$substrate_ratio <- (dat$pebble+1) / (dat$hard_substrate_CCA+1)


## calculate substrate ratio #2 
dat$substrate_ratio_2 <- (dat$pebble_shell+1) / (dat$hard_substrate_CCA+1)


## round off the decimal pts
dat <- decimal.round(dat, "kelp_ratio", 1)
dat <- decimal.round(dat, "kelp_ratio_flipped", 1)
dat <- decimal.round(dat, "substrate_ratio", 1)
dat <- decimal.round(dat, "substrate_ratio_2", 1)


## move column to front of taxa section of dataframe
dat <- move.col(dat, "kelp_ratio", 18)
dat <- move.col(dat, "kelp_ratio_flipped", 19)
dat <- move.col(dat, "substrate_ratio", 20)
dat <- move.col(dat, "substrate_ratio_2", 21)
dat <- move.col(dat, "pebble_shell", 22)
dat <- move.col(dat, "hard_substrate_CCA", 23)
## END ratio calculation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
