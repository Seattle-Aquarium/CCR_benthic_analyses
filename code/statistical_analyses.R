## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Data visualizations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())


## add libraries
library(tidyverse)


## set working directory to home folder
setwd("../")
getwd()


## relative file paths
code <- "code"
figs <- "figs"
label_19 <- "data_output/19_labels"
label_69 <- "data_output/69_labels"


## invoke relative file path 
dat_19 <- read.csv(file.path(label_19, "diversity_T3-2_19_labels.csv"))
dat_69 <- read.csv(file.path(label_69, "diversity_T3-2_69_natural_scale.csv"))
#spp_scores <- read.csv(file.path(label_19, "spp_scores_T3-2_19_natural_scale.csv"))


## classify as factor for color plotting
dat_19$transect <- as.factor(dat_19$transect)
dat_19$site <- as.factor(dat_19$site)
dat_19$key <- as.factor(dat_19$key)


## repeat factor for other dataset
dat_69$transect <- as.factor(dat_69$transect)
dat_69$site <- as.factor(dat_69$site)
dat_69$key <- as.factor(dat_69$key)


## graphing functions 
source(file.path(code, "visualization_functions.R"))
source(file.path(code, "NMDS_functions.R"))
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## statty things ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~