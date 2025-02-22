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
label_19 <- "data_output/19_labels"
label_69 <- "data_output/69_labels"


## invoke relative file path 
dat <- read.csv(file.path(label_69, "diversity_69_labels_VIAME.csv"))
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




## save csv files
write.csv(dat, file.path(label_69, "diversity_69_labels_VIAME.csv"), row.names=FALSE)
## END diversity metrics ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
