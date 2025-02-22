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
figs <- "figs"
#label_69 <- "data_output/69_labels"
label_19 <- "data_output/19_labels"


## source functions 
source(file.path(code, "species_density_functions.R"))


## read in data
dat <- read.csv(file.path(label_19, "diversity_19_labels_VIAME.csv"))
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## data prep ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## rarify data, if necessary
dat <- drop.rows(dat, 2)


## list of transects to retain
transect_list <- c(
  "2_1", 
  "2_2",
  "3_1",
  "3_2",
  "3_3",
  "4_1",
  "4_2",
  "5_1",
  "5_2",
  "5_3",
  "6_2",
  "6_3"
)


## output with filtered transects
filtered <- filter.transects(dat, "key", transect_list)
## END data prep ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## calculate summary metrics including density, sum total abundance ~~~~~~~~~~~~
## calculate density -- run on percent-cover data
density_CoralNet <- calculate.density(filtered, "site", "transect", "soft_sediment", "mobile_invert", 2)


## calculate density -- run on VIAME data
density_VIAME <- calculate.density(dat, "site", "transect", "AN_large", "SS_ochre", 3)


## total obs per each column
total <- total.label.obs(dat, "key", "AN_large", "SS_ochre")


## calculate sum total observations per transect
sum <- sum_total_obs(filtered, "key", "AN_large", "SS_ochre")
## END summary calculations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END SCRIPT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
