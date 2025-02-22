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
input <- "data_input"
label_19 <- "data_output/19_labels"


## source functions 
source(file.path(code, "species_density_functions.R"))


## read in data
#dat <- read.csv(file.path(label_19, "diversity_19_labels_VIAME.csv"))
dat <- read.csv(file.path(input, "HSIL_VIAME_2024.csv"))
Port <- read.csv(file.path(input, "Port_VIAME_2022.csv"))
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## data wrangling ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat <- rm.chars(dat, "Transect.ID", 1, "left")
dat <- rename.columns(dat, "Transect.ID", "transect")
dat <- rename.columns(dat, "Site.ID", "site")
dat <- rename.columns(dat, "Name", "img_name")
dat <- rename.site(dat)
dat <- create.key(dat)

## other df
Port <- rm.chars(Port, "Transect.ID", 1, "left")
Port <- rm.chars(Port, "Site.ID", 1, "left")
Port <- rename.columns(Port, "Transect.ID", "transect")
Port <- rename.columns(Port, "Site.ID", "site")
Port <- rename.columns(Port, "Name", "img_name")
Port <- create.key(Port)
## END data wrangling ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





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
Port <- filter.transects(Port, "key", transect_list)
## END data prep ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## calculate summary metrics including density, sum total abundance ~~~~~~~~~~~~
## calculate density -- run on percent-cover data
density_CoralNet <- calculate.density(filtered, "site", "transect", "soft_sediment", "mobile_invert", 2)


## calculate density -- run on VIAME data
VIAME_2024 <- calculate.density(dat, "site", "transect", "SS_ochre", "SP_shiner", 3)
VIAME_2022 <- calculate.density(Port, "site", "transect", "CR_kelp", "SP_kelp", 3)


## total obs per each column
total <- total.label.obs(dat, "key", "AN_large", "SS_ochre")


## calculate sum total observations per transect
sum <- sum_total_obs(filtered, "key", "AN_large", "SS_ochre")
## END summary calculations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





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
## END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END SCRIPT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
