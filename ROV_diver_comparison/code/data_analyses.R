## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## formal statistical analyses for ROV-diver comparison ~~~~~~~~~~~~~~~~~~~~~~~~
## (plots/figures live in data_visualization.R instead) ~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())


## add libraries
library(tidyverse)
library(stringr)


## set working directory one level up and verify
setwd("../")
getwd()


## relative file paths
diver_output <- "results/diver"
ROV_output <- "results/ROV/abundance"


## read diver + ROV data
## NOTE (updated 2026-07-30): the VIAME-derived abundance data mentioned below
## as "forthcoming" has landed, but under a different name/shape than this
## script expects -- results/ROV/abundance/HSIL_viame_abundance_corrected_
## summed.csv (24 rows, one per site/transect/season/depth, full descriptive
## species column names -- see build_HSIL_viame_abundance_corrected.R), not
## ROV_invert_abundance.csv (which was never produced -- wrangle_ROV_
## abundance_data.R's own source file, data/ROV/ROV_VIAME_abundance_data.csv,
## doesn't exist either). This needs to be rewired to the new file/columns
## rather than just a path fix -- flagging rather than guessing at intent.
diver_invert_abundance <- read_csv(file.path(diver_output, "diver_invert_abundance.csv"))
ROV_abundance <- read_csv(file.path(ROV_output, "ROV_invert_abundance.csv"))
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## ROV vs. diver abundance ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c1 <- diver_invert_abundance %>%
  mutate(type = 'diver') %>%
  select(site, transect, kelp_crab, type)
c2 <- ROV_abundance %>%
  mutate(type = 'ROV') %>%
  select(site, transect, kelp_crab, type)
datm <- rbind(c1, c2)

crabmod <- glm(kelp_crab ~ type + site, data = datm,
               family = poisson(link = "log"))
summary(crabmod)
exp(coefficients(crabmod))
# To-do: add depth, check nb, overdisp?
## END ROV vs. diver abundance ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
