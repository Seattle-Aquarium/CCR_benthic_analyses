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
ROV_output <- "results/ROV"


## read diver + ROV data
## NOTE: ROV_invert_abundance.csv doesn't exist yet -- new VIAME-derived
## abundance data is forthcoming (wrangle_ROV_abundance_data.R will need to
## be re-run once it lands); the read below will fail until then
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
