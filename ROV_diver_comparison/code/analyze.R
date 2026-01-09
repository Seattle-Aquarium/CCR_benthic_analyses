## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## analyze / visualize data for ROV-diver comparison ~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())


## add libraries
library(tidyverse)
library(stringr)
library(MASS)
library(glmmTMB)

## set working directory one level up and verify 
setwd("../")
getwd()


## relative file paths
diver_results <- "results/diver"
ROV_results <- "results/ROV"
results <- "results"
code <- "code"
figs <- "figs"


## source functions 
source(file.path(code, "analyze_functions.R"))


## read diver data
diver_algae_abundance <- read.csv(file.path(diver_output, "diver_algae_abundance.csv"))
diver_algae_density <- read.csv(file.path(diver_output, "diver_algae_density.csv"))
diver_invert_abundance <- read.csv(file.path(diver_output, "diver_invert_abundance.csv"))
diver_invert_density <- read.csv(file.path(diver_output, "diver_invert_density.csv"))
diver_UPC_percentage <- read.csv(file.path(diver_output, "diver_UPC_percentage.csv"))
  

## read ROV data
ROV_abundance <- read.csv(file.path(ROV_output, "ROV_invert_abundance.csv"))
ROV_percent_cover_averaged <- read.csv(file.path(ROV_output, "ROV_percent-cover_averaged.csv"))
ROV_percent_cover <- read.csv(file.path(ROV_output, "ROV_percent-cover.csv"))


## read combined ROV-diver data
inv_abund <- read.csv(file.path(results, "invert_abundances.csv"))
  ## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## basic visualizations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## visualize abundances
visualize.abundance.pairs(x_axis = ROV_abundance,
                          y_axis = diver_invert_abundance, 
                          colname = "cancer_crab",
                          axis_limit = 8)
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


## need to add more ROV-derived percent-cover before we can visualize these
## visualize percent-cover 
#visualize.percent.cover.pairs(x_axis = ROV_percent_cover_averaged,
#                              y_axis = diver_UPC_percentage,
#                              colnames = c("combined_red_algae",
#                                           "combined_green_algae"),
#                              colors = c("darkred", "darkgreen"),
#                              axis_limit = 1)
## END basic visualizations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
zinb_model <- glmmTMB(ochre_mottled_star ~ site + observer + depth + (1|site:transect), 
                      ziformula = ~ 1, 
                      family = nbinom2, 
                      data = inv_abund)

summary(zinb_model)



zinb_model_2 <- glmmTMB(cancer_crab ~ site + observer + depth + (1|site:transect), 
                      ziformula = ~ 1, 
                      family = nbinom2, 
                      data = inv_abund)

summary(zinb_model_2)
## simple analyses of abundances ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
