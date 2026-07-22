## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## analyze / visualize data for ROV-diver comparison ~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
## NOTE: ROV_invert_abundance.csv doesn't exist yet -- new VIAME-derived
## abundance data is forthcoming (wrangle_ROV_abundance_data.R will need to
## be re-run once it lands); the lines below will fail until then
ROV_abundance <- read.csv(file.path(ROV_output, "ROV_invert_abundance.csv"))
ROV_percent_cover_averaged <- read.csv(file.path(ROV_output, "HSIL_percent-cover_transect-averaged.csv"))
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


## visualize percent-cover
## NOTE: x_axis/y_axis are paired positionally (row i of one against row i of
## the other), not joined by site/transect/season key -- confirm the two
## dataframes' rows are in matching order before trusting this plot
#visualize.percent.cover.pairs(x_axis = ROV_percent_cover_averaged,
#                              y_axis = diver_UPC_percentage,
#                              colnames = c("combined_red_algae",
#                                           "combined_green_algae"),
#                              colors = c("darkred", "darkgreen"),
#                              axis_limit = 1)
## END basic visualizations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



