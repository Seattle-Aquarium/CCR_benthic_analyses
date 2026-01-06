## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## wrangle data for ROV-diver comparison ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
ROV_input <- "data_input/ROV"
ROV_output <- "data_output/ROV"
code <- "code"


## source functions 
source(file.path(code, "wrangle_data_functions.R"))


## read ROV data
ROV_percent_cover_t4 <- read.csv(file.path(ROV_input, "short_percent_t4.csv"))
ROV_percent_cover_t6 <- read.csv(file.path(ROV_input, "short_percent_t6.csv"))
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## prep data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
t4 <- consistent.labels(ROV_percent_cover_t4)
t6 <- consistent.labels(ROV_percent_cover_t6)


## stack multiple dfs together; fill missing columns w/ 0's, and save 
dat <- stack.dfs(t6, t4)


## combine red algae categories
dat <- combine.cols(dat, c("red_algae_branching", 
                                  "red_algae_filamentous",
                                  "red_algae_flat_leaf",
                                  "red_algae_bushy"), 
                    "combined_red_algae")


## summarize data: transect average
avg_dat <- average.by.site.transect(dat, "boulder", "combined_red_algae")
## END data processing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## save the new df ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
save.csv(dat, ROV_output, "ROV_percent-cover_data.csv")
save.csv(avg_dat, ROV_output, "ROV_percent-cover_data_averaged.csv")
## END the save ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
