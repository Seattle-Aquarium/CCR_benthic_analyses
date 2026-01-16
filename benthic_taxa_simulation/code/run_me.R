## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## This script enables a user to pull functions from other R scripts in this 
## repo and invoke here to clean metadata, simulate benthic taxa, and visualize
## those data. Any problems running this code should be reported in the Issues
## tab of https://github.com/zhrandell/Seattle_Aquarium_benthic_taxa_simulation
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
rm(list = ls())


## read in libraries
library(tidyverse)
library(readr)
library(rtruncnorm)


## check wd is appropriate
getwd()


## hardcode relative file paths
code <- "../code"
data_input <- "../data_input"
data_output <- "../data_output"
figs <- "../figs"
## END start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## invoke functions to create, clean metadata ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## pull functions from other script
setwd(code)
source("wrangle_metadata.R")


## invoke functions to clean and append data together
dat <- wrangle.csv("metadata.csv")


## overwrite previous .csv file with trimmed file that matches our 8sec still extraction process
trimmed_dat <- every.nth.row("metadata.csv", 8, "metadata.csv")


## create a .csv with transect specific row lengths to guide simulation 
transect_rows <- nrows.transect(trimmed_dat, "nrows_transect.csv")


## create a .csv with site specific row lengths to guide simulation
site_rows <- nrows.site(trimmed_dat, "nrows_site.csv")
## END data processing and file creation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## set parameters ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## pull in csv as to provide proper row lengths for simulations
setwd(data_output)
rows <- read.csv("nrows_site.csv")


## pull in benthic taxa simulation functions
setwd(code)
source("simulate_taxa.R")


## set parameters for simulation: set a unique mean for each species 
## across each of the eight sites. 


## total number of rows
n <- sum(rows[2])


## row_sum - total number of points per simulated image
row_sum <- 500


## lower bound
a <- 0


## red algae 
mean_1_S1 <- 40
mean_1_S2 <- 40
mean_1_S3 <- 40
mean_1_S4 <- 40
mean_1_S5 <- 5
mean_1_S6 <- 5
mean_1_S7 <- 5
mean_1_S8 <- 5


## sugar kelp
mean_2_S1 <- 0
mean_2_S2 <- 7
mean_2_S3 <- 14
mean_2_S4 <- 21
mean_2_S5 <- 28
mean_2_S6 <- 35
mean_2_S7 <- 42
mean_2_S8 <- 45

## green algae, ulva
mean_3_S1 <- 0
mean_3_S2 <- 5
mean_3_S3 <- 15
mean_3_S4 <- 35
mean_3_S5 <- 0
mean_3_S6 <- 5
mean_3_S7 <- 15
mean_3_S8 <- 35

## soft sediment
mean_4_S1 <- 40
mean_4_S2 <- 5
mean_4_S3 <- 40
mean_4_S4 <- 5
mean_4_S5 <- 40
mean_4_S6 <- 5
mean_4_S7 <- 40
mean_4_S8 <- 5

## rock
mean_5_S1 <- 0   
mean_5_S2 <- 10
mean_5_S3 <- 20
mean_5_S4 <- 40
mean_5_S5 <- 30
mean_5_S6 <- 20
mean_5_S7 <- 10
mean_5_S8 <- 0

## bedrock, hard substrate
mean_6_S1 <- 7.14  
mean_6_S2 <- 7.14
mean_6_S3 <- 7.14
mean_6_S4 <- 7.14
mean_6_S5 <- 7.14
mean_6_S6 <- 7.14
mean_6_S7 <- 7.14
mean_6_S8 <- 7.14

## variance parameters 
i1_sd <- 1
i2_sd <- 1
i3_sd <- 1
i4_sd <- 1
i5_sd <- 1
i6_sd <- 1


## simulate abundances - bull kelp
lam_1_S1 <- 7.14
lam_1_S2 <- 7.14
lam_1_S3 <- 7.14
lam_1_S4 <- 7.14
lam_1_S5 <- 7.14
lam_1_S6 <- 7.14
lam_1_S7 <- 7.14
lam_1_S8 <- 7.14


## simulate abundances - kelp crabs
lam_2_S1 <- 7.14
lam_2_S2 <- 7.14
lam_2_S3 <- 7.14
lam_2_S4 <- 7.14
lam_2_S5 <- 7.14
lam_2_S6 <- 7.14
lam_2_S7 <- 7.14
lam_2_S8 <- 7.14
## END parameters ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## invoke functions to simulate data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## simulate important variables
df.1 <- simulate.CoralNet(n, row_sum)


## simulate unimportant variables and bind together data
percent_cover <- simulate.remainder(row_sum, c("remainder"))


## check row sums
percent_cover$row_sum <- rowSums(percent_cover)


## simulate 1st species 
abundances <- simulate.abundances(n)


## bind together percent-coverage, abundances, and metadata
dat <- bind.data(percent_cover, abundances, "metadata.csv")


## save data
setwd(data_output)
write.csv(dat, "interpolation_testing_data.csv", row.names = FALSE)
## END simulations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## invoke functions to prep data for visualization ~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
setwd(code)
source("graphing_functions.R")


## read in data file
setwd(data_output)
dat <- read.csv("example_output.csv")


## filter to site level and melt for plotting
sites <- filter.site(dat, c("1"), c("red_algae", "sugar_kelp", "green_algae", "soft_sediment", 
                                    "small_rocks", "boulder_bedrock", "remainder", "SU"))


## filter to transect level and melt for plotting
transects <- filter.transect(dat, c("1"), c("1_1"), c("red_algae", "sugar_kelp", "green_algae", "soft_sediment", 
                                                      "small_rocks", "boulder_bedrock", "remainder", "SU"))
## END data prep for plotting ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## plot data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## set custom graphing window
my.window(10, 8)


## plot one category across all sites
p1 <- plot.all.sites(dat, 0.75, "Simulated images", "Sugar kelp percent-cover per image")


## plot all categories at a single transect
p1 <- plot.transect(1.75, "Simulated images", "Percent-cover points")
## END data visualization ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
