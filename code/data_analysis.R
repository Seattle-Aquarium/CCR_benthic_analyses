## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Data visualizations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())


## add libraries
library(tidyverse)
library(vegan)
library(parallel)
library(doParallel)
library(foreach)
#library(patchwork)


## set working directory to home folder
setwd("../")
getwd()


## relative file paths
code <- "code"
data_input <- "data_input"
data_output <- "data_output"
figs <- "figs"
active <- "data_output/active"
label_19 <- "data_output/active/19_labels"
label_69 <- "data_output/active/69_labels"


## graphing functions 
source(file.path(code, "data_analysis_functions.R"))


## invoke relative file path 
dat <- read.csv(file.path(label_19, "T3-1_19_labels.csv"))


## classify as factor for color plotting
dat$transect <- as.factor(dat$transect)
dat$site <- as.factor(dat$site)
dat$key <- as.factor(dat$key)
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## prep and run NMDS analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## partition metadata and the community matrix
metadata <- dat[, c(1:6)]
community <- dat[, c(7:25)]


## return percentages to natural scale
natural_scale_comm <- community * 100


## perform transformation if desired/required
log_comm <- log.transform(nat_scale_comm) 
## END NMDS prep ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## set up parallel processing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)
## END parallel processing set up~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## perform NMDS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Example Invocation (Specify Only the Parameters You Want)
nmds_results <- nmds_parallel(log_comm, min = 1000, trymax = 5000)

# Stop parallel cluster
stopCluster(cl)

# Select the best NMDS solution (lowest stress)
best_nmds <- nmds_results[[which.min(sapply(nmds_results, function(x) x$stress))]]

# Plot the best NMDS result
plot(best_nmds, type = "t", main = "Optimized NMDS Ordination")
## END NMDS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



ord <- metaMDS(comm = log_comm, 
               distance="bray", 
               k=2, 
               min = 1000, 
               trymax=5000, 
               autotransform = F, 
               wascores = TRUE)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
