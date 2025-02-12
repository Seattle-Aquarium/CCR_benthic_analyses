## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Data visualizations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
label_19 <- "data_output/active/19_labels"
#label_69 <- "data_output/active/69_labels"




## graphing functions 
source(file.path(code, "data_analysis_functions.R"))


## invoke relative file path 
dat <- read.csv(file.path(label_19, "T3-2_19_labels.csv"))
#dat <- read.csv(file.path(label_19, "ord_pts_T3-2_19_natural_scale.csv"))
#spp_scores <- read.csv(file.path(label_19, "spp_scores_T3-2_19.csv"))

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
log_comm <- log.transform(natural_scale_comm) 


## select log transform on a range of columns
log <- log.transform(dat, 9, 27)


## exponentiate back to the natural scale following log transform
dat <- inverse.log.transform(dat, 9, 27)
## END NMDS prep ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## perform NMDS and save output ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ord <- metaMDS(comm = natural_scale_comm, 
               distance="bray", 
               k=2, 
               min = 1000, 
               trymax=2500, 
               autotransform = F, 
               wascores = TRUE)


## save the ordination 
setwd(label_19)
save(ord, file="ord_T3-2_19_natural_scale.rda")
load(file.path(label_19, "ord_T3-2_19_natural_scale.rda"))


## visualize stress, check ordination, xy coordinates 
## open graphics window
graphics.off()
windows(6,6,record=T)


## plot
plot(ord)
stressplot(ord)
## END ordination ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## NMDS ordination coordinates and spp scores saved as data frame ~~~~~~~~~~~~~~
## save ord point into data frame for plotting, additional analyses
dat <- save.points(metadata, ord$points, natural_scale_comm)


## save species correlation coefficients as separate data.frame and csv 
spp_scores <- save.spp(ord)


## save csv files
write.csv(dat, file.path(label_19, "ord_pts_T3-2_19_natural_scale.csv"), row.names=FALSE)
write.csv(spp_scores, file.path(label_19, "spp_scores_T3-2_19_natural_scale.csv"), row.names=FALSE)
## END extract and save ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
