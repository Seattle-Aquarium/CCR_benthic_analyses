## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Data visualizations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())


## add libraries
library(tidyverse)
library(vegan)
#library(parallel)
#library(doParallel)
#library(foreach)
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
dat <- read.csv(file.path(label_19, "T3-2_19_labels.csv"))
spp_scores <- read.csv(file.path(label_19, "spp_scores_T3-2_19.csv"))

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
## END NMDS prep ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## perform NMDS and save output ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ord <- metaMDS(comm = log_comm, 
               distance="bray", 
               k=2, 
               min = 1000, 
               trymax=2500, 
               autotransform = F, 
               wascores = TRUE)


## save the ordination 
setwd(label_19)
save(ord, file="ord_T3-2_19.rda")
load(file.path(label_19, "ord_T3-2_19.rda"))


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
dat <- save.points(metadata, ord$points, log_comm)


## save species correlation coefficients as separate data.frame and csv 
spp_scores <- save.spp(ord)


## save csv files
#write.csv(dat, file.path(label_19, "ord_pts_T3-2_19.csv"), row.names=FALSE)
#write.csv(spp_scores, file.path(label_19, "spp_scores_T3-2_19.csv"), row.names=FALSE)
## END extract and save ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## plot NMDS ordinations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## open a window 
my.windows(11,11)


## plot NMDS 
p1 <- plot.NMDS(dat)
print(p1)


## plot NMDS and overlay ellipses 
p2 <- plot.NMDS.ellipses(dat)
print(p2)


## plot NMDS and overlay species scores 
p3 <- plot.NMDS.spp_scores(dat)
print(p3)


## save ordination figures as pdf
setwd(figs)
ggplot2::ggsave(filename = "NMDS_spp_scores.pdf", 
                plot = p3, 
                dpi = 1200, 
                width = 11,
                height = 11, 
                units = "in")

## END ordination plotting ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
