## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Data visualizations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())


## add libraries
library(tidyverse)


## set working directory to home folder
setwd("../")
getwd()


## relative file paths
code <- "code"
figs <- "figs"
label_19 <- "data_output/active/19_labels"
#label_69 <- "data_output/active/69_labels"


## invoke relative file path 
dat <- read.csv(file.path(label_19, "ord_pts_T3-2_19_natural_scale.csv"))
spp_scores <- read.csv(file.path(label_19, "spp_scores_T3-2_19_natural_scale.csv"))

## classify as factor for color plotting
dat$transect <- as.factor(dat$transect)
dat$site <- as.factor(dat$site)
dat$key <- as.factor(dat$key)


## graphing functions 
source(file.path(code, "visualization_functions.R"))
source(file.path(code, "NMDS_functions.R"))
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## data transformations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## return percentages to natural scale by multiplying by 100
mult <- multiply.100(dat, 9, 27)


## calculate percentages
divide <- divide.100(dat, 9, 27)


## select log transform on a range of columns
log <- log.transform(dat, 9, 27)


## exponentiate back to the natural scale following log transform
test <- inverse.log.transform(log, 9, 27)
## END transformations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## plot NMDS ordinations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd(label_19)
dat <- read.csv(file.path(label_19, "ord_pts_T3-2_19.csv"))



## open a window 
my.window(13,11)


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
save_plot(p1, "NMDS_ordination", width = 11, height = 8)



ggplot2::ggsave(filename = "NMDS_ordination.pdf", 
                plot = p1, 
                dpi = 1200, 
                width = 11,
                height = 8, 
                units = "in")

## END ordination plotting ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## kernel density plots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## invoke a single label, at a single site, for all transects:
my.window(8,8)
print(single.kernel(log, textured_kelp, site_number=6))  


## invoke a single label at all x8 sites, for all transects: 
my.window(12, 8)
print(all.sites.kernel(dat, textured_kelp))  


## print a single label at all x8 sites, for all labels, and save the pdfs
save_all_categories_kernel(log, labels, width = 12, height = 8)




## prep to print all labels for a single site 
my.window(15, 20)
dat <- dat %>% select(-kelp_holdfast)


## print all labels for x1 site
print(plot_site_density(dat, site_number=2))


## print all labels for x1 site, for all sites, and save all pdfs
save_all_sites_density(log, width = 15, height = 20)





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
