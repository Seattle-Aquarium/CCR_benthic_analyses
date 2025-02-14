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
label_19 <- "data_output/19_labels"
label_69 <- "data_output/69_labels"


## invoke relative file path 
dat <- read.csv(file.path(label_19, "diversity_T3-2_19_natural_scale.csv"))
dat <- read.csv(file.path(label_69, "diversity_T3-2_69_natural_scale.csv"))
dat <- dat %>% select(-kelp_holdfast)
spp_scores <- read.csv(file.path(label_19, "spp_scores_T3-2_19_natural_scale.csv"))


## classify as factor for color plotting
dat$transect <- as.factor(dat$transect)
dat$site <- as.factor(dat$site)
dat$key <- as.factor(dat$key)


## graphing functions 
source(file.path(code, "visualization_functions.R"))
#source(file.path(code, "NMDS_functions.R"))
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## data transformations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## return percentages to natural scale by multiplying by 100
mult <- multiply.100(dat, 9, 27)


## calculate percentages
divide <- divide.100(dat, 9, 27)


## select log transform on a range of columns
log <- log.transform(dat, 9, 26)


## exponentiate back to the natural scale following log transform
test <- inverse.log.transform(log, 9, 27)
## END transformations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## plot NMDS ordinations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## open a window 
my.window(13,11)


## plot NMDS 
p1 <- plot.NMDS(dat)
print(p1)


## plot NMDS and overlay ellipses 
p2 <- plot.NMDS.ellipses(dat)
print(p2)


## plot NMDS and overlay species scores 
p3 <- plot.NMDS.spp.scores(dat)
print(p3)


## save ordination figures as pdf
setwd(figs)
save.plot(p1, "NMDS_ordination", width = 11, height = 8)
## END ordination plotting ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## kernel density plots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## create a window to plot
my.window(12,8)


## invoke a single label, at a single site, for all transects:
print(single.category.1.site(dat, textured_kelp, site_number=6))  


## invoke a single label at all x8 sites, for all transects: 
print(single.category.8.sites(dat, textured_kelp))  


## print a single label at all x8 sites, for all labels, and save the pdfs
getwd()
setwd("../")
all.sites(dat, labels, save_path = "figs", width = 12, height = 8)


## prep to print all labels for a single site 
my.window(12, 16)


## print all labels for x1 site
print(all.categories.1.site(dat, site_number=4))


## save pdf, png for all categories across each site 
getwd()
setwd("../")
all.categories(log, save_path = "figs", width = 9, height = 13)
## END kernel density plots of categories ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## kernel density plots of spp diversity metrics ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## create a window to plot
my.window(12,8)


## print spp diversity 
print(diversity.8.sites(dat, Shannon))


## END kernel density plots of spp diversity ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
