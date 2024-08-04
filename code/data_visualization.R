## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Data visualizations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())


## add libraries
library(tidyverse)


## list out current path
getwd()


## hardcode relative file paths
code <- "../code"
data_input <- "../data_input"
data_output <- "../data_output"
figs <- "../figs"


## invoke relative file path 
setwd(data_output)


## read in csv file 
dat <- read.csv("combined_stipes.csv")
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## graphing params ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## set up custom ggplot theme 
my.theme = theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(), 
                 axis.line = element_line(colour = "black"),
                 axis.title.x=element_text(size=15),
                 axis.title.y=element_text(size=15),
                 axis.text=element_text(size=13),
                 plot.title = element_text(size=13),
                 legend.text = element_text(size=13),
                 legend.title = element_text(size=15)
                 )


my.window <- function(width, height){
  graphics.off()
  windows(width, height, record=TRUE)
}
## END graphing params ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## visualize bull kelp stipe and bundle data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
my.window(15,5)


## convert site and transect to factor
dat$site <- as.factor(dat$site)
dat$transect <- as.factor(dat$transect)


## visualize stipes ~ bundles
p1 <- ggplot(data=dat, aes(bundles, stipes)) + my.theme + 
  geom_point(size=2) + 
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  xlab("bull kelp bundles") + ylab("bull kelp stipes")

print(p1)


## plot stipes across all transects
p2 <- ggplot(data=dat, aes(key, stipes, group_by(site))) + my.theme + 
  geom_point(size=4, pch=21, color="black", aes(fill=site)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust=0)) + 
  xlab("'site_transect' throughout Elliott Bay") + ylab("2022 bull kelp stipes")

print(p2)
## END stipe figs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
