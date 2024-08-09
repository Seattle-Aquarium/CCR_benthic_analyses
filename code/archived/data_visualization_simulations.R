## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Data visualizations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())


## add libraries
library(tidyverse)
library(patchwork)

## list out current path
getwd()


## hardcode relative file paths
code <- "../code"
data_input <- "../data_input"
data_output <- "../data_output"
figs <- "../figs"


## graphing functions 
setwd(code)
source("visualization_functions.R")


## invoke relative file path 
setwd(data_output)


## read in csv file 
dat <- read.csv("combined_stipes.csv")
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## visualize bull kelp stipe and bundle data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
my.window(5,5)


## convert site and transect to factor
dat$site <- as.factor(dat$site)
dat$transect <- as.factor(dat$transect)


## plot stipes as a function of bundles
p1 <- pairwise.plot(dat, 
                    dat$bundles, 
                    dat$stipes, 
                    "bull kelp bundles", 
                    "bull kelp stipes")
print(p1)


## other pairwise plots
p3 <- pairwise.plot(dat, 
                    dat$shell_debris, 
                    dat$sugar_kelp, 
                    "shell debris avg. % of seafloor", 
                    "sugar kelp avg. % of seafloor")
print(p3)


## save figures as pdf and png, if desired
save.figs("sugar-kelp_soft-sediment.pdf", 
          "sugar-kelp_soft-sediment.png", 
          p3, 5, 5)


## plot stipes across all transects
p2 <- ggplot(data=dat, aes(key, stipes, group_by(site))) + my.theme + 
  geom_point(size=4, pch=21, color="black", aes(fill=site)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust=0)) + 
  xlab("'site_transect' throughout Elliott Bay") + ylab("2022 bull kelp stipes")
print(p2)


#library(patchwork)
windows(20,10)

plots <- map(6:23, ~ {
  ggplot(dat, aes_string(x = names(dat)[.x], y = "stipes")) +
    geom_point() + my.theme +
    geom_smooth(method = "loess", color = "red", se=FALSE) +
    theme(axis.title.y = element_blank(),  # Remove x-axis labels
          axis.text.y = element_blank(),   # Remove x-axis text
          plot.title = element_blank(),
          axis.text.x = element_blank())    # Remove plot titles
})


## view all plots
combined_plot <- wrap_plots(plots, ncol=6)
combined_plot
## END figs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
