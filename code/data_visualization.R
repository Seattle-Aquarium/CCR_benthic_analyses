## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Data visualizations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())


## add libraries
library(tidyverse)
library(patchwork)


## set working directory to home folder
setwd("../")
getwd()


## relative file paths
code <- "code"
data_input <- "data_input"
data_output <- "data_output"
figs <- "figs"
urban_kelp <- "data_output/urban_kelp"

## graphing functions 
source(file.path(code, "data_visualization_functions.R"))


## invoke relative file path 
dat <- read.csv(file.path(urban_kelp, "2022_T1_T2_T3.csv"))


## classify as factor for color plotting
dat$transect <- as.factor(dat$transect)
dat$site <- as.factor(dat$site)
dat$key <- as.factor(dat$key)
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## visualize bull kelp stipe and bundle data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
my.window(20,10)


## plot y as a function of x - simple pairwise plot
p1 <- pairwise.plot(dat, 
                    dat$soft_sediment, 
                    dat$hard_substrate, 
                    "soft sediment seafloor %", 
                    "hard substrate seafloor %")
print(p1)


p2 <- facet.site(dat, 
                 dat$shell_debris, 
                 dat$sugar_kelp, 
                 "shell debris %",
                 "sugar kelp %",
                 0.95, 0.95)
print(p2)


## save figs 
save.figs(p2, figs, 
          "shell-debris_sugar-kelp.pdf", 
          "shell-debris_sugar-kelp.png", 
          13, 6.5, 600)
## END simple pairwise plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## "n" pairwise comparisons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# List of columns to visualize
columns_to_visualize <- c("shell_debris", "hard_substrate", "sugar_kelp", "textured_kelp", "red_algae", "green_algae")


# Create a list to store the plots
plot_list <- list()


# Generate pairwise plots for the specified columns
for (i in 1:(length(columns_to_visualize) - 1)) {
  for (j in (i + 1):length(columns_to_visualize)) {
    x <- columns_to_visualize[i]
    y <- columns_to_visualize[j]
    x.lab <- x
    y.lab <- y
    p <- mult.pairwise.plot(dat, x, y, x.lab, y.lab)
    plot_list[[paste(x, y, sep = "_")]] <- p
  }
}


# Combine all the plots into a single large figure using patchwork
combined_plot <- wrap_plots(plot_list, ncol = 5)
combined_plot 


## save figures as pdf and png, if desired
save.figs(combined_plot, figs, 
          "six-pairs.pdf", "six-pairs.png",
          13, 6.5, 600)

## END "n" pairwise comparison plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




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
