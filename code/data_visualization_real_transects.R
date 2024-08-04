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


## graphing functions 
setwd(code)
source("visualization_functions.R")


## invoke relative file path 
setwd(data_output)
dat <- read.csv("2022_T1_50pts.csv")
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## visualize bull kelp stipe and bundle data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
my.window(5,5)


## plot y as a function of x - simple pairwise plot
p1 <- pairwise.plot(dat, 
                    dat$soft_sediment, 
                    dat$red_algae, 
                    "soft sediment seafloor %", 
                    "red algae seafloor %")
print(p1)
## END simple pairwise plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## "n" pairwise comparisons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# List of columns to visualize
columns_to_visualize <- c("red_algae", "green_algae", "sugar_kelp", "soft_sediment")


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
combined_plot <- wrap_plots(plot_list, ncol = 4)
combined_plot 


## save figures as pdf and png, if desired
save.figs("sugar-kelp_soft-sediment.pdf", 
          "sugar-kelp_soft-sediment.png", 
          p3, 5, 5)

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
