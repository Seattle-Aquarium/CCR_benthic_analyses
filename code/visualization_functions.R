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
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## graphing params and functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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


## create a custom theme for n pairwise comparisons
mult.theme = theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   axis.title.x=element_text(size=8),
                   axis.title.y=element_text(size=8),
                   axis.text=element_text(size=7),
                   plot.title = element_text(size=4),
                   legend.text = element_text(size=5),
                   legend.title = element_text(size=5)
                   )


## function to create custom graphing window
my.window <- function(width, height){
  graphics.off()
  windows(width, height, record=TRUE)
}


## function for pairwise visualization
pairwise.plot <- function(data, x, y, x.lab, y.lab){
  fig <- ggplot(data=data, aes(x, y)) + my.theme + coord_fixed() + 
    geom_point(size=1.0, aes(alpha=0.5)) +
    geom_smooth(method = "loess", se = FALSE, color = "red") +
    xlab(x.lab) + ylab(y.lab) + xlim(0,100) + ylim(0,100) +
    theme(legend.position = "none")
  return(fig)
}


## function for modified pairwise plot for "n" pairwise comparisons
mult.pairwise.plot <- function(data, x, y, x.lab, y.lab) {
  ggplot(data, aes_string(x = x, y = y)) + 
    mult.theme + coord_fixed() + 
    geom_point(size = 1, alpha = 0.5, aes(color=site)) +
    #geom_smooth(method = "loess", se = FALSE, color = "red") +
    xlab(x.lab) + ylab(y.lab) + xlim(0, 100) + ylim(0, 100) +
    theme(legend.position = "none")
}


## function to save both pdf and png files of each fig 
save.figs <- function(pdf, png, plot.lab, width.x, height.y){
  setwd(figs)
  ggsave(pdf, plot = plot.lab, width = width.x, height = height.y)
  ggsave(png, plot = plot.lab, width = width.x, height = height.y)
}
## END graphing params & graphing functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
