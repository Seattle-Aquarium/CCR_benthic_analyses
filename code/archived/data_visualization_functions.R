## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Data visualizations functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## graphing params and functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## set up custom ggplot theme 
my.theme = theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(), 
                 axis.line = element_line(colour = "black"),
                 axis.title.x=element_text(size=18),
                 axis.title.y=element_text(size=18),
                 axis.text=element_text(size=14),
                 plot.title = element_text(size=13),
                 legend.text = element_text(size=15),
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

## Define custom labels for the facets
site_labels <- c(
  "1" = "Site 1: Magnolia",
  "2" = "Site 2: EBM west",
  "3" = "Site 3: EBM center",
  "4" = "Site 4: EBM east",
  "5" = "Site 5: Grain Elevator",
  "6" = "Site 6: Sirens of Spring",
  "7" = "Site 7: Pocket Beach",
  "8" = "Site 8: Coast Guard Stn."
)


## transect colors
cols <- scale_color_manual(values=c("#B22222", "#009ACD", "#000000"))
fills <- scale_fill_manual(values=c("#B22222", "#009ACD", "#000000"))




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
    xlab(x.lab) + ylab(y.lab) + xlim(0,1) + ylim(0,1) +
    theme(legend.position = "none")
  return(fig)
}



facet.site <- function(data, x, y, x.lab, y.lab, fig.pos.x, fig.pos.y){
 
   fig <- ggplot(data=data, aes(x, y)) + my.theme +  #coord_fixed() +
    geom_point(size=1.75, alpha=0.65, aes(color=transect)) + cols +
    facet_wrap(~site, labeller = labeller(site = site_labels), ncol=4) +
    theme(strip.text = element_text(size = 14)) +
    xlab(x.lab) + ylab(y.lab) + #xlim(0,1) + ylim(0,1) +
    theme(legend.position = c(fig.pos.x, fig.pos.y), 
          legend.justification = c(1, 1)) + 
    theme(panel.spacing = unit(1, "lines")) +
     
     scale_x_continuous(limits = c(0, 1),  # Set limits here
                        breaks = c(0, 0.25, 0.5, 0.75, 1),  
                        labels = c(0, 0.25, 0.5, 0.75, 1),
                        expand = c(.015, .015)) +  # Remove padding to ensure equal aspect ratio
     scale_y_continuous(limits = c(0, 1),  # Set limits here
                        breaks = c(0, 0.25, 0.5, 0.75, 1),  
                        labels = c(0, 0.25, 0.5, 0.75, 1),
                        expand = c(.015, .015))   # Remove padding to ensure equal aspect ratio
    
  return(fig)
}





## function for modified pairwise plot for "n" pairwise comparisons
mult.pairwise.plot <- function(data, x, y, x.lab, y.lab) {
  ggplot(data, aes_string(x = x, y = y)) + 
    mult.theme + coord_fixed() + 
    geom_point(size = 1, alpha = 0.5)+#, aes(color=site)) +
    #geom_smooth(method = "loess", se = FALSE, color = "red") +
    xlab(x.lab) + ylab(y.lab) + xlim(0, 1) + ylim(0, 1) +
    theme(legend.position = "none")
}



## function to save both pdf and png files of each fig 
save.figs <- function(pdf, png, plot.lab, width.x, height.y){
  setwd(figs)
  ggsave(pdf, plot = plot.lab, width = width.x, height = height.y)
  ggsave(png, plot = plot.lab, width = width.x, height = height.y)
}


## sample down to 50 rows (or n rows)
sample.data <- function(data, nrows, replace.logic){
  new.dat <- data %>%
    group_by(key) %>%
    sample_n(size = nrows, replace = replace.logic) %>%
    ungroup()
  return(new.dat)
}


## function to save figs
save.figs <- function(pX, path, title.pdf, title.png, width.x, height.y, png.dpi){
  ggsave(file.path(path, title.pdf), plot = pX, device = "pdf", width = width.x, height = height.y)
  ggsave(file.path(path, title.png), plot = pX, device = "png", width = width.x, height = height.y, dpi = png.dpi)
}
## END graphing params & graphing functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
