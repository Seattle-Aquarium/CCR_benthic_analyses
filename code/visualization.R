## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Data visualization ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())


## add libraries
library(tidyverse)
library(patchwork)
library(cowplot)


## set working directory to home folder
setwd("../")
getwd()


## relative file paths
code <- "code"
figs <- "figs"
label_19 <- "data_output/19_labels"
transects <- "figs/19_labels/ordinations/transects" 
sites <- "figs/19_labels/ordinations/sites"


## invoke relative file path 
dat <- read.csv(file.path(label_19, "ratios.csv"))
spp_scores <- read.csv(file.path(label_19, "spp_scores_T3-2_19_labels.csv"))


## classify as factor for color plotting
dat$transect <- as.factor(dat$transect)
dat$site <- as.factor(dat$site)
dat$key <- as.factor(dat$key)


## graphing functions 
source(file.path(code, "visualization_functions.R"))
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





## NMDS figs for Port report ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## plot NMDS w/ ellipses to extract legend 
my.window(9,9)


## extract legends 
p0 <- plot.NMDS.ellipses(dat)
print(p0)

p0.5 <- plot.NMDS.transects(dat, highlight_site = c("1"))
print(p0.5)

p0.6 <- plot.NMDS.size.legend(dat, "sugar_kelp", 0.1, 3)
print(p0.6)

p0.7 <- plot.NMDS.size.legend(dat, "textured_kelp", 0.1, 3)
print(p0.7)

## code to extract legend 
site_legend <- get_legend(p0 + theme(legend.position = "right"))
transect_legend <- get_legend(p0.5 + theme(legend.position = "right"))
sugar_kelp_legend <- get_legend(p0.6 + theme(legend.position = "right"))
textured_kelp_legend <- get_legend(p0.7 + theme(legend.position = "right"))


## print ellipses 
p1 <- plot.NMDS.ellipses.noLegend(dat)
print(p1)


## plot NMDS and overlay species scores 
p2 <- plot.NMDS.spp.scores.noLegend(dat)
print(p2)


## sugar kelp
p3 <- plot.NMDS.size.noLegend(dat, "sugar_kelp", 0.1, 3)
print(p3)


## textured kelp 
p4 <- plot.NMDS.size.noLegend(dat, "textured_kelp", 0.1, 3)
print(p4)


## plot NMDS transects 
p5 <- plot.NMDS.transects.noLegend(dat, highlight_site = c("1"))
print(p5)


## plot NMDS transects 
p6 <- plot.NMDS.transects.noLegend(dat, highlight_site = c("5"))
print(p6)
## END NMDS figs for Port report ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## modify figs for Port report ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Remove x-axis text/title from p2
p1_mod <- p1 + theme(axis.title.x = element_blank(),
                     axis.text.x = element_blank())


# Remove both x-axis and y-axis text/title from p3
p2_mod <- p2 + theme(axis.title.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.y = element_blank())

# Keep all text/titles in psug (no modifications needed)
p3_mod <- p3 + theme(axis.title.x = element_blank(),
                     axis.text.x = element_blank())

# Remove y-axis text/title from ptex
p4_mod <- p4 + theme(axis.title.y = element_blank(),
                     axis.text.y = element_blank(),
                     axis.title.x = element_blank(),
                     axis.text.x = element_blank())

p5_mod <- p5 

p6_mod <- p6 + theme(axis.title.y = element_blank(),
                     axis.text.y = element_blank())
## END modification ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## arrange and print ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Ensure all plots have appropriate margins to prevent clipping
p1_mod <- p1_mod + theme(plot.margin = margin(5, 5, 5, 5), panel.spacing = unit(0, "lines"))
p2_mod <- p2_mod + theme(plot.margin = margin(5, 5, 5, 5), panel.spacing = unit(0, "lines"))
p3_mod <- p3_mod + theme(plot.margin = margin(5, 5, 5, 5), panel.spacing = unit(0, "lines"))
p4_mod <- p4_mod + theme(plot.margin = margin(5, 5, 5, 5), panel.spacing = unit(0, "lines"))
p5_mod <- p5_mod + theme(plot.margin = margin(5, 5, 5, 5), panel.spacing = unit(0, "lines"))
p6_mod <- p6_mod + theme(plot.margin = margin(5, 5, 5, 5), panel.spacing = unit(0, "lines"))

# Arrange the 3x2 grid of NMDS plots
ordination_grid <- plot_grid(
  p1_mod, p2_mod,
  p3_mod, p4_mod,
  p5_mod, p6_mod,
  ncol = 2, nrow = 3, align = "hv",
  rel_heights = c(1, 1, 1),  
  rel_widths = c(1, 1)
)

# Create the second-row legends (sugar_kelp and textured_kelp) side-by-side
size_legends_grid <- plot_grid(
  sugar_kelp_legend, textured_kelp_legend,
  ncol = 2, align = "h",
  rel_widths = c(1, 1)  # Ensure equal width
)

# Arrange all legends into a column
legend_grid <- plot_grid(
  site_legend,       # Move Urban Kelp site legend to row 1
  size_legends_grid, # Place sugar_kelp & textured_kelp legends side-by-side in row 2
  transect_legend,   # Move transect legend to row 3
  ncol = 1, align = "v",
  rel_heights = c(1, 1, 1)  # Adjust spacing to position legends correctly
)

# Combine ordination grid with the updated legend layout
final_plot <- plot_grid(
  ordination_grid, legend_grid,
  ncol = 2, rel_widths = c(3.5, 1)  # Adjust width ratio as needed
)

# Display the final figure
my.window(36, 45)
print(final_plot)
## END Port report figure ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## plot NMDS ordinations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## open a window 
my.window(9,9)


## plot NMDS 
p1 <- plot.NMDS(dat)
print(p1)


## plot NMDS w/ size 
p1.5 <- plot.NMDS.size(dat, "soft_sediment", 1, 8)
print(p1.5)


## plot NMDS and overlay ellipses 
p1.9 <- plot.NMDS.ellipses(dat)
print(p1.9)


## plot subset of ellipses
p2.5 <- plot.select.ellipses(dat, highlight_sites = c("1", "6", "7", "8", "2", "3", "4"))
print(p2.5)
save.plot(p2.5, "NMDS_ellipse_1-6-7-8", width = 11, height = 8)


## plot and save all ellipses separately
plot.NMDS.by.site(dat, save_path = sites, width = 11, height = 8, axis_offset = 0.25)


## plot NMDS transects 
p2.75 <- plot.NMDS.transects(dat, highlight_site = c("1"))


## plot and save all transects
plot.NMDS.transects.all(dat, transects, width = 11, height = 8)


## plot NMDS and overlay species scores 
p3 <- plot.NMDS.spp.scores(dat)
print(p3)


p3 <- plot.NMDS.spp.scores.noLegend(dat)
print(p3)


## save ordination figures as pdf
setwd(figs)
save.plot(p3, "NMDS_spp_scores", width = 11, height = 8)
## END NMDS figures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## kernel density plots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## create a window to plot
my.window(12,8)


## invoke a single label, at a single site, for all transects:
print(single.category.1.site(dat, textured_kelp, site_number=6))  
depth_site_span


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





## kernel density plots of spp diversity metrics ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## create a window to plot
my.window(12,8)


## print spp diversity 
print(diversity.8.sites(dat, Simpson))
## END kernel density plots of spp diversity ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
