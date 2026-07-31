## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## visualize photo-level NMDS ordination ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## (the ordination itself is run in NMDS.R instead) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())


## add libraries
library(tidyverse)


## set working directory one level up and verify
setwd("../")
getwd()


## relative file paths
code <- "code"
NMDS_output <- "results/ROV/NMDS"
figs <- "figs"


## source functions
source(file.path(code, "NMDS_visualization_functions.R"))


## read the ordination coordinates (metadata + community matrix + MDS1/MDS2,
## one row per photo) and the percent-cover category correlation scores,
## both saved by NMDS.R
dat <- read_csv(file.path(NMDS_output, "NMDS_ord_pts_photo-level.csv")) %>%
  prep.nmds.data()
spp_scores <- read_csv(file.path(NMDS_output, "NMDS_spp_scores_photo-level.csv"))


dir.create(file.path(figs, "NMDS"), showWarnings = FALSE, recursive = TRUE)
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## 1. site only: Centennial Park vs. Elliott Bay Marina ~~~~~~~~~~~~~~~~~~~~~~~~
## annotated with the most populous/ecologically relevant categories (a
## curated subset of the full 30 -- see NMDS_category_scores.png for all of
## them); faded points let the black arrows/labels stand out, and the legend
## is tucked into the bottom-right corner (empty space in this ordination)
## with its redundant "site" title dropped, since the two category names
## already say what's being colored
focal_categories <- c(
  "pebble", "boulder", "kelp_sieve", "kelp_five_rib", "silt", "green_algae_ulva",
  "red_algae_branching", "red_algae_cca", "sand_fine_shell", "kelp_sugar",
  "red_algae_flat_leaf", "red_algae_filamentous", "cobble", "red_algae_bushy",
  "brown_algae_sargassum", "shell_hash", "kelp_bryozoan", "brown_algae_encrusting",
  "sessile_invertebrates", "mobile_species"
)

site_plot <- visualize.nmds.categories(
  dat, spp_scores, colors = site_colors, categories = focal_categories,
  point_alpha = 0.25, ellipses = TRUE, legend_name = NULL,
  legend_position = c(0.95, 0.05),
  title = "Photo-level NMDS: Centennial Park vs. Elliott Bay Marina"
)
site_plot

ggsave(file.path(figs, "NMDS", "NMDS_by_site.png"), site_plot, width = 9, height = 8, dpi = 300)
## END site only ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## 2. site x depth ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
site_depth_plot <- visualize.nmds(
  dat, color_by = "site", colors = site_colors,
  facets = facet_wrap(~ depth),
  title = "Photo-level NMDS: site x depth"
)
site_depth_plot

ggsave(file.path(figs, "NMDS", "NMDS_by_site_depth.png"), site_depth_plot, width = 12, height = 7, dpi = 300)
## END site x depth ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## 3. most granular: site x depth x season, single panel ~~~~~~~~~~~~~~~~~~~~~~~
## no facets -- all 8 site x depth x season groups shown together, colored by
## site_depth_season_colors (blue family = Elliott Bay Marina, orange family =
## Centennial Park, same hue convention as the violin/spatial figures; shade
## within each family encodes depth x season)
site_depth_season_plot <- visualize.nmds(
  dat, color_by = "site_depth_season", colors = site_depth_season_colors,
  legend_name = "site, depth, season",
  title = "Photo-level NMDS: site x depth x season"
)
site_depth_season_plot

ggsave(file.path(figs, "NMDS", "NMDS_by_site_depth_season.png"), site_depth_season_plot,
      width = 11, height = 8, dpi = 300)
## END most granular ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## 4. percent-cover category correlation vectors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
category_plot <- visualize.nmds.categories(
  dat, spp_scores, colors = site_colors,
  title = "Photo-level NMDS: percent-cover category correlations"
)
category_plot

ggsave(file.path(figs, "NMDS", "NMDS_category_scores.png"), category_plot, width = 10, height = 9, dpi = 300)
## END category correlation vectors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
