## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## visualize data for ROV-diver comparison ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## (formal statistical analyses live in data_analyses.R instead) ~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())


## add libraries
library(tidyverse)
library(stringr)
library(jsonlite)


## set working directory one level up and verify
setwd("../")
getwd()


## relative file paths
diver_output <- "results/diver"
ROV_output <- "results/ROV"
ROV_input <- "data/ROV"
code <- "code"
figs <- "figs"


## source functions
source(file.path(code, "wrangle_data_functions.R"))
source(file.path(code, "data_visualization_functions.R"))


## read diver data
## NOTE: read_csv() (readr), not read.csv() (base) -- base read.csv() runs
## column names through make.names(), which mangles the parentheses/hyphens
## in names like "substrate_rock_(15-25cm-wa)" into "substrate_rock_.15.25cm.wa.".
## read_csv() preserves them as-is, matching the header actually written to disk.
diver_invert_abundance <- read_csv(file.path(diver_output, "diver_invert_abundance.csv"))
diver_UPC_percentage <- read_csv(file.path(diver_output, "diver_UPC_percentage.csv"))


## read ROV data
ROV_percent_cover_averaged <- read_csv(file.path(ROV_output, "HSIL_percent-cover_transect-averaged.csv"))
ROV_percent_cover_photo_level <- read_csv(file.path(ROV_output, "HSIL_percent-cover_photo-level.csv"))
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## ROV-diver percent-cover head-to-head comparisons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## per github.com/Seattle-Aquarium/CCR_benthic_analyses/issues/7, the eight
## focal category pairs of interest, and their approximate match-ups
head_to_head_pairs <- tribble(
  ~category,            ~rov_col,              ~diver_col,
  "combined_red_algae",  "combined_red_algae",  "cover_red_algae",
  "green_algae_ulva",    "green_algae_ulva",    "combined_green_algae",
  "red_algae_cca",       "red_algae_cca",       "cover_crustose_coralline",
  "boulder",             "boulder",             "combined_substrate_boulder",
  "cobble",              "cobble",              "substrate_rock_(15-25cm-wa)",
  "pebble",              "pebble",              "combined_substrate_pebble",
  "sand_fine_shell",     "sand_fine_shell",     "substrate_sand",
  "shell_hash",          "shell_hash",          "substrate_shell_hash"
)


## join ROV + diver data by site/transect/season for each pair (24 points per
## category: 2 sites x 6 transects x 2 seasons), rescaling ROV proportions
## (0-1) up to the diver's 0-100 percentage scale so both axes -- and the 1:1
## reference line -- are on the same footing
head_to_head_data <- build.head.to.head.data(
  rov_df = ROV_percent_cover_averaged,
  diver_df = diver_UPC_percentage,
  pairs = head_to_head_pairs
)


## pull point colors from the Zooniverse labelset (data/ROV/labelset_toolbox_
## zooniverse.json), keyed by our category names. Categories with a direct
## Zooniverse code use that code's color; combined_red_algae has no single
## code of its own, so its color is our best guess -- the average RGB of its
## four constituent ROV categories (red_algae_branching, red_algae_bushy,
## red_algae_filamentous, red_algae_flat_leaf)
category_codes <- c(
  "green_algae_ulva" = "GR_ulva",
  "red_algae_cca"    = "RE_CCA",
  "boulder"          = "SU_bould",
  "cobble"           = "SU_cob",
  "pebble"           = "SU_peb",
  "sand_fine_shell"  = "SU_sand",
  "shell_hash"       = "SU_shell"
)

category_combos <- list(
  combined_red_algae = c("RE_branch", "RE_bush", "RE_fil", "RE_leaf")
)

category_colors <- get.category.colors(
  json_path = file.path(ROV_input, "labelset_toolbox_zooniverse.json"),
  code_map = category_codes,
  combo_map = category_combos
)


## single-category example: combined red algae
red_algae_plot <- visualize.head.to.head(
  data = filter(head_to_head_data, category == "combined_red_algae"),
  colors = category_colors["combined_red_algae"]
) +
  labs(title = "combined_red_algae")
red_algae_plot

ggsave(file.path(figs, "ROV_diver_combined_red_algae.png"),
      red_algae_plot, width = 6, height = 6, dpi = 300)


## all eight focal categories, faceted within a single figure
head_to_head_plot <- visualize.head.to.head(
  data = head_to_head_data,
  colors = category_colors
)
head_to_head_plot

ggsave(file.path(figs, "ROV_diver_head_to_head.png"),
      head_to_head_plot, width = 12, height = 10, dpi = 300)
## END percent-cover head-to-head comparisons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## ROV photo-level spatial visualization ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## full-resolution (not transect-averaged) ROV percent-cover, visualized across
## individual, sequential photos rather than compared against diver data.
##
## Two ROV passes are run per 30m transect -- an outbound pass down one side
## of the meter tape, and a return pass down the other -- so raw photo/capture
## order doesn't map cleanly onto "meters along the transect" (a naive
## sequential x-axis would run the outbound leg 0->30m and then the return
## leg would also run roughly 0->30m, appended after it). Instead,
## add.transect.distance() estimates each photo's GPS distance (m) from its
## transect's first-captured photo, so ordering ALL photos (both passes) by
## that distance interleaves them by physical position along the tape --
## roughly two points per meter mark, one from each pass.
centennial_summer_photos <- ROV_percent_cover_photo_level %>%
  filter(site == "Centennial_Park", season == "summer", transect %in% 1:6) %>%
  add.transect.distance()


## reuse the kelp - Sugar color from the Zooniverse labelset for consistency
## with the head-to-head figures above
kelp_sugar_rgb <- get.zooniverse.rgb(file.path(ROV_input, "labelset_toolbox_zooniverse.json"))
kelp_sugar_color <- rgb(kelp_sugar_rgb["KE_sugar", 1], kelp_sugar_rgb["KE_sugar", 2],
                        kelp_sugar_rgb["KE_sugar", 3], maxColorValue = 255)


## shallow transects (4,5,6) on top, deep transects (1,2,3) on bottom
kelp_sugar_plot <- visualize.photo.level(
  data = centennial_summer_photos,
  category = "kelp_sugar",
  transect_order = c(4, 5, 6, 1, 2, 3),
  color = kelp_sugar_color,
  ncol = 3,
  y_label = "proportion sugar kelp"
) +
  labs(title = "Centennial Park, summer -- sugar kelp")
kelp_sugar_plot

ggsave(file.path(figs, "ROV_photo-level_kelp_sugar_Centennial_summer.png"),
      kelp_sugar_plot, width = 12, height = 8, dpi = 300)
## END ROV photo-level spatial visualization ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## visualize abundances ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## NOTE: ROV_invert_abundance.csv doesn't exist yet -- new VIAME-derived
## abundance data is forthcoming (wrangle_ROV_abundance_data.R will need to
## be re-run once it lands); this block will error out until then, so it's
## placed last rather than in startup
ROV_abundance <- read_csv(file.path(ROV_output, "ROV_invert_abundance.csv"))

visualize.abundance.pairs(x_axis = ROV_abundance,
                          y_axis = diver_invert_abundance,
                          colname = "cancer_crab",
                          axis_limit = 8)
## END abundance visualization ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
