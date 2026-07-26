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
library(legendry)   ## nested (transect > season > site) x-axis guide
library(patchwork)  ## combining + collecting shared legend/axis title
library(ggtext)     ## markdown/italic plot titles (scientific names)


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
diver_algae_density <- read_csv(file.path(diver_output, "diver_algae_density.csv"))


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


## species display names (scientific name, italicized via markdown, then
## common name) -- reused across every kelp figure below
sugar_kelp_name <- "*Saccharina latissima* (sugar kelp)"
sieve_kelp_name <- "*Agarum clathratum* (sieve kelp)"


## shallow transects (4,5,6) on top, deep transects (1,2,3) on bottom
kelp_sugar_plot <- visualize.photo.level(
  data = centennial_summer_photos,
  category = "kelp_sugar",
  transect_order = c(4, 5, 6, 1, 2, 3),
  color = kelp_sugar_color,
  ncol = 3,
  y_label = "proportion sugar kelp"
) +
  labs(title = paste("Centennial Park, summer --", sugar_kelp_name)) +
  theme(plot.title = ggtext::element_markdown())
kelp_sugar_plot

ggsave(file.path(figs, "ROV_photo-level_kelp_sugar_Centennial_summer.png"),
      kelp_sugar_plot, width = 12, height = 8, dpi = 300)
## END ROV photo-level spatial visualization ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## kelp density (diver) vs. percent-cover (ROV) concordance ~~~~~~~~~~~~~~~~~~~
## diver density (individuals, extrapolated/standardized per transect) and
## ROV cover (proportion of photo points) are not the same unit and can't be
## converted into one another without calibration data we don't have (see
## discussion in this thread). These three plots instead compare *relative
## pattern* across the 24 site x transect x season combinations: do the two
## methods agree on which transects have more/less sugar kelp, even though
## their absolute scales differ?
kelp_sugar_comparison <- build.kelp.comparison.data(
  diver_density_df = diver_algae_density,
  rov_cover_df = ROV_percent_cover_averaged,
  species_col = "kelp_sugar"
)

method_colors <- c("diver" = "#1B9E77", "ROV" = "#D95F02")


## same comparison for sieve kelp (kelp_sieve) -- almost exclusively found at
## the Elliott Bay Marina breakwater, a useful contrast to sugar kelp above,
## which was almost exclusively a Centennial Park signal
kelp_sieve_comparison <- build.kelp.comparison.data(
  diver_density_df = diver_algae_density,
  rov_cover_df = ROV_percent_cover_averaged,
  species_col = "kelp_sieve"
)


## 1. standardized overlay -- both series z-scored onto one shared axis,
## sugar kelp (top) and sieve kelp (bottom) combined into a single figure:
## transects ordered site (Centennial Park, then Elliott Bay Marina) > season
## (summer, then winter) > transect (1-6); legend inset into the top panel,
## a single shared y-axis title, and the nested transect/season/site x-axis
## label shown only on the bottom row (top row keeps its tick marks,
## unlabeled, so the two rows still align)
kelp_overlay_stack <- visualize.kelp.standardized.overlay.stack(
  data_top = kelp_sugar_comparison,
  data_bottom = kelp_sieve_comparison,
  title_top = sugar_kelp_name,
  title_bottom = sieve_kelp_name,
  colors = method_colors
)
kelp_overlay_stack

ggsave(file.path(figs, "kelp_sugar_sieve_standardized_overlay.png"),
      kelp_overlay_stack, width = 12, height = 9, dpi = 300)


## 2. bump/slope chart -- per-transect rank in each method, colored by site
kelp_sugar_bump_plot <- visualize.kelp.bump.chart(
  data = kelp_sugar_comparison,
  rank_color_by = "site"
) +
  labs(title = paste0(sugar_kelp_name, ":<br>diver vs. ROV rank agreement by transect")) +
  theme(plot.title = ggtext::element_markdown())
kelp_sugar_bump_plot

ggsave(file.path(figs, "kelp_sugar_bump_chart.png"),
      kelp_sugar_bump_plot, width = 6, height = 8, dpi = 300)


## 3. scatter -- diver density vs. ROV cover, colored by site, with loess trend
kelp_sugar_scatter_plot <- visualize.kelp.scatter(
  data = kelp_sugar_comparison,
  color_by = "site"
) +
  labs(title = paste0(sugar_kelp_name, ": diver density vs. ROV cover")) +
  theme(plot.title = ggtext::element_markdown())
kelp_sugar_scatter_plot

ggsave(file.path(figs, "kelp_sugar_scatter.png"),
      kelp_sugar_scatter_plot, width = 7, height = 6, dpi = 300)


## 4. standardized overlay, sugar + sieve kelp combined into a single row --
## an alternative to the two-row stack above: 4 lines (diver/ROV x sugar/sieve)
## in one panel instead of 2 panels of 2 lines each. Diver gets two shades of
## blue (one per species); ROV gets two shades of orange -- blue's complement
## on the color wheel, chosen so the ROV pair reads as a distinct-but-related
## counterpart to the diver pair rather than clashing with it. Both pairs are
## drawn from ColorBrewer's "Paired" qualitative palette, so the light/dark
## relationship within each color family is perceptually consistent.
kelp_combined_colors <- c(
  "diver_sugar" = "#1F78B4",  # dark blue
  "diver_sieve" = "#A6CEE3",  # light blue
  "ROV_sugar"   = "#FF7F00",  # dark orange
  "ROV_sieve"   = "#FDBF6F"   # light orange
)

kelp_overlay_combined <- visualize.kelp.standardized.overlay.combined(
  data_sugar = kelp_sugar_comparison,
  data_sieve = kelp_sieve_comparison,
  title = paste(sugar_kelp_name, "&", sieve_kelp_name),
  colors = kelp_combined_colors
)
kelp_overlay_combined

ggsave(file.path(figs, "kelp_sugar_sieve_standardized_overlay_combined.png"),
      kelp_overlay_combined, width = 13, height = 6, dpi = 300)
## END kelp density vs. percent-cover concordance ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




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
