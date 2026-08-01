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
ROV_output <- "results/ROV/percent_cover"
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
## focal category pairs of interest. Data now comes from
## results/combined/ROV_diver_percent_cover_combined.csv (the same combined
## file percent-cover_models.R models against), rather than independently
## re-deriving the crosswalk from the raw ROV/diver files -- see
## build.percent.cover.pairs.data() for why.
combined_percent_cover <- read_csv(file.path("results/combined", "ROV_diver_percent_cover_combined.csv"),
                                   show_col_types = FALSE)

## panel order: fixed, per explicit request -- red algae, green algae, CCA,
## then substrate boulder, rock (15-25cm/"cobble"), pebble, shell hash, sand.
## Note this is NOT the "substrate ordered by diver proportion, highest to
## lowest" rule used in the previous version of this figure (that order was
## boulder, pebble, sand, shell hash, rock) -- flagging the difference here
## in case the change in relative substrate order wasn't intentional.
percent_cover_categories <- c(
  "cover_red_algae", "combined_green_algae", "cover_crustose_coralline",
  "combined_substrate_boulder", "substrate_rock_.15.25cm.wa.",
  "combined_substrate_pebble", "substrate_shell_hash", "substrate_sand"
)

## one color per category: red/green reserved for the two algae categories
## they name, the rest hand-picked to avoid competing with that red/green
## convention -- CCA a coralline pink, substrate categories an earth-tone
## gradient (darkest = coarsest material, boulder, down to palest = shell
## hash) since that grain-size ordering is thematically meaningful here in a
## way it isn't for the unrelated abundance taxa
percent_cover_colors <- c(
  cover_red_algae              = "#A6323C",  # dark off-red
  combined_green_algae         = "#4C9A2A",  # green
  cover_crustose_coralline     = "#E07A9E",  # coralline pink
  combined_substrate_boulder   = "#6B5B4F",  # dark brown-gray
  "substrate_rock_.15.25cm.wa." = "#8C7A6B",  # medium brown-gray (cobble)
  combined_substrate_pebble    = "#A89A8C",  # light brown-gray
  substrate_sand                = "#D9C08C",  # sandy tan
  substrate_shell_hash          = "#C7BFC2"   # pale gray
)

## strip-label-only renames -- short display names, per explicit request;
## the underlying column names (and therefore color mapping / data / models
## elsewhere in the pipeline) are untouched
percent_cover_labels <- c(
  cover_red_algae               = "red_algae",
  combined_green_algae          = "green_algae",
  cover_crustose_coralline      = "crustose_coralline",
  combined_substrate_boulder    = "substrate_boulder",
  "substrate_rock_.15.25cm.wa." = "substrate_rock",
  combined_substrate_pebble     = "substrate_pebble",
  substrate_shell_hash          = "substrate_shell_hash",
  substrate_sand                = "substrate_sand"
)


## single-category example: combined red algae
percent_cover_pairs_data <- build.percent.cover.pairs.data(combined_percent_cover, percent_cover_categories)

red_algae_plot <- visualize.head.to.head(
  data = filter(percent_cover_pairs_data, category == "cover_red_algae"),
  colors = percent_cover_colors["cover_red_algae"]
) +
  labs(title = "red_algae")
red_algae_plot

dir.create(file.path(figs, "percent-cover"), showWarnings = FALSE, recursive = TRUE)

ggsave(file.path(figs, "percent-cover", "ROV_diver_combined_red_algae.png"),
      red_algae_plot, width = 6, height = 6, dpi = 300)


## all eight focal categories, faceted within a single figure -- both all-
## season and winter-only versions, same category order/colors/formatting,
## exported as both png and pdf
head_to_head_plot <- visualize.head.to.head(
  data = percent_cover_pairs_data,
  colors = percent_cover_colors,
  category_order = percent_cover_categories,
  labels = percent_cover_labels
)
head_to_head_plot

ggsave(file.path(figs, "percent-cover", "ROV_diver_percent_cover_head_to_head.png"),
      head_to_head_plot, width = 16, height = 8, dpi = 300)
ggsave(file.path(figs, "percent-cover", "ROV_diver_percent_cover_head_to_head.pdf"),
      head_to_head_plot, width = 16, height = 8, dpi = 300)


percent_cover_pairs_data_winter <- build.percent.cover.pairs.data(
  combined_percent_cover %>% filter(season == "winter"),
  percent_cover_categories
)

head_to_head_plot_winter <- visualize.head.to.head(
  data = percent_cover_pairs_data_winter,
  colors = percent_cover_colors,
  category_order = percent_cover_categories,
  labels = percent_cover_labels
)
head_to_head_plot_winter

ggsave(file.path(figs, "percent-cover", "ROV_diver_percent_cover_head_to_head_winter.png"),
      head_to_head_plot_winter, width = 16, height = 8, dpi = 300)
ggsave(file.path(figs, "percent-cover", "ROV_diver_percent_cover_head_to_head_winter.pdf"),
      head_to_head_plot_winter, width = 16, height = 8, dpi = 300)
## END percent-cover head-to-head comparisons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## ROV photo-level spatial data prep ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## full-resolution (not transect-averaged) ROV percent-cover, visualized across
## individual, sequential photos rather than compared against diver data.
## site_season_photos holds every photo (both ROV passes) for each site x
## season combination, with each photo's GPS distance (m) from its transect's
## first-captured photo attached via add.transect.distance() -- the "violin
## distributions" section below uses these directly (zeros excluded within
## the plotting function itself); the "proportion across space" section
## re-derives an outward-pass-only, <=30m-cutoff version per category via
## prep.outward.pass.photos().
site_display_names <- c(Centennial_Park = "Centennial Park",
                        Elliott_Bay_Marina = "Elliott Bay Marina")

## folder name per season ("Summer"/"Winter"), used for both figure families
season_display_names <- c(summer = "Summer", winter = "Winter")

site_season_photos <- list()
for (site_name in names(site_display_names)) {
  for (season_name in names(season_display_names)) {
    site_season_photos[[paste(site_name, season_name, sep = "_")]] <- ROV_percent_cover_photo_level %>%
      filter(site == site_name, season == season_name, transect %in% 1:6) %>%
      add.transect.distance()
  }
}


## species display names (scientific name, italicized via markdown, then
## common name) -- reused across every kelp figure below
sugar_kelp_name <- "*Saccharina latissima* (sugar kelp)"
sieve_kelp_name <- "*Agarum clathratum* (sieve kelp)"


## six-color, depth-grouped palette used by every per-transect figure below --
## both the violin distributions and the proportion-across-space line plots,
## which as of this revision are colored by transect too, replacing an
## earlier version that pulled one color per category from the Zooniverse
## labelset JSON: deep transects (1-3) in blues, shallow transects (4-6) in
## oranges (three shades each, dark -> light) -- echoes the blue/orange
## diver-vs-ROV pairing used in kelp_combined_colors further down, so the six
## transects stay individually distinguishable but still visually group by
## depth at a glance
transect_density_colors <- c(
  "1" = "#08519C", "2" = "#3182BD", "3" = "#6BAED6",
  "4" = "#A63603", "5" = "#E6550D", "6" = "#FD8D3C"
)


## every raw percent-cover category (used as-is by the violin sweep below).
## The spatial sweep adds the 3 combined_* categories (red/green/encrusting
## algae) on top of this, so it shows both the combined view and every
## individual taxon that feeds into it.
all_percent_cover_categories <- c("brown_algae_encrusting", "brown_algae_filamentous",
                                  "brown_algae_fucus", "brown_algae_sargassum",
                                  "green_algae_filamentous", "green_algae_ulva",
                                  "kelp_five_rib", "kelp_bull_blade", "kelp_holdfast", "kelp_sieve",
                                  "kelp_stipe", "kelp_sugar",
                                  "mobile_species",
                                  "red_algae_cca", "red_algae_branching", "red_algae_bushy",
                                  "red_algae_encrusting", "red_algae_filamentous", "red_algae_flat_leaf",
                                  "sessile_invertebrates", "kelp_bryozoan",
                                  "anthropogenic", "boulder", "cobble", "pebble",
                                  "sand_fine_shell", "shell_hash", "silt", "wood_debris",
                                  "unknown_area")

combined_categories <- c("combined_red_algae", "combined_green_algae", "combined_encrusting_algae")

spatial_categories <- c(all_percent_cover_categories, combined_categories)
## END ROV photo-level spatial data prep ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## proportion across space, by category (outward pass, <=30m only) ~~~~~~~~~~~~
## systematic sweep of visualize.photo.level() over every percent-cover
## category (every raw taxon + the 3 combined_* groupings), for both sites
## and both seasons -- saved into figs/proportion_across_space/<site>/<season>/.
## Restricting to the outbound ("out") pass (see add.transect.pass() in
## wrangle_data_functions.R) gives one non-interleaved leg per transect, so
## distance_m alone orders photos along the tape; prep.outward.pass.photos()
## also drops the handful of photos whose GPS distance lands past the actual
## 30m tape length (logging/GPS artifacts, not real transect). Figures are
## stretched wide (18in) so spatial pattern isn't visually compressed within
## each of the 3 facet columns, and the y-axis is fixed to 0-1 (not left to
## float per-category) so cover magnitude is directly comparable across
## categories, even though that makes rarer categories harder to read on
## their own panel.
##
## NOTE for later: if any of these categories show a legible spatial signal
## (e.g. a recurring peak/trough spacing along the transect), that's exactly
## the kind of pattern a wavelet (or similar spatial-frequency) analysis could
## formalize -- worth a follow-up once we've eyeballed this full category set.
for (site_name in names(site_display_names)) {
  for (season_name in names(season_display_names)) {
    season_label <- season_display_names[[season_name]]
    out_dir <- file.path(figs, "proportion_across_space", site_name, season_label)
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

    site_out_photos <- prep.outward.pass.photos(ROV_percent_cover_photo_level, site_name, season_name)

    for (category in spatial_categories) {
      category_label <- format.category.label(category, sugar_kelp_name, sieve_kelp_name)

      spatial_plot <- visualize.photo.level(
        data = site_out_photos,
        category = category,
        transect_order = c(4, 5, 6, 1, 2, 3),
        colors = transect_density_colors,
        ncol = 3,
        x_label = "distance along outward pass (m)",
        y_label = paste("proportion", gsub("_", " ", category))
      ) +
        labs(title = paste(site_display_names[[site_name]], season_name, "outward pass --", category_label)) +
        theme(plot.title = ggtext::element_markdown())

      ggsave(file.path(out_dir, paste0(category, ".png")),
            spatial_plot, width = 18, height = 8, dpi = 300)
    }
  }
}
## END proportion across space, by category ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## violin distributions, by category (given presence, w/ prevalence) ~~~~~~~~~~
## systematic sweep of visualize.category.violin.with.prevalence() over every
## percent-cover category (both ROV passes; zero-cover photos excluded from
## the violin/box/points, with each transect's prevalence -- % of all photos,
## zero included, with any cover -- printed above), for both sites and both
## seasons -- saved into figs/violin/<site>/<season>/.
for (site_name in names(site_display_names)) {
  for (season_name in names(season_display_names)) {
    season_label <- season_display_names[[season_name]]
    out_dir <- file.path(figs, "violin", site_name, season_label)
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

    site_season_data <- site_season_photos[[paste(site_name, season_name, sep = "_")]]

    for (category in all_percent_cover_categories) {
      category_label <- format.category.label(category, sugar_kelp_name, sieve_kelp_name)

      violin_plot <- visualize.category.violin.with.prevalence(
        data = site_season_data,
        category = category,
        colors = transect_density_colors,
        category_label = category_label,
        title = paste(site_display_names[[site_name]], season_name, "--", category_label)
      )

      ggsave(file.path(out_dir, paste0(category, ".png")),
            violin_plot, width = 10, height = 7, dpi = 300)
    }
  }
}
## END violin distributions, by category ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## spatial structure (trial): TTLQV & correlogram ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## first trial of two classic patchiness/spatial-structure techniques (see
## compute.ttlqv() / compute.correlogram() / visualize.spatial.structure() in
## data_visualization_functions.R), computed on the same outward-pass, <=30m
## data as the spatial line plots above -- never on both passes combined
## (they're physically different tracks, ~2m apart across the tape, not
## repeat samples of the same line) and never stitched end-to-end across the
## three replicate transects within a depth (they're separate 30m tracks with
## an unsurveyed ~5m gap between them, so concatenating them would inject a
## fake discontinuity every 30m).
##
## Instead, TTLQV/the correlogram are computed separately per transect, then
## the three replicates within a depth are pooled by averaging their curves
## at each shared block-size/lag (the bold black line in each panel) -- the
## three thin, transect-colored lines underneath show how much the replicates
## agree or disagree, which is itself informative (tight agreement = a robust
## depth-level signal; wide spread = patchiness that varies a lot even within
## a nominally uniform depth stratum).
##
## Trialed for our two focal kelp species only, at the site where each
## dominates: sugar kelp at Centennial Park, sieve kelp at Elliott Bay Marina
## (both summer).
dir.create(file.path(figs, "TTLQV"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(figs, "correlogram"), showWarnings = FALSE, recursive = TRUE)

spatial_structure_trials <- list(
  list(site_name = "Centennial_Park", category = "kelp_sugar", category_label = sugar_kelp_name),
  list(site_name = "Elliott_Bay_Marina", category = "kelp_sieve", category_label = sieve_kelp_name)
)

for (trial in spatial_structure_trials) {
  site_out_photos <- prep.outward.pass.photos(ROV_percent_cover_photo_level, trial$site_name, "summer")
  site_title <- site_display_names[[trial$site_name]]

  ttlqv_by_transect <- compute.spatial.structure.by.transect(
    site_out_photos, trial$category, compute.ttlqv
  )
  ttlqv_plot <- visualize.spatial.structure(
    ttlqv_by_transect, x_col = "block_size_m", y_col = "ttlqv",
    colors = transect_density_colors,
    x_label = "block size (m)", y_label = "TTLQV (block variance)",
    title = paste(site_title, "summer --", trial$category_label, "-- TTLQV")
  )
  ggsave(file.path(figs, "TTLQV", paste0(trial$category, "_", trial$site_name, "_summer.png")),
        ttlqv_plot, width = 10, height = 6, dpi = 300)

  correlogram_by_transect <- compute.spatial.structure.by.transect(
    site_out_photos, trial$category, compute.correlogram
  )
  correlogram_plot <- visualize.spatial.structure(
    correlogram_by_transect, x_col = "lag_mid", y_col = "correlation",
    colors = transect_density_colors,
    x_label = "distance lag (m)", y_label = "spatial autocorrelation",
    title = paste(site_title, "summer --", trial$category_label, "-- correlogram")
  ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40")
  ggsave(file.path(figs, "correlogram", paste0(trial$category, "_", trial$site_name, "_summer.png")),
        correlogram_plot, width = 10, height = 6, dpi = 300)
}
## END spatial structure (trial): TTLQV & correlogram ~~~~~~~~~~~~~~~~~~~~~~~~~~




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

ggsave(file.path(figs, "z-score", "kelp_sugar_sieve_standardized_overlay.png"),
      kelp_overlay_stack, width = 12, height = 9, dpi = 300)


## 2. bump/slope chart -- per-transect rank in each method, colored by site
kelp_sugar_bump_plot <- visualize.kelp.bump.chart(
  data = kelp_sugar_comparison,
  rank_color_by = "site"
) +
  labs(title = paste0(sugar_kelp_name, ":<br>diver vs. ROV rank agreement by transect")) +
  theme(plot.title = ggtext::element_markdown())
kelp_sugar_bump_plot

ggsave(file.path(figs, "z-score", "kelp_sugar_bump_chart.png"),
      kelp_sugar_bump_plot, width = 6, height = 8, dpi = 300)


## 3. scatter -- diver density vs. ROV cover, colored by site, with loess trend
kelp_sugar_scatter_plot <- visualize.kelp.scatter(
  data = kelp_sugar_comparison,
  color_by = "site"
) +
  labs(title = paste0(sugar_kelp_name, ": diver density vs. ROV cover")) +
  theme(plot.title = ggtext::element_markdown())
kelp_sugar_scatter_plot

ggsave(file.path(figs, "head-to-head", "kelp_sugar_scatter.png"),
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

ggsave(file.path(figs, "z-score", "kelp_sugar_sieve_standardized_overlay_combined.png"),
      kelp_overlay_combined, width = 13, height = 6, dpi = 300)
## END kelp density vs. percent-cover concordance ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## visualize abundances ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ROV vs. diver head-to-head, one panel per overlapping abundance taxon (see
## build_combined_abundance.R for how the combined file / the 10-taxon
## overlap list / "key" are constructed). Unlike the percent-cover head-to-
## head figure above, each panel gets its own free axis scale (see
## visualize.abundance.pairs() for why) and a 4-column x 3-row layout.
combined_abundance <- read_csv(file.path("results/combined", "ROV_diver_abundance_combined.csv"))

abundance_taxa <- c("ochre_mottled_star", "cancer_crab", "burrowing_sea_cucumber",
                    "kelp_crab", "leather_star", "plumose_anemone",
                    "green_white_urchin", "california_sea_cucumber",
                    "blood_star", "large_anemone")

## one color per taxon, purely to distinguish panels at a glance (no legend
## is shown -- see visualize.abundance.pairs()); deliberately avoids red and
## green, which are reserved for red algae / green algae throughout the rest
## of this report's figures
abundance_colors <- c(
  ochre_mottled_star      = "#4E79A7",  # blue
  cancer_crab              = "#F28E2B",  # orange
  burrowing_sea_cucumber   = "#9C755F",  # brown
  kelp_crab                = "#B07AA1",  # purple
  leather_star             = "#D37295",  # rose
  plumose_anemone          = "#76B7B2",  # teal
  green_white_urchin       = "#59A5D8",  # sky blue
  california_sea_cucumber  = "#EDC948",  # gold
  blood_star                = "#BAB0AC",  # gray
  large_anemone             = "#6B4C9A"   # violet
)

## strip-label-only renames (the underlying taxon/column names above are
## unchanged everywhere else in the pipeline) -- burrowing_sea_cucumber and
## california_sea_cucumber overflowed their panels at the larger strip-text
## size, so they're shortened here to fit. orange_cucumber also matches this
## taxon's actual Reef Check label ("Orange Cucumber" in invert_name_map,
## wrangle_data_functions.R) more closely than the internal column name does
abundance_labels <- c(
  burrowing_sea_cucumber  = "orange_cucumber",
  california_sea_cucumber = "CA_sea_cucumber"
)

abundance_pairs_data <- build.abundance.pairs.data(combined_abundance, abundance_taxa)

abundance_head_to_head_plot <- visualize.abundance.pairs(
  data = abundance_pairs_data,
  colors = abundance_colors,
  ncol = 4,
  labels = abundance_labels
)
abundance_head_to_head_plot

## polished figure -- exported as both a standard png and a high-quality pdf
abundance_figs_dir <- file.path(figs, "abundance")
dir.create(abundance_figs_dir, showWarnings = FALSE, recursive = TRUE)

ggsave(file.path(abundance_figs_dir, "ROV_diver_abundance_head_to_head.png"),
      abundance_head_to_head_plot, width = 16, height = 12, dpi = 300)
ggsave(file.path(abundance_figs_dir, "ROV_diver_abundance_head_to_head.pdf"),
      abundance_head_to_head_plot, width = 16, height = 12, dpi = 300)
## END abundance visualization ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
