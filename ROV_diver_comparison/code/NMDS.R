## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## NMDS ordination of photo-level ROV percent-cover data ~~~~~~~~~~~~~~~~~~~~~~~
## (figures live in NMDS_visualization.R instead) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())


## add libraries
library(tidyverse)
library(vegan)


## set working directory one level up and verify
setwd("../")
getwd()


## relative file paths
code <- "code"
percent_cover_dir <- "results/ROV/percent_cover"
NMDS_output <- "results/ROV/NMDS"
figs <- "figs"


## ensure the figs/NMDS folder exists for the diagnostic plots below (and for
## NMDS_visualization.R's figures, run separately)
dir.create(file.path(figs, "NMDS"), showWarnings = FALSE, recursive = TRUE)


## source functions
source(file.path(code, "NMDS_functions.R"))


## read photo-level percent-cover data
## NOTE: read_csv() (readr), not read.csv() (base) -- see the NOTE in
## data_visualization.R re: read.csv() mangling column names via make.names()
dat <- read_csv(file.path(percent_cover_dir, "HSIL_percent-cover_photo-level.csv"))
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## partition metadata and community matrix ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## sample unit = individual photo. Community matrix = every raw percent-cover
## category (pebble through red_algae_encrusting); the 3 combined_* columns
## (combined_red_algae, combined_encrusting_algae, combined_green_algae) are
## deliberately excluded -- they're sums of categories already in the matrix,
## so including them would double-count that cover.
metadata <- dat %>% select(Source_file:Distance)
community <- dat %>% select(pebble:red_algae_encrusting)


## every photo's 30 category proportions sum to 1 and there are no all-zero
## categories or rows, so no dummy species / zero-adjustment is needed, and
## since Bray-Curtis is a relative (compositional) measure, no rescaling or
## transformation of the proportions is required before ordinating
stopifnot(all.equal(rowSums(community), rep(1, nrow(community))))
## END partition ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## run NMDS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2D, Bray-Curtis, no autotransform (already-bounded proportions, not counts).
## At n = 1436 photos each random-start monoMDS run takes ~20-25 sec, so a
## trymax of 100+ run serially would take the better part of an hour --
## metaMDS's `parallel` argument runs multiple random starts at once across
## cores instead, cutting that down substantially
ord <- metaMDS(comm = community,
               distance = "bray",
               k = 2,
               trymax = 200,
               autotransform = FALSE,
               wascores = TRUE,
               parallel = max(1, parallel::detectCores() - 2))


## save the ordination object so later scripts (e.g. NMDS_visualization.R)
## can reuse it without re-running metaMDS, which is slow at n = 1436 photos
save(ord, file = file.path(NMDS_output, "NMDS_ord_photo-level.rda"))


## check stress / fit -- saved directly to file (base graphics; no device
## needs to be open first) rather than relying on an interactive window
ord$stress

png(file.path(figs, "NMDS", "NMDS_stressplot.png"), width = 6, height = 6, units = "in", res = 300)
stressplot(ord)
dev.off()

png(file.path(figs, "NMDS", "NMDS_base_plot.png"), width = 7, height = 7, units = "in", res = 300)
plot(ord)
dev.off()
## END NMDS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## extract + save ordination coordinates and category scores ~~~~~~~~~~~~~~~~~~
## bind the ordination (x, y) coordinates back onto metadata + community
## matrix, one row per photo
dat_ord <- save.points(metadata, ord, community)


## percent-cover category correlation coefficients, saved separately
spp_scores <- save.spp(ord)


write.csv(dat_ord, file.path(NMDS_output, "NMDS_ord_pts_photo-level.csv"), row.names = FALSE)
write.csv(spp_scores, file.path(NMDS_output, "NMDS_spp_scores_photo-level.csv"), row.names = FALSE)
## END extract + save ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
