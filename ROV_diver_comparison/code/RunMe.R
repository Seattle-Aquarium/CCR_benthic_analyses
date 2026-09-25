## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## RunMe.R -- single entry point for the ROV-diver methods comparison pipeline
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Run this script (from the code/ folder, e.g. `Rscript RunMe.R`, or open it
## in RStudio with code/ as the working directory and Source it) to reproduce
## every wrangled data file, statistical model, and figure in this project
## from the raw diver/ROV exports in data/.
##
## Every other script in this folder is a plain function-definition file or a
## driver sourced by this one -- none of them load their own packages, set
## their own working directory, or define their own copies of the file-path
## variables below. If you're reading a script and a variable or function
## looks undefined, it's defined here.

rm(list = ls())
closeAllConnections()


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Assumes R was launched with this file's folder (code/) as the working
## directory -- true by default for Rscript code/RunMe.R, and for RStudio
## when this file is open and you Session > Set Working Directory > To Source
## File Location first. setwd("../") below then moves us to the project root
## (ROV_diver_comparison/), which every path below is relative to.
setwd("../")
cat("Working directory:", getwd(), "\n\n")

source("code/load_packages.R")


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Relative file paths ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## One name per folder, used identically by every script below -- no script
## redefines these, and no two of these names ever point at the same folder.
code                    <- "code"
figs                    <- "figs"
data_diver              <- "data/diver"
data_ROV                <- "data/ROV"
results_diver           <- "results/diver"
results_ROV_abundance   <- "results/ROV/abundance"
results_ROV_percent_cover <- "results/ROV/percent_cover"
results_ROV_NMDS        <- "results/ROV/NMDS"
results_combined        <- "results/combined"

## NOT relative -- the raw, full-resolution ROV survey photos live in the
## team's Dropbox, outside this repo (they're what build_HSIL_viame_abundance_
## corrected.R's QA step draws detection boxes onto). Update this to match
## your own machine if you need to run that step; everyone else's copy of
## this pipeline should still work off the cached results/ output, since
## reuse_existing_viame_corrections (below) skips this step by default.
flights_root <- "C:/Users/randellz/Seattle Aquarium Dropbox/Coastal_Climate_Resilience/flights/HSIL"


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Choices ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Skip re-deriving corrected VIAME abundance counts if that stage's output
## already exists. This stage is slow (opens and redraws every official ROV
## survey photo across all 24 transects) and writes its QA imagery outside
## this repo, into the raw flight-photo folders on Dropbox -- so unlike every
## other stage below, it has a real cost to re-running unnecessarily. Set
## FALSE to force a fresh correction (e.g. after new VIAME JSON exports land).
reuse_existing_viame_corrections <- TRUE

## Skip re-running the NMDS ordination if its cached result already exists.
## metaMDS() takes a few minutes even parallelized across cores (200 random
## restarts at n = 1,436 photos). Set FALSE to force a fresh ordination (e.g.
## after the underlying percent-cover data changes).
reuse_existing_nmds <- TRUE


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Source function-definition files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## wrangle_data_functions.R must come first: annotate_viame_detections_
## functions.R references its rov_invert_name_map at (its own) source time,
## not just inside a function body.
source(file.path(code, "wrangle_data_functions.R"))
source(file.path(code, "annotate_viame_detections_functions.R"))
source(file.path(code, "NMDS_functions.R"))
source(file.path(code, "NMDS_visualization_functions.R"))
source(file.path(code, "data_visualization_functions.R"))


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 1. Wrangle raw diver and ROV data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cat("\n[1/8] Wrangling diver data...\n")
source(file.path(code, "wrangle_diver_data.R"))

cat("\n[2/8] Wrangling ROV percent-cover data...\n")
source(file.path(code, "wrangle_HSIL_percent-cover_data.R"))

cat("\n[3/8] Deriving corrected ROV (VIAME) abundance data...\n")
viame_corrected_summed <- file.path(results_ROV_abundance, "HSIL_viame_abundance_corrected_summed.csv")
if (!reuse_existing_viame_corrections || !file.exists(viame_corrected_summed)) {
  source(file.path(code, "build_HSIL_viame_abundance_corrected.R"))
} else {
  cat("  found", viame_corrected_summed, "-- skipping (reuse_existing_viame_corrections = TRUE)\n")
}


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2. Combine ROV + diver data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cat("\n[4/8] Combining ROV + diver abundance and percent-cover data...\n")
source(file.path(code, "build_combined_abundance.R"))
source(file.path(code, "build_combined_percent_cover.R"))


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 3. Fit statistical models ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cat("\n[5/8] Fitting abundance (negative binomial) and percent-cover (beta-binomial) models...\n")
source(file.path(code, "abundance_models.R"))
source(file.path(code, "percent-cover_models.R"))


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 3b. ROV-diver correlations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## companion to the models above: those ask whether one platform records more
## than the other (a question about level), this asks whether the two rise and
## fall together across transects (a question about pattern). Same structure --
## all 8 percent-cover categories, then winter-only CCA + substrate, then all
## 10 abundance taxa -- plus the z-scored versions of each.
cat("\n[6/8] Computing ROV-diver correlations (raw, rank, within-site, z-scored)...\n")
source(file.path(code, "correlations.R"))


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 4. Community structure (NMDS) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cat("\n[7/8] Running / plotting NMDS ordination of photo-level percent-cover...\n")
nmds_ord_rda <- file.path(results_ROV_NMDS, "NMDS_ord_photo-level.rda")
if (!reuse_existing_nmds || !file.exists(nmds_ord_rda)) {
  source(file.path(code, "NMDS.R"))
} else {
  cat("  found", nmds_ord_rda, "-- skipping ordination (reuse_existing_nmds = TRUE)\n")
}
source(file.path(code, "NMDS_visualization.R"))


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 5. Figures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cat("\n[8/8] Generating head-to-head, violin, z-score, and proportion-across-space figures...\n")
source(file.path(code, "data_visualization.R"))


cat("\nDone. See results/ for wrangled data and model output, figs/ for figures.\n")

## Not part of this pipeline: annotate_viame_detections.R is a standalone
## single-transect QA/debugging tool (draws VIAME detection boxes onto one
## transect's photos to spot-check the annotation pipeline) -- it isn't a
## dependency of anything above and doesn't produce data the rest of the
## pipeline reads, so it's left for you to run by hand if you need it.

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
