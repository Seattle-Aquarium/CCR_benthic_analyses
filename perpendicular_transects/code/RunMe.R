## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## RunMe.R -- single entry point for the perpendicular-transect pipeline ~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Run from the code/ folder (`Rscript RunMe.R`, or Source in RStudio after
## Session > Set Working Directory > To Source File Location). Reproduces every
## wrangled file in results/ and every figure in figs/ from the raw CSVs in
## data/ plus the survey photos on Dropbox (flights_root below).
##
## Every other script here is a function-definition file or a driver sourced
## by this one; if a variable or function looks undefined, it's defined here.

rm(list = ls())
closeAllConnections()


## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("../")
cat("Working directory:", getwd(), "\n\n")
source("code/load_packages.R")


## relative file paths ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
code_dir    <- "code"
data_dir    <- "data"
results_dir <- "results"
figs_dir    <- "figs"

## NOT relative -- the full-resolution survey photos and their point-level
## annotation files live on the team Dropbox, outside this repo. Update to
## match your machine.
flights_root <- "C:/Users/randellz/Seattle Aquarium Dropbox/Coastal_Climate_Resilience/flights/Port_of_Seattle/2025/2025_10_08_Fishing_Pier/downward/photos/transects"

## small, slide-resolution copies of every photo, cached here so figures don't
## re-read the ~3.5 MB originals on every run (git-ignored)
thumbs_dir <- file.path(results_dir, "thumbnails")

for (d in c(results_dir, figs_dir, thumbs_dir)) dir.create(d, showWarnings = FALSE, recursive = TRUE)


## source function-definition files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source(file.path(code_dir, "wrangle_functions.R"))
source(file.path(code_dir, "zonation_functions.R"))
source(file.path(code_dir, "nmds_functions.R"))
source(file.path(code_dir, "visualization_functions.R"))


## 1. wrangle ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cat("\n[1/4] Wrangling photo- and point-level data...\n")
source(file.path(code_dir, "wrangle_transects.R"))


## 2. zone boundaries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cat("\n[2/4] Detecting community zone boundaries along each transect...\n")
source(file.path(code_dir, "zonation.R"))


## 3. NMDS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cat("\n[3/4] Ordinating photos (NMDS) for the community-path figure...\n")
source(file.path(code_dir, "nmds.R"))


## 4. figures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cat("\n[4/4] Building figures...\n")
source(file.path(code_dir, "visualization.R"))


cat("\nDone. See results/ for wrangled data, figs/ for figures.\n")

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
