## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## overlay VIAME mobile-species bounding boxes on the real survey photos ~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Single-transect pipeline test/demo -- see annotate_viame_detections_
## functions.R for the shared logic (JSON parsing, frame->filename
## resolution, official-photo/ground-truth correction, drawing style) and for
## the QA tool's full rationale. build_HSIL_viame_abundance_corrected.R runs
## this same logic across all 24 transects and builds the corrected CSVs.




## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls())

library(tidyverse)
library(magick)
library(jsonlite)


## set working directory one level up and verify
setwd("../")
getwd()


ROV_input <- "data/ROV"
code <- "code"

source(file.path(code, "wrangle_data_functions.R"))
source(file.path(code, "annotate_viame_detections_functions.R"))
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## run: Centennial Park, T1 (deep), summer 2024 -- pipeline test case ~~~~~~~~~
flights_root <- "C:/Users/randellz/Seattle Aquarium Dropbox/Coastal_Climate_Resilience/flights/HSIL"

official <- get.official.photos(
  "Centennial", 1, "2024_10_08",
  abundance_csv_path = file.path(ROV_input, "HSIL_viame_abundance.csv"),
  ground_truth_path = file.path(ROV_input, "HSIL_viame_transect_ground_truth.csv")
)

process.transect(
  json_path = file.path(ROV_input, "VIAME_JSON_export_abundances", "2024_10_08_Centennial_T1_cropped.json"),
  transect_dir = file.path(flights_root, "2024/2024_10_08_diver-ROV_Centennial_Park/downward/photos/transects/T1_deep"),
  official_photos = official$Name
)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
