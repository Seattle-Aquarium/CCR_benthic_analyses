## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## load every package used anywhere in the ROV-diver pipeline ~~~~~~~~~~~~~~~~~
## sourced once by RunMe.R -- no individual script below loads its own packages
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## LOAD ORDER MATTERS: tidyverse is loaded LAST, deliberately. Several packages
## below export a function with the same name as a dplyr/tidyr verb (MASS::
## select(), Matrix::expand()/pack()/unpack() via lme4, jsonlite::flatten()),
## and every unqualified select()/filter()/etc. call anywhere in this pipeline
## assumes it gets dplyr's version. R resolves an unqualified call to whichever
## same-named function was attached most recently, so loading tidyverse last
## guarantees its verbs win, no matter what else got loaded first. If you add a
## new package below, add it ABOVE tidyverse, not after it.

load_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

## used by annotate_viame_detections_functions.R / build_HSIL_viame_abundance_corrected.R
## (drawing QA boxes/labels onto ROV survey photos) and by wrangle_HSIL_viame
## abundance/percent-cover data (parsing raw VIAME JSON exports)
load_pkg("magick")
load_pkg("jsonlite")

## abundance_models.R: negative binomial GLMM (glmer.nb(), MASS::glm.nb())
load_pkg("lme4")
load_pkg("MASS")

## percent-cover_models.R: beta-binomial GLMM
load_pkg("glmmTMB")

## NMDS.R: ordination (metaMDS())
load_pkg("vegan")

## data_visualization.R: kelp z-score overlay figure (nested axis guide,
## panel-combining, markdown/italic plot titles)
load_pkg("legendry")
load_pkg("patchwork")
load_pkg("ggtext")

## core data wrangling / plotting, used throughout every script -- LAST, see
## note above
load_pkg("tidyverse")
load_pkg("stringr")

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
