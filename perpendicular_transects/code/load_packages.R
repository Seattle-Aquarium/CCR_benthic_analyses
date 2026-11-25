## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## load every package used in the perpendicular-transect pipeline ~~~~~~~~~~~~~~
## sourced once by RunMe.R -- no other script loads its own packages
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## tidyverse is loaded LAST so its verbs (select(), filter(), ...) win over any
## same-named function exported by a package above it -- see the matching note
## in ROV_diver_comparison/code/load_packages.R.

load_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

load_pkg("magick")     # photo thumbnails, point-overlay exemplars
load_pkg("jsonlite")   # T2 point annotations (Toolbox JSON export)
load_pkg("vegan")      # Bray-Curtis dissimilarity (zonation)
load_pkg("rioja")      # CONISS constrained clustering + broken-stick test (zonation)
load_pkg("ragg")       # PNG device (the default Windows device mangles many small rasters)
load_pkg("patchwork")  # stacking panels into slide-sized figures
load_pkg("ggtext")     # markdown titles / italic taxon names

load_pkg("tidyverse")

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
