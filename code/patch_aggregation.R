## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## script to extract patches and arrange in image grid ~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(magick)


## source functions
source("patch_aggregation_functions.R")


## set working directory to home folder
setwd("../")
getwd()


## relative path to image patches 
patches <- "data_output/patches/labels"
output <- "data_output/patches/figs"
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## invoke functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## use the following names to create individual plots with the first function
category_list <- c(
  "BR_sarg",
  "KE_5rib",
  "KE_sieve",
  "KE_sugar",
  "MS",
  "SU_bould",
  "SU_cob",
  "SU_peb",
  "SU_sand",
  "SU_shell",
  "SU_silt",
  "unknown",
  "KE_stipe",
  "KE_holdfas",
  "RE_CCA",
  "RE_branch",
  "RE_bush",
  "RE_leaf",
  "GR_ulva",
  "RE_fil",
  "SI_kelpBry"
)


## create a single grid image
single.category.image.grid(category = "BR_sarg", n = 36, grid_dims = c(6, 6))


## Loop through each category in category_list and create all grid images
for (category in category_list) {
  create.image.grid(category = category, n = 36, grid_dims = c(6, 6))
}
## END function invocation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
















