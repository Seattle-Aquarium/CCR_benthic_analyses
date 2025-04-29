## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Calculate species density metrics ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())
options(error = NULL)


## add libraries
library(tidyverse)


## set working directory to home folder
setwd("../")
getwd()


## relative file paths
code <- "code"
figs <- "figs"
input <- "data_input"
label_19 <- "data_output/19_labels"


## source functions 
source(file.path(code, "species_density_functions.R"))


## read in data
HSIL <- read.csv(file.path(input, "HSIL_VIAME_2024_v2.csv"))
Port <- read.csv(file.path(input, "Port_VIAME_2022.csv"))
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## data wrangling ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
HSIL <- rm.chars(HSIL, "Transect_ID", 1, "left")
HSIL <- rename.columns(HSIL, "Transect_ID", "transect")
HSIL <- rename.columns(HSIL, "Site_ID", "site")
HSIL <- rename.columns(HSIL, "Img_Name", "img_name")
HSIL <- rename.site(HSIL)
HSIL <- create.key(HSIL)


## other df
Port <- rm.chars(Port, "Transect_ID", 1, "left")
Port <- rm.chars(Port, "Site_ID", 1, "left")
Port <- rename.columns(Port, "Transect_ID", "transect")
Port <- rename.columns(Port, "Site_ID", "site")
Port <- rename.columns(Port, "Img_Name", "img_name")
Port <- create.key(Port)
## END wrangling ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## data prep ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## rarify data, if necessary
HSIL_2 <- retain.rows(HSIL, 2)
HSIL_3 <- retain.rows(HSIL, 3)


## list of transects to retain
transect_list <- c(
  "2_1", 
  "2_2",
  "3_1",
  "3_2",
  "3_3",
  "4_1",
  "4_2",
  "5_1",
  "5_2",
  "5_3",
  "6_2",
  "6_3"
)


## output with filtered transects
Port <- filter.transects(Port, "key", transect_list)
## END HSILa prep ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## calculate summary metrics including density, sum total abundance ~~~~~~~~~~~~
## calculate density -- run on VIAME HSILa
HSIL_density_2 <- calculate.density(HSIL_2, "site", "transect", "SS_ochre", "SP_shiner", 3)
HSIL_density_3 <- calculate.density(HSIL_3, "site", "transect", "SS_ochre", "SP_shiner", 3)
Port_density <- calculate.density(Port, "site", "transect", "CR_kelp", "unknown", 3)


## total obs per each column
total_per_label_Port <- total.label.obs(Port, "key", "CR_kelp", "unknown")
total_per_label_HSIL <- total.label.obs(HSIL, "key", "SS_ochre", "SP_shiner")
total_per_label_HSIL_2 <- total.label.obs(HSIL_2, "key", "SS_ochre", "SP_shiner")
total_per_label_HSIL_3 <- total.label.obs(HSIL_3, "key", "SS_ochre", "SP_shiner")


## calculate sum total observations per transect
sum_total_Port <- sum_total_obs(Port, "key", "CR_kelp", "unknown")
sum_total_HSIL_2 <- sum_total_obs(HSIL_2, "key", "SS_ochre", "SP_shiner")
sum_total_HSIL_3 <- sum_total_obs(HSIL_3, "key", "SS_ochre", "SP_shiner")


## calculate total density
HSIL_total_density_2 <- calculate.total.density(HSIL_2, "site", "transect", "SS_ochre", "SP_shiner", 3)
HSIL_total_density_3 <- calculate.total.density(HSIL_3, "site", "transect", "SS_ochre", "SP_shiner", 3)
Port_total_density <- calculate.total.density(Port, "site", "transect", "CR_kelp", "unknown", 3)
## END summary calculations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





# Perform Wilcoxon signed-rank test for matched samples
wilcox.test(HSIL_density_2$density_SS_ochre, Port_density$density_SS_ochre, paired = TRUE)
wilcox.test(HSIL_density_3$density_SS_ochre, Port_density$density_SS_ochre, paired = TRUE)


wilcox.test(HSIL_density_2$density_SS_leather, Port_density$density_SS_leather, paired = TRUE)
wilcox.test(HSIL_density_3$density_SS_leather, Port_density$density_SS_leather, paired = TRUE)


wilcox.test(HSIL_density_2$density_CR_kelp, Port_density$density_CR_kelp, paired = TRUE)
wilcox.test(HSIL_density_3$density_CR_kelp, Port_density$density_CR_kelp, paired = TRUE)


wilcox.test(HSIL_total_density_2$total_density, Port_total_density$total_density, paired = TRUE)
wilcox.test(HSIL_total_density_3$total_density, Port_total_density$total_density, paired = TRUE)
## END Wilcox signed rank test ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END SCRIPT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
