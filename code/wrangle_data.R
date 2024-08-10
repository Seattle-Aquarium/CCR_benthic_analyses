## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## CoralNet data processing and category editing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
data_input <- "data_input"
data_output <- "data_output"
figs <- "figs"
urban_kelp <- "data_output/urban_kelp"


## source functions 
source(file.path(code, "revise_categories_functions.R"))


dat <- read.csv(file.path(urban_kelp, "2022_revised_categories.csv"))
#dat <- read.csv(file.path(urban_kelp, "2022_all_current_photos.csv"))
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## invoke function to calculate percent-cover ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat <- calculate.percent(dat, soft_sediment, mobile_invert, 2)


## add total percent column at front of data.frame - check that sums to 1 
dat$sum <- rowSums(select(dat, soft_sediment:mobile_invert))
dat <- front.ofthe.line(dat)


## function to calculate the average across columns and round to 2 decimal places
avg_all <- calculate.avg(dat, "soft_sediment", "mobile_invert")
avg_all <- reconstruct.metadata(avg_all)


## open or save csv 
# dat <- read.csv(file.path(urban_kelp, "2022_all_current_photos.csv"))
# write.csv(revised, file.path(urban_kelp, "2022_all_current_photos.csv"), row.names=FALSE)
# write.csv(avg_all, file.path(urban_kelp, "2022_avg_all.csv"), row.names=FALSE)
## END export and revised data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## pull in and bind bull kelp stipes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stipes <- read.csv(file.path(data_input, "bull_kelp_stipes.csv"))
stipes <- create.key(stipes)


## bind stipes to avg_all based on key
avg_all <- bind.stipes(avg_all, stipes)


## save csv w/ bull kelp stipes and category averages
#write.csv(avg_all, file.path(urban_kelp, "2022_avg_all.csv"), row.names=FALSE)
## END bull kelp stipes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## filter and sample down to 50pts per image ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#T1_T2 <- revised %>% filter(transect %in% c("1", "2"))
#count_rows(T1_T2, "key")


## sample down to 50 per transect
#T1_T2 <- sample.data(T1_T2, 50, FALSE)
#count_rows(T1_T2, "key")


## calculate category sums and save 
# sum_T1_T2 <- annotation.sum(T1_T2, "soft_sediment", "mobile_invert")
# write.csv(sum_T1_T2, file.path(urban_kelp, "sum_2022_T1_T2.csv"))


## save the data frame 
# T1_T2 <- read.csv(file.path(urban_kelp, "2022_T1_T2.csv"))
# write.csv(T1_T2, file.path(urban_kelp, "2022_T1_T2.csv"), row.names = FALSE)


## calculate and save photo list
# T1_T2_list <- photo.list(T1_T2)
# write.csv(T1_T2_list, file.path(urban_kelp, "2022_T1_T2_photos.csv"), row.names=FALSE)


## filter down to T3 
T3 <- dat %>% filter(transect %in% c("1", "2", "3"))


## sample 50 rows, or however many present, for T1, T2, and T3 
row_min <- 50
T3 <- sample.down(T3, "key", row_min)


## test how many rows are present - double check 
count_rows(T3, "key")


## function to calculate the average across columns and round to 2 decimal places
avg_T3 <- calculate.avg(T3, "soft_sediment", "mobile_invert")
avg_T3 <- reconstruct.metadata(avg_T3)


## read in stipes
#stipes <- read.csv(file.path(data_input, "bull_kelp_stipes.csv"))
#stipes <- create.key(stipes)


## bind stipes
avg_T3 <- bind.stipes(avg_T3, stipes)


## save avg'd data
#write.csv(avg_T3, file.path(urban_kelp, "2022_avg_T1_T2_T3.csv"), row.names=FALSE)


## calculate category sums and save 
# sum_T1_T2_T3 <- annotation.sum(T1_T2_T3, "soft_sediment", "mobile_invert")
# write.csv(sum_T1_T2_T3, file.path(urban_kelp, "sum_2022_T1_T2_T3.csv"))


## save or read the data
# T1_T2_T3 <- read.csv(file.path(urban_kelp, "2022_T1_T2_T3.csv"))
# write.csv(T1_T2_T3, file.path(urban_kelp, "2022_T1_T2_T3.csv"), row.names = FALSE)


## calculate and save photo list
#T1_T2_T3_list <- photo.list(T1_T2_T3)
#write.csv(T1_T2_T3_list, file.path(urban_kelp, "2022_T1_T2_T3_photos.csv"), row.names = FALSE)
## end data filtering and csv export ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
