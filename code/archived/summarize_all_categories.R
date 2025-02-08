## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## CoralNet data processing and category editing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())


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
annotations_sum <- "../data_output/annotations_summary"


## source functions 
source(file.path(code, "revise_categories_functions.R"))


## read in csv file 
dat <- read.csv(file.path(data_input, "annotations.csv"))
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## calculate sum total annotations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## delete x, y data from photo as it's not needed for these summary data
dat <- dat[, -c(2,3)]


## summarize data
image.sum <- dat %>%
  group_by(name, label) %>%
  summarise(count = n()) %>%
  spread(key = label, value = count, fill = 0)


## Calculate the sum of each column from 2 to 54
column_sums <- as.data.frame(t(apply(image.sum[, 2:54], 2, sum)))
rownames(column_sums)[1] <- "total annotations"


## order data from greatest to least
ordered <- column_sums[, order(-as.numeric(column_sums[1, ]))]


## calculate column with 
ordered$total_annotations <- sum(ordered[1, ])
ordered_cols <- front.ofthe.line(ordered)
remove(ordered, image.sum, column_sums)


## save as list for pasting elsewhere (e.g., Git)
ordered_list <- as.data.frame(t(ordered_cols))


## save raw data
# write.csv(ordered_cols, file.path(annotations_sum, "sum_of_all_categories_by_column.csv"), row.names = FALSE)
# write.csv(ordered_list, file.path(annotations_sum, "sum_of_all_categories_by_list.csv"), row.names = FALSE)
## END sum calculation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
