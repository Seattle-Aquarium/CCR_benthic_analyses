## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## CoralNet data processing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())


## add libraries
library(tidyverse)
library(reshape)


## list out current path
getwd()


## hardcode relative file paths
code <- "../code"
data_input <- "../data_input"
data_output <- "../data_output"
figs <- "../figs"


## invoke relative file path 
setwd(data_output)


## read in csv file 
dat <- read.csv("revised_CoralNet_categories.csv")


## invoke functions from other script
setwd(code)
source("revise_CoralNet_categories.R")
remove(dat2, metadata)
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## function to sample rows by group, with multiple separate draws ~~~~~~~~~~~~~~

## function simulates multiple transects from a single data source
# Define the function for a single column
random_sampling_by_group <- function(data_frame, key_column, value_column, n_samples, n_draws) {
  # Nested function to perform a single draw within a group
  single_draw <- function(data, n) {
    data[sample(nrow(data), size = n, replace = FALSE), ]
  }
  
  # Apply the sampling function to each group and repeat for the specified number of draws
  result_list <- lapply(1:n_draws, function(draw_num) {
    data_frame %>%
      group_by(across(all_of(key_column))) %>%
      group_modify(~ single_draw(.x, n_samples)) %>%
      mutate(draw = draw_num)
  })
  
  # Combine the list of results into a single dataframe
  combined_result_df <- bind_rows(result_list)
  
  return(combined_result_df)
}

# Define a wrapper function to apply the sampling to multiple columns
random_sampling_multiple_columns <- function(data_frame, key_column, value_columns, n_samples, n_draws) {
  # Initialize the combined result dataframe with draw and key columns
  combined_result_df <- random_sampling_by_group(data_frame, key_column, value_columns[1], n_samples, n_draws) %>%
    select(draw, all_of(key_column))
  
  for (col_name in value_columns) {
    sampled_df <- random_sampling_by_group(data_frame, key_column, col_name, n_samples, n_draws)
    combined_result_df[[col_name]] <- sampled_df[[col_name]]
  }
  
  return(combined_result_df)
}
## END function to simulate transects ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## invoke functions to simulate transects & wrangle data ~~~~~~~~~~~~~~~~~~~~~~~

## invoke the functions on real data 
dat <- random_sampling_multiple_columns(dat, "key", c("kelp_bryozoan",
                                                       "soft_sediment",
                                                       "cobble",
                                                       "pebble",
                                                       "unknown",
                                                       "filamentous_brown",
                                                       "acid_weed",
                                                       "rock_weed",
                                                       "kelp_holdfast",
                                                       "sargassum",
                                                       "sugar_kelp",
                                                       "green_algae",
                                                       "textured_kelp",
                                                       "smooth_kelp",
                                                       "other_brown_kelp",
                                                       "red_algae",
                                                       "shell_debris",
                                                       "anthro_substrate",
                                                       "seagrass",
                                                       "sessile_invert",
                                                       "mobile_invert",
                                                       "coralline_algae"), 25, 4)



## clean up simulated data prior to analyses - rename cols
names(dat)[names(dat) == "draw"] <- "transect"
names(dat)[names(dat) == "key"] <- "site"


## order by key/site - key no longer fully captures "site_transect" formating, so
## we will restructire it
dat <- dat %>% arrange(site)


## transform key to site label
dat <- remove_characters(dat, "site", "right", 2)


## create key using proper "site_transect" labeling
dat <- create.key(dat)
dat$transect <- as.factor(dat$transect)


## save dat
setwd(data_output)
write.csv(dat, "simulated_real_transects.csv", row.names=FALSE)
## END data structuring - we are now ready to take an average ~~~~~~~~~~~~~~~~~~





## take the average of each transect ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
avg <- dat %>% 
  group_by(key) %>%
  summarize(across(3:24, \(x) mean(x, na.rm = TRUE)))


## bring site and transect information into avg
avg$transect <- avg$key
avg <- front.ofthe.line(avg)
avg <- remove_characters(avg, "transect", "left", 2)


avg$site <- avg$key
avg <- front.ofthe.line(avg)
avg <- remove_characters(avg, "site", "right", 2)


