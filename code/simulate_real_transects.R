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


# Example usage of the functions 
data_frame <- data.frame(
  key = rep(letters[1:3], each = 100), # Example key column with 3 groups
  value1 = rnorm(300), # Example value column 1 with 300 rows
  value2 = rnorm(300), # Example value column 2 with 300 rows
  value3 = rnorm(300)  # Example value column 3 with 300 rows
)

# Apply the function to draw 10 samples from each group, repeated 4 times, for multiple columns
draws_df <- random_sampling_multiple_columns(data_frame, "key", c("value1", "value2", "value3"), 10, 4)


## invoke the functions on real data 
dat2 <- random_sampling_multiple_columns(dat, "key", c("kelp_bryozoan",
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



