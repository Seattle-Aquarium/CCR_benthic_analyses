## drop every nth row
drop.rows <- function(df, n) {
  df[-seq(n, nrow(df), by = n), ]
}


## function to filter transects by a list
filter.transects <- function(df, transect_col, transect_list) {
  df %>% filter(!!sym(transect_col) %in% transect_list)
}


## function to calculate density
calculate.density <- function(df, site_col, transect_col, start_col, end_col, round_digits) {
  df %>%
    group_by(!!sym(site_col), !!sym(transect_col)) %>%
    summarise(across(all_of(start_col):all_of(end_col), 
                     ~ round(sum(.x, na.rm = TRUE) / n(), round_digits), 
                     .names = "density_{.col}"),
              .groups = "drop") -> density_df
  
  return(density_df)
}


## calculate the total number of observations per spp per transect
total.label.obs <- function(df, transect_col, start_col, end_col) {
  df %>% group_by(!!sym(transect_col)) %>%
    summarise(across(all_of(start_col):all_of(end_col), 
                     ~ sum(.x, na.rm = TRUE), 
                     .names = "total_{.col}"),
              .groups = "drop") -> total_df
  
  return(total_df)
}


## sum total observations
sum_total_obs <- function(df, transect_col, start_col, end_col) {
  df %>%
    select(all_of(transect_col), all_of(start_col):all_of(end_col)) %>%  # Keep only relevant columns
    rowwise() %>%
    mutate(row_total = sum(c_across(all_of(start_col):all_of(end_col)), na.rm = TRUE)) %>%  # Sum across columns
    ungroup() %>%
    group_by(!!sym(transect_col)) %>%
    summarise(total_observations = sum(row_total, na.rm = TRUE), .groups = "drop")  # Sum within each transect
}