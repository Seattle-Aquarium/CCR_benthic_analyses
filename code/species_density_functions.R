## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## functions to calculate species density metrics ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## drop every nth row
drop.rows <- function(df, n) {
  df[-seq(n, nrow(df), by = n), ]
}


## retain every nth row
retain.rows <- function(df, n) {
  df[seq(n, nrow(df), by = n), ]
}


## function to filter transects by a list
filter.transects <- function(df, transect_col, transect_list) {
  df %>% filter(!!sym(transect_col) %in% transect_list)
}


## filter sites
filter.sites <- function(df) {
  sites_to_keep <- c(2, 3, 4, 5, 6, 7)
  filtered_df <- df[df$site %in% sites_to_keep, ]
  return(filtered_df)
}


## function to insert a single column from a dataframe anywhere
move.col <- function(df, col, pos) {
  cols <- names(df)
  cols <- cols[cols != col]
  if (pos < 1) pos <- 1
  if (pos > length(cols) + 1) pos <- length(cols) + 1
  new_order <- append(cols, col, after = pos - 1)
  df <- df[, new_order]
  return(df)
}


## function to round a column off by X decimal points
decimal.round <- function(df, col, X) {
  df[[col]] <- round(df[[col]], digits = X)
  return(df)
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


## Function to calculate total density per transect within each site
calculate.total.density <- function(df, site_col, transect_col, start_col, end_col, round_digits) {
  df %>%
    group_by(!!sym(site_col), !!sym(transect_col)) %>%
    summarise(total_density = round(sum(across(all_of(start_col):all_of(end_col)), na.rm = TRUE) / n(), round_digits),
              .groups = "drop") -> total_density_df
  
  return(total_density_df)
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


## remove characters from column
rm.chars <- function(df, col, X, Y = "left") {
  df[[col]] <- as.character(df[[col]])
  if (Y == "left") {
    df[[col]] <- sapply(df[[col]], function(x) {
      if(nchar(x) <= X) {
        ""
      } else {
        substr(x, X + 1, nchar(x))
      }
    })
  } else if (Y == "right") {
    df[[col]] <- sapply(df[[col]], function(x) {
      if(nchar(x) <= X) {
        ""
      } else {
        substr(x, 1, nchar(x) - X)
      }
    })
  } else {
    stop("Y must be either 'left' or 'right'")
  }
  
  return(df)
}


## function to rename columns
rename.columns <- function(data, old, new) {
  names(data)[names(data) %in% old] <- new
  return(data)
}


## function to create a unique key identifier combining site and transect info
create.key <- function(data){
  data$key <- data$site
  data$key <- with(data, paste0(key,"_",transect))
  data <- front.ofthe.line(data)
  return(data)
}


## function to bring new column to the [1] position 
front.ofthe.line <- function(data){
  num.col <- ncol(data)
  data <- data[c(num.col, 1:num.col-1)]
  return(data)
}


## function to rename sites
rename.site <- function(df) {
  df$site <- as.character(df$site)
  df$site[df$site == "Centennial"] <- 6
  df$site[df$site == "EBM"] <- 3
  df$site <- factor(df$site)
  return(df)
}
## END functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
