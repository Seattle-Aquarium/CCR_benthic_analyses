## function to bring new column to the [1] position 
front.ofthe.line <- function(data){
  num.col <- ncol(data)
  data <- data[c(num.col, 1:num.col-1)]
  return(data)
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


## function to create 1:nrow SU - sample unit - variable 
create.SU <- function(data){
  data$SU <- 1:nrow(data)
  data <- front.ofthe.line(data)
  return(data)
}


## revised function that uses column names, not positions
delete.columns <- function(data, first_col, last_col, cols_to_preserve) {
  cols_to_delete_names <- names(data)[which(names(data) %in% first_col):which(names(data) %in% last_col)]
  cols_to_delete_names <- setdiff(cols_to_delete_names, cols_to_preserve)
  data <- data[, !names(data) %in% cols_to_delete_names]
  return(data)
}


## combine columns - add together the CoralNet counts from multiple categories
combine.columns <- function(df, columns, new_column_name) {
  df <- df %>%
    mutate(!!sym(new_column_name) := rowSums(select(., all_of(columns))))
  return(df)
}

## revised function to sample down to a set number of rows, OR retain the minimum rows present
sample.down <- function(data, col, row_min) {
  data %>%
    group_by(across(all_of(col))) %>%
    reframe(slice_sample(pick(everything()), n = min(n(), row_min), replace = FALSE)) %>%
    ungroup()
}


## additional function that drops any factor with less than 50 rows. For rows with more than 50
## observation, randomly sample down to 50 rows. 
sample.down.filtered <- function(data, col, row_min) {
  data %>%
    group_by(across(all_of(col))) %>%
    filter(n() >= row_min) %>%  # Drop groups with fewer than row_min rows
    reframe(slice_sample(pick(everything()), n = row_min, replace = FALSE)) %>%
    ungroup()
}


## double check how many rows are present with each unique factor of col 
count.rows <- function(data, col) {
  data %>%
    group_by(!!sym(col)) %>%
    summarise(count = n()) %>%
    print(n=32)
}


## calculate percent-cover from CoralNet pt data
calculate.percent <- function(data, first.col, last.col, decimal.place){
   cols <- select(data, {{first.col}}:{{last.col}}) ## specify columns 
  data$original_data_pts <- rowSums(cols) ## calculate sum number of data points per image
  data <- front.ofthe.line(data) ## move sum column to front for easy viewing
  data <- data %>% ## divide cols by sum column to calculate %
    mutate(across(all_of(names(cols)), ~ . / original_data_pts))
  data <- data %>% ## round to 2 decimal places
    mutate(across(all_of(names(cols)), ~ round(., decimal.place)))
  return(data)
}


## function to print a dataframe with the number of photos per transect
photo.list <- function(data){
  transect_summary <- data %>%
    group_by(key) %>% summarise(count = n())
  colnames(transect_summary)[1] <- "transect"
  return(transect_summary)
}


## function to tabulate the total number of annotations
annotation.sum <- function(data, first.col, last.col){
  df <- data %>%
    summarise(across(all_of(first.col):all_of(last.col), sum, na.rm = TRUE))
  df$total_annotations <- sum(df[1, ])
  df <- front.ofthe.line(df)
  df <- df[, order(-as.numeric(df[1, ]))]
  df <- as.data.frame(t(df))
  df <- tibble::rownames_to_column(df, var = "category")
  colnames(df)[2] <- "total_annotations"
  return(df)
}


## calculate the range of depth values 
calculate.depth.spans <- function(dat) {

  dat$depth <- dat$depth * -1
  
  dat <- dat %>%
    group_by(site) %>%
    mutate(depth_site_span = max(depth, na.rm = TRUE) - min(depth, na.rm = TRUE)) %>%
    ungroup()
  
  dat <- dat %>%
    group_by(site, transect) %>%
    mutate(depth_transect_span = max(depth, na.rm = TRUE) - min(depth, na.rm = TRUE)) %>%
    ungroup()
  
  dat <- front.ofthe.line(dat)
  dat <- front.ofthe.line(dat)
  
  return(dat)
}




