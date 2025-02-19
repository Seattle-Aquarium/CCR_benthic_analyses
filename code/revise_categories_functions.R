## function to remove characters from columns
remove.characters <- function(df, column_name, direction, num_chars) {
  trim_chars <- function(x, direction, num_chars) {
    if (direction == "left") {
      return(substr(x, num_chars + 1, nchar(x)))
    } else if (direction == "right") {
      return(substr(x, 1, nchar(x) - num_chars))
    } else {
      stop("Invalid direction. Use 'left' or 'right'.")
    }
  }
  
  df[[column_name]] <- sapply(df[[column_name]], trim_chars, direction = direction, num_chars = num_chars)
  return(df)
}


## function to reorder columns in a dataframe
reorder.cols <- function(data, cols) {
  other_cols <- setdiff(names(data), cols)
  data <- data[, c(cols, other_cols)]
  return(data)
}


## function to bring new column to the [1] position 
front.ofthe.line <- function(data){
  num.col <- ncol(data)
  data <- data[c(num.col, 1:num.col-1)]
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


## copy columns to new dataframe (create metadata df)
split.dataframe <- function(df, num_cols){
  new.df <- df[, 1:num_cols]
  return(new.df)
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


## delete columns once they have been combined
remove.columns <- function(df, columns_to_delete) {
  df <- df %>% select(-all_of(columns_to_delete))
  return(df)
}


## function that uses previous two functions to edit categories
revise.categories <- function(data, category_list, new_column){
  new_category <- category_list
  data <- combine.columns(data, new_category, new_column)
  data <- remove.columns(data, new_category)
  return(data)
}


## function to rename columns
rename.columns <- function(data, old, new) {
  names(data)[names(data) %in% old] <- new
  return(data)
}


## sample down to 50 rows when all categories have 50 or more rows
sample.data <- function(data, nrows, replace.logic){
  new.dat <- data %>%
    group_by(key) %>%
    sample_n(size = nrows, replace = replace.logic) %>%
    ungroup()
  return(new.dat)
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
count_rows <- function(data, col) {
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
  df <- df %>%
    mutate(across(where(is.numeric), ~ . * 100))
  return(df)
}



## function to calculate the average across columns and round to 2 decimal places
calculate.avg <- function(data, first.col, last.col){
  df <- data %>% 
    group_by(key) %>%
    summarize(across(all_of(first.col):all_of(last.col), \(x) mean(x, na.rm = TRUE))) %>%
    mutate(across(all_of(first.col):all_of(last.col), ~ round(., 2)))
  return(df)
}


## add site and transect back in once avgs have been calculated
reconstruct.metadata <- function(data){
  data$site <- data$key
  data$transect <- data$key
  data <- front.ofthe.line(data)
  data <- front.ofthe.line(data)
  data <- remove_characters(data, "site", "right", 2)
  data <- remove_characters(data, "transect", "left", 2)
  return(data)
}


## bind bull kelp stipes to averaged CoralNet data
bind.stipes <- function(data, stipe.df){
  data <- data %>%
    left_join(stipe.df %>% select(key, stipes, bundles), by = "key")
  data <- front.ofthe.line(data)
  data <- front.ofthe.line(data)
  return(data)
}


## function to sum all columns and double check we sum properly to 1
sum.columns <- function(dat, col_1, col_2) {
  dat$total_sum <- rowSums(dat[, col_1:col_2], na.rm = TRUE)
  dat <- front.ofthe.line(dat)
  return(dat)
}





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END function definition ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~