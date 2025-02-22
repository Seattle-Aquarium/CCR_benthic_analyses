## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## script with functions for calculating spp diveristy metrics ~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~






## Calculate diversity metrics
calculate.diversity <- function(species_data) {
  
  species_richness <- rowSums(species_data > 0)  # Count nonzero species
  shannon_index <- diversity(species_data, index = "shannon")
  simpson_index <- diversity(species_data, index = "simpson")
  pielou_evenness <- ifelse(species_richness > 1, shannon_index / log(species_richness), NA)
  
  diversity_df <- data.frame(
    Site = rownames(species_data),
    Species_Richness = species_richness,
    Shannon_Index = shannon_index,
    Simpson_Index = simpson_index,
    Pielou_Evenness = pielou_evenness  # New column for Pielou’s J
  )
  
  return(diversity_df)
}


## function to bind diversity and data
insert.df <- function(df_A, df_B, position) {
  df_B_part1 <- df_B[, 1:(position - 1), drop = FALSE]  # Columns before insertion
  df_B_part2 <- df_B[, position:ncol(df_B), drop = FALSE]  # Columns after insertion
  new_df <- cbind(df_B_part1, df_A, df_B_part2)
  return(new_df)
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


## function to remove unnecessary columns
delete.cols <- function(df, columns_to_remove) {
  df <- df[, !(colnames(df) %in% columns_to_remove), drop = FALSE]
  return(df)
}


## function to rename cols 
rename.cols <- function(df, rename_list) {
  colnames(df) <- ifelse(colnames(df) %in% names(rename_list), rename_list[colnames(df)], colnames(df))
  return(df)
}


## Function to multiple by 100
multiply.100 <- function(data, start_col, end_col) {
  data[, start_col:end_col] <- data[, start_col:end_col] * 100
  return(data)
}


## function to bring new column to the [1] position 
front.ofthe.line <- function(data){
  num.col <- ncol(data)
  data <- data[c(num.col, 1:num.col-1)]
  return(data)
}


## function to create a new column of data re: location
specify.location <- function(data) {
  data <- data %>%
    mutate(location = case_when(
      site %in% c(2, 3, 4) ~ "Elliott_Bay_Marina",
      site %in% c(5, 6, 7) ~ "Centennial_Park",
      site %in% c(1, 8) ~ "other",
      TRUE ~ NA_character_  # Ensures no unexpected values remain
    ))
  
  data <- front.ofthe.line(data)
  return(data)
}



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
