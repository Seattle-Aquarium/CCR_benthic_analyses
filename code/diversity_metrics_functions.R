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




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
