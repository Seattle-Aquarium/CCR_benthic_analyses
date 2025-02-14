## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## script with functions for calculating spp diveristy metrics ~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## calculate diversity metrics
calculate.diversity <- function(species_data) {
  
  species_richness <- rowSums(species_data > 0)  # Count number of nonzero species
  shannon_index <- diversity(species_data, index = "shannon")
  simpson_index <- diversity(species_data, index = "simpson")
  
  diversity_df <- data.frame(
    Site = rownames(species_data),
    Species_Richness = species_richness,
    Shannon_Index = shannon_index,
    Simpson_Index = simpson_index
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





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
