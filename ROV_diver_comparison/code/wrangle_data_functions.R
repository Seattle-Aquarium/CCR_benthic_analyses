## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## functions to work with Reef Check data
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## list of sites to retain
sites_to_retain <- c("Sirens of Spring", 
                     "Elliot Bay Marina")


## rename specific entries in the 'site' column
old_vals <- c("Sirens of Spring", "Centennial Park", "Elliott Bay Marina", "Elliot Bay Marina")
new_vals <- c("Centennial_Park", "Centennial_Park", "Elliott_Bay_Marina", "Elliott_Bay_Marina")


## list of Reef Check names to change 
input_algae_list <- c("5-Ribbed Kelp", 
                      "Sieve Kelp", 
                      "Sugar Kelp", 
                      "Wire Weed")


## new names for algae 
output_algae_list <- c("kelp_five_rib", 
                       "kelp_sieve", 
                       "kelp_sugar",
                       "brown_algae_sargassum")


## define the filtering function
filter.and.sort <- function(df, sites_to_retain) {
  df_filtered <- df %>%
    filter(Site %in% sites_to_retain)
  
  sort_cols <- c("Site", "Transect")
  if ("Category" %in% colnames(df_filtered)) {
    sort_cols <- c(sort_cols, "Category")
  }
  
  df_filtered %>%
    arrange(across(all_of(sort_cols)))
}


## function to rename factor 
rename.factor <- function(df, col, old, new) {
  df[[col]] <- forcats::fct_recode(df[[col]], !!new := old)
  return(df)
}


## function to strip n characters off a column
remove.chars <- function(df, col, n) {
  df %>% mutate(
    {{col}} := str_sub(as.character({{col}}), n + 1)
    )
}


## function to rename columns
rename.columns <- function(df, old_names, new_names) {
  if (length(old_names) != length(new_names)) {
    stop("The length of 'old_names' and 'new_names' must be the same.")
  }
  
  names(df)[names(df) %in% old_names] <- new_names[match(names(df)[names(df) %in% old_names], old_names)]
  return(df)
}


## long to wide form
compress.to.wide <- function(df, value_col, class_col) {
  df %>%
    group_by(site, transect, !!sym(class_col)) %>%
    summarise(Total = sum(.data[[value_col]], na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = all_of(class_col), values_from = Total, values_fill = 0)
}


## function to extrapolate abundance in instances of subsampling 
extrapolate.abundance <- function(df, amount_col, distance_col) {
  df <- df %>%
    mutate(
      extrapolated = case_when(
        !is.na(.data[[distance_col]]) & .data[[distance_col]] != 30 ~ 
          round((.data[[amount_col]] / .data[[distance_col]]) * 30, 1),
        TRUE ~ round(.data[[amount_col]], 1)
      )
    )
  return(df)
}


## combine UPC names
combine.UPC.names <- function(df) {
  df %>% mutate(combined_name = ifelse(grepl("^Superlayer", Classcode),
                                       Classcode, paste(Category, Classcode, sep = "_")
  ))
}


## calculate density:
calculate.density <- function(df, start_col, end_col, divisor) {
  start_idx <- which(names(df) == start_col)
  end_idx <- which(names(df) == end_col)
  
  if (length(start_idx) == 0 || length(end_idx) == 0 || start_idx > end_idx) {
    stop("Invalid start or end column names.")
  }
  
  # Compute density
  df[start_idx:end_idx] <- round(df[start_idx:end_idx] / divisor, 2)
  
  return(df)
}


## retain every nth row
nth.row <- function(df, n) {
  df[seq(1, nrow(df), by = n), ]
}


## delete cols
delete.cols <- function(df, cols_to_remove) {
  df <- df %>% select(-all_of(cols_to_remove))
  return(df)
}


## function to save.csv 
save.csv <- function(df, path, filename){
  write.csv(x = df,
            file = file.path(path, filename),
            row.names= FALSE)
}


## populate ROV dataframe with columns from diver data
add.Reef.Check.inverts <- function(source_df, receiver_df) {
  cols_to_add <- setdiff(names(source_df), names(receiver_df))
  for (col in cols_to_add) {
    receiver_df[[col]] <- 0
  }
  return(receiver_df)
}


## standardize diver invert column names to match those of the ROV
standardize.invert.cols <- function(df) {
  name_map <- c(
    "Rock Crab" = "cancer_crab",
    "Kelp Crab" = "kelp_crab",
    "Kelp Crab (juvenile)" = "kelp_crab_juv",
    "Slender Crab" = "slender_crab",
    "Dungeness Crab" = "dungeness_crab",
    "Green Crab" = "green_crab",
    "Leather Star" = "leather_star",
    "Flat Fish" = "flat_fish",
    "Plumose Anemone" = "plumose_anemone",
    "Rock Scallop" = "scallop",
    "Orange Cucumber" = "burrowing_sea_cucumber",
    "California Sea Cucumber" = "california_sea_cucumber",
    "Blood star" = "blood_star",
    "Large Anemone" = "large_anemone",
    "Gumboot Chiton" = "gumboot_chiton",
    "Blue Striped Star" = "blue_striped_star",
    "Hairy Triton" = "hairy_triton",
    "Short Spined Sea Star" = "short_spined_star",
    "Giant Spined Star" = "giant_spined_star",
    "Dawson sunstar" = "dawson_star",
    "Sunflower Star" = "sunflower_star",
    "Rainbow Star" = "rainbow_star",
    "Bat Star" = "bat_star",
    "Red Urchin" = "red_urchin",
    "Purple Urchin" = "purple_urchin",
    "Green/Pallid Urchin" = "green_white_urchin",
    "Piddock Clam" = "clam_siphon",
    "Giant Pacific Octopus" = "giant_pacific_octopus",
    "Pinto Abalone" = "pinto_abalone"
  )
  
  flipped_map <- setNames(names(name_map), name_map)
  df <- df %>% rename(any_of(flipped_map))
  return(df)
}


## ensure ROV column headers are consistent
consistent.labels <- function(df) {
  names_map <- c(
    "BR_filam"    = "brown_algae_filamentous",
    "GR_fil"      = "green_algae_filamentous",
    "BR_sarg"     = "brown_algae_sargassum",
    "GR_ulva"     = "green_algae_ulva",
    "RE_bush"     = "red_algae_bushy",
    "RE_fil"      = "red_algae_filamentous",
    "RE_leaf"     = "red_algae_flat_leaf",
    "SU_bould"    = "boulder",
    "SU_cob"      = "cobble",
    "SU_peb"      = "pebble",
    "SU_silt"     = "silt",
    "KE_sugar"    = "kelp_sugar",
    "RE_branch"   = "red_algae_branching",
    "SU_sand"     = "sand_fine_shell",
    "unknown"     = "unknown_area",
    "KE_sieve"    = "kelp_sieve",
    "KE_5rib"     = "kelp_five_rib",
    "SU_shell"    = "shell_hash",
    "MS"          = "mobile_species",
    "KE_stipe"    = "kelp_stipe",
    "SI_kelpBry"  = "kelp_bryozoan",
    "SU_anth"     = "anthropogenic"
  )
  
  # Rename columns using the map (if present in df)
  renamed_df <- df
  for (old_name in names(names_map)) {
    if (old_name %in% colnames(df)) {
      colnames(renamed_df)[colnames(renamed_df) == old_name] <- names_map[[old_name]]
    }
  }
  
  return(renamed_df)
}


## rename colums
old_names <- c(
  "Review",
  "AN_large",
  "AN_plumose",
  "GA_gum",
  "GA_abalone",
  "CL_siphon",
  "CL_scall",
  "UR_purp",
  "UR_red",
  "UR_green",
  "SS_ochre",
  "SS_leather",
  "SS_verm",
  "SS_blood",
  "SS_bat",
  "SS_pycno",
  "SS_rainbow",
  "SS_sun",
  "SS_stripe",
  "CU_burrow",
  "CU_cali",
  "CR_cancer",
  "CR_kelp",
  "CR_helmet",
  "CR_sharp",
  "GR_kelp",
  "GR_lingcod",
  "GR_painted",
  "GR_rock",
  "GR_whitesp",
  "SP_kelp",
  "SP_pile",
  "SP_shiner",
  "SP_stripe",
  "RF_black",
  "RF_brown",
  "RF_canary",
  "RF_china",
  "RF_copper",
  "RF_ytail",
  "RF_Yeye",
  "fish_gunn",
  "fish_cab",
  "fish_sculp",
  "fish_flat",
  "fish_wolf"
)

new_names <- c(
  "Review",
  "large_anemone",
  "plumose_anemone",
  "gumboot_chiton",
  "abalone",
  "clam_siphon",
  "scallop",
  "purple_urchin",
  "red_urchin",
  "green_white_urchin",
  "ochre_mottled_star",
  "leather_star",
  "vermillion_star",
  "blood_star",
  "bat_seastar",
  "sunflower_star",
  "rainbow_star",
  "Dawsons_sun_star",
  "striped_sun_star",
  "burrowing_sea_cucumber",
  "california_sea_cucumber",
  "cancer_crab",
  "kelp_crab",
  "helmet_crab",
  "sharpnose_crab",
  "kelp_greenling",
  "lingcod",
  "painted_greenling",
  "rock_greenling",
  "white_spotted_greenling",
  "kelp_perch",
  "pile_perch",
  "shiner_perch",
  "striped_seaperch",
  "black_deacon_rockfish",
  "brown_rockfish",
  "canary_rockfish",
  "china_rockfish",
  "copper_rockfish",
  "yellow_tail_rockfish",
  "yelloweye_rockfish",
  "gunnel_fish",
  "cabezon_buffalo_sculpin",
  "other_large_sculpin",
  "flat_fish",
  "wolf_eel"
)


## function to stack dataframes and fill in the missing info with 0's
stack.dfs <- function(df1, df2) {
  all_cols <- union(names(df1), names(df2))
  
  for (col in setdiff(all_cols, names(df1))) {
    df1[[col]] <- 0
  }
  
  for (col in setdiff(all_cols, names(df2))) {
    df2[[col]] <- 0
  }
  
  df1 <- df1[, all_cols]
  df2 <- df2[, all_cols]
  
  combined <- rbind(df1, df2)
  
  return(combined)
}



combine.cols <- function(df, cols_to_combine, new_col_name) {
  # Check if all columns exist
  missing_cols <- setdiff(cols_to_combine, names(df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Create the new combined column
  df[[new_col_name]] <- rowSums(df[, cols_to_combine], na.rm = TRUE)
  
  return(df)
}



rename.cells <- function(df, column, old_names, new_names) {
  if (length(old_names) != length(new_names)) {
    stop("The length of 'old_names' and 'new_names' must be equal.")
  }
  
  name_map <- setNames(new_names, old_names)
  df[[column]] <- dplyr::recode(df[[column]], !!!name_map)

  return(df)
}


## rename metadata
rename.metadata <- function(df) {
  colnames(df)[colnames(df) == "Site"] <- "site"
  colnames(df)[colnames(df) == "Transect"] <- "transect"
  return(df)
}


## summarize by site / transect 
summarize.by.site.transect <- function(df, start_col, end_col) {
  start_idx <- which(names(df) == start_col)
  end_idx <- which(names(df) == end_col)
  
  if (length(start_idx) == 0 || length(end_idx) == 0 || start_idx > end_idx) {
    stop("Invalid start_col or end_col")
  }
  
  cols_to_sum <- names(df)[start_idx:end_idx]
  
  # Group and summarize
  df %>%
    group_by(site, transect) %>%
    summarise(across(all_of(cols_to_sum), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
}


## average by site / transect, rounded to 2 decimal places
average.by.site.transect <- function(df, start_col, end_col) {
  start_idx <- which(names(df) == start_col)
  end_idx <- which(names(df) == end_col)
  
  if (length(start_idx) == 0 || length(end_idx) == 0 || start_idx > end_idx) {
    stop("Invalid start_col or end_col")
  }
  
  cols_to_avg <- names(df)[start_idx:end_idx]
  
  # Group, calculate mean, and round to 2 decimal places
  df %>%
    group_by(site, transect) %>%
    summarise(across(all_of(cols_to_avg), ~ round(mean(.x, na.rm = TRUE), 3)), .groups = "drop")
}


## function to combine columns 
# Requires: dplyr, tidyr (and optionally forcats if you want factor handling)

combine_with_zero_fill <- function(df_a, df_b, fill_value = 0) {
  stopifnot(is.data.frame(df_a), is.data.frame(df_b))
  
  # Columns unique to each input
  only_a <- setdiff(names(df_a), names(df_b))
  only_b <- setdiff(names(df_b), names(df_a))
  cols_to_fill <- c(only_a, only_b)
  
  # Row-bind; dplyr::bind_rows will create missing columns with NA
  out <- dplyr::bind_rows(df_a, df_b)
  
  # Replace NAs **only** in columns that were absent in one of the inputs
  if (length(cols_to_fill) > 0) {
    out <- out |>
      dplyr::mutate(dplyr::across(
        dplyr::all_of(cols_to_fill),
        ~ {
          # Fill with 0 while respecting column type
          if (is.integer(.x)) {
            tidyr::replace_na(.x, as.integer(fill_value))
          } else if (is.numeric(.x)) {
            tidyr::replace_na(.x, as.numeric(fill_value))
          } else if (is.logical(.x)) {
            # Interpret 0/"0"/FALSE as FALSE, otherwise TRUE
            fv <- if (is.character(fill_value)) tolower(fill_value) else fill_value
            tidyr::replace_na(.x, isTRUE(fv) || (!is.character(fv) && fv != 0))
          } else {
            # For character/factor/other types, fill with "0"
            # (Adjust here if you'd prefer to leave these as NA instead)
            tidyr::replace_na(as.character(.x), "0")
          }
        }
      ))
  }
  
  out
}


## add columns and name them
add_column <- function(df, col_name, col_value, position) {

  n_cols <- ncol(df)
    new_col <- rep(col_value, nrow(df))
  
  before <- df[ , seq_len(position - 1), drop = FALSE]
  after  <- df[ , seq(from = position, to = n_cols), drop = FALSE]
  
  new_df <- cbind(before, setNames(list(new_col), col_name), after)
  rownames(new_df) <- rownames(df)  # preserve rownames if any
  
  return(new_df)
}


## add depth
add_depth_column <- function(df, position) {
  stopifnot("transect" %in% names(df))
  
  n_cols <- ncol(df)
  if (position < 1 || position > (n_cols + 1)) {
    stop("Position must be between 1 and ", n_cols + 1)
  }
  
  # create depth column based on transect values
  depth <- ifelse(df$transect %in% 1:3, "deep",
                  ifelse(df$transect %in% 4:6, "shallow", NA))
  
  # split before/after & insert depth
  before <- df[ , seq_len(position - 1), drop = FALSE]
  after  <- df[ , seq(from = position, to = n_cols), drop = FALSE]
  
  new_df <- cbind(before, depth = depth, after)
  rownames(new_df) <- rownames(df)
  
  return(new_df)
}




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




