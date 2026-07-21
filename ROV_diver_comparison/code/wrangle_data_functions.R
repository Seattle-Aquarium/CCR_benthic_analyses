## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## functions to work with Reef Check data
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



## function to rename columns 
rename_columns <- function(df, name_map) {
  
  if (is.null(names(name_map)) || any(names(name_map) == "")) {
    stop(
      "'name_map' must be a named vector using ",
      '"old_name" = "new_name".'
    )
  }
  
  old_names <- names(name_map)
  new_names <- unname(name_map)
  
  # Only rename columns that actually occur in the dataframe
  present <- old_names %in% names(df)
  old_present <- old_names[present]
  new_present <- new_names[present]
  
  # Prevent multiple existing columns from receiving the same name
  if (anyDuplicated(new_present)) {
    duplicate_names <- unique(new_present[duplicated(new_present)])
    
    stop(
      "Multiple existing columns would be renamed to: ",
      paste(duplicate_names, collapse = ", ")
    )
  }
  
  names(df)[match(old_present, names(df))] <- new_present
  
  df
}


## old and new Reef Check algae names
algae_name_map <- c(
  "Acid Weed"         = "acid_weed",
  "Broad-Ribbed Kelp" = "broad-ribbed_kelp",
  "Bull Kelp"         = "bull_kelp",
  "Feather Boa Kelp"  = "feather_boa_kelp",
  "Five-Ribbed Kelp"  = "kelp_five_rib",
  "Giant Kelp"        = "giant_kelp",
  "Sieve Kelp"        = "kelp_sieve",
  "Sugar Kelp"        = "kelp_sugar",
  "Three-Ribbed Kelp" = "kelp_three_rib",
  "Torn Kelp"         = "torn_kelp",
  "Winged Kelp"       = "winged_kelp",
  "Wire Weed"         = "brown_algae_sargassum",
  "Woody Kelp"        = "woody_kelp"
)


## old and new Reef Check column names 
invert_name_map <- c(
  "Rock Crab"                  = "cancer_crab",
  "Kelp Crab"                  = "kelp_crab",
  "Kelp Crab (Juvenile)"       = "kelp_crab_juv",
  "Slender Crab"               = "slender_crab",
  "Dungeness Crab"             = "dungeness_crab",
  "Green Crab"                 = "green_crab",
  "Leather Star"               = "leather_star",
  "Flat Fish"                  = "flat_fish",
  "Plumose Anemone"            = "plumose_anemone",
  "Rock Scallop"               = "scallop",
  "Orange Cucumber"            = "burrowing_sea_cucumber",
  "California Sea Cucumber"    = "california_sea_cucumber",
  "Large Anemone"              = "large_anemone",
  "Gumboot Chiton"             = "gumboot_chiton",
  "Blue Striped Star"          = "blue_striped_star",
  "Hairy Triton"               = "hairy_triton",
  "Short Spined Sea Star"      = "short_spined_star",
  "Giant Spined Star"          = "giant_spined_star",
  "Dawson sunstar"             = "dawson_star",
  "Sunflower Star"             = "sunflower_star",
  "Rainbow Star"               = "rainbow_star",
  "Bat Star"                   = "bat_star",
  "Blood Star"                 = "blood_star",
  "Dawson's Sun Star"          = "dawson_sun_star",
  "Red Urchin"                 = "red_urchin",
  "Purple Urchin"              = "purple_urchin",
  "Green/Pallid Urchin"        = "green_white_urchin",
  "Piddock Clam"               = "clam_siphon",
  "Giant Pacific Octopus"      = "giant_pacific_octopus",
  "Short Spined Star"          = "short_spined_star",
  "Blue Striped Sun Star"      = "blue_striped_sun_star",
  "Pinto Abalone"              = "pinto_abalone"
)

standardize.invert.cols <- function(df) {
  rename_columns(df, invert_name_map)
}


## list of sites to retain
sites_to_retain <- c("Sirens of Spring", 
                     "Elliot Bay Marina")


## rename specific entries in the 'site' column
old_vals <- c("Sirens of Spring", "Centennial Park", "Elliott Bay Marina", "Elliot Bay Marina")
new_vals <- c("Centennial_Park", "Centennial_Park", "Elliott_Bay_Marina", "Elliott_Bay_Marina")


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


## function to remove summer 2025
remove_summer_2025 <- function(df) {
  dplyr::filter(
    df,
    !Date %in% c("2025-08-28", "2025-08-29")
  )
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


## Long to wide form
compress.to.wide <- function(df, value_col, class_col) {
  df %>%
    group_by(Date, site, transect, !!sym(class_col)) %>%
    summarise(
      Total = sum(.data[[value_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = all_of(class_col),
      values_from = Total,
      values_fill = 0
    )
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


## calculate density. Prefer the `cols` argument (an explicit vector of column
## names) over start_col/end_col where possible: a start/end range is looked
## up by CURRENT column position, so if the df was previously reordered (e.g.
## by reorder.by.total(), which sorts columns by total value) the range can
## silently span the wrong columns -- it'll only "happen" to be correct if the
## intended start/end columns still rank highest/lowest by total.
calculate.density <- function(df, start_col = NULL, end_col = NULL, divisor, cols = NULL) {
  if (is.null(cols)) {
    start_idx <- which(names(df) == start_col)
    end_idx <- which(names(df) == end_col)

    if (length(start_idx) == 0 || length(end_idx) == 0 || start_idx > end_idx) {
      stop("Invalid start or end column names.")
    }

    cols <- names(df)[start_idx:end_idx]
  }

  # Compute density
  df[cols] <- round(df[cols] / divisor, 2)

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


## function to add depth column 
add.depth <- function(df) {
  df %>%
    dplyr::mutate(
      depth = dplyr::case_when(
        transect %in% 1:3 ~ "deep",
        transect %in% 4:6 ~ "shallow",
        TRUE ~ NA_character_
      ),
      .after = transect
    )
}


## function to add season column
add.season <- function(df) {
  df %>%
    dplyr::mutate(
      season = dplyr::case_when(
        format(as.Date(Date), "%m") == "01" ~ "winter",
        format(as.Date(Date), "%m") == "10" ~ "summer",
        TRUE ~ NA_character_
      ),
      .after = Date
    )
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


## function to re-order cols by total 
reorder.by.total <- function(df, start_col, end_col) {
  
  start_idx <- match(start_col, names(df))
  end_idx   <- match(end_col, names(df))
  
  if (is.na(start_idx) || is.na(end_idx) || start_idx > end_idx) {
    stop("Invalid start or end column names.")
  }
  
  target_cols <- names(df)[start_idx:end_idx]
  
  column_totals <- colSums(
    df[target_cols],
    na.rm = TRUE
  )
  
  ordered_cols <- names(
    sort(column_totals, decreasing = TRUE)
  )
  
  before_cols <- if (start_idx > 1) {
    names(df)[seq_len(start_idx - 1)]
  } else {
    character(0)
  }
  
  after_cols <- if (end_idx < ncol(df)) {
    names(df)[(end_idx + 1):ncol(df)]
  } else {
    character(0)
  }
  
  df[c(before_cols, ordered_cols, after_cols)]
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
    "SU_anth"     = "anthropogenic",
    "BR_encrust"  = "brown_algae_encrusting",
    "BR_fucus"    = "brown_algae_fucus",
    "GR_filam"    = "green_algae_filamentous",
    "KE_bull"     = "kelp_bull_blade",
    "KE_holdfas"  = "kelp_holdfast",
    "RE_CCA"      = "red_algae_cca",
    "RE_encrust"  = "red_algae_encrusting",
    "SI"          = "sessile_invertebrates",
    "SU_wood"     = "wood_debris"
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


rov_invert_name_map <- c(
  "Review"      = "review",
  "AN_large"    = "large_anemone",
  "AN_plumose"  = "plumose_anemone",
  "GA_gum"      = "gumboot_chiton",
  "GA_abalone"  = "abalone",
  "CL_siphon"   = "clam_siphon",
  "CL_scall"    = "scallop",
  "UR_purp"     = "purple_urchin",
  "UR_red"      = "red_urchin",
  "UR_green"    = "green_white_urchin",
  "SS_ochre"    = "ochre_mottled_star",
  "SS_leather"  = "leather_star",
  "SS_verm"     = "vermillion_star",
  "SS_blood"    = "blood_star",
  "SS_bat"      = "bat_seastar",
  "SS_pycno"    = "sunflower_star",
  "SS_rainbow"  = "rainbow_star",
  "SS_sun"      = "dawsons_sun_star",
  "SS_stripe"   = "striped_sun_star",
  "CU_burrow"   = "burrowing_sea_cucumber",
  "CU_cali"     = "california_sea_cucumber",
  "CR_cancer"   = "cancer_crab",
  "CR_kelp"     = "kelp_crab",
  "CR_helmet"   = "helmet_crab",
  "CR_sharp"    = "sharpnose_crab",
  "GR_kelp"     = "kelp_greenling",
  "GR_lingcod"  = "lingcod",
  "GR_painted"  = "painted_greenling",
  "GR_rock"     = "rock_greenling",
  "GR_whitesp"  = "white_spotted_greenling",
  "SP_kelp"     = "kelp_perch",
  "SP_pile"     = "pile_perch",
  "SP_shiner"   = "shiner_perch",
  "SP_stripe"   = "striped_seaperch",
  "RF_black"    = "black_deacon_rockfish",
  "RF_brown"    = "brown_rockfish",
  "RF_canary"   = "canary_rockfish",
  "RF_china"    = "china_rockfish",
  "RF_copper"   = "copper_rockfish",
  "RF_ytail"    = "yellow_tail_rockfish",
  "RF_Yeye"     = "yelloweye_rockfish",
  "fish_gunn"   = "gunnel_fish",
  "fish_cab"    = "cabezon_buffalo_sculpin",
  "fish_sculp"  = "other_large_sculpin",
  "fish_flat"   = "flat_fish",
  "fish_wolf"   = "wolf_eel"
)
# for use with ROV data: 
#invert <- rename.columns(invert, rov_invert_name_map)


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


## average by arbitrary grouping cols (e.g. site/transect/depth/season), rounded
## to 3 decimal places; also reports how many rows (photos) went into each mean.
## generalizes average.by.site.transect for datasets w/ more than one grouping
## factor (e.g. HSIL data, which has repeat surveys of the same site/transect
## across two seasons). Takes an explicit vector of column names (`cols`)
## rather than a start/end range, since a start/end range silently breaks if
## the columns were previously reordered (e.g. by reorder.by.total()).
average.by.group <- function(df, group_cols, cols) {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }

  df %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      n_photos = dplyr::n(),
      across(all_of(cols), ~ round(mean(.x, na.rm = TRUE), 3)),
      .groups = "drop"
    )
}


## sum by arbitrary grouping cols; companion to average.by.group(), used e.g.
## to sum per-photo point counts up to transect-level totals
sum.by.group <- function(df, group_cols, cols) {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }

  df %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      n_photos = dplyr::n(),
      across(all_of(cols), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )
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




