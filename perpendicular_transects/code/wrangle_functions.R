## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## functions for wrangling perpendicular-transect percent-cover data ~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## the full Toolbox label set used to annotate these transects (read from the
## label_map embedded in T2's cleaned Toolbox JSON export), short code ->
## descriptive name. Every transect gets every one of these columns, zero-
## filled where a label never occurs, so transects can be stacked and compared
## column-for-column. Descriptive names follow rov_code_map in
## ROV_diver_comparison/code/wrangle_data_functions.R; BR_acid, KE_ribbon, and
## SG are in this label set but not that one.
label_set <- c(
  "SU_silt"    = "silt",
  "SU_sand"    = "sand_fine_shell",
  "SU_shell"   = "shell_hash",
  "SU_peb"     = "pebble",
  "SU_cob"     = "cobble",
  "SU_bould"   = "boulder",
  "SU_anth"    = "anthropogenic",
  "SU_wood"    = "wood_debris",
  "KE_sugar"   = "kelp_sugar",
  "KE_sieve"   = "kelp_sieve",
  "KE_5rib"    = "kelp_five_rib",
  "KE_ribbon"  = "kelp_ribbon",
  "KE_bull"    = "kelp_bull_blade",
  "KE_stipe"   = "kelp_stipe",
  "KE_holdfas" = "kelp_holdfast",
  "BR_filam"   = "brown_algae_filamentous",
  "BR_sarg"    = "brown_algae_sargassum",
  "BR_fucus"   = "brown_algae_fucus",
  "BR_acid"    = "brown_algae_acid_weed",
  "BR_encrust" = "brown_algae_encrusting",
  "RE_branch"  = "red_algae_branching",
  "RE_bush"    = "red_algae_bushy",
  "RE_fil"     = "red_algae_filamentous",
  "RE_leaf"    = "red_algae_flat_leaf",
  "RE_CCA"     = "red_algae_cca",
  "RE_encrust" = "red_algae_encrusting",
  "GR_ulva"    = "green_algae_ulva",
  "GR_filam"   = "green_algae_filamentous",
  "SG"         = "seagrass",
  "MS"         = "mobile_species",
  "SI"         = "sessile_invertebrates",
  "SI_kelpBry" = "kelp_bryozoan",
  "unknown"    = "unknown_area"
)


## label-code spellings that differ between transects' Toolbox projects ->
## the label_set spelling. T1 was annotated with a label set that spelled
## filamentous red algae "RE_filam"; T2 (and rov_code_map) use "RE_fil".
label_aliases <- c("RE_filam" = "RE_fil")


## the nine presentation groups every figure colors by. Order here is the
## stacking order (bottom -> top) and legend order: substrate first (the
## "ground"), then the algae growing on it. Every label_set code belongs to
## exactly one group.
cover_groups <- list(
  "Silt"            = c("SU_silt"),
  "Sand & shell"    = c("SU_sand", "SU_shell"),
  "Hard substrate"  = c("SU_peb", "SU_cob", "SU_bould", "SU_anth"),
  "Sugar kelp"      = c("KE_sugar"),
  "Sieve kelp"      = c("KE_sieve"),
  "Other brown algae" = c("KE_5rib", "KE_ribbon", "KE_bull", "KE_stipe", "KE_holdfas",
                          "BR_filam", "BR_sarg", "BR_fucus", "BR_acid", "BR_encrust"),
  "Red algae"       = c("RE_branch", "RE_bush", "RE_fil", "RE_leaf", "RE_CCA", "RE_encrust"),
  "Green algae"     = c("GR_ulva", "GR_filam", "SG"),
  "Other"           = c("MS", "SI", "SI_kelpBry", "SU_wood", "unknown")
)
stopifnot(setequal(unlist(cover_groups), names(label_set)),
          !anyDuplicated(unlist(cover_groups)))


## read one transect's photo-level percent-cover + telemetry CSV, apply
## label_aliases, and add every label_set code missing from it as a zero column.
## Any category column that is NOT in label_set stops the read rather than
## being silently dropped. Columns after VFR_alt are the category columns.
read.transect <- function(path) {
  df <- read_csv(path, show_col_types = FALSE)
  first_cat <- which(names(df) == "VFR_alt") + 1
  cat_cols <- names(df)[first_cat:ncol(df)]

  for (alias in names(label_aliases)) {
    names(df)[names(df) == alias] <- label_aliases[[alias]]
  }
  cat_cols <- recode(cat_cols, !!!label_aliases)

  unknown_cols <- setdiff(cat_cols, names(label_set))
  if (length(unknown_cols)) stop("unrecognized category column(s) in ", path, ": ",
                                 paste(unknown_cols, collapse = ", "))

  for (code in setdiff(names(label_set), cat_cols)) df[[code]] <- 0
  meta_cols <- names(df)[seq_len(first_cat - 1)]
  df[, c(meta_cols, names(label_set))]
}


## cumulative distance (m) travelled along the ROV's path, from DVL x/y
## (meters, local frame), starting at 0 at the transect's first photo. The raw
## `Distance` column is NOT cumulative (~0.1-0.2 m on every row), hence this.
## Note this follows the ROV's actual path: T2's first ~10 photos drift ~8 m
## sideways along the 14-15 m contour before the ROV turns upslope, and that
## sideways travel counts toward cumulative distance.
add.cumulative.distance <- function(df) {
  df %>%
    group_by(transect) %>%
    arrange(Time, .by_group = TRUE) %>%
    mutate(step_m = sqrt((DVLx - lag(DVLx, default = first(DVLx)))^2 +
                         (DVLy - lag(DVLy, default = first(DVLy)))^2),
           cumulative_distance_m = cumsum(step_m),
           photo_index = row_number()) %>%
    ungroup() %>%
    select(-step_m)
}


## NOAA CO-OPS 6-minute verified water level (m above MLLW) at Seattle
## (station 9447130) for one survey day, local time (LST/LDT). Downloaded once
## and cached at `cache_path` (in data/, so the pipeline runs offline after the
## first run). Only the station, date, and datum go in the request.
read.tide <- function(date, cache_path, station = "9447130") {
  if (!file.exists(cache_path)) {
    ymd <- format(as.Date(date), "%Y%m%d")
    url <- paste0("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?",
                  "product=water_level&station=", station,
                  "&begin_date=", ymd, "&end_date=", ymd,
                  "&datum=MLLW&time_zone=lst_ldt&units=metric&format=csv",
                  "&application=public_client")
    download.file(url, cache_path, quiet = TRUE, mode = "wb")
  }
  read_csv(cache_path, show_col_types = FALSE) %>%
    transmute(datetime = as.POSIXct(`Date Time`, format = "%Y-%m-%d %H:%M", tz = "UTC"),
              water_level_mllw_m = `Water Level`)
}


## tide-correct depth to MLLW (chart datum). ROV depth (the `Depth` column,
## negative down, relative to the sea surface at the time) plus DVL altitude
## gives the seafloor's depth below the surface; subtracting that moment's
## water level (m above MLLW, linearly interpolated from the 6-min gauge
## series) gives seafloor depth below MLLW -- comparable across tides, days,
## and years. Positive = below MLLW. Times are compared as clock times in the
## same local zone (tz = "UTC" is only a label, used for both sides so no
## offset is applied).
add.tide.corrected.depth <- function(df, tide) {
  photo_time <- as.POSIXct(paste(df$Date, df$Time), format = "%m/%d/%Y %H:%M:%S", tz = "UTC")
  stopifnot(!anyNA(photo_time),
            min(photo_time) >= min(tide$datetime), max(photo_time) <= max(tide$datetime))

  df %>%
    mutate(rov_depth_m = -Depth,
           seafloor_depth_m = rov_depth_m + Altitude,
           water_level_mllw_m = approx(as.numeric(tide$datetime), tide$water_level_mllw_m,
                                       xout = as.numeric(photo_time))$y,
           depth_mllw_m = seafloor_depth_m - water_level_mllw_m)
}


## sum label_set codes into the cover_groups columns (proportions, 0-1)
add.cover.groups <- function(df) {
  for (g in names(cover_groups)) df[[g]] <- rowSums(df[cover_groups[[g]]])
  df
}


## read every annotated point (50 per photo: image row/column of the patch
## center, and its label) for one transect, from either a Toolbox CSV export
## (Row/Column/Label columns) or a Toolbox JSON export (center_xy = c(col, row),
## label_short_code; non-patch annotations such as the empty "Review" mask
## Toolbox adds to each image are dropped). label_aliases applied.
read.points <- function(path, transect) {
  if (grepl("\\.csv$", path)) {
    pts <- read_csv(path, show_col_types = FALSE) %>%
      transmute(Name, row = Row, col = Column, code = Label)
  } else {
    js <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    pts <- purrr::imap_dfr(js, function(anns, img_path) {
      anns <- purrr::keep(anns, ~ .x$type == "PatchAnnotation")
      tibble(Name = basename(img_path),
             col = purrr::map_dbl(anns, ~ .x$center_xy[[1]]),
             row = purrr::map_dbl(anns, ~ .x$center_xy[[2]]),
             code = purrr::map_chr(anns, "label_short_code"))
    })
  }
  pts <- pts %>% mutate(transect = transect, code = recode(code, !!!label_aliases))
  stopifnot(all(pts$code %in% names(label_set)))
  pts
}


## group lookup for a vector of label codes
code.to.group <- function(codes) {
  lookup <- setNames(rep(names(cover_groups), lengths(cover_groups)), unlist(cover_groups))
  unname(lookup[codes])
}


## check that each photo's points reproduce its percent-cover row exactly
## (count / 50), so the point overlays and the bars can never disagree
check.points.match.cover <- function(points, photos) {
  from_points <- points %>%
    count(transect, Name, code) %>%
    group_by(transect, Name) %>%
    mutate(p = n / sum(n)) %>%
    ungroup() %>%
    select(-n)
  from_cover <- photos %>%
    select(transect, Name, all_of(names(label_set))) %>%
    pivot_longer(all_of(names(label_set)), names_to = "code", values_to = "p") %>%
    filter(p > 0)
  joined <- full_join(from_points, from_cover, by = c("transect", "Name", "code"),
                      suffix = c("_points", "_cover"))
  max_diff <- max(abs(coalesce(joined$p_points, 0) - coalesce(joined$p_cover, 0)))
  if (max_diff > 1e-9) stop("point annotations disagree with percent cover (max diff ", max_diff, ")")
  invisible(TRUE)
}


## save a data frame as CSV, no row names
save.csv <- function(df, path, filename) {
  write.csv(df, file.path(path, filename), row.names = FALSE)
}




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
