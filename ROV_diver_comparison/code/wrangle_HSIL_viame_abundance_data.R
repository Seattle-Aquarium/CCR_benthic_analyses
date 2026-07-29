## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## wrangle HSIL ROV VIAME abundance data for ROV-diver comparison ~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Source: data/ROV/HSIL_viame_export.csv -- one row per ROV photo reviewed in
## VIAME, "Site ID" ("Centennial"/"EBM") + "Transect ID" ("T1"-"T6") + "Name"
## (photo filename) followed by raw species-code count columns.
##
## IMPORTANT: HSIL_viame_export.csv's own "Transect ID" does NOT reliably
## indicate which transect a photo belongs to -- cross-checked (2026-07-28)
## against the raw flight imagery folder structure at
## .../Coastal_Climate_Resilience/flights/HSIL/<year>/<survey>/downward/.../
## transects/T{1-6}_{deep,shallow}/, it only agrees with the folder a photo
## actually lives in 31% of the time (looks like it tracks the raw video
## *file* number for that dive, not the true GPS transect). The percent-cover
## pipeline's own `transect` column (results/ROV/HSIL_percent-cover_photo-
## level.csv, sourced from the raw nav log's Transect_number field) agrees
## with the folder structure 100% of the time and is the trustworthy source.
##
## data/ROV/HSIL_viame_transect_ground_truth.csv is a one-time export of that
## folder structure (basename -> site/transect/depth, covering all 682 viame
## photos, built by listing the testing+training subfolders for 2024 and the
## edited subfolder for 2025) -- used here instead of re-scanning Dropbox on
## every run. site/transect/depth/Transect_ID for every viame row come from
## this ground truth, not from the viame file's own Site ID/Transect ID
## (those two are kept in the output only as the original, as-shipped values).
##
## `pass` (out/return) isn't recoverable from the folder structure, so it's
## pulled from results/ROV/HSIL_percent-cover_photo-level.csv by matching on
## photo filename where possible. ~13% of viame filenames carry a "_enhanced"
## suffix (low-light correction) not present in the percent-cover export;
## stripping that suffix before matching recovers all of those rows. The
## remaining ~26% of photos were never part of the percent-cover subsample at
## all (VIAME review used a sparser/different set of frames); for those,
## `pass` is inferred from the photo's timestamp relative to the out->return
## turnaround time observed in the matched photos for that same transect
## (passes are strictly chronological -- out, then return -- so this
## reproduces what add.transect.pass() would compute if GPS/nav data were
## available for every photo). `pass_source` flags which method produced
## each row's `pass` value.




## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())


## add libraries
library(tidyverse)
library(stringr)


## set working directory one level up and verify
setwd("../")
getwd()


## relative file paths
ROV_input <- "data/ROV"
ROV_output <- "results/ROV"
code <- "code"
diver_output <- "results/diver"
results <- "results"


## source functions
source(file.path(code, "wrangle_data_functions.R"))


## read data
viame <- read.csv(file.path(ROV_input, "HSIL_viame_export.csv"), check.names = FALSE)
ground_truth <- read.csv(file.path(ROV_input, "HSIL_viame_transect_ground_truth.csv"))
pctcover <- read.csv(file.path(ROV_output, "HSIL_percent-cover_photo-level.csv"))
diver_invert <- read.csv(file.path(diver_output, "diver_invert_abundance.csv"))
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## rename / merge raw species-code columns ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## "fish_sculpin" and "fish_sculp" were confirmed (2026-07-28, by the data
## owner) to be the same category recorded inconsistently -- sum them into a
## single column before renaming.
viame <- combine.cols(viame, c("fish_sculpin", "fish_sculp"), "fish_sculp_combined")
viame <- delete.cols(viame, c("fish_sculpin", "fish_sculp"))
viame <- rename.columns(viame, "fish_sculp_combined", "fish_sculp")


## rov_invert_name_map (wrangle_data_functions.R) covers every remaining code
## except CU_creep and fish_gun, confirmed (2026-07-28) as:
##   CU_creep -> creeping_pedal_sea_cucumber (not previously catalogued)
##   fish_gun -> gunnel_fish (truncated fish_gunn, same category)
extra_species_map <- c(
  "CU_creep" = "creeping_pedal_sea_cucumber",
  "fish_gun" = "gunnel_fish"
)
species_map <- c(rov_invert_name_map, extra_species_map)

raw_species_cols <- setdiff(names(viame), c("Site ID", "Transect ID", "Name"))
stopifnot(all(raw_species_cols %in% names(species_map)))

viame <- rename.columns(viame, names(species_map), unname(species_map))
species_cols <- unname(species_map[raw_species_cols])
stopifnot(!anyDuplicated(species_cols))
## END species column prep ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## derive photo-level metadata: Date/Time from filename, site/transect/depth
## from the folder ground truth (NOT from viame's own Site ID/Transect ID --
## see header note) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## (filenames follow YYYY_MM_DD_HH-MM-SS[_enhanced].jpg)
viame <- viame %>%
  mutate(
    join_name = str_replace(Name, "_enhanced(?=\\.jpg$)", ""),
    Date = as.Date(str_sub(Name, 1, 10), format = "%Y_%m_%d"),
    Time = str_replace_all(str_sub(Name, 12, 19), "-", ":")
  ) %>%
  left_join(ground_truth, by = c("join_name" = "basename"))

## every viame photo must resolve to a transect via the ground truth lookup
stopifnot(all(!is.na(viame$transect)))

viame <- add.season(viame)

viame <- viame %>%
  mutate(
    Transect_ID = paste0(
      dplyr::recode(site, "Centennial_Park" = "CNL", "Elliott_Bay_Marina" = "EBM"),
      "_", dplyr::recode(season, "summer" = "S24", "winter" = "W25"),
      "_T", transect
    )
  )
## END derived metadata ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## bring in `pass` from the percent-cover match, infer it where there's no
## match ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pctcover_match <- pctcover %>%
  select(Name, pc_Date = Date, pc_season = season, pc_pass = pass,
        pc_site = site, pc_transect = transect, pc_depth = depth,
        pc_Transect_ID = Transect_ID, pc_Time = Time)

viame <- viame %>%
  left_join(pctcover_match, by = c("join_name" = "Name"))


## sanity check: filename-derived metadata must agree with the independently
## measured percent-cover metadata wherever a match exists
matched <- !is.na(viame$pc_pass)
stopifnot(
  all(viame$Date[matched] == as.Date(viame$pc_Date[matched])),
  all(viame$season[matched] == viame$pc_season[matched]),
  all(viame$site[matched] == viame$pc_site[matched]),
  all(viame$transect[matched] == viame$pc_transect[matched]),
  all(viame$depth[matched] == viame$pc_depth[matched]),
  all(viame$Transect_ID[matched] == viame$pc_Transect_ID[matched])
)
cat(sprintf("%d of %d photos matched the percent-cover export by filename (%d required an _enhanced-suffix strip)\n",
           sum(matched), nrow(viame),
           sum(matched & viame$Name != viame$join_name)))


## per-transect out->return turnaround time, from the full percent-cover
## reference (independent of which viame rows matched)
time_to_sec <- function(t) {
  parts <- do.call(rbind, strsplit(t, ":"))
  as.integer(parts[, 1]) * 3600 + as.integer(parts[, 2]) * 60 + as.integer(parts[, 3])
}

turnaround <- pctcover %>%
  group_by(Transect_ID) %>%
  summarise(
    threshold_sec = if (any(pass == "return")) min(time_to_sec(Time[pass == "return"])) else Inf,
    .groups = "drop"
  )

viame <- viame %>%
  mutate(time_sec = time_to_sec(Time)) %>%
  left_join(turnaround, by = "Transect_ID") %>%
  mutate(
    pass_source = if_else(matched, "matched", "inferred_time"),
    pass = if_else(matched, pc_pass, if_else(time_sec < threshold_sec, "out", "return"))
  ) %>%
  select(-join_name, -pc_Date, -pc_season, -pc_pass, -pc_site, -pc_transect,
        -pc_depth, -pc_Transect_ID, -pc_Time, -threshold_sec, -time_sec)

cat("pass_source counts:\n")
print(table(viame$pass_source))
## END pass ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## order species columns to align with diver_invert_abundance.csv where the
## category overlaps (for easy visual side-by-side comparison), ROV-only
## categories (fish, cucumbers/urchins not seen by divers, etc.) appended after
diver_species_cols <- setdiff(names(diver_invert), c("Date", "season", "site", "transect", "depth"))
overlap_cols <- intersect(diver_species_cols, species_cols)
rov_only_cols <- sort(setdiff(species_cols, overlap_cols))
species_cols_ordered <- c(overlap_cols, rov_only_cols)


## final column order + chronological/grouped sort, per site > season >
## transect > pass ("all T1 out rows for a site/season, then all T1 return
## rows, then T2...")
viame <- viame %>%
  mutate(
    site = factor(site, levels = c("Centennial_Park", "Elliott_Bay_Marina")),
    season = factor(season, levels = c("summer", "winter")),
    pass = factor(pass, levels = c("out", "return"))
  ) %>%
  arrange(site, season, transect, pass, Time) %>%
  mutate(site = as.character(site), season = as.character(season), pass = as.character(pass)) %>%
  select(`Site ID`, `Transect ID`, Name, Date, season, pass, pass_source,
        site, transect, depth, Transect_ID, Time,
        all_of(species_cols_ordered))
## END final photo-level assembly ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## transect-level sums (24 rows: 2 sites x 2 seasons x 6 transects), directly
## comparable to diver_invert_abundance.csv ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
viame_sums <- sum.by.group(
  viame,
  group_cols = c("Date", "season", "site", "transect", "depth", "Transect_ID"),
  cols = species_cols_ordered
)
stopifnot(nrow(viame_sums) == 24)


## complementarity check vs. diver_invert_abundance.csv, printed for review
cat("\nROV (VIAME) vs. diver total counts, shared categories (summed across all 24 transects):\n")
comparison <- tibble(
  category = overlap_cols,
  ROV_total = sapply(overlap_cols, function(c) sum(viame_sums[[c]])),
  diver_total = sapply(overlap_cols, function(c) sum(diver_invert[[c]]))
) %>% arrange(desc(diver_total))
print(comparison, n = Inf)

cat("\nROV-only categories (no diver equivalent), summed across all 24 transects:\n")
print(tibble(
  category = rov_only_cols,
  ROV_total = sapply(rov_only_cols, function(c) sum(viame_sums[[c]]))
), n = Inf)
## END transect-level sums ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## save the new dataframes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
save.csv(viame, results, "HSIL_abundances_photo_scale.csv")
save.csv(viame_sums, results, "HSIL_abundances_averaged.csv")
## END save ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
