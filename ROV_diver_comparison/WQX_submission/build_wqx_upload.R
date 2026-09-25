## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## build WQX Web upload file (Results & Activities) for EPA submission ~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Builds one CSV of activities + results for the 24 paired ROV/diver transects
## (2 sites x 6 transects x 2 seasons) for import into WQX Web under
## organization SEAQ, project SEAQ_HSIL_ROV.
##
## WQX requires exactly one assemblage per biological activity, so each
## transect survey is split into one activity per protocol x assemblage
## (6 per transect-day, 144 total; see protocols below). The invertebrate
## count activities keep the IDs of the Jan 2026 submission (e.g.
## 2024-10-08_ROV_T1, 2024-10-08_diver_T1), so resubmitting replaces the 24
## summer-2024 activities already in WQX -- whose ROV counts predate the VIAME
## transect ground-truth correction.
##
## Data are submitted as observed, not as transformed for the analysis:
##  - ROV percent cover: raw photo-level classifier output, pooled per transect
##    (every photo has 50 points, so pooled % = mean of photo proportions)
##  - ROV counts: corrected per-transect VIAME sums (results/ROV/abundance/) --
##    the raw VIAME export's own transect IDs are unreliable
##  - diver counts: raw Reef Check counts, NOT extrapolated to 30 m; any count
##    made over < 30 m of tape carries a result comment giving the length
##  - diver UPC: raw point-contact percent, "UPC Cover" category only
##
## Taxon mapping, drops, and the parent-taxon workaround for taxa missing from
## the WQX taxon list all live in taxon_crosswalk.csv. Every value written is
## validated against the EPA domain value lists in domain_values/ (downloaded
## from cdx.epa.gov/wqx/download/DomainValues/ on 2026-09-25).
##
## Run: Rscript build_wqx_upload.R   (from any working directory)





## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
suppressPackageStartupMessages(library(tidyverse))

## locate this script so paths work regardless of working directory
file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
wqx_dir <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg))) else getwd()
root <- dirname(wqx_dir)
domain_dir <- file.path(wqx_dir, "domain_values")
out_dir <- file.path(wqx_dir, "output")
dir.create(out_dir, showWarnings = FALSE)


## WQX identifiers already registered for SEAQ (see Review menu in WQX Web)
project_id <- "SEAQ_HSIL_ROV"

surveys <- tribble(
  ~site,                ~season,  ~date,         ~location_id,
  "Centennial_Park",    "summer", "2024-10-08",  "HSIL_site_1",
  "Centennial_Park",    "winter", "2025-01-27",  "HSIL_site_1",
  "Elliott_Bay_Marina", "summer", "2024-10-09",  "HSIL_site_2",
  "Elliott_Bay_Marina", "winter", "2025-01-28",  "HSIL_site_2"
) %>%
  mutate(date = as.Date(date))

site_names <- c("Centennial Park"   = "Centennial_Park",
                "Sirens of Spring"  = "Centennial_Park",
                "Elliott Bay Marina" = "Elliott_Bay_Marina",
                "Elliot Bay Marina"  = "Elliott_Bay_Marina")


## result-level constants
cover_fields <- list(characteristic = "Cover, species (%)", unit = "%",
                     value_type = "Calculated", intent = "Species Density")
count_fields <- list(characteristic = "Count", unit = "count",
                     value_type = "Actual", intent = "Population Census")
result_status <- "Final"
activity_type <- "Field Msr/Obs"
activity_media <- "Biological"

ochre_comment <- paste("ROV annotation class combines Pisaster ochraceus (ochre star)",
                       "and Evasterias troscheli (mottled star), which are not",
                       "reliably separable in the imagery; reported at family level.")


## crosswalk: keep every row for the unmapped-label check, filter for use
xwalk_all <- read_csv(file.path(wqx_dir, "taxon_crosswalk.csv"),
                      show_col_types = FALSE, na = "")
xwalk <- xwalk_all %>%
  filter(include) %>%
  mutate(morphotype_label = replace_na(morphotype_label, ""),
         user_supplied_taxon = replace_na(user_supplied_taxon, ""),
         ## option B (user 2026-09-25): taxa missing from the WQX taxon list
         ## go in under the nearest listed parent, real name in the label
         taxon_comment = if_else(
           grepl("not in WQX taxon list; submitted under", notes),
           sprintf("Actual taxon: %s. Not in the WQX taxon list at time of submission, so reported under %s.",
                   morphotype_label, wqx_taxon),
           NA_character_))
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## helper functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## stop if the raw data holds a label the crosswalk doesn't know about --
## otherwise a new category in a fresh export would silently vanish
check.labels <- function(labels, set, key_col) {
  known <- xwalk_all %>% filter(dataset == set) %>% pull(all_of(key_col))
  unknown <- setdiff(unique(labels), known)
  if (length(unknown)) {
    stop(sprintf("%s: labels not in taxon_crosswalk.csv: %s",
                 set, paste(unknown, collapse = ", ")))
  }
}


## read one raw Reef Check export, restricted to the 4 paired survey days
read.diver <- function(file) {
  read_csv(file.path(root, "data", "diver", file), show_col_types = FALSE) %>%
    mutate(site = unname(site_names[Site]),
           date = as.Date(Date),
           transect = as.integer(Transect)) %>%
    inner_join(surveys, by = c("site", "date"))
}


## fill explicit zeros for crosswalk taxa absent from a transect's records
complete.zeros <- function(df, set) {
  keys <- xwalk %>% filter(dataset == set) %>% pull(source_label)
  filled <- df %>%
    complete(nesting(site, season, date, location_id, transect),
             source_label = keys, fill = list(value = 0))
  n_added <- nrow(filled) - nrow(df)
  if (n_added > 0) message(sprintf("%s: filled %d implicit zeros", set, n_added))
  filled
}
## END helper functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ROV percent cover ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rov_cover_raw <- read_csv(file.path(root, "data", "ROV", "HSIL_percent_cover.csv"),
                          show_col_types = FALSE)

cover_meta <- c("Source_file", "Name", "Date", "Time", "Site_name", "Transect_number",
                "Transect_ID", "Mode_num", "Mode", "Battery_V", "Battery_A", "Battery_W",
                "Battery_mAh_used", "Battery_Wh_used", "Latitude", "Longitude", "EKFlat",
                "EKFlon", "DVLx", "DVLy", "DVLlat", "DVLlon", "Altitude", "Depth",
                "Depth_std", "Depth_Source", "Heading", "Velocity_mps", "Width", "Area_m2",
                "Distance", "NEDz", "VFR_alt")
check.labels(setdiff(names(rov_cover_raw), cover_meta), "ROV_percent_cover", "source_label")

cover_codes <- xwalk %>% filter(dataset == "ROV_percent_cover") %>% pull(source_label)

rov_cover <- rov_cover_raw %>%
  mutate(site = unname(site_names[Site_name]),
         date = as.Date(Date, format = "%m/%d/%Y"),
         transect = as.integer(Transect_number)) %>%
  inner_join(surveys, by = c("site", "date")) %>%
  group_by(site, season, date, location_id, transect) %>%
  summarise(n_photos_cover = n(),
            across(all_of(cover_codes), ~ mean(.x) * 100),
            .groups = "drop")


## sanity check against the analysis pipeline's transect averages (which are
## rounded to 3 decimals as proportions, so agree to within 0.05 % points)
pipeline_cover <- read_csv(file.path(root, "results", "ROV", "percent_cover",
                                     "HSIL_percent-cover_transect-averaged.csv"),
                           show_col_types = FALSE)
cover_check <- rov_cover %>%
  pivot_longer(all_of(cover_codes), names_to = "source_label", values_to = "ours") %>%
  left_join(xwalk %>% filter(dataset == "ROV_percent_cover") %>%
              select(source_label, repo_column), by = "source_label") %>%
  left_join(pipeline_cover %>%
              pivot_longer(-c(site, transect, depth, season, n_photos),
                           names_to = "repo_column", values_to = "pipeline"),
            by = c("site", "transect", "season", "repo_column"))
max_gap <- max(abs(cover_check$ours - cover_check$pipeline * 100))
if (is.na(max_gap) || max_gap > 0.051) {
  stop(sprintf("ROV cover disagrees with pipeline output (max gap %.3f %% points)", max_gap))
}

rov_cover_long <- rov_cover %>%
  select(-n_photos_cover) %>%
  pivot_longer(all_of(cover_codes), names_to = "source_label", values_to = "value") %>%
  left_join(xwalk %>% filter(dataset == "ROV_percent_cover"), by = "source_label") %>%
  mutate(method = "ROV", !!!cover_fields, value = round(value, 2),
         length_comment = NA_character_)
## END ROV percent cover ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ROV counts ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rov_counts <- read_csv(file.path(root, "results", "ROV", "abundance",
                                 "HSIL_viame_abundance_corrected_summed.csv"),
                       show_col_types = FALSE) %>%
  inner_join(surveys, by = c("site", "season"))

count_meta <- c("site", "transect", "season", "depth", "n_photos", "date", "location_id")
check.labels(setdiff(names(rov_counts), count_meta), "ROV_abundance", "repo_column")

rov_count_long <- rov_counts %>%
  select(-depth, -n_photos) %>%
  pivot_longer(-c(site, season, date, location_id, transect),
               names_to = "repo_column", values_to = "value") %>%
  inner_join(xwalk %>% filter(dataset == "ROV_abundance"), by = "repo_column") %>%
  mutate(method = "ROV", !!!count_fields, length_comment = NA_character_)

## photos per transect, for the activity comments
rov_photos <- rov_cover %>%
  select(site, date, transect, n_photos_cover) %>%
  left_join(rov_counts %>% select(site, date, transect, n_photos_counts = n_photos),
            by = c("site", "date", "transect"))
## END ROV counts ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## diver counts (invertebrates, kelps) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
diver.counts <- function(file, set) {
  raw <- read.diver(file)
  check.labels(raw$Classcode, set, "source_label")
  dupes <- raw %>% count(site, date, transect, Classcode) %>% filter(n > 1)
  if (nrow(dupes)) stop(sprintf("%s: duplicate rows per transect x taxon", set))

  raw %>%
    transmute(site, season, date, location_id, transect,
              source_label = Classcode, value = Amount, distance = Distance) %>%
    semi_join(xwalk %>% filter(dataset == set), by = "source_label") %>%
    complete.zeros(set) %>%
    left_join(xwalk %>% filter(dataset == set), by = "source_label") %>%
    mutate(method = "diver", !!!count_fields,
           length_comment = if_else(
             !is.na(distance) & distance < 30,
             sprintf("Counted over %s m of the 30 m transect (2 m swath, %s m2); not extrapolated.",
                     distance, 2 * distance),
             NA_character_)) %>%
    select(-distance)
}

diver_invert_long <- diver.counts("Invert_Washington_raw_2025.csv", "diver_invert")
diver_algae_long <- diver.counts("Algae_Washington_raw_2025.csv", "diver_algae")
## END diver counts ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## diver UPC cover ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## only the "UPC Cover" layer; superlayer, relief, and substrate are dropped
upc_raw <- read.diver("UPC_Washington_raw_2025.csv") %>%
  filter(Category == "UPC Cover")
check.labels(upc_raw$Classcode, "diver_UPC_cover", "source_label")
if (any(upc_raw$Total_Amount != 30)) stop("UPC: expected 30 points per transect")

## UPC records only categories that were hit, so absent categories are zeros
diver_upc_long <- upc_raw %>%
  transmute(site, season, date, location_id, transect,
            source_label = Classcode, value = Amount / Total_Amount * 100) %>%
  semi_join(xwalk %>% filter(dataset == "diver_UPC_cover"), by = "source_label") %>%
  complete.zeros("diver_UPC_cover") %>%
  left_join(xwalk %>% filter(dataset == "diver_UPC_cover"), by = "source_label") %>%
  mutate(method = "diver", !!!cover_fields, value = round(value, 2),
         length_comment = NA_character_)
## END diver UPC cover ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## assemble activities + results ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## one activity per protocol x assemblage; the suffix goes into the Activity ID
## (<date>_<suffix>_T<n>) -- "ROV" and "diver" are the Jan 2026 IDs.
## "Invertebrates", not "Benthic Macroinvertebrates": WQX ties the latter to
## lab-subsampling fields (habitat selection method, proportion of sample
## processed, target count) that don't apply to visual swath counts
protocols <- tribble(
  ~dataset,            ~assemblage,                  ~suffix,
  "ROV_abundance",     "Invertebrates",              "ROV",
  "ROV_abundance",     "Fish/Nekton",                "ROV_fish",
  "ROV_percent_cover", "Aquatic Vegetation",         "ROV_cover",
  "diver_invert",      "Invertebrates",              "diver",
  "diver_algae",       "Aquatic Vegetation",         "diver_algae",
  "diver_UPC_cover",   "Aquatic Vegetation",         "diver_cover"
)

rov_intro <- paste("ROV survey (BlueROV2, downward-facing camera, ~0.8 m altitude) of a 30 m transect;",
                   "%s stratum; paired with same-day diver survey.")
diver_intro <- paste("Scientific diver survey, Reef Check Kelp Forest Monitoring protocol, 30 m transect;",
                     "%s stratum; paired with same-day ROV survey.")

results <- bind_rows(rov_cover_long, rov_count_long,
                     diver_invert_long, diver_algae_long, diver_upc_long) %>%
  inner_join(protocols, by = c("dataset", "assemblage")) %>%
  mutate(activity_id = sprintf("%s_%s_T%d", format(date, "%Y-%m-%d"), suffix, transect),
         stratum = if_else(transect %in% 1:3, "Deep (~10 m)", "Shallow (~5 m)"),
         result_comment = pmap_chr(
           list(taxon_comment, length_comment,
                if_else(method == "ROV" & repo_column %in% "ochre_mottled_star",
                        ochre_comment, NA_character_)),
           ~ paste(na.omit(c(...)), collapse = " ")))

activities <- results %>%
  distinct(activity_id, suffix, site, date, transect, stratum, location_id) %>%
  left_join(rov_photos, by = c("site", "date", "transect")) %>%
  mutate(activity_comment = case_when(
    suffix %in% c("ROV", "ROV_fish") ~ sprintf(
      paste(rov_intro, "%s counts: all individuals manually annotated in VIAME across %d photos, summed per transect."),
      stratum, if_else(suffix == "ROV", "Macroinvertebrate", "Fish"), n_photos_counts),
    suffix == "ROV_cover" ~ sprintf(
      paste(rov_intro, "Percent cover: CoralNet-Toolbox classifier, 50 random points per photo across %d photos",
            "(%d points), pooled per transect. Classification includes sessile invertebrate and bryozoan",
            "categories alongside algae; substrate categories not reported."),
      stratum, n_photos_cover, n_photos_cover * 50L),
    suffix == "diver" ~ sprintf(
      paste(diver_intro, "Invertebrate counts in a 2 m swath (1 m each side of the tape)."), stratum),
    suffix == "diver_algae" ~ sprintf(
      paste(diver_intro, "Kelp and large brown algae counts in a 2 m swath (1 m each side of the tape)."), stratum),
    suffix == "diver_cover" ~ sprintf(
      paste(diver_intro, "Uniform point contact (UPC) percent cover, 30 points at 1 m intervals; bottom cover",
            "layer only (superlayer, substrate, and relief not reported). Includes a sessile invertebrate category."),
      stratum)))

## column order must match WQX Web import configuration
## "SEAQ_HSIL_ROV_diver_Results_2026_v2" (ID 9477), columns A-R
upload <- results %>%
  left_join(activities %>% select(activity_id, activity_comment), by = "activity_id") %>%
  arrange(date, location_id, suffix, transect, characteristic, wqx_taxon, morphotype_label) %>%
  transmute(
    `Project ID` = project_id,
    `Monitoring Location ID` = location_id,
    `Activity ID` = activity_id,
    `Activity Type` = activity_type,
    `Activity Media Name` = activity_media,
    `Activity Start Date` = format(date, "%Y/%m/%d"),
    `Result Status ID` = result_status,
    `Result Unit` = unit,
    `Characteristic Name` = characteristic,
    `Result Value` = format(value, scientific = FALSE, trim = TRUE, drop0trailing = TRUE),
    `Result Value Type` = value_type,
    `Subject Taxonomic Name` = wqx_taxon,
    `Biological Intent` = intent,
    `Unidentified Species Identifier` = morphotype_label,
    `Subject Taxonomic Name User Supplied` = user_supplied_taxon,
    `Result Comment` = result_comment,
    `Activity Comment` = activity_comment,
    `Assemblage Sampled Name` = assemblage)
## END assemble ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## validate against EPA domain lists + internal consistency ~~~~~~~~~~~~~~~~~~~~
## most lists call the value column "Name"; Activity Type calls it "Code"
read.domain <- function(file) {
  read.csv(file.path(domain_dir, file), check.names = FALSE, stringsAsFactors = FALSE) %>%
    rename(any_of(c(Name = "Code"))) %>%
    mutate(Name = trimws(Name))
}
taxa <- read.delim(file.path(domain_dir, "Taxon.tsv"), quote = "", stringsAsFactors = FALSE)
chars <- read.domain("Characteristic.csv")
units <- read.csv(file.path(domain_dir, "MeasureUnit.csv"), check.names = FALSE)

problems <- c(
  setdiff(upload$`Subject Taxonomic Name`, taxa$Name[taxa$Status == "Accepted"]) %>%
    sprintf(fmt = "taxon not accepted in WQX: %s"),
  setdiff(upload$`Characteristic Name`,
          chars$Name[chars$`Domain Value Status` == "Accepted"]) %>%
    sprintf(fmt = "characteristic not accepted in WQX: %s"),
  setdiff(upload$`Result Unit`, units[[3]]) %>% sprintf(fmt = "unit not in WQX: %s"),
  setdiff(upload$`Biological Intent`, read.domain("BiologicalIntent.csv")$Name) %>%
    sprintf(fmt = "biological intent not in WQX: %s"),
  setdiff(upload$`Result Value Type`, read.domain("ResultValueType.csv")$Name) %>%
    sprintf(fmt = "result value type not in WQX: %s"),
  setdiff(upload$`Result Status ID`, read.domain("ResultStatus.csv")$Name) %>%
    sprintf(fmt = "result status not in WQX: %s"),
  setdiff(upload$`Activity Type`, read.domain("ActivityType.csv")$Name) %>%
    sprintf(fmt = "activity type not in WQX: %s"),
  setdiff(upload$`Activity Media Name`, read.domain("ActivityMedia.csv")$Name) %>%
    sprintf(fmt = "activity media not in WQX: %s"),
  setdiff(upload$`Assemblage Sampled Name`, read.domain("Assemblage.csv")$Name) %>%
    sprintf(fmt = "assemblage not in WQX: %s")
)

## assemblage is activity-level in WQX, so it must not vary within an activity
mixed <- upload %>%
  distinct(`Activity ID`, `Assemblage Sampled Name`) %>%
  count(`Activity ID`) %>%
  filter(n > 1)
if (nrow(mixed)) problems <- c(problems, sprintf("%d activities with mixed assemblages", nrow(mixed)))

dupes <- upload %>%
  count(`Activity ID`, `Characteristic Name`, `Subject Taxonomic Name`,
        `Unidentified Species Identifier`) %>%
  filter(n > 1)
if (nrow(dupes)) problems <- c(problems, sprintf("%d duplicate result keys", nrow(dupes)))

values <- as.numeric(upload$`Result Value`)
if (anyNA(values) || any(values < 0)) problems <- c(problems, "missing or negative result values")
if (any(values[upload$`Result Unit` == "%"] > 100)) problems <- c(problems, "cover value > 100%")
if (any(values[upload$`Result Unit` == "count"] %% 1 != 0)) problems <- c(problems, "non-integer count")

cover_sums <- upload %>%
  filter(`Result Unit` == "%") %>%
  group_by(`Activity ID`) %>%
  summarise(total = sum(as.numeric(`Result Value`)))
if (any(cover_sums$total > 100.5)) problems <- c(problems, "an activity's taxon cover sums to > 100%")

if (max(nchar(upload$`Activity ID`)) > 55) problems <- c(problems, "Activity ID longer than 55 characters")
if (n_distinct(upload$`Activity ID`) != 24 * nrow(protocols)) {
  problems <- c(problems, sprintf("expected %d activities, got %d",
                                  24 * nrow(protocols), n_distinct(upload$`Activity ID`)))
}
if (any(grepl("[^ -~]", unlist(upload)))) problems <- c(problems, "non-ASCII characters in upload")

if (length(problems)) stop(paste(c("validation failed:", problems), collapse = "\n  "))
## END validate ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## write + summarize ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
out_file <- file.path(out_dir, "SEAQ_results_activities.csv")
write_csv(upload, out_file, na = "")

cat(sprintf("\nwrote %s\n  %d results across %d activities\n\n",
            out_file, nrow(upload), n_distinct(upload$`Activity ID`)))
upload %>%
  mutate(protocol = sub("^[0-9-]+_(.*)_T[0-9]+$", "\\1", `Activity ID`)) %>%
  group_by(protocol, `Assemblage Sampled Name`, `Characteristic Name`) %>%
  summarise(activities = n_distinct(`Activity ID`), results = n(), .groups = "drop") %>%
  print()
cat(sprintf("\nnon-zero results: %d\n", sum(as.numeric(upload$`Result Value`) > 0)))
cat(sprintf("results with a comment: %d\n", sum(upload$`Result Comment` != "")))
## END write + summarize ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
