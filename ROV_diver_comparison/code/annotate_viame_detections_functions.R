## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## functions to overlay VIAME mobile-species bounding boxes on real survey
## photos, and to tally per-photo species counts straight from the JSON ~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Shared by annotate_viame_detections.R (single-transect pipeline test) and
## build_HSIL_viame_abundance_corrected.R (all 24 transects + corrected CSVs).
##
## Source JSON: data/ROV/VIAME_raw_export/*.json -- VIAME's native per-track
## export ({"tracks": {"<id>": {"confidencePairs": [[species, conf]],
## "features": [{"frame": N, "bounds": [TL_x,TL_y,BR_x,BR_y]}]}}}). One track
## per detection; looping over "features" also handles the rare track that
## spans multiple frames. Bounding boxes are in the pixel space of the
## CROPPED photos (the "testing"/"training" subfolders for 2024 surveys,
## "edited" for 2025).
##
## Frame -> filename: the JSON has no filename field, only a frame number.
## Confirmed 2026-07-29 (cross-checked against all 83 detections in the
## original per-transect CSV export, zero mismatches): frame_id is the
## 0-indexed rank of that photo within the alphabetically-sorted union of the
## transect folder's testing+training (2024) or edited (2025) subfolders --
## see get.frame.to.name.map().
##
## Official photo list: data/ROV/HSIL_viame_abundance.csv, updated 2026-07-29
## to include every reviewed photo for a transect -- including the ones with
## zero detections, which earlier VIAME exports omitted entirely. Still
## needed: VIAME review covered more photos per transect than the official
## ~1-photo-per-meter out+return set (2026-07-28 conversation), so this file
## -- not "every photo referenced in the JSON" -- defines which photos belong
## in a transect's record.
##
## IMPORTANT: this file's own "Transect ID" column is NOT used to pick out
## the transect -- confirmed 2026-07-29 it carries the same unreliable
## VIAME-side labeling as the original HSIL_viame_export.csv (see
## wrangle_HSIL_viame_abundance_data.R header: 31% correct against the real
## folder structure). E.g. 13 of the 63 rows tagged "Centennial"/"T1" for
## 2024-10-08 are physically stored in the T3_deep folder, one with 3 real
## ochre-star detections -- not just zero-detection noise. Instead, every
## candidate photo (matched on Site ID + survey date only) is cross-checked
## against data/ROV/HSIL_viame_transect_ground_truth.csv (the same
## folder-derived ground truth used in the main abundance pipeline) and
## assigned to a transect based on where the photo file actually lives.




## short VIAME species code -> descriptive name. Extends rov_invert_name_map
## (wrangle_data_functions.R) with codes confirmed (2026-07-28) not already in
## it: fish_sculpin is the same category as fish_sculp (other_large_sculpin),
## CU_creep is creeping_pedal_sea_cucumber, fish_gun is a truncated fish_gunn
## (gunnel_fish). Requires wrangle_data_functions.R already sourced.
species_name_map <- c(
  rov_invert_name_map,
  "fish_sculpin" = "other_large_sculpin",
  "CU_creep" = "creeping_pedal_sea_cucumber",
  "fish_gun" = "gunnel_fish"
)


## the official reviewed-photo set (including zero-detection photos) for one
## transect/survey-date, per HSIL_viame_abundance.csv, with transect
## membership corrected against the folder-derived ground truth (see header).
## Returns a data.frame with columns Name, transect, depth, site (ground-truth
## site name) -- not just a bare filename vector -- so callers don't have to
## re-derive depth/site themselves. Returned Name is always the "_enhanced"-
## stripped canonical filename (the one actually present on disk -- some
## rows carry the "_enhanced" suffix in HSIL_viame_abundance.csv, but the
## real photo file on disk never does; confirmed 2026-07-29 no photo appears
## under both spellings, so this rename is lossless), so every downstream
## consumer (file lookup, detection matching, output naming) works off one
## consistent name instead of silently missing renamed photos.
get.official.photos <- function(site, transect_number, date_prefix,
                                abundance_csv_path, ground_truth_path) {
  abundances <- read_csv(abundance_csv_path, show_col_types = FALSE)
  ground_truth <- read_csv(ground_truth_path, show_col_types = FALSE)

  candidates <- abundances[abundances$`Site ID` == site & startsWith(abundances$Name, date_prefix), ]
  candidates$Name <- str_replace(candidates$Name, "_enhanced(?=\\.jpg$)", "")
  candidates <- dplyr::left_join(candidates, ground_truth, by = c("Name" = "basename"))
  stopifnot(!anyNA(candidates$transect))

  out <- candidates[candidates$transect == transect_number, c("Name", "transect", "depth", "site")]
  stopifnot(nrow(out) > 0, !anyDuplicated(out$Name))
  out[order(out$Name), ]
}


## frame_id -> photo filename for one transect (see header note on how this
## correspondence was established). `search_subfolders` are searched in
## order and their contents pooled/sorted together, matching how VIAME loaded
## the photos when it assigned frame numbers.
get.frame.to.name.map <- function(transect_dir, search_subfolders = c("testing", "training", "edited")) {
  all_images <- character(0)
  for (sub in search_subfolders) {
    d <- file.path(transect_dir, sub)
    if (dir.exists(d)) all_images <- c(all_images, list.files(d, pattern = "\\.jpg$"))
  }
  all_images <- sort(unique(all_images))
  stopifnot(length(all_images) > 0)
  all_images
}


## parse a VIAME tracks JSON export into one row per detection: frame_id,
## bbox corners, species code
parse.viame.json <- function(json_path) {
  raw <- fromJSON(json_path, simplifyVector = FALSE)

  rows <- list()
  for (track in raw$tracks) {
    species <- track$confidencePairs[[1]][[1]]
    for (feat in track$features) {
      bounds <- feat$bounds
      rows[[length(rows) + 1]] <- data.frame(
        frame_id = feat$frame,
        TL_x = bounds[[1]], TL_y = bounds[[2]],
        BR_x = bounds[[3]], BR_y = bounds[[4]],
        species = species,
        stringsAsFactors = FALSE
      )
    }
  }
  dplyr::bind_rows(rows)
}


## three-layer box (black outline -> green -> black inner border), matching
## draw_patch_box() in zooniverse-project/scripts/toolbox_to_subjects.py (the
## CoralNet-Toolbox patch QA style) so ROV percent-cover and VIAME abundance
## QA imagery look consistent. `outset` controls the gap (px) between the
## inner/outer black strokes and the main green box.
draw.detection.box <- function(TL_x, TL_y, BR_x, BR_y, outset = 10) {
  rect(TL_x - outset, TL_y - outset, BR_x + outset, BR_y + outset, border = "black", lwd = 5)
  rect(TL_x, TL_y, BR_x, BR_y, border = "green", lwd = 14)
  rect(TL_x + outset, TL_y + outset, BR_x - outset, BR_y - outset, border = "black", lwd = 5)
}


## white text with a black outline (8-direction offset stroke), matching the
## black-then-white double-draw look of draw_model_label(). Anchored by its
## bottom edge (adj = c(0.5, 0)) a fixed gap above the box -- rather than
## vertically centered on that point -- so the full text block clears the
## green box instead of overlapping it; drops below the box instead if
## there's no room above (mirrors that function's rect_top/rect_bottom
## fallback logic).
draw.detection.label <- function(label, TL_x, TL_y, BR_x, BR_y, cex = 6, stroke_radius = 9, gap = 40) {
  label_x <- (TL_x + BR_x) / 2
  above_y <- TL_y - gap
  below <- above_y <= 80
  label_y <- if (!below) above_y else BR_y + gap
  adj <- if (!below) c(0.5, 0) else c(0.5, 1)

  offsets <- expand.grid(dx = c(-1, 0, 1), dy = c(-1, 0, 1))
  offsets <- offsets[!(offsets$dx == 0 & offsets$dy == 0), ]
  for (i in seq_len(nrow(offsets))) {
    text(label_x + offsets$dx[i] * stroke_radius, label_y + offsets$dy[i] * stroke_radius,
        labels = label, col = "black", cex = cex, font = 2, adj = adj)
  }
  text(label_x, label_y, labels = label, col = "white", cex = cex, font = 2, adj = adj)
}


## process one transect: writes annotated_qaqc/ (boxes+labels for photos with
## detections, plain copies for zero-detection photos) AND returns a long-form
## data.frame of every detection resolved to (photo_name, species_name), plus
## the full official photo list -- so callers can build both the QA imagery
## and a corrected per-photo species table from a single JSON parse/frame
## resolution pass, without redoing either.
process.transect <- function(json_path, transect_dir, official_photos,
                             search_subfolders = c("testing", "training", "edited"),
                             write_images = TRUE) {

  frame_to_name <- get.frame.to.name.map(transect_dir, search_subfolders)

  detections <- parse.viame.json(json_path)
  detections$image <- frame_to_name[detections$frame_id + 1]
  stopifnot(!anyNA(detections$image))

  unmapped_species <- setdiff(unique(detections$species), names(species_name_map))
  if (length(unmapped_species) > 0) {
    stop(sprintf("No descriptive name mapped for species code(s): %s -- add to species_name_map",
                paste(unmapped_species, collapse = ", ")))
  }
  detections$species_name <- unname(species_name_map[detections$species])

  official_with_detections <- intersect(official_photos, unique(detections$image))
  official_without_detections <- setdiff(official_photos, unique(detections$image))
  skipped <- setdiff(unique(detections$image), official_photos)

  cat(sprintf("  %s: %d official photos (%d with detections, %d with zero); %d non-official skipped\n",
             basename(json_path), length(official_photos), length(official_with_detections),
             length(official_without_detections), length(skipped)))

  output_dir <- file.path(transect_dir, "annotated_qaqc")
  if (write_images) dir.create(output_dir, showWarnings = FALSE)

  missing <- character(0)

  for (img_name in official_photos) {
    candidate_paths <- file.path(transect_dir, search_subfolders, img_name)
    found <- candidate_paths[file.exists(candidate_paths)][1]

    if (is.na(found)) {
      missing <- c(missing, img_name)
      next
    }

    if (!write_images) next

    boxes <- dplyr::filter(detections, image == img_name)

    if (nrow(boxes) == 0) {
      file.copy(found, file.path(output_dir, img_name), overwrite = TRUE)
      next
    }

    img <- image_read(found)
    img_plot <- image_draw(img)
    for (i in seq_len(nrow(boxes))) {
      draw.detection.box(boxes$TL_x[i], boxes$TL_y[i], boxes$BR_x[i], boxes$BR_y[i])
      draw.detection.label(boxes$species_name[i], boxes$TL_x[i], boxes$TL_y[i], boxes$BR_x[i], boxes$BR_y[i])
    }
    dev.off()

    image_write(img_plot, file.path(output_dir, img_name))
  }

  if (length(missing) > 0) {
    warning(sprintf("%d official photo(s) not found under %s: %s",
                    length(missing), transect_dir, paste(missing, collapse = ", ")))
  }

  list(
    output_dir = output_dir,
    n_written = length(official_photos) - length(missing),
    detections = detections[detections$image %in% official_photos, c("image", "species_name")]
  )
}
## END functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
