## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## overlay VIAME mobile-species bounding boxes on the real survey photos ~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## QA tool: draws each detection's bounding box + descriptive species name on a COPY of the
## real photo it came from, so the abundance counts in HSIL_viame_export.csv
## can be visually spot-checked against the actual imagery (VIAME's own web
## viewer shows every volunteer pass over a photo, including duplicates from
## overlapping review, so it isn't usable for this directly).
##
## Source CSVs: data/ROV/VIAME_raw_export/*.csv -- VIAME's native per-track
## detection export (id, image name, frame id, bbox TL/BR corners, detection
## confidence, target length, species + species confidence). Box coordinates
## are in the pixel space of the CROPPED photos (the "testing"/"training"
## subfolders for 2024 surveys, "edited" for 2025 -- confirmed by comparing
## max box coordinates, ~4608x4036, against actual image dimensions: the
## cropped set is 4606x4031, the uncropped set is 5044x4414).
##
## IMPORTANT: these raw per-track CSVs cover every photo volunteers reviewed
## in VIAME, which is MORE photos than the official ~1-photo-per-meter set
## actually used for the abundance counts (volunteers deliberately reviewed
## overlapping frames too, for ML training imagery -- see 2026-07-28
## conversation). Annotating those extra photos would misrepresent counts as
## if every reviewed frame counted toward the transect total, so this script
## restricts to whichever photos actually appear as that Transect_ID's rows
## in results/HSIL_abundances_photo_scale.csv (get.official.photos()) before
## drawing anything -- e.g. for CNL_S24_T1, 25 of the 83 photos in the raw
## detections CSV are official; the rest are skipped.
##
## Output: one annotated copy per official photo, written to an
## "annotated_qaqc" subfolder alongside testing/training/edited in that
## photo's own T{n}_{deep,shallow} transect folder -- originals untouched.




## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls())

library(tidyverse)
library(magick)


## set working directory one level up and verify
setwd("../")
getwd()


ROV_input <- "data/ROV"
results <- "results"
code <- "code"

## rov_invert_name_map (short code -> descriptive name, e.g. "SS_ochre" ->
## "ochre_mottled_star") -- the same map used to rename the abundance columns
## in wrangle_HSIL_viame_abundance_data.R, reused here for label text
source(file.path(code, "wrangle_data_functions.R"))
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## the official ~1-photo-per-meter set for a given Transect_ID, per
## results/HSIL_abundances_photo_scale.csv
get.official.photos <- function(transect_id, abundance_csv_path = file.path(results, "HSIL_abundances_photo_scale.csv")) {
  abundances <- read_csv(abundance_csv_path, show_col_types = FALSE)
  photos <- abundances$Name[abundances$Transect_ID == transect_id]
  stopifnot(length(photos) > 0)
  photos
}


## short VIAME species code -> descriptive name for label text. Extends
## rov_invert_name_map with codes confirmed (2026-07-28) not already in it:
## fish_sculpin is the same category as fish_sculp (other_large_sculpin),
## CU_creep is creeping_pedal_sea_cucumber, fish_gun is a truncated fish_gunn
## (gunnel_fish).
species_name_map <- c(
  rov_invert_name_map,
  "fish_sculpin" = "other_large_sculpin",
  "CU_creep" = "creeping_pedal_sea_cucumber",
  "fish_gun" = "gunnel_fish"
)


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


## annotate the official photos for one transect, writing copies to
## <transect_dir>/annotated_qaqc/. `transect_dir` is the T{n}_{deep,shallow}
## folder itself; source photos are located by searching, in order, whichever
## of its testing/training/edited subfolders actually contain the file (2024
## surveys split photos across testing+training, 2025 surveys use edited only)
annotate.transect.detections <- function(csv_path, transect_dir, official_photos,
                                         search_subfolders = c("testing", "training", "edited")) {

  detections <- read_csv(csv_path, skip = 2, col_names = FALSE, show_col_types = FALSE)
  stopifnot(ncol(detections) == 11)
  names(detections) <- c("track_id", "image", "frame_id",
                         "TL_x", "TL_y", "BR_x", "BR_y",
                         "det_conf", "target_length", "species", "species_conf")

  unmapped_species <- setdiff(unique(detections$species), names(species_name_map))
  if (length(unmapped_species) > 0) {
    stop(sprintf("No descriptive name mapped for species code(s): %s -- add to species_name_map",
                paste(unmapped_species, collapse = ", ")))
  }
  detections$species_name <- unname(species_name_map[detections$species])

  output_dir <- file.path(transect_dir, "annotated_qaqc")
  dir.create(output_dir, showWarnings = FALSE)

  images <- intersect(unique(detections$image), official_photos)
  skipped <- setdiff(unique(detections$image), official_photos)
  unresolved <- setdiff(official_photos, unique(detections$image))

  cat(sprintf("Annotating %d official photos (out of %d total photos, %d detections, in %s)\n",
             length(images), length(unique(detections$image)), nrow(detections), basename(csv_path)))
  if (length(skipped) > 0) {
    cat(sprintf("Skipped %d non-official photo(s) present in the raw detections CSV but not in HSIL_abundances_photo_scale.csv\n", length(skipped)))
  }
  if (length(unresolved) > 0) {
    warning(sprintf("%d official photo(s) have no detections in %s: %s",
                    length(unresolved), basename(csv_path), paste(unresolved, collapse = ", ")))
  }

  missing <- character(0)

  for (img_name in images) {
    candidate_paths <- file.path(transect_dir, search_subfolders, img_name)
    found <- candidate_paths[file.exists(candidate_paths)][1]

    if (is.na(found)) {
      missing <- c(missing, img_name)
      next
    }

    boxes <- dplyr::filter(detections, image == img_name)

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
    warning(sprintf("%d photo(s) referenced in %s not found under %s: %s",
                    length(missing), basename(csv_path), transect_dir,
                    paste(missing, collapse = ", ")))
  }

  cat(sprintf("Wrote %d annotated photos to %s\n",
             length(images) - length(missing), output_dir))

  invisible(output_dir)
}
## END function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## run: Centennial Park, T1 (deep), summer 2024 -- pipeline test case ~~~~~~~~~
flights_root <- "C:/Users/randellz/Seattle Aquarium Dropbox/Coastal_Climate_Resilience/flights/HSIL"

annotate.transect.detections(
  csv_path = file.path(ROV_input, "VIAME_raw_export", "2024_10_08_centennial_t1_cropped.csv"),
  transect_dir = file.path(flights_root, "2024/2024_10_08_diver-ROV_Centennial_Park/downward/photos/transects/T1_deep"),
  official_photos = get.official.photos("CNL_S24_T1")
)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
