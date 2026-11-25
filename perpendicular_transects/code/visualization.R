## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## figures for the Fishing Pier perpendicular transects ~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## NOTE: run via RunMe.R -- needs `photos` and `points` (wrangle_transects.R)
## and `boundaries` / `zones` (zonation.R).




## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
photos <- build.thumbnails(photos, flights_root, thumbs_dir)

survey_caption <- paste("Fishing Pier, Elliott Bay · ROV survey 8 Oct 2025 ·",
                        "depth = seafloor depth below MLLW (tide-corrected, NOAA Seattle gauge 9447130)")
## END start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## 1. method: photo -> 50 points -> percent cover ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## three Transect 1 photos spanning the slope: silt with scattered sieve kelp
## (photo 8), a mixed kelp / red / green algae bed on cobble (photo 31), and
## the Ulva + red algae shallows at the low-tide line (photo 55)
exemplars <- photos %>%
  filter(transect == 1, photo_index %in% c(8, 31, 55)) %>%
  arrange(desc(depth_mllw_m)) %>%
  mutate(label = c("Deep", "Mid-slope", "Shallow"))

method_fig <- visualize.method.figure(
  exemplars, points,
  title = "From photo to percent cover",
  subtitle = "Each photo is scored at 50 points; every square marks one point, outlined in the color of what fills it",
  caption = paste("Transect 1, photos 8, 31 and 55 ·", survey_caption)
)
save.figure(method_fig, "1_method_point_overlays")
## END method ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## 2. transect strips: cover over photos over seafloor profile ~~~~~~~~~~~~~~~~~
## one slide per transect, with and without the zone boundaries from zonation.R
for (t in sort(unique(photos$transect))) {
  df <- photos %>% filter(transect == t) %>% arrange(photo_index)
  title <- sprintf("Transect %d: %d photos from %.0f m deep to the low-tide line",
                   t, nrow(df), max(df$depth_mllw_m))
  subtitle <- "Each bar is one photo (50 scored points); the photos sit beneath their bars, in survey order from deep (left) to shallow (right)"

  strip <- visualize.transect.strip(df, title, subtitle, caption = survey_caption)
  save.figure(strip, sprintf("2_transect_strip_T%d", t))

  if (exists("boundaries")) {
    strip_zoned <- visualize.transect.strip(
      df, title, subtitle, caption = survey_caption,
      boundaries = filter(boundaries, transect == t),
      zone_labels = filter(zones, transect == t)
    )
    save.figure(strip_zoned, sprintf("2_transect_strip_T%d_zones", t))
  }

  ## line versions: each cover group's own line on the same 0-100% axis --
  ## per photo, and as a 5-photo (~5 m) running mean that reads more cleanly
  ## with nine groups on one panel
  lines_raw <- visualize.transect.strip(
    df, title,
    subtitle = "Each line follows one group's cover photo by photo, where present; the photos sit beneath, deep (left) to shallow (right)",
    caption = survey_caption, style = "lines"
  )
  save.figure(lines_raw, sprintf("2b_transect_lines_T%d", t))

  lines_smooth <- visualize.transect.strip(
    df, title,
    subtitle = "Each line is one group's cover averaged over 5 neighbouring photos (~5 m); the photos sit beneath, deep (left) to shallow (right)",
    caption = survey_caption, style = "lines", smooth_window = 5
  )
  save.figure(lines_smooth, sprintf("2b_transect_lines_T%d_smoothed", t))
}
## END transect strips ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## 3. zonation summary: kite diagram ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
kite_fig <- visualize.kite(
  photos,
  title = "What lives at what depth",
  subtitle = "Each shape is as wide as that group's average cover within each 1-m depth band",
  caption = survey_caption
)
save.figure(kite_fig, "3_zonation_kite")
## END zonation summary ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## 4. supporting: how abrupt are the zone edges? ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## background for the zone boundaries on the strips (zonation.R) -- more for
## the report than the talk
smw_fig <- visualize.smw.profile(
  smw, photos, boundaries,
  title = "Gradual on Transect 1, abrupt edges on Transect 2",
  subtitle = paste("Community change between neighbouring stretches of photos (split moving window, windows of 4-10 photos),",
                   "\nscaled against the same photos in random order. Dashed verticals: zone boundaries drawn on the transect strips."),
  caption = survey_caption
)
save.figure(smw_fig, "4_zone_edge_strength")
## END zone-edge strength ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## 5. NMDS path: the community's route up the slope ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nmds_fig <- visualize.nmds.path(
  nmds_points, nmds_vectors,
  title = "The community's path up the slope",
  subtitle = paste("Each point is one photo, placed so photos with similar communities sit close together. The faint line joins",
                   "\nphotos in survey order; the bold line follows a 5-photo (~5 m) average, deep to shallow."),
  caption = sprintf(paste("Photo-level NMDS of both transects together (Bray-Curtis, 33 categories), stress = %.2f;",
                          "NMDS1 rotated to align with depth (r² = %.2f). Grey points: the other transect.",
                          "\nCompass: cover groups fitted as vectors (envfit), showing groups with r² ≥ 0.1.\n%s"),
                    nmds_ord$stress, depth_r2, survey_caption)
)
save.figure(nmds_fig, "5_nmds_path")
## END NMDS path ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
