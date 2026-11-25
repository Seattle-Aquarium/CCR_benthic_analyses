## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## photo-level NMDS of both transects -> community path up the slope ~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## NOTE: run via RunMe.R -- needs `photos` from wrangle_transects.R.
##
## All 123 photos (both transects) are ordinated together on the full label
## set (Bray-Curtis). Connecting each transect's photos in survey order then
## traces the route its community takes through that space as the ROV climbs
## the slope -- and shows whether the two transects follow the same route.




## ordinate ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
photos <- photos %>% arrange(transect, photo_index)
nmds_ord <- run.nmds(as.matrix(photos[names(label_set)]),
                     align_with = -photos$depth_mllw_m)   # shallow -> +NMDS1

nmds_points <- photos %>%
  select(transect, photo_index, Name, depth_mllw_m, cumulative_distance_m) %>%
  bind_cols(as.data.frame(vegan::scores(nmds_ord, display = "sites")))

## cover groups (and depth, for its r^2) fitted as vectors
nmds_vectors <- fit.vectors(nmds_ord, photos[c(names(cover_groups), "depth_mllw_m")])
depth_r2 <- nmds_vectors$r2[nmds_vectors$variable == "depth_mllw_m"]
nmds_vectors <- nmds_vectors %>% filter(variable != "depth_mllw_m")

cat(sprintf("  NMDS stress = %.3f; depth vs ordination r^2 = %.2f\n", nmds_ord$stress, depth_r2))
## END ordinate ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## save ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
save.csv(nmds_points, results_dir, "FPR_S25_NMDS_points.csv")
save.csv(nmds_vectors, results_dir, "FPR_S25_NMDS_group_vectors.csv")
## END save ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
