## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## community zones along each transect ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## NOTE: run via RunMe.R -- needs `photos` from wrangle_transects.R.
##
## Two complementary questions, both on the full label set (33 categories,
## Bray-Curtis), photos in survey order:
##   1. where would you draw the zones?  -> CONISS + broken stick. These are
##      the zones drawn on the transect-strip figures.
##   2. which zone edges are abrupt?     -> split moving window (SMW), z-scored
##      against randomly reordered photos; a boundary needs mean z > 2.
## A transect can have zones (1) without abrupt edges (2) when it changes
## gradually -- which is exactly what Transect 1 does.




zonation <- purrr::map(sort(unique(photos$transect)), function(t) {
  df <- photos %>% filter(transect == t) %>% arrange(photo_index)
  X <- as.matrix(df[names(label_set)])

  cz <- coniss.zones(X)
  smw <- smw.analysis(X) %>% mutate(transect = t, .before = 1)

  list(
    boundaries = tibble(transect = t, k = cz$boundary_k, boundary_index = cz$boundary_k + 0.5),
    zones = describe.zones(df, cz$boundary_k) %>% mutate(transect = t, .before = 1),
    smw = smw,
    smw_edges = pick.boundaries(smw) %>% mutate(boundary_index = k + 0.5)
  )
})

boundaries <- purrr::map_dfr(zonation, "boundaries")
zones      <- purrr::map_dfr(zonation, "zones")
smw        <- purrr::map_dfr(zonation, "smw")
smw_edges  <- purrr::map_dfr(zonation, "smw_edges")


zones %>%
  transmute(transect, zone, photos = paste(first_photo, last_photo, sep = "-"),
            depth_m = sprintf("%.1f-%.1f", depth_from, depth_to),
            label = gsub("\n", " + ", label)) %>%
  print(n = Inf)
cat("\nabrupt edges (SMW mean z > 2), between photo k and k + 1:\n")
print(smw_edges, n = Inf)


save.csv(zones %>% mutate(label = gsub("\n", " + ", label)), results_dir, "FPR_S25_zones_CONISS.csv")
save.csv(smw, results_dir, "FPR_S25_zone_edges_SMW_profile.csv")




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
