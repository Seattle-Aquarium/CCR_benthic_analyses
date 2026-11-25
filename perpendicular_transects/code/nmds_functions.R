## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## NMDS functions: photo-level ordination and its community "path" ~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Same set-up as ROV_diver_comparison/code/NMDS.R -- photo = sample unit,
## Bray-Curtis on the raw category proportions, no autotransform -- applied to
## both transects together so their paths share one ordination space.




## 2-D NMDS of photos x categories (proportions). Categories never recorded
## are dropped first (they carry no information and vegan warns on them).
## The solution is then rotated so NMDS1 runs parallel to `align_with`
## (vegan::MDSrotate) -- here height above the seafloor's deepest point, so
## the deep end sits on the left and the shallow end on the right, matching
## the transect strips. NMDS axes have no inherent orientation, so the
## rotation changes nothing about distances between photos.
run.nmds <- function(X, align_with = NULL, k = 2, trymax = 200, seed = 1) {
  X <- X[, colSums(X) > 0, drop = FALSE]
  set.seed(seed)
  ord <- vegan::metaMDS(X, distance = "bray", k = k, trymax = trymax,
                        autotransform = FALSE, wascores = TRUE, trace = 0)
  if (!is.null(align_with)) ord <- vegan::MDSrotate(ord, align_with)
  ord
}


## each cover group fitted onto the ordination as a vector (vegan::envfit):
## the arrow points the way that group's cover increases, its length scaled
## by sqrt(r^2) so groups the ordination explains poorly get short arrows.
## Every arrow is stretched by one common factor so the longest reaches
## `fill` of the photos' farthest extent from the centre (vegan's
## ordiArrowMul() does the same but reads it off an open plot window).
## Also returns depth's fit (r^2), reported on the figure.
fit.vectors <- function(ord, env, fill = 0.8) {
  fit <- vegan::envfit(ord, env, permutations = 0)
  arrows <- as.data.frame(vegan::scores(fit, display = "vectors"))  # already x sqrt(r2)
  names(arrows) <- c("NMDS1", "NMDS2")
  extent <- max(abs(vegan::scores(ord, display = "sites")))
  arrow_mul <- fill * extent / max(sqrt(arrows$NMDS1^2 + arrows$NMDS2^2))
  arrows %>%
    tibble::rownames_to_column("variable") %>%
    mutate(r2 = fit$vectors$r[variable],
           xend = NMDS1 * arrow_mul, yend = NMDS2 * arrow_mul)
}




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
