## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## functions for detecting community zone boundaries along a transect ~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Split moving-window (SMW) boundary analysis (Webster 1973; Ludwig &
## Cornelius 1987; Cornelius & Reynolds 1991), the standard way to find where
## a community changes along an ordered transect. A window of w consecutive
## photos slides along the transect; at each position it is split in half and
## the Bray-Curtis dissimilarity between the two halves' mean composition is
## recorded. Where the window straddles a zone boundary the two halves differ
## sharply and dissimilarity peaks. Each window size's dissimilarity profile
## is standardized against the same transect with its photos randomly
## reordered (z = (D - mean_random) / sd_random), and the z-profiles are
## averaged across several window sizes so a boundary has to show up at more
## than one spatial scale. This is description of one transect -- photos are
## the units -- not inference about the site.




## Bray-Curtis dissimilarity between two composition vectors
bray.curtis <- function(a, b) sum(abs(a - b)) / sum(a + b)


## SMW dissimilarity profile for one window size: D at every split position
## k (between photos k and k+1) with a full half-window on each side
smw.profile <- function(X, w) {
  half <- w / 2
  n <- nrow(X)
  k <- seq(half, n - half)
  vapply(k, function(i) {
    bray.curtis(colMeans(X[(i - half + 1):i, , drop = FALSE]),
                colMeans(X[(i + 1):(i + half), , drop = FALSE]))
  }, numeric(1)) %>%
    setNames(k)
}


## average z-scored SMW profile across window sizes for one transect.
## X: photos (rows, in survey order) x categories (proportions).
## Returns one row per split position: k (boundary falls between photo k and
## k + 1), mean_z, and the number of window sizes contributing at that k.
smw.analysis <- function(X, window_sizes = c(4, 6, 8, 10), n_perm = 999, seed = 1) {
  set.seed(seed)
  X <- X[, colSums(X) > 0, drop = FALSE]
  n <- nrow(X)

  purrr::map_dfr(window_sizes, function(w) {
    obs <- smw.profile(X, w)
    ## null: the same photos in random order. Mean and SD are pooled over all
    ## positions and permutations (Cornelius & Reynolds 1991)
    rand <- replicate(n_perm, smw.profile(X[sample(n), , drop = FALSE], w))
    tibble(window = w, k = as.integer(names(obs)), D = obs,
           z = (obs - mean(rand)) / sd(rand))
  }) %>%
    group_by(k) %>%
    summarise(mean_z = mean(z), n_windows = n(), .groups = "drop")
}


## pick boundaries: local maxima of mean_z that exceed `z_crit`, keeping the
## highest peak and dropping any lesser peak within `min_sep` photos of an
## already-kept one
pick.boundaries <- function(profile, z_crit = 2, min_sep = 6) {
  cand <- profile %>%
    filter(mean_z > z_crit,
           mean_z >= lag(mean_z, default = -Inf),
           mean_z >= lead(mean_z, default = -Inf)) %>%
    arrange(desc(mean_z))
  kept <- integer(0)
  for (i in seq_len(nrow(cand))) {
    if (all(abs(cand$k[i] - kept) >= min_sep)) kept <- c(kept, cand$k[i])
  }
  profile %>% filter(k %in% kept) %>% arrange(k)
}


## zones by sequence-constrained clustering (CONISS; Grimm 1987): photos are
## merged into contiguous groups -- only neighbours along the transect may join
## -- each merge chosen to add the least within-group dispersion (Bray-Curtis).
## The number of zones is the number of splits that each explain more of the
## dispersion than a broken-stick model expects by chance (Bennett 1996),
## counted from the first split until the first one that doesn't. Unlike SMW,
## this partitions a gradual gradient too, so it answers "where would you draw
## the zones" rather than "where are the abrupt edges".
## Returns the boundary positions k (between photo k and k + 1).
coniss.zones <- function(X, max_groups = 10) {
  X <- X[, colSums(X) > 0, drop = FALSE]
  cl <- rioja::chclust(vegan::vegdist(X, method = "bray"), method = "coniss")
  bs <- rioja::bstick(cl, ng = max_groups, plot = FALSE)
  above <- bs$dispersion > bs$bstick
  n_zones <- if (!above[1]) 1 else bs$nGroups[max(which(cumprod(above) == 1))]
  zone_of_photo <- cutree(cl, k = n_zones)
  list(n_zones = n_zones, zone = zone_of_photo,
       boundary_k = which(diff(zone_of_photo) != 0),
       bstick = bs)
}


## short names for zone labels on slides
short_group_names <- c(
  "Silt" = "Silt", "Sand & shell" = "Sand", "Hard substrate" = "Cobble",
  "Sugar kelp" = "Sugar kelp", "Sieve kelp" = "Sieve kelp", "Other brown algae" = "Brown algae",
  "Red algae" = "Red algae", "Green algae" = "Green algae", "Other" = "Other"
)


## describe each zone between boundaries: photo range, depth range, mean
## cover of every group, and a short label naming the groups that make up at
## least `label_min` of the zone's cover on average (largest first, at most
## `max_names`, one per line so narrow zones can still carry their label)
describe.zones <- function(df, boundary_k, label_min = 0.15, max_names = 3) {
  edges <- c(0, sort(boundary_k), nrow(df))
  purrr::map_dfr(seq_len(length(edges) - 1), function(z) {
    rows <- df %>% filter(photo_index > edges[z], photo_index <= edges[z + 1])
    cover <- colMeans(rows[names(cover_groups)])
    top <- sort(cover[cover >= label_min], decreasing = TRUE)
    top <- head(top, max_names)
    tibble(zone = z,
           first_photo = min(rows$photo_index), last_photo = max(rows$photo_index),
           x_mid = (min(rows$photo_index) + max(rows$photo_index)) / 2,
           depth_from = max(rows$depth_mllw_m), depth_to = min(rows$depth_mllw_m),
           label = paste(short_group_names[names(top)], collapse = "\n"),
           !!!as.list(round(cover, 3)))
  })
}




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
