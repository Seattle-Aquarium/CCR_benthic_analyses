## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ROV-diver correlations: do the two platforms track the same pattern? ~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## The GLMMs in abundance_models.R and percent-cover_models.R ask a single
## question: does one platform record systematically MORE (or less) than the
## other, averaged over everything else? That is a question about level. It is
## silent on the second, arguably more important question for a monitoring
## program: do the two platforms rise and fall TOGETHER across transects --
## i.e. when divers call a transect high, does the ROV call it high too?
##
## This script answers that second question, with the same structure as the
## model scripts: all 8 percent-cover categories across all 24 transects,
## then the winter-only refit for CCA + the 5 substrate categories, then all
## 10 abundance taxa across all 24 transects. It reads the same two combined
## data files the model scripts read and contains no data assembly of its own.
##
## THE Z-SCORE IDENTITY -- important, and the reason there is one set of
## correlation tables here and not two. Pearson's r is invariant to any linear
## rescaling of either series, and z-scoring (x - mean) / sd is exactly such a
## rescaling. So the correlation between two platforms' z-scored series is
## NUMERICALLY IDENTICAL to the correlation between their raw series, to the
## last decimal place -- the report already states this in
## subsubsec:distinct_taxa, and it is why r = 0.96 for Saccharina is reported
## as both "the correlation of the z-scores" and "the correlation of the raw
## values" without contradiction. Computing r on z-scores per category would
## therefore reproduce the r column below exactly.
##
## What z-scoring DOES buy, and what the z-score outputs at the bottom of this
## script provide, is a common scale on which otherwise incommensurable series
## can be compared and pooled:
##   1. mean/max |dz| -- the average and worst per-transect disagreement,
##      expressed in standard deviations, which (unlike r) is on an
##      interpretable "how far apart were they on this transect" scale;
##   2. a pooled, cross-taxon r -- every taxon standardized and stacked into a
##      single series, giving one overall agreement statistic per dataset.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## NOTE: run via RunMe.R -- packages, working directory, and the
## results_combined path variable used below are all set up there.
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## read combined data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## the same two files abundance_models.R and percent-cover_models.R read
dat_pc_cor <- read.csv(file.path(results_combined, "ROV_diver_percent_cover_combined.csv"))
dat_abundance_cor <- read.csv(file.path(results_combined, "ROV_diver_abundance_combined.csv"))
## END read data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## category / taxon lists and display names ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## identical to the lists in percent-cover_models.R and abundance_models.R, so
## the correlation tables line up row-for-row with the model tables. The
## display names are the ones already used in the report's model tables, kept
## here so the LaTeX tables can be generated from this output rather than
## re-typed by hand
pc_categories_cor <- c("cover_red_algae", "combined_green_algae",
                       "cover_crustose_coralline", "combined_substrate_boulder",
                       "substrate_rock_.15.25cm.wa.", "combined_substrate_pebble",
                       "substrate_sand", "substrate_shell_hash")

## winter-only subset: CCA + the 5 substrate categories, dropping red and green
## algae, matching percent-cover_models.R's winter_categories
winter_categories_cor <- c("cover_crustose_coralline", "combined_substrate_boulder",
                           "substrate_rock_.15.25cm.wa.", "combined_substrate_pebble",
                           "substrate_sand", "substrate_shell_hash")

abundance_taxa_cor <- c("ochre_mottled_star", "cancer_crab", "burrowing_sea_cucumber",
                        "kelp_crab", "leather_star", "plumose_anemone",
                        "green_white_urchin", "california_sea_cucumber",
                        "blood_star", "large_anemone")

pc_display_names <- c(
  cover_red_algae             = "Red algae cover",
  combined_green_algae        = "Green algae cover",
  cover_crustose_coralline    = "Crustose coralline algae (CCA)",
  combined_substrate_boulder  = "Boulder substrate",
  substrate_rock_.15.25cm.wa. = "Rock, 15-25 cm (cobble) substrate",
  combined_substrate_pebble   = "Pebble substrate",
  substrate_sand              = "Sand substrate",
  substrate_shell_hash        = "Shell hash substrate"
)

abundance_display_names <- c(
  ochre_mottled_star     = "Ochre/mottled star",
  cancer_crab            = "Rock crab",
  burrowing_sea_cucumber = "Burrowing sea cucumber",
  kelp_crab              = "Kelp crab",
  leather_star           = "Leather star",
  plumose_anemone        = "Plumose anemone",
  green_white_urchin     = "Green/pallid urchin",
  california_sea_cucumber = "California sea cucumber",
  blood_star             = "Blood star",
  large_anemone          = "Large anemone"
)
## END category lists ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## pair the two platforms onto one row per transect x season ~~~~~~~~~~~~~~~~~~~
## both combined files are long in `type` (one diver row and one ROV row per
## site x transect x season), and `key` (e.g. "CP_1_summer") is the unique
## identifier for that combination. Widening on `type` gives the paired
## (diver, ROV) vector the correlation needs, and preserves site / season /
## depth for the within-site variants below
pair.platforms <- function(data, variable) {
  data %>%
    select(key, site, season, depth, transect,
           type, value = all_of(variable)) %>%
    pivot_wider(names_from = type, values_from = value) %>%
    rename(diver_value = diver, rov_value = ROV) %>%
    filter(!is.na(diver_value), !is.na(rov_value)) %>%
    arrange(site, season, transect)
}
## END pairing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## pooled within-site correlation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## the headline r below is computed across all 24 transects, which pools two
## structurally very different sites. For any category concentrated at one site
## (Agarum at the breakwater, sand at Centennial Park, boulder likewise), a
## large share of that r is simply both platforms agreeing that one site has
## more of it than the other -- a real form of agreement, but a coarse one. The
## pooled within-site correlation removes each site's own mean from both
## platforms' series first, so what remains is only the transect-to-transect
## agreement WITHIN a site. It is the stricter test of whether the two
## platforms resolve the same fine-grained spatial pattern, and it is the
## direct answer to the reviewer's "at the transect scale (and perhaps site
## level)" question. Reported as an extra column, not in the report's tables.
##
## df is n - (number of sites) - 1 rather than cor.test()'s n - 2, because one
## mean per site was estimated and spent before correlating the residuals.
within.site.correlation <- function(diver_value, rov_value, site) {
  site <- as.factor(site)
  n <- length(diver_value)
  n_groups <- nlevels(droplevels(site))
  df <- n - n_groups - 1

  ## residuals after removing each site's own mean from each platform's series
  diver_resid <- diver_value - ave(diver_value, site)
  rov_resid   <- rov_value   - ave(rov_value,   site)

  if (df < 1 || sd(diver_resid) == 0 || sd(rov_resid) == 0) {
    return(list(r = NA_real_, p = NA_real_, df = df))
  }

  r <- cor(diver_resid, rov_resid)
  ## guard against |r| == 1 exactly, which would divide by zero below
  if (abs(r) >= 1) return(list(r = r, p = 0, df = df))

  t_stat <- r * sqrt(df / (1 - r^2))
  list(r = r, p = 2 * pt(-abs(t_stat), df = df), df = df)
}
## END within-site correlation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## correlate one category/taxon ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## returns every statistic used by any table below, in one row:
##
##   r, r_lower/r_upper, r_p   Pearson correlation of the paired transect
##                             values, its 95% CI (Fisher z) and p-value. This
##                             IS the z-score correlation -- see the header
##                             note -- so no separate Pearson-of-z-scores
##                             column is computed or needed.
##   rho, rho_p                Spearman rank correlation. Included because
##                             several of these series are dominated by one or
##                             two extreme transects (rock crab's Centennial
##                             Park winter transect 1 above all), and Pearson's
##                             r on such a series largely measures whether the
##                             two platforms agree about that one point.
##                             Spearman asks the same question of the rank
##                             ordering only, so a large r / small rho gap is a
##                             direct flag that a single transect is carrying
##                             the result.
##   mean_abs_dz, max_abs_dz   average and worst per-transect gap between the
##                             two platforms' z-scored values, in SD units --
##                             the interpretable common-scale companion to r.
##   r_within_site, ...        see within.site.correlation() above.
##
## A series with zero variance under either platform (a category recorded
## nowhere, or at a constant value) has no defined correlation; those return
## NA with a note rather than erroring, the same convention the model scripts
## use for a failed fit.
correlate.platforms <- function(variable, data, display_names) {
  paired <- pair.platforms(data, variable)
  n <- nrow(paired)

  base_row <- tibble(
    variable = variable,
    display_name = unname(display_names[variable]),
    n = n,
    diver_mean = mean(paired$diver_value),
    rov_mean = mean(paired$rov_value)
  )

  if (n < 4 || sd(paired$diver_value) == 0 || sd(paired$rov_value) == 0) {
    return(bind_cols(base_row, tibble(
      r = NA_real_, r_lower = NA_real_, r_upper = NA_real_, r_p = NA_real_,
      rho = NA_real_, rho_p = NA_real_,
      mean_abs_dz = NA_real_, max_abs_dz = NA_real_,
      r_within_site = NA_real_, r_within_site_p = NA_real_,
      note = "zero variance in at least one platform's series -- correlation undefined"
    )))
  }

  pearson <- cor.test(paired$diver_value, paired$rov_value, method = "pearson")
  ## exact = FALSE: these series are full of tied zeros, and Spearman's exact
  ## p-value is undefined under ties -- the asymptotic p is the standard
  ## fallback and avoids a warning on nearly every taxon
  spearman <- suppressWarnings(
    cor.test(paired$diver_value, paired$rov_value,
             method = "spearman", exact = FALSE)
  )

  ## z-score each platform's series against its OWN mean and SD (never a
  ## shared one -- the whole point is that the two platforms' units are not
  ## assumed commensurable), then measure the per-transect gap between them
  dz <- abs(as.numeric(scale(paired$diver_value)) - as.numeric(scale(paired$rov_value)))

  within <- within.site.correlation(paired$diver_value, paired$rov_value, paired$site)

  bind_cols(base_row, tibble(
    r = unname(pearson$estimate),
    r_lower = pearson$conf.int[1],
    r_upper = pearson$conf.int[2],
    r_p = pearson$p.value,
    rho = unname(spearman$estimate),
    rho_p = spearman$p.value,
    mean_abs_dz = mean(dz),
    max_abs_dz = max(dz),
    r_within_site = within$r,
    r_within_site_p = within$p,
    note = NA_character_
  ))
}
## END single-category correlation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## pooled, cross-taxon z-score correlation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## z-score every category/taxon's series within each platform, stack them all
## into one long pair of vectors, and correlate once. This is the single
## number the z-score standardization uniquely makes possible: an overall
## "across everything we measured, how well do the two platforms track each
## other" statistic, which raw values (in counts vs. proportions, across taxa
## of wildly different abundance) cannot be pooled into.
##
## Note this pooled r is algebraically the unweighted mean of the per-series
## Pearson r values when every series has the same n -- which is the case here
## (24 transects for every category and taxon, 12 in the winter subset). It is
## reported because it is the natural summary, not because it adds independent
## information beyond the per-series column.
pooled.zscore.correlation <- function(variables, data, display_names) {
  stacked <- lapply(variables, function(v) {
    paired <- pair.platforms(data, v)
    if (sd(paired$diver_value) == 0 || sd(paired$rov_value) == 0) return(NULL)
    tibble(
      variable = v,
      z_diver = as.numeric(scale(paired$diver_value)),
      z_rov = as.numeric(scale(paired$rov_value))
    )
  })
  stacked <- bind_rows(stacked)

  fit <- cor.test(stacked$z_diver, stacked$z_rov, method = "pearson")

  tibble(
    n_series = length(unique(stacked$variable)),
    n_pairs = nrow(stacked),
    pooled_r = unname(fit$estimate),
    pooled_r_lower = fit$conf.int[1],
    pooled_r_upper = fit$conf.int[2],
    pooled_r_p = fit$p.value,
    mean_abs_dz = mean(abs(stacked$z_diver - stacked$z_rov))
  )
}
## END pooled correlation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## long-form z-scored series, for the record and for figures ~~~~~~~~~~~~~~~~~~~
## one row per category/taxon x transect x platform, on the standardized scale.
## This is the same transformation build.kelp.comparison.data() /
## prep.kelp.standardized.data() apply to sugar and sieve kelp for the z-score
## overlay figure, generalized to every category and taxon, so any of them can
## be plotted the same way without recomputing the standardization.
build.zscore.long <- function(variables, data, display_names, dataset_label) {
  rows <- lapply(variables, function(v) {
    paired <- pair.platforms(data, v)
    if (sd(paired$diver_value) == 0 || sd(paired$rov_value) == 0) return(NULL)
    paired %>%
      mutate(dataset = dataset_label,
             variable = v,
             display_name = unname(display_names[v]),
             z_diver = as.numeric(scale(diver_value)),
             z_rov = as.numeric(scale(rov_value)),
             dz = z_diver - z_rov) %>%
      select(dataset, variable, display_name, key, site, season, depth, transect,
             diver_value, rov_value, z_diver, z_rov, dz)
  })
  bind_rows(rows)
}
## END z-score long form ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## 1. percent-cover correlations, all 8 categories, all 24 transects ~~~~~~~~~~~
cor_pc_all <- bind_rows(lapply(
  pc_categories_cor, correlate.platforms,
  data = dat_pc_cor, display_names = pc_display_names
))
print(cor_pc_all, width = Inf)

write.csv(cor_pc_all,
          file.path(results_combined, "correlation_percent_cover_all.csv"),
          row.names = FALSE)
## END all-category percent-cover ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## 2. percent-cover correlations, winter only, CCA + 5 substrate ~~~~~~~~~~~~~~~
## the same restriction percent-cover_models.R applies for its winter refit:
## in winter, annual algae and kelp cover is largely senesced, so the ROV's
## point classifications are not competing with overlying vegetation for the
## substrate signal. If that competition is what drives the platforms apart,
## agreement should be tighter here than in the all-season result above.
dat_pc_cor_winter <- dat_pc_cor %>% filter(season == "winter")

cor_pc_winter <- bind_rows(lapply(
  winter_categories_cor, correlate.platforms,
  data = dat_pc_cor_winter, display_names = pc_display_names
))
print(cor_pc_winter, width = Inf)

write.csv(cor_pc_winter,
          file.path(results_combined, "correlation_percent_cover_winter_substrate.csv"),
          row.names = FALSE)
## END winter-only percent-cover ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## 3. abundance correlations, all 10 taxa, all 24 transects ~~~~~~~~~~~~~~~~~~~~
## correlated on raw transect totals, matching both abundance_models.R (which
## models counts with no per-photo offset) and Fig. fig:abundance_head-to-head
## (which plots transect totals), so these r values are exactly what would be
## printed onto that figure's panels. Pearson's r is scale-invariant, so the
## fact that a diver total comes from one 30 x 2 m swath and an ROV total from
## ~60 photos does not affect it.
cor_abundance <- bind_rows(lapply(
  abundance_taxa_cor, correlate.platforms,
  data = dat_abundance_cor, display_names = abundance_display_names
))
print(cor_abundance, width = Inf)

write.csv(cor_abundance,
          file.path(results_combined, "correlation_abundance.csv"),
          row.names = FALSE)
## END abundance correlations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## 3b. rock crab outlier sensitivity ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## abundance_models.R already refits the rock crab model with the Centennial
## Park winter transect 1 outlier removed (62 diver / 130 ROV, an order of
## magnitude above every other transect in the study), and the report reports
## both estimates. The correlation needs the same treatment, and needs it more
## urgently: rock crab's all-data r is the single highest value in the entire
## abundance table, which read at face value would say the two platforms agree
## about rock crab better than about anything else they counted. They do not.
## Every one of the other 23 transects sits between 0 and 3 individuals for
## both platforms, so a Pearson correlation over the full 24 is very nearly a
## two-point correlation -- the outlier and the cloud -- and reduces to a
## statement that both platforms agree that one transect had a great many rock
## crabs. Spearman's rho already flags this (the rank ordering of the other 23
## transects carries almost no signal), and dropping the one transect confirms
## it directly. Both versions are written out; the report should quote the
## all-data value only alongside this one.
rock_crab_outlier_key <- "CP_1_winter"

dat_abundance_no_outlier <- dat_abundance_cor %>%
  filter(key != rock_crab_outlier_key)

cor_rock_crab_sensitivity <- bind_rows(
  correlate.platforms("cancer_crab", dat_abundance_cor,
                      abundance_display_names) %>%
    mutate(subset = "all 24 transects", .after = display_name),
  correlate.platforms("cancer_crab", dat_abundance_no_outlier,
                      abundance_display_names) %>%
    mutate(subset = paste0(rock_crab_outlier_key, " excluded"), .after = display_name)
)
print(cor_rock_crab_sensitivity, width = Inf)

write.csv(cor_rock_crab_sensitivity,
          file.path(results_combined, "correlation_abundance_rock_crab_sensitivity.csv"),
          row.names = FALSE)
## END rock crab sensitivity ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## 4. z-score outputs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## per-series z-score summaries (r, repeated from above by the identity in this
## script's header, plus the mean/max |dz| columns that only exist on the
## standardized scale), with the pooled cross-taxon row appended as the last
## row of each table.
zscore.table <- function(cor_table, pooled, label) {
  per_series <- cor_table %>%
    select(variable, display_name, n, r, r_p, mean_abs_dz, max_abs_dz)

  pooled_row <- tibble(
    variable = "POOLED",
    display_name = paste0("All ", pooled$n_series, " series pooled (standardized)"),
    n = pooled$n_pairs,
    r = pooled$pooled_r,
    r_p = pooled$pooled_r_p,
    mean_abs_dz = pooled$mean_abs_dz,
    max_abs_dz = NA_real_
  )

  bind_rows(per_series, pooled_row) %>% mutate(dataset = label, .before = variable)
}

pooled_pc_all <- pooled.zscore.correlation(pc_categories_cor, dat_pc_cor, pc_display_names)
pooled_pc_winter <- pooled.zscore.correlation(winter_categories_cor, dat_pc_cor_winter, pc_display_names)
pooled_abundance <- pooled.zscore.correlation(abundance_taxa_cor, dat_abundance_cor, abundance_display_names)

zscore_pc <- bind_rows(
  zscore.table(cor_pc_all, pooled_pc_all, "percent-cover, all seasons"),
  zscore.table(cor_pc_winter, pooled_pc_winter, "percent-cover, winter only")
)
zscore_abundance <- zscore.table(cor_abundance, pooled_abundance, "abundance, all seasons")

print(zscore_pc, width = Inf)
print(zscore_abundance, width = Inf)

write.csv(zscore_pc,
          file.path(results_combined, "correlation_zscore_percent_cover.csv"),
          row.names = FALSE)
write.csv(zscore_abundance,
          file.path(results_combined, "correlation_zscore_abundance.csv"),
          row.names = FALSE)

## the underlying standardized series themselves, one row per
## category/taxon x transect, so any of them can be plotted on the shared
## unitless axis used by the sugar/sieve kelp z-score overlay figure
zscore_series <- bind_rows(
  build.zscore.long(pc_categories_cor, dat_pc_cor, pc_display_names,
                    "percent-cover, all seasons"),
  build.zscore.long(winter_categories_cor, dat_pc_cor_winter, pc_display_names,
                    "percent-cover, winter only"),
  build.zscore.long(abundance_taxa_cor, dat_abundance_cor, abundance_display_names,
                    "abundance, all seasons")
)

write.csv(zscore_series,
          file.path(results_combined, "correlation_zscore_series.csv"),
          row.names = FALSE)
## END z-score outputs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
