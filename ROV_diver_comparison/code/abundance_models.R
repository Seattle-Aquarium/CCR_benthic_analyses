## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Abundance models: ROV vs. diver comparison ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Author: Rachael Aber ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Data assembly (joining ROV + diver, transect_id / key derivation) lives
## upstream in build_combined_abundance.R -- this script starts from its
## output, so it only ever contains modeling.




## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls())


## add libraries
library(tidyverse)
library(lme4)
library(MASS)  ## for glm.nb() -- ships with base R, no separate install needed


## set working directory one level up and verify
setwd("../")
getwd()


## relative file paths
combined_input <- "results/combined"
combined_output <- "results/combined"
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## read combined data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat_abundance <- read.csv(file.path(combined_input, "ROV_diver_abundance_combined.csv"))

## explicit factor level order so every model below reads its "type"
## coefficient as ROV relative to a diver reference (R's default alphabetical
## ordering isn't reliable here and would make the coefficient name/sign
## inconsistent depending on locale)
dat_abundance <- dat_abundance %>% mutate(type = factor(type, levels = c("diver", "ROV")))
## END read data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## abundance visualization ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(dat_abundance, aes(x = transect_id, y = ochre_mottled_star, col = type)) +
  geom_point() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

ggplot(dat_abundance, aes(x = transect_id, y = cancer_crab, col = type)) +
  geom_point() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

ggplot(dat_abundance, aes(x = transect_id, y = kelp_crab, col = type)) +
  geom_point()+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )
## END visualization ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## abundance models: ochre mottled star example ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ochre_mod <- glmer(ochre_mottled_star ~ type + site + season + depth + (1|transect_id),
                   data = dat_abundance,
               family = poisson(link = "log"))
summary(ochre_mod)

# Check nb
ochre_mod_disp <- glmer.nb(ochre_mottled_star ~ type + site + season + depth + (1|transect_id),
                   data = dat_abundance)
summary(ochre_mod_disp)
## END ochre mottled star example ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## abundance models across all 10 overlapping taxa ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## negative binomial GLMM, extending the ochre mottled star example above to
## every taxon in dat_abundance, extracting the type (ROV vs. diver)
## coefficient from each into one tidy summary table. Note this is a log
## link, so the exponentiated coefficient is a rate ratio, not an odds ratio
## like the percent-cover tables -- same "ROV relative to diver"
## interpretation, different quantity. Same convergence-safe wrapper as the
## percent-cover loop: a hard failure on one taxon is recorded rather than
## halting the loop, and any convergence warning (fit still succeeds, but
## flagged -- e.g. for the sparser taxa) is captured in the note column
## rather than silently dropped
abundance_taxa <- c("ochre_mottled_star", "cancer_crab", "burrowing_sea_cucumber",
                    "kelp_crab", "leather_star", "plumose_anemone",
                    "green_white_urchin", "california_sea_cucumber",
                    "blood_star", "large_anemone")

fit_abundance_nb <- function(taxon, data) {
  f <- as.formula(paste0(taxon, " ~ type + site + season + depth + (1|transect_id)"))
  glmer.nb(f, data = data)
}

fit_and_report_abundance <- function(taxon, data) {
  warn_msgs <- character(0)
  fit <- withCallingHandlers(
    tryCatch(fit_abundance_nb(taxon, data), error = function(e) e),
    warning = function(w) {
      warn_msgs <<- c(warn_msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(fit = fit, warnings = warn_msgs)
}

abundance_results <- list()
abundance_models <- list()

for (taxon in abundance_taxa) {
  res <- fit_and_report_abundance(taxon, dat_abundance)
  fit <- res$fit
  warn_note <- if (length(res$warnings) > 0) paste(res$warnings, collapse = "; ") else NA_character_

  if (inherits(fit, "merMod")) {
    abundance_models[[taxon]] <- fit
    s <- summary(fit)$coefficients
    abundance_results[[taxon]] <- tibble(
      taxon = taxon,
      converged = length(res$warnings) == 0,  ## fit returned, but only "clean" if warning-free
      note = warn_note,
      estimate_lograte = s["typeROV", "Estimate"],
      se = s["typeROV", "Std. Error"],
      z_value = s["typeROV", "z value"],
      p_value = s["typeROV", "Pr(>|z|)"],
      rate_ratio_ROV_vs_diver = exp(estimate_lograte)
    )
  } else {
    abundance_results[[taxon]] <- tibble(
      taxon = taxon,
      converged = FALSE,
      note = conditionMessage(fit),
      estimate_lograte = NA_real_, se = NA_real_, z_value = NA_real_,
      p_value = NA_real_, rate_ratio_ROV_vs_diver = NA_real_
    )
  }
}

abundance_results <- bind_rows(abundance_results)
print(abundance_results, width = Inf)

write.csv(abundance_results, file.path(combined_output, "abundance_model_results.csv"), row.names = FALSE)
## END all 10 taxa ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## simplify the random effects for the flagged taxa ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## for the taxa flagged above (degenerate Hessian, near-unidentifiability, or
## iteration-limit warnings): drop the (1|transect_id) random intercept and
## refit as a plain (non-mixed) negative binomial GLM via MASS::glm.nb()
## instead. With only 12 transect_id groups, the mixed model was being asked
## to simultaneously estimate a random-intercept variance and an NB
## dispersion parameter from too little data, and for these taxa that
## estimation collapsed or nearly did. This trades away explicit modeling of
## the repeated-measures structure (a transect's summer and winter
## observations are no longer modeled as non-independent) for numerical
## stability -- standard errors from this simplified fit are a reasonable
## approximation, not a fully repeated-measures-corrected one, and that
## trade-off should be noted wherever these taxa are reported
convergence_flagged_taxa <- abundance_results %>% filter(!converged) %>% pull(taxon)
convergence_flagged_taxa

fit_abundance_nb_simple <- function(taxon, data) {
  f <- as.formula(paste0(taxon, " ~ type + site + season + depth"))
  MASS::glm.nb(f, data = data)
}

fit_and_report_abundance_simple <- function(taxon, data) {
  warn_msgs <- character(0)
  fit <- withCallingHandlers(
    tryCatch(fit_abundance_nb_simple(taxon, data), error = function(e) e),
    warning = function(w) {
      warn_msgs <<- c(warn_msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(fit = fit, warnings = warn_msgs)
}

abundance_results_simplified <- list()
abundance_models_simplified <- list()

for (taxon in convergence_flagged_taxa) {
  res <- fit_and_report_abundance_simple(taxon, dat_abundance)
  fit <- res$fit
  warn_note <- if (length(res$warnings) > 0) paste(res$warnings, collapse = "; ") else NA_character_

  if (inherits(fit, "negbin")) {
    abundance_models_simplified[[taxon]] <- fit
    s <- summary(fit)$coefficients
    abundance_results_simplified[[taxon]] <- tibble(
      taxon = taxon,
      model = "nb_no_random_effect",
      converged = length(res$warnings) == 0,
      note = warn_note,
      estimate_lograte = s["typeROV", "Estimate"],
      se = s["typeROV", "Std. Error"],
      z_value = s["typeROV", "z value"],
      p_value = s["typeROV", "Pr(>|z|)"],
      rate_ratio_ROV_vs_diver = exp(estimate_lograte)
    )
  } else {
    abundance_results_simplified[[taxon]] <- tibble(
      taxon = taxon,
      model = "nb_no_random_effect",
      converged = FALSE,
      note = conditionMessage(fit),
      estimate_lograte = NA_real_, se = NA_real_, z_value = NA_real_,
      p_value = NA_real_, rate_ratio_ROV_vs_diver = NA_real_
    )
  }
}

abundance_results_simplified <- bind_rows(abundance_results_simplified)
print(abundance_results_simplified, width = Inf)
## END simplify random effects ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## rock crab (cancer_crab) specific check ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Manual review of the ROV imagery indicated some double-counting of crabs
## in dense aggregations, and the ROV's wider imaging footprint plausibly
## captures additional individuals beyond what divers see along their
## narrower swath -- both effects would be most pronounced at exactly the
## one outlier transect (Centennial Park, transect 1, winter: diver 62, ROV
## 130, an order of magnitude above every other transect's count). Rather
## than smoothing this over across all taxa, we test dropping just this one
## transect (both platforms) from the rock crab model specifically, and only
## the rock crab model -- every other taxon keeps the full 24 transects.
## This is treated as documented real-world methodological difference
## between the platforms, not a statistical convenience, so it is scoped as
## narrowly as possible.
dat_abundance_cancer_crab <- dat_abundance %>%
  filter(key != "CP_1_winter")

nrow(dat_abundance_cancer_crab)  ## expect 46 (48 total minus the 2 rows -- diver + ROV -- for this transect)

## try the mixed model on the excluded data first
res_cancer_crab_excl_mixed <- fit_and_report_abundance("cancer_crab", dat_abundance_cancer_crab)
fit_cancer_crab_excl_mixed <- res_cancer_crab_excl_mixed$fit
cat("mixed model, outlier excluded -- isSingular:",
    if (inherits(fit_cancer_crab_excl_mixed, "merMod")) lme4::isSingular(fit_cancer_crab_excl_mixed) else NA,
    "\n")

## the mixed model comes back singular (the between-transect random-effect
## variance collapses toward zero once the one extreme transect is removed
## -- unsurprising, since that transect was a large share of what made
## transects differ from each other in the first place). A singular fit and
## a plain fixed-effects-only fit are then numerically near-identical, so we
## go straight to the simplified (no random effect) specification on the
## excluded data, same as the other three flagged taxa, for a consistent
## table structure
res_cancer_crab_excl <- fit_and_report_abundance_simple("cancer_crab", dat_abundance_cancer_crab)
fit_cancer_crab_excl <- res_cancer_crab_excl$fit
warn_note_cancer_crab_excl <- if (length(res_cancer_crab_excl$warnings) > 0) paste(res_cancer_crab_excl$warnings, collapse = "; ") else NA_character_

if (inherits(fit_cancer_crab_excl, "negbin")) {
  s <- summary(fit_cancer_crab_excl)$coefficients
  cancer_crab_result_excl <- tibble(
    taxon = "cancer_crab",
    model = "nb_no_random_effect_excl_CP_1_winter",
    converged = length(res_cancer_crab_excl$warnings) == 0,
    note = warn_note_cancer_crab_excl,
    estimate_lograte = s["typeROV", "Estimate"],
    se = s["typeROV", "Std. Error"],
    z_value = s["typeROV", "z value"],
    p_value = s["typeROV", "Pr(>|z|)"],
    rate_ratio_ROV_vs_diver = exp(estimate_lograte)
  )
} else {
  cancer_crab_result_excl <- tibble(
    taxon = "cancer_crab",
    model = "nb_no_random_effect_excl_CP_1_winter",
    converged = FALSE,
    note = conditionMessage(fit_cancer_crab_excl),
    estimate_lograte = NA_real_, se = NA_real_, z_value = NA_real_,
    p_value = NA_real_, rate_ratio_ROV_vs_diver = NA_real_
  )
}

print(cancer_crab_result_excl, width = Inf)

## the excluded-data fit still isn't perfectly warning-free (a mild
## "iteration limit reached" on the dispersion parameter, well short of the
## full-data fit's "alternation limit reached"), but it's the best available
## for this taxon: tighter SE, more stable, and numerically consistent with
## the singular mixed-model version above. Used regardless of its own
## converged flag, since the alternative (the unexcluded fit) is worse on
## every measure -- this is disclosed explicitly in the table note and the
## interpretation text rather than papered over
cancer_crab_final <- cancer_crab_result_excl

abundance_results_final <- bind_rows(
  abundance_results %>% filter(converged) %>% mutate(model = "nb_glmm", .after = taxon),
  abundance_results_simplified %>% filter(taxon != "cancer_crab"),
  cancer_crab_final
) %>%
  mutate(taxon = factor(taxon, levels = abundance_taxa)) %>%
  arrange(taxon) %>%
  mutate(taxon = as.character(taxon))

print(abundance_results_final, width = Inf)

write.csv(abundance_results_final, file.path(combined_output, "abundance_model_results_final.csv"), row.names = FALSE)
## END rock crab check ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
