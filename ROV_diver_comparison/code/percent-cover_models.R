## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Percent-cover models: ROV vs. diver comparison ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Author: Rachael Aber ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Data assembly (joining ROV + diver, category crosswalks, 0-100 -> 0-1
## conversion, transect_id / key derivation) lives upstream in
## build_combined_percent_cover.R -- this script starts from its output, so
## it only ever contains modeling.




## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls())


## add libraries
library(tidyverse)
library(lme4)
library(glmmTMB)


## set working directory one level up and verify
setwd("../")
getwd()


## relative file paths
combined_input <- "results/combined"
combined_output <- "results/combined"
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## read combined data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat_pc <- read.csv(file.path(combined_input, "ROV_diver_percent_cover_combined.csv"))

## explicit factor level order so every model below reads its "type"
## coefficient as ROV relative to a diver reference (R's default alphabetical
## ordering isn't reliable here and would make the coefficient name/sign
## inconsistent depending on locale)
dat_pc <- dat_pc %>% mutate(type = factor(type, levels = c("diver", "ROV")))
## END read data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## percent-cover models (logistic example) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat_pc <- dat_pc %>%
  mutate(cover_red_algae_ind = if_else(cover_red_algae > 0, 1, 0),
         combined_green_algae_ind = if_else(combined_green_algae > 0, 1, 0),
         cover_crustose_coralline_ind = if_else(cover_crustose_coralline > 0, 1, 0),
         combined_substrate_boulder_ind = if_else(combined_substrate_boulder > 0, 1, 0),
         substrate_rock_.15.25cm.wa._ind = if_else(substrate_rock_.15.25cm.wa. > 0, 1, 0),
         combined_substrate_pebble_ind = if_else(combined_substrate_pebble > 0, 1, 0),
         substrate_sand_ind = if_else(substrate_sand > 0, 1, 0),
         substrate_shell_hash_ind = if_else(substrate_shell_hash > 0 , 1, 0)
         )

mod_ssh <- glmer(substrate_shell_hash_ind ~ type + site + season + depth + (1|transect_id),
                 data = dat_pc,
               family = binomial)

summary(mod_ssh)
## END logistic example ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## percent-cover models (binomial examples) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Note that it may be better to compute the numerators directly from detections out of n
dat_pc <- dat_pc %>%
  mutate(cover_red_algae_num = ceiling(cover_red_algae * n),
         combined_green_algae_num = ceiling(combined_green_algae * n),
         cover_crustose_coralline_num = ceiling(cover_crustose_coralline * n),
         combined_substrate_boulder_num = ceiling(combined_substrate_boulder * n),
         substrate_rock_.15.25cm.wa._num = ceiling(substrate_rock_.15.25cm.wa. * n),
         combined_substrate_pebble_num = ceiling(combined_substrate_pebble * n),
         substrate_sand_num = ceiling(substrate_sand * n),
         substrate_shell_hash_num = ceiling(substrate_shell_hash * n)
         )

mod_cra <- glmmTMB(
  cbind(cover_red_algae_num, n - cover_red_algae_num) ~ type + site + season + depth + (1|transect_id),
  family = binomial,
  data = dat_pc
)

summary(mod_cra)

mod_cra_disp <- glmmTMB(
  cbind(cover_red_algae_num, n - cover_red_algae_num) ~ type + site + season + depth + (1|transect_id),
  family = betabinomial(link = "logit"),
  data = dat_pc
)
summary(mod_cra_disp) # preferred

mod_cga_disp <- glmmTMB(
  cbind(combined_green_algae_num, n - combined_green_algae_num) ~ type + site + season + depth + (1|transect_id),
  family = betabinomial(link = "logit"),
  data = dat_pc
)
summary(mod_cga_disp)
## END binomial examples ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## percent-cover models across all 8 categories ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## beta-binomial GLMM, the preferred formulation above, extracting the type
## (ROV vs. diver) coefficient from each into one tidy summary table. Each fit
## is wrapped so that a hard convergence failure on one category (expected
## for the sparsest categories, e.g. cover_crustose_coralline,
## substrate_shell_hash) is recorded rather than halting the loop, and any
## convergence warning (fit still succeeds, but flagged) is captured in the
## note column rather than silently dropped or silently discarding the model
pc_categories <- c("cover_red_algae", "combined_green_algae",
                   "cover_crustose_coralline", "combined_substrate_boulder",
                   "substrate_rock_.15.25cm.wa.", "combined_substrate_pebble",
                   "substrate_sand", "substrate_shell_hash")

fit_pc_betabinom <- function(category, data) {
  num_col <- paste0(category, "_num")
  data$num <- data[[num_col]]
  data$fail <- data$n - data$num
  glmmTMB(
    cbind(num, fail) ~ type + site + season + depth + (1|transect_id),
    family = betabinomial(link = "logit"),
    data = data
  )
}

fit_and_report <- function(category, data) {
  warn_msgs <- character(0)
  fit <- withCallingHandlers(
    tryCatch(fit_pc_betabinom(category, data), error = function(e) e),
    warning = function(w) {
      warn_msgs <<- c(warn_msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(fit = fit, warnings = warn_msgs)
}

pc_results <- list()
pc_models <- list()

for (cat in pc_categories) {
  res <- fit_and_report(cat, dat_pc)
  fit <- res$fit
  warn_note <- if (length(res$warnings) > 0) paste(res$warnings, collapse = "; ") else NA_character_

  if (inherits(fit, "glmmTMB")) {
    pc_models[[cat]] <- fit
    s <- summary(fit)$coefficients$cond
    pc_results[[cat]] <- tibble(
      category = cat,
      converged = TRUE,
      note = warn_note,
      estimate_logodds = s["typeROV", "Estimate"],
      se = s["typeROV", "Std. Error"],
      z_value = s["typeROV", "z value"],
      p_value = s["typeROV", "Pr(>|z|)"],
      odds_ratio_ROV_vs_diver = exp(estimate_logodds)
    )
  } else {
    pc_results[[cat]] <- tibble(
      category = cat,
      converged = FALSE,
      note = conditionMessage(fit),
      estimate_logodds = NA_real_, se = NA_real_, z_value = NA_real_,
      p_value = NA_real_, odds_ratio_ROV_vs_diver = NA_real_
    )
  }
}

pc_results <- bind_rows(pc_results)
print(pc_results, width = Inf)

write.csv(pc_results, file.path(combined_output, "percent_cover_model_results.csv"), row.names = FALSE)
## END all 8 categories ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## winter-only substrate + CCA models (additional check) ~~~~~~~~~~~~~~~~~~~~~~~
## does not replace the all-season models above. CCA and the 5 substrate
## categories compete for the same finite ROV point-classification space with
## overlying biological cover (see diagnose_substrate.R) -- in winter, when
## annual algae/kelp cover is largely absent, that competition should be
## minimal, so ROV/diver agreement for these 6 categories specifically should
## improve relative to the all-season result. There's no season term in this
## formula since there's no season variation left once the data are
## restricted to winter only
dat_pc_winter <- dat_pc %>% filter(season == "winter")

winter_categories <- c("cover_crustose_coralline", "combined_substrate_boulder",
                       "substrate_rock_.15.25cm.wa.", "combined_substrate_pebble",
                       "substrate_sand", "substrate_shell_hash")

fit_pc_betabinom_winter <- function(category, data) {
  num_col <- paste0(category, "_num")
  data$num <- data[[num_col]]
  data$fail <- data$n - data$num
  glmmTMB(
    cbind(num, fail) ~ type + site + depth + (1|transect_id),
    family = betabinomial(link = "logit"),
    data = data
  )
}

fit_and_report_winter <- function(category, data) {
  warn_msgs <- character(0)
  fit <- withCallingHandlers(
    tryCatch(fit_pc_betabinom_winter(category, data), error = function(e) e),
    warning = function(w) {
      warn_msgs <<- c(warn_msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(fit = fit, warnings = warn_msgs)
}

pc_results_winter <- list()
pc_models_winter <- list()

for (cat in winter_categories) {
  res <- fit_and_report_winter(cat, dat_pc_winter)
  fit <- res$fit
  warn_note <- if (length(res$warnings) > 0) paste(res$warnings, collapse = "; ") else NA_character_

  if (inherits(fit, "glmmTMB")) {
    pc_models_winter[[cat]] <- fit
    s <- summary(fit)$coefficients$cond
    pc_results_winter[[cat]] <- tibble(
      category = cat,
      converged = TRUE,
      note = warn_note,
      estimate_logodds = s["typeROV", "Estimate"],
      se = s["typeROV", "Std. Error"],
      z_value = s["typeROV", "z value"],
      p_value = s["typeROV", "Pr(>|z|)"],
      odds_ratio_ROV_vs_diver = exp(estimate_logodds)
    )
  } else {
    pc_results_winter[[cat]] <- tibble(
      category = cat,
      converged = FALSE,
      note = conditionMessage(fit),
      estimate_logodds = NA_real_, se = NA_real_, z_value = NA_real_,
      p_value = NA_real_, odds_ratio_ROV_vs_diver = NA_real_
    )
  }
}

pc_results_winter <- bind_rows(pc_results_winter)
print(pc_results_winter, width = Inf)

write.csv(pc_results_winter, file.path(combined_output, "percent_cover_model_results_winter_substrate.csv"), row.names = FALSE)
## END winter-only models ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## shell hash annotator sanity check ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Every ROV percent-cover point was manually reviewed/corrected by one of
## three annotators, assigned by transect number and applied consistently
## across site and season: transects 1 & 4 -> annotator A, 2 & 5 -> annotator
## B, 3 & 6 -> annotator C.
##
## Caveat up front: annotator is entirely a recoding of transect number (each
## transect number maps to exactly one annotator), so it is crossed cleanly
## with site, season, and depth (each annotator group spans one deep and one
## shallow transect, both sites, both seasons) but is necessarily confounded
## with any REAL ecological difference between those same transect-number
## pairs. This design can flag whether an annotator-linked pattern exists,
## but on its own cannot cleanly separate "annotator artifact" from
## "transects 1/4, 2/5, and 3/6 just have different true shell hash
## abundance." The diver-side comparison below (divers were never reviewed by
## these three annotators) helps triangulate: if the same transect-number
## grouping also predicts diver shell hash, that argues for a real
## ecological pattern rather than an annotation artifact specific to the ROV
## review process.
dat_pc <- dat_pc %>%
  mutate(annotator = case_when(
    transect %in% c(1, 4) ~ "A",
    transect %in% c(2, 5) ~ "B",
    transect %in% c(3, 6) ~ "C"
  ))

mod_ssh_annotator <- glmmTMB(
  cbind(substrate_shell_hash_num, n - substrate_shell_hash_num) ~ type + site + season + depth + annotator + (1|transect_id),
  family = betabinomial(link = "logit"),
  data = dat_pc
)
summary(mod_ssh_annotator)

ssh_annotator_coefs <- summary(mod_ssh_annotator)$coefficients$cond %>%
  as.data.frame() %>%
  rownames_to_column("term")
write.csv(ssh_annotator_coefs, file.path(combined_output, "shell_hash_annotator_model_coefficients.csv"), row.names = FALSE)

## does adding annotator meaningfully improve fit, or change the type (ROV
## vs. diver) effect, relative to the no-annotator model already fit above?
mod_ssh_no_annotator <- pc_models[["substrate_shell_hash"]]

ssh_aic_comparison <- AIC(mod_ssh_no_annotator, mod_ssh_annotator) %>%
  rownames_to_column("model") %>%
  mutate(model = c("no_annotator", "with_annotator"))
write.csv(ssh_aic_comparison, file.path(combined_output, "shell_hash_annotator_AIC_comparison.csv"), row.names = FALSE)
print(ssh_aic_comparison)

## descriptive triangulation: mean shell hash % by annotator's transect-
## number grouping, separately for each platform. A pattern present in BOTH
## columns points to real transect-linked ecology; a pattern present only
## in the ROV column points toward an annotation artifact.
ssh_by_annotator <- dat_pc %>%
  group_by(annotator, type) %>%
  summarise(mean_shell_hash_pct = mean(substrate_shell_hash) * 100, .groups = "drop") %>%
  pivot_wider(names_from = type, values_from = mean_shell_hash_pct)
write.csv(ssh_by_annotator, file.path(combined_output, "shell_hash_by_annotator_and_type.csv"), row.names = FALSE)
print(ssh_by_annotator)
## END annotator check ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
