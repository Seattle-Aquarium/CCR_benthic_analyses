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




## prep binomial numerators for each category ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## converts each category's transect-level proportion back into a point count
## (numerator) out of n classified points, for use in the beta-binomial models
## below
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
## END numerator prep ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




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




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
