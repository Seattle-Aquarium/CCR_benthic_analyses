## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Generalized linear mixed effects models ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())


## add libraries
library(tidyverse)
library(lme4)
library(boot)


## set working directory to home folder
setwd("../")
getwd()


## relative file paths
code <- "code"
figs <- "figs"
label_19 <- "data_output/19_labels"
label_69 <- "data_output/69_labels"


## invoke relative file path 
dat <- read.csv(file.path(label_19, "ratios.csv"))


## repeat factor for other dataset
dat$transect <- as.factor(dat$transect)
dat$site <- as.factor(dat$site)
dat$location <- as.factor(dat$location)
dat$key <- as.factor(dat$key)
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## transformations for GLMM model ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat$hard <- dat$hard_substrate_CCA/100
dat$sugar <- dat$sugar_kelp/100
dat$textured <- dat$textured_kelp/100
dat$pebb <- dat$pebble/100
## END transformations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## glmm w/ sugar kelp ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
null_sk <- glmer(sugar ~ (1|site) + (1|transect:site),
                 weights = rep(100, times = nrow(dat)),
                 data = dat, family = binomial)


fit_sk <- glmer(sugar ~ depth + location + (1|site) + (1|transect:site),
                weights = rep(100, times = nrow(dat)), 
                data = dat, family = binomial)

summary(fit_sk)
anova(null_sk, fit_sk, test = "Chisq") 
pred <- predict(fit_sk, dat)
plot(pred)
## END glmm w/ sugar kelp ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## glmm w/ textured kelp ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
null_tk <- glmer(textured ~ (1|site) + (1|transect:site),
                 weights = rep(100, times = nrow(dat)),
                 data = dat, family = binomial)


fit_tk <- glmer(textured ~ depth + location + (1|site) + (1|transect:site),
                weights = rep(100, times = nrow(dat)), 
                data = dat, family = binomial)

summary(fit_tk)
anova(null_tk, fit_tk, test = "Chisq") 
pred <- predict(fit_sk, dat)
plot(pred)
## END glmm w/ textured kelp ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## glmm w/ red algae ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
null_ra <- glmer(red_algae/100 ~ (1|site) + (1|transect:site),
                 weights = rep(100, times = nrow(dat)),
                 data = dat, family = binomial)


fit_ra <- glmer(red_algae/100 ~ depth + location + (1|site) + (1|transect:site),
                weights = rep(100, times = nrow(dat)), 
                data = dat, family = binomial)

summary(fit_ra)
anova(null_ra, fit_ra, test = "Chisq") 
pred <- predict(fit_ra, dat)
plot(pred)
## END glmm w/ red algae ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## exponentiate any log-odds coefficient output to generate odds ~~~~~~~~~~~~~~~
exp(-0.08)
## END exponentiation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Generalized linear mixed effects models ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
