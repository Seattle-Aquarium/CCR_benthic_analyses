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
dat <- read.csv(file.path(label_19, "percent-cover_abundances.csv"))


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

pred <- predict(fit_sk, dat$depth)
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





## visualize GLMM ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
predicted_prob <- predict(null_sk, type = "response")
predicted_odds <- predicted_prob / (1 - predicted_prob)

predicted_prob_ra <- predict(null_ra, type = "response")

pred_df3 <- data.frame(
  depth = dat$depth,
  odds = predicted_prob_ra
)


pred_df <- data.frame(
  depth = dat$depth,
  odds = predicted_odds
)


pred_df2 <- data.frame(
  depth = dat$depth,
  odds = predicted_prob
)


library(ggplot2)

ggplot(pred_df2, aes(x = depth, y = odds)) +
  geom_point(position = position_jitter(height = 0.0075), alpha = 0.5) +
  geom_smooth(method = "loess", color = "blue") +
  labs(x = "Depth",
       y = "probability of Sugar Kelp",
       title = "probability of sugar kelp vs. depth") +
  theme_minimal()


p1 <- ggplot(pred_df3, aes(x=depth, y=odds)) +
  my.theme +
  geom_point(position = position_jitter(height = 0.0075), alpha = 0.5) +
  geom_smooth(method = "loess", color = "blue") +
  labs(x = "depth (m)",
       y = "probability of sugar kelp",
       title = "probability of sugar kelp vs. depth")

print(p1)
  
my.theme = theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(), 
                 axis.line = element_line(colour = "black"),
                 axis.title=element_text(size=17),
                 axis.text=element_text(size=17),
                 plot.title = element_text(size=17))




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## end of script  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
