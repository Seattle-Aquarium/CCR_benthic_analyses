## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Species richness curves for CCR analysis of Urban Kelp data  
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())


## read in libraries 
library(tidyverse)
library(vegan)


## set working directory
setwd("../")
getwd()


## relative files paths 
data <- "data"
results <- "results"
figs <- "figs"
code <- "code"


## read in csv 
dat <- read.csv(file.path(data, "winter_2024_long.csv"))
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## graphing functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## plotting  
my.theme <- theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.6),
  plot.background  = element_rect(fill = "white", colour = NA),
  panel.background = element_rect(fill = "white", colour = NA),
  strip.background = element_rect(fill = "white", colour = "black", linewidth = 0.6),
  axis.title = element_text(size = 16),
  axis.text  = element_text(size = 14),
  plot.title = element_text(size = 16),
  strip.text.x = element_text(size = 16),
  strip.text.y = element_text(size = 18))


## paste "Transect atop column 
transect_labeller <- function(x) paste("Transect", x)


## graphing window 
graphics.off()
windows(10, 8, record = T)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## species accumualation across photos (in spatial order) within a transect
photo_labels <- dat %>%
  mutate(across(c(site, transect, photo, label), ~ as.character(.x))) %>%
  filter(!is.na(label), label != "") %>%
  mutate(
    transect = as.character(transect),
    photo_id = paste(site, transect, photo, sep = "||")
  ) %>%
  distinct(site, transect, photo, photo_id, label)


photo_order <- photo_labels %>%
  distinct(site, transect, photo, photo_id) %>%
  group_by(site, transect) %>%
  arrange(photo, .by_group = TRUE) %>%
  mutate(photo_index = row_number()) %>%
  ungroup()


photo_order %>%
  group_by(site, transect) %>%
  summarise(n_photos = n())


transect_accum <- photo_order %>%
  left_join(photo_labels, by = c("site", "transect", "photo", "photo_id")) %>%
  arrange(site, transect, photo_index) %>%
  group_by(site, transect) %>%
  group_modify(~ {
    df <- .x
    
    tibble(
      n_photos = sort(unique(df$photo_index)),
      richness = sapply(sort(unique(df$photo_index)), function(k) {
        df %>%
          filter(photo_index <= k) %>%
          summarise(n_distinct(label)) %>%
          pull()
      })
    )
  }) %>%
  ungroup()


## plot 
ggplot(transect_accum,
       aes(x = n_photos, y = richness)) +
  geom_line(linewidth = 1) +
  facet_grid(site ~ transect,
             labeller = labeller(transect = function(x) paste("Transect", x))) +
  labs(
    x = "Number of photos sampled along transect",
    y = "Cumulative richness (percent-cover categories)",
    title = "Species accumulation as photos accumulate within transects"
  ) +
  my.theme
## END species accumulation within transect in spatial order of photos ~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## species accumulation within transect, permutation based ~~~~~~~~~~~~~~~~~~~~~
random_photo_accum <- function(df, n_perm = 200, seed = 1) {
  set.seed(seed)
  
  photos <- unique(df$photo_id)
  n_photos <- length(photos)
  
  mat <- replicate(n_perm, {
    perm <- sample(photos)
    sapply(seq_len(n_photos), function(k) {
      df %>%
        filter(photo_id %in% perm[1:k]) %>%
        summarise(n_distinct(label)) %>%
        pull()
    })
  })
  
  tibble(
    n_photos = seq_len(n_photos),
    med = apply(mat, 1, median),
    lo  = apply(mat, 1, quantile, 0.25),
    hi  = apply(mat, 1, quantile, 0.75)
  )
}


transect_random_accum <- photo_labels %>%
  group_by(site, transect) %>%
  group_modify(~ random_photo_accum(.x, n_perm = 200)) %>%
  ungroup()


## plot 
ggplot(transect_random_accum,
       aes(x = n_photos, y = med)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.25) +
  geom_line(linewidth = 1) +
  facet_grid(site ~ transect,
             labeller = labeller(transect = function(x) paste("Transect", x))) +
  labs(
    x = "Number of photos sampled along transect",
    y = "Cumulative richness (percent-cover categories)",
    title = "Randomized species accumulation across photos (median ± IQR)"
  ) +
  my.theme
## END permutation base plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## add 90 - 95% reference lines ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
photo_frac_transect <- transect_random_accum %>%
  group_by(site, transect) %>%
  mutate(
    Rmax = max(med),
    frac_of_full = med / Rmax
  ) %>%
  ungroup()


ref_lines <- tibble(
  y = c(0.90, 0.95),
  type = c("90%", "95%")
)

ggplot(photo_frac_transect,
       aes(x = n_photos, y = frac_of_full)) +
  geom_line(linewidth = 1) +
  geom_hline(data = ref_lines,
             aes(yintercept = y, linetype = type),
             linewidth = 0.6) +
  facet_grid(site ~ transect,
             labeller = labeller(transect = function(x) paste("Transect", x))) +
  scale_linetype_manual(values = c("90%" = "dashed", "95%" = "dotted")) +
  scale_y_continuous(limits = c(0, 1.05)) +
  labs(
    x = "Number of photos sampled along transect",
    y = "Fraction of total transect richness",
    title = "Convergence of transect richness with increasing number of photos",
    linetype = "Threshold"
  ) +
  my.theme

photo_k_thresholds <- photo_frac_transect %>%
  group_by(site, transect) %>%
  summarise(
    k_90 = min(n_photos[frac_of_full >= 0.90]),
    k_95 = min(n_photos[frac_of_full >= 0.95]),
    .groups = "drop"
  )
## end reference lines ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## another visualization of 90-95% richness across photos ~~~~~~~~~~~~~~~~~~~~~~
dat_long2 <- dat %>%
  mutate(across(c(site, transect, photo, label), ~ stringr::str_trim(as.character(.x)))) %>%
  filter(!is.na(label), label != "") %>%
  mutate(
    transect = as.character(transect),
    photo_id = paste(site, transect, photo, sep = "||")
  ) %>%
  group_by(photo_id) %>%
  mutate(point_id = row_number()) %>%
  ungroup() %>%
  filter(point_id <= 50)  


photo_counts <- dat_long2 %>%
  distinct(site, transect, photo_id) %>%
  group_by(site, transect) %>%
  summarise(
    n_photos_actual = n(),
    .groups = "drop"
  )


photo_k_thresholds2 <- photo_k_thresholds %>%
  left_join(photo_counts, by = c("site", "transect"))


## create plot
ggplot(photo_k_thresholds2,
       aes(x = transect)) +
  geom_point(aes(y = k_90), size = 3) +
  geom_segment(aes(y = k_90, yend = k_95, xend = transect), linewidth = 1) +
  
  geom_point(aes(y = n_photos_actual), color = "red", size = 3) +
    facet_wrap(~ site, scales = "free_x") +
  
  scale_y_continuous(
    limits = c(0, 135),
    breaks = seq(0, 135, 10),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  labs(
    x = "Transect",
    y = "Number of photos",
    title = "Photos required to capture 90–95% of transect richness",
    subtitle = "Black dot = 90%, end of line = 95%, red point = actual number of photos"
  ) +
  
  my.theme
## end second 90-95% view of richness vs photos ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
