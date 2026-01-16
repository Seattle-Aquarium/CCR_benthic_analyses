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





## photo-level species accumulation curves ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clean + standardize
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


## set function for within-photo curve 
within_photo_curve <- function(df_photo, permutations = 199, seed = 1) {
  
  df_photo <- df_photo %>%
    mutate(label = make.names(label)) %>%
    distinct(point_id, label) %>%
    arrange(point_id)
  
  kmax <- n_distinct(df_photo$point_id)
  nlab <- n_distinct(df_photo$label)
  
  # If a photo has no valid labels after filtering, return empty
  if (kmax == 0 || nlab == 0) {
    return(tibble(n_points = integer(0), richness = numeric(0)))
  }
  
  # If only one label appears in the whole photo, richness is always 1
  if (nlab == 1) {
    return(tibble(n_points = 1:kmax, richness = rep(1, kmax)))
  }
  
  # Normal case: >= 2 labels
  df_photo <- df_photo %>% mutate(label = factor(label))
  
  comm <- model.matrix(~ label - 1, data = df_photo)
  
  set.seed(seed)
  sac <- vegan::specaccum(comm, method = "random", permutations = permutations)
  
  tibble(
    n_points = sac$sites,
    richness = sac$richness
  )
}


photo_keys2 <- dat_long2 %>%
  distinct(site, transect, photo, photo_id) %>%
  arrange(site, transect, photo)


## function to run sppacum across the photos
sac_photo_df2 <- purrr::pmap_dfr(
  photo_keys2,
  function(site, transect, photo, photo_id) {
    
    df_p <- dat_long2 %>%
      filter(.data$photo_id == .env$photo_id)
    
    within_photo_curve(df_p, permutations = 199, seed = 1) %>%
      mutate(site = site, transect = transect, photo = photo, photo_id = photo_id)
  }
)


## double check the range - should be 50
range(sac_photo_df2$n_points)


## photo count
sac_photo_df2 %>%
  group_by(photo_id) %>%
  summarise(max_k = max(n_points), .groups = "drop") %>%
  count(max_k, sort = TRUE)


## establish summary and credible interval range
transect_summary2 <- sac_photo_df2 %>%
  group_by(site, transect, n_points) %>%
  summarise(
    med = median(richness, na.rm = TRUE),
    lo  = quantile(richness, 0.25, na.rm = TRUE),
    hi  = quantile(richness, 0.75, na.rm = TRUE),
    .groups = "drop"
  )


## set site and transect as factor
sac_photo_df2 <- sac_photo_df2 %>%
  mutate(
    site = factor(site),
    transect = factor(transect)
  )

transect_summary2 <- transect_summary2 %>%
  mutate(
    site = factor(site),
    transect = factor(transect)
  )


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
windows(12,12,record=T)


## create figure
ggplot() +
  geom_line(data = sac_photo_df2,
            aes(x = n_points, y = richness, group = photo_id), alpha = 0.10) +
  geom_ribbon(data = transect_summary2,
              aes(x = n_points, ymin = lo, ymax = hi, group = transect), alpha = 0.20) +
  geom_line(data = transect_summary2,
            aes(x = n_points, y = med, group = transect), linewidth = 1) +
  facet_grid(
    site ~ transect,
    labeller = labeller(transect = transect_labeller)) +
  scale_x_continuous(breaks = seq(0, 50, 10)) +
  coord_cartesian(xlim = c(0, 50)) +
  labs(
    x = "Number of points sampled within photo (max 50)",
    y = "Within-photo richness (percent-cover categories)",
    title = "Within-photo accumulation: photo replicates with transect summary (median ± IQR)"
  ) + my.theme


rich_frac <- transect_summary2 %>%
  group_by(site, transect) %>%
  mutate(
    R50 = med[n_points == 50],
    frac_of_50 = med / R50
  ) %>%
  ungroup()


rich_frac %>%
  filter(n_points %in% c(10, 20, 30, 40, 50)) %>%
  select(site, transect, n_points, med, frac_of_50)


txt <- capture.output({
  cat("Richness fraction output\n")
  cat("========================\n\n")
  rich_frac %>%
    filter(n_points %in% c(10, 20, 30, 40, 50)) %>%
    select(site, transect, n_points, med, frac_of_50) %>%
    print(n = Inf)
})
writeLines(txt, "results/rich_frac_subset.txt")


k_thresholds <- rich_frac %>%
  filter(n_points <= 50) %>%
  group_by(site, transect) %>%
  summarise(
    k_90 = min(n_points[frac_of_50 >= 0.90]),
    k_95 = min(n_points[frac_of_50 >= 0.95]),
    R50  = first(R50),
    .groups = "drop"
  )


# write formatted text output
sink("results/k_thresholds_summary.txt")
cat("K-threshold summary by site and transect\n")
cat("======================================\n\n")
print(k_thresholds)
sink()


k_summary <- k_thresholds %>%
  summarise(
    k90_median = median(k_90, na.rm = TRUE),
    k90_range  = paste0(min(k_90), "–", max(k_90)),
    k95_median = median(k_95, na.rm = TRUE),
    k95_range  = paste0(min(k_95), "–", max(k_95))
  )


# write formatted text output
sink("results/k_summary_within_photo.txt")
cat("K-threshold summary across sites and transects\n")
cat("======================================\n\n")
print(k_summary)
sink()
## END species-accumulation curves within photos ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



