library(tidyverse)
library(lme4)
library(sf)
library(terra)
library(gstat)

## relative file paths
code <- "code"
figs <- "figs"
data <- "data"

# Fit GLMM to depth + sk data
dat <- read.csv(file.path(data, "percent-cover_abundances.csv"))
dat$transect <- as.factor(dat$transect)
dat$site <- as.factor(dat$site)
dat$location <- as.factor(dat$location)
dat$key <- as.factor(dat$key)
dat$sugar <- dat$sugar_kelp/100
dat$depth_c <- dat$depth - mean(dat$depth)
null_sk <- glmer(sugar ~ (1|site) + (1|site:transect),
                 weights = rep(100, times = nrow(dat)), # 100 "trials"
                 data = dat, family = binomial)
summary(null_sk)
fit_sk <- glmer(sugar ~ depth_c + location + (1|site) + (1|site:transect), 
                weights = rep(100, times = nrow(dat)), # 100 "trials"
                data = dat, family = binomial)
summary(fit_sk)
anova(null_sk, fit_sk)

fit_sk_sq <- glmer(sugar ~ depth_c + I(depth_c^2) + location + (1|site) + (1|site:transect), 
                weights = rep(100, times = nrow(dat)), # 100 "trials"
                data = dat, family = binomial)
summary(fit_sk_sq)

# Visualize for Centennial Park
cp <- dat %>% 
  filter(location == "Centennial_Park")
newdat <- data.frame(
  depth_c = seq(min(cp$depth_c), max(cp$depth_c), length.out = 200),
  location = rep(as.factor("Centennial_Park"), times = 200)
)
newdat$pred <- predict(fit_sk_sq, newdata = newdat, type = "response", re.form = NA) # assume r.e. 0

ggplot(cp, aes(depth_c, sugar)) +
  geom_point() +
  geom_line(data = newdat, aes(depth_c, pred), color = "green") +
  geom_smooth(method = "loess", color = "blue") # note this line does not correspond to any of the above models

# Make sk predictions for points in TIF
r <- rast('data/Extract_Cent_1.tif')
print(r)
plot(r)
crs(r) # combo crs
nlyr(r)
names(r)
pts <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
pts <- st_as_sf(pts, coords = c("x", "y"))
st_crs(pts) <- st_crs(r) 
pts <- pts %>% 
  rename(depth = Extract_Cent_1) %>%
  mutate(location = as.factor('Centennial_Park'))
pts <- pts %>%
  mutate(depth_c = depth - mean(depth)) %>% 
  mutate(pred = predict(fit_sk_sq, newdata = ., re.form = NA, type = "response"))
pts <- st_transform(pts, 32610) # project
ggplot(pts, aes(col = pred)) +
  geom_sf() +
  labs(title = "Sugar kelp", col = "Prediction") +
  theme_bw()

# Create transects poly
transects <- read.csv('data/perpendicular_transects.csv') %>%
  select(Transect_key, Latitude, Longitude, Area_m2, Width) %>%
  mutate(length = Area_m2/Width)
transects <- st_as_sf( 
  transects,
  coords = c("Longitude", "Latitude"),
  crs = 4326
)
transects <- st_transform(transects, 32610)
transects_lines <- transects %>%
  group_by(Transect_key) %>%
  summarise(geometry = st_combine(geometry), .groups = "drop") %>%
  st_cast("LINESTRING")
dist <- mean(transects$Width)
transects_poly <- st_buffer(
  transects_lines,
  dist = dist,
  endCapStyle = "FLAT"
)
ggplot(pts) +
  geom_sf(aes(color = pred)) +
  geom_sf(
    data = transects_poly,
    fill = "red",
    col = "red"
  ) +
  labs(
    title = "Sugar kelp with Transects",
    color = "Prediction"
  ) +
  theme_bw()
  
# Compute overlap
pts_in_poly <- st_filter(pts, transects_poly)
