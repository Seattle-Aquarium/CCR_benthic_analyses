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
null_sk <- glmer(sugar ~ (1|site) + (1|site:transect),
                 weights = rep(100, times = nrow(dat)), # 100 "trials"
                 data = dat, family = binomial)
summary(null_sk)
fit_sk <- glmer(sugar ~ depth + location + (1|site) + (1|site:transect), 
                weights = rep(100, times = nrow(dat)), # 100 "trials"
                data = dat, family = binomial)
summary(fit_sk)
anova(null_sk, fit_sk)

# Make sk predictions for points in TIF; project
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
  mutate(pred = predict(fit_sk, newdata = ., re.form = NA, type = "response"))
pts <- st_transform(pts, 32610) # project
ggplot(pts, aes(col = pred)) +
  geom_sf() +
  labs(title = "Sugar kelp", col = "Prediction") +
  theme_bw()

# Project and create transects poly
transects <- read.csv('data/perpendicular_transects.csv') %>%
  select(Transect_key, Latitude, Longitude, Area_m2, Width) %>%
  mutate(length = Area_m2 / Width)
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
  
# Compute overlap; krig
pts_in_poly <- st_filter(pts, transects_poly)
pts_in_poly_sp <- as(pts_in_poly, "Spatial")
vgm_emp <- variogram(pred ~ 1, pts_in_poly_sp)
plot(vgm_emp)
vgm_fit <- fit.variogram(
  vgm_emp,
  model = vgm(psill = 0.02, model = "Sph", range = 100, nugget = 0.001)
)
plot(vgm_emp, vgm_fit)

pts_sp <- as(pts, "Spatial")
kriged <- krige(
  formula  = pred ~ 1,
  locations = pts_in_poly_sp,
  newdata   = pts_sp,
  model     = vgm_fit,
  nmax = 50,     # local kriging neighborhood
  maxdist = 500  # ignore distant points
)
pts$pred_krig <- kriged$var1.pred
pts$pred_var  <- kriged$var1.var

ggplot(pts, aes(col = pred)) +
  geom_sf()

ggplot(pts, aes(col = pred_krig)) +
  geom_sf()
  