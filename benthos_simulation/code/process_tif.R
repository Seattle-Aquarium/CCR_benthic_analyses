library(terra)
library(sf)

#setwd('./data')

r <- rast('Extract_Cent_1.tif')
print(r)
plot(r)
crs(r)
nlyr(r)
names(r)
df <- as.data.frame(r, xy=TRUE, na.rm=TRUE)


pts <- st_as_sf(df, coords = c("x", "y"), crs = "EPSG:2856")
pts_ll <- st_transform(pts, 4326)
