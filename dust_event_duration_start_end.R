

library(raster)
library(rgdal)

setwd("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/HDF5_outputs/I_method_EUMETSAT/dust_duration_EUMETSAT")


dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/UAE_boundary"
### shapefile for UAE
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))


filenames <- list.files(pattern = "Max_duration_")

filenames <- filenames[10]

# event with the max duration (in HOURS) 
r_max_duration <- stack(filenames, bands = 1)
plot(r_max_duration)
plot(shp_UAE , add = TRUE)
# vec_all <- as.vector(r_max_duration)
# max_val<- (max(vec_all, na.rm = T))


# start-time of the max duration event
r_start_time_dust <- stack(filenames, bands = 2)
plot(r_start_time_dust)
# vec_all <- as.vector(r_start_time_dust)
# max_val<- (max(vec_all, na.rm = T))


# start-time of the max duration event
r_end_time_dust <- stack(filenames, bands = 3)
plot(r_end_time_dust)
# vec_all <- as.vector(r_end_time_dust)
# max_val<- (max(vec_all, na.rm = T))



