
library(readr)
library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
# install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)
library(stringr)


# setwd("F:/Historical_DUST")
source("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/DUST SEVIRI/R_scripts/extract_pnt_raster.R")

# load location of airport in the UAE

# sites_Airports_UAE <- read.csv("F:/Historical_DUST/Airport_Locations_UAE.csv")
sites_Airports_UAE <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/Airport_Locations_UAE.csv")
sites_Airports_UAE <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/Airport_Locations_UAE_new.csv")

rows <- nrow(sites_Airports_UAE)

dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/UAE_boundary"
### shapefile for UAE
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
# names(shp)
plot(shp_UAE)


#############################################################################################
# read of DAILY AOD EVENTS from STACKED Rasters #############################################
#############################################################################################
## make a function that reads each station at each time and extract points ##################
##############################################################################################

#################
# MODIS AQUA ####
#################

setwd("F:/Historical_DUST")
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust")

output_dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plot_MODIS"
filenames <- list.files(pattern = ".tif$")

raster_AQUA <- stack("all_DAYS_AQUA.tif")
n <- length(raster_AQUA@layers)
date_aqua <- read.csv("dates_AQUA_2004_2017.csv")  # n rows as the same number of rasters
date_aqua <- date_aqua %>%
  mutate(year = year(TS_AQUA))

# make YEARLY Average MAPS
 
reference <- raster("all_DAYS_AQUA.tif", band = 6)
plot(reference)

i <- 2010
j <- 39


for (i in 2004:2017) {
  YEAR_rasters <- date_aqua %>%
    filter(year == i)
  length_YEAR <- nrow(YEAR_rasters)
  
  # define start & end of the number of raster to subset for a specific year
  start <- as.numeric(YEAR_rasters$X[1])
  end <- as.numeric(YEAR_rasters$X[1] + length_YEAR -1)
  all_rasters <- stack()    # start an empty raster for each year
  # subset rasters
  raster_AQUA <- stack("all_DAYS_AQUA.tif")[[start:end]]
  # save stack 
  writeRaster(raster_AQUA, paste0(output_dir,"/",i, "_STACK_RASTERS_AOD_AQUA.tif") , options= "INTERLEAVE=BAND", overwrite=T)
  
  
   for (j in 1:length_YEAR) {
     
    AOD_raster_AQUA <- raster(paste0(output_dir,"/",i, "_STACK_RASTERS_AOD_AQUA.tif"), band = j)
    AOD_raster_AQUA = projectRaster(AOD_raster_AQUA, reference)
    
    all_rasters<- stack(all_rasters,AOD_raster_AQUA)
    mean_rasters <- mean(all_rasters, na.rm = TRUE)
    max_rasters <- max(all_rasters, na.rm = TRUE)
    plot(mean_rasters)
    plot(max_rasters)
    writeRaster(mean_rasters, paste0(output_dir,"/","MEAN_MAP_",i,"_YEARLY_AOD_AQUA.tif") , options= "INTERLEAVE=BAND", overwrite=T)
    writeRaster(max_rasters, paste0(output_dir,"/","MAX_MAP_",i,"_YEARLY_AOD_AQUA.tif") , options= "INTERLEAVE=BAND", overwrite=T)
    # clear memory
    gc()

  }
}



##################
# MODIS TERRA ####
##################

setwd("F:/Historical_DUST")
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust")

output_dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plot_MODIS"
filenames <- list.files(pattern = ".tif$")

raster_TERRA <- stack("all_DAYS_TERRA.tif")
n <- length(raster_TERRA@layers)
date_terra <- read.csv("dates_TERRA_2004_2017.csv")  # n rows as the same number of rasters
date_terra <- date_terra %>%
  mutate(year = year(TS_TERRA))

# make YEARLY Average MAPS

reference <- raster("all_DAYS_TERRA.tif", band = 6)
plot(reference)

i <- 2010
j <- 39


for (i in 2004:2017) {
  YEAR_rasters <- date_terra %>%
    filter(year == i)
  length_YEAR <- nrow(YEAR_rasters)
  
  # define start & end of the number of raster to subset for a specific year
  start <- as.numeric(YEAR_rasters$X[1])
  end <- as.numeric(YEAR_rasters$X[1] + length_YEAR -1)
  all_rasters <- stack()    # start an empty raster for each year
  # subset rasters
  raster_TERRA <- stack("all_DAYS_TERRA.tif")[[start:end]]
  # save stack 
  writeRaster(raster_TERRA, paste0(output_dir,"/",i, "_STACK_RASTERS_AOD_TERRA.tif") , options= "INTERLEAVE=BAND", overwrite=T)
  
  
  for (j in 1:length_YEAR) {
    
    AOD_raster_TERRA <- raster(paste0(output_dir,"/",i, "_STACK_RASTERS_AOD_TERRA.tif"), band = j)
    AOD_raster_TERRA = projectRaster(AOD_raster_TERRA, reference)
    
    all_rasters<- stack(all_rasters,AOD_raster_TERRA)
    mean_rasters <- mean(all_rasters, na.rm = TRUE)
    max_rasters <- max(all_rasters, na.rm = TRUE)
    plot(mean_rasters)
    plot(max_rasters)
    writeRaster(mean_rasters, paste0(output_dir,"/","MEAN_MAP_",i,"_YEARLY_AOD_TERRA.tif") , options= "INTERLEAVE=BAND", overwrite=T)
    writeRaster(max_rasters, paste0(output_dir,"/","MAX_MAP_",i,"_YEARLY_AOD_TERRA.tif") , options= "INTERLEAVE=BAND", overwrite=T)
    # clear memory
    gc()
    
  }
}

#######################################################################
#######################################################################
# make a raster stack with all the yearly maps

output_dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plot_MODIS"

all_rasters_mean <- stack()    
all_rasters_max <- stack() 

# load raster with reference extension (it is an empty raster, only zeros)
REFERENCE <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plot_MODIS/reference/20110629_II_Method_M_II_Method_sum.tif")
plot(REFERENCE)

i <- 2004

##########
## AQUA ##
##########

for (i in 2004:2017) {
r_mean <- raster(paste0(output_dir,"/","MEAN_MAP_",i,"_YEARLY_AOD_AQUA.tif"))
## make resolution of MODIS-data as the one of the reference-------------------------------
r_mean = projectRaster(r_mean, REFERENCE)
all_rasters_mean <- stack(all_rasters_mean,r_mean)
all_rasters_mean <- crop(all_rasters_mean, extent(shp_UAE))
all_rasters_mean <- mask(all_rasters_mean, shp_UAE)  
writeRaster(all_rasters_mean, paste0(output_dir,"/","STACK_YEARLY_AQUA_MEAN_MAPS.tif") , options= "INTERLEAVE=BAND", overwrite=T)
r_max <- raster(paste0(output_dir,"/","MAX_MAP_",i,"_YEARLY_AOD_AQUA.tif"))
r_max = projectRaster(r_max, REFERENCE)
all_rasters_max <- stack(all_rasters_max,r_max)
all_rasters_max <- crop(all_rasters_max, extent(shp_UAE))
all_rasters_max <- mask(all_rasters_max, shp_UAE) 
writeRaster(all_rasters_max, paste0(output_dir,"/","STACK_YEARLY_AQUA_MAX_MAPS.tif") , options= "INTERLEAVE=BAND", overwrite=T)
}

# plot(all_rasters_mean)
# plot(all_rasters_max)


###########
## TERRA ##
###########

output_dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plot_MODIS"

all_rasters_mean <- stack()    
all_rasters_max <- stack() 

# load raster with reference extension (it is an empty raster, only zeros)
REFERENCE <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plot_MODIS/reference/20110629_II_Method_M_II_Method_sum.tif")
plot(REFERENCE)



for (i in 2004:2017) {
  r_mean <- raster(paste0(output_dir,"/","MEAN_MAP_",i,"_YEARLY_AOD_TERRA.tif"))
  ## make resolution of MODIS-data as the one of the reference-------------------------------
  r_mean = projectRaster(r_mean, REFERENCE)
  all_rasters_mean <- stack(all_rasters_mean,r_mean)
  all_rasters_mean <- crop(all_rasters_mean, extent(shp_UAE))
  all_rasters_mean <- mask(all_rasters_mean, shp_UAE)  
  writeRaster(all_rasters_mean, paste0(output_dir,"/","STACK_YEARLY_TERRA_MEAN_MAPS.tif") , options= "INTERLEAVE=BAND", overwrite=T)
  r_max <- raster(paste0(output_dir,"/","MAX_MAP_",i,"_YEARLY_AOD_TERRA.tif"))
  r_max = projectRaster(r_max, REFERENCE)
  all_rasters_max <- stack(all_rasters_max,r_max)
  all_rasters_max <- crop(all_rasters_max, extent(shp_UAE))
  all_rasters_max <- mask(all_rasters_max, shp_UAE) 
  writeRaster(all_rasters_max, paste0(output_dir,"/","STACK_YEARLY_TERRA_MAX_MAPS.tif") , options= "INTERLEAVE=BAND", overwrite=T)
}



##################################################################################
# map to be exported MEAN maps (MODIS AQUA) ######################################
##################################################################################

library(leaflet)
library(webshot)
library(htmlwidgets)
library(RColorBrewer)
library(raster)
library(classInt)
library(stringr)
library(ggplot2)

library(viridis)
library(lattice)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plot_MODIS")


#### define an unique colorbar for all the maps in the stack
# load rasters in a stack
raster_stack <- stack("STACK_YEARLY_AQUA_MEAN_MAPS.tif")
# check numbers of years
TS <- seq(from=2004, by=1, to=2017)


vec_all <- as.vector(raster_stack)

max_val<- (max(vec_all, na.rm = T))
min_val<- (min(vec_all,  na.rm = T))
max_val <- 2.6
min_val<- 0

stat_dat <- summary(as.vector(raster_stack))
IQR <- (as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR

low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR <-if ( max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))

# cool = rainbow(20, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
# cool_2 = rainbow(50, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
# warm = rainbow(130, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
# cols = c(rev(cool), rev(cool_2), rev(warm))

cool = rainbow(60, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
cool_2 = rainbow(5, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
warm = rainbow(145, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
cols = c(rev(cool), rev(cool_2), rev(warm))


##########################################
### plots of MEAN maps (MODIS AQUA) ######
##########################################

AOD_images <- stack("STACK_YEARLY_AQUA_MEAN_MAPS.tif")

h <- rasterVis::levelplot(AOD_images, 
                          #    margin=FALSE, main= as.character(TITLE),
                          margin=FALSE, main= "Average AOD (AQUA)",
                          xlab = "",
                          ylab = "",
                          ## about colorbar
                          colorkey=list(
                            space='bottom',                   
                            labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title=expression(paste("        AOD"))
                          ),   
                          ## about the axis
                          par.settings=list(
                            strip.border=list(col='transparent'),
                            strip.background=list(col='transparent'),
                            axis.line=list(col='black')
                          ),
                          scales=list(draw=T, alternating= F),            
                          #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
                          col.regions = cols,
                          at=unique(c(seq(low_IQR, high_IQR, length.out=200))),
                          # names.attr=rep(names(DUST_images))) +
                          names.attr=as.character(seq(from = 2004, to = 2017, by= 1))) +
  latticeExtra::layer(sp.polygons(shp_UAE, col = "black", alpha = 1))
# h


output_dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plot_MODIS"
png(paste0(output_dir, "/", "MEAN_AOD_AQUA_MAPS_YEARLY.png"), width = 1100, height = 900,
    units = "px", pointsize = 100,
    bg = "white", res = 100)
print(h)
dev.off()



##################################################################################
# map to be exported MAX maps (MODIS AQUA) #######################################
##################################################################################

library(leaflet)
library(webshot)
library(htmlwidgets)
library(RColorBrewer)
library(raster)
library(classInt)
library(stringr)
library(ggplot2)

library(viridis)
library(lattice)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plot_MODIS")


#### define an unique colorbar for all the maps in the stack
# load rasters in a stack
raster_stack <- stack("STACK_YEARLY_AQUA_MAX_MAPS.tif")
# check numbers of years
TS <- seq(from=2004, by=1, to=2017)


vec_all <- as.vector(raster_stack)

max_val<- (max(vec_all, na.rm = T))
min_val<- (min(vec_all,  na.rm = T))
max_val <- 3.5
min_val<- 0

stat_dat <- summary(as.vector(raster_stack))
IQR <- (as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR

low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR <-if ( max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))

# cool = rainbow(20, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
# cool_2 = rainbow(50, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
# warm = rainbow(130, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
# cols = c(rev(cool), rev(cool_2), rev(warm))

cool = rainbow(90, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
cool_2 = rainbow(25, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
warm = rainbow(95, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
cols = c(rev(cool), rev(cool_2), rev(warm))


##########################################
### plots of MEAN maps (MODIS AQUA) ######
##########################################

AOD_images <- stack("STACK_YEARLY_AQUA_MAX_MAPS.tif")

h <- rasterVis::levelplot(AOD_images, 
                          #    margin=FALSE, main= as.character(TITLE),
                          margin=FALSE, main= "Maximum AOD (AQUA)",
                          xlab = "",
                          ylab = "",
                          ## about colorbar
                          colorkey=list(
                            space='bottom',                   
                            labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title=expression(paste("        AOD"))
                          ),   
                          ## about the axis
                          par.settings=list(
                            strip.border=list(col='transparent'),
                            strip.background=list(col='transparent'),
                            axis.line=list(col='black')
                          ),
                          scales=list(draw=T, alternating= F),            
                          #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
                          col.regions = cols,
                          at=unique(c(seq(low_IQR, high_IQR, length.out=200))),
                          # names.attr=rep(names(DUST_images))) +
                          names.attr=as.character(seq(from = 2004, to = 2017, by= 1))) +
  latticeExtra::layer(sp.polygons(shp_UAE, col = "black", alpha = 1))
# h


output_dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plot_MODIS"
png(paste0(output_dir, "/", "MAX_AOD_AQUA_MAPS_YEARLY.png"), width = 1100, height = 900,
    units = "px", pointsize = 100,
    bg = "white", res = 100)
print(h)
dev.off()


########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################

##################################################################################
# map to be exported MEAN maps (MODIS TERRA) #####################################
##################################################################################

library(leaflet)
library(webshot)
library(htmlwidgets)
library(RColorBrewer)
library(raster)
library(classInt)
library(stringr)
library(ggplot2)

library(viridis)
library(lattice)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plot_MODIS")


#### define an unique colorbar for all the maps in the stack
# load rasters in a stack
raster_stack <- stack("STACK_YEARLY_TERRA_MEAN_MAPS.tif")
# check numbers of years
TS <- seq(from=2004, by=1, to=2017)


vec_all <- as.vector(raster_stack)

max_val<- (max(vec_all, na.rm = T))
min_val<- (min(vec_all,  na.rm = T))
max_val <- 3.4
min_val<- 0

stat_dat <- summary(as.vector(raster_stack))
IQR <- (as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR

low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR <-if ( max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))

# cool = rainbow(20, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
# cool_2 = rainbow(50, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
# warm = rainbow(130, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
# cols = c(rev(cool), rev(cool_2), rev(warm))

cool = rainbow(60, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
cool_2 = rainbow(5, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
warm = rainbow(145, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
cols = c(rev(cool), rev(cool_2), rev(warm))


##########################################
### plots of MEAN maps (MODIS TERRA) #####
##########################################

AOD_images <- stack("STACK_YEARLY_TERRA_MEAN_MAPS.tif")

h <- rasterVis::levelplot(AOD_images, 
                          #    margin=FALSE, main= as.character(TITLE),
                          margin=FALSE, main= "Average AOD (TERRA)",
                          xlab = "",
                          ylab = "",
                          ## about colorbar
                          colorkey=list(
                            space='bottom',                   
                            labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title=expression(paste("        AOD"))
                          ),   
                          ## about the axis
                          par.settings=list(
                            strip.border=list(col='transparent'),
                            strip.background=list(col='transparent'),
                            axis.line=list(col='black')
                          ),
                          scales=list(draw=T, alternating= F),            
                          #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
                          col.regions = cols,
                          at=unique(c(seq(low_IQR, high_IQR, length.out=200))),
                          # names.attr=rep(names(DUST_images))) +
                          names.attr=as.character(seq(from = 2004, to = 2017, by= 1))) +
  latticeExtra::layer(sp.polygons(shp_UAE, col = "black", alpha = 1))
# h


output_dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plot_MODIS"
png(paste0(output_dir, "/", "MEAN_AOD_TERRA_MAPS_YEARLY.png"), width = 1100, height = 900,
    units = "px", pointsize = 100,
    bg = "white", res = 100)
print(h)
dev.off()



##################################################################################
# map to be exported MAX maps (MODIS TERRA) ######################################
##################################################################################

library(leaflet)
library(webshot)
library(htmlwidgets)
library(RColorBrewer)
library(raster)
library(classInt)
library(stringr)
library(ggplot2)

library(viridis)
library(lattice)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plot_MODIS")


#### define an unique colorbar for all the maps in the stack
# load rasters in a stack
raster_stack <- stack("STACK_YEARLY_TERRA_MAX_MAPS.tif")
# check numbers of years
TS <- seq(from=2004, by=1, to=2017)


vec_all <- as.vector(raster_stack)

max_val<- (max(vec_all, na.rm = T))
min_val<- (min(vec_all,  na.rm = T))
max_val <- 4.4
min_val<- 0

stat_dat <- summary(as.vector(raster_stack))
IQR <- (as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR

low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR <-if ( max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))

# cool = rainbow(20, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
# cool_2 = rainbow(50, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
# warm = rainbow(130, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
# cols = c(rev(cool), rev(cool_2), rev(warm))

cool = rainbow(90, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
cool_2 = rainbow(25, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
warm = rainbow(95, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
cols = c(rev(cool), rev(cool_2), rev(warm))


##########################################
### plots of MEAN maps (MODIS AQUA) ######
##########################################

AOD_images <- stack("STACK_YEARLY_TERRA_MAX_MAPS.tif")

h <- rasterVis::levelplot(AOD_images, 
                          #    margin=FALSE, main= as.character(TITLE),
                          margin=FALSE, main= "Maximum AOD (TERRA)",
                          xlab = "",
                          ylab = "",
                          ## about colorbar
                          colorkey=list(
                            space='bottom',                   
                            labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title=expression(paste("        AOD"))
                          ),   
                          ## about the axis
                          par.settings=list(
                            strip.border=list(col='transparent'),
                            strip.background=list(col='transparent'),
                            axis.line=list(col='black')
                          ),
                          scales=list(draw=T, alternating= F),            
                          #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
                          col.regions = cols,
                          at=unique(c(seq(low_IQR, high_IQR, length.out=200))),
                          # names.attr=rep(names(DUST_images))) +
                          names.attr=as.character(seq(from = 2004, to = 2017, by= 1))) +
  latticeExtra::layer(sp.polygons(shp_UAE, col = "black", alpha = 1))
# h


output_dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plot_MODIS"
png(paste0(output_dir, "/", "MAX_AOD_TERRA_MAPS_YEARLY.png"), width = 1100, height = 900,
    units = "px", pointsize = 100,
    bg = "white", res = 100)
print(h)
dev.off()










