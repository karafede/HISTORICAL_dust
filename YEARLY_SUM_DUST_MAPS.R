
library(readr)
library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
# install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)
library(stringr)


setwd("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/daily_sum_II_Method")

dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/UAE_boundary"
### shapefile for UAE
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
# names(shp)
plot(shp_UAE)


# load raster with reference extension
reference <- raster("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/daily_sum_II_Method/20110629_II_Method_M_II_Method_sum.tif")
plot(reference)
# check resolution (~ 2km)
res(reference)

#############################################################################################
# read SUM of DAILY DUST EVENTS from STACKED Rasters ########################################
#############################################################################################

#################################################
#### II Method Met France Original ##############
#################################################

output_dir <- "F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_II_Method_METFRANCE"
setwd("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/daily_sum_II_Method")
filenames <- list.files(pattern = ".tif$")

# LIST filenames containing a specifc YEAR

# LIST_YEARS <- list(2005, 2006, 2007, 2008, 2009)
LIST_YEARS <- seq(from = 2010, to = 2017, by= 1)  # update with the right YEAR range


# LIST_YEARS <- 2004
# i <- 2004
# j <- 7


# for (i in 2005) {    # just 1 year
for (i in LIST_YEARS) {
filenames_YEAR <- list.files(pattern = c(i, ".tif$"))
# force to list max 365 days
filenames_YEAR <- filenames_YEAR[1:365]
str(filenames_YEAR)
# remove NAs
filenames_YEAR <- na.omit(filenames_YEAR)
filenames_YEAR <- as.character(filenames_YEAR)
LIST_YEARS <- i
all_rasters <- stack()    # stack ALL HOURS together in an unique raster
     for (j in 1:length(filenames_YEAR)) {
       # daily raster
       r <- raster(filenames_YEAR[j])
       # # reproject each raster with the same extent and resolution of the reference raster above
       r = projectRaster(r, reference)
       
       # check if the raster is OK and not saturated
       if (maxValue(r[[1]])==53) {
         r <- 0
       } else {
         r <- raster(filenames_YEAR[j])
       }
       
       # 61 scenes per day every 15 minutes (hours of dust observations) for OLD SEVIRI data
       r <- r/2.542  # 61/24,  max value should be 24h (hours of dust observations)
       # plot(r)
       
       all_rasters<- stack(all_rasters,r)
       sum_rasters <- sum(all_rasters, na.rm = TRUE)
       plot(sum_rasters)
       writeRaster(sum_rasters, paste0(output_dir,"/", LIST_YEARS, "_YEARLY_24h_SUM_II_Method.tif") , options= "INTERLEAVE=BAND", overwrite=T)
       # clear memory
       gc()
     }
}


###################################################################################################
###################################################################################################

# make a raster stack with all the YEARLY 24h sum images

output_dir <- "F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_II_Method_METFRANCE"
setwd("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_II_Method_METFRANCE")
# list .tif files
filenames <- list.files(pattern = ".tif$")

# make an empty raster
all_rasters <- stack()    # stack ALL HOURS together in an unique raster

for (i in 1:length(filenames)) {
  r <- raster(filenames[i])
  all_rasters <- stack(all_rasters,r)
  # crop over UAE
  r <- crop(r, extent(shp_UAE))
  r <- mask(r, shp_UAE)  
}
 
writeRaster(all_rasters, paste0(output_dir,"/", "STACK_YEARLY_24h_SUM_II_Method.tif") , options= "INTERLEAVE=BAND", overwrite=T)

###########################################################
# map to be exported ######################################
###########################################################

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

setwd("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_II_Method_METFRANCE")


#### define an unique colorbar for all the maps in the stack
# load rasters in a stack
raster_stack <- stack("STACK_YEARLY_24h_SUM_II_Method.tif")
# check numbers of years
TS <- seq(from=2004, by=1, to=2010)


vec_all <- as.vector(raster_stack)

max_val<- (max(vec_all, na.rm = T))
min_val<- (min(vec_all,  na.rm = T))


stat_dat <- summary(as.vector(raster_stack))
IQR <- (as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR

low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR <-if ( max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))

cool = rainbow(50, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
cool_2 = rainbow(25, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
warm = rainbow(125, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
cols = c(rev(cool), rev(cool_2), rev(warm))


########################
### plots of maps ######
########################

i <- 2

DUST_images <- stack("STACK_YEARLY_24h_SUM_II_Method.tif")

for (i in 1:length(DUST_images@layers)) {
  TITLE <- TS[i]
  name_time <- TS[i]
  DUST_images <- raster("STACK_YEARLY_24h_SUM_II_Method.tif", band = i)
  # plot(AOD_images)
  
  h <- rasterVis::levelplot(DUST_images, 
                            margin=FALSE, main= as.character(TITLE),
                            xlab = "",
                            ylab = "",
                            ## about colorbar
                            colorkey=list(
                              space='bottom',                   
                              labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                           font=3),
                              axis.line=list(col='black'),
                              width=0.75,
                              title=expression(paste("        DUST (hours) "))
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
                            names.attr=rep(names(DUST_images))) +
    latticeExtra::layer(sp.polygons(shp_UAE))
  h
  
  png(paste0(output_dir, "/", TS[i], "hours_DUST.png"), width = 900, height = 900,
      units = "px", pointsize = 50,
      bg = "white", res = 200)
  print(h)
  dev.off()
  
}




##########################################################################################################
##########################################################################################################

#################################################
#### I Method EUMETSAT ##########################
#################################################

output_dir <- "F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_I_Method_EUMETSAT"
setwd("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/daily_sum_I_Method")
filenames <- list.files(pattern = ".tif$")

# LIST filenames containing a specifc YEAR

# LIST_YEARS <- list(2005, 2006, 2007, 2008, 2009)
LIST_YEARS <- seq(from = 2004, to = 2017, by= 1)


for (i in LIST_YEARS) {
  filenames_YEAR <- list.files(pattern = c(i, ".tif$"))
  # force to list max 365 days
  filenames_YEAR <- filenames_YEAR[1:365]
  str(filenames_YEAR)
  # remove NAs
  filenames_YEAR <- na.omit(filenames_YEAR)
  filenames_YEAR <- as.character(filenames_YEAR)
  LIST_YEARS <- i
  all_rasters <- stack()    # stack ALL HOURS together in an unique raster
  for (j in 1:length(filenames_YEAR)) {
    # daily raster
    r <- raster(filenames_YEAR[j])
    # # reproject each raster with the same extent and resolution of the reference raster above
    r = projectRaster(r, reference)
  
      # check if the raster is OK and not saturated
    if (maxValue(r[[1]])==53) {
      r <- 0
    } else {
      r <- raster(filenames_YEAR[j])
    }
    
    # 96 scenes per day every 15 minutes (hours of dust observations) for OLD SEVIRI data
    r <- r/4  # 96/24,  max value should be 24h (hours of dust observations)
    # plot(r)
    all_rasters<- stack(all_rasters,r)
    sum_rasters <- sum(all_rasters, na.rm = TRUE)
    plot(sum_rasters)
    writeRaster(sum_rasters, paste0(output_dir,"/", LIST_YEARS, "_YEARLY_24h_SUM_I_Method.tif") , options= "INTERLEAVE=BAND", overwrite=T)
    # clear memory
    gc()
  }
}










