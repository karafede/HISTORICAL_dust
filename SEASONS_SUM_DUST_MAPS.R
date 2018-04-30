
library(readr)
library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
# install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)
library(stringr)


# setwd("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/daily_sum_II_Method")

dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/UAE_boundary"
### shapefile for UAE
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
# names(shp)
plot(shp_UAE)


# load raster with reference extension (it is an empty raster, only zeros)

# old data reference until 2011
reference <- raster("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/HDF5_outputs/II_Method_MetFr/II_method_MetFrance/daily_sum_II_MetFrance/20110629_II_Method_II_Method_sum.tif")

# new data reference after 2011
# reference <- raster("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/HDF5_outputs/II_Method_MetFr/II_method_MetFrance/daily_sum_II_MetFrance/20130302_II_Method_II_Method_sum.tif")

plot(reference)
# check resolution (~ 2km)
res(reference)

#############################################################################################
# read SUM of DAILY DUST EVENTS from STACKED Rasters ########################################
#############################################################################################

#################################################
#### II Method Met France Original ##############
#################################################

output_dir <- "F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_II_Method_METFRANCE/seasons_MAPS"

## load data from "2004-03-18" to "2011-06-30"
# setwd("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/daily_sum_II_Method_old")

## load data from "2011-07-01" to "2017"
setwd("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/HDF5_outputs/II_Method_MetFr/II_method_MetFrance/daily_sum_II_MetFrance")
filenames <- list.files(pattern = ".tif$")

# LIST filenames containing a specifc YEAR

# LIST_YEARS <- list(2005, 2006, 2007, 2008, 2009)
LIST_YEARS <- seq(from = 2011, to = 2017, by= 1)  # update with the right YEAR range (2004 - 2017)
# LIST_YEARS <- seq(from = 2004, to = 2011, by= 1)  # update with the right YEAR range (2004 - 2017)


# LIST_YEARS <- 2010
# i <- 2010
# j <- 190



# for (i in 2017) {    # just 1 year
for (i in LIST_YEARS) {
  filenames_YEAR <- list.files(pattern = c(i, ".tif$"))
  # force to list max 365 days
  filenames_YEAR <- filenames_YEAR[1:365]
  str(filenames_YEAR)
  # remove NAs
  filenames_YEAR <- na.omit(filenames_YEAR)
  filenames_YEAR <- as.character(filenames_YEAR)

# extract date from filenames 
year <- str_sub(filenames_YEAR, start = 1, end = -33)
month <- str_sub(filenames_YEAR, start = 5, end = -31)
day <- str_sub(filenames_YEAR, start = 7, end = -29)
Date <- paste0(year,"-", month, "-", day)
Date <- as.Date(Date)


LIST_YEARS <- i

all_rasters_winter <- stack()    # stack ALL HOURS together in an unique raster
all_rasters_spring <- stack()
all_rasters_summer <- stack()
all_rasters_fall <- stack()


     for (j in 1:length(filenames_YEAR)) {
       
       # daily raster
       r <- raster(filenames_YEAR[j])
       # # reproject each raster with the same extent and resolution of the reference raster above
       # r = projectRaster(r, reference)
      
       
       # check if the raster is OK and not saturated
       if (maxValue(r[[1]])==53) {
         r <- reference
       } else {
         r <- raster(filenames_YEAR[j])
        # r = projectRaster(r, reference)
       }
        
       
       # replace vlaues <- 0 into 0 or NA
       values(r)[values(r) < 0] = NA
       
       ###### from  "2004-03-18" to "2011-06-30" #################################################
       # 61 scenes per day every 15 minutes (hours of dust observations) for OLD SEVIRI data
       # r <- r/2.542  # 61/24,  max value should be 24h (hours of dust observations)
       # plot(r)
       
       ###### from  "2011-07-01" 20110701_II_Method_M_II_Method_sum  (181 file) ###################
       # 96 scenes per day every 15 minutes (hours of dust observations) for NEW SEVIRI data
       r <- r/4  # 96/24,  max value should be 24h (hours of dust observations)
       # plot(r)
       
       ##########################################################
       # get the month and classify for the season #############
       month <- str_sub(filenames_YEAR[j], start = 5, end = -31)
       month <- as.numeric(month)
       year <- str_sub(filenames_YEAR[j], start = 1, end = -33)
       year <- as.numeric(year)
       
       if (month %in% c(1:2)) {
         # r <- raster(filenames_YEAR[j])
         r <- projectRaster(r, reference)
         r_winter <- r 
         season <- "WINTER"
       } else if (month==12) {
         # r <- raster(filenames_YEAR[j])
         r <- projectRaster(r, reference)
         r_winter <- r 
         season <- "WINTER"
       } else if (month %in% c(3:5)) {
         # r <- raster(filenames_YEAR[j])
         r <- projectRaster(r, reference)
         r_spring <- r
         season <- "SPRING"
       } else if (month %in% c(6:8)) {
         # r <- raster(filenames_YEAR[j])
         r <- projectRaster(r, reference)
         r_summer <- r
         season <- "SUMMER"
       } else if (month %in% c(9:11)) {
         # r <- raster(filenames_YEAR[j])
         r <- projectRaster(r, reference)
         r_fall <- r
         season <- "FALL"
       }
       
       ########################################
       
       if(exists("r_winter")){
         r <- r_winter
         all_rasters_winter <- stack(all_rasters_winter,r)
         sum_rasters_winter <- sum(all_rasters_winter, na.rm = TRUE)
         writeRaster(sum_rasters_winter, paste0(output_dir,"/", season, "_", year, "_24h_SUM_II_Method.tif") , options= "INTERLEAVE=BAND", overwrite=T)
         remove(r_winter)
         # clear memory
         gc()
       } else if (exists("r_spring")) {
         r <- r_spring
         all_rasters_spring <- stack(all_rasters_spring,r)
         sum_rasters_spring <- sum(all_rasters_spring, na.rm = TRUE)
         writeRaster(sum_rasters_spring, paste0(output_dir,"/", season, "_", year, "_24h_SUM_II_Method.tif"), options= "INTERLEAVE=BAND", overwrite=T)
         remove(r_spring)
         # clear memory
         gc()
       } else if (exists("r_summer")) {
         r <- r_summer
         all_rasters_summer <- stack(all_rasters_summer,r)
         sum_rasters_summer <- sum(all_rasters_summer, na.rm = TRUE)
         writeRaster(sum_rasters_summer, paste0(output_dir,"/", season, "_", year, "_24h_SUM_II_Method.tif"), options= "INTERLEAVE=BAND", overwrite=T)
         remove(r_summer)
         # clear memory
         gc()
       } else if (exists("r_fall"))  {
         r <- r_fall
         all_rasters_fall <- stack(all_rasters_fall,r)
         sum_rasters_fall <- sum(all_rasters_fall, na.rm = TRUE)
         writeRaster(sum_rasters_fall, paste0(output_dir,"/", season, "_", year, "_24h_SUM_II_Method.tif"), options= "INTERLEAVE=BAND", overwrite=T)
         remove(r_fall)
         # clear memory
         gc()
       }
       
     }
}


###################################################################################################
###################################################################################################

# sum rasters for the YEAR 2011, "old" and "new"
r_2011_old_winter <- raster("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_II_Method_METFRANCE/seasons_MAPS/WINTER_2011_24h_SUM_II_Method_old.tif")
r_2011_old_summer <- raster("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_II_Method_METFRANCE/seasons_MAPS/SUMMER_2011_24h_SUM_II_Method_old.tif")
r_2011_old_winter <- projectRaster(r_2011_old_winter, reference)
r_2011_old_summer <- projectRaster(r_2011_old_summer, reference)

r_2011_new_winter <- raster("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_II_Method_METFRANCE/seasons_MAPS/WINTER_2011_24h_SUM_II_Method_new.tif")
r_2011_new_summer <- raster("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_II_Method_METFRANCE/seasons_MAPS/SUMMER_2011_24h_SUM_II_Method_new.tif")
r_2011_new_winter <- projectRaster(r_2011_new_winter, reference)
r_2011_new_summer <- projectRaster(r_2011_new_summer, reference)


r_2011_winter <- sum(r_2011_old_winter, r_2011_new_winter)
plot(r_2011_winter)
writeRaster(r_2011_winter, "F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_II_Method_METFRANCE/seasons_MAPS/WINTER_2011_24h_SUM_II_Method.tif" , options= "INTERLEAVE=BAND", overwrite=T)

r_2011_summer <- sum(r_2011_old_summer, r_2011_new_summer)
plot(r_2011_summer)
writeRaster(r_2011_summer, "F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_II_Method_METFRANCE/seasons_MAPS/SUMMER_2011_24h_SUM_II_Method.tif" , options= "INTERLEAVE=BAND", overwrite=T)

####################################################################
# make a raster stack with all the SEASONAL 24h sum images #########
####################################################################

output_dir <- "F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_II_Method_METFRANCE/seasons_MAPS"
setwd("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_II_Method_METFRANCE/seasons_MAPS")
# list .tif files
filenames <- list.files(pattern = ".tif$")

# make an empty raster
all_rasters_WINTER <- stack()    # stack ALL HOURS together in an unique raster
all_rasters_SPRING <- stack() 
all_rasters_SUMMER <- stack() 
all_rasters_FALL <- stack() 

filenames_WINTER <- list.files(pattern = c("WINTER"))
filenames_SPRING <- list.files(pattern = c("SPRING"))
filenames_SUMMER <- list.files(pattern = c("SUMMER"))
filenames_FALL <- list.files(pattern = c("FALL"))


# WINTER ##
for (i in 1:length(filenames_WINTER)) {
  r <- raster(filenames_WINTER[i])
  r = projectRaster(r, reference)
  values(r)[values(r) < 0] = NA
  all_rasters_WINTER <- stack(all_rasters_WINTER,r)
  # crop over UAE
  all_rasters_WINTER <- crop(all_rasters_WINTER, extent(shp_UAE))
  all_rasters_WINTER <- mask(all_rasters_WINTER, shp_UAE)  
  all_rasters_WINTER_AVG <- mean(all_rasters_WINTER, na.rm = TRUE)
}
 
writeRaster(all_rasters_WINTER, paste0(output_dir,"/", "STACK_WINTER_24h_SUM_II_Method.tif") , options= "INTERLEAVE=BAND", overwrite=T)
writeRaster(all_rasters_WINTER_AVG, paste0(output_dir,"/", "AVG_WINTER_24h_SUM_II_Method.tif") , options= "INTERLEAVE=BAND", overwrite=T)


# SPRING ##
for (i in 1:length(filenames_SPRING)) {
  r <- raster(filenames_SPRING[i])
  r = projectRaster(r, reference)
  values(r)[values(r) < 0] = NA
  all_rasters_SPRING <- stack(all_rasters_SPRING,r)
  # crop over UAE
  all_rasters_SPRING <- crop(all_rasters_SPRING, extent(shp_UAE))
  all_rasters_SPRING <- mask(all_rasters_SPRING, shp_UAE)  
  all_rasters_SPRING_AVG <- mean(all_rasters_SPRING, na.rm = TRUE)
}

writeRaster(all_rasters_SPRING, paste0(output_dir,"/", "STACK_SPRING_24h_SUM_II_Method.tif") , options= "INTERLEAVE=BAND", overwrite=T)
writeRaster(all_rasters_SPRING_AVG, paste0(output_dir,"/", "AVG_SPRING_24h_SUM_II_Method.tif") , options= "INTERLEAVE=BAND", overwrite=T)


# SUMMER ##
for (i in 1:length(filenames_SUMMER)) {
  r <- raster(filenames_SUMMER[i])
  r = projectRaster(r, reference)
  values(r)[values(r) < 0] = NA
  all_rasters_SUMMER <- stack(all_rasters_SUMMER,r)
  # crop over UAE
  all_rasters_SUMMER <- crop(all_rasters_SUMMER, extent(shp_UAE))
  all_rasters_SUMMER <- mask(all_rasters_SUMMER, shp_UAE)  
  all_rasters_SUMMER_AVG <- mean(all_rasters_SUMMER, na.rm = TRUE)
}

writeRaster(all_rasters_SUMMER, paste0(output_dir,"/", "STACK_SUMMER_24h_SUM_II_Method.tif") , options= "INTERLEAVE=BAND", overwrite=T)
writeRaster(all_rasters_SUMMER_AVG, paste0(output_dir,"/", "AVG_SUMMER_24h_SUM_II_Method.tif") , options= "INTERLEAVE=BAND", overwrite=T)



# FALL ##
for (i in 1:length(filenames_FALL)) {
  r <- raster(filenames_FALL[i])
  r = projectRaster(r, reference)
  values(r)[values(r) < 0] = NA
  all_rasters_FALL <- stack(all_rasters_FALL,r)
  # crop over UAE
  all_rasters_FALL <- crop(all_rasters_FALL, extent(shp_UAE))
  all_rasters_FALL <- mask(all_rasters_FALL, shp_UAE)  
  all_rasters_FALL_AVG <- mean(all_rasters_FALL, na.rm = TRUE)
}

writeRaster(all_rasters_FALL, paste0(output_dir,"/", "STACK_FALL_24h_SUM_II_Method.tif") , options= "INTERLEAVE=BAND", overwrite=T)
writeRaster(all_rasters_FALL_AVG, paste0(output_dir,"/", "AVG_FALL_24h_SUM_II_Method.tif") , options= "INTERLEAVE=BAND", overwrite=T)

# Stack seasonal maps

STACK_AVG_II_Method <- stack(all_rasters_WINTER_AVG,
                             all_rasters_SPRING_AVG,
                             all_rasters_SUMMER_AVG,
                             all_rasters_FALL_AVG)

writeRaster(STACK_AVG_II_Method, paste0(output_dir,"/", "AVG_ALL_SEASONS_24h_SUM_II_Method.tif") , options= "INTERLEAVE=BAND", overwrite=T)
plot(STACK_AVG_II_Method)


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

setwd("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_II_Method_METFRANCE/seasons_MAPS")


#### define an unique colorbar for all the maps in the stack
# load rasters in a stack
raster_stack_WINTER <- stack("STACK_WINTER_24h_SUM_II_Method.tif")
raster_stack_SPRING <- stack("STACK_SPRING_24h_SUM_II_Method.tif")
raster_stack_SUMMER <- stack("STACK_SUMMER_24h_SUM_II_Method.tif")
raster_stack_FALL <- stack("STACK_FALL_24h_SUM_II_Method.tif")

# average numbers of hours in each season == 2106
# calculate the percentage of hours of dust events in each season 
# raster_stack_WINTER <- stack("STACK_WINTER_24h_SUM_II_Method.tif")*100/2160
# raster_stack_SPRING <- stack("STACK_SPRING_24h_SUM_II_Method.tif")*100/2160
# raster_stack_SUMMER <- stack("STACK_SUMMER_24h_SUM_II_Method.tif")*100/2160
# raster_stack_FALL <- stack("STACK_FALL_24h_SUM_II_Method.tif")*100/2160

# check numbers of years
TS <- seq(from=2004, by=1, to=2017)

###################
#### WINTER #######
###################


vec_all <- as.vector(raster_stack_WINTER)
max_val<- (max(vec_all, na.rm = T))
max_val<- 750
min_val<- 0
# min_val<- (min(vec_all,  na.rm = T))
stat_dat <- summary(as.vector(raster_stack_WINTER))
IQR <- (as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR
low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR <-if ( max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))
cool = rainbow(15, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
cool_2 = rainbow(55, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
warm = rainbow(130, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
cols = c(rev(cool), rev(cool_2), rev(warm))


########################
### plots of maps ######
########################


DUST_images_WINTER <- stack("STACK_WINTER_24h_SUM_II_Method.tif")

  h <- rasterVis::levelplot(DUST_images_WINTER, 
                        #    margin=FALSE, main= as.character(TITLE),
                            margin=FALSE, main= "WINTER Dust SEVIRI (MetoFrance)",
                            xlab = "",
                            ylab = "",
                            ## about colorbar
                            colorkey=list(
                              space='bottom',                   
                              labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                           font=3),
                              axis.line=list(col='black'),
                              width=1.5,
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
                            # names.attr=rep(names(DUST_images))) +
                            names.attr=as.character(seq(from = 2004, to = 2017, by= 1))) +
    latticeExtra::layer(sp.polygons(shp_UAE, col = "black", alpha = 1))
  # h
  
  
  output_dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots"
  png(paste0(output_dir, "/", "WINTER_MAPS_hours_DUST_MetFrance.png"), width = 1100, height = 900,
      units = "px", pointsize = 100,
      bg = "white", res = 100)
  print(h)
  dev.off()


  ###################
  #### SPRING #######
  ###################
  
  vec_all <- as.vector(raster_stack_SPRING)
  max_val<- (max(vec_all, na.rm = T))
  max_val<- 750
  min_val<- 0
  # min_val<- (min(vec_all,  na.rm = T))
  stat_dat <- summary(as.vector(raster_stack_SPRING))
  IQR <- (as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR
  low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
  high_IQR <-if ( max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))
  cool = rainbow(15, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
  cool_2 = rainbow(55, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
  warm = rainbow(130, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
  cols = c(rev(cool), rev(cool_2), rev(warm))
  
  
  ########################
  ### plots of maps ######
  ########################
  
  DUST_images_SPRING <- stack("STACK_SPRING_24h_SUM_II_Method.tif")
  
  h <- rasterVis::levelplot(DUST_images_SPRING, 
                            #    margin=FALSE, main= as.character(TITLE),
                            margin=FALSE, main= "SPRING Dust SEVIRI (MetoFrance)",
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
                            # names.attr=rep(names(DUST_images))) +
                            names.attr=as.character(seq(from = 2004, to = 2017, by= 1))) +
    latticeExtra::layer(sp.polygons(shp_UAE, col = "black", alpha = 1))
  # h
  
  
  output_dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots"
  png(paste0(output_dir, "/", "SPRING_MAPS_hours_DUST_MetFrance.png"), width = 1100, height = 900,
      units = "px", pointsize = 100,
      bg = "white", res = 100)
  print(h)
  dev.off()
  

  ###################
  #### SUMMER #######
  ###################
  
  vec_all <- as.vector(raster_stack_SUMMER)
  max_val<- (max(vec_all, na.rm = T))
  max_val<- 750
  min_val<- 0
  # min_val<- (min(vec_all,  na.rm = T))
  stat_dat <- summary(as.vector(raster_stack_SUMMER))
  IQR <- (as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR
  low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
  high_IQR <-if ( max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))
  cool = rainbow(15, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
  cool_2 = rainbow(55, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
  warm = rainbow(130, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
  cols = c(rev(cool), rev(cool_2), rev(warm))
  
  
  ########################
  ### plots of maps ######
  ########################
  
  DUST_images_SUMMER <- stack("STACK_SUMMER_24h_SUM_II_Method.tif")
  
  h <- rasterVis::levelplot(DUST_images_SUMMER, 
                            #    margin=FALSE, main= as.character(TITLE),
                            margin=FALSE, main= "SUMMER Dust SEVIRI (MetoFrance)",
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
                            # names.attr=rep(names(DUST_images))) +
                            names.attr=as.character(seq(from = 2004, to = 2017, by= 1))) +
    latticeExtra::layer(sp.polygons(shp_UAE, col = "black", alpha = 1))
  # h
  
  
  output_dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots"
  png(paste0(output_dir, "/", "SUMMER_MAPS_hours_DUST_MetFrance.png"), width = 1100, height = 900,
      units = "px", pointsize = 100,
      bg = "white", res = 100)
  print(h)
  dev.off()
  
  
  ###################
  #### FALL #########
  ###################
  
  vec_all <- as.vector(raster_stack_FALL)
  max_val<- (max(vec_all, na.rm = T))
  max_val<- 750
  min_val<- 0
  # min_val<- (min(vec_all,  na.rm = T))
  stat_dat <- summary(as.vector(raster_stack_FALL))
  IQR <- (as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR
  low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
  high_IQR <-if ( max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))
  cool = rainbow(15, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
  cool_2 = rainbow(55, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
  warm = rainbow(130, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
  cols = c(rev(cool), rev(cool_2), rev(warm))
  
  
  ########################
  ### plots of maps ######
  ########################
  
  DUST_images_FALL <- stack("STACK_FALL_24h_SUM_II_Method.tif")
  
  h <- rasterVis::levelplot(DUST_images_FALL, 
                            #    margin=FALSE, main= as.character(TITLE),
                            margin=FALSE, main= "FALL Dust SEVIRI (MetoFrance)",
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
                            # names.attr=rep(names(DUST_images))) +
                            names.attr=as.character(seq(from = 2004, to = 2017, by= 1))) +
    latticeExtra::layer(sp.polygons(shp_UAE, col = "black", alpha = 1))
  # h
  
  
  output_dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots"
  png(paste0(output_dir, "/", "FALL_MAPS_hours_DUST_MetFrance.png"), width = 1100, height = 900,
      units = "px", pointsize = 100,
      bg = "white", res = 100)
  print(h)
  dev.off()
  
  
##########################################################################################################
##########################################################################################################


  
#################################################
#### I Method EUMETSAT ##########################
#################################################

output_dir <- "F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_I_Method_EUMETSAT/seasons_MAPS"

# load data from "2004-03-18" to "2011-06-30"
# setwd("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/daily_sum_I_Method_old")

# load data from "2011-07-01" to "2017"
  setwd("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/HDF5_outputs/I_method_EUMETSAT/daily_sum_I_EUMETSAT")
filenames <- list.files(pattern = ".tif$")

# LIST filenames containing a specifc YEAR

# LIST_YEARS <- list(2005, 2006, 2007, 2008, 2009)
# LIST_YEARS <- seq(from = 2011, to = 2017, by= 1)  # update with the right YEAR range (2004 - 2017)
LIST_YEARS <- seq(from = 2004, to = 2011, by= 1)  # update with the right YEAR range (2004 - 2017)


# LIST_YEARS <- 2010
# i <- 2012
# j <- 190


# for (i in 2010) {    # just 1 year
for (i in LIST_YEARS) {
  filenames_YEAR <- list.files(pattern = c(i, ".tif$"))
  # force to list max 365 days
  filenames_YEAR <- filenames_YEAR[1:365]
  str(filenames_YEAR)
  # remove NAs
  filenames_YEAR <- na.omit(filenames_YEAR)
  filenames_YEAR <- as.character(filenames_YEAR)
  
  # extract date from filenames 
  year <- str_sub(filenames_YEAR, start = 1, end = -22)
  month <- str_sub(filenames_YEAR, start = 5, end = -20)
  day <- str_sub(filenames_YEAR, start = 7, end = -18)
  Date <- paste0(year,"-", month, "-", day)
  Date <- as.Date(Date)
  
  
  LIST_YEARS <- i
  
  all_rasters_winter <- stack()    # stack ALL HOURS together in an unique raster
  all_rasters_spring <- stack()
  all_rasters_summer <- stack()
  all_rasters_fall <- stack()
  
  
  for (j in 1:length(filenames_YEAR)) {
    
    # daily raster
    r <- raster(filenames_YEAR[j])
    # # reproject each raster with the same extent and resolution of the reference raster above
    # r = projectRaster(r, reference)
    
    
    # check if the raster is OK and not saturated
    if (maxValue(r[[1]])==53) {
      r <- reference
    } else {
      r <- raster(filenames_YEAR[j])
    #  r = projectRaster(r, reference)
    }
    
    
    # replace vlaues <- 0 into 0 or NA
    values(r)[values(r) < 0] = NA
    
    ###### from  "2004-03-18" to "2011-06-30" #################################################
    # 61 scenes per day every 15 minutes (hours of dust observations) for OLD SEVIRI data
    r <- r/2.542  # 61/24,  max value should be 24h (hours of dust observations)
    # plot(r)
    
    ###### from  "2011-07-01" 20110701_II_Method_M_II_Method_sum  (181 file) ###################
    # 96 scenes per day every 15 minutes (hours of dust observations) for NEW SEVIRI data
    # r <- r/4  # 96/24,  max value should be 24h (hours of dust observations)
    # plot(r)
    
    ##########################################################
    # get the month and classify for the season #############
    month <- str_sub(filenames_YEAR[j], start = 5, end = -20)
    month <- as.numeric(month)
    year <- str_sub(filenames_YEAR[j], start = 1, end = -22)
    year <- as.numeric(year)
    
    if (month %in% c(1:2)) {
      # r <- raster(filenames_YEAR[j])
      r <- projectRaster(r, reference)
      r_winter <- r 
      season <- "WINTER"
    } else if (month==12) {
      # r <- raster(filenames_YEAR[j])
      r <- projectRaster(r, reference)
      r_winter <- r 
      season <- "WINTER"
    } else if (month %in% c(3:5)) {
      # r <- raster(filenames_YEAR[j])
      r <- projectRaster(r, reference)
      r_spring <- r
      season <- "SPRING"
    } else if (month %in% c(6:8)) {
      # r <- raster(filenames_YEAR[j])
      r <- projectRaster(r, reference)
      r_summer <- r
      season <- "SUMMER"
    } else if (month %in% c(9:11)) {
      # r <- raster(filenames_YEAR[j])
      r <- projectRaster(r, reference)
      r_fall <- r
      season <- "FALL"
    }
    
    ########################################
    
    if(exists("r_winter")){
      r <- r_winter
      all_rasters_winter <- stack(all_rasters_winter,r)
      sum_rasters_winter <- sum(all_rasters_winter, na.rm = TRUE)
      writeRaster(sum_rasters_winter, paste0(output_dir,"/", season, "_", year, "_24h_SUM_I_Method.tif"), options= "INTERLEAVE=BAND", overwrite=T)
      # clear memory
      remove(r_winter)
      gc()
    } else if (exists("r_spring")) {
      r <- r_spring
      all_rasters_spring <- stack(all_rasters_spring,r)
      sum_rasters_spring <- sum(all_rasters_spring, na.rm = TRUE)
      writeRaster(sum_rasters_spring, paste0(output_dir,"/", season, "_", year, "_24h_SUM_I_Method.tif"), options= "INTERLEAVE=BAND", overwrite=T)
      # clear memory
      remove(r_spring)
      gc()
    } else if (exists("r_summer")) {
      r <- r_summer
      all_rasters_summer <- stack(all_rasters_summer,r)
      sum_rasters_summer <- sum(all_rasters_summer, na.rm = TRUE)
      writeRaster(sum_rasters_summer, paste0(output_dir,"/", season, "_", year, "_24h_SUM_I_Method.tif"), options= "INTERLEAVE=BAND", overwrite=T)
      # clear memory
      remove(r_summer)
      gc()
    } else if (exists("r_fall"))  {
      r <- r_fall
      all_rasters_fall <- stack(all_rasters_fall,r)
      sum_rasters_fall <- sum(all_rasters_fall, na.rm = TRUE)
      writeRaster(sum_rasters_fall, paste0(output_dir,"/", season, "_", year, "_24h_SUM_I_Method.tif"), options= "INTERLEAVE=BAND", overwrite=T)
      # clear memory
      remove(r_fall)
      gc()
    }
    
  }
}



###################################################################################################
###################################################################################################

# sum rasters for the YEAR 2011, "old" and "new"
r_2011_old_summer <- raster("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_I_Method_EUMETSAT/seasons_MAPS/SUMMER_2011_24h_SUM_I_Method_old.tif")
r_2011_new_summer <- raster("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_I_Method_EUMETSAT/seasons_MAPS/SUMMER_2011_24h_SUM_I_Method_new.tif")
r_2011_old_summer <- projectRaster(r_2011_old_summer, reference)
r_2011_new_summer <- projectRaster(r_2011_new_summer, reference)


r_2011_old_winter <- raster("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_I_Method_EUMETSAT/seasons_MAPS/WINTER_2011_24h_SUM_I_Method_old.tif")
r_2011_new_winter <- raster("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_I_Method_EUMETSAT/seasons_MAPS/WINTER_2011_24h_SUM_I_Method_new.tif")
r_2011_old_winter <- projectRaster(r_2011_old_winter, reference)
r_2011_new_winter <- projectRaster(r_2011_new_winter, reference)


r_2011_summer <- sum(r_2011_old_summer, r_2011_new_summer)
plot(r_2011_summer)
writeRaster(r_2011_summer, "F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_I_Method_EUMETSAT/seasons_MAPS/SUMMER_2011_24h_SUM_I_Method.tif" , options= "INTERLEAVE=BAND", overwrite=T)


r_2011_winter <- sum(r_2011_old_winter, r_2011_new_winter)
plot(r_2011_winter)
writeRaster(r_2011_summer, "F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_I_Method_EUMETSAT/seasons_MAPS/WINTER_2011_24h_SUM_I_Method.tif" , options= "INTERLEAVE=BAND", overwrite=T)



#############################################################
# make a raster stack with all the YEARLY 24h sum images ####
#############################################################

output_dir <- "F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_I_Method_EUMETSAT/seasons_MAPS"
setwd("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_I_Method_EUMETSAT/seasons_MAPS")
# list .tif files
filenames <- list.files(pattern = ".tif$")

# make an empty raster
all_rasters_WINTER <- stack()    # stack ALL HOURS together in an unique raster
all_rasters_SPRING <- stack() 
all_rasters_SUMMER <- stack() 
all_rasters_FALL <- stack() 

filenames_WINTER <- list.files(pattern = c("WINTER"))
filenames_SPRING <- list.files(pattern = c("SPRING"))
filenames_SUMMER <- list.files(pattern = c("SUMMER"))
filenames_FALL <- list.files(pattern = c("FALL"))


# WINTER ##
for (i in 1:length(filenames_WINTER)) {
  r <- raster(filenames_WINTER[i])
  r = projectRaster(r, reference)
  values(r)[values(r) < 0] = NA
  all_rasters_WINTER <- stack(all_rasters_WINTER,r)
  # crop over UAE
  all_rasters_WINTER <- crop(all_rasters_WINTER, extent(shp_UAE))
  all_rasters_WINTER <- mask(all_rasters_WINTER, shp_UAE)  
  all_rasters_WINTER_AVG <- mean(all_rasters_WINTER, na.rm = TRUE)
}

writeRaster(all_rasters_WINTER, paste0(output_dir,"/", "STACK_WINTER_24h_SUM_I_Method.tif") , options= "INTERLEAVE=BAND", overwrite=T)
writeRaster(all_rasters_WINTER_AVG, paste0(output_dir,"/", "AVG_WINTER_24h_SUM_I_Method.tif") , options= "INTERLEAVE=BAND", overwrite=T)


# SPRING ##
for (i in 1:length(filenames_SPRING)) {
  r <- raster(filenames_SPRING[i])
  r = projectRaster(r, reference)
  values(r)[values(r) < 0] = NA
  all_rasters_SPRING <- stack(all_rasters_SPRING,r)
  # crop over UAE
  all_rasters_SPRING <- crop(all_rasters_SPRING, extent(shp_UAE))
  all_rasters_SPRING <- mask(all_rasters_SPRING, shp_UAE)  
  all_rasters_SPRING_AVG <- mean(all_rasters_SPRING, na.rm = TRUE)
}

writeRaster(all_rasters_SPRING, paste0(output_dir,"/", "STACK_SPRING_24h_SUM_I_Method.tif") , options= "INTERLEAVE=BAND", overwrite=T)
writeRaster(all_rasters_SPRING_AVG, paste0(output_dir,"/", "AVG_SPRING_24h_SUM_I_Method.tif") , options= "INTERLEAVE=BAND", overwrite=T)


# SUMMER ##
for (i in 1:length(filenames_SUMMER)) {
  r <- raster(filenames_SUMMER[i])
  r = projectRaster(r, reference)
  values(r)[values(r) < 0] = NA
  all_rasters_SUMMER <- stack(all_rasters_SUMMER,r)
  # crop over UAE
  all_rasters_SUMMER <- crop(all_rasters_SUMMER, extent(shp_UAE))
  all_rasters_SUMMER <- mask(all_rasters_SUMMER, shp_UAE)  
  all_rasters_SUMMER_AVG <- mean(all_rasters_SUMMER, na.rm = TRUE)
}

writeRaster(all_rasters_SUMMER, paste0(output_dir,"/", "STACK_SUMMER_24h_SUM_I_Method.tif") , options= "INTERLEAVE=BAND", overwrite=T)
writeRaster(all_rasters_SUMMER_AVG, paste0(output_dir,"/", "AVG_SUMMER_24h_SUM_I_Method.tif") , options= "INTERLEAVE=BAND", overwrite=T)



# FALL ##
for (i in 1:length(filenames_FALL)) {
  r <- raster(filenames_FALL[i])
  r = projectRaster(r, reference)
  values(r)[values(r) < 0] = NA
  all_rasters_FALL <- stack(all_rasters_FALL,r)
  # crop over UAE
  all_rasters_FALL <- crop(all_rasters_FALL, extent(shp_UAE))
  all_rasters_FALL <- mask(all_rasters_FALL, shp_UAE)  
  all_rasters_FALL_AVG <- mean(all_rasters_FALL, na.rm = TRUE)
}

writeRaster(all_rasters_FALL, paste0(output_dir,"/", "STACK_FALL_24h_SUM_I_Method.tif") , options= "INTERLEAVE=BAND", overwrite=T)
writeRaster(all_rasters_FALL_AVG, paste0(output_dir,"/", "AVG_FALL_24h_SUM_I_Method.tif") , options= "INTERLEAVE=BAND", overwrite=T)



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

setwd("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_I_Method_EUMETSAT/seasons_MAPS")


#### define an unique colorbar for all the maps in the stack
# load rasters in a stack
raster_stack_WINTER <- stack("STACK_WINTER_24h_SUM_I_Method.tif")
raster_stack_SPRING <- stack("STACK_SPRING_24h_SUM_I_Method.tif")
raster_stack_SUMMER <- stack("STACK_SUMMER_24h_SUM_I_Method.tif")
raster_stack_FALL <- stack("STACK_FALL_24h_SUM_I_Method.tif")
# check numbers of years
TS <- seq(from=2004, by=1, to=2017)

###################
#### WINTER #######
###################


vec_all <- as.vector(raster_stack_WINTER)
max_val<- (max(vec_all, na.rm = T))
max_val<- 750
min_val<- 0
# min_val<- (min(vec_all,  na.rm = T))
stat_dat <- summary(as.vector(raster_stack_WINTER))
IQR <- (as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR
low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR <-if ( max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))
cool = rainbow(15, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
cool_2 = rainbow(55, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
warm = rainbow(130, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
cols = c(rev(cool), rev(cool_2), rev(warm))


########################
### plots of maps ######
########################


DUST_images_WINTER <- stack("STACK_WINTER_24h_SUM_I_Method.tif")

h <- rasterVis::levelplot(DUST_images_WINTER, 
                          #    margin=FALSE, main= as.character(TITLE),
                          margin=FALSE, main= "WINTER Dust SEVIRI (EUMETSAT)",
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
                          # names.attr=rep(names(DUST_images))) +
                          names.attr=as.character(seq(from = 2004, to = 2017, by= 1))) +
  latticeExtra::layer(sp.polygons(shp_UAE, col = "black", alpha = 1))
# h


output_dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots"
png(paste0(output_dir, "/", "WINTER_MAPS_hours_DUST_EUMETSAT.png"), width = 1100, height = 900,
    units = "px", pointsize = 100,
    bg = "white", res = 100)
print(h)
dev.off()


###################
#### SPRING #######
###################

vec_all <- as.vector(raster_stack_SPRING)
max_val<- (max(vec_all, na.rm = T))
max_val<- 750
min_val<- 0
# min_val<- (min(vec_all,  na.rm = T))
stat_dat <- summary(as.vector(raster_stack_SPRING))
IQR <- (as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR
low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR <-if ( max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))
cool = rainbow(15, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
cool_2 = rainbow(55, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
warm = rainbow(130, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
cols = c(rev(cool), rev(cool_2), rev(warm))


########################
### plots of maps ######
########################

DUST_images_SPRING <- stack("STACK_SPRING_24h_SUM_I_Method.tif")

h <- rasterVis::levelplot(DUST_images_SPRING, 
                          #    margin=FALSE, main= as.character(TITLE),
                          margin=FALSE, main= "SPRING Dust SEVIRI (EUMETSAT)",
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
                          # names.attr=rep(names(DUST_images))) +
                          names.attr=as.character(seq(from = 2004, to = 2017, by= 1))) +
  latticeExtra::layer(sp.polygons(shp_UAE, col = "black", alpha = 1))
# h


output_dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots"
png(paste0(output_dir, "/", "SPRING_MAPS_hours_DUST_EUMETSAT.png"), width = 1100, height = 900,
    units = "px", pointsize = 100,
    bg = "white", res = 100)
print(h)
dev.off()


###################
#### SUMMER #######
###################

vec_all <- as.vector(raster_stack_SUMMER)
max_val<- (max(vec_all, na.rm = T))
max_val<- 750
min_val<- 0
# min_val<- (min(vec_all,  na.rm = T))
stat_dat <- summary(as.vector(raster_stack_SUMMER))
IQR <- (as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR
low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR <-if ( max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))
cool = rainbow(15, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
cool_2 = rainbow(55, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
warm = rainbow(130, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
cols = c(rev(cool), rev(cool_2), rev(warm))


########################
### plots of maps ######
########################

DUST_images_SUMMER <- stack("STACK_SUMMER_24h_SUM_I_Method.tif")

h <- rasterVis::levelplot(DUST_images_SUMMER, 
                          #    margin=FALSE, main= as.character(TITLE),
                          margin=FALSE, main= "SUMMER Dust SEVIRI (EUMETSAT)",
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
                          # names.attr=rep(names(DUST_images))) +
                          names.attr=as.character(seq(from = 2004, to = 2017, by= 1))) +
  latticeExtra::layer(sp.polygons(shp_UAE, col = "black", alpha = 1))
# h


output_dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots"
png(paste0(output_dir, "/", "SUMMER_MAPS_hours_DUST_EUMETSAT.png"), width = 1100, height = 900,
    units = "px", pointsize = 100,
    bg = "white", res = 100)
print(h)
dev.off()


###################
#### FALL #########
###################

vec_all <- as.vector(raster_stack_FALL)
max_val<- (max(vec_all, na.rm = T))
max_val<- 750
min_val<- 0
# min_val<- (min(vec_all,  na.rm = T))
stat_dat <- summary(as.vector(raster_stack_FALL))
IQR <- (as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR
low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR <-if ( max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))
cool = rainbow(15, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
cool_2 = rainbow(55, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
warm = rainbow(130, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
cols = c(rev(cool), rev(cool_2), rev(warm))


########################
### plots of maps ######
########################

DUST_images_FALL <- stack("STACK_FALL_24h_SUM_I_Method.tif")

h <- rasterVis::levelplot(DUST_images_FALL, 
                          #    margin=FALSE, main= as.character(TITLE),
                          margin=FALSE, main= "FALL Dust SEVIRI (EUMETSAT)",
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
                          # names.attr=rep(names(DUST_images))) +
                          names.attr=as.character(seq(from = 2004, to = 2017, by= 1))) +
  latticeExtra::layer(sp.polygons(shp_UAE, col = "black", alpha = 1))
# h


output_dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots"
png(paste0(output_dir, "/", "FALL_MAPS_hours_DUST_EUMETSAT.png"), width = 1100, height = 900,
    units = "px", pointsize = 100,
    bg = "white", res = 100)
print(h)
dev.off()





#################################################
# average SEASONAL maps (I and II method) #######
#################################################

#################
#### WINTER #####
#################

# load maps (they must have the same number of layers)
MAPS_I_method_EUMETSAT <- stack("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_I_Method_EUMETSAT/seasons_MAPS/STACK_WINTER_24h_SUM_I_Method.tif")
# plot(MAPS_I_method_EUMETSAT)
MAPS_II_method_MetFrance <- stack("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_II_Method_METFRANCE/seasons_MAPS/STACK_WINTER_24h_SUM_II_Method.tif")
# plot(MAPS_II_method_MetFrance)



AVG_DUST_image_WINTER <- stack() 
# i <-  9

for (i in 1:length(MAPS_I_method_EUMETSAT@layers)) {
    DUST_images_EUMETSAT <- raster(MAPS_I_method_EUMETSAT, i)
    DUST_images_MetFrance <- raster(MAPS_II_method_MetFrance, i)
    
    r <- mean(DUST_images_EUMETSAT, DUST_images_MetFrance)
    # r <- max(DUST_images_EUMETSAT, DUST_images_MetFrance)
    # r <- min(DUST_images_EUMETSAT, DUST_images_MetFrance)
    
    AVG_DUST_image_WINTER <- stack(AVG_DUST_image_WINTER,r)
    
}

# plot(AVG_DUST_image)
writeRaster(AVG_DUST_image_WINTER, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots/DUST_WINTER_AVG_MAPS.tif", options= "INTERLEAVE=BAND", overwrite=T)



###########################################################
# map to be exported ######################################
###########################################################

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots")

#### define an unique colorbar for all the maps in the stack
# load rasters in a stack
raster_stack <- stack("DUST_WINTER_AVG_MAPS.tif")
# check numbers of years
TS <- seq(from=2004, by=1, to=2017)


vec_all <- as.vector(raster_stack)
max_val<- (max(vec_all, na.rm = T))
max_val <- 750
min_val<- 0
# min_val<- (min(vec_all,  na.rm = T))


stat_dat <- summary(as.vector(raster_stack))
IQR <- (as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR

low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR <-if ( max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))

cool = rainbow(15, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
cool_2 = rainbow(50, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
warm = rainbow(135, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
cols = c(rev(cool), rev(cool_2), rev(warm))


########################
### plots of maps ######
########################
DUST_images_WINTER <- stack("DUST_WINTER_AVG_MAPS.tif")

h <- rasterVis::levelplot(DUST_images_WINTER, 
                          #    margin=FALSE, main= as.character(TITLE),
                          margin=FALSE, main= "Dust SEVIRI - WINTER (EUMETSAT & MeteoFrance)",
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
                          # names.attr=rep(names(DUST_images))) +
                          names.attr=as.character(seq(from = 2004, to = 2017, by= 1))) +
  latticeExtra::layer(sp.polygons(shp_UAE, col = "black", alpha = 1))
# h

output_dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots"
png(paste0(output_dir, "/", "WINTER_MAPS_mean_hours_DUST_EUMETSAT_&_METFRANCE.png"), width = 1100, height = 900,
    units = "px", pointsize = 100,
    bg = "white", res = 100)
print(h)
dev.off()



#################
#### SUMMER #####
#################

# load maps (they must have the same number of layers)
MAPS_I_method_EUMETSAT <- stack("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_I_Method_EUMETSAT/seasons_MAPS/STACK_SUMMER_24h_SUM_I_Method.tif")
# plot(MAPS_I_method_EUMETSAT)
MAPS_II_method_MetFrance <- stack("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_II_Method_METFRANCE/seasons_MAPS/STACK_SUMMER_24h_SUM_II_Method.tif")
# plot(MAPS_II_method_MetFrance)

AVG_DUST_image_SUMMER <- stack() 

for (i in 1:length(MAPS_I_method_EUMETSAT@layers)) {
  DUST_images_EUMETSAT <- raster(MAPS_I_method_EUMETSAT, i)
  DUST_images_MetFrance <- raster(MAPS_II_method_MetFrance, i)
  
  r <- mean(DUST_images_EUMETSAT, DUST_images_MetFrance)
  # r <- max(DUST_images_EUMETSAT, DUST_images_MetFrance)
  # r <- min(DUST_images_EUMETSAT, DUST_images_MetFrance)
  
  AVG_DUST_image_SUMMER <- stack(AVG_DUST_image_SUMMER,r)
  
}

# plot(AVG_DUST_image)
writeRaster(AVG_DUST_image_SUMMER, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots/DUST_SUMMER_AVG_MAPS.tif", options= "INTERLEAVE=BAND", overwrite=T)



###########################################################
# map to be exported ######################################
###########################################################

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots")

#### define an unique colorbar for all the maps in the stack
# load rasters in a stack
raster_stack <- stack("DUST_SUMMER_AVG_MAPS.tif")
# check numbers of years
TS <- seq(from=2004, by=1, to=2017)


vec_all <- as.vector(raster_stack)
max_val<- (max(vec_all, na.rm = T))
max_val <- 750
min_val<- 0
# min_val<- (min(vec_all,  na.rm = T))


stat_dat <- summary(as.vector(raster_stack))
IQR <- (as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR

low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR <-if ( max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))

cool = rainbow(15, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
cool_2 = rainbow(50, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
warm = rainbow(135, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
cols = c(rev(cool), rev(cool_2), rev(warm))


########################
### plots of maps ######
########################
AVG_DUST_image_SUMMER <- stack("DUST_SUMMER_AVG_MAPS.tif")

h <- rasterVis::levelplot(AVG_DUST_image_SUMMER, 
                          #    margin=FALSE, main= as.character(TITLE),
                          margin=FALSE, main= "Dust SEVIRI - SUMMER (EUMETSAT & MeteoFrance)",
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
                          # names.attr=rep(names(DUST_images))) +
                          names.attr=as.character(seq(from = 2004, to = 2017, by= 1))) +
  latticeExtra::layer(sp.polygons(shp_UAE, col = "black", alpha = 1))
# h

output_dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots"
png(paste0(output_dir, "/", "SUMMER_MAPS_mean_hours_DUST_EUMETSAT_&_METFRANCE.png"), width = 1100, height = 900,
    units = "px", pointsize = 100,
    bg = "white", res = 100)
print(h)
dev.off()


#################
#### SPRING #####
#################

# load maps (they must have the same number of layers)
MAPS_I_method_EUMETSAT <- stack("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_I_Method_EUMETSAT/seasons_MAPS/STACK_SPRING_24h_SUM_I_Method.tif")
# plot(MAPS_I_method_EUMETSAT)
MAPS_II_method_MetFrance <- stack("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_II_Method_METFRANCE/seasons_MAPS/STACK_SPRING_24h_SUM_II_Method.tif")
# plot(MAPS_II_method_MetFrance)

AVG_DUST_image_SPRING <- stack() 

for (i in 1:length(MAPS_I_method_EUMETSAT@layers)) {
  DUST_images_EUMETSAT <- raster(MAPS_I_method_EUMETSAT, i)
  DUST_images_MetFrance <- raster(MAPS_II_method_MetFrance, i)
  
  r <- mean(DUST_images_EUMETSAT, DUST_images_MetFrance)
  # r <- max(DUST_images_EUMETSAT, DUST_images_MetFrance)
  # r <- min(DUST_images_EUMETSAT, DUST_images_MetFrance)
  
  AVG_DUST_image_SPRING <- stack(AVG_DUST_image_SPRING,r)
  
}

# plot(AVG_DUST_image)
writeRaster(AVG_DUST_image_SPRING, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots/DUST_SPRING_AVG_MAPS.tif", options= "INTERLEAVE=BAND", overwrite=T)



###########################################################
# map to be exported ######################################
###########################################################

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots")

#### define an unique colorbar for all the maps in the stack
# load rasters in a stack
raster_stack <- stack("DUST_SPRING_AVG_MAPS.tif")
# check numbers of years
TS <- seq(from=2004, by=1, to=2017)


vec_all <- as.vector(raster_stack)
max_val<- (max(vec_all, na.rm = T))
max_val <- 750
min_val<- 0
# min_val<- (min(vec_all,  na.rm = T))


stat_dat <- summary(as.vector(raster_stack))
IQR <- (as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR

low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR <-if ( max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))

cool = rainbow(15, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
cool_2 = rainbow(50, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
warm = rainbow(135, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
cols = c(rev(cool), rev(cool_2), rev(warm))


########################
### plots of maps ######
########################

AVG_DUST_image_SPRING <- stack("DUST_SPRING_AVG_MAPS.tif")

h <- rasterVis::levelplot(AVG_DUST_image_SPRING, 
                          #    margin=FALSE, main= as.character(TITLE),
                          margin=FALSE, main= "Dust SEVIRI - SPRING (EUMETSAT & MeteoFrance)",
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
                          # names.attr=rep(names(DUST_images))) +
                          names.attr=as.character(seq(from = 2004, to = 2017, by= 1))) +
  latticeExtra::layer(sp.polygons(shp_UAE, col = "black", alpha = 1))
# h

output_dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots"
png(paste0(output_dir, "/", "SPRING_MAPS_mean_hours_DUST_EUMETSAT_&_METFRANCE.png"), width = 1100, height = 900,
    units = "px", pointsize = 100,
    bg = "white", res = 100)
print(h)
dev.off()


#################
#### FALL #######
#################

# load maps (they must have the same number of layers)
MAPS_I_method_EUMETSAT <- stack("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_I_Method_EUMETSAT/seasons_MAPS/STACK_FALL_24h_SUM_I_Method.tif")
# plot(MAPS_I_method_EUMETSAT)
MAPS_II_method_MetFrance <- stack("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_II_Method_METFRANCE/seasons_MAPS/STACK_FALL_24h_SUM_II_Method.tif")
# plot(MAPS_II_method_MetFrance)

AVG_DUST_image_FALL <- stack() 

for (i in 1:length(MAPS_I_method_EUMETSAT@layers)) {
  DUST_images_EUMETSAT <- raster(MAPS_I_method_EUMETSAT, i)
  DUST_images_MetFrance <- raster(MAPS_II_method_MetFrance, i)
  
  r <- mean(DUST_images_EUMETSAT, DUST_images_MetFrance)
  # r <- max(DUST_images_EUMETSAT, DUST_images_MetFrance)
  # r <- min(DUST_images_EUMETSAT, DUST_images_MetFrance)
  
  AVG_DUST_image_FALL <- stack(AVG_DUST_image_FALL,r)
  
}

# plot(AVG_DUST_image)
writeRaster(AVG_DUST_image_FALL, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots/DUST_FALL_AVG_MAPS.tif", options= "INTERLEAVE=BAND", overwrite=T)



###########################################################
# map to be exported ######################################
###########################################################

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots")

#### define an unique colorbar for all the maps in the stack
# load rasters in a stack
raster_stack <- stack("DUST_FALL_AVG_MAPS.tif")
# check numbers of years
TS <- seq(from=2004, by=1, to=2017)


vec_all <- as.vector(raster_stack)
max_val<- (max(vec_all, na.rm = T))
max_val <- 750
min_val<- 0
# min_val<- (min(vec_all,  na.rm = T))


stat_dat <- summary(as.vector(raster_stack))
IQR <- (as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR

low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR <-if ( max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))

cool = rainbow(15, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
cool_2 = rainbow(50, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
warm = rainbow(135, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
cols = c(rev(cool), rev(cool_2), rev(warm))


########################
### plots of maps ######
########################

AVG_DUST_image_FALL <- stack("DUST_FALL_AVG_MAPS.tif")

h <- rasterVis::levelplot(AVG_DUST_image_FALL, 
                          #    margin=FALSE, main= as.character(TITLE),
                          margin=FALSE, main= "Dust SEVIRI - FALL (EUMETSAT & MeteoFrance)",
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
                          # names.attr=rep(names(DUST_images))) +
                          names.attr=as.character(seq(from = 2004, to = 2017, by= 1))) +
  latticeExtra::layer(sp.polygons(shp_UAE, col = "black", alpha = 1))
# h

output_dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots"
png(paste0(output_dir, "/", "FALL_MAPS_mean_hours_DUST_EUMETSAT_&_METFRANCE.png"), width = 1100, height = 900,
    units = "px", pointsize = 100,
    bg = "white", res = 100)
print(h)
dev.off()


####################################################
### AVERAGE all seasons from the two METHODS #######
####################################################

WINTER_MetFrance <- raster("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_II_Method_METFRANCE/seasons_MAPS/AVG_WINTER_24h_SUM_II_Method.tif") 
WINTER_EUMETSAT <- raster("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_I_Method_EUMETSAT/seasons_MAPS/AVG_WINTER_24h_SUM_I_Method.tif")

SPRING_MetFrance <- raster("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_II_Method_METFRANCE/seasons_MAPS/AVG_SPRING_24h_SUM_II_Method.tif") 
SPRING_EUMETSAT <- raster("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_I_Method_EUMETSAT/seasons_MAPS/AVG_SPRING_24h_SUM_I_Method.tif")

SUMMER_MetFrance <- raster("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_II_Method_METFRANCE/seasons_MAPS/AVG_SUMMER_24h_SUM_II_Method.tif") 
SUMMER_EUMETSAT <- raster("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_I_Method_EUMETSAT/seasons_MAPS/AVG_SUMMER_24h_SUM_I_Method.tif")

FALL_MetFrance <- raster("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_II_Method_METFRANCE/seasons_MAPS/AVG_FALL_24h_SUM_II_Method.tif") 
FALL_EUMETSAT <- raster("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_I_Method_EUMETSAT/seasons_MAPS/AVG_FALL_24h_SUM_I_Method.tif")

WINTER_MetFrance <- WINTER_MetFrance*100/2160
SPRING_MetFrance <- SPRING_MetFrance*100/2160
SUMMER_MetFrance <- SUMMER_MetFrance*100/2160
FALL_MetFrance <- FALL_MetFrance*100/2160


WINTER_EUMETSAT <- WINTER_EUMETSAT*100/2160
SPRING_EUMETSAT <- SPRING_EUMETSAT*100/2160
SUMMER_EUMETSAT <- SUMMER_EUMETSAT*100/2160
FALL_EUMETSAT <- FALL_EUMETSAT*100/2160

# WINTER_MAP <- mean(WINTER_MetFrance, WINTER_EUMETSAT)
# SPRING_MAP <- mean(SPRING_MetFrance, SPRING_MetFrance)
# SUMMER_MAP <- mean(SUMMER_MetFrance, SUMMER_EUMETSAT)
# FALL_MAP <- mean(FALL_MetFrance, FALL_EUMETSAT)

# STACK_seasons <- stack(WINTER_MAP,
#                       SPRING_MAP,
#                       SUMMER_MAP,
#                       FALL_MAP)

STACK_seasons <- stack(WINTER_EUMETSAT,
                       SPRING_EUMETSAT,
                       SUMMER_EUMETSAT,
                       FALL_EUMETSAT)

STACK_seasons <- stack(WINTER_MetFrance,
                       SPRING_MetFrance,
                       SUMMER_MetFrance,
                       FALL_MetFrance)




# TS <- seq(from=2004, by=1, to=2017)
vec_all <- as.vector(STACK_seasons)
max_val<- (max(vec_all, na.rm = T))
max_val <- 260
min_val<- 0
# min_val<- (min(vec_all,  na.rm = T))


stat_dat <- summary(as.vector(STACK_seasons))
IQR <- (as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR

low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR <-if ( max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))

cool = rainbow(80, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
cool_2 = rainbow(40, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
warm = rainbow(100, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
cols = c(rev(cool), rev(cool_2), rev(warm))


########################
### plots of maps ######
########################


h <- rasterVis::levelplot(STACK_seasons, 
                          #    margin=FALSE, main= as.character(TITLE),
                          # margin=FALSE, main= "Dust by SEASONS (EUMETSAT & MeteoFrance)",
                          # margin=FALSE, main= "Dust by SEASONS (MeteoFrance)",
                          margin=FALSE, main= "Dust by SEASONS (EUMETSAT)",
                          xlab = "",
                          ylab = "",
                          ## about colorbar
                          colorkey=list(
                            space='bottom',                   
                            labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=1.5,
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
                          # names.attr=rep(names(DUST_images))) +
                          names.attr=c("Winter", "Spring", "Summer", "Fall")) +
  latticeExtra::layer(sp.polygons(shp_UAE, col = "black", alpha = 1))
#h


# output_dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots"
# png(paste0(output_dir, "/", "Mean_DUST_by_season_METFRANCE.png"), width = 1200, height = 900,
#     units = "px", pointsize = 100,
#     bg = "white", res = 100)
# print(h)
# dev.off()

output_dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots"
png(paste0(output_dir, "/", "Mean_DUST_by_season_EUMETSAT.png"), width = 1200, height = 900,
    units = "px", pointsize = 100,
    bg = "white", res = 100)
print(h)
dev.off()

# output_dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots"
# png(paste0(output_dir, "/", "Mean_DUST_by_season_EUMETSAT_&_METFRANCE.png"), width = 1200, height = 900,
#     units = "px", pointsize = 100,
#     bg = "white", res = 100)
# print(h)
# dev.off()

