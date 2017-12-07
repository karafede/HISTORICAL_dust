
library(RCurl)
library(stringr)
library(plyr)
library(dplyr)
library(threadr)
library(gdalUtils)
library(rgdal)
library(raster)
library(RNetCDF)
library(readr)
library(gstat)
library(curl)
library(leaflet)
library(webshot)
library(htmlwidgets)

### data ectraction from HDF files -----------------------------------------------

#################
## MODIS TERRA ##
#################

setwd("F:/Historical_DUST")
main <- getwd()
list_directories <- dir(pattern = "MODIS_TERRA")

#### !!!!! temporary remove the directory for the year 2017 !!!!!!

k <- 1

for (k in 1:length(list_directories)) {
  setwd(paste0(main,"/",list_directories[k]))
  wd <- setwd(paste0(main,"/",list_directories[k]))
  

# setwd("F:/Historical_DUST/MODIS_TERRA_2017")
# wd <- setwd("F:/Historical_DUST/MODIS_TERRA_2017")


# list data for each directory
DAYS <- str_sub(list.dirs(), start = 3, end = -1)
DAYS <- DAYS[-1]

# make a look for each directory that corresponds to each day

#-----START of the LOOP for all files------------------------

i <- 9

for (i in 1:length(DAYS)) {
  date <- DAYS[i]
  setwd(paste0(wd,"/",DAYS[i]))
  filenames <- list.files(pattern = "\\.hdf$") 

  
  ###############################################################################
  # collate the tiles together
  
  filenames <- list.files(pattern = "\\.hdf$")
  
  dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/UAE_boundary"
  ### shapefile for UAE
  shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")
  
  # ----- Transform to EPSG 4326 - WGS84 (required)
  shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
  # names(shp)
  
  shp_UAE@data$name <- 1:nrow(shp_UAE)
  # plot(shp_UAE)
  
  
  # # make a function to generate all .csv files with Lon, lat and value ##----
  ######-----------------------------------------------------------------------
  
  i <- 1
  file <- filenames[i]
  
  extract_HDF <- function (file) {      ## this is the filenames 
    
    # get list of field names
    nome <- str_sub(file, start = 1, end = -9)
   
    # working with FWtools library--------
    # get list of field names.....please wait untit it open the phyton libraries from the PC (need to have QGis with Gdal utilities installed)
    sds <- get_subdatasets(file)
    #  AOD <- sds[64]  ## field value AOD AOD_550_Dark_Target_Deep_Blue_Combined # 10km
  
    #############
    # AOD 10km ##
    #############
    AOD <- sds[64]  # AOD at 550 nm (1km)
    filename <- rasterTmpFile()
    extension(filename) <- 'tif'
    gdal_translate(sds[64], dst_dataset = filename) 
    AOD <- raster(filename)
    
    # data values for AOD
    AOD <- rasterToPoints(AOD)  
    colnames(AOD) <- c("x", "y", "AOD")
    AOD <- as.data.frame(AOD)
    # AOD$AOD <- (AOD$AOD)/1000

    
    #############
    # lon 10km ##
    #############
    lon <- sds[71]  # longitude
    filename <- rasterTmpFile()
    extension(filename) <- 'tif'
    gdal_translate(sds[71], dst_dataset = filename) 
    lon <- raster(filename)
    
    # data values for lon
    lon <- rasterToPoints(lon)  
    colnames(lon) <- c("x", "y", "lon")
    lon <- as.data.frame(lon)
  
    #############
    # lat 10km ##
    #############
    lat <- sds[72]  # latitude
    filename <- rasterTmpFile()
    extension(filename) <- 'tif'
    gdal_translate(sds[72], dst_dataset = filename) 
    lat <- raster(filename)
    
    # data values for lon
    lat <- rasterToPoints(lat)  
    colnames(lat) <- c("x", "y", "lat")
    lat <- as.data.frame(lat) 

    
    
    # Join  lat, lon 
    Lat_Lon <- lat %>% 
      inner_join(lon, c("x", "y"))
    
    Lat_Lon_AOD <- Lat_Lon %>% 
      inner_join(AOD, c("x", "y"))  
    
    MODIS_data <- Lat_Lon_AOD %>%
      dplyr:: select(lon, lat, AOD)
    MODIS_data <- na.omit(MODIS_data)
    
    write.csv(MODIS_data, file = paste(nome,".csv", sep = ""), row.names=FALSE)
    
  }  
  
  
  BBB <- lapply(filenames, extract_HDF)
  
  # delete HDF files
 # if (file.exists(filenames)) file.remove(filenames)  
  
  ######################################################################################
  ######################################################################################
  
  # collate the tiles together ####-------------------------------------
  
  filenames_tiles <- list.files(pattern = "\\.csv$")
  
  LAT = NULL
  LON = NULL
  aod = NULL
  
  
  ## Bind all data together 
  for (i in 1:length(filenames_tiles)) {
    lon <- read_csv(filenames_tiles[i])[,1]
    lat <- read_csv(filenames_tiles[i])[,2]
    AOD <- read_csv(filenames_tiles[i])[,3]
    LON = rbind(LON, data.frame(lon))
    LAT = rbind(LAT, data.frame(lat))
    aod = rbind(aod, data.frame(AOD))
  }
  
  MODIS04_data <- cbind(LON, LAT, aod)
  MODIS04_data <- subset(MODIS04_data, !is.na(values) & !lat == -999 & !lon == -999)
  
  write.csv(MODIS04_data, paste0("AOD_MOD04_10km_UAE","_",date,".csv"))
  
  # head(MODIS04_data)
  
}
  
  
}
  

#################
## MODIS AQUA ###
#################


setwd("F:/Historical_DUST")
main <- getwd()
list_directories <- dir(pattern = "MODIS_AQUA")

#### !!!!! temporary remove the directory for the year 2017 !!!!!!

k <- 1

for (k in 1:length(list_directories)) {
  setwd(paste0(main,"/",list_directories[k]))
  wd <- setwd(paste0(main,"/",list_directories[k]))

# setwd("F:/Historical_DUST/MODIS_AQUA_2017")
# wd <- setwd("F:/Historical_DUST/MODIS_AQUA_2017")


# list data for each directory
DAYS <- str_sub(list.dirs(), start = 3, end = -1)
DAYS <- DAYS[-1]

# make a look for each directory that corresponds to each day

#-----START of the LOOP for all files------------------------

i <- 9

for (i in 1:length(DAYS)) {
  date <- DAYS[i]
  setwd(paste0(wd,"/",DAYS[i]))
  filenames <- list.files(pattern = "\\.hdf$") 
  
  
  ###############################################################################
  # collate the tiles together
  
  filenames <- list.files(pattern = "\\.hdf$")
  
  dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/UAE_boundary"
  ### shapefile for UAE
  shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")
  
  # ----- Transform to EPSG 4326 - WGS84 (required)
  shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
  # names(shp)
  
  shp_UAE@data$name <- 1:nrow(shp_UAE)
  # plot(shp_UAE)
  
  
  # # make a function to generate all .csv files with Lon, lat and value ##----
  ######-----------------------------------------------------------------------
  
  i <- 1
  file <- filenames[i]
  
  extract_HDF <- function (file) {      ## this is the filenames 
    
    # get list of field names
    nome <- str_sub(file, start = 1, end = -9)
    
    # working with FWtools library--------
    # get list of field names.....please wait untit it open the phyton libraries from the PC (need to have QGis with Gdal utilities installed)
    sds <- get_subdatasets(file)
    #  AOD <- sds[64]  ## field value AOD AOD_550_Dark_Target_Deep_Blue_Combined # 10km
    
    #############
    # AOD 10km ##
    #############
    AOD <- sds[64]  # AOD at 550 nm (1km)
    filename <- rasterTmpFile()
    extension(filename) <- 'tif'
    gdal_translate(sds[64], dst_dataset = filename) 
    AOD <- raster(filename)
    
    # data values for AOD
    AOD <- rasterToPoints(AOD)  
    colnames(AOD) <- c("x", "y", "AOD")
    AOD <- as.data.frame(AOD)
    # AOD$AOD <- (AOD$AOD)/1000
    
    
    #############
    # lon 10km ##
    #############
    lon <- sds[71]  # longitude
    filename <- rasterTmpFile()
    extension(filename) <- 'tif'
    gdal_translate(sds[71], dst_dataset = filename) 
    lon <- raster(filename)
    
    # data values for lon
    lon <- rasterToPoints(lon)  
    colnames(lon) <- c("x", "y", "lon")
    lon <- as.data.frame(lon)
    
    #############
    # lat 10km ##
    #############
    lat <- sds[72]  # latitude
    filename <- rasterTmpFile()
    extension(filename) <- 'tif'
    gdal_translate(sds[72], dst_dataset = filename) 
    lat <- raster(filename)
    
    # data values for lon
    lat <- rasterToPoints(lat)  
    colnames(lat) <- c("x", "y", "lat")
    lat <- as.data.frame(lat) 
    
    
    
    # Join  lat, lon 
    Lat_Lon <- lat %>% 
      inner_join(lon, c("x", "y"))
    
    Lat_Lon_AOD <- Lat_Lon %>% 
      inner_join(AOD, c("x", "y"))  
    
    MODIS_data <- Lat_Lon_AOD %>%
      dplyr:: select(lon, lat, AOD)
    MODIS_data <- na.omit(MODIS_data)
    
    write.csv(MODIS_data, file = paste(nome,".csv", sep = ""), row.names=FALSE)
    
  }  
  
  
  BBB <- lapply(filenames, extract_HDF)
  
  
  # delete HDF files
  # if (file.exists(filenames)) file.remove(filenames)  
  
  ######################################################################################
  ######################################################################################
  
  # collate the tiles together ####-------------------------------------
  
  filenames_tiles <- list.files(pattern = "\\.csv$")
  
  LAT = NULL
  LON = NULL
  aod = NULL
  
  
  ## Bind all data together 
  for (i in 1:length(filenames_tiles)) {
    lon <- read_csv(filenames_tiles[i])[,1]
    lat <- read_csv(filenames_tiles[i])[,2]
    AOD <- read_csv(filenames_tiles[i])[,3]
    LON = rbind(LON, data.frame(lon))
    LAT = rbind(LAT, data.frame(lat))
    aod = rbind(aod, data.frame(AOD))
  }
  
  MODIS04_data <- cbind(LON, LAT, aod)
  MODIS04_data <- subset(MODIS04_data, !is.na(values) & !lat == -999 & !lon == -999)
  
  write.csv(MODIS04_data, paste0("AOD_MYD04_10km_UAE","_",date,".csv"))
  

}

}

#####################################################################################################################
#####################################################################################################################
############## RASTERIZE MODIS data #################################################################################
#####################################################################################################################

library(readr)
library(sp)
library(raster)
library(gstat)
library(rgdal)
library(RNetCDF)
library(ncdf4)
library(stringr)
library(raster)


#### importing the UAE shapefile to use as a masking 
dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/UAE_boundary"
### shapefile for UAE
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
# names(shp)

shp_UAE@data$name <- 1:nrow(shp_UAE)
# plot(shp_UAE)



###########
# TERRA ###
###########

wd_AOD <- "F:/Historical_DUST/MODIS_TERRA_2017"
setwd(wd_AOD)


# list directories
DAYS_TERRA <- str_sub(list.dirs(), start = 3, end = -1)
DAYS_TERRA <- DAYS_TERRA[-1]

i <- 2

resl_ras= 0.1

for (i in 1:length(DAYS_TERRA)) {

  date <- DAYS_TERRA[i]
  setwd(paste0(wd_AOD,"/",DAYS_TERRA[i]))
  
  filenames_TERRA <- list.files(pattern = "AOD_MOD04_10km_UAE_")
  
  ##### make a function to create a raster from an irregular data frame (lat , lon , AOD)
  raster_irregular <- function(filenames_AQUA, resl_ras= 0.1){
    
    date <- str_sub(filenames_TERRA, start = 20, end = -5)
    AOD_TERRA <- read_csv(filenames_TERRA)[-1]  # load the one without NA values
    
    colnames(AOD_TERRA) <- c('x', 'y', 'z')
    # x.range <- as.numeric(c(floor(min(AOD_TERRA$x)-1),ceiling(max(AOD_TERRA$x)+1)))  # min/max longitude of the interpolation area
    # y.range <- as.numeric(c(floor(min(AOD_TERRA$y)-1),ceiling(max(AOD_TERRA$y)+1)))  # min/max latitude of the interpolation area
    
    x.range <- as.numeric(c(38,78))
    y.range <- as.numeric(c(5,44))
    
    grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = resl_ras),
                       y = seq(from = y.range[1], to = y.range[2], by = resl_ras))  # expand points to grid
    
    grd_1<- dplyr::filter(grd, grd$x == 45)
    nrow(grd_1)
    grd_2<- dplyr::filter(grd, grd$y == 15)
    nrow(grd_2)
    # r <- raster(xmn=min(AOD_AQUA$x), xmx=max(AOD_AQUA$x), ymn=min(AOD_AQUA$y), 
    #             ymx=max(AOD_AQUA$y), ncol=4601, nrow= 3201)
    
    r <- raster(xmn=min(AOD_TERRA$x), xmx=max(AOD_TERRA$x), ymn=min(AOD_TERRA$y),
                ymx=max(AOD_TERRA$y), ncol=nrow(grd_2)*0.8, nrow= nrow(grd_1)*0.8)
    
    raster_TERRA <- rasterize(AOD_TERRA[, 1:2], r, AOD_TERRA[,3], fun=mean)
    
    res(raster_TERRA)
    plot(raster_TERRA)
    projection(raster_TERRA) <- CRS("+proj=longlat +datum=WGS84")
    projection(raster_TERRA) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(shp_UAE, add=TRUE, lwd=1)
    raster_TERRA <- crop(raster_TERRA, extent(shp_UAE))
    raster_TERRA <- mask(raster_TERRA, shp_UAE)  
    plot(raster_TERRA)
    
    writeRaster(raster_TERRA, paste0(wd_AOD,"/",date,".tif"), overwrite = TRUE)   

  }
  
  BBB <- lapply(filenames_TERRA, raster_irregular)
  
}



###########
# AQUA ####
###########

wd_AOD <- "F:/Historical_DUST/MODIS_AQUA_2017"
setwd(wd_AOD)


# list directories
DAYS_AQUA <- str_sub(list.dirs(), start = 3, end = -1)
DAYS_AQUA <- DAYS_AQUA[-1]

i <- 2

resl_ras= 0.1

for (i in 1:length(DAYS_AQUA)) {
  date <- DAYS_AQUA[i]
  setwd(paste0(wd_AOD,"/",DAYS_AQUA[i]))
  
  filenames_AQUA <- list.files(pattern = "AOD_MYD04_10km_UAE_")
  
  ##### make a function to create a raster from an irregular data frame (lat , lon , AOD)
  raster_irregular <- function(filenames_AQUA, resl_ras= 0.1){
    
    date <- str_sub(filenames_AQUA, start = 20, end = -5)
    AOD_AQUA <- read_csv(filenames_AQUA)[-1]  # load the one without NA values
    
    colnames(AOD_AQUA) <- c('x', 'y', 'z')
    # x.range <- as.numeric(c(floor(min(AOD_AQUA$x)-1),ceiling(max(AOD_AQUA$x)+1)))  # min/max longitude of the interpolation area
    # y.range <- as.numeric(c(floor(min(AOD_AQUA$y)-1),ceiling(max(AOD_AQUA$y)+1)))  # min/max latitude of the interpolation area
    
    x.range <- as.numeric(c(38,78))
    y.range <- as.numeric(c(6,46))
    
    grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = resl_ras),
                       y = seq(from = y.range[1], to = y.range[2], by = resl_ras))  # expand points to grid
    
    grd_1<- dplyr::filter(grd, grd$x == 46)
    nrow(grd_1)
    grd_2<- dplyr::filter(grd, grd$y == 15)
    nrow(grd_2)
    # r <- raster(xmn=min(AOD_AQUA$x), xmx=max(AOD_AQUA$x), ymn=min(AOD_AQUA$y), 
    #             ymx=max(AOD_AQUA$y), ncol=4601, nrow= 3201)
    
    r <- raster(xmn=min(AOD_AQUA$x), xmx=max(AOD_AQUA$x), ymn=min(AOD_AQUA$y),
                ymx=max(AOD_AQUA$y), ncol=nrow(grd_2)*0.8, nrow= nrow(grd_1)*0.8)
    
    raster_AQUA <- rasterize(AOD_AQUA[, 1:2], r, AOD_AQUA[,3], fun=mean)
    
    res(raster_AQUA)
    plot(raster_AQUA)
    projection(raster_AQUA) <- CRS("+proj=longlat +datum=WGS84")
    projection(raster_AQUA) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(shp_UAE, add=TRUE, lwd=1)
    raster_AQUA <- crop(raster_AQUA, extent(shp_UAE))
    raster_AQUA <- mask(raster_AQUA, shp_UAE)  
    plot(raster_AQUA)
    
    writeRaster(raster_AQUA, paste0(wd_AOD,"/", date,".tif"), overwrite = TRUE)   
  }
  
  BBB <- lapply(filenames_AQUA, raster_irregular)
  
}



  
  
  
  
  
  