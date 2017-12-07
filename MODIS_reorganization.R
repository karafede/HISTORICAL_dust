
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


# script to re-organise all hdf file downloaded from the website of LADADS NASA into folders by day number

setwd("F:/Historical_DUST")
main <- getwd()

# run separately for MODIS TERRA and for MODIS AQUA
# list_directories <- dir(pattern = "MODIS_TERRA")
list_directories <- dir(pattern = "MODIS_AQUA")

#### !!!!! temporary remove the directory for the year 2017 !!!!!!

# k <- 1

for (k in 1:length(list_directories)) {
  setwd(paste0(main,"/",list_directories[k]))
  wd <- setwd(paste0(main,"/",list_directories[k]))
  getwd()


# setwd("F:/Historical_DUST/MODIS_TERRA_2017")
# setwd("F:/Historical_DUST/MODIS_AQUA_2017")

# folder_day <- as.character("002")

# wd <- getwd()
# Sys.time()
# current_date <- str_sub(Sys.time(), start = 1, end = -10)
# str(current_date)

# make a list of all .hdf files and create a folder for each day
filenames <- list.files(pattern = "\\.hdf$") 

# make a list of the day (from the .hdf files)
for (i in 1:length(filenames)) {
  year <- str_sub(filenames[i], start = 11, end = -31)
  day <- str_sub(filenames[i], start = 15, end = -28)
  dir.create(day)
}



# list folder names
DAYS <- str_sub(list.dirs(), start = 3, end = -1)
DAYS <- DAYS[-1]

# match folder names with specific filenames
# copy files into respective folder names
for (i in 1:length(DAYS)) {
  # AAA <- list.files(pattern = paste0(year,"001"))
  files_day <- list.files(pattern = paste0("A",year,DAYS[i]))
for (j in 1: length(files_day))
    file.copy(files_day[j], DAYS[i])
}

}

########################################################################
