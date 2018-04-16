

library(readr)
library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
# install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)
library(stringr)

# load location of airport in the UAE

sites_Airports_UAE <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/Airport_Locations_UAE_new.csv")


# load AERONET data
AERONET_MASDAR <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/AERONET_L2/AERONET_MASDAR.csv")
AERONET_MEZAIRA <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/AERONET_L2/AERONET_MEZAIRA.csv")

AERONET_MASDAR <- AERONET_MASDAR %>%
  mutate(date = ymd(Date)) %>%
  dplyr::select(-X,
                - Date,
                - time)

AERONET_MEZAIRA <- AERONET_MEZAIRA %>%
  mutate(date = ymd(Date)) %>%
  dplyr::select(-X,
                - Date,
                - time)


# load METAR data
METAR <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/METAR_data/All_METAR_DUST_hourly.csv")
METAR <- METAR %>%
  mutate(date = date(date)) %>%
  dplyr::select(-X)

# get data at the Air port of Abu Dhabi
METAR_ABU_DHABI_AUH  <- METAR %>%
  filter(station == "ABU DHABI INTL")

# join METAR data at the Airport with AERONET MASDAR data data by hour
METAR_ABU_DHABI_AUH <- METAR_ABU_DHABI_AUH %>%
  left_join(AERONET_MASDAR, by = c("date", "hour"))



