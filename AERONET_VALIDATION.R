
library(readr)
library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
# install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)
library(stringr)
library(ggplot2)

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

max(AERONET_MASDAR$AOD_500nm)

AERONET_MEZAIRA <- AERONET_MEZAIRA %>%
  mutate(date = ymd(Date)) %>%
  dplyr::select(-X,
                - Date,
                - time)

max(AERONET_MEZAIRA$AOD_500nm)

# load all METAR data
METAR_all <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/METAR_data/All_METAR_hourly.csv")
METAR_all <- METAR_all %>%
  mutate(date = date(date)) %>%
  dplyr::select(-X)

# load METAR data for dusty days ONLY
METAR_DUST <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/METAR_data/All_METAR_DUST_hourly.csv")
METAR_DUST <- METAR_DUST %>%
  mutate(date = date(date)) %>%
  dplyr::select(-X)

# get data at the Airport of Abu Dhabi
METAR_ABU_DHABI_AUH  <- METAR_all %>%
  filter(station == "ABU DHABI INTL")

# join METAR data at the Airport of Abu Dhabi with AERONET MASDAR data data by hour
METAR_AERONET_AUH <- METAR_ABU_DHABI_AUH %>%
  left_join(AERONET_MASDAR, by = c("date", "hour"))

# add Date and Time
METAR_AERONET_AUH$DateTime <- paste0(METAR_AERONET_AUH$date, " ", METAR_AERONET_AUH$hour, ":00")
str(METAR_AERONET_AUH)
METAR_AERONET_AUH <-METAR_AERONET_AUH %>%
  mutate(DateTime = ymd_hm(DateTime)) %>%
  filter(date > "2012-01-01")

# all AEORONET and METAR data in Abu Dhabi
write.csv(METAR_AERONET_AUH, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/AERONET_L2/METAR_AERONET_AUH.csv")



library(ggplot2)
library(scales)
library(reshape2)

# plot all AERONET (all days) at Masdar AUH)
plot <- ggplot(METAR_AERONET_AUH, aes(DateTime, AOD_500nm)) +
  theme_bw() +
  geom_point(aes(y = AOD_500nm, col = "AOD_500nm"), alpha=1, col="red") +
#  geom_line(aes(y = DAILY_AOD_TERRA, col = "DAILY_AOD_TERRA"), alpha=0.3, col="blue") +
#  scale_color_discrete(name = "Y series", labels = c("DAILY_AOD_AQUA", "DAILY_AOD_TERRA")) +
  # stat_smooth(method = "loess") +
  theme(strip.text = element_text(size = 12)) + 
  ylab(expression(paste("AOD"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=15, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=15, colour = "black")) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y"))
#  ylim(0, 100)
plot




# load AERONET data with ONLY CLOUDY DAYS
METAR_AERONET_AUH_CLOUD <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/AERONET_L2/METAR_AERONET_AUH_CLOUD.csv")
str(METAR_AERONET_AUH_CLOUD)
METAR_AERONET_AUH_CLOUD <- METAR_AERONET_AUH_CLOUD %>%
  mutate(DateTime = mdy_hm(DateTime),
         AOD_500nm_CLOUD = AOD_500nm) 


# all AEORONET and METAR data in Abu Dhabi CLEAR SKY ONLY
METAR_AERONET_AUH_CLEAR_SKY <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/AERONET_L2/METAR_AERONET_AUH_CLEAR_SKY.csv")
str(METAR_AERONET_AUH_CLEAR_SKY)
METAR_AERONET_AUH_CLEAR_SKY <- METAR_AERONET_AUH_CLEAR_SKY %>%
  mutate(DateTime = mdy_hm(DateTime),
         AOD_500nm_CLEAR_SKY = AOD_500nm) 


METAR_AERONET_AUH_CLOUD <- METAR_AERONET_AUH_CLOUD %>%
  group_by(DateTime) %>%
  summarise(AOD_500nm_CLOUD = mean(AOD_500nm_CLOUD))

METAR_AERONET_AUH_CLEAR_SKY <- METAR_AERONET_AUH_CLEAR_SKY %>%
  group_by(DateTime) %>%
  summarise(AOD_500nm_CLEAR_SKY = mean(AOD_500nm_CLEAR_SKY))


METAR_AERONET_AUH_CLOUD <- METAR_AERONET_AUH_CLOUD %>%
  dplyr::select(DateTime,
                AOD_500nm_CLOUD)

METAR_AERONET_AUH_CLEAR_SKY <- METAR_AERONET_AUH_CLEAR_SKY %>%
  dplyr::select(DateTime,
                AOD_500nm_CLEAR_SKY)


METAR_AERONET_AUH <- METAR_AERONET_AUH %>%
  dplyr::select(DateTime,
                AOD_500nm) %>%
  group_by(DateTime) %>%
  summarise(AOD_500nm = mean(AOD_500nm))


# join AERONET CLOUD with AERONET ALL data (cloud and clear sky)
METAR_AERONET_AUH <- METAR_AERONET_AUH %>%
  left_join(METAR_AERONET_AUH_CLOUD, by = "DateTime")
str(METAR_AERONET_AUH)


# join AERONET CLOUD with AERONET ALL and CLEAR SKY)
METAR_AERONET_AUH <- METAR_AERONET_AUH %>%
  left_join(METAR_AERONET_AUH_CLEAR_SKY, by = "DateTime")
str(METAR_AERONET_AUH)


# plot.....
plot <- ggplot(METAR_AERONET_AUH, aes(DateTime, AOD_500nm)) +
  theme_bw() +
  geom_point(aes(y = AOD_500nm, col = "AOD_500nm"), alpha=1, col="black", size = 1.5) +
  geom_point(aes(y = AOD_500nm_CLEAR_SKY, col = "AOD_500nm_CLEAR_SKY"), alpha=1, col="blue", size = 1.5) +
 geom_point(aes(y = AOD_500nm_CLOUD, col = "AOD_500nm_CLOUD"), alpha=1, col="red", size = 1.5) +
  # stat_smooth(method = "loess") +
  theme(strip.text = element_text(size = 12)) + 
  ylab(expression(paste("AOD"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=15, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=20),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20, colour = "black")) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y"))
#  ylim(0, 100)
plot



output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/AERONET_L2/"

png(paste0(output_folder,"AERONET_AUH_clear_and_CLOUD.jpg"),
    width = 1600, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()


# LOAD MODIS TERRA and AQUA data

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust")
# AQUA @ 1:00 pm
extracted_AQUA <- read.csv("extracted_AOD_AQUA_DAILY_UAE_Airports.csv")
extracted_AQUA$DateTime <- paste0(extracted_AQUA$DateTime, " ", "13:00:00")
extracted_AQUA <- extracted_AQUA %>%
  mutate(DateTime = ymd_hms(DateTime))
str(extracted_AQUA)

# filter data for ABU Dhabi only
extracted_AQUA <- extracted_AQUA %>%
  filter(station == "ABU DHABI INTL") %>%
  filter(!DAILY_AOD_AQUA == 999) %>%
  dplyr::select(-X,
                - station)

# TERRA @ 10 am
extracted_TERRA <- read.csv("extracted_AOD_TERRA_DAILY_UAE_Airports.csv")
extracted_TERRA <- na.omit(extracted_TERRA)
extracted_TERRA$DateTime <- paste0(extracted_TERRA$DateTime, " ", "10:00:00")
extracted_TERRA <- extracted_TERRA %>%
  mutate(DateTime = ymd_hms(DateTime))
str(extracted_TERRA)

# filter data for ABU Dhabi only
extracted_TERRA <- extracted_TERRA %>%
  filter(station == "ABU DHABI INTL") %>%
  filter(!DAILY_AOD_TERRA == 999) %>%
  dplyr::select(-X,
                - station)


# join AERONET data with MODIS data (TERRA @ 10:00 am, AQUA @ 01:00 pm)
METAR_AERONET_AUH <- METAR_AERONET_AUH %>%
  left_join(extracted_TERRA, by = "DateTime")
str(METAR_AERONET_AUH)

METAR_AERONET_AUH <- METAR_AERONET_AUH %>%
  left_join(extracted_AQUA, by = "DateTime")
str(METAR_AERONET_AUH)


# plot.....
plot <- ggplot(METAR_AERONET_AUH, aes(DateTime, AOD_500nm)) +
  theme_bw() +
#  geom_point(aes(y = AOD_500nm, col = "AOD_500nm"), alpha=1, col="black", size = 1.5) +
  geom_point(aes(y = AOD_500nm_CLEAR_SKY, col = "AOD_500nm_CLEAR_SKY"), alpha=1, col="blue", size = 1.5) +
#  geom_point(aes(y = AOD_500nm_CLOUD, col = "AOD_500nm_CLOUD"), alpha=1, col="red", size = 1.5) +
  geom_point(aes(y = DAILY_AOD_TERRA, col = "DAILY_AOD_TERRA"), alpha=1, col="red", size = 1.5, shape=2) +
  geom_point(aes(y = DAILY_AOD_TERRA, col = "DAILY_AOD_AQUA"), alpha=1, col="red", size = 1.5, shape=2) +
 # geom_smooth(method="lm", aes(y = DAILY_AOD_AQUA, col = "DAILY_AOD_AQUA"), formula = y ~ poly(x, 26), size = 1, fill = "blue", col = "black") +  
  # stat_smooth(method = "loess") +
  theme(strip.text = element_text(size = 12)) + 
  ylab(expression(paste("AOD"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=25, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=25),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=25, colour = "black")) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  ylim(0, 2.3)
plot


# save plot
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/AERONET_L2/"

png(paste0(output_folder,"AERONET_MODIS_AUH_CLEAR_SKY.jpg"),
    width = 1600, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()
