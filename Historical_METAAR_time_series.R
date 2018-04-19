
library(readr)
library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
# install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)
library(stringr)


setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/METAR_data")

# https://www7.ncdc.noaa.gov/CDO/cdopoemain.cmd?datasetabbv=DS3505&countryabbv=&georegionabbv=&resolution=40

# abbreviations 
# https://www.aviationweather.gov/static/help/taf-decode.php



############################################
####### time -series #######################
############################################

# load location of airport in the UAE

sites_Airports_UAE <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/Airport_Locations_UAE_new.csv")


library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# load data

OMAA <- read.csv("OMAA_mash_clean_timestamp_no_duplicates.csv", na.strings = " ")
OMAA <- OMAA %>%
  mutate(date = ymd_hms(date)) %>%
  mutate(station = "ABU DHABI INTL") %>%
  dplyr::select(date, station, Weather, VIS, CLOUD)


OMAA <- OMAA[!is.na(OMAA$Weather),]
OMAA <- OMAA[!is.na(OMAA$VIS),]
OMAA <- na.omit(OMAA)


OMDB <- read.csv("OMDB_mash_clean_timestamp_no_duplicates.csv")
OMDB <- OMDB %>%
  mutate(date = ymd_hms(date)) %>%  
  mutate(station = "DUBAI INTL") %>%
  dplyr::select(date, station, Weather, VIS,CLOUD)
  
  
OMAL <- read.csv("OMAL_mash_clean_timestamp_no_duplicates.csv")
OMAL <- OMAL %>%
  mutate(date = ymd_hms(date)) %>%
  mutate(station = "AL AIN INTL") %>%
  dplyr::select(date, station, Weather, VIS, CLOUD)


OMSJ <- read.csv("OMSJ_mash_clean_timestamp_no_duplicates.csv")
OMSJ <- OMSJ %>%
  mutate(date = ymd_hms(date)) %>%
  mutate(station = "SHARJAH INTL") %>%
  dplyr::select(date, station, Weather, VIS, CLOUD)

OMDW <- read.csv("OMDW_mash_clean_timestamp_no_duplicates.csv")
OMDW <- OMDW %>%
  mutate(date = ymd_hms(date)) %>%
  mutate(station = "AL MAKTOUM INTL") %>%
  dplyr::select(date, station, Weather, VIS, CLOUD)

OMAD <- read.csv("OMAD_mash_clean_timestamp_no_duplicates.csv")
OMAD <- OMAD %>%
  mutate(date = ymd_hms(date)) %>%
  mutate(station = "BATEEN") %>%
  dplyr::select(date, station, Weather, VIS, CLOUD)

OMFJ <- read.csv("OMFJ_mash_clean_timestamp_no_duplicates.csv")
OMFJ <- OMFJ %>%
  mutate(date = ymd_hms(date)) %>%
  mutate(station = "FUJAIRAH INTL") %>%
  dplyr::select(date, station, Weather, VIS, CLOUD)

OMRK <- read.csv("OMRK_mash_clean_timestamp_no_duplicates.csv")
OMRK <- OMRK %>%
  mutate(date = ymd_hms(date)) %>%
  mutate(station = "RAS AL KHAIMAH INTL") %>%
  dplyr::select(date, station, Weather, VIS, CLOUD)


# bind all data together
All_METAR <- rbind(OMAA, OMDB, OMAL, OMSJ, OMDW, OMAD, OMFJ, OMRK)

str(All_METAR)  

All_METAR <- All_METAR %>%
  mutate(hour = hour(date))

write.csv(All_METAR, "All_METAR_hourly.csv")


# select onlt dust events (DUST --> D, SAND --> S)
All_METAR_DUST <- All_METAR %>%
  filter(Weather %in% c("D", "S"))
  
# assign count for each row with Dust (1 hour --> 1 count)
if (All_METAR_DUST$Weather %in% c("D", "S")) {
  All_METAR_DUST$count <- 1
}


All_METAR_DUST$VIS <- as.numeric(All_METAR_DUST$VIS)
# All_METAR_DUST <- na.omit(All_METAR_DUST)

All_METAR_DUST <- All_METAR_DUST %>%
  mutate(hour = hour(date))

write.csv(All_METAR_DUST, "All_METAR_DUST_hourly.csv")

##############################################
##############################################
# remove cloudy days in excel.
All_METAR_DUST <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/METAR_data/All_METAR_DUST_hourly.csv")
All_METAR_DUST_CLEAR <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/METAR_data/All_METAR_DUST_hourly_CLEAR_SKY.csv")
str(All_METAR_DUST_CLEAR)


###########################
# aggregate data by DAY ###
###########################

All_METAR_DUST_DAILY <- All_METAR_DUST %>%
  mutate(date = date(date))
All_METAR_DUST_DAILY <- All_METAR_DUST_DAILY %>%
  group_by(station,
           date)

# save daily data
write.csv(All_METAR_DUST_DAILY, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/All_METAR_DUST_DAILY.csv")


# only CLEAR sky
All_METAR_DUST_CLEAR_DAILY <- All_METAR_DUST_CLEAR %>%
  mutate(date = mdy_hm(date))
All_METAR_DUST_CLEAR_DAILY <- All_METAR_DUST_CLEAR_DAILY %>%
  group_by(station,
           date)

# save daily data
write.csv(All_METAR_DUST_CLEAR_DAILY, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/All_METAR_DUST_CLEAR_DAILY.csv")

###########################
# aggregate data by YEAR ##
###########################

All_METAR_DUST <- All_METAR_DUST %>%
  mutate(YEAR = year(date))

METAR_DATA_YEAR <- All_METAR_DUST %>%
  group_by(YEAR, station) %>%
  summarize(ANNUAL_SUM = sum(count, na.rm = TRUE),
            MIN_VIS = min(VIS, na.rm = TRUE),
            MAX_VIS = max(VIS, na.rm = TRUE))

str(METAR_DATA_YEAR)

# create a Data format field
METAR_DATA_YEAR$Date <- paste(METAR_DATA_YEAR$YEAR, "-12-31")
str(METAR_DATA_YEAR)
METAR_DATA_YEAR <- METAR_DATA_YEAR %>%
  mutate(Date = ymd(Date))
METAR_DATA_YEAR$Date <- as.POSIXct(METAR_DATA_YEAR$Date)


write.csv(METAR_DATA_YEAR, "METAR_DUST_VIS_YEAR.csv")


# only with CLEAR SKY #########
###############################


All_METAR_DUST_CLEAR <- All_METAR_DUST_CLEAR %>%
  mutate(date = mdy_hm(date))

METAR_DUST_CLEAR <- All_METAR_DUST_CLEAR %>%
  mutate(YEAR = year(date))

METAR_DUST_CLEAR <- METAR_DUST_CLEAR %>%
  group_by(YEAR, station) %>%
  summarize(ANNUAL_SUM = sum(count, na.rm = TRUE),
            MIN_VIS = min(VIS, na.rm = TRUE),
            MAX_VIS = max(VIS, na.rm = TRUE))


# create a Data format field
METAR_DUST_CLEAR$Date <- paste(METAR_DUST_CLEAR$YEAR, "-12-31")
str(METAR_DUST_CLEAR)
METAR_DUST_CLEAR <- METAR_DUST_CLEAR %>%
  mutate(Date = ymd(Date))
METAR_DUST_CLEAR$Date <- as.POSIXct(METAR_DUST_CLEAR$Date)


write.csv(METAR_DUST_CLEAR, "METAR_DUST_CLEAR_VIS_YEAR.csv")

###########################################################
## ANNUAL plot ############################################
###########################################################

# dust events

plot <- ggplot(METAR_DATA_YEAR, aes(Date, ANNUAL_SUM)) +
  theme_bw() +
  geom_line(aes(y = ANNUAL_SUM, col = "ANNUAL_SUM"), alpha=1, col="black") +
  # stat_smooth(method = "loess") +
  facet_wrap(~ station) +
  theme(strip.text = element_text(size = 12)) + 
  ylab(expression(paste("Duration of Dust (hours)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=10, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) +
scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) 
#  ylim(0, 100)
plot



#################################################################################################
##### bar plot ##################################################################################

# dust events including CLOUDS

plot <- ggplot(METAR_DATA_YEAR, aes(Date, ANNUAL_SUM)) +
  theme_bw() +
  geom_bar(stat="identity") +
  facet_wrap(~ station) +
  # stat_smooth(method = "lm", se = FALSE) +
  theme(strip.text = element_text(size = 10)) + 
  ggtitle("METAR - dust hours with clouds") +
  ylab(expression(paste("Duration of Dust (hours)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=9, colour = "black", face="bold"),
        plot.title = element_text(color="red", size=14, face="bold.italic", hjust = 0.5)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) 
# ylim(0, 160000)
plot


# plot ###
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/METAR_data/plots/"

png(paste0(output_folder,"Annual_DUST_METAR_2004_2018_with_CLOUDS.png"), width = 2000, height = 1000,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()


################################
# dust events with CLEAR SKY ###
################################

plot <- ggplot(METAR_DUST_CLEAR, aes(Date, ANNUAL_SUM)) +
  theme_bw() +
  geom_bar(stat="identity") +
  facet_wrap(~ station) +
  # stat_smooth(method = "lm", se = FALSE) +
  theme(strip.text = element_text(size = 10)) + 
  ggtitle("METAR - dust hours with clear sky") +
  ylab(expression(paste("Duration of Dust (hours)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=9, colour = "black", face="bold"),
        plot.title = element_text(color="blue", size=14, face="bold.italic", hjust = 0.5)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) 
# ylim(0, 160000)
plot


# plot ###
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/METAR_data/plots/"

png(paste0(output_folder,"Annual_DUST_METAR_2004_2018_CLEAR_SKY.png"), width = 2000, height = 1000,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()





#####################################################################################################
#####################################################################################################

# visibiliy (using clear sky data)

plot <- ggplot(METAR_DUST_CLEAR, aes(Date, MIN_VIS)) +
  theme_bw() +
  geom_bar(stat="identity") +
  facet_wrap(~ station) +
  # stat_smooth(method = "lm", se = FALSE) +
  theme(strip.text = element_text(size = 10)) + 
  ggtitle("METAR - Minimum Visibility with clear sky") +
  ylab(expression(paste("Minimum Visibility (km)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=9, colour = "black", face="bold"),
        plot.title = element_text(color="blue", size=14, face="bold.italic", hjust = 0.5)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) 
# ylim(0, 160000)
plot


# plot ###
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/METAR_data/plots/"

png(paste0(output_folder,"Minimum_Visibility_METAR_2004_2018.png"), width = 2000, height = 1000,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()

