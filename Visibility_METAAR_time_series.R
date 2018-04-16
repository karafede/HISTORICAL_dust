
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
# download remarks

METAR_data_2004_2010 <- read.table("METAAR_data_2004_2010.txt", skip = 1, sep="\t")
METAR_data_2010_2018 <- read.table("METAAR_data_2010_2018.txt", skip = 1, sep="\t")

# make table from single columns in the date
Station_number_2004 <- as.data.frame(str_sub(METAR_data_2004_2010[,1], start = 1, end = -143))
colnames(Station_number_2004) <- "station_ID"

Station_number_2010 <- as.data.frame(str_sub(METAR_data_2010_2018[,1], start = 1, end = -143))
colnames(Station_number_2010) <- "station_ID"

DateTime_2004 <- as.data.frame(str_sub(METAR_data_2004_2010[,1], start = 13, end = -123))
colnames(DateTime_2004) <- "DateTime"

DateTime_2010 <- as.data.frame(str_sub(METAR_data_2010_2018[,1], start = 13, end = -123))
colnames(DateTime_2010) <- "DateTime"

Visby_2004 <- as.data.frame(str_sub(METAR_data_2004_2010[,1], start = 53, end = -92))
colnames(Visby_2004) <- "visby"

Visby_2010 <- as.data.frame(str_sub(METAR_data_2010_2018[,1], start = 53, end = -92))
colnames(Visby_2010) <- "visby"

MET_CONDS_2004 <- as.data.frame(str_sub(METAR_data_2004_2010[,1], start = 57, end = -89))
colnames(MET_CONDS_2004) <- "MET_CONDS"

MET_CONDS_2010 <- as.data.frame(str_sub(METAR_data_2010_2018[,1], start = 57, end = -89))
colnames(MET_CONDS_2010) <- "MET_CONDS"

METAR_data_2004_2010 <- cbind(DateTime_2004,
                              Station_number_2004,
                              Visby_2004,
                              MET_CONDS_2004)

METAR_data_2010_2018 <- cbind(DateTime_2010,
                              Station_number_2010,
                              Visby_2010,
                              MET_CONDS_2010)

str(METAR_data_2004_2010)

METAR_data_2004_2010 <- METAR_data_2004_2010 %>%
  mutate(DateTime = ymd_hm(DateTime))

str(METAR_data_2004_2010)

METAR_data_2010_2018 <- METAR_data_2010_2018 %>%
  mutate(DateTime = ymd_hm(DateTime))


METAR_DATA_2004_2018 <- rbind(METAR_data_2004_2010,
                              METAR_data_2010_2018)

str(METAR_DATA_2004_2018)


# clean data from special symbol ###################################################
METAR_DATA_2004_2018$visby <- gsub("[\\****]", "", METAR_DATA_2004_2018$visby)
METAR_DATA_2004_2018$visby <- as.numeric(METAR_DATA_2004_2018$visby)

METAR_DATA_2004_2018$MET_CONDS <- gsub("\\**", "", METAR_DATA_2004_2018$MET_CONDS)
METAR_DATA_2004_2018$MET_CONDS <- as.numeric(METAR_DATA_2004_2018$MET_CONDS)

# remove lines with NA in the MET_conds column
METAR_DATA_2004_2018 <- METAR_DATA_2004_2018[!is.na(METAR_DATA_2004_2018$MET_CONDS),]
METAR_DATA_2004_2018 <- METAR_DATA_2004_2018[!is.na(METAR_DATA_2004_2018$visby),]


##############################
#### station INFO ############
##############################


# load station info
station_info <- read.table("info_stations.txt", skip = 2, sep="\t")

# select station code and station name
station_ID <- as.data.frame(str_sub(station_info[,1], start = 1, end = -149))
colnames(station_ID) <- "station_ID"

station_name <- as.data.frame(str_sub(station_info[,1], start = 13, end = -115))
colnames(station_name) <- "station"

station_info <- cbind(station_ID,
                      station_name)


# join info stations with data

METAR_DATA_2004_2018 <- METAR_DATA_2004_2018 %>%
  dplyr::left_join(station_info, by=("station_ID"))

# remove lines wtih NA in the station column
METAR_DATA_2004_2018 <- METAR_DATA_2004_2018[!is.na(METAR_DATA_2004_2018$station),]


###############################
#### subset dust events #######
###############################

# 06: Widespread dust in suspension in the air, not raised by wind at or near the station at the time of observation

# 07: Dust or sand raised by wind at or near the station at the time of observation, but no well-developed dust whirl(s) or sand whirl(s),
# and no duststorm or sandstorm seen or, in the case  of ships, blowing spray at the station

# 08: Well developed dust whirl(s) or sand whirl(s) seen at or near the station during the preceding hour or at the time of observation, but
# no duststorm or sandstorm

# 09: Duststorm or sandstorm within sight at the time of observation, or at the station during the preceding hour

# 30-35  Duststorm, sandstorm

##########################################################################
# assign 1 when there is dust event and 0 when there is no dust event ####
##########################################################################

METAR_DATA_2004_2018$DUST_flag <- ifelse(METAR_DATA_2004_2018$MET_CONDS %in% c(06, 07, 08, 09, 30:35), 1, 0)


# consider only 1 flag per hour

METAR_DATA_2004_2018 <- METAR_DATA_2004_2018 %>%
  mutate(Date = date(DateTime),
         hour = hour(DateTime))

METAR_DATA_2004_2018 <- METAR_DATA_2004_2018 %>%
  group_by(station,
           Date,
           hour) %>%
  summarize(DUST_flag = mean(DUST_flag))


# daily sum of hourly data (dust event)

METAR_DATA_2004_2018_DAILY <- METAR_DATA_2004_2018 %>%
  group_by(station,
           Date) %>%
  summarize(DAILY_SUM = sum(DUST_flag))  # max value should be 24h


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
  dplyr::select(date, Weather)


OMAA <- OMAA[!is.na(OMAA$Weather),]
OMAA <- na.omit(OMAA)


OMDB <- read.csv("OMDB_mash_clean_timestamp_no_duplicates.csv")
OMDB <- OMDB %>%
  mutate(date = ymd_hms(date)) %>%
  dplyr::select(date, Weather)
  
  
OMAL <- read.csv("OMAL_mash_clean_timestamp_no_duplicates.csv")
OMAL <- OMAL %>%
  mutate(date = ymd_hms(date)) %>%
  dplyr::select(date, Weather)

# merge all data together
All_METAR <- OMDB %>%
  left_join(OMAA, by = c("date"))
  
  
  
  

METAR_DATA_2004_2018_DAILY$Date <- as.POSIXct(METAR_DATA_2004_2018_DAILY$Date)
str(METAR_DATA_2004_2018_DAILY)

# plot daily VISIBILITY data

plot <- ggplot(METAR_DATA_2004_2018_DAILY, aes(Date, DAILY_SUM)) +
  theme_bw() +
  geom_line(aes(y = DAILY_SUM, col = "DAILY_SUM"), alpha=1, col="black") +
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


#### save plot ###############################################################
##############################################################################

output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/METAR_data/plots/"


png(paste0(output_folder,"DAILY_dust_METAR_2004_2018.png"), width = 2000, height = 1000,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()


###################################################################################################

###########################
# aggregate data by YEAR ##
###########################

METAR_DATA_2004_2018_DAILY <- METAR_DATA_2004_2018_DAILY %>%
  mutate(YEAR = year(Date))

METAR_DATA_YEAR <- METAR_DATA_2004_2018_DAILY %>%
  group_by(YEAR, station) %>%
  summarize(ANNUAL_AVG = sum(DAILY_SUM, na.rm = TRUE))

str(METAR_DATA_YEAR)


###########################################################
## ANNUAL plot ############################################
###########################################################


plot <- ggplot(METAR_DATA_YEAR, aes(YEAR, ANNUAL_AVG)) +
  theme_bw() +
  geom_line(aes(y = ANNUAL_AVG, col = "ANNUAL_AVG"), alpha=1, col="black") +
  # stat_smooth(method = "loess") +
  facet_wrap(~ station) +
  theme(strip.text = element_text(size = 12)) + 
  ylab(expression(paste("Duration of Dust (hours)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=10, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) 
#  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) 
#  ylim(0, 100)
plot



#################################################################################################
##### bar plot ##################################################################################

plot <- ggplot(METAR_DATA_YEAR, aes(YEAR, ANNUAL_AVG)) +
  theme_bw() +
  geom_bar(stat="identity") +
  facet_wrap(~ station) +
  stat_smooth(method = "lm", se = FALSE) +
  theme(strip.text = element_text(size = 12)) + 
  ylab(expression(paste("Duration of Dust (hours)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=10, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) 
# ylim(0, 160000)
plot


# plot ###
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/METAR_data/plots/"

png(paste0(output_folder,"Annual_DUST_METAR_2004_2018.png"), width = 2000, height = 1000,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()

#####################################################################################################
#####################################################################################################

