
library(readr)
library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
# install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)
library(stringr)


setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/METAR_data")

# data_visibility <- read_csv("visibility_data_2004_2017.csv")
METAR_data_2004_2010 <- read.table("METAAR_data_2004_2010.txt", skip = 1, sep="\t")
METAR_data_2010_2018 <- read.table("METAAR_data_2010_2018.txt", skip = 1, sep="\t")

# make table from single columns in the date
Station_number_2004 <- as.data.frame(str_sub(METAR_data_2004_2010[,1], start = 1, end = -143))
colnames(Station_number_2004) <- "station"

Station_number_2010 <- as.data.frame(str_sub(METAR_data_2010_2018[,1], start = 1, end = -143))
colnames(Station_number_2010) <- "station"

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


# clean data 
METAR_DATA_2004_2018$visby <- gsub("[\\****]", "", METAR_DATA_2004_2018$visby)
METAR_DATA_2004_2018$visby <- as.numeric(METAR_DATA_2004_2018$visby)

METAR_DATA_2004_2018$MET_CONDS <- gsub("\\**", "", METAR_DATA_2004_2018$MET_CONDS)
METAR_DATA_2004_2018$MET_CONDS <- as.numeric(METAR_DATA_2004_2018$MET_CONDS)

# remove lines with NA in the MET_conds column
METAR_DATA_2004_2018 <- METAR_DATA_2004_2018[!is.na(METAR_DATA_2004_2018$MET_CONDS),]
METAR_DATA_2004_2018 <- METAR_DATA_2004_2018[!is.na(METAR_DATA_2004_2018$visby),]





###########################
# station INFO ############
###########################


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



METAR_DATA_2004_2018_DUST <- METAR_DATA_2004_2018 %>%
  filter(MET_CONDS %in% c(55, 97))



############################################
####### time -series #######################
############################################

library(ggplot2)
library(scales)
library(reshape2)


str(data_visibility)

data_visibility$Date <- ymd(data_visibility$Date)

str(data_visibility)


data_visibility <- data_visibility %>%
  dplyr::select(NAME,
         Date,
         HrMn,
         Visby_meters)
names(data_visibility)[names(data_visibility) == 'NAME'] <- 'station'

# AVERAGED daily visibility
data_visibility <- data_visibility %>%
  group_by(station,
           Date) %>%
  summarize(DAILY_visby = mean(Visby_meters, na.rm = TRUE))
  

# remove some stations with poor data
data_visibility <- data_visibility %>%
  filter(!station %in% c("BUHASA", "ZIRKU",
                         "ASH SHARIQAH SW","SIR ABU NAIR"))

# plot daily VISIBILITY data

plot <- ggplot(data_visibility, aes(Date, DAILY_visby)) +
  theme_bw() +
  geom_line(aes(y = DAILY_visby, col = "DAILY_visby"), alpha=1, col="black") +
 # stat_smooth(method = "loess") +
  facet_wrap(~ station) +
  theme(strip.text = element_text(size = 12)) + 
  ylab(expression(paste("visibility (meters)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=10, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) 
 # scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) 
#  ylim(0, 100)
plot


#### save plot ###############################################################
##############################################################################

output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/METAR_data/visibility/plots/"


png(paste0(output_folder,"daily_visibility_2004_2017.png"), width = 2000, height = 1000,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()


###################################################################################################

###########################
# aggregate data by YEAR ##
###########################

data_visibility <- data_visibility %>%
  mutate(YEAR = year(Date))

data_visibility_YEAR <- data_visibility %>%
  group_by(YEAR, station) %>%
  summarize(ANNUAL_AVG_visby = mean(DAILY_visby, na.rm = TRUE))

str(data_visibility_YEAR)


###########################################################
## ANNUAL plot ############################################
###########################################################


plot <- ggplot(data_visibility_YEAR, aes(YEAR, ANNUAL_AVG_visby)) +
  theme_bw() +
  geom_line(aes(y = ANNUAL_AVG_visby, col = "ANNUAL_AVG_visby"), alpha=1, col="black") +
  # stat_smooth(method = "loess") +
  facet_wrap(~ station) +
  theme(strip.text = element_text(size = 12)) + 
  ylab(expression(paste("Annual Visibility"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=10, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) 
#  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) 
#  ylim(0, 100)
plot



#################################################################################################
##### bar plot ##################################################################################

plot <- ggplot(data_visibility_YEAR, aes(YEAR, ANNUAL_AVG_visby)) +
  theme_bw() +
  geom_bar(stat="identity") +
  facet_wrap(~ station) +
  stat_smooth(method = "lm", se = FALSE) +
  theme(strip.text = element_text(size = 12)) + 
  ylab(expression(paste("Annual Visibility"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=10, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) 
# ylim(0, 160000)
plot


# plot ###
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/METAR_data/visibility/plots/"

png(paste0(output_folder,"Annual_visibility_2004_2017.png"), width = 2000, height = 1000,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()



