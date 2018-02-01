
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

rows <- nrow(sites_Airports_UAE)


#############################################################################################
# read of DAILY AOD EVENTS from STACKED Rasters #############################################
#############################################################################################
## make a function that reads each station at each time and extract points ##################
##############################################################################################

#################
# MODIS AQUA ####
#################

setwd("F:/Historical_DUST")
filenames <- list.files(pattern = ".tif$")

raster_AQUA <- stack("all_DAYS_AQUA.tif")
n <- length(raster_AQUA@layers)
date_aqua <- read.csv("dates_AQUA_2004_2017.csv")  # n rows as the same number of rasters

extracted_AOD <- NULL
DateTime_AOD <- NULL
site_AOD <- NULL

i <- 14

for (i in 1:n) {

TS <- date_aqua$TS_AQUA[i]
TS <- as.POSIXct(TS)
TS <- as.Date(TS)
class(TS)

AOD_raster_AQUA <- raster("all_DAYS_AQUA.tif", band = i)  
 plot(AOD_raster_AQUA)
 # values <- values(AOD_raster_AQUA)
 
 if (all(is.na(values(AOD_raster_AQUA)))) {
   EXTRACTED_AOD <- data.frame(matrix(999, ncol = 1, nrow = rows))
   colnames(EXTRACTED_AOD) <- "values_extr"
   }  else {
     EXTRACTED_AOD <- extract_points(AOD_raster_AQUA, sites_Airports_UAE)
   }
  extracted_AOD = rbind(extracted_AOD, EXTRACTED_AOD)    
  DATETIME_AOD <- as.data.frame(rep(TS, nrow(sites_Airports_UAE)))           
  DateTime_AOD <- rbind(DateTime_AOD, DATETIME_AOD)
  SITE_AOD <- as.data.frame(sites_Airports_UAE$Site)
  site_AOD <- rbind(site_AOD, SITE_AOD)
   
}

extracted_AOD <- cbind(DateTime_AOD, extracted_AOD, site_AOD)
colnames(extracted_AOD) <- c("DateTime", "DAILY_AOD_AQUA", "station")


# save data-------------------------------------
write.csv(extracted_AOD, "F:/Historical_DUST/extracted_AOD_AQUA_DAILY_UAE_Airports.csv")
extracted_AOD_AQUA <- read.csv("F:/Historical_DUST/extracted_AOD_AQUA_DAILY_UAE_Airports.csv")




#################################################################################################
#################################################################################################


#################
# MODIS TERRA ###
#################

setwd("F:/Historical_DUST")
filenames <- list.files(pattern = ".tif$")

raster_TERRA <- stack("all_DAYS_TERRA.tif")
n <- length(raster_TERRA@layers)
date_terra <- read.csv("dates_TERRA_2004_2017.csv")  # n rows as the same number of rasters

extracted_AOD <- NULL
DateTime_AOD <- NULL
site_AOD <- NULL

i <- 14

for (i in 1:n) {
  
  TS <- date_terra$TS_TERRA[i]
  TS <- as.POSIXct(TS)
  TS <- as.Date(TS)
  class(TS)
  
  AOD_raster_TERRA <- raster("all_DAYS_TERRA.tif", band = i)  
  plot(AOD_raster_TERRA)
  
  if (all(is.na(values(AOD_raster_TERRA)))) {
    EXTRACTED_AOD <- data.frame(matrix(999, ncol = 1, nrow = rows))
    colnames(EXTRACTED_AOD) <- "values_extr"
  }  else {
    EXTRACTED_AOD <- extract_points(AOD_raster_TERRA, sites_Airports_UAE)
  }
  extracted_AOD = rbind(extracted_AOD, EXTRACTED_AOD)    
  DATETIME_AOD <- as.data.frame(rep(TS, nrow(sites_Airports_UAE)))           
  DateTime_AOD <- rbind(DateTime_AOD, DATETIME_AOD)
  SITE_AOD <- as.data.frame(sites_Airports_UAE$Site)
  site_AOD <- rbind(site_AOD, SITE_AOD)
}

extracted_AOD <- cbind(DateTime_AOD, extracted_AOD, site_AOD)
colnames(extracted_AOD) <- c("DateTime", "DAILY_AOD_TERRA", "station")


# save data-------------------------------------
write.csv(extracted_AOD, "F:/Historical_DUST/extracted_AOD_TERRA_DAILY_UAE_Airports.csv")
extracted_AOD_TERRA <- read.csv("F:/Historical_DUST/extracted_AOD_TERRA_DAILY_UAE_Airports.csv")

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################

#######################
#### End of data extraction

####################################################################################################
####################################################################################################

############################################
####### time -series #######################
############################################

library(ggplot2)
library(scales)
library(reshape2)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust")
extracted_AQUA <- read.csv("extracted_AOD_AQUA_DAILY_UAE_Airports.csv")
extracted_TERRA <- read.csv("extracted_AOD_TERRA_DAILY_UAE_Airports.csv")

# filter out all the 999 numbers
extracted_AQUA <- extracted_AQUA %>%
  filter(!DAILY_AOD_AQUA == 999)

extracted_TERRA <- extracted_TERRA %>%
  filter(!DAILY_AOD_TERRA == 999)



# names(extracted_SUM_DUST_I_Method)[names(extracted_SUM_DUST_I_Method) == 'SUM_DAILY_DUST'] <- 'SUM_DAILY_DUST_I_meth'

extracted_AQUA$DateTime <- ymd(extracted_AQUA$DateTime)
extracted_TERRA$DateTime <- ymd(extracted_TERRA$DateTime)


# average data from the same NAME location 
extracted_AQUA <- extracted_AQUA %>%
  group_by(station, DateTime) %>%
  summarize(DAILY_AOD_AQUA = mean(DAILY_AOD_AQUA))

extracted_TERRA <- extracted_TERRA %>%
  group_by(station, DateTime) %>%
  summarize(DAILY_AOD_TERRA = mean(DAILY_AOD_TERRA))



extracted_AQUA$DateTime <- as.POSIXct(extracted_AQUA$DateTime)
extracted_TERRA$DateTime <- as.POSIXct(extracted_TERRA$DateTime)


# join MODIS AQUA wth MODIS TERRA
all_DUST_MODIS <- extracted_AQUA %>%
  left_join(extracted_TERRA, by = c("DateTime", "station"))

str(all_DUST_MODIS)

# remove some stations with poor data
# all_DUST_MODIS <- all_DUST_MODIS %>%
#   filter(!station %in% c("DELMA", "DUBAI", "ZIRKU",
#                          "ASH SHARIQAH SW", "RAS-AL-KHAIMA","SIR ABU NAIR", "DUBAI MINHAD AB"))


######################################
# plot hourly data AQUA and TERRA ####
######################################


plot <- ggplot(all_DUST_MODIS, aes(DateTime, DAILY_AOD_AQUA)) +
  theme_bw() +
  geom_line(aes(y = DAILY_AOD_AQUA, col = "DAILY_AOD_AQUA"), alpha=1, col="red") +
  geom_line(aes(y = DAILY_AOD_TERRA, col = "DAILY_AOD_TERRA"), alpha=0.3, col="blue") +
  # scale_colour_manual("", 
  #                     breaks = c("SUM_DAILY_DUST_II_meth", "SUM_DAILY_DUST_I_meth"),
  #                     values = c("SUM_DAILY_DUST_II_meth"="red", 
  #                                "SUM_DAILY_DUST_I_meth"="blue")) +
  scale_color_discrete(name = "Y series", labels = c("DAILY_AOD_AQUA", "DAILY_AOD_TERRA")) +
 # stat_smooth(method = "loess") +
  facet_wrap(~ station) +
  theme(strip.text = element_text(size = 12)) + 
  ylab(expression(paste("AOD"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=10, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) 
#  ylim(0, 100)
plot


#### save plot ###############################################################
##############################################################################

output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plot_MODIS/"


png(paste0(output_folder,"DAILY_AOD_MODIS_AQUA_TERRA.png"), width = 2000, height = 1000,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()


###################################################################################################

###########################
# aggregate data by YEAR ##
###########################

all_DUST_MODIS <- all_DUST_MODIS %>%
  mutate(YEAR = year(DateTime))

all_DUST_MODIS_ANNUAL <- all_DUST_MODIS %>%
  group_by(YEAR, station) %>%
  summarize(ANNUAL_AVG_AQUA = mean(DAILY_AOD_AQUA, na.rm = TRUE),
            ANNUAL_AVG_TERRA = mean(DAILY_AOD_TERRA, na.rm = TRUE))

str(all_DUST_MODIS_ANNUAL)


###########################################################
## ANNUAL plot ############################################
###########################################################


plot <- ggplot(all_DUST_MODIS_ANNUAL, aes(YEAR, ANNUAL_AVG_AQUA)) +
  theme_bw() +
  geom_line(aes(y = ANNUAL_AVG_AQUA, col = "ANNUAL_AVG_AQUA"), alpha=1, col="red") +
  geom_line(aes(y = ANNUAL_AVG_TERRA, col = "ANNUAL_AVG_TERRA"), alpha=1, col="blue") +
  scale_color_discrete(name = "Y series", labels = c("ANNUAL_AVG_AQUA", "ANNUAL_AVG_TERRA")) +
  # stat_smooth(method = "loess") +
  facet_wrap(~ station) +
  theme(strip.text = element_text(size = 12)) + 
  ylab(expression(paste("Average ANNUAL AOD"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=10, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) 
#  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) 
#  ylim(0, 100)
plot


# plot ###
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plot_MODIS/"

png(paste0(output_folder,"Annual_AVERAGE_AOD.png"), width = 2000, height = 1000,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()




#################################################################################################
##### bar plot ##################################################################################

## AQUA ###
plot <- ggplot(all_DUST_MODIS_ANNUAL, aes(YEAR, ANNUAL_AVG_AQUA)) +
  theme_bw() +
  geom_bar(stat="identity") +
  facet_wrap(~ station) +
  stat_smooth(method = "lm", se = FALSE) +
  theme(strip.text = element_text(size = 12)) + 
  ylab(expression(paste("Average ANNUAL AOD"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=10, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) 
#  ylim(0, 160000)
plot







