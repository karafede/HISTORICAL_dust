
library(readr)
library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
# install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)
library(stringr)

# load function to extract points from rasters at chosen location
source("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/DUST SEVIRI/R_scripts/extract_pnt_raster.R")

library(ggplot2)
library(scales)
library(reshape2)


# load location of airport in the UAE

# sites_Airports_UAE <- read.csv("F:/Historical_DUST/Airport_Locations_UAE.csv")
sites_Airports_UAE <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/Airport_Locations_UAE_new.csv")

#############################################################################################
# read SUM of DAILY DUST EVENTS from SEVIRI data ############################################

SEVIRI_DAILY_SUM_DUST <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/DAILY_DUST_SUM_SEVIRI.csv")

str(SEVIRI_DAILY_SUM_DUST)
SEVIRI_DAILY_SUM_DUST <- SEVIRI_DAILY_SUM_DUST %>%
  mutate(date = date(DateTime))


names(SEVIRI_DAILY_SUM_DUST)[names(SEVIRI_DAILY_SUM_DUST) == 'SUM_DAILY_DUST_I_meth'] <- 'DAILY_SUM_EUMETSAT'
names(SEVIRI_DAILY_SUM_DUST)[names(SEVIRI_DAILY_SUM_DUST) == 'SUM_DAILY_DUST_II_meth'] <- 'DAILY_SUM_MeteoFrance'

# add seasons ###############################################################################

## get the months of observations
SEVIRI_DAILY_SUM_DUST$month <- factor(format(SEVIRI_DAILY_SUM_DUST$date, format = "%b"), levels = month.abb)

## Define seasons
SEVIRI_DAILY_SUM_DUST$season <- character(length = nrow(SEVIRI_DAILY_SUM_DUST))
SEVIRI_DAILY_SUM_DUST$season[SEVIRI_DAILY_SUM_DUST$month %in% month.abb[c(1:2)]] <- "winter"
SEVIRI_DAILY_SUM_DUST$season[SEVIRI_DAILY_SUM_DUST$month %in% month.abb[c(12)]] <- "winter"
SEVIRI_DAILY_SUM_DUST$season[SEVIRI_DAILY_SUM_DUST$month %in% month.abb[c(3:5)]] <- "spring"
SEVIRI_DAILY_SUM_DUST$season[SEVIRI_DAILY_SUM_DUST$month %in% month.abb[c(6:8)]] <- "summer"
SEVIRI_DAILY_SUM_DUST$season[SEVIRI_DAILY_SUM_DUST$month %in% month.abb[c(9:11)]] <- "fall"
SEVIRI_DAILY_SUM_DUST$season <- factor(SEVIRI_DAILY_SUM_DUST$season, levels = c("winter","spring","summer","fall"))


# load METAR data 
All_METAR_DUST_DAILY <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/All_METAR_DUST_DAILY.csv")


str(All_METAR_DUST_DAILY)
All_METAR_DUST_DAILY <- All_METAR_DUST_DAILY %>%
  mutate(date = date(date))

All_METAR_DUST_DAILY <- All_METAR_DUST_DAILY %>%
  group_by(date,
           station) %>%
  summarize(sum_DUST = sum(count))


## get the months of observations
All_METAR_DUST_DAILY$month <- factor(format(All_METAR_DUST_DAILY$date, format = "%b"), levels = month.abb)

## Define seasons
All_METAR_DUST_DAILY$season <- character(length = nrow(All_METAR_DUST_DAILY))
All_METAR_DUST_DAILY$season[All_METAR_DUST_DAILY$month %in% month.abb[c(1:2)]] <- "winter"
All_METAR_DUST_DAILY$season[All_METAR_DUST_DAILY$month %in% month.abb[c(12)]] <- "winter"
All_METAR_DUST_DAILY$season[All_METAR_DUST_DAILY$month %in% month.abb[c(3:5)]] <- "spring"
All_METAR_DUST_DAILY$season[All_METAR_DUST_DAILY$month %in% month.abb[c(6:8)]] <- "summer"
All_METAR_DUST_DAILY$season[All_METAR_DUST_DAILY$month %in% month.abb[c(9:11)]] <- "fall"
All_METAR_DUST_DAILY$season <- factor(All_METAR_DUST_DAILY$season, levels = c("winter","spring","summer","fall"))


# join SEVIRI DUST data with METAR data
DAILY_SUM_DUST <- All_METAR_DUST_DAILY %>%
  left_join(SEVIRI_DAILY_SUM_DUST, by = c("date", "station", "season"))
  
# remove all Na rows from DAILY_SUM_EUMETSAT
DAILY_SUM_DUST <- DAILY_SUM_DUST[!is.na(DAILY_SUM_DUST$DAILY_SUM_EUMETSAT),]


# make yearly averages by seasons
DAILY_SUM_DUST <- DAILY_SUM_DUST %>%
  mutate(YEAR = year(date))

YEARLY_SUM_DUST_SEASON <- DAILY_SUM_DUST %>%
  group_by(station,
           YEAR,
           season) %>%
  summarize(SUM_EUMETSAT = sum(DAILY_SUM_EUMETSAT, na.rm = TRUE),
            SUM_MeteoFrance = sum(DAILY_SUM_MeteoFrance, na.rm = TRUE),
            SUM_METAR = sum(sum_DUST, na.rm = TRUE))

str(YEARLY_SUM_DUST_SEASON)

# YEARLY_SUM_DUST_SEASON$date <- paste0(YEARLY_SUM_DUST_SEASON$YEAR, "-12-31") 
# YEARLY_SUM_DUST_SEASON$date <- as.Date(YEARLY_SUM_DUST_SEASON$date)

# melt data
# YEARLY_SUM_DUST_SEASON$YEAR <- NULL

YEARLY_SUM_DUST_SEASON <- melt(YEARLY_SUM_DUST_SEASON, id=c("YEAR","station", "season"))
str(YEARLY_SUM_DUST_SEASON)

YEARLY_SUM_DUST_SEASON$station <- as.factor(YEARLY_SUM_DUST_SEASON$station)


############################################
####### bar plots ##########################
############################################

library(ggplot2)
library(scales)
library(reshape2)


# rename variables
levels(YEARLY_SUM_DUST_SEASON$variable) <- gsub("^SUM_MeteoFrance$","MeteoFrance_Meth", levels(YEARLY_SUM_DUST_SEASON$variable))
levels(YEARLY_SUM_DUST_SEASON$variable) <- gsub("^SUM_EUMETSAT$","EUMETSAT_Meth", levels(YEARLY_SUM_DUST_SEASON$variable))
levels(YEARLY_SUM_DUST_SEASON$variable) <- gsub("^SUM_METAR$","METAR", levels(YEARLY_SUM_DUST_SEASON$variable))

levels(YEARLY_SUM_DUST_SEASON$station) <- gsub("^ABU DHABI INTL$","Abu Dhabi", levels(YEARLY_SUM_DUST_SEASON$station))
levels(YEARLY_SUM_DUST_SEASON$station) <- gsub("^AL AIN INTL$","Al Ain", levels(YEARLY_SUM_DUST_SEASON$station))
levels(YEARLY_SUM_DUST_SEASON$station) <- gsub("^AL MAKTOUM INTL$","Al Maktoum", levels(YEARLY_SUM_DUST_SEASON$station))
levels(YEARLY_SUM_DUST_SEASON$station) <- gsub("^BATEEN$","Bateen", levels(YEARLY_SUM_DUST_SEASON$station))
levels(YEARLY_SUM_DUST_SEASON$station) <- gsub("^DUBAI INTL$","Dubai", levels(YEARLY_SUM_DUST_SEASON$station))
levels(YEARLY_SUM_DUST_SEASON$station) <- gsub("^FUJAIRAH INTL$","Fujairah", levels(YEARLY_SUM_DUST_SEASON$station))
levels(YEARLY_SUM_DUST_SEASON$station) <- gsub("^RAS AL KHAIMAH INTL$","Ras Al Khaimah", levels(YEARLY_SUM_DUST_SEASON$station))
levels(YEARLY_SUM_DUST_SEASON$station) <- gsub("^SHARJAH INTL$","Sharjah", levels(YEARLY_SUM_DUST_SEASON$station))


# Create a named character vector that relates factor levels to colors.
grays = c(MeteoFrance_Meth="red", EUMETSAT_Meth="blue", METAR="black")

YEARLY_SUM_DUST_SEASON_ABU <- YEARLY_SUM_DUST_SEASON %>%
  filter(station == "Abu Dhabi")

plot <- ggplot(YEARLY_SUM_DUST_SEASON_ABU, aes(YEAR, value, fill = variable)) +
  theme_bw() +
  geom_bar(stat="identity", width = 0.8, position = "dodge") +
  scale_fill_manual(values=grays) +
  ggtitle("Abu Dhabi") +
  theme(plot.title = element_text(hjust = 0.5)) +
  # facet_grid(station ~ season) +
  facet_wrap(~ season) +
  theme( strip.text = element_text(size = 14)) + 
  ylab(expression(paste("Seasonal duration of Dust (hours)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=10, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=10),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=8, colour = "black")) +
# scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
ylim(0, 500)
plot



# save plot
ggsave(plot=plot, filename=paste0("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots/Abu_Dhabi_Annual_Seasonal_Hours_of_dust_SEVIRI_and_METAR.png"), height=5, width=10)




YEARLY_SUM_DUST_SEASON_DUBAI <- YEARLY_SUM_DUST_SEASON %>%
  filter(station == "Dubai")

plot <- ggplot(YEARLY_SUM_DUST_SEASON_DUBAI, aes(YEAR, value, fill = variable)) +
  theme_bw() +
  geom_bar(stat="identity", width = 0.8, position = "dodge") +
  scale_fill_manual(values=grays) +
  ggtitle("Dubai") +
  theme(plot.title = element_text(hjust = 0.5)) +
  # facet_grid(station ~ season) +
  facet_wrap(~ season) +
  theme( strip.text = element_text(size = 14)) + 
  ylab(expression(paste("Seasonal duration of Dust (hours)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=10, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=10),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=8, colour = "black")) +
  # scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  ylim(0, 500)
plot



# save plot
ggsave(plot=plot, filename=paste0("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots/Dubai_Annual_Seasonal_Hours_of_dust_SEVIRI_and_METAR.png"), height=5, width=10)



###########################################################################################
###########################################################################################

