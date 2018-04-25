
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
sites_Airports_UAE <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/Airport_Locations_UAE_new.csv")

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
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust")
filenames <- list.files(pattern = ".tif$")

raster_AQUA <- stack("all_DAYS_AQUA.tif")
n <- length(raster_AQUA@layers)
date_aqua <- read.csv("dates_AQUA_2004_2017.csv")  # n rows as the same number of rasters

extracted_AOD <- NULL
DateTime_AOD <- NULL
site_AOD <- NULL

i <- 10

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
# write.csv(extracted_AOD, "F:/Historical_DUST/extracted_AOD_AQUA_DAILY_UAE_Airports.csv")
# extracted_AOD_AQUA <- read.csv("F:/Historical_DUST/extracted_AOD_AQUA_DAILY_UAE_Airports.csv")

write.csv(extracted_AOD, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/extracted_AOD_AQUA_DAILY_UAE_Airports.csv")
extracted_AOD_AQUA <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/extracted_AOD_AQUA_DAILY_UAE_Airports.csv")


#################################################################################################
#################################################################################################


#################
# MODIS TERRA ###
#################

setwd("F:/Historical_DUST")
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust")
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
# write.csv(extracted_AOD, "F:/Historical_DUST/extracted_AOD_TERRA_DAILY_UAE_Airports.csv")
# extracted_AOD_TERRA <- read.csv("F:/Historical_DUST/extracted_AOD_TERRA_DAILY_UAE_Airports.csv")

write.csv(extracted_AOD, "F:/Historical_DUST/extracted_AOD_TERRA_DAILY_UAE_Airports.csv")
extracted_AOD_TERRA <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/extracted_AOD_TERRA_DAILY_UAE_Airports.csv")

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
# library(tidyr)

# calculate the mean for each station and subtract this from the data

# calculate mean value of all MODIS data
# spread_AQUA <- spread(all_DUST_MODIS[1:3], station, DAILY_AOD_AQUA)
# str(spread_AQUA)
# MODIS_TERRA <- all_DUST_MODIS %>%
#   dplyr::select(station,
#          DateTime,
#          DAILY_AOD_TERRA)
# spread_TERRA <- spread(MODIS_TERRA , station, DAILY_AOD_TERRA)
# str(spread_TERRA)
# 
# # calculate mean value for each column
# mean_AQUA <- colMeans(spread_AQUA[-1], na.rm = TRUE)
# mean_TERRA <- colMeans(spread_TERRA[-1], na.rm = TRUE)
# 
# # attach mean to the last line of the data frame (remove DateTime)
# spread_AQUA <- rbind(spread_AQUA[-1], mean_AQUA)
# spread_TERRA <- rbind(spread_TERRA[-1], mean_TERRA)
# 
# # subtract the last line from each element in the data frame
# 
# MODIS_AQUA <- NULL
# MODIS_TERRA <- NULL
# 
# for(i in 1:nrow(spread_AQUA)){
#   result = spread_AQUA[i,] - spread_AQUA[nrow(spread_AQUA),]
#   MODIS_AQUA = rbind(MODIS_AQUA, result)
#   print (result)
# }
# # remove last line
# MODIS_AQUA <- MODIS_AQUA[1:nrow(MODIS_AQUA)-1, ]
# 
# for(i in 1:nrow(spread_TERRA)){
#   result = spread_TERRA[i,] - spread_TERRA[nrow(spread_AQUA),]
#   MODIS_TERRA = rbind(MODIS_TERRA, result)
#   print (result)
# }
# 
# # remove last line
# MODIS_TERRA <- MODIS_TERRA[1:nrow(MODIS_TERRA)-1, ]
# 
# gather_AQUA <- gather(MODIS_AQUA, station, DAILY_AOD_AQUA)
# gather_TERRA <- gather(MODIS_TERRA, station, DAILY_AOD_TERRA)
# all_DUST_MODIS_bkg <- cbind(all_DUST_MODIS[2], gather_AQUA[,], gather_TERRA[2])
# colnames(all_DUST_MODIS_bkg) <- c("DateTime", "station", "DAILY_AOD_AQUA", "DAILY_AOD_TERRA")



# calculate mean value of all MODIS data
all_DUST_MODIS_MEAN <- all_DUST_MODIS %>%
  group_by(station) %>%
  summarize(MEAN_AQUA = mean(DAILY_AOD_AQUA, na.rm=TRUE),
            MEAN_TERRA = mean(DAILY_AOD_TERRA, na.rm=TRUE))

# take the minimum
all_DUST_MODIS_Min <- all_DUST_MODIS_MEAN %>%
  summarize(min_AQUA = min(MEAN_AQUA),
            min_TERRA = min(MEAN_TERRA))


MIN_AQUA <- 0.3930954
MIN_TERRA <- 0.3547656

all_DUST_MODIS$DAILY_AOD_AQUA <- all_DUST_MODIS$DAILY_AOD_AQUA - MIN_AQUA
all_DUST_MODIS$DAILY_AOD_TERRA <- all_DUST_MODIS$DAILY_AOD_TERRA - MIN_TERRA


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

# make average by YEAR and by STATION
all_DUST_MODIS_ANNUAL <- all_DUST_MODIS %>%
  group_by(YEAR, station) %>%
  summarize(ANNUAL_AVG_AQUA = mean(DAILY_AOD_AQUA, na.rm = TRUE),
            ANNUAL_AVG_TERRA = mean(DAILY_AOD_TERRA, na.rm = TRUE))

# calculate MAXIMUM by YEAR and by STATION
all_DUST_MODIS_ANNUAL <- all_DUST_MODIS %>%
  group_by(YEAR, station) %>%
  summarize(ANNUAL_MAX_AQUA = max(DAILY_AOD_AQUA, na.rm = TRUE),
            ANNUAL_MAX_TERRA = max(DAILY_AOD_TERRA, na.rm = TRUE))

str(all_DUST_MODIS_ANNUAL)

# create a Data format field
all_DUST_MODIS_ANNUAL$Date <- paste(all_DUST_MODIS_ANNUAL$YEAR, "-12-31")
str(all_DUST_MODIS_ANNUAL)
all_DUST_MODIS_ANNUAL <- all_DUST_MODIS_ANNUAL %>%
  mutate(Date = ymd(Date))
all_DUST_MODIS_ANNUAL$Date <- as.POSIXct(all_DUST_MODIS_ANNUAL$Date)


# remove some stations
all_DUST_MODIS_ANNUAL <- all_DUST_MODIS_ANNUAL %>%
  filter(!station %in% c("DELMA", "DUBAI", "ZIRKU",
                         "ASH SHARIQAH SW", "RAS-AL-KHAIMA","SIR ABU NAIR", "DUBAI MINHAD AB"))

write.csv(all_DUST_MODIS_ANNUAL, "all_DUST_MAX_AOD_TERRA_AQUA_ANNUAL.csv")

STAT_DUST_MODIS <- all_DUST_MODIS_ANNUAL %>%
  group_by(station) %>%
  summarise(MAX_AOD_AQUA = max(ANNUAL_MAX_AQUA, na.rm = TRUE),
            MAX_AOD_TERRA = max(ANNUAL_MAX_TERRA, na.rm = TRUE))
write.csv(STAT_DUST_MODIS, "STAT_DUST_MODIS.csv")


###########################################################
## ANNUAL plot ############################################
###########################################################


plot <- ggplot(all_DUST_MODIS_ANNUAL, aes(Date, ANNUAL_MAX_AQUA)) +
  theme_bw() +
  geom_line(aes(y = ANNUAL_MAX_AQUA, col = "ANNUAL_MAX_AQUA"), alpha=1, col="red") +
  geom_line(aes(y = ANNUAL_MAX_TERRA, col = "ANNUAL_MAX_TERRA"), alpha=1, col="blue") +
  scale_color_discrete(name = "Y series", labels = c("ANNUAL_MAX_AQUA", "ANNUAL_MAX_TERRA")) +
  # stat_smooth(method = "loess") +
  facet_wrap(~ station) +
  theme(strip.text = element_text(size = 9)) + 
  ylab(expression(paste("Maximum ANNUAL AOD"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=8, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=10),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) +
scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) 
#  ylim(0, 100)
plot


# plot ###
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plot_MODIS/"

png(paste0(output_folder,"Annual_MAXIMUM_AOD.png"), width = 2000, height = 1000,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()




#################################################################################################
##### bar plot ##################################################################################

###########
## AQUA ###
###########

plot <- ggplot(all_DUST_MODIS_ANNUAL, aes(Date, ANNUAL_MAX_AQUA)) +
  theme_bw() +
  geom_bar(stat="identity") +
  facet_wrap(~ station) +
  stat_smooth(method = "lm", se = FALSE) +
  theme(strip.text = element_text(size = 14)) + 
  ylab(expression(paste("Maximum ANNUAL AOD (MODIS Aqua)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=8, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) 
#   ylim(0, 160000)
plot


# plot ###
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plot_MODIS/"

png(paste0(output_folder,"Annual_MAXIMUM_AOD_hist_AQUA.png"), width = 2000, height = 1000,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()


############
## TERRA ###
############

plot <- ggplot(all_DUST_MODIS_ANNUAL, aes(Date, ANNUAL_MAX_TERRA)) +
  theme_bw() +
  geom_bar(stat="identity") +
  facet_wrap(~ station) +
  stat_smooth(method = "lm", se = FALSE) +
  theme(strip.text = element_text(size = 14)) + 
  ylab(expression(paste("Maximum ANNUAL AOD (MODIS Terra)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=8, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) 
#   ylim(0, 160000)
plot


# plot ###
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plot_MODIS/"

png(paste0(output_folder,"Annual_MAXIMUM_AOD_hist_TERRA.png"), width = 2000, height = 1000,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()

###########################################################################


library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(splines)
library(plyr)


# reload MODIS MAX yearly AOD
all_DUST_MODIS_ANNUAL <-  read.csv("all_DUST_MAX_AOD_TERRA_AQUA_ANNUAL.csv")

# load METAAR data (sum dust) # CLEAR SKY
METAR_DATA_YEAR <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/METAR_data/METAR_DUST_CLEAR_VIS_YEAR.csv")

# join METAR data and SEVIRI data by YEAR and by STATION
all_DUST_MODIS_ANNUAL <- all_DUST_MODIS_ANNUAL %>%
  left_join(METAR_DATA_YEAR, by = c("YEAR", "station"))

###################################################################################
###################################################################################
# correlate MODIS data (AQUA) with VISIBILITY events from METAR ###################

# remove all Na rows from ANNUAL_SUM column
all_DUST_MODIS_ANNUAL <- all_DUST_MODIS_ANNUAL[!is.na(all_DUST_MODIS_ANNUAL$ANNUAL_SUM),]


## this function includes the intercept~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lm_eqn <- function(df){
  m <- lm(MIN_VIS ~  ANNUAL_MAX_AQUA, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(a = format(coef(m)[2], digits = 2),
                        b = format(coef(m)[1], digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}


# define regression equation for each station
eq_DUST <- ddply(all_DUST_MODIS_ANNUAL, .(station),lm_eqn)

# plot correlation 

plot <- ggplot(all_DUST_MODIS_ANNUAL, aes(x = ANNUAL_MAX_AQUA, y=MIN_VIS)) +
  theme_bw() +
  geom_point(size = 2, color='black') +    # Use hollow circles
  geom_smooth(method=lm, formula=y~x, fill=NA) +  # Add linear regression line  (force to origin)
  facet_wrap( ~ station) +
  theme( strip.text = element_text(size = 12)) + 
  ylab(expression(paste("METAR (minimum visibility - km)"))) +
  xlab(expression(paste("MAX AOD (AQUA)"))) +
  # ylim(c(-1, 5)) +
  # xlim(c(0, 3.5)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=12)) +
  geom_text(data = eq_DUST, aes(x = 2, y = 4.5, label = V1),
            parse = TRUE, inherit.aes=FALSE, size = 5, color = "red" )
plot

# save plot
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plot_MODIS/"

png(paste0(output_folder,"Correlation_MIN_VIS_METAAR_vs_AOD_AQUA.jpg"),
    width = 1600, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()

################################################################################
# correlate MODIS data (TERRA) with VISIBILITY events from METAR ################

## this function includes the intercept~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lm_eqn <- function(df){
  m <- lm(MIN_VIS ~  ANNUAL_MAX_TERRA, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(a = format(coef(m)[2], digits = 2),
                        b = format(coef(m)[1], digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

# define regression equation for each station
eq_DUST <- ddply(all_DUST_MODIS_ANNUAL, .(station),lm_eqn)

# plot correlation 

plot <- ggplot(all_DUST_MODIS_ANNUAL, aes(x = ANNUAL_MAX_TERRA, y=MIN_VIS)) +
  theme_bw() +
  geom_point(size = 2, color='black') +    # Use hollow circles
  geom_smooth(method=lm, formula=y~x, fill=NA) +  # Add linear regression line  (force to origin)
  facet_wrap( ~ station) +
  theme( strip.text = element_text(size = 12)) + 
  ylab(expression(paste("METAR (minimum visibility - km)"))) +
  xlab(expression(paste("MAX AOD (TERRA)"))) +
  # ylim(c(-1, 5)) +
  # xlim(c(0, 3.5)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=12)) +
  geom_text(data = eq_DUST, aes(x = 2, y = 4.5, label = V1),
            parse = TRUE, inherit.aes=FALSE, size = 5, color = "red" )
plot

# save plot
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plot_MODIS/"

png(paste0(output_folder,"Correlation_MIN_VIS_METAAR_vs_AOD_TERRA.jpg"),
    width = 1600, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()


#############################################################################################
#############################################################################################
#############################################################################################

###################################################################################
###################################################################################
# correlate MODIS data (AQUA) with DUST events from METAR #########################

# remove all Na rows from ANNUAL_SUM column
all_DUST_MODIS_ANNUAL <- all_DUST_MODIS_ANNUAL[!is.na(all_DUST_MODIS_ANNUAL$ANNUAL_SUM),]


## this function includes the intercept~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lm_eqn <- function(df){
  m <- lm(ANNUAL_SUM ~  ANNUAL_MAX_AQUA, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(a = format(coef(m)[2], digits = 2),
                        b = format(coef(m)[1], digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}


# define regression equation for each station
eq_DUST <- ddply(all_DUST_MODIS_ANNUAL, .(station),lm_eqn)

# plot correlation 

plot <- ggplot(all_DUST_MODIS_ANNUAL, aes(x = ANNUAL_MAX_AQUA, y=ANNUAL_SUM)) +
  theme_bw() +
  geom_point(size = 2, color='black') +    # Use hollow circles
  geom_smooth(method=lm, formula=y~x, fill=NA) +  # Add linear regression line  (force to origin)
  facet_wrap( ~ station) +
  theme( strip.text = element_text(size = 12)) + 
  ylab(expression(paste("Hours of Dust (METAR)"))) +
  xlab(expression(paste("MAX AOD (AQUA)"))) +
  # ylim(c(0, 1200)) +
  # xlim(c(0, 3.5)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=12)) +
  geom_text(data = eq_DUST, aes(x = 2, y = 650, label = V1),
            parse = TRUE, inherit.aes=FALSE, size = 5, color = "red" )
plot

# save plot
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plot_MODIS/"

png(paste0(output_folder,"Correlation_DUST_METAAR_vs_AOD_AQUA.jpg"),
    width = 1600, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()

################################################################################
# correlate MODIS data (TERRA) with DUST events from METAR #####################

## this function includes the intercept~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lm_eqn <- function(df){
  m <- lm(ANNUAL_SUM ~  ANNUAL_MAX_TERRA, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(a = format(coef(m)[2], digits = 2),
                        b = format(coef(m)[1], digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

# define regression equation for each station
eq_DUST <- ddply(all_DUST_MODIS_ANNUAL, .(station),lm_eqn)

# plot correlation 

plot <- ggplot(all_DUST_MODIS_ANNUAL, aes(x = ANNUAL_MAX_TERRA, y=ANNUAL_SUM)) +
  theme_bw() +
  geom_point(size = 2, color='black') +    # Use hollow circles
  geom_smooth(method=lm, formula=y~x, fill=NA) +  # Add linear regression line  (force to origin)
  facet_wrap( ~ station) +
  theme( strip.text = element_text(size = 12)) + 
  ylab(expression(paste("Hours of Dust (METAR)"))) +
  xlab(expression(paste("MAX AOD (TERRA)"))) +
  # ylim(c(0, 1200)) +
  # xlim(c(0, 3.5)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=12)) +
  geom_text(data = eq_DUST, aes(x = 2, y = 650, label = V1),
            parse = TRUE, inherit.aes=FALSE, size = 5, color = "red" )
plot

# save plot
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plot_MODIS/"

png(paste0(output_folder,"Correlation_DUST_METAAR_vs_AOD_TERRA.jpg"),
    width = 1600, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()


#############################################################################################
#############################################################################################