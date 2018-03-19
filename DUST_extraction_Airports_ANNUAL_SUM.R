
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


# load location of airport in the UAE

# sites_Airports_UAE <- read.csv("F:/Historical_DUST/Airport_Locations_UAE.csv")
sites_Airports_UAE <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/Airport_Locations_UAE_new.csv")

#############################################################################################
# read SUM of DAILY DUST EVENTS from STACKED Rasters ########################################
#############################################################################################
## make a function that reads each station at each time and extract points ##################
##############################################################################################


#################################################
#### II Method Met France Original ##############
#################################################

setwd("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_II_Method_METFRANCE")
filenames <- list.files(pattern = ".tif$")
filenames <- filenames[1:14]

# read rasters of the ANNUAL SUM of DUST EVENTS with SEVIRI data (II method)

i <- 7
# filenames <- filenames[i]

extracted_SUM_DUST <- NULL
DateTime_DUST <- NULL
site_DUST <- NULL

for (i in 1:length(filenames)) {

year <- str_sub(filenames[i], start= 1, end=-30)
DATE <- paste0(year,"-12-31")
TS <- as.Date(DATE)
class(TS)


 r <- raster(filenames[i])
 plot(r)

  # add conditions to filter out only GOOD maps!!!!
 values(r)[values(r) < 0] = NA
 
 
 SUM_DAILY_DUST_raster <- r

  EXTRACTED_DUST <- extract_points(SUM_DAILY_DUST_raster, sites_Airports_UAE)
  extracted_SUM_DUST = rbind(extracted_SUM_DUST, EXTRACTED_DUST)    
  DATETIME_DUST <- as.data.frame(rep(TS, nrow(sites_Airports_UAE)))           
  DateTime_DUST <- rbind(DateTime_DUST, DATETIME_DUST)
  SITE_DUST <- as.data.frame(sites_Airports_UAE$Site)
  site_DUST <- rbind(site_DUST, SITE_DUST)
  
 }

extracted_SUM_DUST <- cbind(DateTime_DUST, extracted_SUM_DUST, site_DUST)
colnames(extracted_SUM_DUST) <- c("DateTime", "SUM_DAILY_DUST", "station")


# save data-------------------------------------
write.csv(extracted_SUM_DUST, "F:/Historical_DUST/hours_ANNUAL_DUST_UAE_Airports_II_Method_MetFrance.csv")
write.csv(extracted_SUM_DUST, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/hours_ANNUAL_DUST_UAE_Airports_II_Method_MetFrance.csv")
extracted_SUM_DUST <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/hours_ANNUAL_DUST_UAE_Airports_II_Method_MetFrance.csv")

##########################################################################################################
##########################################################################################################

#################################################
#### I Method ###################################
#################################################

# read rasters of the DAILY SUM of DUST EVENTS with SEVIRI data (I method)

setwd("F:/Historical_DUST/SEVIRI_DUST_MASK_outputs/yearly_maps_I_Method_EUMETSAT")
filenames <- list.files(pattern = ".tif$")
filenames <- filenames[1:14]

i <- 7
# filenames <- filenames[i]

extracted_SUM_DUST <- NULL
DateTime_DUST <- NULL
site_DUST <- NULL

for (i in 1:length(filenames)) {
  
  year <- str_sub(filenames[i], start= 1, end=-29)
  DATE <- paste0(year,"-12-31")
  TS <- as.Date(DATE)
  class(TS)
  
  r <- raster(filenames[i])
  plot(r)
  
  # add conditions to filter out only GOOD maps!!!!
  values(r)[values(r) < 0] = NA
  
  
  SUM_DAILY_DUST_raster <- r
  
  EXTRACTED_DUST <- extract_points(SUM_DAILY_DUST_raster, sites_Airports_UAE)
  extracted_SUM_DUST = rbind(extracted_SUM_DUST, EXTRACTED_DUST)    
  DATETIME_DUST <- as.data.frame(rep(TS, nrow(sites_Airports_UAE)))           
  DateTime_DUST <- rbind(DateTime_DUST, DATETIME_DUST)
  SITE_DUST <- as.data.frame(sites_Airports_UAE$Site)
  site_DUST <- rbind(site_DUST, SITE_DUST)
  
}

extracted_SUM_DUST <- cbind(DateTime_DUST, extracted_SUM_DUST, site_DUST)
colnames(extracted_SUM_DUST) <- c("DateTime", "SUM_DAILY_DUST", "station")


# save data-------------------------------------
write.csv(extracted_SUM_DUST, "F:/Historical_DUST/hours_ANNUAL_DUST_UAE_Airports_I_Method_EUMETSAT.csv")
write.csv(extracted_SUM_DUST, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/hours_ANNUAL_DUST_UAE_Airports_I_Method_EUMETSAT.csv")
extracted_SUM_DUST <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/hours_ANNUAL_DUST_UAE_Airports_I_Method_EUMETSAT.csv")


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


extracted_SUM_DUST_I_Method <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/hours_ANNUAL_DUST_UAE_Airports_I_Method_EUMETSAT.csv")
extracted_SUM_DUST_II_Method <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/hours_ANNUAL_DUST_UAE_Airports_II_Method_MetFrance.csv")


names(extracted_SUM_DUST_I_Method)[names(extracted_SUM_DUST_I_Method) == 'SUM_DAILY_DUST'] <- 'ANNUAL_SUM_I_meth'
names(extracted_SUM_DUST_II_Method)[names(extracted_SUM_DUST_II_Method) == 'SUM_DAILY_DUST'] <- 'ANNUAL_SUM_II_meth'

str(extracted_SUM_DUST_I_Method)
str(extracted_SUM_DUST_II_Method)

extracted_SUM_DUST_I_Method$DateTime <- ymd(extracted_SUM_DUST_I_Method$DateTime)
extracted_SUM_DUST_II_Method$DateTime <- ymd(extracted_SUM_DUST_II_Method$DateTime)


# average data if from the same station
extracted_SUM_DUST_I_Method <- extracted_SUM_DUST_I_Method %>%
 group_by(station, DateTime) %>%
  summarize(ANNUAL_SUM_I_meth = mean(ANNUAL_SUM_I_meth))



# average data if from the same station
extracted_SUM_DUST_II_Method <- extracted_SUM_DUST_II_Method %>%
  group_by(station, DateTime) %>%
  summarize(ANNUAL_SUM_II_meth = mean(ANNUAL_SUM_II_meth))


head(extracted_SUM_DUST_I_Method)
head(extracted_SUM_DUST_II_Method)


extracted_SUM_DUST_I_Method$DateTime <- as.POSIXct(extracted_SUM_DUST_I_Method$DateTime)
extracted_SUM_DUST_II_Method$DateTime <- as.POSIXct(extracted_SUM_DUST_II_Method$DateTime)


all_DUST_SUM <- extracted_SUM_DUST_I_Method %>%
  left_join(extracted_SUM_DUST_II_Method, by = c("DateTime", "station"))


# remove some stations with poor data
all_DUST_SUM <- all_DUST_SUM %>%
  filter(!station %in% c("DELMA", "DUBAI", "ZIRKU",
                         "ASH SHARIQAH SW", "RAS-AL-KHAIMA","SIR ABU NAIR", "DUBAI MINHAD AB"))




plot <- ggplot(all_DUST_SUM, aes(DateTime, ANNUAL_SUM_II_meth)) +
  theme_bw() +
  geom_line(aes(y = ANNUAL_SUM_II_meth, col = "ANNUAL_SUM_II_meth"), alpha=1, col="red") +
  geom_line(aes(y = ANNUAL_SUM_I_meth, col = "ANNUAL_SUM_I_meth"), alpha=1, col="blue") +
  # scale_colour_manual("", 
  #                     breaks = c("SUM_DAILY_DUST_II_meth", "SUM_DAILY_DUST_I_meth"),
  #                     values = c("SUM_DAILY_DUST_II_meth"="red", 
  #                                "SUM_DAILY_DUST_I_meth"="blue")) +
  scale_color_discrete(name = "Y series", labels = c("ANNUAL_SUM_II_meth", "ANNUAL_SUM_I_meth")) +
 # stat_smooth(method = "loess") +
  facet_wrap(~ station) +
  theme(strip.text = element_text(size = 12)) + 
  ylab(expression(paste("Duration of Dust (hours)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=10, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  ylim(0, 1200)
plot



#### save plot ###############################################################
##############################################################################

output_folder <- "F:/Historical_DUST/plots/"
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots/"


png(paste0(output_folder,"Time_Series_ANNUAL_Hours_dust_comparisons.png"), width = 2000, height = 1000,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()


#################################################################################################
##### bar plot ##################################################################################


## I method
plot <- ggplot(all_DUST_SUM, aes(DateTime, ANNUAL_SUM_I_meth)) +
  theme_bw() +
  geom_bar(stat="identity") +
  facet_wrap(~ station) +
  # stat_smooth(method = "lm", se = FALSE) +
  theme(strip.text = element_text(size = 12)) + 
  ylab(expression(paste("Annual duration of Dust (hours)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=10, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  ylim(0, 1200)
plot


# plot ###
output_folder <- "F:/Historical_DUST/plots/"
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots/"

png(paste0(output_folder,"Annual_Hours_of_dust_I_Method.png"), width = 2000, height = 1000,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()



# II method
plot <- ggplot(all_DUST_SUM, aes(DateTime, ANNUAL_SUM_II_meth)) +
  theme_bw() +
  geom_bar(stat="identity") +
  facet_wrap(~ station) +
  # stat_smooth(method = "lm", se = FALSE) +
  theme(strip.text = element_text(size = 12)) + 
  ylab(expression(paste("Annual duration of Dust (hours)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=10, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
ylim(0, 1200)
plot


# plot ###
output_folder <- "F:/Historical_DUST/plots/"
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots/"

png(paste0(output_folder,"Annual_Hours_of_dust_II_Method.png"), width = 2000, height = 1000,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()


##############################################################
# make and average between the outcome of the two methods

all_DUST_SUM_AVERAGE <- all_DUST_SUM %>%
  group_by(DateTime, station) %>%
  summarise(AVG_DUST = mean(ANNUAL_SUM_I_meth:ANNUAL_SUM_II_meth))


plot <- ggplot(all_DUST_SUM_AVERAGE, aes(DateTime, AVG_DUST)) +
  theme_bw() +
  geom_bar(stat="identity") +
  facet_wrap(~ station) +
  # stat_smooth(method = "lm", se = FALSE) +
  theme(strip.text = element_text(size = 12)) + 
  ylab(expression(paste("Annual duration of Dust (hours)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=10, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
ylim(0, 1000)
plot


# plot ###
output_folder <- "F:/Historical_DUST/plots/"
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots/"

png(paste0(output_folder,"Annual_Hours_of_dust_AVG_I_&_IIMethod.png"), width = 2000, height = 1000,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()




##########################################
# melt data from methodologies together


all_DUST_SUM <- all_DUST_SUM %>%
   mutate(YEAR = year(DateTime))


 all_DUST_SUM <- all_DUST_SUM %>%
   dplyr::select(-DateTime)

# str(all_DUST_SUM)


all_SUM_data <- melt(all_DUST_SUM, id=c("YEAR","station"))
str(all_SUM_data)

# rename variables
levels(all_SUM_data$variable) <- gsub("^ANNUAL_SUM_II_meth$","MeteoFrance_Meth", levels(all_SUM_data$variable))
levels(all_SUM_data$variable) <- gsub("^ANNUAL_SUM_I_meth$","EUMETSAT_Meth", levels(all_SUM_data$variable))


# Create a named character vector that relates factor levels to colors.
grays = c(MeteoFrance_Meth="red", EUMETSAT_Meth="blue")

plot <- ggplot(all_SUM_data, aes(YEAR, value, fill = variable)) +
  theme_bw() +
  geom_bar(stat="identity", width = 1, position = "dodge") +
  scale_fill_manual(values=grays) +
  facet_wrap(~ station) +
  theme(strip.text = element_text(size = 12)) + 
  ylab(expression(paste("Annual duration of Dust (hours)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=10, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) +
# scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) 
 ylim(0, 1200)
plot

# save plot
ggsave(plot=plot, filename=paste0("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots/Annual_Hours_of_dust_I_and_II_Method.png"), height=5, width=10)



#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
