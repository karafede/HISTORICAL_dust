
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

# add the field of the year
all_DUST_SUM <- all_DUST_SUM %>%
  mutate(YEAR = year(DateTime))


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
  theme(strip.text = element_text(size = 10)) + 
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
  theme(strip.text = element_text(size = 10)) +
  ggtitle("SEVIRI - EUMETSAT") +
  ylab(expression(paste("Annual duration of Dust (hours)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=10, colour = "black", face="bold"),
        plot.title = element_text(color="red", size=14, face="bold.italic", hjust = 0.5)) +
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
  theme(strip.text = element_text(size = 10)) + 
  ggtitle("SEVIRI - MeteoFrance") +
  ylab(expression(paste("Annual duration of Dust (hours)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=10, colour = "black", face="bold"),
        plot.title = element_text(color="blue", size=14, face="bold.italic", hjust = 0.5)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
ylim(0, 800)
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
  theme(strip.text = element_text(size = 10)) + 
  ggtitle("SEVIRI - MeteoFrance & EUMETSAT") +
  ylab(expression(paste("Annual duration of Dust (hours)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=10, colour = "black", face="bold"),
        plot.title = element_text(color="black", size=14, face="bold.italic", hjust = 0.5)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  ylim(0, 750)
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
# all_SUM_data$YEAR <- as.Date(all_SUM_data$YEAR, origin="2004")

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
  theme(strip.text = element_text(size = 10)) + 
  ylab(expression(paste("Annual duration of Dust (hours)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=10, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) +
  # scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  ylim(0, 1200)
plot

# save plot
ggsave(plot=plot, filename=paste0("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots/Annual_Hours_of_dust_I_and_II_Method.png"), height=5, width=10)



###############################################################
# load METAAR data (sum dust)
METAR_DATA_YEAR <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/METAR_data/METAR_DUST_VIS_YEAR.csv")

# join METAR data and SEVIRI data by YEAR and by STATION
all_DUST_SUM <- all_DUST_SUM %>%
  left_join(METAR_DATA_YEAR, by = c("YEAR", "station"))

# remove all Na rows from DAILY_SUM_EUMETSAT
all_DUST_SUM <- all_DUST_SUM[!is.na(all_DUST_SUM$ANNUAL_SUM),]

##########################################
# melt data with SEVIRI data

all_DUST_SUM <- all_DUST_SUM %>%
  dplyr::select(-Date,
                -X,
                - MAX_VIS,
                - MIN_VIS)

all_SUM_data <- melt(all_DUST_SUM, id=c("YEAR","station"))
str(all_SUM_data)

# rename variables
levels(all_SUM_data$variable) <- gsub("^ANNUAL_SUM_II_meth$","MeteoFrance_Meth", levels(all_SUM_data$variable))
levels(all_SUM_data$variable) <- gsub("^ANNUAL_SUM_I_meth$","EUMETSAT_Meth", levels(all_SUM_data$variable))
levels(all_SUM_data$variable) <- gsub("^ANNUAL_SUM$","METAR", levels(all_SUM_data$variable))

# Create a named character vector that relates factor levels to colors.
grays = c(MeteoFrance_Meth="red", EUMETSAT_Meth="blue", METAR="grey")

plot <- ggplot(all_SUM_data, aes(YEAR, value, fill = variable)) +
  theme_bw() +
  geom_bar(stat="identity", width = 1, position = "dodge") +
  scale_fill_manual(values=grays) +
  facet_wrap(~ station) +
  theme( strip.text = element_text(size = 12)) + 
  theme(strip.text = element_text(size = 10)) + 
  ylab(expression(paste("Annual duration of Dust (hours)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=10, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) +
  # scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  ylim(0, 1200)
plot


# save plot
ggsave(plot=plot, filename=paste0("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots/Annual_Hours_of_dust_SEVIRI_and_METAR.png"), height=5, width=10)

###############################################################
###############################################################

# make a correlation between SEVIRI I and II method (MeteoFrance and EUMETSAT)
library(plyr)

######## STATISTICS #####################################################
####### equation ########################################################

## this function includes the intercept~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# lm_eqn <- function(df){
#   m <- lm(ANNUAL_SUM_II_meth ~  ANNUAL_SUM_I_meth, df);
#   eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
#                    list(a = format(coef(m)[2], digits = 2),
#                         b = format(coef(m)[1], digits = 2),
#                         r2 = format(summary(m)$r.squared, digits = 3)))
#   as.character(as.expression(eq));
# }

#### this funtion FORCE regression to pass through the origin ###########
lm_eqn <- function(df){
  m <- lm(ANNUAL_SUM_II_meth ~ -1 + ANNUAL_SUM_I_meth, df);
  eq <- substitute(italic(y) == b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(b = format(coef(m)[1], digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

# define regression equation for each season
eq_DUST <- ddply(all_DUST_SUM, .(station),lm_eqn)


# plot correlation 

plot <- ggplot(all_DUST_SUM, aes(x = ANNUAL_SUM_I_meth, y=ANNUAL_SUM_II_meth)) +
  theme_bw() +
  geom_point(size = 2, color='black') +    # Use hollow circles
  geom_smooth(method=lm, formula=y~x-1, fill=NA) +  # Add linear regression line  (force to origin)
# geom_smooth(method=lm, formula=y~x, fill=NA) +  
  facet_wrap( ~ station) +
  theme( strip.text = element_text(size = 12)) + 
  ylab(expression(paste("Hours of Dust (Meteo France)"))) +
  xlab(expression(paste("Hours of Dust (EUMETSAT)"))) +
  ylim(c(0, 700)) +
  xlim(c(0, 1200)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=12)) +
  geom_text(data = eq_DUST, aes(x = 600, y = 600, label = V1),
            parse = TRUE, inherit.aes=FALSE, size = 4, color = "red" )
plot

# save plot
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots/"

png(paste0(output_folder,"Correlation_MetFrance_vs_EUMETSAT.jpg"),
    width = 1600, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()

#####################################################################
# correlate SEVIRI DUST data (EUMETSAT) with DUST events from METAR

all_DUST_SUM_clean <- all_DUST_SUM[!is.na(all_DUST_SUM$ANNUAL_SUM),]


lm_eqn <- function(df){
  m <- lm(ANNUAL_SUM ~ -1 + ANNUAL_SUM_I_meth, df);
  eq <- substitute(italic(y) == b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(b = format(coef(m)[1], digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

## this function includes the intercept~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# lm_eqn <- function(df){
#   m <- lm(ANNUAL_SUM ~  ANNUAL_SUM_I_meth, df);
#   eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
#                    list(a = format(coef(m)[2], digits = 2),
#                         b = format(coef(m)[1], digits = 2),
#                         r2 = format(summary(m)$r.squared, digits = 3)))
#   as.character(as.expression(eq));
# }

# define regression equation for each season
eq_DUST <- ddply(all_DUST_SUM_clean, .(station),lm_eqn)


# plot correlation 

plot <- ggplot(all_DUST_SUM_clean, aes(x = ANNUAL_SUM_I_meth, y=ANNUAL_SUM)) +
  theme_bw() +
  geom_point(size = 2, color='black') +    # Use hollow circles
  geom_smooth(method=lm, formula=y~x-1, fill=NA) +  # Add linear regression line  (force to origin)
  facet_wrap( ~ station) +
  theme( strip.text = element_text(size = 12)) + 
  ylab(expression(paste("Hours of Dust (METAR)"))) +
  xlab(expression(paste("Hours of Dust (EUMETSAT)"))) +
  ylim(c(0, 1000)) +
  xlim(c(0, 800)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=12)) +
  geom_text(data = eq_DUST, aes(x = 600, y = 800, label = V1),
            parse = TRUE, inherit.aes=FALSE, size = 4, color = "red" )
plot

# save plot
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots/"

png(paste0(output_folder,"Correlation_METAAR_vs_EUMETSAT.jpg"),
    width = 1600, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()


#####################################################################
# correlate SEVIRI DUST data (MeteoFrance) with DUST events from METAR

lm_eqn <- function(df){
  m <- lm(ANNUAL_SUM ~ -1 + ANNUAL_SUM_II_meth, df);
  eq <- substitute(italic(y) == b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(b = format(coef(m)[1], digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

# define regression equation for each season
eq_DUST <- ddply(all_DUST_SUM_clean, .(station),lm_eqn)


# plot correlation 

plot <- ggplot(all_DUST_SUM_clean, aes(x = ANNUAL_SUM_II_meth, y=ANNUAL_SUM)) +
  theme_bw() +
  geom_point(size = 2, color='black') +    # Use hollow circles
  geom_smooth(method=lm, formula=y~x-1, fill=NA) +  # Add linear regression line  (force to origin)
  facet_wrap( ~ station) +
  theme( strip.text = element_text(size = 12)) + 
  ylab(expression(paste("Hours of Dust (METAR)"))) +
  xlab(expression(paste("Hours of Dust (Meteo France)"))) +
  ylim(c(0, 1000)) +
  xlim(c(0, 800)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=12)) +
  geom_text(data = eq_DUST, aes(x = 600, y = 800, label = V1),
            parse = TRUE, inherit.aes=FALSE, size = 4, color = "red" )
plot

# save plot
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots/"

png(paste0(output_folder,"Correlation_METAAR_vs_MeteoFrance.jpg"),
    width = 1600, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()



#####################################################################
# correlate SEVIRI DUST data (EUMETSAT) with VISIBILITY events from METAR

## this function includes the intercept~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lm_eqn <- function(df){
  m <- lm(MIN_VIS ~  ANNUAL_SUM_I_meth, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(a = format(coef(m)[2], digits = 2),
                        b = format(coef(m)[1], digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}


# define regression equation for each season
eq_DUST <- ddply(all_DUST_SUM_clean, .(station),lm_eqn)


# plot correlation 

plot <- ggplot(all_DUST_SUM_clean, aes(x = ANNUAL_SUM_I_meth, y=MIN_VIS)) +
  theme_bw() +
  geom_point(size = 2, color='black') +    # Use hollow circles
  geom_smooth(method=lm, formula=y~x, fill=NA) +  # Add linear regression line  (force to origin)
  facet_wrap( ~ station) +
  theme( strip.text = element_text(size = 12)) + 
  ylab(expression(paste("METAR (minimum visibility - km)"))) +
  xlab(expression(paste("Hours of DUST (EUMETSAT)"))) +
  ylim(c(-1, 5)) +
  xlim(c(0, 600)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=12)) +
  geom_text(data = eq_DUST, aes(x = 300, y = 4, label = V1),
            parse = TRUE, inherit.aes=FALSE, size = 4, color = "red" )
plot

# save plot
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots/"

png(paste0(output_folder,"Correlation_MIN_VIS_METAAR_vs_DUST_EUMETSAT.jpg"),
    width = 1600, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()



#######################################################################
# correlate SEVIRI DUST data (MeteoFrance) VISIBILITY events from METAR


## this function includes the intercept~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lm_eqn <- function(df){
  m <- lm(MIN_VIS ~  ANNUAL_SUM_II_meth, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(a = format(coef(m)[2], digits = 2),
                        b = format(coef(m)[1], digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}


# define regression equation for each season
eq_DUST <- ddply(all_DUST_SUM_clean, .(station),lm_eqn)

# plot correlation 

plot <- ggplot(all_DUST_SUM_clean, aes(x = ANNUAL_SUM_II_meth, y=MIN_VIS)) +
  theme_bw() +
  geom_point(size = 2, color='black') +    # Use hollow circles
  geom_smooth(method=lm, formula=y~x, fill=NA) +  # Add linear regression line  (force to origin)
  facet_wrap( ~ station) +
  theme( strip.text = element_text(size = 12)) + 
  ylab(expression(paste("METAR (minimum visibility - km)"))) +
  xlab(expression(paste("Hours of DUST (Meteo France)"))) +
  ylim(c(-1, 5)) +
  xlim(c(0, 800)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=12)) +
  geom_text(data = eq_DUST, aes(x = 300, y = 4, label = V1),
            parse = TRUE, inherit.aes=FALSE, size = 4, color = "red" )
plot

# save plot
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots/"

png(paste0(output_folder,"Correlation_MIN_VIS_METAAR_vs_DUST_MeteoFrance.jpg"),
    width = 1600, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()


###################################################################################
# use averatges DUST event from MeteFrance and EUMETSAT data ######################

all_DUST_SUM_AVERAGE <- all_DUST_SUM_AVERAGE %>%
  mutate(YEAR = year(DateTime))

all_DUST_SUM_AVERAGE <- all_DUST_SUM_AVERAGE %>%
  left_join(METAR_DATA_YEAR, by = c("YEAR", "station"), na.rm = TRUE)

all_DUST_SUM_AVERAGE <- all_DUST_SUM_AVERAGE[!is.na(all_DUST_SUM_AVERAGE$ANNUAL_SUM),]

#####################################################################
# correlate SEVIRI DUST data (MeteoFrance and EUMETESAT) with DUST events from METAAR

lm_eqn <- function(df){
  m <- lm(ANNUAL_SUM ~ -1 + AVG_DUST, df);
  eq <- substitute(italic(y) == b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(b = format(coef(m)[1], digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

# define regression equation for each season
eq_DUST <- ddply(all_DUST_SUM_AVERAGE, .(station),lm_eqn)


# plot correlation 

plot <- ggplot(all_DUST_SUM_AVERAGE, aes(x = AVG_DUST, y=ANNUAL_SUM)) +
  theme_bw() +
  geom_point(size = 2, color='black') +    # Use hollow circles
  geom_smooth(method=lm, formula=y~x-1, fill=NA) +  # Add linear regression line  (force to origin)
  facet_wrap( ~ station) +
  theme( strip.text = element_text(size = 12)) + 
  ylab(expression(paste("Hours of Dust (METAR)"))) +
  xlab(expression(paste("Hours of Dust (Meteo France & EUMETSAT)"))) +
  ylim(c(0, 1000)) +
  xlim(c(0, 800)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=12)) +
  geom_text(data = eq_DUST, aes(x = 600, y = 800, label = V1),
            parse = TRUE, inherit.aes=FALSE, size = 4, color = "red" )
plot

# save plot
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots/"

png(paste0(output_folder,"Correlation_METAAR_vs_MeteoFrance_EUMETSAT.jpg"),
    width = 1600, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()


###################################################################################
###################################################################################
# correlate SEVIRI DUST data (MeteoFrance & EUMETSAT) VISIBILITY events from METAR


## this function includes the intercept~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lm_eqn <- function(df){
  m <- lm(MIN_VIS ~  AVG_DUST, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(a = format(coef(m)[2], digits = 2),
                        b = format(coef(m)[1], digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}


# define regression equation for each season
eq_DUST <- ddply(all_DUST_SUM_AVERAGE, .(station),lm_eqn)

# plot correlation 

plot <- ggplot(all_DUST_SUM_AVERAGE, aes(x = AVG_DUST, y=MIN_VIS)) +
  theme_bw() +
  geom_point(size = 2, color='black') +    # Use hollow circles
  geom_smooth(method=lm, formula=y~x, fill=NA) +  # Add linear regression line  (force to origin)
  facet_wrap( ~ station) +
  theme( strip.text = element_text(size = 12)) + 
  ylab(expression(paste("METAR (minimum visibility - km)"))) +
  xlab(expression(paste("Hours of DUST (Meteo France & EUMETSAT)"))) +
  ylim(c(-1, 5)) +
  xlim(c(0, 800)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=12)) +
  geom_text(data = eq_DUST, aes(x = 300, y = 4, label = V1),
            parse = TRUE, inherit.aes=FALSE, size = 4, color = "red" )
plot

# save plot
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots/"

png(paste0(output_folder,"Correlation_MIN_VIS_METAAR_vs_DUST_MeteoFrance_and_EUMETSAT.jpg"),
    width = 1600, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()


#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
