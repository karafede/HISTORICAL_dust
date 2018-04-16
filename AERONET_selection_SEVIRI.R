
library(readr)
library(stringr)
library(lubridate)
library(dplyr)
library(ggplot2)


setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/AERONET_L2")


# 2012-------------------------------------------------------------------------
AERONET_2012_MASDAR <- read.csv("20120101_20121231_Masdar_Institute.csv")
AERONET_2012_MASDAR <- AERONET_2012_MASDAR %>%
  mutate(Date = mdy(date))

AERONET_2012_MASDAR <- AERONET_2012_MASDAR %>%
  select(Date,
         time,
         AOD_500nm)


# extract hours
AERONET_2012_MASDAR$hour <- as.numeric(str_sub(AERONET_2012_MASDAR$time, start = 1, end = -7))
AERONET_2012_MASDAR$hour <- sprintf("%02d", AERONET_2012_MASDAR$hour)


# 2013-------------------------------------------------------------------------
AERONET_2013_MASDAR <- read.csv("20130101_20131231_Masdar_Institute.csv")
AERONET_2013_MASDAR <- AERONET_2013_MASDAR %>%
  mutate(Date = mdy(date))

AERONET_2013_MASDAR <- AERONET_2013_MASDAR %>%
  select(Date,
         time,
         AOD_500nm)


# extract hours
AERONET_2013_MASDAR$hour <- as.numeric(str_sub(AERONET_2013_MASDAR$time, start = 1, end = -7))
AERONET_2013_MASDAR$hour <- sprintf("%02d", AERONET_2013_MASDAR$hour)



# 2014-------------------------------------------------------------------------
AERONET_2014_MASDAR <- read.csv("20140101_20141231_Masdar_Institute.csv")
AERONET_2014_MASDAR <- AERONET_2014_MASDAR %>%
  mutate(Date = mdy(date))

AERONET_2014_MASDAR <- AERONET_2014_MASDAR %>%
  select(Date,
         time,
         AOD_500nm)


# extract hours
AERONET_2014_MASDAR$hour <- as.numeric(str_sub(AERONET_2014_MASDAR$time, start = 1, end = -7))
AERONET_2014_MASDAR$hour <- sprintf("%02d", AERONET_2014_MASDAR$hour)


# 2015-------------------------------------------------------------------------
AERONET_2015_MASDAR <- read.csv("20150101_20151231_Masdar_Institute.csv")
AERONET_2015_MASDAR <- AERONET_2015_MASDAR %>%
  mutate(Date = mdy(date))

AERONET_2015_MASDAR <- AERONET_2015_MASDAR %>%
  select(Date,
         time,
         AOD_500nm)


# extract hours
AERONET_2015_MASDAR$hour <- as.numeric(str_sub(AERONET_2015_MASDAR$time, start = 1, end = -7))
AERONET_2015_MASDAR$hour <- sprintf("%02d", AERONET_2015_MASDAR$hour)


# 2016-------------------------------------------------------------------------
AERONET_2016_MASDAR <- read.csv("20160101_20161231_Masdar_Institute.csv")
AERONET_2016_MASDAR <- AERONET_2016_MASDAR %>%
  mutate(Date = mdy(date))

AERONET_2016_MASDAR <- AERONET_2016_MASDAR %>%
  select(Date,
         time,
         AOD_500nm)


# extract hours
AERONET_2016_MASDAR$hour <- as.numeric(str_sub(AERONET_2016_MASDAR$time, start = 1, end = -7))
AERONET_2016_MASDAR$hour <- sprintf("%02d", AERONET_2016_MASDAR$hour)


# 2017-------------------------------------------------------------------------
AERONET_2017_MASDAR <- read.csv("20170101_20171231_Masdar_Institute.csv")
AERONET_2017_MASDAR <- AERONET_2017_MASDAR %>%
  mutate(Date = mdy(date))

AERONET_2017_MASDAR <- AERONET_2017_MASDAR %>%
  select(Date,
         time,
         AOD_500nm)


# extract hours
AERONET_2017_MASDAR$hour <- as.numeric(str_sub(AERONET_2017_MASDAR$time, start = 1, end = -7))
AERONET_2017_MASDAR$hour <- sprintf("%02d", AERONET_2017_MASDAR$hour)



# bind all years togehter
AERONET_MASDAR <- rbind(AERONET_2012_MASDAR,
                        AERONET_2013_MASDAR,
                        AERONET_2014_MASDAR,
                        AERONET_2015_MASDAR,
                        AERONET_2016_MASDAR,
                        AERONET_2017_MASDAR)

AERONET_MASDAR <- AERONET_MASDAR %>%
  filter(!AOD_500nm == -999.000000)

write.csv(AERONET_MASDAR, "AERONET_MASDAR.csv")


############################################
## MEZAIRA AERONET #########################

# 2004-------------------------------------------------------------------------
AERONET_2004_MEZAIRA <- read.csv("20040101_20041231_Mezaira.csv")
AERONET_2004_MEZAIRA <- AERONET_2004_MEZAIRA %>%
  mutate(Date = mdy(date))

AERONET_2004_MEZAIRA <- AERONET_2004_MEZAIRA %>%
  select(Date,
         time,
         AOD_500nm)


# extract hours
AERONET_2004_MEZAIRA$hour <- as.numeric(str_sub(AERONET_2004_MEZAIRA$time, start = 1, end = -7))
AERONET_2004_MEZAIRA$hour <- sprintf("%02d", AERONET_2004_MEZAIRA$hour)

# 2007-------------------------------------------------------------------------
AERONET_2007_MEZAIRA <- read.csv("20070101_20071231_Mezaira.csv")
AERONET_2007_MEZAIRA <- AERONET_2007_MEZAIRA %>%
  mutate(Date = mdy(date))

AERONET_2007_MEZAIRA <- AERONET_2007_MEZAIRA %>%
  select(Date,
         time,
         AOD_500nm)


# extract hours
AERONET_2007_MEZAIRA$hour <- as.numeric(str_sub(AERONET_2007_MEZAIRA$time, start = 1, end = -7))
AERONET_2007_MEZAIRA$hour <- sprintf("%02d", AERONET_2007_MEZAIRA$hour)


# 2008-------------------------------------------------------------------------
AERONET_2008_MEZAIRA <- read.csv("20080101_20081231_Mezaira.csv")
AERONET_2008_MEZAIRA <- AERONET_2008_MEZAIRA %>%
  mutate(Date = mdy(date))

AERONET_2008_MEZAIRA <- AERONET_2008_MEZAIRA %>%
  select(Date,
         time,
         AOD_500nm)


# extract hours
AERONET_2008_MEZAIRA$hour <- as.numeric(str_sub(AERONET_2008_MEZAIRA$time, start = 1, end = -7))
AERONET_2008_MEZAIRA$hour <- sprintf("%02d", AERONET_2008_MEZAIRA$hour)


# 2009 -------------------------------------------------------------------------
AERONET_2009_MEZAIRA <- read.csv("20090101_20091231_Mezaira.csv")
AERONET_2009_MEZAIRA <- AERONET_2009_MEZAIRA %>%
  mutate(Date = mdy(date))

AERONET_2009_MEZAIRA <- AERONET_2009_MEZAIRA %>%
  select(Date,
         time,
         AOD_500nm)


# extract hours
AERONET_2009_MEZAIRA$hour <- as.numeric(str_sub(AERONET_2009_MEZAIRA$time, start = 1, end = -7))
AERONET_2009_MEZAIRA$hour <- sprintf("%02d", AERONET_2009_MEZAIRA$hour)




# 2010 -------------------------------------------------------------------------
AERONET_2010_MEZAIRA <- read.csv("20100101_20101231_Mezaira.csv")
AERONET_2010_MEZAIRA <- AERONET_2010_MEZAIRA %>%
  mutate(Date = mdy(date))

AERONET_2010_MEZAIRA <- AERONET_2010_MEZAIRA %>%
  select(Date,
         time,
         AOD_500nm)


# extract hours
AERONET_2010_MEZAIRA$hour <- as.numeric(str_sub(AERONET_2010_MEZAIRA$time, start = 1, end = -7))
AERONET_2010_MEZAIRA$hour <- sprintf("%02d", AERONET_2010_MEZAIRA$hour)



# 2011 -------------------------------------------------------------------------
AERONET_2011_MEZAIRA <- read.csv("20110101_20111231_Mezaira.csv")
AERONET_2011_MEZAIRA <- AERONET_2011_MEZAIRA %>%
  mutate(Date = mdy(date))

AERONET_2011_MEZAIRA <- AERONET_2011_MEZAIRA %>%
  select(Date,
         time,
         AOD_500nm)


# extract hours
AERONET_2011_MEZAIRA$hour <- as.numeric(str_sub(AERONET_2011_MEZAIRA$time, start = 1, end = -7))
AERONET_2011_MEZAIRA$hour <- sprintf("%02d", AERONET_2011_MEZAIRA$hour)

# 2012 -------------------------------------------------------------------------
AERONET_2012_MEZAIRA <- read.csv("20120101_20121231_Mezaira.csv")
AERONET_2012_MEZAIRA <- AERONET_2012_MEZAIRA %>%
  mutate(Date = mdy(date))

AERONET_2012_MEZAIRA <- AERONET_2012_MEZAIRA %>%
  select(Date,
         time,
         AOD_500nm)


# extract hours
AERONET_2012_MEZAIRA$hour <- as.numeric(str_sub(AERONET_2012_MEZAIRA$time, start = 1, end = -7))
AERONET_2012_MEZAIRA$hour <- sprintf("%02d", AERONET_2012_MEZAIRA$hour)

# 2013 -------------------------------------------------------------------------
AERONET_2013_MEZAIRA <- read.csv("20130101_20131231_Mezaira.csv")
AERONET_2013_MEZAIRA <- AERONET_2013_MEZAIRA %>%
  mutate(Date = mdy(date))

AERONET_2013_MEZAIRA <- AERONET_2013_MEZAIRA %>%
  select(Date,
         time,
         AOD_500nm)


# extract hours
AERONET_2013_MEZAIRA$hour <- as.numeric(str_sub(AERONET_2013_MEZAIRA$time, start = 1, end = -7))
AERONET_2013_MEZAIRA$hour <- sprintf("%02d", AERONET_2013_MEZAIRA$hour)

# 2014 -------------------------------------------------------------------------
AERONET_2014_MEZAIRA <- read.csv("20140101_20141231_Mezaira.csv")
AERONET_2014_MEZAIRA <- AERONET_2014_MEZAIRA %>%
  mutate(Date = mdy(date))

AERONET_2014_MEZAIRA <- AERONET_2014_MEZAIRA %>%
  select(Date,
         time,
         AOD_500nm)


# extract hours
AERONET_2014_MEZAIRA$hour <- as.numeric(str_sub(AERONET_2014_MEZAIRA$time, start = 1, end = -7))
AERONET_2014_MEZAIRA$hour <- sprintf("%02d", AERONET_2014_MEZAIRA$hour)


# 2015 -------------------------------------------------------------------------
AERONET_2015_MEZAIRA <- read.csv("20150101_20151231_Mezaira.csv")
AERONET_2015_MEZAIRA <- AERONET_2015_MEZAIRA %>%
  mutate(Date = mdy(date))

AERONET_2015_MEZAIRA <- AERONET_2015_MEZAIRA %>%
  select(Date,
         time,
         AOD_500nm)


# extract hours
AERONET_2015_MEZAIRA$hour <- as.numeric(str_sub(AERONET_2015_MEZAIRA$time, start = 1, end = -7))
AERONET_2015_MEZAIRA$hour <- sprintf("%02d", AERONET_2015_MEZAIRA$hour)



# 2016 -------------------------------------------------------------------------
AERONET_2016_MEZAIRA <- read.csv("20160101_20161231_Mezaira.csv")
AERONET_2016_MEZAIRA <- AERONET_2016_MEZAIRA %>%
  mutate(Date = mdy(date))

AERONET_2016_MEZAIRA <- AERONET_2016_MEZAIRA %>%
  select(Date,
         time,
         AOD_500nm)


# extract hours
AERONET_2016_MEZAIRA$hour <- as.numeric(str_sub(AERONET_2016_MEZAIRA$time, start = 1, end = -7))
AERONET_2016_MEZAIRA$hour <- sprintf("%02d", AERONET_2016_MEZAIRA$hour)

# 2017 -------------------------------------------------------------------------
AERONET_2017_MEZAIRA <- read.csv("20170101_20171231_Mezaira.csv")
AERONET_2017_MEZAIRA <- AERONET_2017_MEZAIRA %>%
  mutate(Date = mdy(date))

AERONET_2017_MEZAIRA <- AERONET_2017_MEZAIRA %>%
  select(Date,
         time,
         AOD_500nm)


# extract hours
AERONET_2017_MEZAIRA$hour <- as.numeric(str_sub(AERONET_2017_MEZAIRA$time, start = 1, end = -7))
AERONET_2017_MEZAIRA$hour <- sprintf("%02d", AERONET_2017_MEZAIRA$hour)





# bind all years togehter
AERONET_MEZAIRA <- rbind(AERONET_2004_MEZAIRA,
                        AERONET_2007_MEZAIRA,
                        AERONET_2008_MEZAIRA,
                        AERONET_2009_MEZAIRA,
                        AERONET_2010_MEZAIRA,
                        AERONET_2011_MEZAIRA,
                        AERONET_2012_MEZAIRA,
                        AERONET_2013_MEZAIRA,
                        AERONET_2014_MEZAIRA,
                        AERONET_2015_MEZAIRA,
                        AERONET_2016_MEZAIRA,
                        AERONET_2017_MEZAIRA)

AERONET_MEZAIRA <- AERONET_MEZAIRA %>%
  filter(!AOD_500nm == -999.000000)

write.csv(AERONET_MEZAIRA, "AERONET_MEZAIRA.csv")


####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
################# OLD STUFF ########################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################

# load MODIS data exctracted at the location of the AERONET station in MASDAR


MODIS_2013_MASDAR <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/MODIS_LAADS_NASA/2013_MODIS_processed/csv/extracted_MASDAR.csv")
MODIS_2014_MASDAR <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/MODIS_LAADS_NASA/2014_MODIS_processed/csv/extracted_MASDAR.csv")
MODIS_2015_MASDAR <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/MODIS_LAADS_NASA/2015_MODIS_processed/csv/extracted_MASDAR.csv")
MODIS_2016_MASDAR <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/MODIS_LAADS_NASA/2016_MODIS_processed/csv/extracted_MASDAR.csv")

# bind all years together
MODIS_MASDAR <- rbind(MODIS_2013_MASDAR,
                      MODIS_2014_MASDAR,
                      MODIS_2015_MASDAR,
                      MODIS_2016_MASDAR)

# make date for of MODIS data as the one of AERONET
MODIS_MASDAR$Date <- as.Date(parse_date_time(MODIS_MASDAR$Date,"mdy"))



# select only AERONET station site
MODIS_MASDAR <- MODIS_MASDAR %>%
  filter(Site == "MI_AERONET")


##################################################################################
###################################################################################

# merge AERONET data with MODIS data
MODIS_AERONET <- MODIS_MASDAR %>%
  left_join(AERONET_MASDAR, "Date")

# remove all lines with NA
MODIS_AERONET <- na.omit(MODIS_AERONET)

## get the months of observations
MODIS_AERONET$month <- factor(format(MODIS_AERONET$Date, format = "%b"), levels = month.abb)

## Define seasons
MODIS_AERONET$season <- character(length = nrow(MODIS_AERONET))
MODIS_AERONET$season[MODIS_AERONET$month %in% month.abb[c(1:2)]] <- "winter"
MODIS_AERONET$season[MODIS_AERONET$month %in% month.abb[c(12)]] <- "winter"
MODIS_AERONET$season[MODIS_AERONET$month %in% month.abb[c(3:5)]] <- "spring"
MODIS_AERONET$season[MODIS_AERONET$month %in% month.abb[c(6:8)]] <- "summer"
MODIS_AERONET$season[MODIS_AERONET$month %in% month.abb[c(9:11)]] <- "fall"
MODIS_AERONET$season <- factor(MODIS_AERONET$season, levels = c("winter","spring","summer","fall"))

MODIS_AERONET <- MODIS_AERONET %>%
  select(Date,
         Value,
         AOT,
         season)
colnames(MODIS_AERONET) <- c("Date","MODIS_AOD", "AERONET_AOD", "season")


## correlation between MODIS and AERONET~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot(MODIS_AERONET$MODIS_AOD, MODIS_AERONET$AERONET_AOD)


# check your data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot <- ggplot(MODIS_AERONET, aes(season, MODIS_AOD)) +
  theme_bw() +
  geom_boxplot() + 
  guides(fill=FALSE) +   # no legend
  ylim(0, 2) 
plot


# check your data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot <- ggplot(MODIS_AERONET, aes(season, AERONET_AOD)) +
  theme_bw() +
  geom_boxplot() + 
  guides(fill=FALSE) +   # no legend
  ylim(0, 2) 
plot


#### fit function and label for AOD ----------------------------------------------
#### this funtion FORCE regression to pass through the origin #######################

lm_eqn <- function(df){
  m <- lm(MODIS_AOD ~ -1 + AERONET_AOD, df);
  eq <- substitute(italic(y) == b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(b = format(coef(m)[1], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}


# # this function includes the intercept~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# lm_eqn <- function(df){
#   m <- lm(MODIS_AOD ~  AERONET_AOD, df);
#   eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
#                    list(a = format(coef(m)[2], digits = 2),
#                         b = format(coef(m)[1], digits = 2),
#                         r2 = format(summary(m)$r.squared, digits = 3)))
#   as.character(as.expression(eq));
# }
#####################################################################################

# plot with regression line-----


jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/AERONET_MODIS/MODIS_vs_AERONET_MASDAR_2013_2016.jpg',    
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

library(plyr)

# define regression equation for each season
eq <- ddply(MODIS_AERONET, .(season),lm_eqn)


ggplot(MODIS_AERONET, aes(y=MODIS_AOD, x=AERONET_AOD)) +
  theme_bw() +
  geom_jitter(colour=alpha("black",0.2) ) +
  facet_grid(season ~ .) +
  theme(strip.text = element_text(size = 20)) + 
  scale_color_manual(values = c("#ff0000", "#0000ff", "#000000", "#ffb732")) + 
  geom_smooth(method = "lm", formula = y ~ -1 + x) +  # force fit through the origin
  ylab(expression("AOD (MODIS)")) +
  xlab(expression("AOD (AERONET)")) +
  ylim(c(0,2)) + 
  xlim(c(0,2)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=14)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=14)) +
  geom_text(data = eq, aes(x = 1.6, y = 0.25, label = V1),
            parse = TRUE, inherit.aes=FALSE, size = 6, color = "black" ) + 
  ggtitle("AOD-MODIS vs AERONET @ MASDAR") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5)) 
 

par(oldpar)
dev.off()

#############################################################################
