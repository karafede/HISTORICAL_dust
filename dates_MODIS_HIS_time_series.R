
###############
# MODIS TERRA #
###############

## UTC time ###################
# fix time at 07:15 UTC am ####


library(lubridate)
library(stringr)

# time <- Sys.time()
# year <- str_sub(time, start = 0, end = -16)
# month <- str_sub(time, start = 6, end = -13)
# day <- str_sub(time, start = 9, end = -10)
# start <- as.POSIXct(paste0(year,"-",month,"-", day, " ", "00:","00:","01"))


# 2004
start <- as.POSIXct("2004-01-01 07:15:01")
interval <- 1440 #minutes (1 day)
end <- start + as.difftime(366, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS_2004_a <- TS[1:49]
TS_2004_b <- TS[51:366]
TS_2004 <- c(TS_2004_a, TS_2004_b)


# 2005
start <- as.POSIXct("2005-01-01 07:15:01")
interval <- 1440 #minutes (1 day)
end <- start + as.difftime(365, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS_2005_a <- TS[1:205]
TS_2005_b <- TS[207:330]
TS_2005_c <- TS[332:365]
TS_2005 <- c(TS_2005_a, TS_2005_b, TS_2005_c)


# 2006
start <- as.POSIXct("2006-01-01 07:15:01")
interval <- 1440 #minutes (1 day)
end <- start + as.difftime(365, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS_2006_a <- TS[1:171]
TS_2006_b <- TS[173:234]
TS_2006_c <- TS[236:365]
TS_2006 <- c(TS_2006_a, TS_2006_b, TS_2006_c)

# 2007
start <- as.POSIXct("2007-01-01 07:15:01")
interval <- 1440 #minutes (1 day)
end <- start + as.difftime(365, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS_2007 <- TS[1:365]


# 2008
start <- as.POSIXct("2008-01-01 07:15:01")
interval <- 1440 #minutes (1 day)
end <- start + as.difftime(365, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS_2008_a <- TS[1:354]
TS_2008_b <- TS[359:366]
TS_2008 <- c(TS_2008_a, TS_2008_b)

# 2009
start <- as.POSIXct("2009-01-01 07:15:01")
interval <- 1440 #minutes (1 day)
end <- start + as.difftime(365, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS_2009 <- TS[1:365]

# 2010
start <- as.POSIXct("2010-01-01 07:15:01")
interval <- 1440 #minutes (1 day)
end <- start + as.difftime(365, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS_2010_a <- TS[1:176]
TS_2010_b <- TS[178:365]
TS_2010 <- c(TS_2010_a, TS_2010_b)


# 2011
start <- as.POSIXct("2011-01-01 07:15:01")
interval <- 1440 #minutes (1 day)
end <- start + as.difftime(365, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS_2011_a <- TS[1:170]
TS_2011_b <- TS[172:365]
TS_2011 <- c(TS_2011_a, TS_2011_b)


# 2012
start <- as.POSIXct("2012-01-01 07:15:01")
interval <- 1440 #minutes (1 day)
end <- start + as.difftime(366, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS_2012 <- TS[1:366]


# 2013
start <- as.POSIXct("2013-01-01 07:15:01")
interval <- 1440 #minutes (1 day)
end <- start + as.difftime(365, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS_2013 <- TS[1:365]



# 2014
start <- as.POSIXct("2014-01-01 07:15:01")
interval <- 1440 #minutes (1 day)
end <- start + as.difftime(365, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS_2014_a <- TS[1:298]
TS_2014_b <- TS[300:365]
TS_2014 <- c(TS_2014_a, TS_2014_b)


# 2015
start <- as.POSIXct("2015-01-01 07:15:01")
interval <- 1440 #minutes (1 day)
end <- start + as.difftime(365, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS_2015 <- TS[1:365]

# 2016
start <- as.POSIXct("2016-01-01 07:15:01")
interval <- 1440 #minutes (1 day)
end <- start + as.difftime(365, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS_2016_a <- TS[1:49]
TS_2016_b <- TS[59:366]
TS_2016 <- c(TS_2016_a, TS_2016_b)


# 2017
start <- as.POSIXct("2017-01-01 07:15:01")
interval <- 1440 #minutes (1 day)
end <- start + as.difftime(365, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS_2017_a <- TS[1:113]
TS_2017_b <- TS[115:217]
TS_2017_c <- TS[219:326]
TS_2017 <- c(TS_2017_a, TS_2017_b, TS_2017_c)

TS_TERRA <- c(TS_2004, TS_2005, TS_2006, TS_2007, TS_2008, TS_2009, TS_2010,
              TS_2011, TS_2012, TS_2013, TS_2014, TS_2015, TS_2016, TS_2017)

TS_TERRA <- as.data.frame(TS_TERRA)
write.csv(TS_TERRA, "dates_TERRA_2004_2017.csv")



###############
# MODIS AQUA ##
###############

## UTC time ###################
# fixt time at 09:30 UTC am ###


library(lubridate)
library(stringr)

# time <- Sys.time()
# year <- str_sub(time, start = 0, end = -16)
# month <- str_sub(time, start = 6, end = -13)
# day <- str_sub(time, start = 9, end = -10)
# start <- as.POSIXct(paste0(year,"-",month,"-", day, " ", "00:","00:","01"))


# 2004
start <- as.POSIXct("2004-01-01 09:30:01")
interval <- 1440 #minutes (1 day)
end <- start + as.difftime(366, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS_2004 <- TS[1:366]


# 2005
start <- as.POSIXct("2005-01-01 09:30:01")
interval <- 1440 #minutes (1 day)
end <- start + as.difftime(365, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS_2005 <- TS[1:365]


# 2006
start <- as.POSIXct("2006-01-01 09:30:01")
interval <- 1440 #minutes (1 day)
end <- start + as.difftime(365, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS_2006 <- TS[1:365]


# 2007
start <- as.POSIXct("2007-01-01 09:30:01")
interval <- 1440 #minutes (1 day)
end <- start + as.difftime(365, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS_2007 <- TS[1:365]


# 2008
start <- as.POSIXct("2008-01-01 09:30:01")
interval <- 1440 #minutes (1 day)
end <- start + as.difftime(366, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS_2008_a <- TS[1:101]
TS_2008_b <- TS[103:366]
TS_2008 <- c(TS_2008_a, TS_2008_b)


# 2009
start <- as.POSIXct("2009-01-01 09:30:01")
interval <- 1440 #minutes (1 day)
end <- start + as.difftime(365, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS_2009 <- TS[1:365]


# 2010
start <- as.POSIXct("2010-01-01 09:30:01")
interval <- 1440 #minutes (1 day)
end <- start + as.difftime(365, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS_2010 <- TS[1:365]


# 2011
start <- as.POSIXct("2011-01-01 09:30:01")
interval <- 1440 #minutes (1 day)
end <- start + as.difftime(365, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS_2011 <- TS[1:365]


# 2012
start <- as.POSIXct("2012-01-01 09:30:01")
interval <- 1440 #minutes (1 day)
end <- start + as.difftime(366, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS_2012 <- TS[1:366]


# 2013
start <- as.POSIXct("2013-01-01 09:30:01")
interval <- 1440 #minutes (1 day)
end <- start + as.difftime(365, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS_2013 <- TS[1:365]


# 2014
start <- as.POSIXct("2014-01-01 09:30:01")
interval <- 1440 #minutes (1 day)
end <- start + as.difftime(365, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS_2014 <- TS[1:365]


# 2015
start <- as.POSIXct("2015-01-01 09:30:01")
interval <- 1440 #minutes (1 day)
end <- start + as.difftime(365, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS_2015 <- TS[1:365]


# 2016
start <- as.POSIXct("2016-01-01 09:30:01")
interval <- 1440 #minutes (1 day)
end <- start + as.difftime(366, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS_2016 <- TS[1:366]

# 2017
start <- as.POSIXct("2017-01-01 09:30:01")
interval <- 1440 #minutes (1 day)
end <- start + as.difftime(326, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS_2017 <- TS[1:326]



TS_AQUA <- c(TS_2004, TS_2005, TS_2006, TS_2007, TS_2008, TS_2009, TS_2010,
              TS_2011, TS_2012, TS_2013, TS_2014, TS_2015, TS_2016, TS_2017)

TS_AQUA <- as.data.frame(TS_AQUA)
write.csv(TS_AQUA, "dates_AQUA_2004_2017.csv")




