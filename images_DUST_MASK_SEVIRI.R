

library(raster)


# load shape files UAE
# dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRFChem_domain"
# ### shapefile for UAE
# shp_UAE <- readOGR(dsn = dir, layer = "ADMIN_domain_d01_WRFChem_small")
# 
# # ----- Transform to EPSG 4326 - WGS84 (required)
# shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
# # names(shp)
# plot(shp_UAE)

dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/UAE_boundary"
### shapefile for UAE
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
# names(shp)
plot(shp_UAE)



# # sites_Airports_UAE 
sites_Airports_UAE <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/Airport_Locations_UAE_mapping.csv")
coordinates(sites_Airports_UAE) <- ~ Longitude + Latitude
# new data reference after 2011
reference <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/validation_RGB/EUMETSAT/Seviri_20150331_I_Method.tif")[[3]]
crs(sites_Airports_UAE) <- projection(reference)
plot(reference)




#########################################
# select raster of dust MASKS EUMETSAT ##
#########################################


setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/validation_RGB/EUMETSAT")

# load dust mask (96 images)
mask_EUMETSAT <- stack("Seviri_20150402_I_Method.tif")


# 7am UTC
April_02_06UTC <- raster("Seviri_20150402_I_Method.tif", band = 40)  # 12 UTC
plot(April_02_06UTC)
plot(shp_UAE, add = TRUE)


# # 10pm UTC
# April_02_10UTC <- raster("Seviri_20150402_I_Method.tif", band = 40)  # 12 UTC
# plot(April_02_12UTC)
# plot(shp_UAE, add = TRUE)


# 13pm UTC
April_02_13UTC <- raster("Seviri_20150402_I_Method.tif", band = 55)  # 12 UTC
plot(April_02_13UTC)
plot(shp_UAE, add = TRUE)


# 13pm UTC
April_03_07UTC <- raster("Seviri_20150403_I_Method.tif", band = 35)  # 12 UTC
plot(April_03_07UTC)
plot(shp_UAE, add = TRUE)




r <- stack(April_02_06UTC, April_02_13UTC, April_03_07UTC)
names <- c("2 April 2015 06:00 UTC", "2 April 2015 10:00 UTC", "3 April 2015 07:00 UTC")


vec_all <- as.vector(r)
max_val<- 1
min_val<- 0
low_IQR <- 0 
high_IQR <-1
cols <- colorRampPalette(c("white", "red"))

h <- rasterVis::levelplot(r, 
                          #    margin=FALSE, main= as.character(TITLE),
                          margin=FALSE, main= "(DUST MASKS - EUMETSAT)",
                          xlab = "",
                          ylab = "",
                          ## about colorbar
                          colorkey=list(
                            space='bottom',                   
                            labels= list(at= floor(as.numeric(seq(low_IQR, high_IQR, length.out=2))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title=expression(paste("        mask "))
                          ),   
                          ## about the axis
                          par.settings=list(
                            strip.border=list(col='transparent'),
                            strip.background=list(col='transparent'),
                            axis.line=list(col='black')
                          ),
                          scales=list(draw=T, alternating= F),            
                          #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
                          col.regions = cols,
                          # names.attr=as.character("2 April 2015 12:00 UTC")) +
                          names.attr= names, size = 5) +
                          # names.attr=as.character(seq(from = 2004, to = 2017, by= 1))) +
  latticeExtra::layer(sp.polygons(shp_UAE, col = "black", alpha = 1),
                      sp.points(sites_Airports_UAE, size=20, pch = 10, col = "blue"))
 h
 
 output_dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/validation_RGB/EUMETSAT"
 png(paste0(output_dir, "/", "masks_EUMETSAT.png"), width = 1500, height = 900,
     units = "px", pointsize = 100,
     bg = "white", res = 100)
 print(h)
 dev.off()
 
 ############################################
 # select raster of dust MASKS MeteoFrance ##
 ############################################
 
 
 setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/validation_RGB/MeteoFrance")
 
 # 7am UTC
 April_02_06UTC <- raster("Seviri_20150402_II_Method_METFr_Orig.tif", band = 31)  # 12 UTC
 plot(April_02_06UTC)
 plot(shp_UAE, add = TRUE)
 

 # 13pm UTC
 April_02_13UTC <- raster("Seviri_20150402_II_Method_METFr_Orig.tif", band = 49)  # 12 UTC
 plot(April_02_13UTC)
 plot(shp_UAE, add = TRUE)
 
 
 # 13pm UTC
 April_03_07UTC <- raster("Seviri_20150403_II_Method_METFr_Orig.tif", band = 35)  # 12 UTC
 plot(April_03_07UTC)
 plot(shp_UAE, add = TRUE)

 r <- stack(April_02_06UTC, April_02_13UTC, April_03_07UTC)
 names <- c("2 April 2015 06:00 UTC", "2 April 2015 10:00 UTC", "3 April 2015 07:00 UTC")
 
 ############
 # map ######
 ############
 
 vec_all <- as.vector(r)
 max_val<- 1
 min_val<- 0
 low_IQR <- 0 
 high_IQR <-1
 cols <- colorRampPalette(c("white", "red"))
 
 h <- rasterVis::levelplot(r, 
                           #    margin=FALSE, main= as.character(TITLE),
                           margin=FALSE, main= "(DUST MASKS - Meteo France)",
                           xlab = "",
                           ylab = "",
                           ## about colorbar
                           colorkey=list(
                             space='bottom',                   
                             labels= list(at= floor(as.numeric(seq(low_IQR, high_IQR, length.out=2))),
                                          font=3),
                             axis.line=list(col='black'),
                             width=0.75,
                             title=expression(paste("        mask "))
                           ),   
                           ## about the axis
                           par.settings=list(
                             strip.border=list(col='transparent'),
                             strip.background=list(col='transparent'),
                             axis.line=list(col='black')
                           ),
                           scales=list(draw=T, alternating= F),            
                           #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
                           col.regions = cols,
                           # names.attr=as.character("2 April 2015 12:00 UTC")) +
                           names.attr= names, size = 5) +
   # names.attr=as.character(seq(from = 2004, to = 2017, by= 1))) +
   latticeExtra::layer(sp.polygons(shp_UAE, col = "black", alpha = 1),
                       sp.points(sites_Airports_UAE, size=20, pch = 10, col = "blue"))
 h
 
 output_dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/validation_RGB/MeteoFrance"
 png(paste0(output_dir, "/", "masks_MeteoFrance.png"), width = 1500, height = 900,
     units = "px", pointsize = 100,
     bg = "white", res = 100)
 print(h)
 dev.off()
 
 
 
 
 