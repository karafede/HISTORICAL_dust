
library(rgdal)
library(raster)
library(leaflet)
library(htmlwidgets)
library(readr)

#load METAR sites
sites_Airports_UAE <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/Airport_Locations_UAE_mapping.csv")
colnames(sites_Airports_UAE) <- c("station", "latitude", "longitude")



#########################################################################
#########################################################################

#### importing the UAE shapefile to use as a masking 
dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/website_MODIS/UAE_boundary"
### shapefile for UAE
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp_UAE <- spTransform(shp_UAE, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# names(shp)

shp_UAE@data$name <- 1:nrow(shp_UAE)
plot(shp_UAE)

########################################################################
########################################################################

map <- leaflet(shp_UAE) %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
  
  addPolygons(stroke = TRUE, smoothFactor = 1, fillOpacity = 0,
              weight = 0.7, color = "#000000",
              group = "shp_UAE") %>%
  
  addCircleMarkers(data = sites_Airports_UAE,
                   lng = ~ longitude, lat = ~ latitude,
                   radius = 10, stroke = FALSE, fillOpacity = 1, popup = ~ station,
                   color = "black", 
                   group = "sites_NCMS_selected") %>%

  
  addLayersControl(
    baseGroups = c("Toner Lite", "Road map", "Topographical", "Satellite"),
    overlayGroups = c("sites_NCMS_selected",  "sites_NCMS"),
    options = layersControlOptions(collapsed = TRUE)) # %>%
 # hideGroup(c("sites_NCMS_selected")) 

map



# save map
saveWidget(map, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/HISTORICAL_dust/plots/METAR_stations.html", selfcontained = FALSE)


##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
