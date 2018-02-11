#Map Supplements
library(rgeos)
library(dplyr)
library(leaflet)

locations <- read.csv("data/locations.csv")
jcomm <- filter(locations, Type == "Institution")
xyz <- jcomm[, c(4,3)]
location_dot <- SpatialPointsDataFrame(coords = xyz, data = jcomm, 
                                       proj4string = CRS("+proj=longlat +
                                                         datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
location_dot <- spTransform(location_dot, CRS("+proj=utm +zone=18 +datum=NAD27"))
buffer <- gBuffer(location_dot, width = 804.672)  
buffer <- spTransform(buffer, CRS("+proj=longlat + datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
buffer2 <- gBuffer(location_dot, width = 1609.34)  
buffer2 <- spTransform(buffer2, CRS("+proj=longlat + datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


#icons for map

icon1 <- awesomeIcons(
  icon = 'ios-people',
  iconColor = 'black',
  library = 'ion',
  markerColor = "blue"
)

icon2 <- awesomeIcons(
  icon = 'location',
  iconColor = 'black',
  library = 'ion',
  markerColor = "purple"
)

icon3 <- awesomeIcons(
  icon = 'leaf',
  iconColor = 'black',
  library = 'ion',
  markerColor = "green"
)
icon4 <- awesomeIcons(
  icon = 'star',
  iconColor = 'black',
  library = 'ion',
  markerColor = "red"
)

icon5 <- awesomeIcons(
  icon = 'home',
  iconColor = 'black',
  library = 'ion',
  markerColor = "pink"
)