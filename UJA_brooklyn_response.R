#############################################################################
# Program Name:   UJA_brooklyn_response.R
# Location:       /Users/zongyangli/Documents/Github/NYC-demographic/UJA_brooklyn_response.R
# Author:         
# Date Created:   
# Project:        
# Purpose:        
#############################################################################



########################################################################
## Prepare  ##

source("/Users/zongyangli/Documents/Github/R-Key-functions/Start up.R")

# load packages 
package_list <- c("rnaturalearth", "tmap", "osmar", "OpenStreetMap", "ggmap", "tmaptools")
new_packages <- package_list[package_list %notin% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)

library(rnaturalearth)
library(tmap)
library(osmar)
library(OpenStreetMap)
library(ggmap)
library(tmaptools)
library(rgdal)
library(broom)
library(rgeos)

library(readxl)
library(Cairo) #font

# load Shape Files
tract_origin <- readOGR(dsn = path.expand("/Users/zongyangli/Google Drive/Job/RA - Public Good/Data/Shape files/tract_shape_files"), layer = "nyct2010")
tract <- readOGR(dsn = path.expand("/Users/zongyangli/Google Drive/Job/RA - Public Good/Data/Shape files/zip_shape_files"))
zip_focus <- c(11215, 10002, 11201, 11204, 11205, 11207, 11209, 11210, 11211, 11213, 11214, 11215, 11216, 11217, 11218, 11220, 11221, 11222, 11223, 11225, 11226, 11230, 11231, 11233, 11234, 11235, 11236, 11238, 11249)
zip_focus <- c(11213, 11226, 11225, 11218, 11231, 11216, 11217, 11201, 11238, 11215)


##################################################
# Geocode crosswalk
##################################################

uja_brooklyn_response <- read.csv("/Users/zongyangli/Google Drive/Job/RA - Public Good/Projects/UJA_Brooklyn/Survey/brooklyn_response_zip.csv") %>% mutate(zip = as.character(zip))

########################################################################
## ZIP to Tract ##

    zip_tract <- read_excel('/Users/zongyangli/Google Drive/Job/RA - Public Good/Data/ZIP_TRACT_062017.xlsx') %>% mutate(zip = as.numeric(ZIP), tract = as.numeric(TRACT))

    # merge
    uja_brooklyn_response_full <- uja_brooklyn_response %>%
      left_join(zip_tract[,c('zip','tract')], by = 'zip') %>%
      mutate(CT2010 = substr(tract,6,11))


########################################################################
## Merge with meta data (tract to puma) ##

meta_origin <- tract_origin@data
colnames(meta_origin)[9] <- "puma"

meta_origin$CT2010 <- as.character(meta_origin$CT2010)
meta_origin <- meta_origin %>% left_join(uja_brooklyn_response_full[,c('CT2010','zip','frequency')], by = "CT2010") 

########################################################################
## Final Output  ##

uja_brooklyn_response_final <- meta_origin %>%
  filter(!is.na(zip)) %>%
  select(zip,puma,BoroName,NTAName,frequency) %>%
  distinct(zip,puma,BoroName,NTAName,frequency) # from tract to puma unique level

    setwd('/Users/zongyangli/Google Drive/Job/RA - Public Good/Projects/UJA_Brooklyn/Survey/')
    write.csv(uja_brooklyn_response_final,'uja_brooklyn_response_final.csv',row.names = F)



##################################################
# Mapping
##################################################

########################################################################
## Merge with meta data (tract to puma) ##

meta <- tract@data
colnames(meta)[1] <- "zip"

as.numeric.factor <- function(x) {as.character(levels(x))[x]}
meta$zip <- as.numeric.factor(meta$zip)

meta <- meta %>% left_join(uja_brooklyn_response[,c('zip','frequency','neighborhood')], by = "zip") 
meta <- Reduce(function(x, y) merge(x, y, by='zip', all.x = T), list(meta,uja_brooklyn_response))
tract@data <- meta

setwd('/Users/zongyangli/Google Drive/Job/RA - Public Good/Data/Shape files/')
dir.create("tempdir")
writeOGR(obj=tract, dsn="tempdir", layer="tract", driver="ESRI Shapefile")
########################################################################
## Make Maps  ##

tm_plot <- function(tract,var){
      tm_shape(tract[tract$zip %in% zip_focus,]) + tm_borders(col = "black") +       
      # tm_shape(tract[tract$CTY_FIPS == '047',]) + tm_borders(col = "black") +       
      tm_polygons(col = var, palette = "Blues", alpha = .9) + 
      tm_layout(legend.text.size=0.8,
                legend.title.size=.001,
                legend.position = c("left","top"), 
                legend.bg.color = "gray100", 
                legend.bg.alpha=.2, 
                legend.frame='gray100', 
                legend.height= 2,
                legend.width = 2,
                legend.hist.width=.15,
                legend.hist.height=.15, 
                legend.hist.bg.color="gray60", 
                legend.hist.bg.alpha=.5) +
      # tm_shape(tract[tract$zip == 11212,]) +
      # tm_fill(col = "gray85", alpha = .6) + tm_borders(col = "black") +      
      # tm_shape(tract[tract$zip == 11203,]) +
      # tm_fill(col = "gray85", alpha = .6) + tm_borders(col = "black") +
      # tm_shape(tract[tract$zip == 11228,]) +
      # tm_fill(col = "gray85", alpha = .6) + tm_borders(col = "black") +
      # tm_shape(tract[tract$zip == 11229,]) +
      # tm_fill(col = "gray85", alpha = .6) + tm_borders(col = "black") +
      # tm_shape(tract[tract$zip == 11219, ]) +
      # tm_fill(col = "gray85", alpha = .6) + tm_borders(col = "black") +
      tm_layout(main.title = NA, 
                main.title.size = 1.8, fontfamily = "sans", main.title.position = 0.13)
}

tm_plot(tract,'frequency') # (8.71*5.54 inch)
tm_plot(tract,'tot_hh') 



############################################################
############################################################
####################    END PROGRAM    #####################
############################################################
############################################################





########################################################################
## Some previous attempts  ##

# Some attempts

NLD <- read_osm(bb("Barclays Center, Brooklyn , New York"), ext = 55, type = "stamen-terrain") # Read Open Street Map data      
brooklyn_osm <- read_osm(london, type = "stamen-watercolor", zoom = 13)
  ## somehow the OpenStreetMap package can't be download

regions <- ne_download(scale = "large", type = "states", category = "cultural") # download data from Natural Earth 
london <- regions[which(regions$woe_name == "Greater London"),]
tm_shape(brooklyn)

# PARKS
meta[693, 14] <- 2100
meta[447, 14] <- 2100
tract@data <- meta


#2015 Regular


tm_shape(tract[tract$tract %in% tracts, ]) + tm_borders() + 
  tm_polygons("fam_households_2015", palette = "YlOrRd", alpha = .9, labels = NULL) +
  tm_legend(text.size=1,
            title.size=.001,
            position = c("left","top"), 
            bg.color = "white", 
            bg.alpha=.2, 
            frame="gray50", 
            height=.6, 
            hist.width=.2,
            hist.height=.2, 
            hist.bg.color="gray60", 
            hist.bg.alpha=.5) +
  tm_shape(tract[tract$tract == 177 | tract$tract == 175, ], ) +
  tm_fill(col = "white") +
  tm_shape(tract[tract$tract == 177 | tract$tract == 175, ], ) +
  tm_fill(col = "forestgreen", alpha = .6) + tm_borders() + 
  tm_layout(main.title = "Number of Family Households in 2015", 
            main.title.size = 2.2, fontfamily = "serif", main.title.position = 0.03, 
            legend.format = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500)) +
  save_tmap(filename = "Family Households in 2015.png", width = 6.5, height = 8, dpi = 700)
