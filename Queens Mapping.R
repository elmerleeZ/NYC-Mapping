#############################################################################
# Program Name:   Queens Mapping.R
# Location:       /Users/zongyangli/Documents/Github/NYC-demographic/Queens Mapping.R
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
library(tmap)
library(broom)
library(rgeos)

library(Cairo) #font

# load Shape Files
tract <- readOGR(dsn = path.expand("/Users/zongyangli/Google Drive/Job/RA - Public Good/Public Good Code_Resources/Data/tract_shape_files"), layer = "nyct2010")
  # tract <- readOGR(dsn = path.expand("/Users/zongyangli/Google Drive/Job/RA - Public Good/Data/Shape files/NYC PUMA"))
puma_focus <- c(4101,4102)


########################################################################
## Reshape Data  ##

# load data files
setwd('/Users/zongyangli/Google Drive/Job/RA - Public Good/Projects/UJA_Queens/output') 
queens_clean <- read.csv("queens_clean.csv") %>% select(-X) %>% mutate(puma = as.character(substr(geoid,4,7)))

## Merge with meta data

meta <- tract@data
colnames(meta)[1] <- "tract"
colnames(meta)[9] <- "puma"
meta$puma <- as.character(meta$puma)
meta <- left_join(meta, queens_clean, by = "puma")

tract@data <- meta


########################################################################
## Make Maps  ##


tm_plot <- function(tract,var){
      tm_shape(tract[tract$puma %in% puma_focus,]) + tm_borders(col = "white") + 
      tm_polygons(col = var, palette = "Purples", alpha = .9) + 
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
      tm_shape(tract[tract$tract == 99,]) +
      tm_fill(col = "forestgreen", alpha = .6) + tm_borders(col = "white") +
      tm_shape(tract[tract$tract == 299, ]) +
      tm_fill(col = "forestgreen", alpha = .6) + tm_borders(col = "white") +
      tm_layout(main.title = NA, 
                main.title.size = 1.8, fontfamily = "sans", main.title.position = 0.13)
}

tm_plot(tract,'tot_pop') # (8.71*4.54 inch)
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
