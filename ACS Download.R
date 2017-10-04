#############################################################################
# Program Name:   ACS Download.R
# Location:       /Users/zongyangli/Google Drive/Job/RA - Public Good/Public Good Code_Resources的副本/R Code/ACS Download.R
# Author:         Zongyang Li
# Date Created:   
# Project:        
# Purpose:        Download ACS Datasets from API 
#############################################################################


devtools::install_github("austensen/acsplus")
library(acs)
library(acsplus)
library(dplyr)

api.key.install(key = "1d9149a1d6d2eb5c7cfbc7dd7c74d092c415a8a8")


########################################################################
## Set up geography variables  ##


# nyc_tracts <- geo.make(state = "NY", county = c(5, 47, 61, 81, 85), tract = "*")
brooklyn_tracts <- geo.make(state = "NY", county = 47, tract = "*")

variables <- c(
  acs_vars("B01003", 1), # Total Population
  acs_vars("B01002", 1), # Median Age 
  acs_vars("B02011", 1), # Asian
  acs_vars("B02009", 1), # Black 
  acs_vars("B03003", 3), # Hispanic
  acs_vars("B02008", 1), # White
  acs_vars("B11001",1:9), # Household Type
  acs_vars("B19001",1:17), # Household Income in past 12 months
  acs_vars("B19013",1), # Median Household Income in past 12 months
  acs_vars("B19083",1) # Gini Index
)


########################################################################
## Download ACS data  ##

raw_acs <- acs_download(geo = brooklyn_tracts,
                        geoid = geoid_tract,
                        vars = variables,
                        span = 5,
                        end_years = 2015,
                        get_se = FALSE,
                        dataset = "acs")


raw_acs_col <- as.data.frame(estimate(acs.fetch(endyear = 2015, span = 5, geography = brooklyn_tracts, 
                                                      variable = variables, col.names = "pretty")))




