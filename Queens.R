#############################################################################
# Program Name:   Queens.R
# Location:       /Users/zongyangli/Documents/Github/NYC-demographic/Queens.R
# Author:         
# Date Created:   
# Project:        
# Purpose:        
#############################################################################


source("/Users/zongyangli/Documents/Github/R-Key-functions/Start up.R")

library(acs)
library(acsplus)
library(dplyr)
library(tidyr)
library(readxl)
library(tidycensus)

api.key.install(key = "1d9149a1d6d2eb5c7cfbc7dd7c74d092c415a8a8")
census_api_key("1d9149a1d6d2eb5c7cfbc7dd7c74d092c415a8a8", install = T)


########################################################################
## Get data  ##

## Specify variables
variables <- c(
  acs_vars("B01002",1), # Median Age
  acs_vars("B01003",1), # Total Population
  acs_vars("B02008",1), # Num White
  acs_vars("B02009",1), # Num Black
  acs_vars("B02011",1), # Num Asian
  acs_vars("B03003",3), # Num Hispanic
  acs_vars("B11001",1), # Num Household
  acs_vars("B15003",17:25), # Educational Attanment - high school and above
  acs_vars("B19013",1), # Median Household Income in past 12 months
  acs_vars("B25064",1) # Median Gross Rent
)

## Get Data
acs_raw <- get_acs(geography = 'public use microdata area', state = '36', variables = variables, survey = "acs1", endyear = 2016)

## Clean data
acs_clean <- acs_raw %>%
	select(-moe) %>%
	spread(key = variable, value = estimate) %>%
	filter(GEOID %in% c('3604101','3604109','3604102')) 

names(acs_clean) <- c('geoid', 'neighborhood', 'age_median', 'tot_pop', 'num_white', 'num_black', 'num_asian', 'num_hispanic', 'tot_hh', 'num_hs_diploma', 'num_hs_ged', 'num_clg_1yr', 'num_clg_1yr_more', 'num_associate', 'num_bachelor', 'num_master', 'num_professional', 'num_doctor', 'med_hhincome', 'rent_median')
queens_clean <- as.data.frame(acs_clean) %>%
	mutate(
		pct_white = num_white/tot_pop,
		pct_black = num_black/tot_pop,
		pct_asian = num_asian/tot_pop,
		pct_hispanic = num_hispanic/tot_pop,
		pct_high_sch_higher = (num_hs_diploma+num_hs_ged+num_clg_1yr+num_clg_1yr_more+num_associate+num_bachelor+num_master+num_professional+num_doctor)/tot_pop,
		pct_college_degree = (num_associate+num_bachelor+num_master+num_professional+num_doctor)/tot_pop) %>%
	select(geoid,neighborhood,tot_pop,tot_hh,age_median,med_hhincome,rent_median,starts_with('pct'),everything())

# clean neighborhood names
for (i in c(1:3)){
queens_clean <- as.data.frame(lapply(queens_clean, gsub, pattern='NYC-Queens Community District ' %S% i %S% '--', replacement=''))
}
queens_clean <- as.data.frame(lapply(queens_clean, gsub, pattern=' PUMA, New York', replacement=''))



########################################################################
## Get Median Housing Price Data for Single Household from Furman  ##

hh_price_med_raw <- read_csv('/Users/zongyangli/Google Drive/Job/RA - Public Good/Public Good Code_Resources - Copy/Data/Housing/communitydistrict-mediansalespriceperunit1familybuilding2016.csv')

hh_price_med_2016 <- hh_price_med_raw %>%
    select(-short_name,-long_name) %>%
    mutate(
      neighborhood = `Community District`,
      boro = substr(neighborhood,1,2),
      suboro = substr(neighborhood,4,5),
      boro_code = ifelse(boro == 'BX','037',ifelse(boro == 'MN','038',ifelse(boro == 'SI','039',ifelse(boro == 'BK','040','041')))),
      puma = boro_code %S% suboro, 
      geoid = '36' %S% boro_code %S% suboro
      ) %>%
    select(-boro,-suboro,-boro_code) %>%
    select(geoid,neighborhood,everything()) %>%
    gather(`2000`,`2001`,`2002`,`2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2010`, `2014`, `2015`,`2016`, key = "year", value = "hh_price_med") %>%
    select(geoid,year,hh_price_med) %>%
    filter(year == 2016) %>% select(geoid,hh_price_med)

########################################################################
## Merge & final output  ##

queens_output <- queens_clean %>%
	left_join(hh_price_med_2016, by = 'geoid') %>%
	select(geoid,neighborhood,tot_pop,tot_hh,age_median,med_hhincome,rent_median,hh_price_med,starts_with('pct'),everything())


## Export

setwd('/Users/zongyangli/Google Drive/Job/RA - Public Good/Projects/UJA_Queens/output')
write.csv(queens_output,"queens_output.csv")


