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
library(readxl)
library(tidycensus)

api.key.install(key = "1d9149a1d6d2eb5c7cfbc7dd7c74d092c415a8a8")
census_api_key("1d9149a1d6d2eb5c7cfbc7dd7c74d092c415a8a8", install = T)


########################################################################
## Get data  ##

## Specify variables
variables <- c(
  acs_vars("B01003",1), # Total Population
  acs_vars("B02008",1), # White
  acs_vars("B02009",1), # Black
  acs_vars("B02011",1), # Asian
  acs_vars("B03003",3), # Hispanic
  acs_vars("B11001",1), # Household
  acs_vars("B15003",17:25), # Educational Attanment - high school and above
  acs_vars("B19013",1) # Median Household Income in past 12 months
)

## Get Data
acs_raw <- get_acs(geography = 'public use microdata area', state = '36', variables = variables, survey = "acs1", endyear = 2016)

## Clean data
acs_clean <- acs_raw %>%
	select(-moe) %>%
	spread(key = variable, value = estimate) %>%
	filter(GEOID %in% c('3604101','3604102')) 

names(acs_clean) <- c('geoid','neighborhood','tot_pop','num_white', 'num_black', 'num_asian', 'num_hispanic', 'tot_hh','num_hs_diploma', 'num_hs_ged', 'num_clg_1yr', 'num_clg_1yr_more', 'num_associate', 'num_bachelor', 'num_master', 'num_professional', 'num_doctor','med_hhincome')

queens_clean <- acs_clean %>%
	mutate(
		pct_white = num_white/tot_pop,
		pct_black = num_black/tot_pop,
		pct_asian = num_asian/tot_pop,
		pct_hispanic = num_hispanic/tot_pop,
		pct_high_sch_higher = (num_hs_diploma+num_hs_ged+num_clg_1yr+num_clg_1yr_more+num_associate+num_bachelor+num_master+num_professional+num_doctor)/tot_pop,
		pct_college_degree = (num_associate+num_bachelor+num_master+num_professional+num_doctor)/tot_pop
		) %>%
	select(geoid,neighborhood,tot_pop,tot_hh,med_hhincome,starts_with('pct'),everything())

# clean neighborhood names
for (i in c(1,3)){
queens_clean <- as.data.frame(lapply(queens_clean, gsub, pattern='NYC-Queens Community District ' %S% i %S% '--', replacement=''))
}
queens_clean <- as.data.frame(lapply(queens_clean, gsub, pattern=' PUMA, New York', replacement=''))

## Export

setwd('/Users/zongyangli/Google Drive/Job/RA - Public Good/Projects/UJA_Queens/output')
write.csv(queens_clean,"queens_clean.csv")


