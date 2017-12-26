#############################################################################
# Program Name:   Food Stamp Match 2013.R
# Location:       //Chgoldfs/pru/Elmer_Work/R/MinWage/Food Stamp Match 2013.R
# Author:         
# Date Created:   
# Project:        
# Purpose:        
#############################################################################


########################################################################
## Prepare and Import  ##

library(readr)
library(tidyr)
library(dplyr)
library(tidyverse)

foodstamp_raw <- read.csv("\\\\chgoldfs\\pru\\Food Stamp Research\\2013 Admin sample.csv",header=T)


########################################################################
## Clean data ##

# indicate income quantiles
	#?? since the NAs are delted, the distribution will be different
fs.inc75 <- quantile(foodstamp_raw$TotalIncome,0.75,na.rm=T)
fs.inc90 <- quantile(foodstamp_raw$TotalIncome,0.90,na.rm=T)

foodstamp_raw <- foodstamp_raw %>% select(FSYearAmt,TotalIncome,HouseholdSize,Children,ElderlyHHH,AgeDis,age_yr,CDnum) 
#?? Delete all the rows with NA 
foodstamp_raw <- foodstamp_raw[complete.cases(foodstamp_raw), ] # 44935 to 13012

foodstamp_clean <- foodstamp_raw %>%
	mutate(
		age2 = age_yr^2, 
		inccat = ifelse(TotalIncome<fs.inc75, 1, 
					ifelse(TotalIncome>=fs.inc75 & TotalIncome<fs.inc90, 2, 
						ifelse(TotalIncome>=fs.inc90, 3, NA))))


# Regress FS amt on characteristics
fit <- lm(FSYearAmt~factor(inccat)+HouseholdSize+Children+ElderlyHHH+AgeDis+age_yr+age2,data=foodstamp_clean)

# Setup foodstamp pmm
foodstamp_pmm <- foodstamp_clean %>%
		mutate(
			pm = predict(fit),
			SERIALNO = 0, 
			SPORDER = 0, 
			id = 0, 
			fsv = FSYearAmt) %>%
		select(SERIALNO,SPORDER,id,CDnum,pm,fsv)


#Read in ACS Data
#acs <- read.csv("C:\\WORK\\2013 Report Work\\Work at 253\\Minimum wage simulation\\Food Stamps\\RUN7 ACS Food Stamp Variables 2013.csv",header=T)
# acs_raw <- read.csv("\\\\Chgoldfs\\pru\\Quan work\\MinWage\\Minimum_Wage_by_Amount_Simulation\\Run21- 9 an hour\\Food Stamps\\RUN7 ACS Food Stamp Variables 2013.csv",header=T)
acs_raw <- read.csv('//Chgoldfs/pru/Elmer_Work/MinWage/Run21- 9 an hour/Food Stamps/food_stamp_var_2013.csv')  
acs.inc75 <- quantile(acs_raw$TotalIncome,0.75)
acs.inc90 <- quantile(acs_raw$TotalIncome,0.90)

acs_clean <- acs_raw %>%
		mutate(
			age2 = age_yr^2, 
			inccat = ifelse(TotalIncome<acs.inc75, 1, 
						ifelse(TotalIncome>=acs.inc75 & TotalIncome<acs.inc90, 2, 
							ifelse(TotalIncome>=acs.inc90, 3, NA))))

acs_pmm <- acs_clean %>%
		mutate(
			pm = predict(fit,newdata=acs_clean), 
			fsv = 0, 
			id = 1) %>%
		select(SERIALNO,SPORDER,id,CDnum,pm,fsv)

# Set up the complete PMM
pmm_test <- rbind(foodstamp_pmm,acs_pmm)


########################################################################
## Match Admin and ACS  ##

source("\\\\chgoldfs\\pru\\5-Misc\\PMM Research\\PMM Food Stamps.R")
fs_matched <- match(pmm_test)

food_stamp_out_2013 <- fs_matched %>%
		filter(id == 1) %>%
		mutate(FSmatch = pmm) %>% # rename
		select(SERIALNO,SPORDER,FSmatch)

write.csv(food_stamp_out_2013, file = '//Chgoldfs/pru/Elmer_Work/MinWage/Run21- 9 an hour/Food Stamps/food_stamp_out_2013.csv')  



