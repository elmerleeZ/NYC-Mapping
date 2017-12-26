#############################################################################
# Program Name:   Food Stamp Variables 2013 - Elmer.R
# Location:       //Chgoldfs/pru/Elmer_Work/R/MinWage/foodstamp/Food Stamp Variables 2013 - Elmer.R
# Author:         
# Date Created:   12/11/2017
# Project:        
# Purpose:        
#############################################################################


########################################################################
## Prepare and Import  ##

library(readr)
library(tidyr)
library(dplyr)
library(tidyverse)

# base_2013 <- read.csv("//Chgoldfs/pru/Elmer_Work/MinWage/Run21- 9 an hour/childcare/2013 min wage BASE.csv")

base_2013_clean <- read.csv("//Chgoldfs/pru/Elmer_Work/MinWage/Run21- 9 an hour/base_2013_clean.csv")
childcare_out <- read.csv("//Chgoldfs/pru/Elmer_Work/MinWage/Run21- 9 an hour/childcare/childcare_out.csv")


base_2013_clean_w_childcare <- base_2013_clean %>%
	left_join(childcare_out[,c("SERIALNO", "SPORDER", "childcare")], by=c("SERIALNO", "SPORDER"))

########################################################################
## Create Variables  ##

food_stamp_var_2013 <- base_2013_clean_w_childcare %>%
	mutate(
		fs_unit_count = 1,
		ElderlyHHH = ifelse(FSUREL == 1 & AGEP >= 65, 1, 0), 
		child_flag = ifelse(AGEP <= 21, 1, 0)) %>%
	group_by(SERIALNO, PovUnit_PU) %>%
	mutate(MOOP_PU = max(MOOP, na.rm = T)) %>% ungroup() %>%
	group_by(SERIALNO, FSU) %>%
	mutate(
		fsu_size = sum(fs_unit_count), 
		AgeDis = max(ELDERLYDIS), 
		Children=sum(child_flag), 
		inc_fsu = sum(Inc_adj), 
		earn_fsu = sum(IncPers_adj), 
		childcare_fsu = sum(childcare)) %>% 
	ungroup() %>%
	mutate(
		unearn_fsu = inc_fsu - earn_fsu, 
		earn_fsu_adj = earn_fsu*0.8, 
		inc_fsu_adj = earn_fsu_adj + unearn_fsu, 
		month_inc_adj = inc_fsu_adj/12, 
		fs_standard_deduc = ifelse(fsu_size <= 3, 149, 
								ifelse(fsu_size  == 4, 160, 
									ifelse(fsu_size  == 5, 187, 
										ifelse(fsu_size >= 6, 214, 0)))),
		childcare_fsu = ifelse(is.na(childcare_fsu), 0, childcare_fsu), 
		MOOP_PU = ifelse(is.na(MOOP_PU), 0, MOOP_PU), 

		childcare_deduc = childcare_fsu/12, 
		moop_deduc = ifelse(AgeDis == 1, MOOP_PU/12, 0), 
		# turn the NAs of above deductions to 0 to facilitate calculation
		month_inc_adj = ifelse(is.na(month_inc_adj), 0, month_inc_adj), 
		fs_standard_deduc = ifelse(is.na(fs_standard_deduc), 0, fs_standard_deduc), 
		childcare_deduc = ifelse(is.na(childcare_deduc), 0, childcare_deduc), 
		moop_deduc = ifelse(is.na(moop_deduc), 0, moop_deduc), 
		## 
		TotalIncome = month_inc_adj - fs_standard_deduc - childcare_deduc - moop_deduc, 
		month_shelter = HOOP_CEO/12, 
		excess_shelter = month_shelter - (TotalIncome/2), 
		excess_shelter_deduc = excess_shelter, 
		excess_shelter_deduc = ifelse(excess_shelter < 0, 0, 
								  ifelse(excess_shelter > 469 & AgeDis == 0, 469, excess_shelter)), 
		excess_shelter_deduc = ifelse(is.na(excess_shelter_deduc), 0, excess_shelter_deduc), 
		TotalIncome = TotalIncome - excess_shelter_deduc, 
		TotalIncome = ifelse(is.na(TotalIncome), 0, ifelse(TotalIncome >= 0 , TotalIncome, 0)), 
		SSIi = ifelse(SSI_adj > 0, 1, 0), 
		PAi = ifelse(PA_adj > 0, 1, 0)) %>%
	group_by(SERIALNO, FSU) %>%
	mutate(
		SSIdum= max(SSIi), 
		PAdum= max(PAi)) %>% ungroup() %>%
	mutate(
		pafsf = ifelse(PAdum == 1 | SSIdum == 1, 1, 0), 
		inc_fsu = ifelse(inc_fsu <= 0, 0, inc_fsu), 
		FSeligible = ifelse((FSUREL == 1 & (CIT < 5 | (CIT == 5 & YOEP <= 2008))) & 
							((fsu_size == 1 & (inc_fsu/11490) <= 1.5) | (fsu_size == 2 & (inc_fsu/15510) <= 1.5) | 
							(fsu_size == 3 & (inc_fsu/19530) <= 1.5) | (fsu_size == 4 & (inc_fsu/23550) <= 1.5) |
							(fsu_size == 5 & (inc_fsu/27570) <= 1.5) | (fsu_size == 6 & (inc_fsu/31590) <= 1.5) |
							(fsu_size == 7 & (inc_fsu/35610) <= 1.5) | (fsu_size == 8 & (inc_fsu/39630) <= 1.5) |
							(fsu_size > 8 & (inc_fsu/39630+((fsu_size-8)*4020)) <= 1.5)), 1, 0), 
		FSreceive = ifelse(FS == 1, 1, 0), 
		FSelnor = ifelse(FSUREL == 1 & FSeligible == 1 & FSreceive == 0, 1, 0), 
		fsimp = ifelse(FSelnor == 1 & pafsf == 1, 1, 0)) %>%
	# rename some variables
	mutate(HouseholdSize = fsu_size, age_yr = AGEP, CDnum = CD) %>%
	filter(FSUREL == 1 & (FSreceive==1 | fsimp==1)) %>%
	select(SERIALNO,SPORDER,TotalIncome,HouseholdSize,Children,ElderlyHHH,AgeDis,age_yr,CDnum,WGTP)
	# select(SERIALNO,SPORDER,month_inc_adj,fs_standard_deduc,childcare_fsu,childcare_deduc,AgeDis,MOOP,MOOP_PU,moop_deduc,month_shelter, excess_shelter,excess_shelter_deduc, TotalIncome,fsu_size,Children,ElderlyHHH,AgeDis,AGEP,CD,WGTP)


write.csv(food_stamp_var_2013, file = '//Chgoldfs/pru/Elmer_Work/MinWage/Run21- 9 an hour/Food Stamps/food_stamp_var_2013.csv',row.names = F)

