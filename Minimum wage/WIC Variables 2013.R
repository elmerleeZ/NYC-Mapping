#############################################################################
# Program Name:   WIC Variables 2013.R
# Location:       //Chgoldfs/pru/Elmer_Work/R/MinWage/WIC/WIC Variables 2013.R
# Author:         
# Date Created:   
# Project:        
# Purpose:        
#############################################################################


########################################################################
## Prepare & Import  ##

library(readr)
library(tidyr)
library(dplyr)
library(tidyverse)
library(foreign)

# base_2013 <- read.csv("//Chgoldfs/pru/Elmer_Work/MinWage/Run21- 9 an hour/childcare/2013 min wage BASE.csv")

base_2013_clean <- read.csv("//Chgoldfs/pru/Elmer_Work/MinWage/Run21- 9 an hour/base_2013_clean.csv")

WIC_var_2013 <- base_2013_clean %>%
	mutate(
		H_numper= PovUnit_Pop_PU, 
		# EDUCATION
			# For YEAR < 2008		
			# Educ_match = ifelse(SCHL <9, 1, ifelse(SCHL >= 9 & SCHL <=13, 2, ifelse(SCHL >= 14, 3, NA))), 
			# For YEAR > 2008
		Educ_match = ifelse(SCHL <=15, 1, ifelse(SCHL >= 16 & SCHL <=21, 2, ifelse(SCHL >= 22, 3, NA))), 
		# WORK EXPERIENCE		
			# For YEAR < 2008		
		# WorkExp = ifelse(is.na(WKW), 3, 
		# 			ifelse(is.na(WKHP), 3, 
		# 				ifelse(WKW > 49 & WKHP > 34, 1, 
		# 					ifelse((WKW <=49 & WKW > 0 ) | (WKHP <= 34 & WKHP > 0),2,NA)))), 
		# For YEAR >= 2008		
		WorkExp = ifelse(is.na(WKW), 3, 
					ifelse(is.na(WKHP), 3, 
						ifelse(WKW ==1 & WKHP > 34, 1, 
							ifelse((WKW >=2 ) | (WKHP <= 34 & WKHP > 0),2,NA)))), 
		sex_flag = ifelse(SEX==1 & PovRel==1,1, 
						ifelse(SEX==2 & PovRel==1, 0, NA)), 
		# Under 18 persons in household
		child_flag = ifelse(AGEP<18, 1,NA), 
		fer_test = ifelse(FER==1, 1, 0), 
		# Calculate the school lunch eligibility
		income_ratio = Povinc_pu/Poverty_Guideline, 
		# Any children or babies
		wic_child = ifelse(AGEP %in% c(1,2,3,4),1,0), 
		wic_baby = ifelse(AGEP == 0,1,0) ) %>%
	group_by(SERIALNO,PovUnit_PU) %>%
	mutate(
		sex_flag_sum=sum(sex_flag,na.rm=T), 
		# fer variable will identify children whose parents had them in the past 12 months
		fer_test_sum=sum(fer_test,na.rm=T), 
		wic_child_sum=sum(wic_child,na.rm=T), 
		wic_baby_sum=sum(wic_baby,na.rm=T),
		adjpap_pu=sum(PA_adj,na.rm=T), 
		adjpap_pu=ifelse(is.na(adjpap_pu),0,adjpap_pu), 
		ssip_pu = sum(SSIP,na.rm=T), 
		ssip_pu = ifelse(is.na(ssip_pu),0,ssip_pu) ) %>% 
	ungroup() %>%
	mutate(
		# Some dummies
		Infant_Present_HH = ifelse(wic_baby_sum>0,1,0),
		dummy_HFOODSP = ifelse(FS==1,1,0),
		# WIC eligibility
		WIC_child_elig = ifelse(wic_child==1 & (adjpap_pu>0 | dummy_HFOODSP==1 | income_ratio<=1.85),1,0),
		WIC_baby_elig = ifelse(wic_baby==1 & (adjpap_pu>0 | dummy_HFOODSP==1 | income_ratio<=1.85),1,0),
		HINS4 = ifelse(YEAR<2008,NA,HINS4),
		WIC_woman_elig = ifelse(is.na(FER),0,
							ifelse(YEAR>2008 & (FER==1 & (adjpap_pu>0 | dummy_HFOODSP==1 | income_ratio<=1.85 | HINS4==1)),1,
								ifelse(YEAR<=2008 & (FER==1 & (adjpap_pu>0 | dummy_HFOODSP==1 | income_ratio<=1.85)),1,0)))
		) %>%
	group_by(SERIALNO,PovUnit_PU) %>%
	mutate(
		wic_baby_sum=ifelse(is.na(WIC_baby_elig),0,sum(WIC_baby_elig)),
		wic_child_sum=ifelse(is.na(WIC_child_elig),0,sum(WIC_child_elig)) )%>%
	ungroup() %>% mutate(WIC_woman_elig = ifelse(wic_baby_sum==0 & WIC_woman_elig==1,0,WIC_woman_elig)) %>%
	group_by(SERIALNO,PovUnit_PU) %>%
	mutate(wic_woman_sum = ifelse(is.na(WIC_woman_elig),0,sum(WIC_woman_elig))) %>%
	ungroup() %>%
	mutate(
		WIC_elig_HH = ifelse((wic_child_sum>0 | wic_baby_sum>0 | wic_woman_sum>0) & (PovRel==1),1,0)) %>%
	# Setup for the Logit Regression for participation rate for Free and Reduced School Lunch 
	mutate(
		Male = ifelse(SEX==1,1,0),
		Female = ifelse(SEX==1,0,1),
		# Citizen Dummies
		CitizenStat_Native_Born = ifelse(CitizenStat==1,1,0),
		CitizenStat_Naturalized = ifelse(CitizenStat==2,1,0),
		CitizenStat_Not = ifelse(CitizenStat==3,1,0),
		# Ethnicity Dummies
		Ethnicity_White = ifelse(Ethnicity==1,1,0),
		Ethnicity_Black = ifelse(Ethnicity==2,1,0),
		Ethnicity_Asian_Other = ifelse(Ethnicity %in% c(3,5),1,0),
		Ethnicity_Hispanic = ifelse(Ethnicity==4,1,0),
		# Education
		low_educ = ifelse(Educ_match==1,1,0),
		Med_educ = ifelse(Educ_match==2,1,0),
		high_educ = ifelse(Educ_match==3,1,0),
		# Work Experience
		WorkExp_FT_YR = ifelse(WorkExp==1,1,0),
		WorkExp_LT_FT = ifelse(WorkExp==2,1,0),
		WorkExp_NW = ifelse(WorkExp==3,1,0),
		# Household Type
		single_female_HH = ifelse(FamilyType2_PU==3,1,0),
		exp_var = ifelse(WIC_elig_HH==1 & YEAR == 2013, 
			-1.966 +
			0.074 * Ethnicity_White +
			0.771 * Ethnicity_Black +
			0.653 * Ethnicity_Hispanic +
			-0.341 * Med_educ +
			-1.376 * high_educ +
			-0.036 * single_female_HH +
			0.85 * Infant_Present_HH +
			0.348 * income_ratio +
			0.029 * H_numper +
			0.561 * dummy_HFOODSP +
			-0.036 * CitizenStat_Naturalized +
			0.164 * CitizenStat_Not +
			0.373 * WorkExp_LT_FT +
			0.35 * WorkExp_NW, NA), 
		prob_value = ifelse(WIC_elig_HH==1,1/ (1 + (exp(-exp_var))),NA)) %>%
	group_by(SERIALNO,PovUnit_PU) %>%
	mutate(
		pov_unit_prob_value = max(prob_value,na.rm=T),
		pov_unit_prob_value = ifelse(pov_unit_prob_value == -Inf, NA, pov_unit_prob_value)) %>%
	ungroup() %>%
	# select(SERIALNO, SPORDER, H_numper, Educ_match, WorkExp, sex_flag, sex_flag_sum, child_flag, income_ratio, fer_test, fer_test_sum, wic_child, wic_baby, Infant_Present_HH, dummy_HFOODSP, adjpap_pu, ssip_pu, WIC_child_elig, WIC_baby_elig, HINS4, WIC_woman_elig, wic_baby_sum, wic_child_sum, wic_woman_sum, WIC_elig_HH, Female, Male, CitizenStat_Native_Born, CitizenStat_Naturalized, CitizenStat_Not, Ethnicity_White, Ethnicity_Black, Ethnicity_Asian_Other, Ethnicity_Hispanic, low_educ, Med_educ, high_educ, WorkExp_FT_YR, WorkExp_LT_FT, WorkExp_NW, single_female_HH)
	select(SERIALNO, SPORDER, YEAR, PWGTP, PovUnit_PU, WorkExp, WIC_baby_elig, WIC_child_elig, WIC_woman_elig, wic_baby_sum, pov_unit_prob_value, H_numper, Educ_match, sex_flag, sex_flag_sum, child_flag, income_ratio, fer_test, fer_test_sum, wic_child, wic_baby, Infant_Present_HH, dummy_HFOODSP, adjpap_pu, ssip_pu, HINS4, wic_child_sum, wic_woman_sum, WIC_elig_HH, Female, Male, CitizenStat_Native_Born, CitizenStat_Naturalized, CitizenStat_Not, Ethnicity_White, Ethnicity_Black, Ethnicity_Asian_Other, Ethnicity_Hispanic, low_educ, Med_educ, high_educ, WorkExp_FT_YR, WorkExp_LT_FT, WorkExp_NW, single_female_HH)


########################################################################
## Export ##


write.csv(WIC_var_2013, file = '//Chgoldfs/pru/Elmer_Work/MinWage/Run21- 9 an hour/WIC/WIC_var_2013.csv',row.names = F)






