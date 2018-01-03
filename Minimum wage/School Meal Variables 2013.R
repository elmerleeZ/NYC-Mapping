#############################################################################
# Program Name:   School Meal Variables 2013.R
# Location:       //Chgoldfs/pru/Elmer_Work/R/MinWage/schoolmeal/School Meal Variables 2013.R
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



school_meal_var_2013 <- base_2013_clean %>%
	mutate(
		H_numper = PovUnit_Pop_PU, 
		income_ratio = Povinc_pu/Poverty_Guideline, 
		PA_flag = ifelse(PAP>0 & REL==0,1,0)) %>% # how to deal with NA) 
	group_by(SERIALNO,PovUnit_PU) %>%
	mutate(PA_flag_sum=sum(PA_flag, na.rm = T)) %>% ungroup() %>% # don't know if na.rm is right
	mutate(
		PA_flag_sum = ifelse(is.na(PA_flag_sum), 0, PA_flag_sum), 
		SL_income_elig = ifelse(income_ratio <1.3 | (FS!=2 & FS!=0) | PA_flag_sum>0,1,0), # NA not dealt
		SL_red_income_elig = ifelse(income_ratio>=1.3 & income_ratio<1.85 & SL_income_elig != 1, 1,0), # NA not dealt
		# For year < 2008		
		Educ_match = ifelse(SCHL <9, 1, ifelse(SCHL >= 9 & SCHL <=13, 2, ifelse(SCHL >= 14, 3, NA))), 
		# For year > 2008
		Educ_match = ifelse(SCHL <=15, 1, ifelse(SCHL >= 16 & SCHL <=21, 2, ifelse(SCHL >= 22, 3, NA))), 
		
		# Age Vriables 
		Elem_age = ifelse(AGEP>=5 & AGEP<=10, 1, NA), 
		MS_age = ifelse(AGEP>=11 & AGEP<=13, 1, NA), 
		HS_age = ifelse(AGEP>=14 & AGEP<18, 1, NA), 
		sch_age = ifelse(AGEP>=5 & AGEP<18, 1, NA)) %>% 
	group_by(SERIALNO,PovUnit_PU) %>%
	mutate(
		sch_age_sum=sum(sch_age, na.rm = T),
		Elem_age_sum=sum(Elem_age, na.rm = T),
		MS_age_sum=sum(MS_age, na.rm = T),
		HS_age_sum=sum(HS_age, na.rm = T)
		) %>% ungroup() %>%
	mutate(filter_sch_age = ifelse(sch_age_sum > 0 & AGEP>=5, 1, 0)) %>% # NA not dealt
	# here play a trick so that the filter can be applied to all variables (Similar to filter in SPSS)
	group_by(SERIALNO,PovUnit_PU,filter_sch_age) %>%
	mutate(a_age_min=min(AGEP)) %>% ungroup() %>%
	mutate(
		a_age_min = ifelse(filter_sch_age ==1,a_age_min,NA), ## end of trick
		elig_free_lunch = ifelse(SL_income_elig==1 & sch_age_sum>0,1,0),
		elig_free_lunch = ifelse(is.na(elig_free_lunch),0,elig_free_lunch), 
		elig_reduced_lunch = ifelse(SL_red_income_elig==1 & sch_age_sum>0,1,0), 
		elig_reduced_lunch = ifelse(is.na(elig_reduced_lunch),0,elig_reduced_lunch), 
		regression_pop = ifelse(elig_reduced_lunch==1 | elig_free_lunch==1,1,0), 
		# SEX Vars
		Male = ifelse(SEX==1,1,0),
		Female = ifelse(SEX==1,0,1),
		# Citizen Dummies
		CitizenStat_Native_Born = ifelse(CitizenStat==1,1,0),
		CitizenStat_Naturalized = ifelse(CitizenStat==2,1,0),
		CitizenStat_Not = ifelse(CitizenStat==3,1,0),
		# Ethnicity Dummies
		Ethnicity_White = ifelse(Ethnicity==1,1,0),
		Ethnicity_Black = ifelse(Ethnicity==2,1,0),
		Ethnicity_Asian_Other = ifelse(Ethnicity==3|Ethnicity==5,1,0),
		Ethnicity_Hispanic = ifelse(Ethnicity==4,1,0),
		# Food Stamp
		dummy_HFOODSP = ifelse(FS==1,1,0),
		# Medicaid
		dummy_MA = ifelse(HINS4==1,1,ifelse(HINS4==2,0,NA)),
		# Education
		low_educ = ifelse(Educ_match==1,1,0), 
		Med_educ = ifelse(Educ_match==2,1,0), 
		high_educ = ifelse(Educ_match==3,1,0), 
		# Household type
		Household_type_single = ifelse(FamilyType2_PU==2 | FamilyType2_PU==3,1,0),
		# Work Experience
		WorkExp_FT_YR = ifelse(WorkExp==1,1,0),
		WorkExp_LT_FT = ifelse(WorkExp==2,1,0),
		WorkExp_NW = ifelse(WorkExp==3,1,0)) %>%
	mutate(
		exp_var = ifelse(regression_pop==1 & PovRel==1 & YEAR == 2013, 
			1.6 + 
			   -0.013 * AGEP
			 + -0.08  * Ethnicity_White
			 + 0.249 * Ethnicity_Black
			 + 0.518 * Ethnicity_Hispanic
			 + 0.037 * Female
			 + -0.034 * Med_educ
			 + -0.499 * high_educ
			 + -0.368 * income_ratio
			 + -0.086 * H_numper
			 + -0.07 * a_age_min
			 + 1.347 * dummy_HFOODSP
			 + 0.248 * CitizenStat_Naturalized
			 + 0.389 * CitizenStat_Not
			 + -0.16  * WorkExp_LT_FT
			 + -0.219 * WorkExp_NW
			 + 0.506 * Household_type_single,NA),
		prob_value = ifelse(regression_pop==1 & PovRel==1,1/ (1 + (exp(-exp_var))),NA)
		) %>%
	group_by(SERIALNO,PovUnit_PU) %>%
	mutate(
		pov_unit_prob_value=max(prob_value,na.rm = T), 
		pov_unit_prob_value = ifelse(pov_unit_prob_value == -Inf, NA, pov_unit_prob_value)) %>%
	ungroup() %>%
	# select(AGEP, Ethnicity_White, Ethnicity_Black, Ethnicity_Hispanic, Female, Med_educ, high_educ, income_ratio, H_numper, a_age_min, dummy_HFOODSP, CitizenStat_Naturalized, CitizenStat_Not, WorkExp_LT_FT, WorkExp_NW, Household_type_single)	
	select(SERIALNO,SPORDER,H_numper,Povinc_pu,Poverty_Guideline,income_ratio, PA_flag, PA_flag_sum, SL_income_elig, SL_red_income_elig, Educ_match, Elem_age, MS_age, HS_age, sch_age, sch_age_sum, Elem_age_sum, MS_age_sum, HS_age_sum, a_age_min, elig_free_lunch, elig_reduced_lunch, regression_pop, Female, Male, CitizenStat_Native_Born, CitizenStat_Naturalized, CitizenStat_Not, Ethnicity_White, Ethnicity_Black, Ethnicity_Asian_Other, Ethnicity_Hispanic, dummy_HFOODSP, low_educ, Med_educ, high_educ, Household_type_single, WorkExp_FT_YR, WorkExp_LT_FT, WorkExp_NW, exp_var, prob_value, pov_unit_prob_value)

school_meal_2013_quan <- read.spss('//Chgoldfs/pru/Quan work/MinWage/Minimum_Wage_by_Amount_Simulation/Run21- 9 an hour/School Meals/RUN7 2013 School Meals Vars.sav', to.data.frame=TRUE)
