#############################################################################
# Program Name:   Minimum Wage Setup - Elmer.R
# Location:       //Chgoldfs/pru/Elmer_Work/R/MinWage/Minimum Wage Setup - Elmer.R
# Author:         Elmer
# Date Created:   
# Project:        
# Purpose:        Set Up Min Wage Variables
#############################################################################


library(dplyr)
library(tidyverse)
library(foreign)


### FOR SPSS, have to save data to save the changes
# base_2013 <- read.spss("//Chgoldfs/pru/Elmer_Work/MinWage/Run21- 9 an hour/2013 min wage BASE.sav", to.data.frame=TRUE)
base_2013 <- read.csv("//Chgoldfs/pru/Elmer_Work/MinWage/Run21- 9 an hour/2013 min wage BASE for R.csv")

base_2013_clean <- base_2013 %>%
	mutate(
		adult = ifelse(AGEP>=16,1,0),
		weeksworked = ifelse(adult ==1 & !is.na(WKW),
		                     ifelse(WKW ==1, 50, 
		                            ifelse(WKW == 2, 49, 
		                                   ifelse(WKW == 3, 44,
		                                          ifelse(WKW == 4, 33,
		                                                 ifelse(WKW == 5, 20,
		                                                        ifelse(WKW == 6, 6,0)))))),ifelse(adult ==1 & is.na(WKW),0,NA)), 
		hrlywage = ifelse(Wage_adj > 0 & WKHP > 0,Wage_adj/(WKHP*weeksworked), 0),
		ORIG_incpers_adj = IncPers_adj,
		# ORIG_hrlywage = hrlywage,
		orig_inc_adj = Inc_adj,
		orig_wage_adj = Wage_adj,
		ORIG_HINCP = HINCP,
		ORIG_INCHH_ADJ = IncHH_adj,
		ORIG_POVINC_PU = PovInc_PU, 
		filter_mw = ifelse(AGEP>=16 & AGEP<=64 & !is.na(WKW) & WKW>0 & COW!=6 & hrlywage>6.25 & hrlywage<10.00,1,0), 
		# cautious: for the range function in SPSS, the corresponding in R is >= & <=
		filter_no_hs = ifelse(filter_mw == 1 & AGEP >=16 & AGEP <=29 & SCHL < 16, 1, 0), 
		filter_hs = ifelse(filter_mw ==1 & (AGEP >=30 & AGEP <=64 | AGEP >=16 & AGEP <=29 & SCHL >= 16),1,0), 
		INCPERS_ADJ_noW = IncPers_adj - Wage_adj, 
		INC_ADJ_noW = Inc_adj - Wage_adj, 
		GetRaise = ifelse(filter_mw == 1 & hrlywage >= 6.25 & hrlywage <=7.25, 1, 
						ifelse(filter_mw == 1 & hrlywage > 7.25 & hrlywage <=10.00, 2, 0)), 
		hrlywage2 = ifelse(filter_mw == 1 & hrlywage >= 6.25 & hrlywage <=7.25, (hrlywage - 6.25) * 2.75 + 6.25, 
						ifelse(filter_mw == 1 & hrlywage > 7.25 & hrlywage <=10.00, (hrlywage - 7.25) * 0.363636363636364 + 9.00, 
					 		ifelse(filter_mw != 1, hrlywage, 0))), 
		WAGE_ADJ2 = ifelse(filter_mw == 1,hrlywage2*(WKHP*weeksworked),Wage_adj), 
		INCPERS_ADJ2 = INCPERS_ADJ_noW + WAGE_ADJ2, 
		INC_ADJ2 = INC_ADJ_noW + WAGE_ADJ2, 
		IncPers_adj = INCPERS_ADJ2, 
		Inc_adj = as.numeric(INC_ADJ2), 
		Wage_adj = WAGE_ADJ2, 
		MINWAGE_RAISED = ifelse(GetRaise > 0, 1, 0)) %>%
	group_by(SERIALNO, PovUnit_PU) %>%
	mutate(
		Povinc_pu=sum(Inc_adj, na.rm = T), 
		MINWAGE_RAISED_PU = max(MINWAGE_RAISED)) %>% ungroup() %>%
	group_by(SERIALNO) %>%
		mutate(HINCP=sum(PINCP,na.rm = T)) %>% ungroup() %>%
	mutate(
		IncHH_adj = HINCP * moneyadjust, 
		Income1 = Povinc_pu,
		PERAGI = Int_adj+Wage_adj+Semp_adj+OI_adj+Ret_adj, 
		tu_spouse_flag = ifelse(TaxUnitRel==2, 1, 0)) %>%
	group_by(SERIALNO, TaxUnit_TU) %>%
		mutate(spouse_flag_tu=max(tu_spouse_flag)) %>% ungroup() %>%
	mutate(tuspousefilter = ifelse(spouse_flag_tu==1 & TaxUnitRel %in% c(1,2), 1, 0)) %>%
	group_by(SERIALNO, TaxUnit_TU) %>%
		mutate(
			IncMJ_adj_TOT_TU = ifelse(tuspousefilter ==1,sum(Inc_adj[which(tuspousefilter==1)]), NA), # has to condition tuspousefilter==1 two times; one in generating variable, one in summing
			AGIMJ_TOT_TU = ifelse(tuspousefilter ==1,sum(PERAGI[which(tuspousefilter==1)]), NA),
			WAGEMJ_ADJ_TOT_TU = ifelse(tuspousefilter ==1,sum(Wage_adj[which(tuspousefilter==1)]), NA)) %>%
  	ungroup() 
# %>% select(SERIALNO,TaxUnit_TU,Inc_adj,AGEP,tuspousefilter,adult,weeksworked,hrlywage, ORIG_incpers_adj, orig_inc_adj, orig_wage_adj, ORIG_HINCP, ORIG_INCHH_ADJ, ORIG_POVINC_PU, filter_mw, filter_no_hs, filter_hs, INCPERS_ADJ_noW, INC_ADJ_noW, GetRaise, hrlywage2, WAGE_ADJ2, INCPERS_ADJ2, INC_ADJ2, IncPers_adj, Wage_adj,Semp_adj,MINWAGE_RAISED, 
		# Povinc_pu, MINWAGE_RAISED_PU, HINCP, IncHH_adj, Income1, PERAGI, tu_spouse_flag, spouse_flag_tu, tuspousefilter, IncMJ_adj_TOT_TU, AGIMJ_TOT_TU, WAGEMJ_ADJ_TOT_TU)


##################################################
# Export
##################################################

setwd('//Chgoldfs/pru/Elmer_Work/MinWage/Run21- 9 an hour')
write.csv(base_2013_clean, file = 'base_2013_clean.csv')


##################################################
# Check matching
##################################################

# base_2013_origin <- read.spss("//Chgoldfs/pru/Elmer_Work/MinWage/Run21- 9 an hour/2013 min wage BASE.sav", to.data.frame=TRUE)

# base_2013_origin_clean <- base_2013_origin %>%
#   	select(SERIALNO,TaxUnit_TU,Inc_adj,AGEP,tuspousefilter,adult,weeksworked,hrlywage, ORIG_incpers_adj, orig_inc_adj, orig_wage_adj, ORIG_HINCP, ORIG_INCHH_ADJ, ORIG_POVINC_PU, filter_mw, filter_no_hs, filter_hs, INCPERS_ADJ_noW, INC_ADJ_noW, GetRaise, hrlywage2, WAGE_ADJ2, INCPERS_ADJ2, INC_ADJ2, IncPers_adj, Wage_adj, MINWAGE_RAISED, 
#          Povinc_pu, MINWAGE_RAISED_PU, HINCP, IncHH_adj, Income1, PERAGI, tu_spouse_flag, spouse_flag_tu, tuspousefilter, IncMJ_adj_TOT_TU, AGIMJ_TOT_TU, WAGEMJ_ADJ_TOT_TU)


# 			test <- as.data.frame(psych::describe(base_2013_clean))
# 			origin <- as.data.frame(psych::describe(base_2013_origin_clean))
# 			setwd('C:/Users/ELi/Desktop')
# 			write.csv(test, file = 'test.csv')
# 			write.csv(origin, file = 'origin.csv')
# 			## TEST IS SUCCESSFUL

# # Example: serialno 1991, hrlywage 8.060392

# #base_2013_origin_clean$filter_mw_test = base_2013_origin_clean$filter_mw
# base_2013_origin_clean$IncMJ_adj_TOT_TU_test = base_2013_origin_clean$IncMJ_adj_TOT_TU
# base_2013_origin_clean_cut <- base_2013_origin_clean[,c("SERIALNO","AGEP","hrlywage","IncMJ_adj_TOT_TU_test")]
# base_2013_clean <- base_2013_clean %>% 
# 	left_join(base_2013_origin_clean_cut, by=c("SERIALNO","AGEP","hrlywage")) %>%
# 	mutate(IncMJ_adj_TOT_TU_test = IncMJ_adj_TOT_TU_test - IncMJ_adj_TOT_TU)





