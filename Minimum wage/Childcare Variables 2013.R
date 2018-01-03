#############################################################################
# Program Name:   ACS 2013 Childcare Variables - Elmer.R
# Location:       //Chgoldfs/pru/Elmer_Work/R/MinWage/childcare/ACS 2013 Childcare Adjust Variables - Elmer.R
# Author:         Elmer Li
# Date Created:   12-07-2017
# Project:        Min Wage
# Purpose:        Generate child care variables for min wage evalutaoin
#############################################################################

library(dplyr)
library(tidyverse)
library(foreign)

########################################################################
## Import data  ##

# base_2013_clean <- read.spss("//Chgoldfs/pru/Elmer_Work/MinWage/Run21- 9 an hour/2013 min wage BASE.sav", to.data.frame=TRUE)
base_2013_clean <- read.csv("//Chgoldfs/pru/Elmer_Work/MinWage/Run21- 9 an hour/base_2013_clean.csv")


##################################################
# Adjusted Variables
##################################################

childcare_var_adjust_2013 <- base_2013_clean %>%
  mutate(
    adult = ifelse(is.na(AGEP), 0, ifelse(AGEP>=16,1,0)), # Due to Quan's code, the defination is still >= 16
    earn = Wage_adj+Semp_adj, 
    weeksworked = ifelse(adult ==1 & !is.na(WKW),
                         ifelse(WKW ==1, 50, 
                                ifelse(WKW == 2, 49, 
                                       ifelse(WKW == 3, 44,
                                              ifelse(WKW == 4, 33,
                                                     ifelse(WKW == 5, 20,
                                                            ifelse(WKW == 6, 6,0)))))),ifelse(adult ==1 & is.na(WKW),0,NA))) %>%
  select(AGEP,adult,WKW,SERIALNO,SPORDER,PovUnit_PU,NO_MHU,REL_MHU,REL_U_MHU,SPOUSE_U_MHU,PovRel,Income1,PersIncMJ_adj_TOT_TU,earn,weeksworked,FILESTAT, FILER, TaxUnit_TU)

##################################################
# Variables
##################################################

childcare_var_2013 <- base_2013_clean %>%
  mutate(
    # earned income
    earn = Wage_adj+Semp_adj, 
    # Ethnicity
    racedum = ifelse(Ethnicity == 1 | Ethnicity == 3, 1, 0), 
    # child variables
    ch05 = ifelse(AGEP >= 0 & AGEP <= 5, 1, 0), 
    ch612 = ifelse(AGEP >= 6 & AGEP <= 12, 1, 0), 
    ch1317 = ifelse(AGEP >= 13 & AGEP <= 17, 1, 0), 
    ch12u = ifelse(AGEP <= 12, 1, 0), 
    # female 
    feminc = ifelse(SEX == 2, IncPers_adj, 0), 
    # food stamp
    fsdum = ifelse(FS == 1,1,0),
    # rent property
    #RentOrOwn = as.numeric(as.character(RentOrOwn)), 
    rentprop = ifelse(RentOrOwn > 1, 1, 0),
    # work related 
    adult = ifelse(is.na(AGEP), 0, ifelse(AGEP>=16,1,0)),     
    weeksworked = ifelse(adult ==1 & !is.na(WKW),
                         ifelse(WKW ==1, 50, 
                                ifelse(WKW == 2, 49, 
                                       ifelse(WKW == 3, 44,
                                              ifelse(WKW == 4, 33,
                                                     ifelse(WKW == 5, 20,
                                                            ifelse(WKW == 6, 6,0)))))),ifelse(adult ==1 & is.na(WKW),0,NA))) %>%
  group_by(SERIALNO, PovUnit_PU) %>%
  mutate(
    earnf = sum(earn),
    ch05f = sum(ch05),
    ch612f = sum(ch612),
    ch1317f = sum(ch1317),
    ch12uf = sum(ch12u),
    femincf = max(feminc,na.rm = T), 
    educate = max(SCHL,na.rm = T), 
    wkhrs = sum(WKHP)) %>%
  ungroup() %>%
  mutate(
    # female income proportion
    propfeminc= ifelse(Income1 > 0, femincf/Income1, 0),
    # education
    hsdum = ifelse(educate >= 16 & educate <= 17, 1, 0), 
    scdum = ifelse(educate >= 18 & educate <= 20, 1, 0), 
    collegedum = ifelse(educate == 21, 1, 0), 
    graddum = ifelse(educate > 21, 1, 0)) %>%
  # rename variables
  mutate(
    mardum = SpousePres_PU, 
    incf = Income1, 
    adults = PovAdults_PU) %>%
  # filter based on certain condition
  filter(ch12uf > 0 & weeksworked > 0 & PAOC != 4) %>% 
  select(SERIALNO,SPORDER,PovRel,PovUnit_PU,SEX,mardum,incf,earnf,racedum,adults,
         propfeminc,wkhrs,ch05f,ch612f,ch1317f,ch12uf,fsdum,hsdum,scdum,collegedum,graddum,rentprop,weeksworked,SFR,SFN,PWGTP)

##################################################
# Export
##################################################


setwd("//Chgoldfs/pru/Elmer_Work/MinWage/Run21- 9 an hour/childcare")

write.csv(childcare_var_2013, file = 'childcare_var_2013.csv')
write.csv(childcare_var_adjust_2013, file = 'childcare_var_adjust_2013.csv')

