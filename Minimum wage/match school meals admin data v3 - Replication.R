
setwd("\\\\chgoldfs\\pru\\Krampner Work\\2013 Report\\School Meals\\School Meals administrative target")
source("\\\\chgoldfs\\pru\\12-2005-2013 Poverty Report\\Data\\UNITS\\Functions for Units V2.R")


library(psych)
library(dplyr)
library(tidyverse)


########################################################################
## Prepare Data  ##

# File below contains only variables contained in MEALS_INVARS List

Yr <- 2013

SchoolMealsFile <- paste("\\\\Chgoldfs\\pru\\Quan work\\MinWage\\Minimum_Wage_by_Amount_Simulation\\Run21- 9 an hour\\School Meals\\RUN7 2013 NYC School Meals InVars.csv",sep="")
SM_raw <- read.csv(SchoolMealsFile,header = TRUE, sep = ",",na.strings="NA",comment.char="")

## Create new, basic clean
SM_clean <- SM_raw %>% 
  # Create new variables
  mutate(
    receive_free_SB = 0,
     receive_reduced_SB = 0,
     receive_free_lunch = 0,
     receive_reduced_lunch = 0,
     Free_SB_povunit = 0,
     Reduced_SB_povunit = 0,
     SchoolBreakfastValue_PU = 0,
     Free_lunch_povunit = 0,
     Reduced_lunch_povunit = 0,
     SchoolLunchValue_PU = 0,
     FLAG = 0) %>%
  # Change NA to 0
  mutate(
    Elem_age = ifelse(is.na(Elem_age),0,Elem_age),
    MS_age = ifelse(is.na(MS_age),0,MS_age),
    HS_age = ifelse(is.na(HS_age),0,HS_age),
    pov_unit_prob_value = ifelse(is.na(pov_unit_prob_value),0,pov_unit_prob_value)) 
 

 Yr <- SM[1, "YEAR"]
 MealValuesFile <- c("\\\\chgoldfs\\pru\\12-2005-2013 Poverty Report\\Data\\School Meals\\Admin Data for code.CSV")
 MealValues <- read.csv(MealValuesFile,header = TRUE, sep = ",",na.strings="NA",comment.char="")
 POPCOUNT <- MealValues[MealValues[ , "YEAR"]==Yr , ]

## locate target values
 elem_free_break = POPCOUNT[POPCOUNT[,"Grade"]=="Elem" & POPCOUNT[,"Meal"]=="BRK" & POPCOUNT[ , "MealType"]=="Free", "Pop"]	
 elem_reduc_break = POPCOUNT[POPCOUNT[,"Grade"]=="Elem" & POPCOUNT[,"Meal"]=="BRK" & POPCOUNT[ , "MealType"]=="Reduc", "Pop"]
 middle_free_break = POPCOUNT[POPCOUNT[,"Grade"]=="MS" & POPCOUNT[,"Meal"]=="BRK" & POPCOUNT[ , "MealType"]=="Free", "Pop"]
 middle_reduc_break = POPCOUNT[POPCOUNT[,"Grade"]=="MS" & POPCOUNT[,"Meal"]=="BRK" & POPCOUNT[ , "MealType"]=="Reduc", "Pop"]
 high_free_break = POPCOUNT[POPCOUNT[,"Grade"]=="HS" & POPCOUNT[,"Meal"]=="BRK" & POPCOUNT[ , "MealType"]=="Free", "Pop"]
 high_reduc_break = POPCOUNT[POPCOUNT[,"Grade"]=="HS" & POPCOUNT[,"Meal"]=="BRK" & POPCOUNT[ , "MealType"]=="Reduc", "Pop"]

 elem_free_lunch = POPCOUNT[POPCOUNT[,"Grade"]=="Elem" & POPCOUNT[,"Meal"]=="LUN" & POPCOUNT[ , "MealType"]=="Free", "Pop"]	
 elem_reduc_lunch = POPCOUNT[POPCOUNT[,"Grade"]=="Elem" & POPCOUNT[,"Meal"]=="LUN" & POPCOUNT[ , "MealType"]=="Reduc", "Pop"]
 middle_free_lunch = POPCOUNT[POPCOUNT[,"Grade"]=="MS" & POPCOUNT[,"Meal"]=="LUN" & POPCOUNT[ , "MealType"]=="Free", "Pop"]
 middle_reduc_lunch = POPCOUNT[POPCOUNT[,"Grade"]=="MS" & POPCOUNT[,"Meal"]=="LUN" & POPCOUNT[ , "MealType"]=="Reduc", "Pop"]
 high_free_lunch = POPCOUNT[POPCOUNT[,"Grade"]=="HS" & POPCOUNT[,"Meal"]=="LUN" & POPCOUNT[ , "MealType"]=="Free", "Pop"]
 high_reduc_lunch = POPCOUNT[POPCOUNT[,"Grade"]=="HS" & POPCOUNT[,"Meal"]=="LUN" & POPCOUNT[ , "MealType"]=="Reduc", "Pop"]

 SM_clean[SM_clean[,"elig_free_lunch"]==1 & SM_clean[,"Elem_age"]==1 , "FLAG"] <- 1
 SM_clean[SM_clean[,"elig_reduced_lunch"]==1 & SM_clean[,"Elem_age"]==1 , "FLAG"] <- 2
 SM_clean[SM_clean[,"elig_free_lunch"]==1 & SM_clean[,"MS_age"]==1 , "FLAG"] <- 3
 SM_clean[SM_clean[,"elig_reduced_lunch"]==1 & SM_clean[,"MS_age"]==1 , "FLAG"] <- 4
 SM_clean[SM_clean[,"elig_free_lunch"]==1 & SM_clean[,"HS_age"]==1 , "FLAG"] <- 5
 SM_clean[SM_clean[,"elig_reduced_lunch"]==1 & SM_clean[,"HS_age"]==1 , "FLAG"] <- 6

# sort data by index/probabilities
 SM_clean <- SM_clean[order(SM_clean$pov_unit_prob_value,decreasing=TRUE),]

# seperate out datasets
 elemFree <- SM_clean[SM_clean[,"FLAG"]==1 , ]
 elemReduc <- SM_clean[SM_clean["FLAG"]==2 , ]
 middleFree <- SM_clean[SM_clean["FLAG"]==3 , ]
 middleReduc <- SM_clean[SM_clean["FLAG"]==4 , ]
 highFree <- SM_clean[SM_clean["FLAG"]==5 , ]
 highReduc <- SM_clean[SM_clean["FLAG"]==6 , ]
 Other <- SM_clean[SM_clean["FLAG"]==0 , ]


##################################################
# Breakfast
##################################################


########################################################################
## Elementary SCHOOL BREAKFAST ##

## Free
 elemFree_output <- elemFree %>%
    mutate(
      elem_free_break = elem_free_break, # to use this value in mutate, have to create a new variable
      REMAINING = elem_free_break - sum(PWGTP), 
      PWGTP_cum = cumsum(PWGTP), 
      PWGTP_cum_diff = abs(elem_free_break - PWGTP_cum), 
      PWGTP_cum_diff_smallest = min(PWGTP_cum_diff), 
      receive_free_SB = ifelse(elem_free_break >= sum(PWGTP), 1, 
                          ifelse(PWGTP_cum < elem_free_break,1,
                            # following is to check whether above or below the thresold has the smallest diff
                            ifelse(PWGTP_cum - elem_free_break == PWGTP_cum_diff_smallest, 1, 0)))) %>%
    select(-elem_free_break, -PWGTP_cum_diff, PWGTP_cum_diff_smallest)


## Reduced price
# Build a new function
REMAINING <- elemFree_output$REMAINING[1]
cumsumfromright <- function(x) rev(cumsum(rev(x)))

elemReduc_output <- elemReduc %>%
    mutate(
      REMAINING = REMAINING,
      elem_reduc_break = elem_reduc_break, 
      PWGTP_cum = cumsum(PWGTP), 
      PWGTP_cum_diff_remain = abs(PWGTP_cum - REMAINING), 
      PWGTP_cum_diff_remain_min = min(PWGTP_cum_diff_remain), 
      PWGTP_cum_diff_remain_reduc = abs(PWGTP_cum - elem_reduc_break),
      PWGTP_cum_diff_remain_reduc_min = min(PWGTP_cum_diff_remain_reduc), 
      PWGTP_cum_right = cumsumfromright(PWGTP), 
      receive_reduced_SB = ifelse(REMAINING > 0, 
                              ifelse(elem_reduc_break >= sum(PWGTP), 1, 
                              ifelse(REMAINING + elem_reduc_break >= sum(PWGTP),
                                  ifelse(PWGTP_cum_right <= elem_reduc_break, 1,0), 
                              ifelse(PWGTP_cum <= REMAINING,1,ifelse(PWGTP_cum - REMAINING == PWGTP_cum_diff_remain_min, 1, 0)))), 
                           ifelse(elem_reduc_break >= sum(PWGTP), 1,
                              ifelse(PWGTP_cum < elem_reduc_break,1,
                              ifelse(PWGTP_cum - elem_reduc_break == PWGTP_cum_diff_remain_reduc_min, 1, 0)))), 
      receive_free_SB = ifelse(REMAINING > 0,
                              ifelse(elem_reduc_break >= sum(PWGTP), 0,
                                ifelse(PWGTP_cum_right <= elem_reduc_break, 0,1)),0)) %>%
    select(-REMAINING, -elem_reduc_break, -PWGTP_cum, -PWGTP_cum_diff_remain, -PWGTP_cum_diff_remain_min, -PWGTP_cum_diff_remain_reduc, -PWGTP_cum_diff_remain_reduc_min, -PWGTP_cum_right)


########################################################################
## MIDDLE SCHOOL BREAKFAST ##

## Free
 middleFree_output <- middleFree %>%
    mutate(
      middle_free_break = middle_free_break, # to use this value in mutate, have to create a new variable
      REMAINING = middle_free_break - sum(PWGTP), 
      PWGTP_cum = cumsum(PWGTP), 
      PWGTP_cum_diff = abs(middle_free_break - PWGTP_cum), 
      PWGTP_cum_diff_smallest = min(PWGTP_cum_diff), 
      receive_free_SB = ifelse(middle_free_break >= sum(PWGTP), 1, 
                          ifelse(PWGTP_cum < middle_free_break,1,
                            # following is to check whether above or below the thresold has the smallest diff
                            ifelse(PWGTP_cum - middle_free_break == PWGTP_cum_diff_smallest, 1, 0)))) %>%
    select(-middle_free_break, -PWGTP_cum_diff, -PWGTP_cum, -PWGTP_cum_diff_smallest)


## Reduced price
# Build a new function
REMAINING <- middleFree_output$REMAINING[1]
cumsumfromright <- function(x) rev(cumsum(rev(x)))

middleReduc_output <- middleReduc %>%
    mutate(
      REMAINING = REMAINING,
      middle_reduc_break = middle_reduc_break, 
      PWGTP_cum = cumsum(PWGTP), 
      PWGTP_cum_diff_remain = abs(PWGTP_cum - REMAINING), 
      PWGTP_cum_diff_remain_min = min(PWGTP_cum_diff_remain), 
      PWGTP_cum_diff_remain_reduc = abs(PWGTP_cum - middle_reduc_break),
      PWGTP_cum_diff_remain_reduc_min = min(PWGTP_cum_diff_remain_reduc), 
      PWGTP_cum_right = cumsumfromright(PWGTP), 
      receive_reduced_SB = ifelse(REMAINING > 0, 
                              ifelse(middle_reduc_break >= sum(PWGTP), 1, 
                              ifelse(REMAINING + middle_reduc_break >= sum(PWGTP),
                                  ifelse(PWGTP_cum_right <= middle_reduc_break, 1,0), 
                              ifelse(PWGTP_cum <= REMAINING,1,ifelse(PWGTP_cum - REMAINING == PWGTP_cum_diff_remain_min, 1, 0)))), 
                           ifelse(middle_reduc_break >= sum(PWGTP), 1,
                              ifelse(PWGTP_cum < middle_reduc_break,1,
                              ifelse(PWGTP_cum - middle_reduc_break == PWGTP_cum_diff_remain_reduc_min, 1, 0)))), 
      receive_free_SB = ifelse(REMAINING > 0,
                              ifelse(middle_reduc_break >= sum(PWGTP), 0,
                                ifelse(PWGTP_cum_right <= middle_reduc_break, 0,1)),0)) %>%
    select(-REMAINING, -middle_reduc_break, -PWGTP_cum, -PWGTP_cum_diff_remain, -PWGTP_cum_diff_remain_min, -PWGTP_cum_diff_remain_reduc, -PWGTP_cum_diff_remain_reduc_min, -PWGTP_cum_right)



########################################################################
## HIGH SCHOOL BREAKFAST ##

## Free
 highFree_output <- highFree %>%
    mutate(
      high_free_break = high_free_break, # to use this value in mutate, have to create a new variable
      REMAINING = high_free_break - sum(PWGTP), 
      PWGTP_cum = cumsum(PWGTP), 
      PWGTP_cum_diff = abs(high_free_break - PWGTP_cum), 
      PWGTP_cum_diff_smallest = min(PWGTP_cum_diff), 
      receive_free_SB = ifelse(high_free_break >= sum(PWGTP), 1, # if Target > available weighted rows, then all assigned with free breakfast
                          ifelse(PWGTP_cum < high_free_break,1, # if Target > accumulative weighted rows, then assigned with free breakfast
                            # following is to check whether above or below the thresold has the smallest diff
                            ifelse(PWGTP_cum - high_free_break == PWGTP_cum_diff_smallest, 1, 0)))) %>%
    select(-high_free_break, -PWGTP_cum_diff, -PWGTP_cum, -PWGTP_cum_diff_smallest)


## Reduced price
# Build a new function
REMAINING <- highFree_output$REMAINING[1]
cumsumfromright <- function(x) rev(cumsum(rev(x)))

highReduc_output <- highReduc %>%
    mutate(
      REMAINING = REMAINING,
      high_reduc_break = high_reduc_break, 
      PWGTP_cum = cumsum(PWGTP), 
      PWGTP_cum_diff_remain = abs(PWGTP_cum - REMAINING), 
      PWGTP_cum_diff_remain_min = min(PWGTP_cum_diff_remain), 
      PWGTP_cum_diff_remain_reduc = abs(PWGTP_cum - high_reduc_break),
      PWGTP_cum_diff_remain_reduc_min = min(PWGTP_cum_diff_remain_reduc), 
      PWGTP_cum_right = cumsumfromright(PWGTP), 
      receive_reduced_SB = ifelse(REMAINING > 0, 
                              ifelse(high_reduc_break >= sum(PWGTP), 1, 
                              ifelse(REMAINING + high_reduc_break >= sum(PWGTP),
                                  ifelse(PWGTP_cum_right <= high_reduc_break, 1,0), 
                              ifelse(PWGTP_cum <= REMAINING,1,ifelse(PWGTP_cum - REMAINING == PWGTP_cum_diff_remain_min, 1, 0)))), 
                           ifelse(high_reduc_break >= sum(PWGTP), 1,
                              ifelse(PWGTP_cum < high_reduc_break,1,
                              ifelse(PWGTP_cum - high_reduc_break == PWGTP_cum_diff_remain_reduc_min, 1, 0)))), 
      receive_free_SB = ifelse(REMAINING > 0,
                              ifelse(high_reduc_break >= sum(PWGTP), 0,
                                ifelse(PWGTP_cum_right <= high_reduc_break, 0,1)),0)) %>%
    select(-REMAINING, -high_reduc_break, -PWGTP_cum, -PWGTP_cum_diff_remain, -PWGTP_cum_diff_remain_min, -PWGTP_cum_diff_remain_reduc, -PWGTP_cum_diff_remain_reduc_min, -PWGTP_cum_right)





##################################################
# Lunch
##################################################


########################################################################
## Elementary SCHOOL BREAKFAST ##

## Free
 elemFree_output <- elemFree_output %>%
    mutate(
      elem_free_lunch = elem_free_lunch, # to use this value in mutate, have to create a new variable
      REMAINING = elem_free_lunch - sum(PWGTP), 
      PWGTP_cum = cumsum(PWGTP), 
      PWGTP_cum_diff = abs(elem_free_lunch - PWGTP_cum), 
      PWGTP_cum_diff_smallest = min(PWGTP_cum_diff), 
      receive_free_lunch = ifelse(elem_free_lunch >= sum(PWGTP), 1, 
                          ifelse(PWGTP_cum < elem_free_lunch,1,
                            # following is to check whether above or below the thresold has the smallest diff
                            ifelse(PWGTP_cum - elem_free_lunch == PWGTP_cum_diff_smallest, 1, 0)))) %>%
    select(-elem_free_lunch,-PWGTP_cum, -PWGTP_cum_diff, -PWGTP_cum_diff_smallest)

## Reduced price
# Build a new function
REMAINING <- elemFree_output$REMAINING[1]
elemFree_output <- elemFree_output %>% select(-REMAINING)
cumsumfromright <- function(x) rev(cumsum(rev(x)))

elemReduc_output <- elemReduc_output %>%
    mutate(
      REMAINING = REMAINING,
      elem_reduc_lunch = elem_reduc_lunch, 
      PWGTP_cum = cumsum(PWGTP), 
      PWGTP_cum_diff_remain = abs(PWGTP_cum - REMAINING), 
      PWGTP_cum_diff_remain_min = min(PWGTP_cum_diff_remain), 
      PWGTP_cum_diff_remain_reduc = abs(PWGTP_cum - elem_reduc_lunch),
      PWGTP_cum_diff_remain_reduc_min = min(PWGTP_cum_diff_remain_reduc), 
      PWGTP_cum_right = cumsumfromright(PWGTP), 
      receive_reduced_lunch = ifelse(REMAINING > 0, 
                              ifelse(elem_reduc_lunch >= sum(PWGTP), 1, 
                              ifelse(REMAINING + elem_reduc_lunch >= sum(PWGTP),
                                  ifelse(PWGTP_cum_right <= elem_reduc_lunch, 1,0), 
                              ifelse(PWGTP_cum <= REMAINING,1,ifelse(PWGTP_cum - REMAINING == PWGTP_cum_diff_remain_min, 1, 0)))), 
                           ifelse(elem_reduc_lunch >= sum(PWGTP), 1,
                              ifelse(PWGTP_cum < elem_reduc_lunch,1,
                              ifelse(PWGTP_cum - elem_reduc_lunch == PWGTP_cum_diff_remain_reduc_min, 1, 0)))), 
      receive_free_lunch = ifelse(REMAINING > 0,
                              ifelse(elem_reduc_lunch >= sum(PWGTP), 0,
                                ifelse(PWGTP_cum_right <= elem_reduc_lunch, 0,1)),0)) %>%
    select(-REMAINING, -elem_reduc_lunch, -PWGTP_cum, -PWGTP_cum_diff_remain, -PWGTP_cum_diff_remain_min, -PWGTP_cum_diff_remain_reduc, -PWGTP_cum_diff_remain_reduc_min, -PWGTP_cum_right)


########################################################################
## MIDDLE SCHOOL lunchFAST ##

## Free
 middleFree_output <- middleFree_output %>%
    mutate(
      middle_free_lunch = middle_free_lunch, # to use this value in mutate, have to create a new variable
      REMAINING = middle_free_lunch - sum(PWGTP), 
      PWGTP_cum = cumsum(PWGTP), 
      PWGTP_cum_diff = abs(middle_free_lunch - PWGTP_cum), 
      PWGTP_cum_diff_smallest = min(PWGTP_cum_diff), 
      receive_free_lunch = ifelse(middle_free_lunch >= sum(PWGTP), 1, 
                          ifelse(PWGTP_cum < middle_free_lunch,1,
                            # following is to check whether above or below the thresold has the smallest diff
                            ifelse(PWGTP_cum - middle_free_lunch == PWGTP_cum_diff_smallest, 1, 0)))) %>%
    select(-middle_free_lunch, -PWGTP_cum_diff, -PWGTP_cum, -PWGTP_cum_diff_smallest)


## Reduced price
# Build a new function
REMAINING <- middleFree_output$REMAINING[1]
middleFree_output <- middleFree_output %>% select(-REMAINING)
cumsumfromright <- function(x) rev(cumsum(rev(x)))

middleReduc_output <- middleReduc_output %>%
    mutate(
      REMAINING = REMAINING,
      middle_reduc_lunch = middle_reduc_lunch, 
      PWGTP_cum = cumsum(PWGTP), 
      PWGTP_cum_diff_remain = abs(PWGTP_cum - REMAINING), 
      PWGTP_cum_diff_remain_min = min(PWGTP_cum_diff_remain), 
      PWGTP_cum_diff_remain_reduc = abs(PWGTP_cum - middle_reduc_lunch),
      PWGTP_cum_diff_remain_reduc_min = min(PWGTP_cum_diff_remain_reduc), 
      PWGTP_cum_right = cumsumfromright(PWGTP), 
      receive_reduced_lunch = ifelse(REMAINING > 0, 
                              ifelse(middle_reduc_lunch >= sum(PWGTP), 1, 
                              ifelse(REMAINING + middle_reduc_lunch >= sum(PWGTP),
                                  ifelse(PWGTP_cum_right <= middle_reduc_lunch, 1,0), 
                              ifelse(PWGTP_cum <= REMAINING,1,ifelse(PWGTP_cum - REMAINING == PWGTP_cum_diff_remain_min, 1, 0)))), 
                           ifelse(middle_reduc_lunch >= sum(PWGTP), 1,
                              ifelse(PWGTP_cum < middle_reduc_lunch,1,
                              ifelse(PWGTP_cum - middle_reduc_lunch == PWGTP_cum_diff_remain_reduc_min, 1, 0)))), 
      receive_free_lunch = ifelse(REMAINING > 0,
                              ifelse(middle_reduc_lunch >= sum(PWGTP), 0,
                                ifelse(PWGTP_cum_right <= middle_reduc_lunch, 0,1)),0)) %>%
    select(-REMAINING, -middle_reduc_lunch, -PWGTP_cum, -PWGTP_cum_diff_remain, -PWGTP_cum_diff_remain_min, -PWGTP_cum_diff_remain_reduc, -PWGTP_cum_diff_remain_reduc_min, -PWGTP_cum_right)



########################################################################
## HIGH SCHOOL lunchFAST ##

## Free
 highFree_output <- highFree_output %>%
    mutate(
      high_free_lunch = high_free_lunch, # to use this value in mutate, have to create a new variable
      REMAINING = high_free_lunch - sum(PWGTP), 
      PWGTP_cum = cumsum(PWGTP), 
      PWGTP_cum_diff = abs(high_free_lunch - PWGTP_cum), 
      PWGTP_cum_diff_smallest = min(PWGTP_cum_diff), 
      receive_free_lunch = ifelse(high_free_lunch >= sum(PWGTP), 1, # if Target > available weighted rows, then all assigned with free lunchfast
                          ifelse(PWGTP_cum < high_free_lunch,1, # if Target > accumulative weighted rows, then assigned with free lunchfast
                            # following is to check whether above or below the thresold has the smallest diff
                            ifelse(PWGTP_cum - high_free_lunch == PWGTP_cum_diff_smallest, 1, 0)))) %>%
    select(-high_free_lunch, -PWGTP_cum_diff, -PWGTP_cum, -PWGTP_cum_diff_smallest)


## Reduced price
# Build a new function
REMAINING <- highFree_output$REMAINING[1]
highFree_output <- highFree_output %>% select(-REMAINING)
cumsumfromright <- function(x) rev(cumsum(rev(x)))

highReduc_output <- highReduc_output %>%
    mutate(
      REMAINING = REMAINING,
      high_reduc_lunch = high_reduc_lunch, 
      PWGTP_cum = cumsum(PWGTP), 
      PWGTP_cum_diff_remain = abs(PWGTP_cum - REMAINING), 
      PWGTP_cum_diff_remain_min = min(PWGTP_cum_diff_remain), 
      PWGTP_cum_diff_remain_reduc = abs(PWGTP_cum - high_reduc_lunch),
      PWGTP_cum_diff_remain_reduc_min = min(PWGTP_cum_diff_remain_reduc), 
      PWGTP_cum_right = cumsumfromright(PWGTP), 
      receive_reduced_lunch = ifelse(REMAINING > 0, 
                              ifelse(high_reduc_lunch >= sum(PWGTP), 1, 
                              ifelse(REMAINING + high_reduc_lunch >= sum(PWGTP),
                                  ifelse(PWGTP_cum_right <= high_reduc_lunch, 1,0), 
                              ifelse(PWGTP_cum <= REMAINING,1,ifelse(PWGTP_cum - REMAINING == PWGTP_cum_diff_remain_min, 1, 0)))), 
                           ifelse(high_reduc_lunch >= sum(PWGTP), 1,
                              ifelse(PWGTP_cum < high_reduc_lunch,1,
                              ifelse(PWGTP_cum - high_reduc_lunch == PWGTP_cum_diff_remain_reduc_min, 1, 0)))), 
      receive_free_lunch = ifelse(REMAINING > 0,
                              ifelse(high_reduc_lunch >= sum(PWGTP), 0,
                                ifelse(PWGTP_cum_right <= high_reduc_lunch, 0,1)),0)) %>%
    select(-REMAINING, -high_reduc_lunch, -PWGTP_cum, -PWGTP_cum_diff_remain, -PWGTP_cum_diff_remain_min, -PWGTP_cum_diff_remain_reduc, -PWGTP_cum_diff_remain_reduc_min, -PWGTP_cum_right)



school_meal_out <- rbind(elemFree_output, elemReduc_output, middleFree_output, middleReduc_output, highFree_output, highReduc_output,Other)
school_meal_out <- school_meal_out[order(school_meal_out[,"SERIALNO"],school_meal_out[,"PovUnit_PU"],decreasing=FALSE),]

school_meal_out <- school_meal_out %>%
    group_by(SERIALNO,PovUnit_PU) %>%
    mutate(
      Free_SB_povunit = sum(receive_free_SB),
      Reduced_SB_povunit = sum(receive_reduced_SB),
      Free_lunch_povunit = sum(receive_free_lunch),
      Reduced_lunch_povunit = sum(receive_reduced_lunch)
      )

 

 if (Yr==2005) {
	school_meal_out$SchoolBreakfastValue_PU <- 175 * ((school_meal_out$Free_SB_povunit * 1.23) + (school_meal_out$Reduced_SB_povunit * 1.23)) 
	school_meal_out$SchoolLunchValue_PU <- 175 * ((school_meal_out$Free_lunch_povunit * 2.451) + (school_meal_out$Reduced_lunch_povunit * 2.055)) 
 }
 if (Yr==2006) {
	school_meal_out$SchoolBreakfastValue_PU <- 175 * ((school_meal_out$Free_SB_povunit * 1.27) + (school_meal_out$Reduced_SB_povunit * 1.27)) 
	school_meal_out$SchoolLunchValue_PU <- 175 * ((school_meal_out$Free_lunch_povunit * 2.505) + (school_meal_out$Reduced_lunch_povunit * 2.109)) 
 }
 if (Yr==2007) {
	school_meal_out$SchoolBreakfastValue_PU <- 175 * ((school_meal_out$Free_SB_povunit * 1.31) + (school_meal_out$Reduced_SB_povunit * 1.31)) 
	school_meal_out$SchoolLunchValue_PU <- 175 * ((school_meal_out$Free_lunch_povunit * 2.630) + (school_meal_out$Reduced_lunch_povunit * 2.230)) 
 }
 if (Yr==2008) {
	school_meal_out$SchoolBreakfastValue_PU <- 175 * ((school_meal_out$Free_SB_povunit * 1.35) + (school_meal_out$Reduced_SB_povunit * 1.35)) 
	school_meal_out$SchoolLunchValue_PU <- 175 * ((school_meal_out$Free_lunch_povunit * 2.726) + (school_meal_out$Reduced_lunch_povunit *2.36)) 
 }
 if (Yr==2009) {
	school_meal_out$SchoolBreakfastValue_PU <- 175 * ((school_meal_out$Free_SB_povunit * 1.40) + (school_meal_out$Reduced_SB_povunit * 1.40)) 
	school_meal_out$SchoolLunchValue_PU <- 175 * ((school_meal_out$Free_lunch_povunit * 2.855 ) + (school_meal_out$Reduced_lunch_povunit * 2.455)) 
 }
 if (Yr==2010) {
	school_meal_out$SchoolBreakfastValue_PU <- 175 * ((school_meal_out$Free_SB_povunit * 1.46) + (school_meal_out$Reduced_SB_povunit * 1.46)) 
	school_meal_out$SchoolLunchValue_PU <- 175 * ((school_meal_out$Free_lunch_povunit * 2.91) + (school_meal_out$Reduced_lunch_povunit * 2.508)) 
 }
 if (Yr==2011) {
	school_meal_out$SchoolBreakfastValue_PU <- 175 * ((school_meal_out$Free_SB_povunit * 1.48) + (school_meal_out$Reduced_SB_povunit * 1.48)) 
	school_meal_out$SchoolLunchValue_PU <- 175 * ((school_meal_out$Free_lunch_povunit * 2.956) + (school_meal_out$Reduced_lunch_povunit * 2.556)) 
 }
 if (Yr==2012) {
	school_meal_out$SchoolBreakfastValue_PU <- 175 * ((school_meal_out$Free_SB_povunit * 1.51) + (school_meal_out$Reduced_SB_povunit * 1.51)) 
	school_meal_out$SchoolLunchValue_PU <- 175 * ((school_meal_out$Free_lunch_povunit * 3.036) + (school_meal_out$Reduced_lunch_povunit * 2.636)) 
 }
 if (Yr==2013) {
	school_meal_out$SchoolBreakfastValue_PU <- 175 * ((school_meal_out$Free_SB_povunit * 1.55) + (school_meal_out$Reduced_SB_povunit * 1.55)) 
	school_meal_out$SchoolLunchValue_PU <- 175 * ((school_meal_out$Free_lunch_povunit * 3.121) + (school_meal_out$Reduced_lunch_povunit * 2.721)) 
 }

# School breakfast reimbursement values for 2013 at: http://www.fns.usda.gov/sites/default/files/NAPs12-13.pdf
# 2013 School lunch values from Jessica Semega of Census from CPS values 9/22/14

OUTVARS <- c("SERIALNO",
 "SPORDER",
 "receive_free_SB",
 "receive_reduced_SB",
 "receive_free_lunch",
 "receive_reduced_lunch",
 "Free_SB_povunit",
 "Reduced_SB_povunit",
 "SchoolBreakfastValue_PU",
 "Free_lunch_povunit",
 "Reduced_lunch_povunit",
 "SchoolLunchValue_PU")

 #OUTFILENAME<-paste("C:\\WORK\\2013 Report Work\\Work at 253\\Minimum wage simulation\\School Meals\\RUN7_2013_School_Meal_OutVars.csv",sep="")
 OUTFILENAME<-paste("\\\\Chgoldfs\\pru\\Quan work\\MinWage\\Minimum_Wage_by_Amount_Simulation\\Run21- 9 an hour\\School Meals\\RUN7_2013_School_Meal_OutVars.csv",sep="")
 SM_FINAL <- school_meal_out[,OUTVARS]
write.csv(SM_FINAL,OUTFILENAME,row.names=F,na="")






###############################################


xtabs(school_meal_out$PWGTP[school_meal_out$Elem_age==1] ~ school_meal_out$receive_reduced_SB[school_meal_out$Elem_age==1],exclude=NULL,na.action=na.pass)
print(elem_reduc_break)
xtabs(school_meal_out$PWGTP[school_meal_out$Elem_age==1] ~ school_meal_out$receive_free_SB[school_meal_out$Elem_age==1],exclude=NULL,na.action=na.pass)
print(elem_free_break)
xtabs(school_meal_out$PWGTP[school_meal_out$MS_age==1] ~ school_meal_out$receive_reduced_SB[school_meal_out$MS_age==1],exclude=NULL,na.action=na.pass)
print(middle_reduc_break)
xtabs(school_meal_out$PWGTP[school_meal_out$MS_age==1] ~ school_meal_out$receive_free_SB[school_meal_out$MS_age==1],exclude=NULL,na.action=na.pass)
print(middle_free_break)
xtabs(school_meal_out$PWGTP[school_meal_out$HS_age==1] ~ school_meal_out$receive_reduced_SB[school_meal_out$HS_age==1],exclude=NULL,na.action=na.pass)
print(high_reduc_break)
xtabs(school_meal_out$PWGTP[school_meal_out$HS_age==1] ~ school_meal_out$receive_free_SB[school_meal_out$HS_age==1],exclude=NULL,na.action=na.pass)
print(high_free_break)

xtabs(school_meal_out$PWGTP[school_meal_out$Elem_age==1] ~ school_meal_out$receive_reduced_lunch[school_meal_out$Elem_age==1],exclude=NULL,na.action=na.pass)
print(elem_reduc_lunch)
xtabs(school_meal_out$PWGTP[school_meal_out$Elem_age==1] ~ school_meal_out$receive_free_lunch[school_meal_out$Elem_age==1],exclude=NULL,na.action=na.pass)
print(elem_free_lunch)
xtabs(school_meal_out$PWGTP[school_meal_out$MS_age==1] ~ school_meal_out$receive_reduced_lunch[school_meal_out$MS_age==1],exclude=NULL,na.action=na.pass)
print(middle_reduc_lunch)
xtabs(school_meal_out$PWGTP[school_meal_out$MS_age==1] ~ school_meal_out$receive_free_lunch[school_meal_out$MS_age==1],exclude=NULL,na.action=na.pass)
print(middle_free_lunch)
xtabs(school_meal_out$PWGTP[school_meal_out$HS_age==1] ~ school_meal_out$receive_reduced_lunch[school_meal_out$HS_age==1],exclude=NULL,na.action=na.pass)
print(high_reduc_lunch)
xtabs(school_meal_out$PWGTP[school_meal_out$HS_age==1] ~ school_meal_out$receive_free_lunch[school_meal_out$HS_age==1],exclude=NULL,na.action=na.pass)
print(high_free_lunch)





