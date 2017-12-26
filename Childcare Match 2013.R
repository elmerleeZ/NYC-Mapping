#############################################################################
# Program Name:   Childcare Match
# Location:       \\Chgoldfs\pru\Quan work\MinWage\Minimum_Wage_by_Amount_Simulation\Run21- 9 an hour\childcare\Childcare Match 2013 - Replication.R
# Author:         
# Date Created:   
# Project:        
# Purpose:        
#############################################################################

# source('\\\\Chgoldfs\\pru\\Elmer_Work\\R\\R-Key-Function\\Start up.R')

library(dplyr)
library(tidyverse)
library(matrixStats) # use the rowMin function


########################################################################
## SIPP value codes  ##

#sex# 
	#1=male# #2=female#
#race# 
	#1=white# #2=black# #3=asian# #4=other#
#ms (marital status)# 
	#1=married, spouse present# #2=married, spouse absent# #3=widowed# #4=divorced# #5=separated# #6=never married#
#educate#
	#<39=no degree# #39=HS# #40=some college# #41=certificate# #43=associate's degree# #44=BA# #45=MA# #46=professional degree# #47=PhD#

#CPI Index#
cpipanel04 <- 191.80
cpipanel08 <- 216.50
cpi01 <- 177.042
cpi04 <- 188.908
cpi05 <- 195.267
cpi06 <- 201.550
cpi07 <- 207.335
cpi08 <- 215.247
cpi09 <- 214.549
cpi10 <- 218.079
cpi11 <- 224.746
cpi12 <- 229.594
cpi13 <- 232.957

#CPI Childcare Index#
cpiccpanel04 <- 191.55
cpiccpanel08 <- 236.97
cpicc01 <- 164.1
cpicc04 <- 187.1
cpicc05 <- 195.350
cpicc06 <- 206.000
cpicc07 <- 214.877
cpicc08 <- 224.635
cpicc09 <- 232.828
cpicc10 <- 240.432
cpicc11 <- 247.064
cpicc12 <- 253.141
cpicc13 <- 259.406

# inflation Rate
infl08 = cpi13/cpipanel08
infl04 = cpi13/cpipanel04

# inflation rate
inflcc08 = cpicc13/cpiccpanel08
inflcc04 = cpicc13/cpiccpanel04


###############################################################################
## Generate New Variables
###############################################################################

setwd("\\\\chgoldfs\\pru\\5-Misc\\PMM Research\\")
library(foreign)


## Merge in Data, create variables
load("SIPPfam0408.RData")
sipp_raw <- sipp
sipp_var <- read.csv("SIPPvar0408.csv",header=TRUE)
sipp_var$hhid <- paste(sipp_var$ssuid,sipp_var$shhadid,sipp_var$spanel,sep="")
sipp_var$epppnum <- as.numeric(as.character(sipp_var$epppnum))

sipp_clean <- sipp_raw %>% 
		select(hhid,epppnum,famidnew,tage,umpfhh, famsize, chf, ch1317f, fref, ch05f, ch612f) %>%
		mutate(hhid = as.character(hhid), epppnum = as.numeric(as.character(epppnum))) %>%
		left_join(sipp_var, by=c("hhid","epppnum")) %>%
		
# Generate demo dummy variables#
		mutate(
			racedum = ifelse(race%in%c(1,3),1,0),
			mardum = ifelse(ms==1,1,ifelse(fref==1 & umpfhh==1,1,0)),
			fsdum = ifelse(thfdstp>0,1,0)) %>%
		mutate(nowrk = ifelse(tage>=18 & hrswrked==0 & mardum==1,1,0)) %>%
		mutate(
			edufem = ifelse(sex==2,eeducate,0),
			tptotinc = ifelse(tptotinc<0,0,tptotinc), # clean - less than 0 to be 0
			tpearn = ifelse(tpearn<0,0,tpearn), # clean - less than 0 to be 0
			feminc = ifelse(sex==2,tpearn,0)) %>%
		
# Aggregate family-level variables#
		group_by(famidnew) %>%
		mutate(
			wkhrs = sum(hrswrked,na.rm=T), 
			nowrkf = sum(nowrk,na.rm=T), 
			eduf = max(eeducate,na.rm=T),
			edufemf = max(edufem,na.rm=T),
			incf = sum(tptotinc,na.rm=T),
			earnf = sum(tpearn,na.rm=T),
			femincf = sum(feminc,na.rm=T)) %>% 
    ungroup() %>%
		mutate(
			hsdum  = ifelse(eduf==39,1,0),
			scdum  = ifelse(eduf >= 40 & eduf <= 43,1,0),
			collegedum  = ifelse(eduf==44,1,0),
			graddum  = ifelse(eduf>=45,1,0),
			graddum  = ifelse(eduf>=45,1,0)
			) %>%

## Compute equivalized family income#
		mutate(
			adults = famsize - chf, 
			equiv = ifelse(chf==0 & adults>0, (adults^0.7)/2.1576693, 
						ifelse(chf>0 & adults==1, ((1+0.8+(0.5*(chf-1)))^0.7)/2.1576693, 
							ifelse(chf>0 & adults>1,((adults+(0.5*chf))^0.7)/2.1576693, NA )))) %>%

## Equivalize and inflation-adjust income#
		mutate(
			# family income
			incf = (incf/equiv)*12,
			incf = ifelse(spanel==2008, incf*infl08, 
						ifelse(spanel==2004, incf*infl04, NA)), 
			earnf = ifelse(earnf <0, 0, earnf), 
			lninc = log(1+incf), 
			# family earning
			earnf = (earnf/equiv)*12,
			earnf = ifelse(spanel==2008, earnf*infl08, 
						ifelse(spanel==2004, earnf*infl04, NA)), 
			earnf = ifelse(earnf <0, 0, earnf), 
			lnearn = log(1+earnf), 
			# female earning
			femincf = (femincf/equiv)*12,
			femincf = ifelse(spanel==2008, femincf*infl08, 
						ifelse(spanel==2004, femincf*infl04, NA)), 
			femincf = ifelse(femincf <0, 0, femincf), 
			# proportoin of female income
			propfeminc = ifelse(incf>0,(femincf/incf)*100,0)) %>%
		group_by(famidnew) %>%
		mutate(childcaref = sum(childcare, na.rm = T)) %>% 
		ungroup() %>%

## Inflation-adjust childcare spending#
		mutate(
			# adjusted childcare spending
			childcaref = ifelse(spanel==2008,childcaref*inflcc08, 
							ifelse(spanel==2004,childcaref*inflcc04, NA)), 
			ch12u = ifelse(tage<=12,1,0)) %>%
		group_by(famidnew) %>%
		mutate(
			ch12uf = sum(ch12u, na.rm = T)) %>% ungroup()

## Subset
sipp_2 <- subset(sipp_clean,(sipp_clean$rdesgpnt==1 & sipp_clean$wkhrs>0 & sipp_clean$nowrkf==0 & sipp_clean$incf>0 & sipp_clean$ch12uf>0 & sipp_clean$urban==1))


###############################################################################
## GAM Regression
###############################################################################

# Run GAM regressions on married, unmarried and pooled samples#
# install.packages('mgcv')
library(mgcv)

#GAM Fit
fit1.gam_test <- gam(childcaref~s(earnf)+s(ch05f,k=3)+s(ch1317f,k=3)+
					fsdum+hsdum+scdum+collegedum+graddum+s(adults,k=5)+s(propfeminc), 
					weights=wpfinwgt,subset=mardum==1,data=sipp_2)

#Model Summary
summary(fit1.gam_test)

#Plot Smoothed Functions
#plot(fit1.gam_test,scale=0,se=2,shade=TRUE,pages=1,shade.col=5)

#GAM Fit
fit2.gam_test <- gam(childcaref~s(earnf)+s(ch05f,k=3)+s(ch1317f,k=3)+
					fsdum+hsdum+scdum+collegedum+graddum+s(adults,k=5)+s(propfeminc),
					weights=wpfinwgt,subset=mardum==0,data=sipp_2)

#Model Summary
summary(fit2.gam_test)

#Plot Smoothed Functions
#plot(fit2.gam_test,scale=0,se=2,shade=TRUE,pages=1,shade.col=5)

#predicted means using separate regressions for married/unmarried#
sipp_2$pm <- 0
sipp_2$pm[sipp_2$mardum==1] <- predict(fit1.gam_test)
sipp_2$pm[sipp_2$mardum==0] <- predict(fit2.gam_test)

sipp_2 <- sipp_2 %>%
	select(famidnew,hhid,epppnum,tage,umpfhh,famsize, chf,ch05f,ch612f,ch1317f,fref,ssuid, shhadid,spanel,urban,sex,race,eeducate, tpearn,tptotinc,thfdstp,rentprop,lookwrk,rdesgpnt, wffinwgt,childcare,ms,hrswrked,wpfinwgt,racedum, mardum,fsdum,nowrk,wkhrs,nowrkf,eduf, edufem,edufemf,hsdum,scdum,collegedum, graddum, incf,earnf,feminc,femincf,adults,equiv, lninc,lnearn,propfeminc, childcaref, ch12u,ch12uf,pm)


###############################################################################
## Import ACS Data
###############################################################################

## Code
#ceo_povrel#
	#1=head# #2=spouse# #3=other# #4=dependent#
#SFR#
	#1=husband/wife no children# #2=husband/wife with children# #3=parent in parent/child subfamily# #4-6=child#
#matrix column names#
	#1=SERIALNO,2=SPORDER,3=ceo_povrel,4=SFN,5=SFR,6=mardum,7=sex#
# pm = predicted means

## Read in ACS file 
# acs_raw <- read.csv("\\\\Chgoldfs\\pru\\Quan work\\MinWage\\Minimum_Wage_by_Amount_Simulation\\Run21- 9 an hour\\childcare\\RUN19 ACS 2013 Childcare Variables.csv",header=T)
acs_raw <- read.csv("//Chgoldfs/pru/Elmer_Work/MinWage/Run21- 9 an hour/childcare/childcare_var_2013.csv",header=T)

## Change of NA to 0 ##
acs_raw[is.na(acs_raw)==T] <- 0  ## why we need to do this? 


## Generate designated parent flag 
acs_clean <- acs_raw %>%
		mutate(dpar = ifelse(PovRel == 1 & mardum == 0,1, 
						ifelse(PovRel == 1 & mardum ==1 & SEX == 2,1,0))) 
		acs_clean$dpar[acs_clean$SFR == 3] <- 1
		acs_clean$dpar[acs_clean$SFR == 2] <- 1
		# restrict data
		acs_clean <- acs_clean %>% filter(dpar==1 & incf>0)

# Compute equivalized income
acs_final <- acs_clean %>%
		mutate(
			chf = ch05f+ch612f+ch1317f,
			equiv = ifelse(chf==0 & adults>0, (adults^0.7)/2.1576693, 
						ifelse(chf>0 & adults==1, ((1+0.8+(0.5*(chf-1)))^0.7)/2.1576693, 
							ifelse(chf>0 & adults>1,((adults+(0.5*chf))^0.7)/2.1576693, NA ))),
			incf = incf/equiv,
			earnf = earnf/equiv,
			propfeminc = propfeminc*100,
			incf = ifelse(incf<0,0,incf),
			earnf = ifelse(earnf<0,0,earnf),
			lninc = log(1+incf),
			lnearn = log(1+earnf))

## Compute predicted means for split samples
acs_final$pm <- 0
acs_final$pm[acs_final$mardum==1] <- predict(fit1.gam_test,newdata=acs_final[acs_final$mardum==1,])
acs_final$pm[acs_final$mardum==0] <- predict(fit2.gam_test,newdata=acs_final[acs_final$mardum==0,])

## Create pmm dataset#

sipppmm_test <- sipp_2 %>%
		mutate(
			SERIALNO = 0,
			SPORDER = 0,
			id = 0) %>%
		select(SERIALNO,SPORDER,id,mardum,pm,childcaref)

pmm_test <- acs_final %>%
		mutate(id = 1, childcaref = 0) %>%
		select(SERIALNO,SPORDER,id,mardum,pm,childcaref) %>%
		rbind(sipppmm_test)

source("\\\\chgoldfs\\pru\\5-Misc\\PMM Research\\PMM Childcare.R") # to sucessfully run this code, the file name must be pmm
pmm <- pmm_test
pmm <- match(pmm)
pmm_test <- pmm
pmm_test <- pmm_test[,c("SERIALNO","SPORDER","pmm")]


# acs_input <- read.csv("\\\\Chgoldfs\\pru\\Quan work\\MinWage\\Minimum_Wage_by_Amount_Simulation\\Run21- 9 an hour\\childcare\\RUN19_ACS 2013 Childcare Adjust Variables.csv",header=T)
acs_input <- read.csv("//Chgoldfs/pru/Elmer_Work/MinWage/Run21- 9 an hour/childcare/childcare_var_adjust_2013.csv",header=T)
acs_input[is.na(acs_input)==T] <- 0
pmm_test$df <- 1

acs_clean2 <- acs_input %>%
		merge(pmm_test,by=c("SERIALNO","SPORDER"),all.x=T)
## Change of NA to 0 ##
acs_clean2[is.na(acs_clean2)==T] <- 0		

acs_2 <- acs_clean2 %>%
		filter(FILER==1 | SPOUSE_U_MHU>0) %>%
		group_by(SERIALNO,NO_MHU) %>%
		summarise( # here must use summarise; mutate won't work - what's the reason? 
			weekswrk_1 = min(weeksworked, na.rm = T), 
			mpartinc = sum(earn, na.rm = T)
			) %>% ungroup() 


acs_clean3 <- acs_clean2 %>%
		left_join(acs_2,by=c("SERIALNO","NO_MHU")) %>%
		mutate(FILESTAT = ifelse(FILESTAT==0 & df==1, 1, FILESTAT) ) %>% 
		mutate(
			weekswrk_1 = ifelse(is.na(weekswrk_1), 0, weekswrk_1), 
			mpartinc = ifelse(is.na(mpartinc), 0, mpartinc)
			) %>%
		mutate(
			unmarpartinc = ifelse(FILER==1 & REL_MHU==1, mpartinc - earn, 0), 
			unpart = 0 
			) %>% 
		group_by(SERIALNO,NO_MHU) %>%
		mutate(
			# unpartf = sum(unpart, na.rm = T)
			unpartf = 0
			) %>% ungroup() %>%
		mutate(
			spouseinc = ifelse(FILESTAT==1 & PersIncMJ_adj_TOT_TU > 0, PersIncMJ_adj_TOT_TU - earn,0), 
			childcostlimit = ifelse(FILESTAT==1 & earn>=spouseinc, spouseinc, 
								ifelse(FILESTAT==1 & earn<spouseinc, earn, 
									ifelse(FILESTAT%in%c(2,3) & unpartf==0,earn, 
										ifelse(FILESTAT%in%c(2,3) & unpartf>0 & REL_MHU==1 & earn>=unmarpartinc,unmarpartinc, 
											ifelse(FILESTAT%in%c(2,3) & unpartf>0 & REL_MHU==1 & earn<unmarpartinc, earn,0))))), 
			childtax = 0) %>%
		# select(SERIALNO, NO_MHU, SPORDER, PovUnit_PU, REL_MHU, REL_U_MHU, SPOUSE_U_MHU, PovRel, Income1, PersIncMJ_adj_TOT_TU, earn, weeksworked, FILESTAT, FILER, TaxUnit_TU, pmm, df, weekswrk_1, mpartinc, unmarpartinc, unpart, unpartf, spouseinc, childcostlimit, childtax)
			 # the default value of childtax is 0
		select(SERIALNO,SPORDER,PovUnit_PU,PovRel,Income1,childcostlimit,weekswrk_1,FILESTAT,pmm,childtax,df,TaxUnit_TU) %>%
		# group_by(SERIALNO,SPORDER,PovUnit_PU,PovRel,TaxUnit_TU) %>% # the reason for such grouping is that the function min() takes only one value
		mutate(childtax = ifelse(FILESTAT %in% c(1:3) & childcostlimit >=0, pmin(childcostlimit,pmm*weekswrk_1),
							ifelse(FILESTAT %in% c(1:3) & childcostlimit <0, pmin(0,pmm*weekswrk_1),childtax))) 
		# %>% ungroup() 



			test <- as.data.frame(psych::describe(acs_clean3))
			origin <- as.data.frame(psych::describe(acs))
			setwd('C:/Users/ELi/Desktop')
			write.csv(test, file = 'test.csv')
			write.csv(origin, file = 'origin.csv')
			## TEST IS SUCCESSFUL


acs_clean3_1 <- acs_clean3 %>%
		group_by(SERIALNO,TaxUnit_TU) %>%
		summarise(childcare_TU = sum(childtax, na.rm = T)) %>% ungroup() 
		names(acs_clean3_1) <- c("SERIALNO","TaxUnit_TU","childcare_TU")

acs_clean3_2 <- acs_clean3 %>%
		group_by(SERIALNO,PovUnit_PU) %>%
		summarise(childcare_PU = sum(childtax, na.rm = T)) %>% ungroup() 
		names(acs_clean3_2) <- c("SERIALNO","PovUnit_PU","childcare_PU")

childcare_out <- acs_clean3 %>%
		merge(acs_clean3_1, by = c("SERIALNO","TaxUnit_TU"), all.x = T) %>%
		merge(acs_clean3_2, by = c("SERIALNO","PovUnit_PU"), all.x = T) %>%
		mutate(
			childcare = childtax,
			childcare_TU = ifelse(is.na(childcare_TU), 0, childcare_TU), 
			childcare_PU = ifelse(is.na(childcare_PU), 0, childcare_PU)
			) %>%
		#select(SERIALNO,PovUnit_PU,TaxUnit_TU,SPORDER,PovRel, Income1,childcostlimit,weekswrk_1,FILESTAT,pmm,childtax,df,childcare_TU,childcare_PU)
		select(SERIALNO,SPORDER,childcare,childcare_TU,childcare_PU,df)


#write.table(out,file="C:\\WORK\\2013 Report Work\\Work at 253\\Minimum_Wage_by_Amount_Simulation\\Run19- 8_75_an_hour\\childcare\\RUN19 Childcare Values 2013 GAM Fit.txt",sep="\t",row.names=F)
# write.table(childcare_out,file="//Chgoldfs/pru/Elmer_Work/MinWage/Run21- 9 an hour/childcare/RUN19 Childcare Values 2013 GAM Fit.txt",sep="\t",row.names=F)
write.csv(childcare_out,file="//Chgoldfs/pru/Elmer_Work/MinWage/Run21- 9 an hour/childcare/childcare_out.csv",row.names=F)


############################################################
############################################################
####################    END PROGRAM    #####################
############################################################
############################################################

########################################################################
## Left Questions  ##


# what's the difference between merge(all.x = T) and left_join