
source("\\\\chgoldfs\\pru\\12-2005-2013 Poverty Report\\Data\\UNITS\\Functions for Units V2.R")
library(dplyr)
library(tidyverse)

# File below contains only variables contained in WIC_INVARS List

########################################################################
## Clean Data  ##

# WICFile <- paste("C:\\WORK\\2013 Report Work\\Work at 253\\Minimum wage simulation\\WIC\\RUN7 2013 WIC InVars.CSV",sep="")
# WICFile <- paste("\\\\Chgoldfs\\pru\\Quan work\\MinWage\\Minimum_Wage_by_Amount_Simulation\\Run21- 9 an hour\\WIC\\RUN7 2013 WIC InVars.CSV",sep="")
 
WW_raw <- read.csv('//Chgoldfs/pru/Elmer_Work/MinWage/Run21- 9 an hour/WIC/WIC_var_2013.csv')
WW_input <- WW_raw %>% 
	# Create new vars, delete old ones
	mutate(
		receive_WIC_baby = 0, 
		receive_WIC_child = 0, 
		receive_WIC_woman = 0, 
		TOTAL_RECEIVING_WIC = 0, 
		WICReceipt_PU = 0, 
		WICValue_PU = 0, 
		wic_baby_elig = WIC_baby_elig, 
		wic_child_elig = WIC_child_elig, 
		wic_woman_elig = WIC_woman_elig) %>%
	# Turn NA into 0s
	mutate(
		wic_baby_elig = ifelse(is.na(wic_baby_elig), 0, wic_baby_elig), 
		wic_child_elig = ifelse(is.na(wic_child_elig), 0, wic_child_elig), 
		wic_woman_elig = ifelse(is.na(wic_woman_elig), 0, wic_woman_elig), 
		wic_baby_sum = ifelse(is.na(wic_baby_sum), 0, wic_baby_sum), 
		pov_unit_prob_value = ifelse(is.na(pov_unit_prob_value), 0, pov_unit_prob_value)) %>%
	mutate(
		FLAG = ifelse(wic_baby_elig == 1, 1, 
				ifelse(wic_child_elig == 1, 2, 
					ifelse(wic_woman_elig & wic_baby_sum > 0, 3, 0)))) 

# Specify protion of data that would be assigned with WIC
 Yr <- WW_input[1, "YEAR"]
 BabyTarget <- .530
 ChildTarget <- .308
 WomanTarget <- .321

# Sort data by index/probabilities to assign
 WW_input <- WW_input[order(WW_input$pov_unit_prob_value,decreasing=TRUE),]

# Seperate out the dataset based on flag
 WIC_Baby <- WW_input[WW_input[,"FLAG"]==1 , ]
 WIC_Child <- WW_input[WW_input["FLAG"]==2 , ]
 WIC_Woman <- WW_input[WW_input["FLAG"]==3 , ]
 Other <- WW_input[WW_input["FLAG"]==0 , ]


########################################################################
## WIC - Babies  ##

 BabyTotal <- sum(WIC_Baby[,"PWGTP"])
 ind.stop <- 0

 for (i in 1:nrow(WIC_Baby)){
 	if ((sum(WIC_Baby[1:i,"PWGTP"])/BabyTotal)>=BabyTarget) { # if the sum of the weight is greater than target, then enough
		ind.stop <- i
		break
	}
 }

 if (sum(WIC_Baby[1:ind.stop,"PWGTP"])-BabyTotal >
   abs(sum(WIC_Baby[1:(ind.stop-1),"PWGTP"])-BabyTotal) ) { ind.stop <- (i-1) } # compare the gap between adding one more subject or not at the margin

 WIC_Baby$receive_WIC_baby[1:ind.stop] <- 1


########################################################################
## WIC - Children  ##

 ChildTotal <- sum(WIC_Child[,"PWGTP"])
 ind.stop <- 0
 for (i in 1:nrow(WIC_Child)){
 	if ((sum(WIC_Child[1:i,"PWGTP"])/ChildTotal)>=ChildTarget) {
		ind.stop <- i
		break
	}
 }
 if (sum(WIC_Child[1:ind.stop,"PWGTP"])-ChildTotal >
   abs(sum(WIC_Child[1:(ind.stop-1),"PWGTP"])-ChildTotal) ) { ind.stop <- (i-1) }

 WIC_Child$receive_WIC_child[1:ind.stop] <- 1

########################################################################
## WIC - Woman  ##

 WomanTotal <- sum(WIC_Woman[,"PWGTP"])
 ind.stop <- 0
 for (i in 1:nrow(WIC_Woman)){
 	if ((sum(WIC_Woman[1:i,"PWGTP"])/WomanTotal)>=WomanTarget) {
		ind.stop <- i
		break
	}
 }
 if (sum(WIC_Woman[1:ind.stop,"PWGTP"])-WomanTotal >
   abs(sum(WIC_Woman[1:(ind.stop-1),"PWGTP"])-WomanTotal) ) { ind.stop <- (i-1) }

 WIC_Woman$receive_WIC_woman[1:ind.stop] <- 1

# Bind all together
 WW_all <- rbind(WIC_Baby,WIC_Child,WIC_Woman,Other)


 ########################################################################
 ## Merge All Together  ##

WIC_out_2013 <- WW_all %>%
 	mutate(WIC_total = receive_WIC_baby + receive_WIC_child + receive_WIC_woman) %>%
 	group_by(SERIALNO,PovUnit_PU) %>%
 	mutate(WICReceipt_PU = sum(WIC_total, na.rm = T)) %>% ungroup() %>%
 	mutate(WICValue_PU = WICReceipt_PU * (12 * 54.71)) %>% # this value will change by year
 #The numbers multiplied by 12 at end of formulas are average monthly benefit for NY State residents, found at: http://www.fns.usda.gov/pd/25wifyavgfd$.htm
 	select(SERIALNO, SPORDER, receive_WIC_baby, receive_WIC_child, receive_WIC_woman, WIC_total, WICReceipt_PU, WICValue_PU)

 	# library(psych)
		# 	test <- as.data.frame(psych::describe(WW_output))
		# 	origin <- as.data.frame(psych::describe(WW_FINAL))
		# 	setwd('C:/Users/ELi/Desktop')
		# 	write.csv(test, file = 'test.csv')
		# 	write.csv(origin, file = 'origin.csv')
			## TEST IS SUCCESSFUL

 # OUTFILENAME<-paste("C:\\WORK\\2013 Report Work\\Work at 253\\Minimum wage simulation\\WIC\\RUN7 2013_WIC_OutVars.csv",sep="")
 # OUTFILENAME<-paste("\\\\Chgoldfs\\pru\\Quan work\\MinWage\\Minimum_Wage_by_Amount_Simulation\\Run21- 9 an hour\\WIC\\RUN7 2013_WIC_OutVars.csv",sep="")
 # write.csv(WW_output,OUTFILENAME,row.names=F,na="")

write.csv(WIC_out_2013, file = '//Chgoldfs/pru/Elmer_Work/MinWage/Run21- 9 an hour/WIC/WIC_out_2013.csv')  



