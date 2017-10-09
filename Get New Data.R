#############################################################################
# Program Name:   Get New Data.R
# Location:       /Users/zongyangli/Google Drive/Job/RA - Public Good/Public Good Code_Resources - Copy/R Code/Get New Data.R
# Author:         
# Date Created:   09/24/2017
# Project: UJA      
# Purpose: Get new data about population and housing conditions    
#############################################################################


source("/Users/zongyangli/Documents/Github/R-Key-functions/Start up.R")

library(acs)
library(acsplus)
library(dplyr)
library(readxl)

install.packages("devtools")
library(devtools)
install_github("walkerke/tidycensus")


install.packages("tidycensus")
library(tidycensus)

api.key.install(key = "1d9149a1d6d2eb5c7cfbc7dd7c74d092c415a8a8")
census_api_key("1d9149a1d6d2eb5c7cfbc7dd7c74d092c415a8a8", install = T)


########################################################################
## Try tidycensus  ##

income <- get_acs(geography = "place", variables = "B19013_001", survey = "acs1", endyear = 2010)
  # works for year 2012-2016, fail for year 2010 - 2011

variables <- c(
  acs_vars("B01003",1), # Total Population
  acs_vars("B19013",1), # Median Household Income in past 12 months
  # acs_vars("B11001",1), # Household
  acs_vars("B25003",1:3) # Household, Owner Occupied Renter Occupied
)

for(i in 2010:2016) {
    acs_raw <- get_acs(geography = 'public use microdata area', state = '36', variables = variables, survey = "acs1", endyear = i)
    assign("acs_raw_" %S% i,acs_raw)
}


########################################################################
## Set up geography variables  ##

# nyc_tracts <- geo.make(state = "NY", county = c(5, 47, 61, 81, 85), tract = "*")
ny_puma <- geo.make(state = "NY", puma = "*")

variables <- c(
  acs_vars("B01003",1), # Total Population
  acs_vars("B19013",1), # Median Household Income in past 12 months
  # acs_vars("B11001",1), # Household
  acs_vars("B25003",1:3), # Household, Owner Occupied Renter Occupied
  acs_vars("B25064",1) # Median Gross Rent
  # acs_vars("B25031",4:5) # Median Gross Rent 3 & 4 bedrooms
)



########################################################################
## Get ACS Directly from Fact Finder  ##

acs_B01003_2016 <- read.csv("/Users/zongyangli/Google Drive/Job/RA - Public Good/Public Good Code_Resources - Copy/Data/ACS/ACS_16_1YR_B01003_with_ann.csv") %>% filter(GEO.id != 'Id') %>% mutate(B01003e001 = HD01_VD01, year = 2016) %>% select(GEO.id2,year,B01003e001)
acs_B01003_2011 <- read.csv("/Users/zongyangli/Google Drive/Job/RA - Public Good/Public Good Code_Resources - Copy/Data/ACS/ACS_11_1YR_B01003_with_ann.csv") %>% filter(GEO.id != 'Id') %>% mutate(B01003e001 = HD01_VD01, year = 2011) %>% select(GEO.id2,year,B01003e001)
acs_B01003_2010 <- read.csv("/Users/zongyangli/Google Drive/Job/RA - Public Good/Public Good Code_Resources - Copy/Data/ACS/ACS_10_1YR_B01003_with_ann.csv") %>% filter(GEO.id != 'Id') %>% mutate(B01003e001 = HD01_VD01, year = 2010) %>% select(GEO.id2,year,B01003e001)
acs_B01003 <- Reduce(function(x, y) merge(x, y, by=c('GEO.id2','year','B01003e001'), all=TRUE), list(acs_B01003_2016, acs_B01003_2011, acs_B01003_2010))


acs_B19013_2016 <- read.csv("/Users/zongyangli/Google Drive/Job/RA - Public Good/Public Good Code_Resources - Copy/Data/ACS/ACS_16_1YR_B19013_with_ann.csv") %>% filter(GEO.id != 'Id') %>% mutate(B19013e001 = HD01_VD01, year = 2016) %>% select(GEO.id2,year,B19013e001)
acs_B19013_2011 <- read.csv("/Users/zongyangli/Google Drive/Job/RA - Public Good/Public Good Code_Resources - Copy/Data/ACS/ACS_11_1YR_B19013_with_ann.csv") %>% filter(GEO.id != 'Id') %>% mutate(B19013e001 = HD01_VD01, year = 2011) %>% select(GEO.id2,year,B19013e001)
acs_B19013_2010 <- read.csv("/Users/zongyangli/Google Drive/Job/RA - Public Good/Public Good Code_Resources - Copy/Data/ACS/ACS_10_1YR_B19013_with_ann.csv") %>% filter(GEO.id != 'Id') %>% mutate(B19013e001 = HD01_VD01, year = 2010) %>% select(GEO.id2,year,B19013e001)
acs_B19013 <- Reduce(function(x, y) merge(x, y, by=c('GEO.id2','year','B19013e001'), all=TRUE), list(acs_B19013_2016, acs_B19013_2011, acs_B19013_2010))


acs_B25003_2016 <- read.csv("/Users/zongyangli/Google Drive/Job/RA - Public Good/Public Good Code_Resources - Copy/Data/ACS/ACS_16_1YR_B25003_with_ann.csv") %>% filter(GEO.id != 'Id') %>% mutate(B25003e001 = HD01_VD01,B25003e002 = HD01_VD02,B25003e003 = HD01_VD03, year = 2016) %>% select(GEO.id2,year,B25003e001,B25003e002,B25003e003)
acs_B25003_2011 <- read.csv("/Users/zongyangli/Google Drive/Job/RA - Public Good/Public Good Code_Resources - Copy/Data/ACS/ACS_11_1YR_B25003_with_ann.csv") %>% filter(GEO.id != 'Id') %>% mutate(B25003e001 = HD01_VD01,B25003e002 = HD01_VD02,B25003e003 = HD01_VD03, year = 2011) %>% select(GEO.id2,year,B25003e001,B25003e002,B25003e003)
acs_B25003_2010 <- read.csv("/Users/zongyangli/Google Drive/Job/RA - Public Good/Public Good Code_Resources - Copy/Data/ACS/ACS_10_1YR_B25003_with_ann.csv") %>% filter(GEO.id != 'Id') %>% mutate(B25003e001 = HD01_VD01,B25003e002 = HD01_VD02,B25003e003 = HD01_VD03, year = 2010) %>% select(GEO.id2,year,B25003e001,B25003e002,B25003e003)
acs_B25003 <- Reduce(function(x, y) merge(x, y, by=c('GEO.id2','year','B25003e001','B25003e002','B25003e003'), all=TRUE), list(acs_B25003_2016, acs_B25003_2011, acs_B25003_2010))

acs_B25064_2016 <- get_acs(geography = 'public use microdata area', state = '36', variables = 'B25064_001', survey = "acs1", endyear = 2016) %>% mutate(GEO.id2 = GEOID, B25064e001 = estimate, year = 2016) %>% select(GEO.id2,year,B25064e001)
acs_B25064_2011 <- read.csv("/Users/zongyangli/Google Drive/Job/RA - Public Good/Public Good Code_Resources - Copy/Data/ACS/ACS_11_1YR_B25064_with_ann.csv") %>% filter(GEO.id != 'Id') %>% mutate(B25064e001 = HD01_VD01, year = 2011) %>% select(GEO.id2,year,B25064e001)
acs_B25064_2010 <- read.csv("/Users/zongyangli/Google Drive/Job/RA - Public Good/Public Good Code_Resources - Copy/Data/ACS/ACS_10_1YR_B25064_with_ann.csv") %>% filter(GEO.id != 'Id') %>% mutate(B25064e001 = HD01_VD01, year = 2010) %>% select(GEO.id2,year,B25064e001)
acs_B25064 <- Reduce(function(x, y) merge(x, y, by=c('GEO.id2','year','B25064e001'), all=TRUE), list(acs_B25064_2016, acs_B25064_2011, acs_B25064_2010))


acs_raw_2 <- Reduce(function(x, y) merge(x, y, by=c('GEO.id2','year'), all=TRUE), list(acs_B01003,acs_B19013,acs_B25003,acs_B25064)) %>%
    mutate(end_year = year,geoid = GEO.id2) %>% select(-year,-GEO.id2)



########################################################################
## Download ACS data  ##

acs_raw_1 <- acs_download(geo = ny_puma,
                        geoid = geoid_puma,
                        vars = variables,
                        span = 1,
                        end_years = c(2012:2015),
                        get_se = FALSE,
                        dataset = "acs")

# Get neighborhood name
geo_name <- read.csv("/Users/zongyangli/Google Drive/Job/RA - Public Good/Public Good Code_Resources - Copy/Data/ACS/ACS_16_1YR_B01003_with_ann.csv") %>% filter(GEO.id != 'Id') %>% 
    mutate(geo_name = GEO.display.label, geoid = as.character(GEO.id2)) %>% select(geo_name,geoid) %>% unique()


## Merge all together
acs_raw <- Reduce(function(x, y) merge(x, y, by=c('geoid','end_year',"B01003e001","B19013e001","B25003e001","B25003e002","B25003e003","B25064e001"), all=TRUE), list(acs_raw_1,acs_raw_2)) %>%
    select(-geo_name) %>% left_join(geo_name, by = 'geoid')


acs_clean <- acs_raw %>% 
    mutate(puma = substr(geoid,3,7),
           year = as.character(end_year), 
           neighborhood = geo_name,
           tot_pop = as.numeric(B01003e001),
           hhinc_med = as.numeric(B19013e001),
           tot_hhd = as.numeric(B25003e001),
           hhd_owner = as.numeric(B25003e002),
           hhd_renter = as.numeric(B25003e003),
           rent_median = as.numeric(B25064e001)) %>%
    filter(puma %in% c(
        '04001', '04002', '04003', '04004', '04005', '04006', '04011', '04012', '04014', '04015'
      )) %>%
    select(geoid,puma,year,neighborhood,tot_pop,hhinc_med,tot_hhd,hhd_owner,hhd_renter,rent_median) %>%
    mutate(pct_owner = hhd_owner/tot_hhd,pct_renter = hhd_renter/tot_hhd)


## Regulate Name:
for (i in 1:14){
    acs_clean <- as.data.frame(lapply(acs_clean, gsub, pattern='NYC-Brooklyn Community District ' %S% i %S% '--', replacement=''))
}
    acs_clean <- as.data.frame(lapply(acs_clean, gsub, pattern=' PUMA, New York', replacement=''))
    acs_clean <- as.data.frame(lapply(acs_clean, gsub, pattern=' PUMA; New York', replacement=''))


########################################################################
## Get Median Housing Price Data for Single Household from Furman  ##

hh_price_med_raw <- read_csv('/Users/zongyangli/Google Drive/Job/RA - Public Good/Public Good Code_Resources - Copy/Data/Housing/communitydistrict-mediansalespriceperunit1familybuilding2016.csv')

hh_price_med <- hh_price_med_raw %>%
    select(-short_name,-long_name) %>%
    mutate(
      neighborhood = `Community District`,
      boro = substr(neighborhood,1,2),
      suboro = substr(neighborhood,4,5),
      boro_code = ifelse(boro == 'BX','037',ifelse(boro == 'MN','038',ifelse(boro == 'SI','039',ifelse(boro == 'BK','040','041')))),
      puma = boro_code %S% suboro
      ) %>%
    select(-boro,-suboro,-boro_code) %>%
    select(puma,neighborhood,everything()) %>%
    gather(`2000`,`2001`,`2002`,`2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2010`, `2014`, `2015`,`2016`, key = "year", value = "hh_price_med") %>%
    select(puma,year,hh_price_med)


########################################################################
## Merge All  ##

uja_demo_raw <- acs_clean %>% left_join(hh_price_med,by = c("puma", "year"))


base <- uja_demo_raw %>% 
    filter(year == 2010) %>%
    mutate(
      tot_pop_2010 = tot_pop,
      tot_hhd_2010 = tot_hhd,
      hhinc_med_2010 = hhinc_med,
      hhinc_med_2010 = hhinc_med,
      pct_owner_2010 = pct_owner,
      pct_renter_2010 = pct_renter,
      hh_price_med_2010 = hh_price_med,
      rent_median_2010 = rent_median
      ) %>% 
    select(-tot_pop,-hhinc_med,-hhinc_med,-pct_owner,-pct_renter,-hh_price_med,-year,-neighborhood,-tot_hhd,-hhd_owner,-hhd_renter,-rent_median)



setwd('/Users/zongyangli/Google Drive/Job/RA - Public Good/Output')
write.csv(uja_demo_raw,"uja_demo_raw.csv")
write.csv(base,"base.csv")
uja_demo_raw <- read.csv("uja_demo_raw.csv") %>% mutate(puma = str_pad(puma, 5, pad = "0")) %>% select(-X)
base <- read.csv("base.csv") %>% mutate(puma = str_pad(puma, 5, pad = "0")) %>% select(-X)



uja_demo <- uja_demo_raw %>% 
    left_join(base, by = c('puma','geoid')) %>%
    mutate(
      pct_chg_tot_pop = (as.numeric(tot_pop)/ as.numeric(tot_pop_2010))*100 - 100,
      pct_chg_tot_hhd = (as.numeric(tot_hhd)/ as.numeric(tot_hhd_2010))*100 - 100,
      pct_chg_hhinc_med = (as.numeric(hhinc_med)/as.numeric(hhinc_med_2010))*100 - 100,
      pct_chg_hhinc_med = (as.numeric(hhinc_med)/as.numeric(hhinc_med_2010))*100 - 100,
      pct_chg_pct_owner = (as.numeric(pct_owner)/as.numeric(pct_owner_2010))*100 - 100,
      pct_chg_pct_renter = (as.numeric(pct_renter)/as.numeric(pct_renter_2010))*100 - 100,
      pct_chg_hh_price_med = (as.numeric(hh_price_med)/as.numeric(hh_price_med_2010))*100 - 100,
      pct_chg_rent_median = (as.numeric(rent_median)/as.numeric(rent_median_2010))*100 - 100
      ) %>%
    select(-tot_pop_2010,-tot_hhd_2010,-hhinc_med_2010,-hhinc_med_2010,-pct_owner_2010,-pct_renter_2010,-hh_price_med_2010,rent_median_2010)


setwd('/Users/zongyangli/Google Drive/Job/RA - Public Good/Output')
write.csv(uja_demo,"uja_demo.csv")


###############################################################################
## Graph
###############################################################################


# Try to build function
ggplot_set <- function(data,rate,rate_type,year){
  ggplot(data=subset(data,!is.na(rate)), aes(x=year, y=rate,colour=rate_type)) + 
    theme_minimal() +
    #scale_colour_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, space = "Lab", na.value = "grey50", guide = "colourbar")
    theme(
      text=element_text(family="Times"), 
      panel.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray80", size = 0.3),
      panel.grid.major.x = element_blank(),
      axis.text.y = element_text( size = 12),
      axis.text.x = element_text(size = 10),
      axis.ticks = element_line(colour = 'gray50'),
      axis.ticks.length = unit(.25, "cm"),
      axis.ticks.x = element_line(colour = "black"),
      axis.ticks.y = element_blank(),
      plot.title = element_text(hjust = 0.5, vjust=2.12,size = 12),
      legend.title = element_blank(),
      legend.text = element_text(size = 8),
      legend.key.size =  unit(0.10, "in"), # legend size
      legend.position =  "bottom" # legend position
      )
}

graph_tot_pop <- ggplot_set(uja_demo,uja_demo$pct_chg_tot_pop,uja_demo$neighborhood,uja_demo$year) + 
    geom_line(size=1.5) +
    scale_colour_brewer(palette="Spectral") +
    guides(color=guide_legend(nrow=3,byrow=TRUE)) + 
    labs(title = "% Change in Total Population 2010-2016",x="",y="% Change") + 
    scale_x_continuous(breaks= c(2010, seq(2010,2016,1))) 



uja_demo$year <- as.factor(uja_demo$year)

  ggplot(data=subset(uja_demo,!is.na(pct_chg_tot_pop)), aes(x=neighborhood, y=pct_chg_tot_pop,colour=year)) + 
    theme_minimal() +
    #scale_colour_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, space = "Lab", na.value = "grey50", guide = "colourbar")
    theme(
      text=element_text(family="Times"), 
      panel.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray80", size = 0.3),
      panel.grid.major.x = element_blank(),
      axis.text.y = element_text( size = 12),
      axis.text.x = element_text(size = 10),
      axis.ticks = element_line(colour = 'gray50'),
      axis.ticks.length = unit(.25, "cm"),
      axis.ticks.x = element_line(colour = "black"),
      axis.ticks.y = element_blank(),
      plot.title = element_text(hjust = 0.5, vjust=2.12,size = 12),
      legend.title = element_blank(),
      legend.text = element_text(size = 8),
      legend.key.size =  unit(0.10, "in"), # legend size
      legend.position =  "bottom" # legend position
      )+ 
    geom_bar()



graph_tot_pop <- ggplot_set(uja_demo,uja_demo$tot_pop,uja_demo$year,uja_demo$neighborhood) + 
    geom_bar(stat="identity")

    
ggsave("/Users/zongyangli/Desktop/Pct Change in Total Population 2010-2016.pdf", plot = graph_tot_pop, width = 8.7, height = 6, units = "in")












########################################################################
## Get IPUMS Data  ##

setwd('/Users/zongyangli/Google Drive/Job/RA - Public Good/Public Good Code_Resources - Copy/Data/IPUMS')

## Import IPUMS

ipums_raw <- read_csv('usa_00008.csv')
names(ipums_raw) <- names(ipums_raw) %>% str_to_lower() %>% make.names(unique = TRUE, allow_ = T)

ipums_clean <- ipums_raw %>% 
					mutate(puma = str_pad(puma, 5, pad = "0")) %>%
					dplyr::filter(puma %in% c(
			            "03710", "03705", "03708", "03707", "03706", "03701", "03709", "03703", "03704", "03702", "04001", "04004", "04003", "04002", "04008", "04005", "04012",
			            "04006", "04011", "04013", "04017", "04014", "04018", "04015", "04016", "04007", "04010", "04009", "03810", "03809", "03807", "03808", "03806", "03805", "03802", "03803", "03804", "03801",
			            "04101", "04109", "04102", "04107", "04110", "04108", "04103", "04106", "04111", "04113", "04104", "04112", "04105", "04114", "03903", "03902", "03901"))
						          
						   renter = ifelse(ownershp == 2, 1, 0),
						   owner = ifelse(renter ==1,0,1),

						   )

## CPI Adjuter

cpi_raw <- read_excel("/Users/zongyangli/Google Drive/Job/RA - Public Good/Public Good Code_Resources - Copy/Data/CPI/SeriesReport-20170924213606_8aa78f.xlsx", skip = 11)
cpi_raw <- cpi_raw %>% select(Year, Annual) %>% mutate(cpi_adj = Annual/263.365) # 2016 Price






