# Title: Extract 2004, 2007, 2011, 2014, and 2017 Ag Census and Ag Survey data from RNASSQS
# Subtitle: Production (Weight and $) -- For Use with WiNDC v3.0.3
# Authors: Rachel Pompa
# Date: 2 April 2023

### READ IN NASS QUICK STAT DATA -----------------------------------------------

library(tidyverse)
library(rnassqs)
library(readxl)
library(dplyr)

#API Token
Sys.setenv(NASSQS_TOKEN = "387C433A-76A4-3C05-BB4A-3FC16CAB36D6")

# Provides the NASS Quick Stat parameter names
nassqs_params()

# Documentation on all of the parameters is available at https://quickstats.nass.usda.gov/api#param_define
nassqs_params("agg_level_desc", "source_desc")


states = c("AL","AK","AZ","AR","CA","CO","CT","DE","FL",
           "GA","HI","ID","IL","IN","IA","KS","KY","LA","ME",
           "MD","MA","MI","MN","MS","MO","MT","NE","NV","NH",
           "NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI",
           "SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")


### AUTOMATED NASSQS DATA EXTRACTION -------------------------------------------

# Please extract the following to mirror WiNDC NASS set:
# (1) Production Weight

    # GTAP SECTOR to NASSQS Production Weight and Production ($):

    # PDR - NASS: Rice (CWT)
    # WHT - NASS: Wheat (BU)
    # GRO - NASS: Corn (BU) + Sorghum (BU) + Barley (BU) +Rye (BU) + Oats (BU) + Millet (BU)
    # V_F - NASS: Non-Citrus (Tons) + Citrus (Tons) + Tree nut (Tons) + Vegetable (CWT)
    # OSD - NASS: Soybeans (BU) + Cotton (Tons) + Sunflower (LB) + Canola (LB) + Rapeseed (LB)
    # C_B - NASS: Sugarbeets (Tons) + Sugercane (Tons)
    # PFB - NASS: Cotton (Bales)
    # OCR - NASS: Tobacco (LB) + Hay (LB) + Sunflower (LB) + Herbs (LB) + Coffee (LB)
    # CTL - NASS: Beef Cattle (Heads) + Milk Cattle (Heads) + Sheep (Heads) + Bison (Heads) + Equine (Heads) + Equine Other (Heads)
    # OAP - NASS: Eggs (Count) + Chickens (Heads) + Turkeys (Heads) + Ducks (Heads) + Honey (LB) + Geese (Heads) + Hogs (Heads)
    # RMK - NASS: Milk (LB)
    # WOL - NASS: Wool (LB)

# WARNING: ONLY ADD 1,000 DATA VALUES (FOR 50 STATES)
# The number of returned records must be less than 50,000
setwd("C:/Users/raepo/Dropbox/My PC (DESKTOP-06FQURM)/Downloads/NASS")
extract_prod <- read_excel("extract_prod.xlsx") 

#Query data from NASS
  #To get data for some of the years, we need to use SURVEY and CENSUS
prod_weight <- nassqs(source_desc =c("SURVEY","CENSUS"),
                     commodity_desc = extract_prod$commodity, 
                     domain_desc = "TOTAL",
                     short_desc = extract_prod$prod_weight,
                     agg_level_desc = "STATE",
                     state_alpha = states,
                     year = c(2017))

#Filter data from NASS
prod_weight_c <- prod_weight %>%  
  select(source_desc, state_alpha, commodity_desc, short_desc, year, Value, unit_desc) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  rename(st=state_alpha, ns=commodity_desc, yr=year, unit=unit_desc)

prod_weight_commodities <- distinct(prod_weight_c, short_desc)

# Export CSV files
write.csv(prod_weight_commodities, file="prod_weight_commoditiesv01.csv", row.names = FALSE)

write.csv(prod_weight_c, file="nass_prod_weight_v01.csv", row.names = FALSE)

# Cleaning data
prod_weight_conversion <- prod_weight_c

prod_weight_conversion <- prod_weight_conversion %>%
  mutate(Newvalue = case_when(short_desc == extract_prod$prod_weight[1] ~ Value/extract_prod$conversion[1],
                              short_desc != extract_prod$prod_weight[1] ~ Value)) %>%
  mutate(Newunit = case_when(short_desc == extract_prod$prod_weight[1] ~ extract_prod$unit[1],
                              short_desc != extract_prod$prod_weight[1] ~ unit))

for(i in 2:length(prod_weight_c)){
  prod_weight_conversion <- prod_weight_conversion %>%
    mutate(Newvalue = case_when(short_desc == extract_prod$prod_weight[i] ~ Value/extract_prod$conversion[i],
                                short_desc != extract_prod$prod_weight[i] ~ Newvalue))%>%
    mutate(Newunit = case_when(short_desc == extract_prod$prod_weight[i] ~ extract_prod$unit[i],
                               short_desc != extract_prod$prod_weight[i] ~ Newunit))
  
}

prod_weight_final <- prod_weight_conversion %>%  
  select(source_desc, st, ns, short_desc, yr, Newvalue, Newunit) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  rename(Value = Newvalue, unit=Newunit)

# Export CSV files
write.csv(prod_weight_final, file="prod_weight_finalv01.csv", row.names = FALSE)
