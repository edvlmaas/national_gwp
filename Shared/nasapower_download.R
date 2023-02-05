
################################################################################
## Author: Mitch Baum-modified by Ellen Maas into a generic function
## Purpose: Download data from NASA power 
## Required information: Start Date, End Date, Latitude, and Longitude 
################################################################################

nasapower_download <- function(path, filename, location, start_end_date) {
## Packages to run script

# Code to install r packages (only needs to be run the first time)
#install.packages(c("tidyverse", "nasapower"))

# Code to activate r packages (needs to be run EVERY time)
library(tidyverse)
library(nasapower)

# Create full filename with path  
myfile <- paste0(path, filename)  

## COMPUTES DAILY MAXT MINT RADN AND PRECIPITATION TO BE SAVED IN .csv FILE
### to list all daily ag parameters (pars) available, run: 
### lst <- query_parameters(community = "ag", temporal_api = "daily")
csvdata <- get_power(community = "ag",
                     lonlat = location,                               ## LONGITUDE,  LATITUDE
                     pars = c("PRECTOTCORR",                          ## Precipitation (mm day-1) 
                              "ALLSKY_SFC_SW_DWN",                    ## sky shortwave downward irradiance (MJ/m^2/day)
                              "T2M_MIN",                              ## Mean daily min temp at 2 Meters (C)
                              "T2M_MAX",                              ## Mean daily max temp at 2 Meters (C) 
                              "RH2M",                                 ## Relative humidity at 2 Meters (%)
                              "WS2M_MIN",                             ## Minimum wind speed at 2 Meters (m/s)
                              "WS2M_MAX"),                            ## Maximum wind speed at 2 Meters (m/s)
                     dates = start_end_date,                          ## START DATE, END DATE ("yyyy-mm-dd")
                     temporal_api = "daily") %>%          
  mutate(year = YEAR, 
         month = MM,
         day = DD,
         dayofyear = DOY,
         radn_MJm2 = ALLSKY_SFC_SW_DWN, 
         radn_Ld = ALLSKY_SFC_SW_DWN*0.041868, # convert from MJ/m^2/day to Langley/day
         radn_Wm2 = ALLSKY_SFC_SW_DWN*11.57407407, # convert from MJ/m^2/day to W/m^2/day
         maxt_C = T2M_MAX, 
         mint_C = T2M_MIN,
         rain_mm = PRECTOTCORR,
         rain_cm = round(PRECTOTCORR/10,2),
         rhum_pct = RH2M,
         maxw_ms = WS2M_MAX,
         minw_ms = WS2M_MIN,
         meanw_ms = round((WS2M_MAX+WS2M_MIN)/2,2),
         ## renames all not available data 
         radn_MJm2 = ifelse(radn_MJm2 == -999, round(abs(mean(radn_MJm2)), digits = 2), radn_MJm2),   
         radn_Ld = ifelse(radn_Ld == -999, round(abs(mean(radn_Ld)), digits = 2), radn_Ld), # for Daycent
         radn_Wm2 = ifelse(radn_Wm2 == -999, -99.99, radn_Wm2),  # for DNDC
         rain_mm = ifelse(rain_mm == -999, NA, rain_mm),
         rain_cm = ifelse(rain_cm == -999, NA, rain_cm),
         maxt_C = ifelse(maxt_C == -999, NA, maxt_C),
         mint_C = ifelse(mint_C == -999, NA, mint_C),        
         rhum_pct = ifelse(rhum_pct == -999, NA, rhum_pct),
         maxw_ms = ifelse(maxw_ms == -999, NA, maxw_ms),
         minw_ms = ifelse(minw_ms == -999, NA, minw_ms),
         meanw_ms = ifelse(meanw_ms == -999, NA, meanw_ms)) %>%
  select(year, month, day, dayofyear, radn_MJm2, radn_Ld, radn_Wm2, maxt_C, mint_C, rain_mm, rain_cm, 
         rhum_pct, maxw_ms, minw_ms, meanw_ms)

write.csv(csvdata, myfile, 
            quote = F,             
            row.names = F)  

}