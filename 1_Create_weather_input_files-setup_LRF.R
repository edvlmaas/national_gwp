#######################################
# Script: 1_Create_weather_input_files-setup_LRF.R
# Author: Ellen Maas
# Date: July 11, 2022
# Description: This procedure generates weather input files for every model in the 
# format needed by each, including past and current weather from weather stations
# and nasapower and future projected climate via CMIP6 earth system models. 
# There are some gaps of missing data from past and current weather, so 
# it fills in those values with NASA Power data (which includes radiation data). 
#######################################
# Calls:
# f_Compile_historical_weather_files (but only needs to do so once)
# Monthly_UPET_Correct.R
# Shared/nasapower_download.R
#######################################
# Audit Log:
# 7/11/2022: Created script.
# 12/7/2022: Included CMIP6 data.
# 1/24/2023: Rewrote to use apsimx weather functions per Fernando Miguez of ISU.
# 1/31/2023: Rewrote to use APISM Classic met file format.
#######################################

print("Starting 1_Create_weather_input_files-setup_LRF.R")

library(readxl)
library(readr)
library(lubridate)
library(tidyverse)
library(apsimx)

source("Monthly_UPET_Correct.R")

###########################
## import local data
###########################

# start with observed data at or near site for experimental period

Raw_site <-  read_xlsx(paste0(obs_path, obs_filename),
                       sheet=curr_wth_tab,range="A1:L1296") %>%
  mutate(date=`Weather Date`,
         year=year(date))

## Only take complete years [LRF is missing 2003 weather data, though]
## Make sure data are chronological order oldest->newest (they are)
## and rename columns to APSIM naming to make using APSIM functions
## easier
Clean_site <- Raw_site[Raw_site$year %in% experiment_year_range,] %>%
  mutate(date=as.Date(date),
         month=month(date),
         day=day(date),
         dayofyear=yday(date), # day of the year
         radn = NA, #Solar_Radiation MJ/m2,
         maxt = `Temp Max degC`, # deg C
         mint = `Temp Min degC`, # deg C
         rain = `Precip mm/d`, # mm/day
         rhum_pct = `RH %`, #relative humidity
         meanw_ms = `Wind Speed m/s`
  ) 

#####################################################
###### only need to run this once per site ##########

# # compile and import historical data for spin-up-to-equilibrium period for 
# # Daycent, RothC, and Millennial
#source("f_Compile_historical_weather_files.R")
#f_Compile_historical_weather_files(as.Date(min(Clean_site$date)))

#####################################################


Hist_site <- read_csv(paste0(wth_path,hist_wth_filename),
                      show_col_types = FALSE) %>%
  mutate(prec_cm=PRCP/10)

Hist_site_mon <- read_csv(paste0(wth_path,hist_wth_mon_filename),
                          show_col_types = F) 


#save_probs <- problems()



###########################
## import fill-in from Mesonet and NASA Power
###########################


new_met <- data.frame()
# 1984 is first year Mesonet has data for this site including radiation
## iem_dat and new2 are of type "met"
for(j in 1984:end_exp_period_year) {
  #use iem for point-scale, iemre for grid scale
  iem_met <- get_iem_apsim_met(lonlat=c(longitude,latitude),
                               dates=c(paste0(j,"-01-01"),paste0(j,"-12-31")))
  new_met <- rbind(new_met,iem_met)
}

pwr_met <- get_power_apsim_met(lonlat=c(longitude,latitude),
                               dates=c(paste0("1984","-01-01"),paste0(end_exp_period_year,"-12-31")))
new_met <- add_column_apsim_met(new_met,value=pwr_met$windspeed,name="windspeed", units="m/s")

check_apsim_met(new_met)
# the above function found missing radiation data (-99 value) so replace
# with NASA Power data
new_met[new_met$radn<0,"radn"] <- pwr_met[new_met$radn<0,"radn"]

# does gap-filling, but will return an error if it's clean (no discontinuities);
# using "try()" with silent=TRUE will ignore that error and continue execution,
# but keep in mind that ALL errors will be ignored
try(
  new_met <- napad_apsim_met(new_met),
  silent=TRUE
)

# will fix 365-366 day year after have run napad
new_met <- impute_apsim_met(new_met)

# convert to data frame for data merging in subsequent steps
new_met_df <- as.data.frame(new_met) %>%
  mutate(date=as.Date(day-1, origin=paste0(as.character(year),"-01-01"),),
         year=year(date),
         month=month(date),
         dayofyear=day,
         day=day(date),
         meanw_ms=windspeed)



###########################
## merge data
###########################

# order of preference for data:
#  1) site data
#  2) iem data  
#  3) NASA POWER (already done in previous step, into new2_df)
#  x4) average of everything else (maybe month's average?); using APSIM
# functions means that there are no gaps, at least in experimental data

## create a full list of dates in the range of the experimental period
full_data <- seq(as.Date(experiment_start_date),as.Date(experiment_end_date),by = "1 day")
full_data  <- data.frame(date=full_data)
## create a new matrix that includes all DATEs in full_data and all
# experimental-period data
new1 <- merge(full_data, 
              Clean_site[,c("date","radn","maxt","mint","rain","meanw_ms")],
              by="date", 
              all=TRUE) %>%
  mutate(year=year(date),
         month=month(date),
         day=day(date),
         dayofyear=yday(date))

# join APSIM library-generated data
new2 <- left_join(new1[,c("date","year","month","day","dayofyear",
                          "radn","maxt","mint","rain","meanw_ms")],
                  new_met_df,by=c("date","year","dayofyear","month","day"))
new2[is.na(new2$radn.x),"radn.x"] <- new2[is.na(new2$radn.x),"radn.y"]
new2[is.na(new2$maxt.x),"maxt.x"] <- new2[is.na(new2$maxt.x),"maxt.y"]
new2[is.na(new2$mint.x),"mint.x"] <- new2[is.na(new2$mint.x),"mint.y"]
new2[is.na(new2$rain.x),"rain.x"] <- new2[is.na(new2$rain.x),"rain.y"]
new2[is.na(new2$meanw_ms),"meanw_ms"] <- new2[is.na(new2$meanw_ms),"windspeed"]

# find any columns with NA cells
na_find_col <- names(which(colSums(is.na(new2))>0))
na_find_row <- new[is.na(new$year),]

# clean up and add more unit conversions
new3 <- new2[,c("date","year","month","day","dayofyear",
                      "radn.x","maxt.x","mint.x","rain.x","meanw_ms")]
colnames(new3) <- c("date","year","month","day","dayofyear",
                    "radn","maxt","mint","rain","meanw_ms")

# add in dates after site's experimental period to end of study's experimental 
# period (2021)
new_dat <- rbind(new3,new_met_df[new_met_df$year>2010,c("date","year","month",
                                                        "day","dayofyear","radn",
                                                        "maxt","mint","rain",
                                                        "meanw_ms")]) %>%
  mutate(tavg = (maxt + mint)/2,
         radn_MJm2 = radn,
         radn_Wm2 = radn_MJm2*11.57407407, # convert from MJ/m^2/day to W/m^2/day
         radn_Ld = radn_MJm2*0.041868, # convert from MJ/m^2/day to Langley/day
         maxt_C = maxt, # deg C
         mint_C = mint, # deg C
         rain_mm = rain, # mm/day
         rain_cm = rain_mm/10 # cm/day
  )

#**********************************************************************
##### Future weather
#**********************************************************************

###########################
## build baseline-to-2100 data
###########################


new_dat_2100 <- new_dat
weather_28yr <- new_dat[new_dat$year %in% 1994:2021,]

for (i in 1:3) {
  weather_28yr$year <- weather_28yr$year+28
  new_dat_2100 <- rbind(new_dat_2100, weather_28yr)
}


#**********************************************************************
##### Clean up
#**********************************************************************

rm(list = c("Raw_site","new_met","iem_met","pwr_met","new_met_df","new1",
            "new2","new3","full_data","na_find_col","na_find_row",
            "weather_28yr"))



############################
# notes with Fernando
# take weather station data, better at precip than nasapower
# pwr <- get_power_apsim_met(lonlat = c(-93,42), dates = c("2012-01-01","2012-12-31"),
# filename="power.met")

#this will get data from weather station
#iem.met2 <- get_iem_apsim_met(lonlat = c(-93.77, 42.02),  (may be better for precip than power)
#dates = c("2012-01-01","2012-12-31"))
#summary(iem.met2)
# no wind, but can get from nasapower to combine; radiation is good to use from nasapower
# use iem.met2 for stats
# https://mesonet.agron.iastate.edu/request/coop/fe.phtml
# iem.met2$radn <- pwr$radn
# ames <- add_column_apsim_met(ames, value = val, name = "vp", units = "(hPa)") is more
#reliable way to replace data elements (includes units)
#check_apsim_met(iem.met2) will report warnings (if no errors, nothing returns)
# napad_apsim_met(met) does gap-filling
# impute_apsim_met is final step; will fix 365-366 day year after have run napad
#
# as_apsim_met will convert data to format
# read_apsim_met/write_apsim_met to read wr; check will look for data issues (NAs, etc.)
#
# get_iemre_apsim_met may be preferable for using it grid-scale (can with Power too, though)
#
# compare_apsim_met gives diffs/bias between files - good for comparing point-scale data
#############################

