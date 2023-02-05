#######################################
# Script: 1_Create_weather_input_files-setup2.R
# Author: Ellen Maas
# Date: July 11, 2022
# Description: This procedure generates weather input files for every model in the 
# format needed by each, including past and current weather from weather stations
# and nasapower and future projected climate via CMIP6 earth system models. 
# There are some gaps of missing data from past and current weather, so 
# it fills in those values with NASA Power data (which includes radiation data). 
#######################################
# Calls:
# Monthly_UPET_Correct.R
# Shared/nasapower_download.R
#######################################
# Audit Log:
# 7/11/2022: Created script.
# 12/7/2022: Included CMIP6 data.
#######################################

print("Starting 1_Create_weather_input_files-setup2.R")

library(lubridate)
library(tidyverse)
library(apsimx)

source("Monthly_UPET_Correct.R")


###########################
## import local data
###########################

# import historical data for spin-up-to-equilibrium period for Daycent, RothC, and Millennial
Hist_site <- read_csv(paste0(wth_path,hist_wth_filename),
                      show_col_types = FALSE) %>%
  mutate(TMAX=round(TMAX,1),
         date=as.Date(Date,format="%m/%d/%Y"),
         day=Day,
         month=Month,
         year=Year,
         dayofyear=yday(date),
         prec_cm=PRCP/10)

Hist_site_mon <- read_csv(paste0(wth_path,hist_wth_mon_filename),
                          show_col_types = F) 

# import observed data at or near site for experimental period
Raw_site <- read_csv(paste0(wth_path,curr_local_wth_filename),
                     skip=45, col_names = TRUE, col_types = c('c','d','c','d','c','d','c','d','c','d',
                                                              'c','d','c','d','c','d','c','d','c','d',
                                                              'c','d','c','d','d','d'))
#save_probs <- problems()

## Only take complete years
temp_site <- Raw_site[Raw_site$Year %in% experiment_year_range,]

## Reverse rows so oldest data is first
Clean_site <- temp_site[order(nrow(temp_site):1),] %>%
  mutate(date=as.Date(date,format="%m/%d/%Y"),
         year=Year, # Split out date into day, month, year
         month=month(date),
         day=day(date),
         dayofyear=yday(date), # day of the year
         radn_MJm2 = Solar_Radiation*(60*60*24)/1000000, # convert from watts/m^2 to MJ/m^2 
         radn_Ld = Solar_Radiation/0.484583, # convert from watts/m^2 to Langley/day
         radn_Wm2 = Solar_Radiation,
         maxt_C = air_temp_max, # deg C
         mint_C = Air_Temp_Min, # deg C
         rain_mm = precipitation, # mm
         rain_cm = precipitation/10 # cm
  ) 


###########################
## import fill-in NASA
###########################

# Generate NASA POWER data, then import and clean

source("Shared/nasapower_download.R") #creates function locally
nasapower_download(path = wth_path, # where to put the data
                   filename = nasapower_output_filename, # what to call the file it generates
                   location = c(longitude, latitude),
                   start_end_date = c(experiment_start_date, experiment_end_date))

Raw_NASA <- read_csv(paste0(wth_path,"/",nasapower_output_filename), 
                     col_names = T, show_col_types = F)
Clean_NASA <- Raw_NASA


###########################
## merge data
###########################

# order of preference for data:
#  1) site data
#  2) NASA POWER
#  3) average of everything else (maybe month's average?)

## create a full list of dates in the range of the data
full_data <- seq(min(Clean_site$date),max(Clean_site$date),by = "1 day")
full_data  <- data.frame(date=full_data)
# ## create a new matrix that includes all DATEs in full_data and all data in gull_lake
new <- merge(Clean_site,
             full_data, 
             by="date", 
             all=TRUE)
new$year <- ifelse(is.na(new$Year),
                   lubridate::year(new$date),
                   new$Year)
new$month <- ifelse(is.na(new$month),
                    lubridate::month(new$date),
                    new$month)
new$day <- ifelse(is.na(new$day),
                  lubridate::day(new$date),
                  new$day)
new$dayofyear <- yday(new$date)

# join NASA, and fill in missing data
#new_dat <- left_join(Clean_KBS[,27:36],Clean_NASA,by=c("year","month","day","dayofyear"))
new_dat <- left_join(new[,27:ncol(new)],Clean_NASA,by=c("year","month","day","dayofyear"))
new_dat[is.na(new_dat$radn_MJm2.x),5] <- new_dat[is.na(new_dat$radn_MJm2.x),12]
new_dat[is.na(new_dat$radn_Ld.x),6] <- new_dat[is.na(new_dat$radn_Ld.x),13]
new_dat[is.na(new_dat$radn_Wm2.x),7] <- new_dat[is.na(new_dat$radn_Wm2.x),14]
new_dat[is.na(new_dat$maxt_C.x),8] <- new_dat[is.na(new_dat$maxt_C.x),15]
new_dat[is.na(new_dat$mint_C.x),9] <- new_dat[is.na(new_dat$mint_C.x),16]
new_dat[is.na(new_dat$rain_mm.x),10] <- new_dat[is.na(new_dat$rain_mm.x),17]
new_dat[is.na(new_dat$rain_cm.x),11] <- new_dat[is.na(new_dat$rain_cm.x),18]

# calculate daily average temp
new_dat$tavg <- (new_dat$maxt_C.x + new_dat$mint_C.x)/2

# find any columns with NA cells
na_find_col <- names(which(colSums(is.na(new_dat))>0))
na_find_row <- new[is.na(new$year),]

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

# new_dat <- data.frame()
# #### Alternate coding, based on notes above:
# for(j in 1990:experiment_end_year) {
# iem_dat <- get_iemre_apsim_met(lonlat=c(longitude,latitude),
#                                dates=c(paste0(j,"-01-01"),paste0(j,"-12-31")),
#                                fillin.radn=T)
# pwr_dat <- get_power_apsim_met(lonlat=c(longitude,latitude),
#                            dates=c(paste0(j,"-01-01"),paste0(experiment_end_year,"-12-31")))
# val <- abs(rnorm(nrow(iem_dat),10))
# iem_dat <- add_column_apsim_met(iem_dat,value=val,name="windspeed", units="m/s")
# new_dat <- rbind(new_dat,iem_dat)
# }
