setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(readxl)
library(readr)
library(lubridate)
library(tidyverse)
library(apsimx)

site_name <- "LRF"
latitude <- 42.410
longitude <- -85.372
experiment_start_year <- 2003
experiment_end_year <- 2010
experiment_year_range <- experiment_start_year:experiment_end_year
experiment_start_date <- "2003-01-01"
experiment_end_date <- "2010-12-31"
end_exp_phase_year <- 2021
apsim_path <- dirname(rstudioapi::getSourceEditorContext()$path)

###########################
## import local data
###########################

# start with observed data at or near site for experimental period

Raw_site <-  read_xlsx("LibertyResearchFarm.xlsx",
                       sheet="WeatherDaily",range="A1:L1296") %>%
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


###########################
## import fill-in from Mesonet and NASA Power
###########################


new_met <- data.frame()
# 1984 is first year Mesonet has data for this site including radiation
## iem_dat and new2 are of type "met"
for(j in 1984:end_exp_phase_year) {
  #use iem for point-scale, iemre for grid scale
  iem_met <- get_iem_apsim_met(lonlat=c(longitude,latitude),
                               dates=c(paste0(j,"-01-01"),paste0(j,"-12-31")))
  new_met <- rbind(new_met,iem_met)
}

pwr_met <- get_power_apsim_met(lonlat=c(longitude,latitude),
                               dates=c(paste0("1984","-01-01"),paste0(end_exp_phase_year,"-12-31")))
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

# clean up and add more unit conversions and dates after site's experimental 
# period to end of study's experimental period (2021)
new3 <- new2[,c("date","year","month","day","dayofyear",
                      "radn.x","maxt.x","mint.x","rain.x","meanw_ms")]
colnames(new3) <- c("date","year","month","day","dayofyear",
                    "radn","maxt","mint","rain","meanw_ms")

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

###########################
## Convert into an APSIM met-formatted file and write out
###########################

# Convert into an APSIM met-formatted file
APSIM_basic <- as_apsim_met(new_dat[,c("year","dayofyear","radn_MJm2",
                                       "maxt_C","mint_C","rain_mm")],
                            filename="basic_wth_exp.met",
                            site=site_name,
                            latitude=latitude,
                            longitude=longitude,
)

# find any columns with NA cells
try(
  APSIM_basic <- napad_apsim_met(APSIM_basic),
  silent=TRUE
)

write_apsim_met(APSIM_basic, wrt.dir=apsim_path, filename="basic_wth_exp.met")
