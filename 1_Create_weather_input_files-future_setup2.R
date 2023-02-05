#######################################
# Script: 1_Create_weather_input_files-future_setup2.R
# Author: Ellen Maas
# Date: 12/7/2022
# Description: This procedure generates weather input files for every model in the 
# format needed by each from CMIP6 future climate projections. 
#######################################
# Calls:
# Monthly_UPET_Correct.R
#######################################
# Audit Log:
# 12/4/2022: Created script.
#######################################

print("Starting 1_Create_weather_input_files-future_setup2.R")

library(readr)
library(dplyr)


#**********************************************************************
# Extract data at lat/lon for this site/cell and climate scenario num

curwd <- getwd()
setwd('D:/CMIP6')

i <- if_else(clim_scenario_num==2 | clim_scenario_num==3, "GFDL-ESM4",
             "UKESM1-0-LL")
j <- if_else(clim_scenario_num==2 | clim_scenario_num==4, "ssp126",
             "ssp585")

setwd(i)

for(k in c('pr','rsds','sfcwind','tasmax','tasmin')) {
  # get files for the current ESM, scenario, and climate attribute
  filesList <- list.files(pattern=paste0('^',k,'_',j,'_.*\\.dat'))
  # open each file and find the records for lat/lon
  data_df <- data.frame()
  
  for(l in filesList) {
    temp_file_df <- read_table(l,
                               col_names = c("val","lon","lat","daynum",
                                             "day","mon","yr","dayofyr",
                                             "daysinmon"))
    lat_lon_df <- temp_file_df[temp_file_df$lon > longitude-0.25 & temp_file_df$lon < longitude+0.25 &
                                 temp_file_df$lat > latitude-0.25 & temp_file_df$lat < latitude+0.25,]
    data_df <- rbind(data_df,lat_lon_df)
  } ## end for loop over selected files (l)
  
  # make sure data are in correct order, remove any duplicates,
  # and save according to climate attribute name
  if(k=='pr') {
    pr_df <- arrange(data_df,yr,dayofyr) %>% distinct(yr,dayofyr,.keep_all=TRUE)
  } else if(k=='rsds') {
    rsds_df <- arrange(data_df,yr,dayofyr) %>% distinct(yr,dayofyr,.keep_all=TRUE)
  } else if(k=='sfcwind') {
    sfcwind_df <- arrange(data_df,yr,dayofyr) %>% distinct(yr,dayofyr,.keep_all=TRUE)
  } else if(k=='tasmax') {
    tasmax_df <- arrange(data_df,yr,dayofyr) %>% distinct(yr,dayofyr,.keep_all=TRUE)
  } else if(k=='tasmin') {
    tasmin_df <- arrange(data_df,yr,dayofyr) %>% distinct(yr,dayofyr,.keep_all=TRUE)
  } ## end if
  
} ## end for loop over climate elements (k)

setwd(curwd)

# # write out dfs for testing purposes
# write.csv(pr_df,file="pr_gfdl_ssp126_df.csv")
# write.csv(rsds_df,file="rsds_gfdl_ssp126_df.csv")
# write.csv(sftwind_df,file="sfcwind_gfdl_ssp126_df.csv")
# write.csv(tasmax_df,file="tasmax_gfdl_ssp126_df.csv")
# write.csv(tasmin_df,file="tasmin_gfdl_ssp126_df.csv")


#**********************************************************************
# Build data frame replicating structure of new_df from current weather data

# convert data to alternative units
rsds_df$radn_MJm2 <- rsds_df$val*(60*60*24)/1000000 # convert fr# om watts/m^2 to MJ/m^2 
rsds_df$radn_Ld <- rsds_df$val/0.484583 # convert from watts/m^2 to Langley/day
rsds_df$radn_Wm2 <- rsds_df$val
pr_df$rain_mm <- pr_df$val*10 # convert from cm/day to mm/day

f_dat <- rsds_df[,c("yr","mon","day","dayofyr")]
f_dat <- cbind(f_dat,rsds_df[,c("radn_MJm2","radn_Ld","radn_Wm2")],
                 tasmax_df$val, tasmin_df$val, pr_df$rain_mm, pr_df$val,
                 sfcwind_df$val) 
colnames(f_dat) <- c("year","month","day","dayofyear","radn_MJm2","radn_Ld",
                       "radn_Wm2","maxt_C","mint_C","rain_mm","rain_cm",
                       "meanw_ms")
f_dat$tavg = rowMeans(f_dat[,c("maxt_C","mint_C")],na.rm=T)
fut_clim_dat <- f_dat[f_dat$year > experiment_end_year,]

#**********************************************************************
# Write out file

write.csv(fut_clim_dat,file=paste0(fut_weather_path,"fut_clim_scenario_",clim_scenario_num,'.csv'),row.names=F)



