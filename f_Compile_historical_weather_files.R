#######################################
# Script: f_Compile_historical_weather_files.R
# Author: Ellen Maas
# Date: Jan 27, 2023
# Description: This procedure compiles historical weather data from multiple
# sources or stations, such as from NOAA Climate Data Online. 
#######################################
# Calls:
#######################################
# Audit Log:
# 1/27/2023: Created script.
#######################################

f_Compile_historical_weather_files <- function(end_date) {
  
  print("Starting f_Compile_historical_weather_files.R")
  
library(readr)
library(dplyr)
library(magrittr)
library(pracma)
library(sqldf)
library(lubridate)
library(stringi)

#*************************************************************************

# import and clean data
Raw <- read_csv(paste0(wth_path,hist_raw_wth_filename),show_col_types=F)
Clean <- Raw[,c("STATION","NAME","LATITUDE","LONGITUDE","ELEVATION","DATE",
                "AWND","PRCP","TMAX","TMIN")] %>%
# Split out date into day, month, year
 mutate(
   date=as.Date(DATE,format="%m/%d/%Y"))

#*************************************************************************

# build data by hierarchy

## order of preference for data:
##  * LUBBOCK 9 N, TX US (US Cropping Systems Research site)
##  * LUBBOCK INTERNATIONAL AIRPORT, TX US
##  * LUBBOCK 13.2 N, TX US (N county, but is 2011-2021)
##  * average of everything else

data_CropSysResrch <- Clean[Clean[,2]=="LUBBOCK 9 N, TX US",]
data_airport <- Clean[Clean[,2]=="LUBBOCK INTERNATIONAL AIRPORT, TX US",]
data_Ncounty <- Clean[Clean[,2]=="LUBBOCK 13.2 N, TX US",]

#
# Add in missing days until experimental period data starts
## solution taken from my stack overflow posting
## create a full list of dates in the range of the data
full_data <- seq(min(Clean$date),max(end_date-1),by = "1 day")
full_data  <- data.frame(date=full_data) %>%
  mutate(day=day(date),
         month=month(date),
         year=year(date),
         dayofyr=yday(date))
## create a new matrix that includes all dates in full_data and all data at 
## CropSysResrch and the airport
new <- merge(full_data, 
             data_CropSysResrch[data_CropSysResrch$date<end_date,],
             by="date", 
             all=TRUE) %>%
  merge(data_airport[data_airport$date<end_date,],
        by="date",
        all=TRUE)

# fill in missing data
## whole dates
new[is.na(new$NAME.x),c("NAME.x","AWND.x","PRCP.x","TMAX.x","TMIN.x")] <- 
  new[is.na(new$NAME.x),c("NAME.y","AWND.y","PRCP.y","TMAX.y","TMIN.y")]
## individual elements
new[is.na(new$AWND.x),"AWND.x"] <- new[is.na(new$AWND.x),"AWND.y"]
new[is.na(new$PRCP.x),"PRCP.x"] <- new[is.na(new$PRCP.x),"PRCP.y"]
new[is.na(new$TMIN.x),"TMIN.x"] <- new[is.na(new$TMIN.x),"TMIN.y"]
new[is.na(new$TMAX.x),"TMAX.x"] <- new[is.na(new$TMAX.x),"TMAX.y"]

# find any columns with NA cells
na_find_col <- names(which(colSums(is.na(new))>0))
na_find_row <- new[is.na(new$NAME.x),]
na_find_wind <- new[is.na(new$AWND.x),]
na_find_prcp <- new[is.na(new$PRCP.x),]
na_find_tmin <- new[is.na(new$TMIN.x),]
na_find_tmax <- new[is.na(new$TMAX.x),]


# fill in any remaining missing data with mid-century average

avg_sub <- new[new$year>=1961 & new$year<=1990,
                c("month","day","AWND.x","PRCP.x","TMAX.x","TMIN.x")]

avg <- avg_sub %>%
  group_by(month,day) %>% 
  summarize(STATION = "30_year_avg", NAME = "30_year_avg",
            AWND = mean(AWND.x, na.rm=TRUE),
            PRCP = mean(PRCP.x, na.rm=TRUE), 
            TMAX = mean(TMAX.x, na.rm=TRUE), 
            TMIN = mean(TMIN.x, na.rm=TRUE))

new2 <- left_join(new[,c("date","day","month","year","dayofyr","NAME.x",
                         "AWND.x","PRCP.x","TMIN.x","TMAX.x")],
                  avg,by=c("month","day"))
new2[is.na(new2$NAME.x),c("NAME.x","AWND.x","PRCP.x","TMIN.x","TMAX.x")] <- 
  new2[is.na(new2$NAME.x),c("NAME.x","AWND.x","PRCP.x","TMIN.x","TMAX.x")]
#new2[is.na(new2$AWND.x)|is.na(new2$PRCP.x)|is.na(new2$TMIN.x)|is.na(new2$TMAX.x),"NAME.x"] <-
#  new2[is.na(new2$AWND.x)|is.na(new2$PRCP.x)|is.na(new2$TMIN.x)|is.na(new2$TMAX.x),"NAME"]
new2[is.na(new2$AWND.x),"AWND.x"] <- new2[is.na(new2$AWND.x),"AWND"]
new2[is.na(new2$PRCP.x),"PRCP.x"] <- new2[is.na(new2$PRCP.x),"PRCP"]
new2[is.na(new2$TMIN.x),"TMIN.x"] <- new2[is.na(new2$TMIN.x),"TMIN"]
new2[is.na(new2$TMAX.x),"TMAX.x"] <- new2[is.na(new2$TMAX.x),"TMAX"]
new2[is.nan(new2$AWND.x),"AWND.x"] <- new2[is.nan(new2$AWND.x),"AWND"]
new2[is.nan(new2$PRCP.x),"PRCP.x"] <- new2[is.nan(new2$PRCP.x),"PRCP"]
new2[is.nan(new2$TMIN.x),"TMIN.x"] <- new2[is.nan(new2$TMIN.x),"TMIN"]
new2[is.nan(new2$TMAX.x),"TMAX.x"] <- new2[is.nan(new2$TMAX.x),"TMAX"]

NOAA_Obs <- new2[,c("date","day","month","year","dayofyr","NAME.x",
                    "AWND.x","PRCP.x","TMIN.x","TMAX.x")]
colnames(NOAA_Obs) <- c("date","day","month","year","dayofyr","StationName",
                        "AWND","PRCP","TMIN","TMAX")
#"PRCP","PRCP_Attributes","TMAX","TMAX_Attributes",
#"TMIN","TMIN_Attributes","Day","Month","Year")

# write_out_daily_file
# this will be in mm/day
#
# write out a "detail" file in CSV format
write.csv(NOAA_Obs, file=paste0(wth_path,"NOAA-based Daily Lubbock 1940-2021.csv")
          ,row.names=FALSE)


#*******************************************************************************
######  Create monthly data for RothC

# Don't need wind for RothC
OPEMonthly_sub <- NOAA_Obs[,c("month","year","PRCP","TMIN","TMAX")] %>%
  mutate(Calc_TAVG = rowMeans(select(.,c("TMIN","TMAX"))))
# convert PRCP and PET back to mm/mon for RothC
OPEMonthly <- OPEMonthly_sub %>%
  group_by(year,month) %>% 
  summarize(PRCP = round(sum(PRCP, na.rm=TRUE),2), 
            Calc_TAVG = round(mean(Calc_TAVG, na.rm=TRUE),2))


# Add Thornthwaite & Mather (1955) OPE
# Add monthly head index
OPEMonthly$Hm <- ifelse(OPEMonthly$Calc_TAVG<=0,0,(0.2*OPEMonthly$Calc_TAVG)^1.514)
# Add annual heat index
OPEAnnHIdx_sub <- OPEMonthly[,c("year","Hm")]
OPEAnnHIdx <- OPEAnnHIdx_sub %>%
  group_by(year) %>%
  summarize(Ha = round(sum(Hm,na.rm=TRUE)),2)
OPEMonthly <- left_join(OPEMonthly,OPEAnnHIdx,by="year")
# Add "a" constant
OPEMonthly$a <- (0.000000675*OPEMonthly$Ha^3) - (0.0000771*OPEMonthly$Ha^2) + (0.01792*OPEMonthly$Ha) + 0.49239
# Add raw UPET
OPEMonthly$UPETraw <- ifelse(OPEMonthly$Calc_TAVG<=0,0,
                             ifelse(OPEMonthly$Calc_TAVG>0 & OPEMonthly$Calc_TAVG<27,
                                    (0.53*((10*(OPEMonthly$Calc_TAVG/OPEMonthly$Ha))^OPEMonthly$a)),
                                    (-0.015*OPEMonthly$Calc_TAVG^2 + 1.093 - 14.208)))
OPEMonthly$TM_PET <- Monthly_UPET_Correct(OPEMonthly$UPETraw,OPEMonthly$month)

OPEMonthly$TM_OPE <- round(OPEMonthly$TM_PET/0.75,1)

# R write_out_monthly_file
## this will be in mm/mon for RothC

write.csv(OPEMonthly[,c("year","month","PRCP","Calc_TAVG","TM_OPE")], 
          file=paste0(wth_path,"NOAA-based Monthly Lubbock 1940-2021 with OPE.csv"),
          row.names=FALSE)

} #end function


