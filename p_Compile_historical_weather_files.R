---
title: "Correlate_PET_Estimators"
author: "Ellen Maas"
date: "July 25, 2019"
output: html_document
---

```{r setup, include=FALSE}
rm(list=ls())
knitr::opts_chunk$set(echo = TRUE)
```

```{r libraries, message=FALSE}
library(readxl)
library(dplyr)
library(magrittr)
library(pracma)
library(sqldf)
library(lubridate)
library(stringi)
source("Monthly_UPET_Correct.R")
```

```{r import_and_clean}
Raw <- read_xlsx("NOAA CDO Kalamazoo Compiled.xlsx",
                 1,col_names=TRUE,col_types=c("text","text","numeric","numeric","numeric",
                                              "date","numeric","text","numeric","text",
                                              "numeric","text"))
Clean <- Raw

# Keep only US stations
#Clean <- filter(Clean, substr(STATION,1,2)=="US")
#Clean <- filter(Clean, STATION=="USC00202015")

# Split out date into day, month, year
Clean <- mutate(Clean, Day=as.numeric(format(as.Date(DATE,format="%Y-%m-%d"), "%d")))
Clean <- mutate(Clean, Month=as.numeric(format(as.Date(DATE,format="%Y-%m-%d"), "%m")))
Clean <- mutate(Clean, Year=as.numeric(format(as.Date(DATE,format="%Y-%m-%d"), "%Y")))
```

```{r build data by hierarchy}
# order of preference for data:
#  * USC00203504	GULL LAKE BIOLOGICAL STATION, MI US
#  * USW00094815	KALAMAZOO BATTLE CREEK INTERNATIONAL AIRPORT, MI US
#  * average of everything else

gull_lake <- Clean[Clean[,2]=="GULL LAKE BIOLOGICAL STATION, MI US",]
kzoo_airport <- Clean[Clean[,2]=="KALAMAZOO BATTLE CREEK INTERNATIONAL AIRPORT, MI US",]
all_else <- Clean[Clean[,2]!="GULL LAKE BIOLOGICAL STATION, MI US" &
    Clean[,2]!="KALAMAZOO BATTLE CREEK INTERNATIONAL AIRPORT, MI US",]

#
# Add in missing days
## solution taken from my stack overflow posting
## create a full list of dates in the range of the data
full_data <- seq(min(Clean$DATE),max(Clean$DATE),by = "1 day")
full_data  <- data.frame(DATE=full_data)
## create a new matrix that includes all DATEs in full_data and all data in gull_lake
new <- merge(gull_lake,
             full_data, 
             by="DATE", 
             all=TRUE)
new$Year <- ifelse(is.na(new$Year),
                   lubridate::year(new$DATE),
                   new$Year)
new$Month <- ifelse(is.na(new$Month),
                    lubridate::month(new$DATE),
                    new$Month)
new$Day <- ifelse(is.na(new$Day),
                  lubridate::day(new$DATE),
                  new$Day)

# join kzoo_airport and fill in missing data
new2 <- left_join(new,kzoo_airport,by="DATE")
new2[is.na(new2$STATION.x),2:12] <- new2[is.na(new2$STATION.x),16:26]

## Create daily average of all_else to fill in missing days from last step
all_else_avg <- all_else %>%
  group_by(DATE) %>% 
  summarize(STATION.x = "all_else_avg", NAME.x = "all_else_avg", LATITUDE.x = min(LATITUDE, na.rm=TRUE),
            LONGITUDE.x = mean(LONGITUDE, na.rm=TRUE), ELEVATION.x = mean(ELEVATION, na.rm=TRUE),
            PRCP.x = mean(PRCP, na.rm=TRUE), TMAX.x = mean(TMAX, na.rm=TRUE), 
            TMIN.x = mean(TMIN, na.rm=TRUE) 
            )

# join all_else_avg and fill in missing data
new3 <- left_join(new2,all_else_avg,by="DATE")
new3[is.na(new3$STATION.x.x),c(2:7,9,11)] <- new3[is.na(new3$STATION.x.x),c(30:37)]

# fill in any remaining missing data with mid-century average

avg_sub <- new3[new3$Year.x>=1951 & new3$Year.x<=1980,
                c("STATION.x.x","NAME.x.x","LATITUDE.x.x","LONGITUDE.x.x","ELEVATION.x.x",
                  "Month.x","Day.x","PRCP.x.x","TMAX.x.x","TMIN.x.x")]

avg <- avg_sub %>%
  group_by(Month.x,Day.x) %>% 
  summarize(STATION = "30_year_avg", NAME = "30_year_avg",
            LATITUDE = mean(LATITUDE.x.x, na.rm=TRUE), 
            LONGITUDE = mean(LONGITUDE.x.x, na.rm=TRUE), 
            ELEVATION = mean(ELEVATION.x.x,na.rm=TRUE),
            PRCP = mean(PRCP.x.x, na.rm=TRUE), 
            TMAX = mean(TMAX.x.x, na.rm=TRUE), TMIN = mean(TMIN.x.x, na.rm=TRUE))

new4 <- left_join(new3[,1:15],avg,by=c("Month.x","Day.x"))
new4[is.na(new4$STATION.x.x),c(2:7,9,11)] <- new4[is.na(new4$STATION.x.x),16:23]
new4[is.na(new4$PRCP.x.x),7] <- new4[is.na(new4$PRCP.x.x),21]
new4[is.na(new4$TMAX.x.x),9] <- new4[is.na(new4$TMAX.x.x),22]
new4[is.na(new4$TMIN.x.x),11] <- new4[is.na(new4$TMIN.x.x),23]
new4[is.nan(new4$PRCP.x.x),7] <- new4[is.nan(new4$PRCP.x.x),21]
new4[is.nan(new4$TMAX.x.x),9] <- new4[is.nan(new4$TMAX.x.x),22]
new4[is.nan(new4$TMIN.x.x),11] <- new4[is.nan(new4$TMIN.x.x),23]

NOAA_Obs <- new4[,1:15]
colnames(NOAA_Obs) <- c("Date","Station","Name","Latitude","Longitude","Elevation",
                        "PRCP","PRCP_Attributes","TMAX","TMAX_Attributes",
                        "TMIN","TMIN_Attributes","Day","Month","Year")
```

```{R write_out_daily_file}
# this will be in mm/mon for RothC
#
# write out a "detail" file in CSV format
write.csv(NOAA_Obs, file="NOAA-based Daily Kalamazoo 1900-2020.csv",row.names=FALSE)
```

###########################################
# Add Droogers and Allan (2002) EVAP
###########################################

```{R Add Droogers & Allen OPE}
# Add average temperature column
NOAA_Obs <- mutate(NOAA_Obs, Calc_TAVG=(TMAX+TMIN)/2)
# Add latitude in radians
NOAA_Obs <- mutate(NOAA_Obs, LatitudeRad=NOAA_Obs$Latitude*pi/180)
# Add Julian day (day of the year)
NOAA_Obs <- mutate(NOAA_Obs,
                   JulianDay=as.numeric(format(as.Date(NOAA_Obs$Date,format="%Y-%m-%d"),
                                               "%j")))
# Add solar declination
NOAA_Obs <- mutate(NOAA_Obs, SolarDecl=0.4093*sin((((2*pi)/365)*NOAA_Obs$JulianDay)-1.405))
# Add sunset hour
NOAA_Obs <- mutate(NOAA_Obs, SunsetHour=acos(-tan(NOAA_Obs$LatitudeRad)*tan(NOAA_Obs$SolarDecl)))
# Add distance between earth and sun
NOAA_Obs <- mutate(NOAA_Obs, DistEarthSun=1+0.033*cos(((2*pi)/365)*NOAA_Obs$JulianDay))
# Add the water equivalent of extraterrestrial radiation
NOAA_Obs <- mutate(NOAA_Obs, ExtraRad=15.392*NOAA_Obs$DistEarthSun*
                ((NOAA_Obs$SunsetHour*sin(NOAA_Obs$LatitudeRad)*sin(NOAA_Obs$SolarDecl))+
                   (cos(NOAA_Obs$LatitudeRad)*cos(NOAA_Obs$SolarDecl)*sin(NOAA_Obs$SunsetHour))))
#
# Calculate Droogers and Allen (2002) potential evapotranspiration
NOAA_Obs <- mutate(NOAA_Obs,DA_PET=0.0013*NOAA_Obs$ExtraRad*(((NOAA_Obs$TMAX+NOAA_Obs$TMIN)/2)+17)*
                    (NOAA_Obs$TMAX-NOAA_Obs$TMIN-(0.0123*NOAA_Obs$PRCP))^0.76)
NOAA_Obs <- mutate(NOAA_Obs,DA_OPE=round(DA_PET/0.75,1))
```

```{r add Tegos et al. OPE}
NOAA_Obs <- mutate(NOAA_Obs, Ra=((24*60/pi) * 82 * NOAA_Obs$DistEarthSun * 
                 ((NOAA_Obs$SunsetHour*sin(NOAA_Obs$LatitudeRad)*sin(NOAA_Obs$SolarDecl))+
                  (cos(NOAA_Obs$LatitudeRad)*cos(NOAA_Obs$SolarDecl)*sin(NOAA_Obs$SunsetHour))))
                 )
NOAA_Obs <- mutate(NOAA_Obs, Te_PET=(0.0000641357278041525 * NOAA_Obs$Ra) /
                     (1 - (0.0226701035899885 * ((NOAA_Obs$TMIN + NOAA_Obs$TMAX)/2) ))
)
NOAA_Obs <- mutate(NOAA_Obs,Te_OPE=round(Te_PET/0.75,1))
```

```{R write_out_daily_file}
# this will be in mm/mon for RothC
#
# write out a "detail" file in CSV format
write.csv(NOAA_Obs, file="Daily Kalamazoo 1900-2020 with OPE.csv",row.names=FALSE)
```



###########################################
#
# Create monthly weather with "observed" and "raw" EVAP
#
# Best correlation was with Thornthwaite and Mather, based on Detroit data, so
# add it here, then decrease by 13%, according to "Correlate_PET_Estimators.Rmd"
# to use as "observed" EVAP.
#
###########################################


```{r aggregate to monthly}
# Need monthly for T&M PET
OPEMonthly_sub <- NOAA_Obs[,c("Month","Year","PRCP","Calc_TAVG","DA_OPE","Te_OPE")]
# convert PRCP and PET back to mm/mon for RothC
OPEMonthly <- OPEMonthly_sub %>%
  group_by(Year,Month) %>% 
  summarize(PRCP = round(sum(PRCP, na.rm=TRUE),2), 
            Calc_TAVG = round(mean(Calc_TAVG, na.rm=TRUE),2),
            DA_OPE = round(sum(DA_OPE, na.rm=TRUE),2), 
            Te_OPE = round(sum(Te_OPE, na.rm=TRUE),2)
            )

```

```{R Add Thornthwaite & Mather (1955) OPE}
# Add monthly head index
OPEMonthly$Hm <- ifelse(OPEMonthly$Calc_TAVG<=0,0,(0.2*OPEMonthly$Calc_TAVG)^1.514)
# Add annual heat index
OPEAnnHIdx_sub <- OPEMonthly[,c("Year","Hm")]
OPEAnnHIdx <- OPEAnnHIdx_sub %>%
  group_by(Year) %>%
  summarize(Ha = round(sum(Hm,na.rm=TRUE)),2)
OPEMonthly <- left_join(OPEMonthly,OPEAnnHIdx,by="Year")
# Add "a" constant
OPEMonthly$a <- (0.000000675*OPEMonthly$Ha^3) - (0.0000771*OPEMonthly$Ha^2) + (0.01792*OPEMonthly$Ha) + 0.49239
# Add raw UPET
OPEMonthly$UPETraw <- ifelse(OPEMonthly$Calc_TAVG<=0,0,
                             ifelse(OPEMonthly$Calc_TAVG>0 & OPEMonthly$Calc_TAVG<27,
                                    (0.53*((10*(OPEMonthly$Calc_TAVG/OPEMonthly$Ha))^OPEMonthly$a)),
                                    (-0.015*OPEMonthly$Calc_TAVG^2 + 1.093 - 14.208)))
OPEMonthly$TM_PET <- Monthly_UPET_Correct(OPEMonthly$UPETraw,OPEMonthly$Month)

OPEMonthly$TM_OPE <- round(OPEMonthly$TM_PET/0.75,1)

# downscale 13% for "observed" OPE 
OPEMonthly$TM_OPE_down <- OPEMonthly$TM_OPE*(1-(13/100))
```

```{r move Te_OPE to end of columns to preserve existing references}

Te_col <- OPEMonthly$Te_OPE
OPEMonthly$Te_OPE <- NULL
OPEMonthly$Te_OPE <- Te_col
```

```{R write_out_monthly_file}
# this will be in mm/mon for RothC
#
# write out a "detail" file in CSV format
write.csv(OPEMonthly[,c(2,1,4,3,13,12,5,14)], 
          file="Monthly Kalamazoo 1900-2020 with OPE.csv",row.names=FALSE)
```

