#######################################
# Script: 1_Create_weather_input_files-RothC.R
# Author: Ellen Maas
# Date: July 11, 2022
# Output: Function doesn't return any data, hence the "p" (procedure) naming
# convention. It creates files in the appropriate folder for each model.
# Description: "This procedure generates weather input files for every model in the 
# format needed by each. There are some gaps of missing data on various days, so 
# it fills in those values with NASA Power data (which includes radiation data). 
# Calls custom nasapower_download function."
#######################################

print("Starting 1_Create_weather_input_files-RothC.R")

source("Monthly_UPET_Correct.R")
rothc_weather_path <- paste0(rothc_path,"weather/")
  
if(weather_ind=="C") {
  
  # create initial 28-year average for spin-up and 1920-1949 timeframe
  Monthly_init <- Hist_site_mon[Hist_site_mon$Year %in% 1950:1977, 
                                c("Month","PRCP","Calc_TAVG","TM_OPE_down")]
  
  WeatherInit <- Monthly_init[,c("Month","PRCP","Calc_TAVG","TM_OPE_down")] %>%
    group_by(Month) %>%
    summarize(PRCP = round(mean(PRCP, na.rm=TRUE),2), Calc_TAVG = round(mean(Calc_TAVG, na.rm=TRUE),2),
              OpenPanEvap = round(mean(TM_OPE_down, na.rm=TRUE),2))
  
  
  ###########################
  ## Equilibrium
  
  RothC_equil <- "Eqil.dat"
  weather_path <- paste(rothc_weather_path,RothC_equil, sep="")
  # create initialization weather files (to equilibrium at land conversion) - 28-year average from 1950-1977
  cat(paste("'MI-Initialization, NOAA average weather 1950-1977; with weighted average clay and sampling depth'",sep=""),
      file=weather_path, sep="\n",append=FALSE)
  write.table(WeatherInit[,c(3,2,4)], file=weather_path, col.names=FALSE, row.names=FALSE, sep="\t", 
              quote=FALSE, append=TRUE)
  cat("19","25", file=weather_path, sep="\t", append=TRUE)
  
  
  ###########################
  ## Experimental and Future
  
  
  # Aggregate daily KBS weather to monthly
  OPEMonthly_sub_2100 <- new_dat_2100[,c("month","year","rain_mm.x","tavg")]
  #
  OPEMonthly_2100 <- OPEMonthly_sub_2100 %>%
    group_by(year,month) %>% 
    summarize(PRCP = round(sum(rain_mm.x, na.rm=TRUE),2), 
              Calc_TAVG = round(mean(tavg, na.rm=TRUE),2)
    )
  colnames(OPEMonthly_2100) <- c("Year","Month","PRCP","Calc_TAVG")
  
  # Calculate T&M and reduce by 13%
  ## Add monthly head index
  OPEMonthly_2100$Hm <- ifelse(OPEMonthly_2100$Calc_TAVG<=0,0,(0.2*OPEMonthly_2100$Calc_TAVG)^1.514)
  ## Add annual heat index
  OPEAnnHIdx_sub_2100 <- OPEMonthly_2100[,c("Year","Hm")]
  OPEAnnHIdx_2100 <- OPEAnnHIdx_sub_2100 %>%
    group_by(Year) %>%
    summarize(Ha = round(sum(Hm,na.rm=TRUE)),2)
  OPEMonthly_2100 <- left_join(OPEMonthly_2100,OPEAnnHIdx_2100,by="Year")
  ## Add "a" constant
  OPEMonthly_2100$a <- (0.000000675*OPEMonthly_2100$Ha^3) - 
    (0.0000771*OPEMonthly_2100$Ha^2) + 
    (0.01792*OPEMonthly_2100$Ha) + 0.49239
  ## Add raw UPET
  OPEMonthly_2100$UPETraw <- ifelse(OPEMonthly_2100$Calc_TAVG<=0,0,
                                    ifelse(OPEMonthly_2100$Calc_TAVG>0 & OPEMonthly_2100$Calc_TAVG<27,
                                           (0.53*((10*(OPEMonthly_2100$Calc_TAVG/OPEMonthly_2100$Ha))^OPEMonthly_2100$a)),
                                           (-0.015*OPEMonthly_2100$Calc_TAVG^2 + 1.093 - 14.208)))
  OPEMonthly_2100$TM_PET <- Monthly_UPET_Correct(OPEMonthly_2100$UPETraw,OPEMonthly_2100$Month)
  
  OPEMonthly_2100$TM_OPE <- round(OPEMonthly_2100$TM_PET/0.75,1)
  
  # downscale 13% for "observed" OPE 
  OPEMonthly_2100$TM_OPE_down <- OPEMonthly_2100$TM_OPE*(1-(13/100))
  
  
  ###########################
  ## Write annual files
  
  # Catenate historicalexperimental, and future baseline data together
  base_weather <- rbind(Hist_site_mon[Hist_site_mon$Year<1989,
                                      c("Month","Year","Calc_TAVG","PRCP","TM_OPE_down")],
                        OPEMonthly_2100[,c("Month","Year","Calc_TAVG","PRCP","TM_OPE_down")])
  
  # split Hist_site_mon into a list of monthly records indexed by year through 1988
  lst <- split(base_weather,base_weather$Year)

  ## create annual RothC weather files with initial text lines
  lapply(names(lst),
         function(myfun, lst) {cat(paste(site_name,", NOAA weather year ",myfun,"; with weighted average clay and sampling depth'",sep=""),
                               file=paste(rothc_weather_path,clim_scenario_num,substr(myfun,2,4),".dat", sep=""), sep="\n",append=FALSE)
           write.table(lst[[myfun]], file=paste(rothc_weather_path,clim_scenario_num,substr(myfun,2,4),".dat", sep=""),
                       col.names=FALSE, row.names=FALSE, sep="\t", 
                       quote=FALSE, append=TRUE)
           cat("19","25", file=paste(rothc_weather_path,clim_scenario_num,substr(myfun,2,4),".dat", sep=""), 
               sep="\t", append=TRUE)
         },
         lst)
  
} else if(weather_ind=="F") {
  
  fut_dat <- read.csv(file=paste0(fut_weather_path,"fut_clim_scenario_",clim_scenario_num,'.csv'))
  
  # bind experimental and future data together
  exp_dat <- new_dat[,c("month","year","rain_mm.x","tavg")]
  colnames(exp_dat) <- c("month","year","rain_mm","tavg")
  OPE_all <- rbind(exp_dat,fut_dat[,c("month","year","rain_mm","tavg")])
  
  # Aggregate daily KBS weather to monthly
  OPEMonthly_sub_esm <- OPE_all
  #
  OPEMonthly_esm <- OPEMonthly_sub_esm %>%
    group_by(year,month) %>% 
    summarize(PRCP = round(sum(rain_mm, na.rm=TRUE),2), 
              Calc_TAVG = round(mean(tavg, na.rm=TRUE),2)
    )
  colnames(OPEMonthly_esm) <- c("Year","Month","PRCP","Calc_TAVG")
  
  # Calculate T&M and reduce by 13%
  ## Add monthly head index
  OPEMonthly_esm$Hm <- ifelse(OPEMonthly_esm$Calc_TAVG<=0,0,(0.2*OPEMonthly_esm$Calc_TAVG)^1.514)
  ## Add annual heat index
  OPEAnnHIdx_sub_esm <- OPEMonthly_esm[,c("Year","Hm")]
  OPEAnnHIdx_esm <- OPEAnnHIdx_sub_esm %>%
    group_by(Year) %>%
    summarize(Ha = round(sum(Hm,na.rm=TRUE)),2)
  OPEMonthly_esm <- left_join(OPEMonthly_esm,OPEAnnHIdx_esm,by="Year")
  ## Add "a" constant
  OPEMonthly_esm$a <- (0.000000675*OPEMonthly_esm$Ha^3) - 
    (0.0000771*OPEMonthly_esm$Ha^2) + 
    (0.01792*OPEMonthly_esm$Ha) + 0.49239
  ## Add raw UPET
  OPEMonthly_esm$UPETraw <- ifelse(OPEMonthly_esm$Calc_TAVG<=0,0,
                                    ifelse(OPEMonthly_esm$Calc_TAVG>0 & OPEMonthly_esm$Calc_TAVG<27,
                                           (0.53*((10*(OPEMonthly_esm$Calc_TAVG/OPEMonthly_esm$Ha))^OPEMonthly_esm$a)),
                                           (-0.015*OPEMonthly_esm$Calc_TAVG^2 + 1.093 - 14.208)))
  OPEMonthly_esm$TM_PET <- Monthly_UPET_Correct(OPEMonthly_esm$UPETraw,OPEMonthly_esm$Month)
  
  OPEMonthly_esm$TM_OPE <- round(OPEMonthly_esm$TM_PET/0.75,1)
  
  # downscale 13% for "observed" OPE 
  OPEMonthly_esm$TM_OPE_down <- OPEMonthly_esm$TM_OPE*(1-(13/100))
  
  
  ###########################
  ## Write annual files
  
  # Catenate historical, experimental, and future baseline data together
  base_weather <- rbind(Hist_site_mon[Hist_site_mon$Year<1989,
                                      c("Month","Year","Calc_TAVG","PRCP","TM_OPE_down")],
                        OPEMonthly_esm[,c("Month","Year","Calc_TAVG","PRCP","TM_OPE_down")])
  
  # split Hist_site_mon into a list of monthly records indexed by year through 1988
  lst <- split(base_weather,base_weather$Year)
  
  ## create annual RothC weather files with initial text lines
  lapply(names(lst),
         function(myfun, lst) {cat(paste(site_name,", NOAA weather year ",myfun,"; with weighted average clay and sampling depth'",sep=""),
                               file=paste(rothc_weather_path,clim_scenario_num,substr(myfun,2,4),".dat", sep=""), sep="\n",append=FALSE)
           write.table(lst[[myfun]], file=paste(rothc_weather_path,clim_scenario_num,substr(myfun,2,4),".dat", sep=""),
                       col.names=FALSE, row.names=FALSE, sep="\t", 
                       quote=FALSE, append=TRUE)
           cat("19","25", file=paste(rothc_weather_path,clim_scenario_num,substr(myfun,2,4),".dat", sep=""), 
               sep="\t", append=TRUE)
         },
         lst)

} else {
  
  print(paste0("Unknown weather_ind=",weather_ind,"in 1_Create_weather_input_files-RothC.R"))

}# if weather_ind == C or F

rm(list=c("Monthly_init","WeatherInit","RothC_equil","weather_path","OPEMonthly_sub_2100",
          "OPEMonthly_2100"," OPEAnnHIdx_sub_2100","OPEAnnHIdx_2100","base_weather","lst",
          "myfun","fut_dat","exp_dat","OPE_all","OPEMonthly_sub_esm","OPEMonthly_esm",
          "OPEAnnHIdx_sub_esm","OPEAnnHIdx_esm"))