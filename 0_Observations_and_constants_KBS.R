#######################################
# Procedure: 0_Observations_and_constants
# Author: Ellen Maas
# Date: Oct 2, 2022
# Description: It is designed to be run as-is from calling scripts in order
# to create the variables in the local space. It imports data from files and sets 
# values to shared variables that will be used throughout the project.
#######################################

#rm(list=ls())

# may need to change this for linux:
# https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio

suppressMessages({

print("Starting 0_Observations_and_constants.R")

  
library(readxl)
library(magrittr)
library(lubridate)
library(tidyverse)
#library(graphics)
#library(ggplot2)
library(broom)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)

##################################################
#################### constants ###################
##################################################



scenario_df <- data.frame(climate_scenario_num=c(1,1,1,1,1,1,1,1,1,1,
                                                 1,1,1,1,1,
                                                 2,2,2,2,2,2,2,2,2,2,
                                                 2,2,2,2,2,
                                                 3,3,3,3,3,3,3,3,3,3,
                                                 3,3,3,3,3,
                                                 4,4,4,4,4,4,4,4,4,4,
                                                 4,4,4,4,4,
                                                 5,5,5,5,5,5,5,5,5,5,
                                                 5,5,5,5,5),
                          mgmt_scenario_grp=c(1,2,3,4,4,4,4,5,5,5,
                                              6,6,6,6,6,
                                              1,2,3,4,4,4,4,5,5,5,
                                              6,6,6,6,6,
                                              1,2,3,4,4,4,4,5,5,5,
                                              6,6,6,6,6,
                                              1,2,3,4,4,4,4,5,5,5,
                                              6,6,6,6,6,
                                              1,2,3,4,4,4,4,5,5,5,
                                              6,6,6,6,6),
                          mgmt_scenario_opt=c("","","",1,2,3,4,1,2,3,
                                              1,2,3,4,5,
                                              "","","",1,2,3,4,1,2,3,
                                              1,2,3,4,5,
                                              "","","",1,2,3,4,1,2,3,
                                              1,2,3,4,5,
                                              "","","",1,2,3,4,1,2,3,
                                              1,2,3,4,5,
                                              "","","",1,2,3,4,1,2,3,
                                              1,2,3,4,5),
                          scenario_name=c("1_1","1_2","1_3","1_41",
                                          "1_42","1_43","1_44","1_51",
                                          "1_52","1_53","1_61","1_62",
                                          "1_63","1_64","1_65",
                                          "2_1","2_2","2_3","2_41",
                                          "2_42","2_43","2_44","2_51",
                                          "2_52","2_53","2_61","2_62",
                                          "2_63","2_64","2_65",
                                          "3_1","3_2","3_3","3_41",
                                          "3_42","3_43","3_44","3_51",
                                          "3_52","3_53","3_61","3_62",
                                          "3_63","3_64","3_65",
                                          "4_1","4_2","4_3","4_41",
                                          "4_42","4_43","4_44","4_51",
                                          "4_52","4_53","4_61","4_62",
                                          "4_63","4_64","4_65",
                                          "5_1","5_2","5_3","5_41",
                                          "5_42","5_43","5_44","5_51",
                                          "5_52","5_53","5_61","5_62",
                                          "5_63","5_64","5_65"),
                          scenario_abbrev=c("CR","NT-CR","CC-CR",
                                            "RF05-CR","RF15-CR","RF25-CR",
                                            "RF35-CR","RR50-CR","RR25-CR",
                                            "RR00-CR","BC19-CR","BC38-CR",
                                            "BC57-CR","BC76-CR","BC96-CR",
                                            "CR","NT-CR","CC-CR",
                                            "RF05-CR","RF15-CR","RF25-CR",
                                            "RF35-CR","RR50-CR","RR25-CR",
                                            "RR00-CR","BC19-CR","BC38-CR",
                                            "BC57-CR","BC76-CR","BC96-CR",
                                            "CR","NT-CR","CC-CR",
                                            "RF05-CR","RF15-CR","RF25-CR",
                                            "RF35-CR","RR50-CR","RR25-CR",
                                            "RR00-CR","BC19-CR","BC38-CR",
                                            "BC57-CR","BC76-CR","BC96-CR",
                                            "CR","NT-CR","CC-CR",
                                            "RF05-CR","RF15-CR","RF25-CR",
                                            "RF35-CR","RR50-CR","RR25-CR",
                                            "RR00-CR","BC19-CR","BC38-CR",
                                            "BC57-CR","BC76-CR","BC96-CR",
                                            "CR","NT-CR","CC-CR",
                                            "RF05-CR","RF15-CR","RF25-CR",
                                            "RF35-CR","RR50-CR","RR25-CR",
                                            "RR00-CR","BC19-CR","BC38-CR",
                                            "BC57-CR","BC76-CR","BC96-CR"),
                          scenario_descriptor=c("Crop Rotation", #KBS T1
                                                "No Till, Crop Rotation", #KBS T2
                                                "Cover Crop, Crop Rotation", #KBS T3
                                                "Redu Fert 5%, Crop Rotation",
                                                "Redu Fert 15%, Crop Rotation",
                                                "Redu Fert 25%, Crop Rotation",
                                                "Redu Fert 35%, Crop Rotation",
                                                "Rmv Resid 50%, Crop Rotation",
                                                "Rmv Resid 25%, Crop Rotation",
                                                "Rmv Resid 0%, Crop Rotation",
                                                "Biochar 19 Mgha, Crop Rotation",
                                                "Biochar 38 Mgha, Crop Rotation",
                                                "Biochar 57 Mgha, Crop Rotation",
                                                "Biochar 76 Mgha, Crop Rotation",
                                                "Biochar 96 Mgha, Crop Rotation"),
                          climate_esm=c("Baseline","Baseline","Baseline","Baseline",
                                        "Baseline","Baseline","Baseline","Baseline",
                                        "Baseline","Baseline","Baseline","Baseline",
                                        "Baseline","Baseline","Baseline",
                                        "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                        "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                        "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                        "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                        "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                        "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                        "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                        "GFDL_ESM4","GFDL_ESM4","GFDL_ESM4",
                                        "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                        "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                        "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                        "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                        "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                        "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                        "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL",
                                        "UKESM1-0-LL","UKESM1-0-LL","UKESM1-0-LL"),
                          climate_esm_scenario=c("Baseline","Baseline","Baseline","Baseline",
                                                 "Baseline","Baseline","Baseline","Baseline",
                                                 "Baseline","Baseline","Baseline","Baseline",
                                                 "Baseline","Baseline","Baseline",
                                                 "SSP1-2.6","SSP1-2.6","SSP1-2.6","SSP1-2.6",
                                                 "SSP1-2.6","SSP1-2.6","SSP1-2.6","SSP1-2.6",
                                                 "SSP1-2.6","SSP1-2.6","SSP1-2.6","SSP1-2.6",
                                                 "SSP1-2.6","SSP1-2.6","SSP1-2.6",
                                                 "SSP5-8.5","SSP5-8.5","SSP5-8.5","SSP5-8.5",
                                                 "SSP5-8.5","SSP5-8.5","SSP5-8.5","SSP5-8.5",
                                                 "SSP5-8.5","SSP5-8.5","SSP5-8.5","SSP5-8.5",
                                                 "SSP5-8.5","SSP5-8.5","SSP5-8.5",
                                                 "SSP1-2.6","SSP1-2.6","SSP1-2.6","SSP1-2.6",
                                                 "SSP1-2.6","SSP1-2.6","SSP1-2.6","SSP1-2.6",
                                                 "SSP1-2.6","SSP1-2.6","SSP1-2.6","SSP1-2.6",
                                                 "SSP1-2.6","SSP1-2.6","SSP1-2.6",
                                                 "SSP5-8.5","SSP5-8.5","SSP5-8.5","SSP5-8.5",
                                                 "SSP5-8.5","SSP5-8.5","SSP5-8.5","SSP5-8.5",
                                                 "SSP5-8.5","SSP5-8.5","SSP5-8.5","SSP5-8.5",
                                                 "SSP5-8.5","SSP5-8.5","SSP5-8.5")
)

scenario_abbrev <- 
  if_else(mgmt_scenario_num=="1","CR",
  if_else(mgmt_scenario_num=="2","NT-CR",
  if_else(mgmt_scenario_num=="3","CC-CR",
  if_else(mgmt_scenario_num=="41","RF05-CR",
  if_else(mgmt_scenario_num=="42","RF15-CR",
  if_else(mgmt_scenario_num=="43","RF25-CR",
  if_else(mgmt_scenario_num=="44","RF35-CR",
  if_else(mgmt_scenario_num=="51","RR50-CR",
  if_else(mgmt_scenario_num=="52","RR25-CR",
  if_else(mgmt_scenario_num=="53","RR00-CR",
  if_else(mgmt_scenario_num=="61","BC19-CR",
  if_else(mgmt_scenario_num=="62","BC38-CR",
  if_else(mgmt_scenario_num=="63","BC57-CR",
  if_else(mgmt_scenario_num=="64","BC76-CR",
  if_else(mgmt_scenario_num=="65","BC96-CR",
          "Missing Descriptor"
          )))))))))))))))

scenario_descriptor <- 
  if_else(mgmt_scenario_num=="1","Crop Rotation", #KBS T1
  if_else(mgmt_scenario_num=="2","No Till, Crop Rotation", #KBS T2
  if_else(mgmt_scenario_num=="3","Cover Crop, Crop Rotation", #KBS T3
  if_else(mgmt_scenario_num=="41","Redu Fert 5%, Crop Rotation",
  if_else(mgmt_scenario_num=="42","Redu Fert 15%, Crop Rotation",
  if_else(mgmt_scenario_num=="43","Redu Fert 25%, Crop Rotation",
  if_else(mgmt_scenario_num=="44","Redu Fert 35%, Crop Rotation",
  if_else(mgmt_scenario_num=="51","Rmv Resid 50%, Crop Rotation",
  if_else(mgmt_scenario_num=="52","Rmv Resid 25%, Crop Rotation",
  if_else(mgmt_scenario_num=="53","Rmv Resid 0%, Crop Rotation",
  if_else(mgmt_scenario_num=="61","Biochar 19 Mgha, Crop Rotation",
  if_else(mgmt_scenario_num=="62","Biochar 38 Mgha, Crop Rotation",
  if_else(mgmt_scenario_num=="63","Biochar 57 Mgha, Crop Rotation",
  if_else(mgmt_scenario_num=="64","Biochar 76 Mgha, Crop Rotation",
  if_else(mgmt_scenario_num=="65","Biochar 96 Mgha, Crop Rotation",
          "Missing Descriptor"
          )))))))))))))))

climate_scenario_descriptor <- 
  if_else(clim_scenario_num=="1","Baseline",
  if_else(clim_scenario_num=="2","GFDL_ESM4 Low",
  if_else(clim_scenario_num=="3","GFDL_ESM4 High",
  if_else(clim_scenario_num=="4","UKESM1-0-LL Low",
  if_else(clim_scenario_num=="5","UKESM1-0-LL High",
          "Missing Descriptor")))))
  
scenario_descriptor_full <- paste0(scenario_descriptor, "; ",climate_scenario_descriptor)

site_id <- 0
elevation_m = 288
land_conversion_year <- 1850
experiment_year_range <- experiment_start_year:experiment_end_year
year_range_2100=experiment_start_year:2100
experiment_start_date <- "1989-01-01"
experiment_end_date <- "2021-12-31"

depth_m <- 0.25
equil_C_input <- 305.00 #244.21 #210.84 # g C/m^2 annually
surface_C_init <- 60 # Mg C ha-1

control_treatment <- "T8"
control_treatment_num <- 8
treatment <- if_else(mgmt_scenario_num==1, "T1",
             if_else(mgmt_scenario_num==2, "T2",
             if_else(mgmt_scenario_num==3, "T3",
             if_else(mgmt_scenario_grp==4, "T1",
             if_else(mgmt_scenario_grp==5, "T1",
             if_else(mgmt_scenario_grp==6, "T1",
             "Error"))))))
treatment_num <- if_else(mgmt_scenario_num==1, 1,
                 if_else(mgmt_scenario_num==2, 2,
                 if_else(mgmt_scenario_num==3, 3,
                 if_else(mgmt_scenario_grp==4, 1,
                 if_else(mgmt_scenario_grp==5, 1,
                 if_else(mgmt_scenario_grp==6, 1,
                 0))))))
soil_temp_bias <- if_else(mgmt_scenario_num==1, 5.0,
                  if_else(mgmt_scenario_num==2, 4.5,
                  if_else(mgmt_scenario_num==3, 4.5,
                  if_else(mgmt_scenario_grp==4, 5.0,
                  if_else(mgmt_scenario_grp==5, 5.0,
                  if_else(mgmt_scenario_grp==6, 5.0,
                  0))))))
soil_moist_bias <- if_else(mgmt_scenario_num==1, 2.0,
                   if_else(mgmt_scenario_num==2, 0,
                   if_else(mgmt_scenario_num==3, 0,
                   if_else(mgmt_scenario_grp==4, 2.0,
                   if_else(mgmt_scenario_grp==5, 2.0,
                   if_else(mgmt_scenario_grp==6, 4.0,
                   0))))))
covercrop_aftercorn <- "Oats"
covercrop_afterwheat <- "Red Clover"
covercrop_aftercorn_APSIM <- "Wintaroo"
covercrop_afterwheat_APSIM <- "Colenso"
covercrop_aftercorn_Daycent <- "OAT1"
covercrop_afterwheat_Daycent <- "CLVC"

obs_path <- paste0("Data/",site_name,"/Calibration/")
hist_path <- paste0("Data/",site_name,"/Historical Land Use and Yields/")
hist_filename <- "MI-Kalamazoo County historical yields and C input.xlsx"
fut_filename <- "MI-Kalamazoo County future yields and C input.xlsx"
wth_path <- paste0("Data/",site_name,"/Weather/") 
hist_wth_filename <- "NOAA-based Daily Kalamazoo 1900-2020.csv"
hist_wth_mon_filename <- "Monthly Kalamazoo 1900-2020 with OPE.csv"
curr_local_wth_filename <- "12-lter+weather+station+daily+weather+all+variates+1657202230.csv"
nasapower_output_filename <- paste0(site_name,"_np.csv")
obs_yield_filename <- "Yield by Year and Treatment.csv"
obs_bd_filename <- "71-soil+bulk+density+surface+1656513020.csv"
obs_C_filename <- "Soil Total Carbon and Nitrogen - Surface.csv"
obs_Cdeep_filename <- "164-soil+total+carbon+and+nitrogen+by+depth+deep+cores+1656512927.csv"
obs_BDdeep_filename <- "308-soil+bulk+density+by+depth+deep+cores+1656513026.csv"
obs_soiltemp_filename <- "167-soil+temperature+with+trace+gas+sampling+1658515465.csv"
obs_soilmoist_filename <- "157-soil+moisture+with+trace+gas+sampling+1656513014.csv"
obs_ghg_filename <- "28-n2o+ch4+co2+fluxes+via+static+chambers+1656512661.csv"
obs_fert_filename <- "RFertilizer.csv"
obs_mb_filename <- "25-soil+microbial+biomass+via+chloroform+fumigation+1656513120.csv"
obs_plant_cn_filename <- "73-tissue+carbon+and+nitrogen+1667424583.csv"
obs_biomass_filename <- "39-annual+crops+and+alfalfa+biomass+1667489393.csv"

apsim_path <- paste0("APSIM/",site_name,"/") # for weather data - should replace with Daycent's
apsim_db_filename <- paste0("scen_",scenario_name,".db")
apsim_bc_filename <- if(mgmt_scenario_grp==6) {
  if_else(mgmt_scenario_opt==1,paste0("BC19_",clim_scenario_num,".csv"),
  if_else(mgmt_scenario_opt==2,paste0("BC38_",clim_scenario_num,".csv"),
  if_else(mgmt_scenario_opt==3,paste0("BC57_",clim_scenario_num,".csv"),
  if_else(mgmt_scenario_opt==4,paste0("BC76_",clim_scenario_num,".csv"),
  if_else(mgmt_scenario_opt==5,paste0("BC96_",clim_scenario_num,".csv"),
          "Error")))))
}
daycent_path <- paste0("Daycent/",site_name,"/")
dndc_path <- paste0("LDNDC/ldndc-1.30.4.win64/projects/",site_name,"/")
rothc_path <- paste0("RothC/",site_name,"/")
mill_path <- paste0("Millennial/R/simulation/",site_name,"/")

results_path <- paste0(site_name,"_results/")

# 9-color palette with grey and black. Colors in order are:
#[1]black, [2]dark blue, [3]green, [4]light blue, [5]grey,
#[6]pink, [7]red, [8]orange, [9]yellow
cbPalette9 <- c("#000000","#0072B2","#009E73","#56B4E9","#999999",
                "#CC79A7","#D55E00","#E69F00","#F0E442")


###########################################################
#################### observational data ###################
###########################################################


####################### historical averages #######################

Hist_raw <- read_xlsx(paste0(hist_path,"MI-Kalamazoo County historical yields and C input.xlsx"),
                      sheet="Kalamazoo County-Calcs",range="A2:AW171")
HistY_Mgha <- Hist_raw[Hist_raw$Year<=1987,c(1,8:10)] %>%
  mutate(year=Year,
         maize_yield_mgha=`Corn g/m^2...8`/100,
         soybean_yield_mgha=`Soybean g/m^2...9`/100,
         wheat_yield_mgha=`Wheat g/m^2...10`/100)



######################## measured observations #######################

############ surface samples 


## Bulk density
ObsBD_raw <- read.csv(paste0(obs_path,obs_bd_filename),
                  skip=23) %>%
  mutate(date=as.Date(Date, format="%m/%d/%Y"),
         year=year(date))
ObsBD_mean <- ObsBD_raw %>%
  group_by(year,Treatment,Replicate) %>%
  summarize(mean_BD=round(mean(Bulk_density),2),
            Treatment=str_trim(Treatment))
### set BD to control treatment value for equiv. soil mass
ObsBD <- ObsBD_mean[ObsBD_mean$Treatment==control_treatment,] %>%
  group_by(year,Treatment) %>%
  summarize(mean_BD=round(mean(mean_BD),2))

## C percent
ObsC_pct <- read.csv(paste0(obs_path,obs_C_filename),
                     skip=27) %>%
  mutate(date=as.Date(sample_date, format="%m/%d/%Y"))

##  C stock

ObsC_Mgha_all <- ObsC_pct %>%
  group_by(year,treatment) %>%
  summarize(mean_cpct=round(mean(mean_c,na.rm=T),2),
            cstock=mean_cpct*ObsBD$mean_BD*25)

### BD is from control plot to calculate C stocks
ObsC_Mgha <- ObsC_pct[ObsC_pct$treatment==treatment_num,] %>%
  group_by(year) %>%
  summarize(mean_cpct=round(mean(mean_c,na.rm=T),2),
            cstock=mean_cpct*ObsBD$mean_BD*25)

ObsC_control_Mgha <- ObsC_pct[ObsC_pct$treatment==control_treatment_num,] %>%
  group_by(year) %>%
  summarize(mean_cpct=round(mean(mean_c,na.rm=T),2),
            cstock=mean_cpct*ObsBD$mean_BD*25)
initC <- mean(ObsC_control_Mgha$cstock)

# initC is being ignored and 60 Mg C ha-1 forced in due to calibrating the
# start C to RothC and evidence of decreasing C in the control plot during the experimental
# period. It's losing .31 Mg C ha-1 yr-1, so the calculated mean of 43 Mg C ha-1
# for the control plots isn't realistic for the assumed level in 1850. Also, if
# the start is 60 and Bolinder input used as-is without adjustments, RothC hits
# the start of the treatment plot observations dead-on.
ObsC_Mgha <- rbind(c(land_conversion_year, NA, 60),ObsC_Mgha)

ObsC_Mgha_mean_5yr <- mean(ObsC_Mgha$mean_cpct[1:5])

## Yield
ObsYield_raw <- read.csv(paste0(obs_path,obs_yield_filename))
ObsYield <- ObsYield_raw[ObsYield_raw$Treatment==treatment,]
ObsYield$mean_yield <- ObsYield$mean_yield/1000
ObsYield$mean_yield_gm2 <- ObsYield$mean_yield*100

## Soil temp
ObsTemp_raw <- read.csv(paste0(obs_path,obs_soiltemp_filename),
                    skip=21)%>%
  mutate(date=as.Date(date, format="%m/%d/%Y"))
ObsTemp_all <- ObsTemp_raw[ObsTemp_raw$year >= 1999,
                       c("date","year","treatment","soil_temperature","replicate")] %>%
  group_by(date,year,treatment) %>%
  summarize(soil_temperature=round(mean(soil_temperature,na.rm=T),1))
ObsTemp <- ObsTemp_raw[ObsTemp_raw$treatment==treatment & ObsTemp_raw$year >= 1999,
                       c("date","year","soil_temperature","replicate")] %>%
  group_by(date,year) %>%
  summarize(soil_temperature=round(mean(soil_temperature,na.rm=T),1))

## Soil gases - all in g ha-1 d-1
ObsGas_raw <- read.csv(paste0(obs_path,obs_ghg_filename),
                   skip=36) %>%
  mutate(date=as.Date(Sample_Date, format="%m/%d/%Y"),
         year=Year)
ObsGas_mean <- ObsGas_raw %>%
  group_by(date,year,Treatment) %>%
  summarize(CH4_C=round(mean(CH4_C),2),
            CO2_C=round(mean(CO2_C),2),
            N2O_N=round(mean(N2O_N),2))
ObsGas_all <- ObsGas_mean[,c("date","year","Treatment","CH4_C","CO2_C","N2O_N")]
ObsGas <- ObsGas_raw[ObsGas_raw$Treatment==treatment,c("date","year","CH4_C","CO2_C",
                                                  "N2O_N")]

## Soil moisture
ObsGSM <- read.csv(paste0(obs_path,obs_soilmoist_filename),
                   skip=24) %>%
  mutate(date=as.Date(date, format="%m/%d/%Y"),
         year=year(date))
ObsVSM_mean <- left_join(ObsGSM[,c("date","moisture","treatment","replicate")],
                ObsBD_mean,
                by=c("treatment" = "Treatment","replicate" = "Replicate"),
                all=TRUE) %>%
  mutate(VSM=round(moisture*mean_BD,2)) %>%
  group_by(date,treatment) %>%
  summarize(mean_VSM=round(mean(VSM*100),0)) %>%
  mutate(year=year(date))
ObsVSM <- ObsVSM_mean[ObsVSM_mean$treatment==treatment,]

## Microbial biomass
ObsMB_raw <- read.csv(paste0(obs_path,obs_mb_filename),
                  skip=68) %>%
  mutate(date=as.Date(date, format="%m/%d/%Y"),
         year=Year,
         )
ObsMB_mean <- ObsMB_raw %>%
  group_by(year,trt,date) %>%
  summarize(mean_MB_ugg=round(mean(cfibio_c,na.rm=T),0))

# ObsMB <- ObsBD[,c("Treatment","mean_BD")] %>%
#   merge(ObsMB_mean[ObsMB_mean$trt==treatment,],
#         by.x = "Treatment",
#         by.y = "trt",
#         all=T) %>%
#   mutate(mean_MB_gm2=mean_MB_ugg * mean_BD / 100)

ObsMB_all <- ObsMB_mean %>%
  mutate(bd=ObsBD$mean_BD,
         mb_gm2=mean_MB_ugg*bd/100,
         mb_mgha=mb_gm2/100)

ObsMB <- ObsMB_mean[ObsMB_mean$trt==treatment,] %>%
  mutate(bd=ObsBD$mean_BD,
         mb_gm2=mean_MB_ugg*bd/100,
         mb_mgha=mb_gm2/100)

## Plant tissue C and N content
ObsPltCN_raw <- read.csv(paste0(obs_path,obs_plant_cn_filename),
                         skip=24) %>%
  mutate(date=as.Date(sample_date, format="%m/%d/%Y"),
         year=year(date),
         Treatment=Trt)

###check for plant types
#unique(ObsPltCN_raw[ObsPltCN_raw$Trt %in% c("T1","T2","T3"),"species"])

ObsPltCN_mean <- ObsPltCN_raw %>%
  group_by(year,Treatment,species,type) %>%
  summarize(percent_C=round(mean(percent_C),2),
            percent_N=round(mean(percent_N),2)) %>%
  mutate(crop=if_else(species=="Zea mays L. (*)", "Maize",
                      if_else(species=="Glycine max L. (*)", "Soybean",
                              if_else(species=="Triticum aestivum L. (*)", "Wheat",
                                      species))),
         cn_ratio=percent_C/percent_N
  )

### "widen" the results to match format of Daycent data
ObsPltCN_wide <- pivot_wider(ObsPltCN_mean,
                             names_from = type,
                             values_from = c("percent_C","percent_N",
                                             "cn_ratio",)) %>%
  select(year,Treatment,species,crop,percent_C_SEED,percent_C_STOVER,
         percent_N_SEED,percent_N_STOVER,cn_ratio_SEED,cn_ratio_STOVER)

ObsGrainCN <- ObsPltCN_mean[ObsPltCN_mean$Treatment==treatment
                            & ObsPltCN_mean$type=="SEED",] %>%
  left_join(ObsYield[,!names(ObsYield)=="mean_yield"],
            by=c("year","Treatment","crop")) %>%
  mutate(grainC_gm2=percent_C/100*mean_yield_gm2,
         grainN_gm2=percent_N/100*mean_yield_gm2)


## biomass (to calculate C and N of stover)
ObsBiomass_raw <- read.csv(paste0(obs_path,obs_biomass_filename),
                           skip=29) %>%
  mutate(date=as.Date(Date, format="%m/%d/%Y"),
         year=Year)

ObsBiomass_mean <- ObsBiomass_raw %>%
  group_by(year,Treatment,Species,Fraction) %>%
  summarize(biomass_gm2=round(mean(Biomass),2)) %>%
  mutate(crop=if_else(Species=="Zea mays L. (*)", "Maize",
                      if_else(Species=="Glycine max L. (*)", "Soybean",
                              if_else(Species=="Triticum aestivum L. (*)", "Wheat",
                                      Species)))
  )

ObsBiomass <- ObsBiomass_mean[ObsBiomass_mean$Treatment==treatment,] 

### now "widen" the results to match format of Daycent data, add N data
ObsBiomass_wide <- pivot_wider(ObsBiomass, 
                             names_from = Fraction,
                             values_from = "biomass_gm2") %>%
  select(year,Treatment,Species,crop,SEED,WHOLE,STOVER) %>%
  mutate(STOVER=WHOLE-SEED)

ObsStoverCN <- ObsBiomass_wide[ObsBiomass_wide$Treatment==treatment,
                               !names(ObsBiomass_wide) %in% c("SEED","WHOLE")]  %>%
  left_join(ObsPltCN_wide[,c("year","Treatment","crop","percent_C_STOVER","percent_N_STOVER")],
            by=c("year","Treatment","crop")) %>%
  mutate(stoverC_gm2=percent_C_STOVER/100*STOVER,
         stoverN_gm2=percent_N_STOVER/100*STOVER)


############### deep core samples 

ObsBDdeep_raw <- read.csv(paste0(obs_path,obs_BDdeep_filename),
                  skip=32)
ObsBDdeep_mean <- ObsBDdeep_raw %>%
  group_by(year,treatment,section) %>%
  summarize(depth=round(mean(horizon_length),0),
    mean_BD=round(mean(gravel_free_bulk_density),2))
ObsBDdeep <- ObsBDdeep_mean[ObsBDdeep_mean$treatment==treatment,]

### add sample depths for each section
ObsBDdeep_sampledepths <- ObsBDdeep_mean %>%
  group_by(treatment) %>%
  summarize(sample_depth=sum(depth))


## deep core samples - C percent
ObsCdeep_pct <- read.csv(paste0(obs_path,obs_Cdeep_filename),
                     skip=27) %>%
  mutate(date=as.Date(sample_date, format="%m/%d/%Y"),
         year=year(date),
         section=depth)
ObsCdeep_pct_mean <- ObsCdeep_pct %>%
  group_by(year,treatment,section) %>%
  summarize(mean_C=round(mean(c_percent),4))

### calculate C stock
#### join C and bulk density data; fill in NA bulk density data (in the Deep
#### layers) with the Middle value; calculate the stock for each group of
#### year/treatment/depth section; then calculate the % stock in each layer
ObsCBDdeep <- inner_join(ObsBDdeep_mean,
                       ObsCdeep_pct_mean,
                       by=c("year","treatment","section"))%>%
  group_by(treatment) %>%
  mutate(mean_BD_adj = if_else(is.na(mean_BD) & section=="Deep", 
                               mean_BD[section=="Middle"], 
                               mean_BD),
         cstock=mean_C*mean_BD_adj*depth,
         fraction_C=cstock/sum(cstock)) %>%
  ungroup

ObsCBDdeep_cstock <- ObsCBDdeep %>%
  group_by(year,treatment) %>%
  summarize(cstock=sum(cstock)) %>%
  mutate(top_25cm=cstock*0.63)

#### add initial equilibrium C from control plot for year of land conversion
ObsCdeep_Mgha <- rbind(data.frame(year=land_conversion_year,
                                  ObsCBDdeep_cstock[ObsCBDdeep_cstock$treatment==control_treatment,c("treatment","cstock")]),
                       ObsCBDdeep_cstock[ObsCBDdeep_cstock$treatment==treatment,c("year","treatment","cstock")]) 

### calculate C stock to 25 cm
ObsCdeep_calcs <- ObsCBDdeep %>%
  group_by(treatment) %>%
  mutate(c_cm=cstock/depth,
         addtl_c=if_else(section=="Surface",0,
                 if_else(section=="Middle",c_cm*(25-depth[section=="Surface"]),
                 0)
                 ),
         tot_C=if_else(section=="Surface",cstock[section=="Surface"]+addtl_c[section=="Middle"],0)
         )

ObsCdeep_25cm <- ObsCdeep_calcs[ObsCdeep_calcs$section=="Surface",c("year","treatment","tot_C")]

ObsCdeep_top25_pct <- cbind(ObsCdeep_25cm[,1:2],ObsCdeep_25cm$tot_C/ObsCBDdeep_cstock$cstock)


##################################
# Add fertilizer for GHG reference
##################################

Fert_APSIM <- as.data.frame(read.csv(paste0(obs_path,obs_fert_filename)) %>%
  mutate(date=as.Date(date,format="%Y-%m-%d")))
Fert <- as.data.frame(read.csv(paste0(obs_path,obs_fert_filename)) %>%
  mutate(date=as.Date(date,format="%m/%d/%Y")))

#######################
# Bring in weather data
#######################

# Obs_wth <- read.csv(paste0(apsim_path,"/basic_wth_",clim_scenario_num,".csv"),
#                    skip=2) %>%
#   mutate(meant=round((maxt+mint)/2,1),
#          date=as.Date(day-1, origin=paste0(as.character(year),"-01-01"),),
#          source="Air"
#   )

}) # end suppressMessages

