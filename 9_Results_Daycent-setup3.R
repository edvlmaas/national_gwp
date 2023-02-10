# title: "9_Results_Daycent-setup3.R"
# author: "Ellen Maas"
# date: "8/8/2022"
# output: html_document
#test

print("Starting 9_Results_Daycent-setup3.R")

library(readxl)
library(magrittr)
library(lubridate)
library(tidyverse)
library(graphics)
library(ggplot2)
library(broom)
#test


#**********************************************************************

# import Daycent modeled points

### most output files (*.out) are limited in time to the specific phase
### they are run from, so they need to be concatenated together in order
### to have the full range of results in one place


### harvest 

Day_base_harvest <- read_csv(paste0(daycent_path,paste0("harvest_base_",scenario_name,".csv")),
                             col_names = TRUE, show_col_types = F)
Day_exp_harvest <- read_csv(paste0(daycent_path,paste0("harvest_exp_",scenario_name,".csv")),
                            col_names = TRUE, show_col_types = F)
Day_fut_harvest <- read_csv(paste0(daycent_path,paste0("harvest_fut_",scenario_name,".csv")),
                            col_names = TRUE, show_col_types = F)

Day_harvest <- rbind(Day_base_harvest,Day_exp_harvest,Day_fut_harvest) %>%
  mutate(year=floor(time),
         cn_grain_ratio=cgrain/`egrain(N)`,
         cn_stover_ratio=cstraw/`estraw(N)`)

## soil temperature

Day_base_soiltavg <- read.fwf(paste0(daycent_path,paste0("soiltavg_base_",scenario_name,".out")),
                              widths=c(12,5,8,8,8,8,8,8,8,6),
                              col.names=c("time","dayofyear","layer1","layer2","layer3",
                                          "layer4","layer5","layer6","layer7","layer8"),
                              colClasses=c("numeric","numeric","numeric","numeric","numeric",
                                           "numeric","numeric","numeric","numeric","numeric"))
Day_exp_soiltavg <- read.fwf(paste0(daycent_path,paste0("soiltavg_exp_",scenario_name,".out")),
                             widths=c(12,5,8,8,8,8,8,8,8,6), 
                             col.names=c("time","dayofyear","layer1","layer2","layer3",
                                         "layer4","layer5","layer6","layer7","layer8"),
                             colClasses=c("numeric","numeric","numeric","numeric","numeric",
                                          "numeric","numeric","numeric","numeric","numeric"))
Day_fut_soiltavg <- read.fwf(paste0(daycent_path,paste0("soiltavg_fut_",scenario_name,".out")),
                             widths=c(12,5,8,8,8,8,8,8,8,6), 
                             col.names=c("time","dayofyear","layer1","layer2","layer3",
                                         "layer4","layer5","layer6","layer7","layer8"),
                             colClasses=c("numeric","numeric","numeric","numeric","numeric",
                                          "numeric","numeric","numeric","numeric","numeric"))

DayT_C <- rbind(Day_exp_soiltavg,Day_fut_soiltavg) %>%
  mutate(year=floor(time),
         date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1)
DayT_C <- DayT_C %>%
         mutate(mean_3_4=round(rowMeans(DayT_C[,c("layer3","layer4")]),2))

# additional version including base phase data
DayT_C_all <- rbind(Day_base_soiltavg,Day_exp_soiltavg,Day_fut_soiltavg) %>%
  mutate(year=floor(time),
         date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1)
DayT_C_all <- DayT_C_all %>%
  mutate(mean_3_4=round(rowMeans(DayT_C_all[,c("layer3","layer4")]),2))


# ### calibrated version
# DayT_C_calib <- rbind(Day_exp_soiltavg,Day_fut_soiltavg) %>%
#   mutate(year=floor(time),
#          date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01")))

## soil moisture

Day_base_vswc <- read.fwf(paste0(daycent_path,paste0("vswc_base_",scenario_name,".out")),
                          widths=c(10,7,10,10,10,10,10,10,10,6),
                          col.names=c("time","dayofyear","layer1","layer2","layer3",
                                      "layer4","layer5","layer6","layer7","layer8"),
                          colClasses=c("numeric","numeric","numeric","numeric","numeric",
                                       "numeric","numeric","numeric","numeric","numeric"))
Day_exp_vswc <- read.fwf(paste0(daycent_path,paste0("vswc_exp_",scenario_name,".out")),
                         widths=c(10,7,10,10,10,10,10,10,10,6), 
                         col.names=c("time","dayofyear","layer1","layer2","layer3",
                                     "layer4","layer5","layer6","layer7","layer8"),
                         colClasses=c("numeric","numeric","numeric","numeric","numeric",
                                      "numeric","numeric","numeric","numeric","numeric")) 
Day_fut_vswc <- read.fwf(paste0(daycent_path,paste0("vswc_fut_",scenario_name,".out")),
                         widths=c(10,7,10,10,10,10,10,10,10,6), 
                         col.names=c("time","dayofyear","layer1","layer2","layer3",
                                     "layer4","layer5","layer6","layer7","layer8"),
                         colClasses=c("numeric","numeric","numeric","numeric","numeric",
                                      "numeric","numeric","numeric","numeric","numeric")) 

DayM_V <- rbind(Day_exp_vswc,Day_fut_vswc) %>%
  mutate(year=floor(time),
         date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1,
         layer1_pct=layer1*100,
         layer2_pct=layer2*100,
         layer3_pct=layer3*100,
         layer4_pct=layer4*100,
         layer5_pct=layer5*100,
         layer6_pct=layer6*100,
         layer7_pct=layer7*100,
         layer8_pct=layer8*100)

DayM_V_all <- rbind(Day_base_vswc,Day_exp_vswc,Day_fut_vswc) %>%
  mutate(year=floor(time),
         date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1,
         layer1_pct=layer1*100,
         layer2_pct=layer2*100,
         layer3_pct=layer3*100,
         layer4_pct=layer4*100,
         layer5_pct=layer5*100,
         layer6_pct=layer6*100,
         layer7_pct=layer7*100,
         layer8_pct=layer8*100)


## N2O and CH4 emissions

Day_exp_methane <- read.fwf(paste0(daycent_path,paste0("methane_exp_",scenario_name,".out")),
                            widths=c(4,6,12,12,12,12,12,12,12,12,12,12,12,12,12,
                                     12,12,12,12,12,12),
                            col.names=c("year","DOY","aglivc","bglivcj","bglivcm",
                                        "prev_mcprd1","prev_mcprd2","prev_mcprd3",
                                        "COM","ppt","irri","watr2sat","avgst_10cm",
                                        "TI","Cr","Eh","Feh","CH4_prod","CH4_Ep",
                                        "CH4_Ebl","CH4_oxid")
                            ,skip=1)
Day_fut_methane <- read.fwf(paste0(daycent_path,paste0("methane_fut_",scenario_name,".out")),
                            widths=c(4,6,12,12,12,12,12,12,12,12,12,12,12,12,12,
                                     12,12,12,12,12,12),
                            col.names=c("year","DOY","aglivc","bglivcj","bglivcm",
                                        "prev_mcprd1","prev_mcprd2","prev_mcprd3",
                                        "COM","ppt","irri","watr2sat","avgst_10cm",
                                        "TI","Cr","Eh","Feh","CH4_prod","CH4_Ep",
                                        "CH4_Ebl","CH4_oxid")
                            ,skip=1)

Day_methane <- rbind(Day_exp_methane[,c("year","DOY","CH4_Ep","CH4_Ebl","CH4_oxid")],
                     Day_fut_methane[,c("year","DOY","CH4_Ep","CH4_Ebl","CH4_oxid")]) %>%
  mutate(CH4_emis_gCmd=CH4_Ep+CH4_Ebl,
         CH4_emis_gChad=CH4_emis_gCmd*10000,
         dayofyear=DOY)

# Day_base_summary <- read.fwf(paste0(daycent_path,paste0("summary_base.out")),
#                              widths=c(10,5,9,9,9,13,13,13,13,13),
#                              col.names=c("time","dayofyear","tmax","tmin","ppt",
#                                          "N2O_gNhad","NOflux","CH4_gChad","NIT","CO2resp"),
#                              skip=1) 
Day_exp_summary <- read.fwf(paste0(daycent_path,paste0("summary_exp_",scenario_name,".out")),
                            widths=c(10,5,9,9,9,13,13,13,13,13),
                            col.names=c("time","dayofyear","tmax","tmin","ppt",
                                        "N2O_gNhad","NOflux","CH4_oxid_gChad","NIT","CO2resp"),
                            skip=1) 
Day_fut_summary <- read.fwf(paste0(daycent_path,paste0("summary_fut_",scenario_name,".out")),
                            widths=c(10,5,9,9,9,13,13,13,13,13),
                            col.names=c("time","dayofyear","tmax","tmin","ppt",
                                        "N2O_gNhad","NOflux","CH4_oxid_gChad","NIT","CO2resp"),
                            skip=1) 

#Day_summary <- rbind(Day_base_summary,Day_exp_summary,Day_fut_summary)
Day_summary <- rbind(Day_exp_summary,Day_fut_summary) %>%
  mutate(year=floor(time)) %>%
  merge(Day_methane, by=c("year","dayofyear")) %>%
  mutate(CH4_net_gChad=CH4_emis_gChad-CH4_oxid_gChad) %>%
  arrange(year,dayofyear)

  
Day_base_soiln <- read.fwf(paste0(daycent_path,paste0("soiln_base_",scenario_name,".out")),
                           widths=c(8,6,14,14,14,14,14,14,14,14,14,14,14,14,14,14),
                           col.names=c("time","dayofyear","ammonium","NO3_ppm0","NO3_ppm1",
                                       "NO3_ppm2","NO3_ppm3","NO3_ppm4","NO3_ppm5","NO3_ppm6",
                                       "NO3_ppm7","NO3_ppm8","NO3_ppm9","NO3_ppm10",
                                       "NO3_ppm11","NO3_ppm12"),
                           skip=1)

Day_exp_soiln <- read.fwf(paste0(daycent_path,paste0("soiln_exp_",scenario_name,".out")),
                          widths=c(8,6,14,14,14,14,14,14,14,14,14,14,14,14,14,14),
                           col.names=c("time","dayofyear","ammonium","NO3_ppm0","NO3_ppm1",
                                       "NO3_ppm2","NO3_ppm3","NO3_ppm4","NO3_ppm5","NO3_ppm6",
                                       "NO3_ppm7","NO3_ppm8","NO3_ppm9","NO3_ppm10",
                                       "NO3_ppm11","NO3_ppm12"),
                           skip=1)

Day_fut_soiln <- read.fwf(paste0(daycent_path,paste0("soiln_fut_",scenario_name,".out")),
                          widths=c(8,6,14,14,14,14,14,14,14,14,14,14,14,14,14,14),
                          col.names=c("time","dayofyear","ammonium","NO3_ppm0","NO3_ppm1",
                                      "NO3_ppm2","NO3_ppm3","NO3_ppm4","NO3_ppm5","NO3_ppm6",
                                      "NO3_ppm7","NO3_ppm8","NO3_ppm9","NO3_ppm10",
                                      "NO3_ppm11","NO3_ppm12"),
                          skip=1)

Day_soiln <- rbind(Day_base_soiln,Day_exp_soiln) %>%
  mutate(year=floor(time),
         NO3_ppm=NO3_ppm0+NO3_ppm1+NO3_ppm2+NO3_ppm3)
  
Day_soiln_all <- rbind(Day_base_soiln,Day_exp_soiln,Day_fut_soiln) %>%
  mutate(year=floor(time),
         NO3_ppm=NO3_ppm0+NO3_ppm1+NO3_ppm2+NO3_ppm3)

#**********************************************************************


### future .lis contains all data from year 1 in equilibrium through 2100
lis_output <- read.table(paste0(daycent_path,paste0("sched_fut_",scenario_name,".lis")),
#lis_output <- read.table(paste0(daycent_path,paste0("sched_exp_",scenario_name,".lis")),
                                                  col.names = c("time","somsc_gm2","somtc","somte(1)",
                                       "crpval","cinput","somse(1)","petann",
                                       "tminrl(1)","minerl(1,1)","minerl(2,1)",
                                       "minerl(3,1)","minerl(4,1)","minerl(5,1)",
                                       "minerl(6,1)","minerl(7,1)","minerl(8,1)",
                                       "aglivc","bglivcj","bglivcm","cgrain",
                                       "crmvst","hi","clitad(1)","clitad(2)",
                                       "elitad(1,1)","elitad(2,1)"),
                         colClasses=c("numeric","numeric","numeric","numeric",
                                      "character","numeric","numeric","numeric",
                                      "numeric","numeric","numeric","numeric",
                                      "numeric","numeric","numeric","numeric",
                                      "numeric","numeric","numeric","numeric",
                                      "numeric","numeric","numeric","numeric",
                                      "numeric","numeric","numeric"),
                         skip=45)

## need to remove duplicate years where phases join (base-exp, exp-fut)
## and 2100
DayC_Mgha <- lis_output[!((lis_output$cinput == 0 & 
                           (lis_output$time == experiment_start_year | lis_output$time == experiment_end_year+1)) |
                          lis_output$time == 2100),c("time","somsc_gm2")] %>%  
  mutate(year=floor(time),
         base=round(somsc_gm2/100,1)
  )


DayY_Mgha <- Day_harvest[substr(Day_harvest$crpval,2,5)!="CLVC" &
                           substr(Day_harvest$crpval,2,4)!="OAT",] %>%
  select(time,cgrain,crpval) %>%
  mutate(year=floor(time),
         yield=cgrain/100/.45, #g C/m^2 converted to Mg/ha, then divided by .45 to convert C mass to yield mass
         crop=if_else(substr(crpval,2,2)=="C", "Maize",
                      if_else(substr(crpval,2,2)=="S", "Soybean", 
                              if_else(substr(crpval,2,2)=="W", "Wheat", "Unknown")))
  )

DayY_Mgha_pivwid <- pivot_wider(DayY_Mgha,names_from="crop",values_from="yield")

#Daycent doesn't output bulk density

DayGN_ghaday <- Day_summary[,c("time","dayofyear","N2O_gNhad")] %>%
  mutate(year=floor(time),
         date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1)

DayGN_ann_gha <- DayGN_ghaday %>%
  group_by(year) %>%
  summarize(N2OEmissions_ghayr=sum(N2O_gNhad))

DayGN_cum_gha <- DayGN_ghaday[,c("year","dayofyear","date","N2O_gNhad")] %>%
  mutate(N2O_gha = cumsum(N2O_gNhad)) %>%
  select(-N2O_gNhad)

#DayGC_ghaday <- Day_summary_base[,c("time","dayofyear","N2Oflux")] %>%
#  mutate(year=floor(time))

DayGM_ghaday <- Day_summary[,c("time","dayofyear","CH4_net_gChad")] %>%
  mutate(year=floor(time),
         date=as.Date(dayofyear,origin=paste0(as.character(year),"-01-01"))-1)

DayGM_ann_gha <- DayGM_ghaday %>%
  group_by(year) %>%
  summarize(CH4Emissions_ghayr=sum(CH4_net_gChad))

DayGM_cum_gha <- DayGM_ghaday[,c("year","dayofyear","date","CH4_net_gChad")] %>%
  mutate(CH4_gha = cumsum(CH4_net_gChad)) %>%
  select(-CH4_net_gChad)

DayPltCN <- Day_harvest[substr(Day_harvest$crpval,2,5)!="CLVC" &
                          substr(Day_harvest$crpval,2,4)!="OAT",] %>%
  select(year,crpval,cgrain,`egrain(N)`,cstraw,`estraw(N)`,
         cn_grain_ratio,cn_stover_ratio) %>%
  mutate(crop=if_else(substr(crpval,2,2)=="C", "Maize",
             if_else(substr(crpval,2,2)=="S", "Soybean", 
                     if_else(substr(crpval,2,2)=="W", "Wheat", "Unknown")))
  )

DayCI_gm2yr <- lis_output[!((lis_output$cinput == 0 & 
                               (lis_output$time == experiment_start_year | lis_output$time == experiment_end_year+1)) |
                              lis_output$time == 2100),c("time","clitad.2.")] %>%  
  mutate(year=floor(time),
         base=`clitad.2.`
  )
  
DayNI_gm2yr <- lis_output[!((lis_output$cinput == 0 & 
                               (lis_output$time == experiment_start_year | lis_output$time == experiment_end_year+1)) |
                              lis_output$time == 2100),c("time","elitad.2.1.")] %>%  
  mutate(year=floor(time),
         base=`elitad.2.1.`
  )

#**********************************************************************

# write out results for use later in ensemble results
output_annual_data <- cbind(merge(DayY_Mgha_pivwid[,c("year","Maize","Soybean","Wheat")],
                           DayC_Mgha[,c("time","base")],
                           by.x="year", by.y="time",
                           all=TRUE),
                           "Daycent",scenario_name,clim_scenario_num,
                           mgmt_scenario_grp,mgmt_scenario_opt)
                           

colnames(output_annual_data) <- c("year","MaizeYld_Mgha","SoyYld_Mgha",
                                  "WheatYld_Mgha","SOC_Mgha","model_name",
                                  "scenario_name","climate_scenario_num",
                                  "mgmt_scenario_grp_num","mgmt_scenario_opt_num")

output_daily_data <- cbind(DayGN_ghaday[,c("date","year","dayofyear","N2O_gNhad")],
                           DayGN_cum_gha[,"N2O_gha"],
                           DayGM_ghaday[,"CH4_net_gChad"],
                           DayGM_cum_gha[,"CH4_gha"],
                           "Daycent",scenario_name,clim_scenario_num,
                           mgmt_scenario_grp,mgmt_scenario_opt)

colnames(output_daily_data) <- c("date","year","dayofyear","N2O_emit_gha","N2O_cum_gha",
                                 "CH4_net_gha","CH4_cum_gha",
                                 "model_name","scenario_name","climate_scenario_num",
                                 "mgmt_scenario_grp_num","mgmt_scenario_opt_num")

write.table(output_annual_data,file=paste0(results_path,"Annual_results_compilation_",
                                           scenario_name,"_Daycent.csv"),
            col.names=T,row.names=F,sep=",",append=F)
write.table(output_daily_data,file=paste0(results_path,"Daily_results_compilation_",
                                          scenario_name,"_Daycent.csv"),
            col.names=T,row.names=F,sep=",",append=F)


#**********************************************************************


# merge observed and modeled data

MaizeYld_Mgha <- merge(ObsYield[ObsYield$crop=="Maize",c("year","mean_yield")],
                       DayY_Mgha[DayY_Mgha$crop=="Maize", #DayY_Mgha$yield != 0,
                                 c("year","yield")],
                       by="year",
                       all=TRUE) %>%
  merge(HistY_Mgha[,c("year","maize_yield_mgha")],
        by="year",
        all=TRUE)
colnames(MaizeYld_Mgha) <- c("year","Observed","Daycent","Historical")

MaizeYld_Mgha_piv <- pivot_longer(MaizeYld_Mgha, c(-year),
                                  names_to = "source",
                                  values_to = "yield_val")

##
SoyYld_Mgha <- merge(ObsYield[ObsYield$crop=="Soybean",c("year","mean_yield")],
                     DayY_Mgha[DayY_Mgha$crop=="Soybean", #DayY_Mgha$yield != 0,
                               c("year","yield")],
                     by="year",
                     all=TRUE) %>%
  merge(HistY_Mgha[HistY_Mgha$year>=1954,c("year","soybean_yield_mgha")],
        by="year",
        all=TRUE)
colnames(SoyYld_Mgha) <- c("year","Observed","Daycent","Historical")

SoyYld_Mgha_piv <- pivot_longer(SoyYld_Mgha, c(-year),
                                names_to = "source",
                                values_to = "yield_val")

##
WheatYld_Mgha <- merge(ObsYield[ObsYield$crop=="Wheat",c("year","mean_yield")],
                       DayY_Mgha[DayY_Mgha$crop=="Wheat", #DayY_Mgha$yield != 0,DayY_Mgha$yield != 0,
                                 c("year","yield")],
                       by="year",
                       all=TRUE) %>%
  merge(HistY_Mgha[HistY_Mgha$year>=1954,c("year","wheat_yield_mgha")],
        by="year",
        all=TRUE)
colnames(WheatYld_Mgha) <- c("year","Observed","Daycent","Historical")

WheatYld_Mgha_piv <- pivot_longer(WheatYld_Mgha, c(-year),
                                  names_to = "source",
                                  values_to = "yield_val")

##
Cstock_Mgha <- merge(ObsC_Mgha[,c("year","cstock")],
                     DayC_Mgha[,c("year","base")],
                     by="year",
                     all=TRUE)
colnames(Cstock_Mgha) <- c("year","Observed","Daycent")

Cstock_Mgha_piv <-  pivot_longer(Cstock_Mgha, c(-year),
                                 names_to = "source",
                                 values_to = "C_val")

##
SoilTemp_C <- merge(ObsTemp[,c("date","soil_temperature")],
                    DayT_C[,c("date","mean_3_4")],#DayT_C[,c("date","layer3")],
                    by="date",
                    all=TRUE)
colnames(SoilTemp_C) <- c("date","Observed","Daycent")

SoilTemp_C_piv <- pivot_longer(SoilTemp_C, c(-date),
                               names_to = "source",
                               values_to = "temp_val")

##
SoilMoist_VSM <- merge(ObsVSM[,c("date","year","mean_VSM")],
                       DayM_V[,c("date","year","layer4_pct")],
                       by=c("date","year"),
                       all=TRUE)
colnames(SoilMoist_VSM) <- c("date","year","Observed","Daycent")

SoilMoist_VSM_piv <- pivot_longer(SoilMoist_VSM, c(-date, -year),
                                  names_to = "source",
                                  values_to = "h2o_val")

##
# SoilBD_gcc <- merge(ObsBD[,c("year","mean_BD")],
#                     APSIMB_gcc[APSIMB_gcc$date=="1996-01-01",c("year","BulkDensity_gcc")],
#                     by="year",
#                     all=TRUE)
# colnames(SoilBD_gcc) <- c("year","Observed","APSIM")

SoilBD_gcc <- ObsBD[,c("year","mean_BD")]
colnames(SoilBD_gcc) <- c("year","Observed")

SoilBD_gcc_piv <- pivot_longer(SoilBD_gcc, c(-year),
                               names_to = "source",
                               values_to = "bd_val")

##
N2O_ghaday <- merge(ObsGas[,c("date","N2O_N")],
                    DayGN_ghaday[,c("date","N2O_gNhad")],
                    by="date",
                    all=TRUE)
colnames(N2O_ghaday) <- c("date","Observed","Daycent")

N2O_ghaday_piv <- pivot_longer(N2O_ghaday, c(-date),
                               names_to = "source",
                               values_to = "n2o_val")

N2O_ghayr <- DayGN_ann_gha
colnames(N2O_ghayr) <- c("year","Daycent")

N2O_ghayr_piv <- pivot_longer(N2O_ghayr, c(-year),
                              names_to = "source",
                              values_to = "n2o_val")

##
CH4_ghaday <- merge(ObsGas[,c("date","CH4_C")],
                    DayGM_ghaday[,c("date","CH4_net_gChad")],
                    by="date",
                    all=TRUE)
colnames(CH4_ghaday) <- c("date","Observed","Daycent")

CH4_ghaday_piv <- pivot_longer(CH4_ghaday, c(-date),
                               names_to = "source",
                               values_to = "ch4_val")

CH4_ghayr <- DayGM_ann_gha
colnames(N2O_ghayr) <- c("year","Daycent")

CH4_ghayr_piv <- pivot_longer(CH4_ghayr, c(-year),
                              names_to = "source",
                              values_to = "ch4_val")

##
grainC_gm2 <- merge(ObsGrainCN[ObsGrainCN$crop %in% c("Maize","Soybean","Wheat"),
                                  c("year","crop","grainC_gm2")],
                    DayPltCN[,c("year","crop","cgrain")],
                    by=c("year","crop"),
                    all=TRUE)
colnames(grainC_gm2) <- c("year","crop","Observed","Daycent")

grainC_gm2_piv <- pivot_longer(grainC_gm2, c(-year,-crop),
                                 names_to = "source",
                                 values_to = "grainC_val")

grainN_gm2 <- merge(ObsGrainCN[ObsGrainCN$crop %in% c("Maize","Soybean","Wheat"),
                                  c("year","crop","grainN_gm2")],
                    DayPltCN[,c("year","crop","egrain(N)")],
                    by=c("year","crop"),
                    all=TRUE)
colnames(grainN_gm2) <- c("year","crop","Observed","Daycent")

grainN_gm2_piv <- pivot_longer(grainN_gm2, c(-year,-crop),
                               names_to = "source",
                               values_to = "grainN_val")

##
stoverC_gm2 <- merge(ObsStoverCN[ObsStoverCN$crop %in% c("Maize","Soybean","Wheat"),
                                 c("year","crop","stoverC_gm2")],
                     DayPltCN[,c("year","crop","cstraw")],
                     by=c("year","crop"),
                     all=TRUE)
colnames(stoverC_gm2) <- c("year","crop","Observed","Daycent")

stoverC_gm2_piv <- pivot_longer(stoverC_gm2, c(-year,-crop),
                                names_to = "source",
                                values_to = "grainC_val")

stoverN_gm2 <- merge(ObsStoverCN[ObsStoverCN$crop %in% c("Maize","Soybean","Wheat"),
                                   c("year","crop","stoverN_gm2")],
                     DayPltCN[,c("year","crop","estraw(N)")],
                     by=c("year","crop"),
                     all=TRUE)
colnames(stoverN_gm2) <- c("year","crop","Observed","Daycent")

stoverN_gm2_piv <- pivot_longer(stoverN_gm2, c(-year,-crop),
                               names_to = "source",
                               values_to = "grainN_val")

##
grainCN <- merge(grainC_gm2,
                 grainN_gm2,
                 by=c("year","crop"),
                 all=TRUE) %>%
  mutate(Observed=Observed.x/Observed.y,
         Daycent=Daycent.x/Daycent.y) %>%
  select(year,crop,Observed,Daycent)

grainCN_piv <- pivot_longer(grainCN, c(-year,-crop),
                            names_to = "source",
                            values_to = "grainCN_val")

stoverCN <- merge(stoverC_gm2,
                   stoverN_gm2,
                 by=c("year","crop"),
                 all=TRUE) %>%
  mutate(Observed=Observed.x/Observed.y,
         Daycent=Daycent.x/Daycent.y) %>%
  select(year,crop,Observed,Daycent)

stoverCN_piv <- pivot_longer(stoverCN, c(-year,-crop),
                            names_to = "source",
                            values_to = "stoverCN_val")
