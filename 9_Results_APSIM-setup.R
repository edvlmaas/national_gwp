#title: "9_Results_APSIM-setup.R"
#author: "Ellen Maas"
#date: "7/22/2022"
#output: html_document
#description: "Runs all graphs for the APSIM simulation at KBS, MI."

print("Starting 9_Results_APSIM-setup.R")

library(apsimx)
library(readxl)
library(magrittr)
library(lubridate)
library(tidyverse)
library(graphics)
library(ggplot2)



#**********************************************************************

# import APSIM modeled points

if(mgmt_scenario_grp!=6) {

APSIM_out <- read_apsimx(file=apsim_db_filename,src.dir=apsim_path) %>%
  mutate(date=Date,
         year=year(date),
         month=month(date),
         day=day(date),
         SoilTemp_020cm_C=SoilTemp_20cm_C)


} else {
  
  APSIM_out <- read.csv(paste0(apsim_path,apsim_bc_filename),skip=4,
                            col.names = c("Date","BulkDensity_gcc(1)","SoyYield_kgha",
                                          "WheatYield_kgha","MaizeYield_kgha",
                                          "VolH2O_20cm","SoilTemp_20cm_C",
                                          "soy_biomass_kgha","wheat_biomass_kgha",
                                          "maize_biomass_kgha","stover_kgha",
                                          "dul_20cm","sat_20cm","ph_20cm",
                                          "N2O_bylayer_kgha(1)","N2O_bylayer_kgha(2)",
                                          "N2O_bylayer_kgha(3)","N2O_bylayer_kgha(4)",
                                          "N2O_bylayer_kgha(5)","oc_bylayer_kgha(1)",
                                          "oc_bylayer_kgha(2)","oc_bylayer_kgha(3)",
                                          "oc_bylayer_kgha(4)","oc_bylayer_kgha(5)",
                                          "blank"),
                            check.names = FALSE, header=FALSE) %>%
    mutate(date=as.Date(Date, "%d/%m/%Y"),
      year=year(date),
      month=month(date),
      day=day(date),
      MaizeYield_gm2=MaizeYield_kgha/10,
      SoybeanYield_gm2=SoyYield_kgha/10,
      WheatYield_gm2=WheatYield_kgha/10,
      TotalSOC_25cm_Mgha=(`BulkDensity_gcc(1)`*20*`oc_bylayer_kgha(1)`) +
        (`BulkDensity_gcc(1)`*5*`oc_bylayer_kgha(2)`),
           N2O_25cm_kgha=`N2O_bylayer_kgha(1)`+(`N2O_bylayer_kgha(2)`*0.25))

}

# soil carbon
APSIMC_Mgha <- APSIM_out[APSIM_out$month==7 & APSIM_out$day==15,
                         c("year","TotalSOC_25cm_Mgha")] %>%
  mutate(TotalSOC_25cm_Mgha=round(TotalSOC_25cm_Mgha,1))

# yield
APSIMY_Mgha <- APSIM_out[,c("year","MaizeYield_kgha","SoyYield_kgha",
                                "WheatYield_kgha")] %>%
  group_by(year) %>%
  summarize(MaizeYield_Mgha=round(max(MaizeYield_kgha/1000),3),
            SoyYield_Mgha=round(max(SoyYield_kgha/1000),3),
            WheatYield_Mgha=round(max(WheatYield_kgha/1000),3))

## soil temperature
APSIMT_C <- APSIM_out[,c("date","year","SoilTemp_20cm_C")] %>%
  mutate(SoilTemp_20cm_C=round(SoilTemp_20cm_C,1)) 

## soil temperature with bias correction
APSIMT_C_calib <- APSIM_out[,c("date","year","SoilTemp_20cm_C")] %>%
  mutate(SoilTemp_20cm_C=round(SoilTemp_20cm_C,1)-soil_temp_bias) 

## volumetric soil moisture
APSIMM_V <- APSIM_out[,c("date","year","VolH2O_20cm")] %>%
  mutate(VolH2O_20cm=round(VolH2O_20cm*100,0))

## volumetric soil moisture with bias correction
APSIMM_V_calib <- APSIM_out[,c("date","year","VolH2O_20cm")] %>%
  mutate(VolH2O_20cm=round(VolH2O_20cm*100-soil_moist_bias,0))

## bulk density
APSIMB_gcc <- APSIM_out[,c("date","year","BulkDensity_gcc(1)")] %>%
  mutate(BulkDensity_gcc=round(APSIM_out$`BulkDensity_gcc(1)`,2))

## N2O emissions
APSIMGN_ghaday <- APSIM_out[,c("date","year","N2O_25cm_kgha")] %>%
  mutate(N2OEmissions_ghaday = round(N2O_25cm_kgha*1000,2),
         dayofyear = yday(date))

APSIMGN_ann_gha <- APSIMGN_ghaday %>%
  group_by(year) %>%
  summarize(N2OEmissions_ghayr=sum(N2OEmissions_ghaday))

APSIMGN_cum_gha <- APSIMGN_ghaday %>%
  mutate(N2O_gha = cumsum(round(N2O_25cm_kgha*1000,2))) %>%
  select(date,year,N2O_gha)

#**********************************************************************

# write out results for use later in ensemble results
output_annual_data <- cbind(APSIMY_Mgha,APSIMC_Mgha[,"TotalSOC_25cm_Mgha"],
                            "APSIM",scenario_name,clim_scenario_num,
                            mgmt_scenario_grp,mgmt_scenario_opt)
colnames(output_annual_data) <- c("year","MaizeYld_Mgha","SoyYld_Mgha",
                                  "WheatYld_Mgha","SOC_Mgha","model_name",
                                  "scenario_name","climate_scenario_num",
                                  "mgmt_scenario_grp_num","mgmt_scenario_opt_num")
                            
output_daily_data <- cbind(APSIMGN_ghaday[,c("date","year","dayofyear",
                                             "N2OEmissions_ghaday")],
                           APSIMGN_cum_gha[,"N2O_gha"],NA,NA,
                           "APSIM",scenario_name,clim_scenario_num,
                           mgmt_scenario_grp,mgmt_scenario_opt)
colnames(output_daily_data) <- c("date","year","dayofyear","N2O_emit_gha","N2O_cum_gha",
                                 "CH4_net_gha","CH4_cum_gha",
                                 "model_name","scenario_name","climate_scenario_num",
                                 "mgmt_scenario_grp_num","mgmt_scenario_opt_num")

write.table(output_annual_data,file=paste0(results_path,"Annual_results_compilation_",
                                           scenario_name,"_APSIM.csv"),
            col.names=T,row.names=F,sep=",",append=F)
write.table(output_daily_data,file=paste0(results_path,"Daily_results_compilation_",
                                          scenario_name,"_APSIM.csv"),
            col.names=T,row.names=F,sep=",",append=F)

#**********************************************************************

# merge observed and modeled data for graphing model-specific results

MaizeYld_Mgha <- merge(ObsYield[ObsYield$crop=="Maize",c("year","mean_yield")],
             APSIMY_Mgha[APSIMY_Mgha$MaizeYield_Mgha != 0,
                             c("year","MaizeYield_Mgha")],
             by="year",
             all=TRUE)
colnames(MaizeYld_Mgha) <- c("year","Observed","APSIM")

MaizeYld_Mgha_piv <- pivot_longer(MaizeYld_Mgha, c(-year),
               names_to = "source",
               values_to = "yield_val")

##
SoyYld_Mgha <- merge(ObsYield[ObsYield$crop=="Soybean",c("year","mean_yield")],
             APSIMY_Mgha[APSIMY_Mgha$SoyYield_Mgha != 0,
                             c("year","SoyYield_Mgha")],
             by="year",
             all=TRUE)
colnames(SoyYld_Mgha) <- c("year","Observed","APSIM")

SoyYld_Mgha_piv <- pivot_longer(SoyYld_Mgha, c(-year),
               names_to = "source",
               values_to = "yield_val")

##
WheatYld_Mgha <- merge(ObsYield[ObsYield$crop=="Wheat",c("year","mean_yield")],
             APSIMY_Mgha[APSIMY_Mgha$WheatYield_Mgha != 0,
                             c("year","WheatYield_Mgha")],
             by="year",
             all=TRUE)
colnames(WheatYld_Mgha) <- c("year","Observed","APSIM")

WheatYld_Mgha_piv <- pivot_longer(WheatYld_Mgha, c(-year),
               names_to = "source",
               values_to = "yield_val")

##
Cstock_Mgha <- merge(ObsC_Mgha[,c("year","cstock")],
             APSIMC_Mgha,
             by="year",
             all=TRUE)
colnames(Cstock_Mgha) <- c("year","Observed","APSIM")

Cstock_Mgha_piv <-  pivot_longer(Cstock_Mgha, c(-year),
               names_to = "source",
               values_to = "C_val")

##
SoilTemp_C <- merge(ObsTemp[,c("date","soil_temperature")],
             APSIMT_C[,c("date","SoilTemp_20cm_C")],
             by="date",
             all=TRUE)
colnames(SoilTemp_C) <- c("date","Observed","APSIM")

SoilTemp_C_piv <- pivot_longer(SoilTemp_C, c(-date),
               names_to = "source",
               values_to = "temp_val") %>%
  mutate(year=year(date))

## calibrated
SoilTemp_C_calib <- merge(ObsTemp[,c("date","soil_temperature")],
             APSIMT_C_calib[,c("date","SoilTemp_20cm_C")],
             by="date",
             all=TRUE)
colnames(SoilTemp_C_calib) <- c("date","Observed","APSIM")

SoilTemp_C_piv_calib <- pivot_longer(SoilTemp_C_calib, c(-date),
               names_to = "source",
               values_to = "temp_val") %>%
  mutate(year=year(date))

##
SoilMoist_VSM <- merge(ObsVSM[,c("date","mean_VSM")],
                       APSIMM_V[,c("date","VolH2O_20cm")],
                       by="date",
                       all=TRUE)
colnames(SoilMoist_VSM) <- c("date","Observed","APSIM")

SoilMoist_VSM_piv <- pivot_longer(SoilMoist_VSM, c(-date),
               names_to = "source",
               values_to = "h2o_val") %>%
  mutate(year=year(date))

## calibrated
SoilMoist_VSM_calib <- merge(ObsVSM[,c("date","mean_VSM")],
                       APSIMM_V_calib[,c("date","VolH2O_20cm")],
                       by="date",
                       all=TRUE)
colnames(SoilMoist_VSM_calib) <- c("date","Observed","APSIM")

SoilMoist_VSM_piv_calib <- pivot_longer(SoilMoist_VSM_calib, c(-date),
               names_to = "source",
               values_to = "h2o_val") %>%
  mutate(year=year(date))

##
SoilBD_gcc <- merge(ObsBD[,c("year","mean_BD")],
                    APSIMB_gcc[APSIMB_gcc$date=="1996-01-01",c("year","BulkDensity_gcc")],
                    by="year",
                    all=TRUE)
colnames(SoilBD_gcc) <- c("year","Observed","APSIM")

SoilBD_gcc_piv <- pivot_longer(SoilBD_gcc, c(-year),
               names_to = "source",
               values_to = "bd_val")

N2O_ghaday <- merge(ObsGas[,c("date","N2O_N")],
                    APSIMGN_ghaday[,c("date","N2OEmissions_ghaday")],
                    by="date",
                    all=TRUE)
colnames(N2O_ghaday) <- c("date","Observed","APSIM")

N2O_ghaday_piv <- pivot_longer(N2O_ghaday, c(-date),
                               names_to = "source",
                               values_to = "n2o_val")

N2O_ghayr <- APSIMGN_ann_gha
colnames(N2O_ghayr) <- c("year","APSIM")

N2O_ghayr_piv <- pivot_longer(N2O_ghayr, c(-year),
                               names_to = "source",
                               values_to = "n2o_val")
