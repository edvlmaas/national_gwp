#---
#title: 9_Results_RothC-setup.R
#author: Ellen Maas
#date: 8/30/2022
#output: html_document

print("Starting 9_Results_RothC-setup.R")

library(readxl)
#library(plotly)
library(magrittr)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(ggplot2)


#**********************************************************************

# import RothC

rdata_filename <- if_else(nchar(scenario_name)==3, paste0(rothc_path,"graph/",scenario_name," .263"),
                         paste0(rothc_path,"graph/",scenario_name,".263"))

# import RothC modeled points (MON  YR  DPM  RPM  BIO  HUM  TOTAL  CO2  D14C)
RothCObs_df_raw <- read.fwf(rdata_filename,
                       widths=c(3,5,10,9,9,9,9,10,9),
                       col.names = c("MON","year","DPM","RPM","BIO","HUM","ModC","CO2","D14C"),
                       colClasses=c("numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric"),skip=1)

RothCC_Mgha <- RothCObs_df_raw[,c("year","ModC","CO2")] #%>%
#  mutate(CO2_monthly_Mgha=CO2-lag(CO2,default=0),
#         CO2_monthly_ghad=CO2_monthly_Mgha*1000000)


#**********************************************************************

# write out results for use later in ensemble results

output_annual_data <- cbind(RothCC_Mgha$year,NA,NA,NA,
                            RothCC_Mgha[,"ModC"],
                            "RothC",scenario_name,clim_scenario_num,
                            mgmt_scenario_grp,mgmt_scenario_opt)
colnames(output_annual_data) <- c("year","MaizeYld_Mgha","SoyYld_Mgha",
                                  "WheatYld_Mgha","SOC_Mgha","model_name",
                                  "scenario_name","climate_scenario_num",
                                  "mgmt_scenario_grp_num","mgmt_scenario_opt_num")

write.table(output_annual_data,file=paste0(results_path,"Annual_results_compilation_",
                                           scenario_name,"_RothC.csv"),
            col.names=T,row.names=F,sep=",",append=F)

#**********************************************************************


# merge observed and modeled data

##
Cstock_Mgha <- merge(ObsC_Mgha[,c("year","cstock")],
             RothCC_Mgha[c("year","ModC")],
             by="year",
             all=TRUE)
colnames(Cstock_Mgha) <- c("year","Observed","RothC")

Cstock_Mgha_piv <-  pivot_longer(Cstock_Mgha, c(-year),
               names_to = "source",
               values_to = "C_val")

