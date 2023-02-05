# ---
# title: "9_Results_Millennial-setup.R"
# author: "Ellen Maas"
# date: "8/30/2022"
# output: html_document

print("Starting 9_Results_Millennial-setup.R")

library(readxl)
#library(plotly)
library(magrittr)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)

# local constants

mill_baseinput_filename <- "siteenviron_base_in.txt"



#*************************************************************

# import Millennial


mill_base_df_raw <- read.csv(file=paste0(mill_path,"base_out_",scenario_name,".csv"))
mill_scen_df <- read.csv(file=paste0(mill_path,"scenario_out_",scenario_name,".csv"))

mill_daily_df <- rbind(mill_base_df_raw,mill_scen_df)
mill_df <- mill_daily_df[month(mill_daily_df$date)==1 & day(mill_daily_df$date)==1,] %>%
  mutate(TOC_Mgha=TOC/100)

millC_Mgha <- mill_df[,c("year","TOC_Mgha")]

# add C input to graph to see how millennial responds to input
mill_dailyCinput_df <- read.delim(file=paste0(mill_path,mill_baseinput_filename),sep="\t") %>%
  mutate(year=year(date))

mill_annualCinput_df <- mill_dailyCinput_df %>%
  group_by(year) %>%
  summarize(totC=sum(forc_npp))

# reduce Millennial C to limit to top 25 cm
## calculate the Mgha to reduce the output by to get from 1 m C to top 25 cm
## can't just multiply by a fraction as the values end up compressed, and we just
## need everything to drop down by a set amount
# pct_diff_1m_25cm <- if_else(mgmt_scenario_num==1 | mgmt_scenario_grp %in% 4:7, 0.63,
#                     if_else(mgmt_scenario_num==2, 0.5,
#                     if_else(mgmt_scenario_num==3, 0.47,
#                     0)))
reduceCby <- if_else(mgmt_scenario_num==1 | mgmt_scenario_grp %in% 4:6, 35,
             if_else(mgmt_scenario_num==2, 35,
             if_else(mgmt_scenario_num==3, 33,
             0)))
millC_Mgha_25cm <- data.frame(year=millC_Mgha$year,
                            cstock=millC_Mgha$TOC_Mgha-reduceCby) 

# add daily microbial CO2 (model output is cumulative)
mill_base_df <- mill_base_df_raw %>%
  mutate(date=as.Date(date),
    CO2_daily_gm2d=CO2-lag(CO2,default=0),
         CO2_daily_ghad=CO2_daily_gm2d*10000)


#**********************************************************************

# write out results for use later in ensemble results
output_annual_data <- cbind(millC_Mgha_25cm$year,NA,NA,NA,
                            millC_Mgha_25cm[,"cstock"],
                            "Millennial",scenario_name,clim_scenario_num,
                            mgmt_scenario_grp,mgmt_scenario_opt)
colnames(output_annual_data) <- c("year","MaizeYld_Mgha","SoyYld_Mgha",
                                  "WheatYld_Mgha","SOC_Mgha","model_name",
                                  "scenario_name","climate_scenario_num",
                                  "mgmt_scenario_grp_num","mgmt_scenario_opt_num")

write.table(output_annual_data,file=paste0(results_path,"Annual_results_compilation_",
                                           scenario_name,"_Millennial.csv"),
            col.names=T,row.names=F,sep=",",append=F)

#*************************************************************

# merge data

# Carbon, full 1 m depth
Cstock_Mgha <- merge(ObsC_Mgha[,c("year","cstock")],
                     millC_Mgha,
                     #millC_Mgha_25cm,
                     by="year",
                     all=TRUE)
colnames(Cstock_Mgha) <- c("year","Observed","Millennial")

Cstock_Mgha_piv <-  pivot_longer(Cstock_Mgha, c(-year),
               names_to = "source",
               values_to = "C_val")

## C for full 1 m depth
Cat1850 <- as.numeric(millC_Mgha[millC_Mgha$year==1850,"TOC_Mgha"])
Cat1989 <- as.numeric(millC_Mgha[millC_Mgha$year==1989,"TOC_Mgha"])
Cdiff_1850_1989 <- Cat1850-Cat1989

# Carbon, 25 cm depth
Cstock_Mgha_25cm <- merge(ObsC_Mgha[,c("year","cstock")],
                     millC_Mgha_25cm,
                     by="year",
                     all=TRUE)
colnames(Cstock_Mgha_25cm) <- c("year","Observed","Millennial")

Cstock_Mgha_piv_25cm <-  pivot_longer(Cstock_Mgha_25cm, c(-year),
               names_to = "source",
               values_to = "C_val")


# Microbial
#CO2_ghaday <- merge(ObsGas_all[ObsGas_all$Treatment %in% treatment,
#                     c("date","year","Treatment","CO2_C")],
#                    mill_base_df,
#                    by=c("date","year"),
#                    all=TRUE)

mbio_gm2_all <- left_join(mill_base_df[,c("year","date","MIC")],
                      ObsMB_all[ObsMB_all$trt==treatment,c("year","date","trt","mb_gm2")],
                      by=c("date","year"))
colnames(mbio_gm2_all) <- c("year","date","Millennial","treatment","Observed")

mbio_gm2 <- inner_join(mill_base_df[,c("year","date","MIC")],
                      ObsMB_all[ObsMB_all$trt==treatment,c("year","date","trt","mb_gm2")],
                      by=c("date","year"))
colnames(mbio_gm2) <- c("year","date","Millennial","treatment","Observed")
