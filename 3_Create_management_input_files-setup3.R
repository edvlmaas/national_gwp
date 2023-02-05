#######################################
# File: "3_Create_management_input_files-setup3.R"
# Author: "Ellen Maas"
# Date: "Sept 23, 2022"
# Output: It creates files in the appropriate folder for each model.
# Description: "This procedure generates management data - including crop,
# fertilizer, irrigation, fertilizer, harvest, etc. - for every model in the 
# format needed by each. For APSIM, fertilizer is collected as kg N/ha of the N 
# in the fertilizer (not kg/ha of the whole fertilizer)."
#######################################
# Audit Log
# 9/30/2022: Modified to include T3 data for scenario 3.
# 10/3/2022: Changed hairy vetch crop type to white clover.
# 10/22/2022: Changed from a function format to script.
#######################################

suppressMessages({
  
  print("Starting 3_Create_management_input_files-setup3.R")

library(readxl)
library(dplyr)
#library(magrittr)
#library(pracma)
#library(sqldf)
library(lubridate)
#library(stringi)
library(tidyverse)
library(data.table)
library(XML)
library(berryFunctions)


# local constants

       
mgmt_path=paste0("Data/",site_name,"/Management/")
adjusted_ops_filename="clean_ops_ext_adj.csv"

## adjust inputs for future scenarios

fert_adjust <- if_else(mgmt_scenario_num==41, 0.95, # reduce N by 5%
                if_else(mgmt_scenario_num==42, 0.85, # reduce N by 15%
                if_else(mgmt_scenario_num==43, 0.75, # reduce N by 25%
                if_else(mgmt_scenario_num==44, 0.65, # reduce N by 35%
                1
                ))))
resid_adjust_chr <- if_else(mgmt_scenario_num==51, "50", # incorporate 50% residue
                if_else(mgmt_scenario_num==52, "25", # incorporate 75% residue
                if_else(mgmt_scenario_num==53, "0",  # incorporate 0% residue
                "75"
                )))
#resid_adjust_chr <- sub(".*\\.","",as.character(resid_adjust))
#resid_adjust <- if_else(resid_adjust_chr!="", as.numeric(paste0("0.",resid_adjust_chr)),
#                        1)
resid_adjust <- as.numeric(paste0("0.",resid_adjust_chr))

# import



# look at each management activity to check data consistency
ops_ext_adj_raw <- read_csv(paste0(mgmt_path,adjusted_ops_filename),
                    col_names = TRUE, show_col_types=F) #, col_types = c("n","c","c","c","n","c","c","D","c","c"))
ops_ext_adj <- ops_ext_adj_raw[ops_ext_adj_raw$treatment==treatment,]
temp_ops_ext_adj <- ops_ext_adj[(ops_ext_adj$observation_type!="Fertilizer application"),
                                names(ops_ext_adj) != "...11"] %>%
  mutate(date = as.Date(date,format="%m/%d/%Y"),
         rate_kg_ha = NA,
         n_rate_kg_ha = NA,
         p_rate_kg_ha = NA,
         k_rate_kg_ha = NA,
         rate_seeds_ha=rate*2.471,
         rate_seeds_m2=round(rate_seeds_ha/10000,0),
         row_spacing_mm = ifelse(crop %in% c("Maize","Soybean"), 762, 178),
         seed_depth_mm = 50,
         cultivar = ifelse(crop=="Maize","B_100",
                    ifelse(crop=="Soybean","Generic_MG2",
                    ifelse(crop=="Wheat","Yecora",
                    ifelse(crop=="WhiteClover","Kopu",
                    ifelse(crop=="RedClover",covercrop_afterwheat_APSIM,
                    ifelse(crop=="Oats",covercrop_aftercorn_APSIM,
                           "")))))),
         n_rate_g_m2=NA,
         p_rate_g_m2=NA,
         k_rate_g_m2=NA,
         # translate field ops to Daycent codes
         daycent_mgmt_code=if_else(obs_code=="plow","CULT K",
                           if_else(obs_code=="disk","CULT H",
                           if_else(obs_code=="cultivate","CULT D",
                           if_else(obs_code=="cultipack","CULT CP",
                           if_else(obs_code=="finish","CULT D",
                           if_else(obs_code=="hoe","CULT ROW",
                           if_else(obs_code=="plant"&crop=="Maize"&year<2011,"CROP C7",
                           if_else(obs_code=="plant"&crop=="Maize"&year>=2011,"CROP C8",
                           if_else(obs_code=="plant"&crop=="Soybean","CROP SYBN",
                           if_else(obs_code=="plant"&crop=="Wheat","CROP W4EG",
						               if_else(obs_code=="plant"&crop=="WhiteClover","CROP CLVC",
						               if_else(obs_code=="plant"&crop=="RedClover","CROP CLVC",
						               if_else(obs_code=="plant"&crop=="Oats","CROP OAT1",
                           ## assume 75% stover removal on all crops
                           if_else(obs_code=="grain",paste0("HARV G75S"), 
                           ## assuming all-in-one event, ignore "stover" events
                           #if_else(obs_code=="stover"&crop=="Wheat","HARV G90S", 
						               if_else(obs_code %in% c("winterkill","mow"),"HARV KILL", 
                           NULL))))))))))))))),
         daycent_mgmt_code2=if_else(obs_code=="plant","PLTM",NULL)
         ) 

# create separate rows for secondary Daycent codes
exp_ops_ext_adj <- rbind(temp_ops_ext_adj[,1:23],
                         setNames(temp_ops_ext_adj[!is.na(temp_ops_ext_adj$daycent_mgmt_code2),c(1:22,24)],
                                  names(temp_ops_ext_adj[,1:23])))

# for easy viewing groups of management:
#soil_prep <- temp_ops_ext_adj[temp_ops_ext_adj$observation_type=="Soil Preparation",]
#planting <- temp_ops_ext_adj[temp_ops_ext_adj$observation_type=="Planting",]
fertilize <- temp_ops_ext_adj[temp_ops_ext_adj$observation_type=="Fertilizer application",]
#harvest <- temp_ops_ext_adj[temp_ops_ext_adj$observation_type=="Harvest",]

# add fertilizer detail
raw_fert <- read_csv(paste0("Data/",site_name,"/Calibration/RFertilizer.csv"),
                    col_names = TRUE, show_col_types = F)
#
temp1_fert <- raw_fert %>%
  group_by(obs_date,treatment,material) %>%
  summarize(rate_kg_ha=round(mean(rate_kg_ha,na.rm=T),0),
            n_rate_kg_ha=round(mean(n_rate_kg_ha,na.rm=T),0),
            p_rate_kg_ha=round(mean(p_rate_kg_ha,na.rm=T),0),
            k_rate_kg_ha=round(mean(k_rate_kg_ha,na.rm=T),0))
temp2_fert <- temp1_fert %>%
    mutate(date=as.Date(obs_date,format="%m/%d/%Y"),
         year=year(date),
         rate=round(rate_kg_ha*0.892179,0), # convert to lb/ac
         units="lb/ac",
         observation_type="Fertilizer application",
         equipment=NA,
         crop=NA,
         # APSIM codes for Fertiliser and crop modules
         obs_code=if_else(material %like% "lime", "CalciteCA",
                  if_else(n_rate_kg_ha>0, "NO3N", NULL)),
         obs_code2=if_else(p_rate_kg_ha>0, "RockP", NULL),
         rate_seeds_ha=NA,
         rate_seeds_m2=NA,
         row_spacing_mm=NA,
         seed_depth_mm=NA,
         cultivar=NA,
         n_rate_g_m2=n_rate_kg_ha/10,
         p_rate_g_m2=p_rate_kg_ha/10,
         k_rate_g_m2=k_rate_kg_ha/10,
         daycent_mgmt_code=if_else(n_rate_g_m2>0,paste0("FERT (",n_rate_g_m2,"N)"),NULL),
         daycent_mgmt_code2=if_else(p_rate_g_m2>0,paste0("FERT (",p_rate_g_m2,"P)"),NULL)
    ) 

# save fertilizer for other use
#write.csv(temp_fert, file="Site and Region Info/Calibration/RFertilizer.csv",
#            row.names=F, quote=T)

# create separate rows for additional APSIM and Daycent codes
## start with just secondary codes
exp_fert <- rbind(temp2_fert[,c(2:15,17:25)],
                    setNames(temp2_fert[!is.na(temp2_fert$daycent_mgmt_code2),c(2:14,16:24,26)],
                             names(temp2_fert[,c(2:15,17:25)])))
# ## do the same with tertiary codes
# exp_fert <- rbind(exp1_fert,
#                     setNames(temp2_fert[!is.na(temp2_fert$daycent_mgmt_code3),c(2:15,17:24,27)],
#                              names(exp1_fert)))


# combine field ops with separate fertilizer records
combined_ops <- rbind(exp_ops_ext_adj, exp_fert) %>%
  arrange(date,observation_type) # reorder all the records again

full_ops_ext_adj <- as.data.frame(combined_ops)
full_ops_ext_adj$till_depth <- ifelse(full_ops_ext_adj$obs_code=="plow",110,
                               ifelse(full_ops_ext_adj$obs_code %in% c("disk","hoe"),30,
                               ifelse(full_ops_ext_adj$observation_type=="Soil Preparation",5,
                                      NA)))

# restrict to start:end dates
full_ops_ext_adj <- full_ops_ext_adj[full_ops_ext_adj$year %in% experiment_year_range,]

}) # end suppressMessages