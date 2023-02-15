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
  
  print("Starting 3_Create_management_input_files-setup.R")

library(readxl)
library(dplyr)
#library(magrittr)
#library(pracma)
#library(sqldf)
library(lubridate)
#library(stringi)
# library(tidyverse)
# library(data.table)
# library(XML)
# library(berryFunctions)


  #**********************************************************************
  ## local constants

       
mgmt_path=paste0("Data/",site_name,"/")
adjusted_ops_filename="LibertyResearchFarm.xlsx"

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


#**********************************************************************
## import

######### fertilizer ##########

## use Fert from 0_Observations_and_constants and put into same object
## as before with KBS
temp1_fert <- Fert %>%
  group_by(date,year,treatment,amend_type) %>%
  summarize(n_rate_kg_ha=round(mean(totalN_kgha,na.rm=T),0),
            p_rate_kg_ha=round(mean(totalP_kgha,na.rm=T),0),
            k_rate_kg_ha=round(mean(totalK_kgha,na.rm=T),0))
## need to include ops-specific columns as NA so columns will match when put together
temp2_fert <- temp1_fert %>%
  mutate(date=as.Date(date,format="%m/%d/%Y"),
         year=year(date),
         observation_type="Fertilizer application",
         material=amend_type,
         rate=NA,
         units=NA,
         rowwidth_cm=NA,
         equipment=NA,
         crop=NA,
         # APSIM codes for Fertiliser and crop modules
         cultivar=NA,
         obs_code=if_else(amend_type %like% "lime", "CalciteCA",
                          if_else(n_rate_kg_ha>0, "NO3N", NULL)),
         obs_code2=if_else(p_rate_kg_ha>0, "RockP", NULL),
         row_spacing_mm=NA,
         seed_depth_mm=NA,
         n_rate_g_m2=n_rate_kg_ha/10,
         p_rate_g_m2=p_rate_kg_ha/10,
         k_rate_g_m2=k_rate_kg_ha/10,
         daycent_mgmt_code=if_else(n_rate_g_m2>0,paste0("FERT (",n_rate_g_m2,"N)"),NULL),
         daycent_mgmt_code2=if_else(p_rate_g_m2>0,paste0("FERT (",p_rate_g_m2,"P)"),NULL)
  ) 

# create separate rows for additional APSIM and Daycent codes
## start with just secondary codes
exp_fert <- rbind(temp2_fert[,c(1:3,5:16,18:23)],
        setNames(temp2_fert[!is.na(temp2_fert$daycent_mgmt_code2),c(1:3,5:15,17:22,24)],
                 names(temp2_fert[,c(1:3,5:16,18:23)])))


############ tillage, planting, and harvest ###########

obs_tillage <- obs_tillage_raw[,c("date","year","treatment","Crop","Tillage Event","replicate")] %>%
  mutate(observation_type="Soil Preparation",
         material=NA,
         rate=NA,
         units=NA,
         rowwidth_cm=NA,
         equipment=`Tillage Event`,
         crop=if_else(grepl("Sorghum",Crop,fixed=TRUE), "Sorghum",
              if_else(grepl("Cotton", Crop,fixed=TRUE), "Cotton",
              "Error")),
         cultivar=NA,
         obs_code=if_else(grepl("Disk",equipment,fixed=TRUE), "disk",
                  if_else(grepl("Rod-weed",equipment,fixed=TRUE), "hoe",
                  if_else(grepl("Plow",equipment,fixed=TRUE), "plow",
                  if_else(grepl("Sweep Till",equipment,fixed=TRUE), "finish",
                  if_else(grepl("Rototiller",equipment,fixed=TRUE), "cultivate",
                  "Error"
                  ))))),
         obs_code2=NA,
         ) %>%
  select(-c(`Tillage Event`,Crop,replicate)) %>%
  distinct() %>% # removes duplicates due to replicates
  arrange(date,treatment)


obs_planting <- obs_planting_raw[,c("date","year","treatment","replicate","Crop",
                                    "Cultivar","Planting Density kg/ha",
                                    "Planting Method","Row Width cm")] %>%
  mutate(observation_type="Planting",
         material=paste(Cultivar,Crop),
         rate=`Planting Density kg/ha`,
         units="kg/ha",
         rowwidth_cm=`Row Width cm`,
         equipment=NA,
         crop=if_else(grepl("Sorghum",Crop,fixed=TRUE), "Sorghum",
                      if_else(grepl("Cotton", Crop,fixed=TRUE), "Cotton",
                      if_else(grepl("Rye", Crop,fixed=TRUE), "Rye",
                              "Error"))),
         cultivar=Cultivar,
         obs_code="plant",
         obs_code2=NA,
         ) %>%
  select(-c(Crop,replicate,`Planting Density kg/ha`,
            `Planting Method`,`Row Width cm`,Cultivar)) %>%
  distinct() %>% # removes duplicates due to replicates
  arrange(date,treatment)


obs_harvest <- obs_harvest_raw[,c("date","year","treatment","replicate","Crop",
                                  "Harvested Frac")] %>%
  mutate(observation_type="Harvest",
         material=Crop,
         rate=NA,
         units=NA,
         rowwidth_cm=NA,
         equipment=NA,
         crop=if_else(grepl("Sorghum",Crop,fixed=TRUE), "Sorghum",
                      if_else(grepl("Cotton", Crop,fixed=TRUE), "Cotton",
                              "Error")),
         cultivar=NA,
         obs_code=if_else(`Harvested Frac`=="Lint","lint",
                  if_else(`Harvested Frac` == "Grain", "grain",
                  if_else(`Harvested Frac` == "None", "none",
                          "Error"))),
         obs_code2=NA,
         ) %>%
  select(-c(Crop,replicate,`Harvested Frac`)) %>%
  distinct() %>% # removes duplicates due to replicates
  arrange(date,treatment)

temp_ops <- rbind(obs_tillage, obs_planting, obs_harvest) %>%
  mutate(date = as.Date(date,format="%m/%d/%Y"),
         n_rate_kg_ha = NA,
         p_rate_kg_ha = NA,
         k_rate_kg_ha = NA,
         row_spacing_mm = ifelse(crop %in% c("Maize","Soybean"), 762, 178),
         seed_depth_mm = 50,
         cultivar = ifelse(crop=="Cotton","S71BR",
                    ifelse(crop=="Sorghum","Buster",
                    ifelse(crop=="Rye",covercrop_APSIM,
                    ""))),
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
                           if_else(obs_code=="plant"&crop=="Cotton"&year<2011,"CROP COT",
                           if_else(obs_code=="plant"&crop=="Sorghum"&year>=2011,"CROP SORG",
                           if_else(obs_code=="plant"&crop=="Rye",paste("CROP",covercrop_Daycent),
                           ## assume 75% stover removal on all crops
                           if_else(obs_code=="grain",paste0("HARV G75S"), 
                           ## assuming all-in-one event, ignore "stover" events
                           #if_else(obs_code=="stover"&crop=="Wheat","HARV G90S", 
                           if_else(obs_code %in% c("winterkill","mow"),"HARV KILL", 
                           NULL))))))))))),
         daycent_mgmt_code2=if_else(obs_code=="plant","PLTM",NULL)
  ) 

# create separate rows for secondary Daycent codes
exp_ops <- rbind(temp_ops[,c(1:3,14:16,4:12,17:22)],
                 setNames(temp_ops[!is.na(temp_ops$daycent_mgmt_code2),c(1:3,14:16,4:11,13,17:21,23)],
                          names(temp_ops[,c(1:3,14:16,4:12,17:22)])))


############ combine all field ops ###########

combined_ops <- rbind(exp_ops, exp_fert) %>%
  arrange(date,observation_type) # reorder all the records again

full_ops_ext_adj <- as.data.frame(combined_ops)
full_ops_ext_adj$till_depth <- ifelse(full_ops_ext_adj$obs_code=="plow",110,
                               ifelse(full_ops_ext_adj$obs_code %in% c("disk","hoe"),30,
                               ifelse(full_ops_ext_adj$observation_type=="Soil Preparation",5,
                                      NA)))

# restrict to start:end dates
full_ops_ext_adj <- full_ops_ext_adj[full_ops_ext_adj$year %in% experiment_year_range,]

}) # end suppressMessages