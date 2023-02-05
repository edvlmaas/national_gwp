#######################################
# File: "00_Main.R"
# Author: "Ellen Maas"
# Date: "Nov 11, 2022"
# Description: This is the master process that drives the looping
# through all the scenarios and which will be what runs on each 
# cluster for the gridded study."
#
#######################################
# Calls:
# 0_Controller.R
#
#######################################
# Audit Log
# 11/11/2022: Created script.
# 11/17/2022: Added ensemble results script to end.
# 12/21/2022: Added weather script in climate loop.
#######################################
library(pracma)
library(dplyr)

# start timer
tic()

rm(list=ls())
master_path <- "~/Modeling"
setwd(master_path)
apsimx_options(exe.path="/bin/lib64/R/library/apsimx/R/")

site_name <- "KBS"
latitude = 42.410
longitude = -85.372
experiment_start_year <- 1989
experiment_end_year <- 2021
fut_weather_path <- paste0("Data/CMIP6/",site_name,"/")

#source("p_Create_future_weather_files.R")

# Loop through the scenarios; set which climate and management
# scenario numbers to use for this run:
clim_nums <- c(1:5)
mgmt_grps <- c(1:6)

for (x in clim_nums) { # climate scenarios
  print("************************************")
  print("####### New climate scenario #######")
  print("************************************")
  print(paste0("climate scenario: ",x))
  clim_scenario_num <- x
  #p_Create_future_weather_files(clim_scenario_num,latitude,longitude,
  #                              experiment_end_year)
  #source("1_Create_weather_input_files.R")
  for (y in mgmt_grps) { # management scenario groups
    mgmt_scenario_grp <- y # scenario group number
    max_scenario_options <- if_else(y==4, 4, # option numbers for those with incremental adjustments
                            if_else(y==5, 3,
                            if_else(y==6, 5, 1)))
    
    for (z in 1:max_scenario_options) {
      print("************************************")
      print(paste0("climate scenario: ",x))
      print(paste0("mgmt scenario: ",y))
      print(paste0("mgmt option: ",z))
      mgmt_scenario_opt <- if(max_scenario_options==1) "" else z
      mgmt_scenario_num <- as.numeric(paste0(mgmt_scenario_grp,mgmt_scenario_opt))
      scenario_name <- paste0(clim_scenario_num,"_",mgmt_scenario_num) 
      source("0_Controller2.R")
    }
    
  } # end loop through management scenario groups
} # end loop through climate scenarios

#source("10_Model_Ensemble_results-combined_scenarios2.R")

# end timer
run_time <- round(toc(echo=TRUE)/60,1)
print(paste0("Run time is ",run_time," minutes, ",run_time/60," hours."))

      