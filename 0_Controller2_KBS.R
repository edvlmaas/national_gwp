#######################################
# File: "0_Controller"
# Author: "Ellen Maas"
# Date: "Sept 23, 2022"
# Description: This script is the control file for the project. 
# It generates all the data input files for all models for a 
# given site. It includes weather, soil, and management. It 
# creates the inputs for APSIM, Daycent, and LDNDC, then runs 
# these three models. Then the process is repeated for RothC 
# and Millennial, which use Daycent output for their input."
#
#######################################
# Audit Log
# 10/23/2022: Converted from .Rmd to .R. Added reporting scripts.
# 11/4/2022: Converted results analysis files to a function
# 11/11/2022: Commented out leading variables due to the creation
# of 00_Main.R, which drives the master process looping through
# all the scenarios.
#
#######################################

 print("Starting 0_Controller.R")

# rm(list=ls())
# master_path <- "~/Modeling"
# setwd(master_path)
# #
# # # Constants
# site_name <- "KBS"
# latitude = 42.410
# longitude = -85.372
# experiment_start_year <- 1989
# experiment_end_year <- 2021
# clim_scenario_num <- 1
# mgmt_scenario_grp <- 1 # scenario group number
# mgmt_scenario_opt <- "" # scenario detail number; put "" if none
# mgmt_scenario_num <- as.numeric(paste0(mgmt_scenario_grp,mgmt_scenario_opt))
# scenario_name <- paste0(clim_scenario_num,"_",mgmt_scenario_num)


# Scenario-dependent scripts and functions

## These are used in multiple functions.
 source("0_Observations_and_constants_KBS.R")


#*************************************************************
#*************************************************************
#* Setup models
#*************************************************************
#*************************************************************


# # Soil data
# 
# if(mgmt_scenario_grp!=6) {
#   ## Prerequisite: APSIM .apsimx file must already exist
#   ## Scenario 6 is setup manually in APSIM Classic
#   source("2_Create_soil_data-setup3.R")
#   #
#   source("2_Create_soil_data-APSIM.R")
#   source("2_Create_soil_data-Daycent.R")
#   source("2_Create_soil_data-LDNDC.R")
#   # RothC only uses clay content, which is included in the weather input file.
#   source("2_Create_soil_data-Millennial.R")
# }


#*************************************************************

# # Management input files (APSIM, Daycent, LDNDC)
# 
# source("3_Create_management_input_files-setup3.R")
# #
# source("3_Create_management_input_files-APSIM.R")
# if(mgmt_scenario_grp!=6) {
#   source("3_Create_management_input_files-Daycent4.R")
#  source("3_Create_management_input_files-LDNDC.R")
# Management input files for RothC, Millennial are created after Daycent runs
# }


#*************************************************************

# # Other files
# if(mgmt_scenario_grp!=6) {
#   source("4_Create_additional_files-LDNDC.R")
# }


#*************************************************************
#*************************************************************
#* Run models
#*************************************************************
#*************************************************************

## APSIM Classic is currently run manually (all mgmt scenario 6).

## To run APSIM Next Gen (all other mgmt scenarios), copy and 
## paste the management data file into the Operations model 
## window, clear the data store, save, and run.

# # APSIM
# if(mgmt_scenario_grp!=6) {
#   source(paste0(apsim_path,"run_APSIM.R"))
# }

# Daycent
# if(mgmt_scenario_grp!=6) {
# source(paste0("Daycent/run_Daycent4.R"))
# source(paste0("Daycent/run_Daycent_eq.R"))
# source(paste0("Daycent/run_Daycent_base.R"))
# source(paste0("Daycent/run_Daycent_exp.R"))
# source(paste0("Daycent/run_Daycent_fut.R"))
# # LDNDC
# }


#*************************************************************
#*************************************************************
#* Graph and analyze APSIM, Daycent, and LDNDC
#*************************************************************
#*************************************************************

# APSIM
 source("9_Results_APSIM-setup.R")
#
model_name <- "APSIM"
# if(clim_scenario_num==1 & mgmt_scenario_grp %in% c(1,2,3)) {
#   source("9_Results_APSIM-calibration2.R")
# }
source("9_Results_APSIM-future.R")
source("p_Results_analysis.R")

#*************************************************************

 # Daycent
 if(mgmt_scenario_grp!=6) {
  source("9_Results_Daycent-setup3.R")
  model_name <- "Daycent"
  # if(clim_scenario_num==1 & mgmt_scenario_grp %in% c(1,2,3)) {
  #   source("9_Results_Daycent-calibration2.R")
  # }
  source("9_Results_Daycent-future.R")
  source("p_Results_analysis.R")
 }


#*************************************************************
#*************************************************************
#* Set up and run Millennial and RothC, driving from Daycent
#*************************************************************
#*************************************************************


# if(mgmt_scenario_grp!=6) {
# # Management input files (RothC, Millennial)
# source("3_Create_management_input_files-setupRM4.R")
# 
# ## Millennial
#   source("3_Create_management_input_files-Millennial2.R")
#   source(paste0(mill_path,"run_Millennial.R"))
# 
#   # ## RothC
#   # source("3_Create_management_input_files-RothC2.R")
#   # source("4_Create_additional_files-RothC2.R")
#
# RothC is currently run manually: after management and scenario
# files are created, open RothC and run the model.
# }


#*************************************************************
#*************************************************************
#* Graph and analyze RothC and Millennial
#*************************************************************
#*************************************************************


# Millennial
 if(mgmt_scenario_grp!=6) {
   source("9_Results_Millennial-setup.R")
  #
  model_name <- "Millennial"
  # if(clim_scenario_num==1 & mgmt_scenario_grp %in% c(1,2,3)) {
  #   source("9_Results_Millennial-calibration.R")
  # }
  source("9_Results_Millennial-future.R")
  source("p_Results_analysis.R")
}


#*************************************************************

  # RothC
if(mgmt_scenario_grp!=6) {
 source("9_Results_RothC-setup.R")
#
model_name <- "RothC"
# if(clim_scenario_num==1 & mgmt_scenario_grp %in% c(1,2,3)) {
#   source("9_Results_RothC-calibration.R")
# }
  source("9_Results_RothC-future.R")
  source("p_Results_analysis.R")
}


#*************************************************************



#*************************************************************
#*************************************************************
#* Graph ensemble compilations
#*************************************************************
#*************************************************************

source("10_Model_Ensemble_results-by_scenario.R")

