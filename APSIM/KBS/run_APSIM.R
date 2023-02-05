# run_APSIM.R
# Notes for implementation
# Need to be able to update:
## Weather file anme
## Clock start date
## Field/SurfaceOrganicMatter: name of initial residue pool, mass of initial surface residue, C:N ratio
## Report(?): crop variables (if include models and variables, can just ignore if not used?)
## Operations
## Soil model (already done)

#######################################################################################################
#
# run_APSIM.R
#
# Author: Ellen Maas Sept 24, 2022
#
# Description: Automates the APSIM simulations. APSIM *must* start with a pre-existing .apsimx file.
#              This file copies a template/"dummy" file and tailors it for the site, then runs it.
#
#######################################################################################################

print("run_APSIM.R")

library(apsimx)

# Clear memory before rerunning the script. 
#rm(list=ls())

# Set the model path to the location of your files.
setwd(apsim_path)

# # just for development:
# mgmt_scenario_grp <- 6 # scenario group number
# mgmt_scenario_det <- 1 # scenario detail number; put "" if none
# mgmt_scenario_num <- as.numeric(paste0(mgmt_scenario_grp,mgmt_scenario_det))
# scenario_name <- paste0(clim_scenario_num,"_",mgmt_scenario_num)

# Other needed constants
sim_filename <- paste0("scen_",scenario_name,".apsimx")

# --------------- Step 1: Copy generic file into site folder --------------- 
#
# This copies the generic .apsimx file from the base APSIM folder into the 
# site-specific folder designated by site_name.

#file.copy("generic_1_1.apsimx", new_filename, overwrite=TRUE)

# --------------- Step 2: Customize .apsimx for the site and scenario --------------- 
#
# Load climate, soil, management (Operations), clear the data store, also add
# info for cover crop soil-related info

# --------------- Step 3: Run model --------------- 
#
# Windows:
# this works from cmd prompt: C:\Program Files\APSIM2022.6.7044.0\bin>Models.exe C:\Users\edmaas\Documents\Modeling\APSIM\KBS\scen_1_61.apsimx
#system(paste0("C:/Program Files/APSIM2022.6.7044.0/bin/Models.exe ", new_filename), wait=TRUE)

# set source directory to full path
sim.dir <- getwd()
sim <- apsimx(sim_filename, src.dir = sim.dir, value = "report")

# Linux: ?

# --------------- Step 4: Clean up ----------------
#
# Set the model path back to the master folder
setwd(master_path)


