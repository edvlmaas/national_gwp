###########################################################################
# Script: run_Daycent4.R
# Author: Ellen Maas July 27, 2022
#         (Adapted from run_DD17CentEVI_Clovis_ds.R)
# Description: Simulates corn-soy-wheat trials at the Kellogg Biological 
# Station, MI.
#
###########################################################################
# Audit Trail
# 9/27/2022: Rewrote to more generic time points, removed site name
# 12/13/2022: Reverted equilibrium run naming to remove scenario number.
# 1/3/2023: Added soiln.out to output files processing.
###########################################################################

print("Starting run_Daycent4.R")

#***** If running this script independently, uncomment this group 
# Clear memory before rerunning the script. 
#rm(list=ls())
#setwd("~")
#site_name="KBS"
# Load observations and global constants
#source("Modeling/0_Observations_and_constants.R")
#*****

# Set the model path to the location of your files.
prior_path <- getwd()
model_path = paste0("~/Modeling/Daycent/",site_name,"/")
#modelPath = paste0("~/Modeling/Daycent/KBS - Copy/") # for testing
setwd(model_path)

# local constants
daycent_executable <- "DD17centEVI.exe"
daycent_list100 <- "DD17list100.exe"
#daycent_executable <- "./DDcentEVI_rev279"
#daycent_list100 <- "./DDlist100_rev279"

# --------------- Step 0: Install this scenario's input files ---------------------
#
# Load scenario-specific input files.
if(mgmt_scenario_grp==2) {
  file.copy("soils_2.in","soils.in", overwrite=TRUE)
  file.copy("site_2.100","site.100", overwrite=TRUE)
  file.copy("fix_2.100","fix.100", overwrite=TRUE)
} else if(mgmt_scenario_grp==3) {
  file.copy("soils_3.in","soils.in", overwrite=TRUE)
  file.copy("site_3.100","site.100", overwrite=TRUE)
  file.copy("fix_3.100","fix.100", overwrite=TRUE)
} else {
  file.copy("soils_1.in","soils.in", overwrite=TRUE)
  file.copy("site_1.100","site.100", overwrite=TRUE)
  file.copy("fix_1.100","fix.100", overwrite=TRUE)
}

# use default crop.100 except for future for scenario 3
file.copy("crop_allothers.100","crop.100", overwrite=TRUE)

# --------------- Step 1: Run equilibrium simulation (4000-year spin-up) ---------------
#
# This generates native plant and soil conditions up to the point of conversion
# from the native ecosystem to historical cropping at the site.
#
# This equilibrium simulation takes a long time, so don't execute
# these commands if you already have an equilibrium binary file (*.bin)
# and you haven't made any changes to any parameter files.

# Equilibrium: 4000 years of grazed grassland
# Every scenario run uses the same equilibrium schedule file, so there is only one
print("**********Daycent equilibrium simulation*********")
file.copy("outfiles_eq.in", "outfiles.in", overwrite=TRUE)
unlink(paste0("sched_eq.bin"))
unlink(paste0("sched_eq.lis"))
# -s parameter is the schedule file name, -n parameter is the binary output file name,
# minus the dot-extentions
system(paste0(daycent_executable," -s sched_eq -n sched_eq"), wait=TRUE)
system(paste0(daycent_list100," sched_eq sched_eq outvars.txt"), wait=TRUE)

# the following old code created different equil files for each scenario - why?
file.copy("outfiles_eq.in", "outfiles.in", overwrite=TRUE)
unlink(paste0("sched_eq_",scenario_name,".bin"))
unlink(paste0("sched_eq_",scenario_name,".lis"))
# -s parameter is the schedule file name, -n parameter is the binary output file name,
# minus the dot-extentions
system(paste0(daycent_executable," -s sched_eq ",
              " -n sched_eq_",scenario_name), wait=TRUE)
system(paste0(daycent_list100," sched_eq sched_eq_",scenario_name,
              " outvars.txt"), wait=TRUE)

# --------------- Step 2: Run base cropping simulations (land conversion - start exp) ---------------

# Base cropping schedule: date of land conversion -> start of experimental period
# Every scenario run uses the same base schedule file, so there is only one
print("**********Daycent base simulation*********")
unlink(paste0("sched_base_",scenario_name,".bin"))
unlink(paste0("sched_base_",scenario_name,".lis"))
unlink("cflows.out")
unlink(paste0("cflows_base_",scenario_name,".out"))
#file.copy("dc_sip.csv", "dc_sip_base.csv", overwrite=TRUE)
unlink("harvest.csv")
unlink(paste0("harvest_base_",scenario_name,".csv"))
unlink("livec.out")
unlink(paste0("livec_base_",scenario_name,".out"))
unlink("methane.out")
unlink(paste0("methane_base_",scenario_name,".out"))
#file.copy("nflux.out", "nflux_base.out", overwrite=TRUE)
unlink("soiltavg.out")
unlink(paste0("soiltavg_base_",scenario_name,".out"))
unlink("soiln.out")
unlink(paste0("soiln_base_",scenario_name,".out"))
unlink("summary.out")
unlink(paste0("summary_base_",scenario_name,".out"))
unlink("vswc.out")
unlink(paste0("vswc_base_",scenario_name,".out"))

file.copy("outfiles_base.in", "outfiles.in", overwrite=TRUE)

system(paste0(daycent_executable," -s sched_base",
              " -n sched_base_",scenario_name,
              " -e sched_eq"), wait=TRUE)
system(paste0(daycent_list100," sched_base sched_base_",scenario_name,
              " outvars.txt"), wait=TRUE)

file.copy("cflows.out", paste0("cflows_base_",scenario_name,".out"), overwrite=TRUE)
#file.copy("dc_sip.csv", "dc_sip_base.csv", overwrite=TRUE)
file.copy("harvest.csv", paste0("harvest_base_",scenario_name,".csv"), overwrite=TRUE)
file.copy("livec.out", paste0("livec_base_",scenario_name,".out"), overwrite=TRUE)
file.copy("methane.out", paste0("methane_base_",scenario_name,".out"), overwrite=TRUE)
#file.copy("nflux.out", "nflux_base.out", overwrite=TRUE)
file.copy("soiln.out", paste0("soiln_base_",scenario_name,".out"), overwrite=TRUE)
file.copy("soiltavg.out", paste0("soiltavg_base_",scenario_name,".out"), overwrite=TRUE)
file.copy("summary.out", paste0("summary_base_",scenario_name,".out"), overwrite=TRUE)
file.copy("vswc.out", paste0("vswc_base_",scenario_name,".out"), overwrite=TRUE)

# --------------- Step 3: Run experimental period simulations  ---------------

print("**********Daycent experimental period simulation*********")
# Remove all prior output files
unlink(paste0("sched_exp_",scenario_name,".bin"))
unlink(paste0("sched_exp_",scenario_name,".lis"))
unlink("cflows.out")
unlink(paste0("cflows_exp_",scenario_name,".out"))
#file.copy("dc_sip.csv", paste0("dc_sip_exp_",scenario_name,".csv"), overwrite=TRUE)
unlink("harvest.csv")
unlink(paste0("harvest_exp_",scenario_name,".csv"))
unlink("livec.out")
unlink(paste0("livec_exp_",scenario_name,".out"))
unlink("methane.out")
unlink(paste0("methane_exp_",scenario_name,".out"))
#file.copy("nflux.out", paste0("nflux_exp_",scenario_name,".out"), overwrite=TRUE)
unlink("soiln.out")
unlink(paste0("soiln_exp_",scenario_name,".out"))
unlink("soiltavg.out")
unlink(paste0("soiltavg_exp_",scenario_name,".out"))
unlink("summary.out")
unlink(paste0("summary_exp_",scenario_name,".out"))
unlink("vswc.out")
unlink(paste0("vswc_exp_",scenario_name,".out"))

# Experiment cropping schedule: full experimental period
file.copy("outfiles_base.in", "outfiles.in", overwrite=TRUE)

system(paste0(daycent_executable," -s sched_exp_",scenario_name,
              " -n sched_exp_",scenario_name,
              " -e sched_base_", scenario_name), wait=TRUE)
system(paste0(daycent_list100," sched_exp_",scenario_name,
              " sched_exp_",scenario_name," outvars.txt"), wait=TRUE)

file.copy("cflows.out", paste0("cflows_exp_",scenario_name,".out"), overwrite=TRUE)
#file.copy("dc_sip.csv", paste0("dc_sip_exp_",scenario_name,".csv"), overwrite=TRUE)
file.copy("harvest.csv", paste0("harvest_exp_",scenario_name,".csv"), overwrite=TRUE)
file.copy("livec.out", paste0("livec_exp_",scenario_name,".out"), overwrite=TRUE)
file.copy("methane.out", paste0("methane_exp_",scenario_name,".out"), overwrite=TRUE)
#file.copy("nflux.out", paste0("nflux_exp_",scenario_name,".out"), overwrite=TRUE)
file.copy("soiln.out", paste0("soiln_exp_",scenario_name,".out"), overwrite=TRUE)
file.copy("soiltavg.out", paste0("soiltavg_exp_",scenario_name,".out"), overwrite=TRUE)
file.copy("summary.out", paste0("summary_exp_",scenario_name,".out"), overwrite=TRUE)
file.copy("vswc.out", paste0("vswc_exp_",scenario_name,".out"), overwrite=TRUE)

# --------------- Step 4: Run future emissions scenarios (end exp-2100) --------------- 

# fudge the future results because everything but wheat yield is dropping
if(mgmt_scenario_grp==3) {
  file.copy("crop_3_fut.100","crop.100", overwrite=TRUE)
}

print("**********Daycent future simulation*********")
# Remove all prior output files
unlink(paste0("sched_fut_",scenario_name,".bin"))
unlink(paste0("sched_fut_",scenario_name,".lis"))
unlink("cflows.out")
unlink(paste0("cflows_fut_",scenario_name,".out"))
unlink("harvest.csv")
unlink(paste0("harvest_fut_",scenario_name,".csv"))
unlink("livec.out")
unlink(paste0("livec_fut_",scenario_name,".out"))
unlink("methane.out")
unlink(paste0("methane_fut_",scenario_name,".out"))
#file.copy("dc_sip.csv", paste0("dc_sip_fut_",scenario_name,".csv"), overwrite=TRUE)
#file.copy("nflux.out", paste0("nflux_fut_",scenario_name,".out"), overwrite=TRUE)
unlink("soiln.out")
unlink(paste0("soiln_fut_",scenario_name,".out"))
unlink("soiltavg.out")
unlink(paste0("soiltavg_fut_",scenario_name,".out"))
unlink("summary.out")
unlink(paste0("summary_fut_",scenario_name,".out"))
unlink("vswc.out")
unlink(paste0("vswc_fut_",scenario_name,".out"))

# Future schedule: end of experimental period-2100
file.copy("outfiles_base.in", "outfiles.in", overwrite=TRUE) #generate same output files as baseline
system(paste0(daycent_executable," -s sched_fut_",scenario_name,
              " -n sched_fut_",scenario_name,
              " -e sched_exp_",scenario_name), wait=TRUE)
system(paste0(daycent_list100," sched_fut_",scenario_name,
              " sched_fut_",scenario_name," outvars.txt"), wait=TRUE)
file.copy("cflows.out", paste0("cflows_fut_",scenario_name,".out"), overwrite=TRUE)
file.copy("harvest.csv", paste0("harvest_fut_",scenario_name,".csv"), overwrite=TRUE)
file.copy("livec.out", paste0("livec_fut_",scenario_name,".out"), overwrite=TRUE)
file.copy("methane.out", paste0("methane_fut_",scenario_name,".out"), overwrite=TRUE)
#file.copy("dc_sip.csv", paste0("dc_sip_fut_",scenario_name,".csv"), overwrite=TRUE)
#file.copy("nflux.out", paste0("nflux_fut_",scenario_name,".out"), overwrite=TRUE)
file.copy("soiln.out", paste0("soiln_fut_",scenario_name,".out"), overwrite=TRUE)
file.copy("soiltavg.out", paste0("soiltavg_fut_",scenario_name,".out"), overwrite=TRUE)
file.copy("summary.out", paste0("summary_fut_",scenario_name,".out"), overwrite=TRUE)
file.copy("vswc.out", paste0("vswc_fut_",scenario_name,".out"), overwrite=TRUE)

# put it back
file.copy("crop_allothers.100","crop.100", overwrite=TRUE)

# --------------- Step 5: Reset working directory --------------- 
setwd(prior_path)

