#######################################
# Function: "p_Create_soil_data"
# Author: "Ellen Maas"
# Date: "July 11, 2022"
# Output: Function doesn't return any data, hence the "p" (procedure) naming
# convention. It creates files in the appropriate folder for each model.
# Description: "This procedure generates soil data for the site."
#######################################

library(apsimx)

###########################
# APSIM
###########################


edit_apsim_replace_soil_profile(paste0("scen_",clim_scenario_num,"_",mgmt_scenario_num,".apsim"), 
                                soil.profile=sps[[1]],
                                src.dir = apsim_path,
                                wrt.dir = apsim_path,
                                overwrite = TRUE)

