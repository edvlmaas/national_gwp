#######################################
# Function: "p_Create_soil_data"
# Author: "Ellen Maas"
# Date: "July 11, 2022"
# Output: Function doesn't return any data, hence the "p" (procedure) naming
# convention. It creates files in the appropriate folder for each model.
# Description: "This procedure generates soil data for the site."
#######################################


###########################
# Millennial
###########################


##Soil parameter set

###########################
# create parameter files

parameters <- data.frame(param_name = c("param_pH","param_bulkd","param_pc","param_claysilt"),
                         param_value = c(soil_df[4,"PH"],soil_df[4,"BD"]*1000,0.86,
                                         (soil_df[4,"ParticleSizeClay"]+soil_df[4,"ParticleSizeSilt"])))
  
###########################
# write parameter files
## this does NOT need scenario designators; always the same for the site
write.table(parameters, file=paste0(mill_path,"/siteparams_in.txt"),
            row.names=F, quote=F, col.names=F, sep=' ')
