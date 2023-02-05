#######################################
# Function: "p_Create_soil_data"
# Author: "Ellen Maas"
# Date: "July 11, 2022"
# Output: Function doesn't return any data, hence the "p" (procedure) naming
# convention. It creates files in the appropriate folder for each model.
# Description: "This procedure generates soil data for the site."
#######################################


###########################
#Daycent
###########################

# collect: upper depth of soil layer (cm), lower depth of soil layer (cm), bulk density (g/cc),
# field capacity (volumetric), wilting point (volumetric), 

## cannot name with scenario numbers
write.table(soil_df[,c("upper_depth_cm","lower_depth_cm","BD","DUL","LL15","evap_coef",
                       "root_fraction","sand_fraction","clay_fraction","OM_fraction",
                       "deltamin","ksat_cmsec","PH")], 
            file=paste0(daycent_path,"soils_",scenario_name,".in"),
            row.names=F, quote=F, col.names=F, sep=' ')

