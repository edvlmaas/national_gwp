#######################################
# Script: "1_Create_soil_data-setup_LRF"
# Author: "Ellen Maas"
# Date: "July 11, 2022"
# Description: This procedure generates soil data for the site.
#######################################

#!!!!!!!!!! Note: 
######## Limit to 100 cm depth to avoid data issues, and this is also the depth
######## limit on the study ########### 
#!!!!!!


library(apsimx)
library(stringr)
library(dplyr)
library(tidyverse)
library(soiltexture)
library(xml2)
library(lubridate)


# local constants


Cpct_0to10 <- as.numeric(ObsBD$mean_BD)
Cpct_20to40 <- as.numeric(ObsBDdeep_mean[ObsBDdeep_mean$treatment==control_treatment &
                                  ObsBDdeep_mean$section=="Middle","mean_BD"])
Cpct_40to60 <- as.numeric(ObsBDdeep_mean[ObsBDdeep_mean$treatment==control_treatment &
                                  ObsBDdeep_mean$section=="Middle","mean_BD"])
Cpct_60to80 <- as.numeric(ObsBDdeep_mean[ObsBDdeep_mean$treatment==control_treatment &
                                  ObsBDdeep_mean$section=="Deep","mean_BD"])
Cpct_80to200 <- as.numeric(ObsBDdeep_mean[ObsBDdeep_mean$treatment==control_treatment &
                                  ObsBDdeep_mean$section=="Deep","mean_BD"])


###########################
#import and clean
###########################


# download soil data from SSURGO for the lat/lon into a list of "soil.profile"
# classes, pre-formatted for APSIM
sps_raw <- get_ssurgo_soil_profile(lonlat = c(longitude, latitude), nsoil=1)
sps <- sps_raw

# edit attributes from site data and APSIM calibration, relative to each scenario
# based on deep soil cores from 2001 and APSIM calibration

## Bulk density presents an unusual challenge in that it needs to be fixed at the 
## control plot BD for equivalent soil mass between the initial C at land conversion 
## and the current day, because APSIM will compute the SOC stock as BD*Carbon*depth.
## HOWEVER, the other flow attributes (AirDry, LL15, DUL, SAT) need to reflect the
## actual BD at the site (?? I believe) so that the system functions as it actually
## is. So APSIM should be initially calibrated with the treatment BD, then the
## BD changed to the control plot.
##
## For LRF, bulk density for the soil profile will be set to the actual value.
## The control plot bd (in this case, the continuous cotton, with the lowest bd),
## will just be used to calculate the SOC stock, for equivl soil mass comparison.
##
## soil layers are in 20 cm increments to 200 cm
sps[[1]]$soil$BD <- c(Cpct_0to20, 1.2, Cpct_40to60, Cpct_60to80, Cpct_80to200, 
                          Cpct_80to200, Cpct_80to200, Cpct_80to200, Cpct_80to200, Cpct_80to200)
sps[[1]]$soil$AirDry <- c(0.03376907, 0.03376907, 0.03376907, 0.13822352, 
                              0.06916603, 0.02266792, 0.02266792, 0.02266792, 0.02266792, 0.02266792)
sps[[1]]$soil$LL15 <- c(0.06753815, 0.06753815, 0.06753815, 0.13822352, 
                            0.06916603, 0.02266792, 0.02266792, 0.02266792, 0.02266792, 0.02266792)
sps[[1]]$soil$DUL <- if(mgmt_scenario_num==3)
                        c(0.27, 0.27, 0.27, 0.246, 0.142, 0.069, 0.069, 
                          0.069, 0.033, 0.033) else
                        c(0.26, 0.26, 0.26, 0.246, 0.142, 0.069, 0.069, 
                          0.069, 0.033, 0.033)
## original values, using BD of actual treatments; was changed to values in line
## with control plot BD because APSIM won't allow SAT values that high.
# sps[[1]]$soil$SAT <- if(mgmt_scenario_num==1)
#                            c(0.438, 0.438, 0.438, 0.393, 0.385,
#                            0.406, 0.406, 0.406, 0.428, 0.428) else
#                      if(mgmt_scenario_num==2)
#                            c(0.433, 0.433, 0.433, 0.393, 0.385,
#                            0.406, 0.406, 0.406, 0.428, 0.428)
## these values are adjusted to make APSIM happy in layers 2,3,9,10
sps[[1]]$soil$SAT <- if(mgmt_scenario_num==3) 
                        c(0.58, 0.54, 0.393, 0.393, 0.385,
                           0.406, 0.406, 0.406, 0.414, 0.414) else
                        c(0.438, 0.395, 0.395, 0.393, 0.385,
                           0.406, 0.406, 0.406, 0.414, 0.414)
sps[[1]]$soil$Carbon <- if(mgmt_scenario_num==1 | mgmt_scenario_grp %in% c(4:7))
                           c(0.87, 0.43, 0.43, 0.233, 0.19, 0, 0, 0, 0, 0) else 
                        if(mgmt_scenario_num==2)
                           c(0.99, 0.44, 0.44, 0.354, 0.19, 0, 0, 0, 0, 0) else
                        if(mgmt_scenario_num==3)
                           c(0.93, 0.44, 0.44, 0.354, 0.19, 0, 0, 0, 0, 0)
sps[[1]]$soil$SoilCNRatio <- c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10)
sps[[1]]$soil$PH <- c(5.5, 5.5, 5.5, 6.5, 6.5, 6.5, 6.5, 6.5, 6.5, 6.5)
sps[[1]]$soil$ParticleSizeClay <- c(19, 19, 19, 23, 12, 5, 5, 5, 1, 1)
sps[[1]]$soil$ParticleSizeSilt <- c(38, 38, 38, 26, 17, 8, 8, 8, 4, 4)
sps[[1]]$soil$ParticleSizeSand <- c(43, 43, 43, 51, 71, 87, 87, 87, 95, 95)
sps[[1]]$soil$Maize.LL <- c(0.06753815, 0.06753815, 0.06753815, 0.13822352,
                                0.06916603, 0.02266792, 0.02266792, 0.02266792, 0.02266792, 0.02266792)
sps[[1]]$soil$Soybean.LL <- c(0.06753815, 0.06753815, 0.06753815, 0.13822352,
                              0.06916603, 0.02266792, 0.02266792, 0.02266792, 0.02266792, 0.02266792)
sps[[1]]$soil$Wheat.LL <- c(0.06753815, 0.06753815, 0.06753815, 0.13822352,
                                0.06916603, 0.02266792, 0.02266792, 0.02266792, 0.02266792, 0.02266792)
if(mgmt_scenario_num==3) {
sps[[1]]$soil$WhiteClover.KL <- c(0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06)
sps[[1]]$soil$WhiteClover.LL <- c(0.06753815, 0.06753815, 0.06753815, 0.13822352,
                                0.06916603, 0.02266792, 0.02266792, 0.02266792, 0.02266792, 0.02266792)
sps[[1]]$soil$WhiteClover.XF <- c(1,1,1,1,1,1,1,1,1,1)
sps[[1]]$soil$RedClover.KL <- c(0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06)
sps[[1]]$soil$RedClover.LL <- c(0.06753815, 0.06753815, 0.06753815, 0.13822352,
                                0.06916603, 0.02266792, 0.02266792, 0.02266792, 0.02266792, 0.02266792)
sps[[1]]$soil$RedClover.XF <- c(1,1,1,1,1,1,1,1,1,1)
sps[[1]]$soil$Oats.KL <- c(0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06,0.06)
sps[[1]]$soil$Oats.LL <- c(0.06753815, 0.06753815, 0.06753815, 0.13822352,
                                0.06916603, 0.02266792, 0.02266792, 0.02266792, 0.02266792, 0.02266792)
sps[[1]]$soil$Oats.XF <- c(1,1,1,1,1,1,1,1,1,1)
}

# extract just soil data into a dataframe
soil_df_raw <- sps[[1]]$soil

# add three more depths at the top (for Daycent, recommended for trace gas subroutines),
# then add new columns which Daycent also needs

three_layers <- rbind(soil_df_raw[1,], soil_df_raw[1,], soil_df_raw[1,])
three_layers[1,"Depth"] <- "0-2"
three_layers[1,"Thickness"] <- 20
three_layers[1,"FOM"] <- 25
three_layers[2,"Depth"] <- "2-5"
three_layers[2,"Thickness"] <- 30
three_layers[2,"FOM"] <- 25
three_layers[3,"Depth"] <- "5-10"
three_layers[3,"Thickness"] <- 50
three_layers[3,"FOM"] <- 50

soil_df <- three_layers %>%
  rbind(soil_df_raw) %>%
  mutate(upper_depth_cm = as.numeric(word(Depth, 1, sep="-")),
         lower_depth_cm = as.numeric(word(Depth, 2, sep="-")),
         root_fraction = c(0.01, 0.04, 0.25, 0.30, 0.15, 0.1, 0.05, 0.04, 0.03,
                           0.02, 0.01, 0, 0),
         sand_fraction = ParticleSizeSand/100,
         clay_fraction = ParticleSizeClay/100,
         OM_fraction = Carbon*2/100,
         deltamin = c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 
                      0.01, 0.01, 0.01, 0.01),
         ksat_cmsec = KS/(10*24*60*60),
         evap_coef = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
soil_df[4,"Depth"] <- "10-20"
soil_df[4,"Thickness"] <- 100
soil_df[4,"FOM"] <- 50
soil_df[4,"upper_depth_cm"] <- 10

soil_df$KS_cmmin <- soil_df$KS * (1/(10*24*60))

soil_df$LL15_dm3m3 <- soil_df$LL15*1000
soil_df$DUL_dm3m3 <- soil_df$DUL*1000
soil_df$LL15_mmm3 <- soil_df$LL15*1000000000*0.001
soil_df$DUL_mmm3 <- soil_df$DUL*1000000000*0.001

# calculate soil type code from soil texture

soil_texture_df <- soil_df[1,c("sand_fraction","clay_fraction")]
colnames(soil_texture_df) <- c("SAND","CLAY")
soil_texture_df$SILT <- 1 - (soil_texture_df$SAND + soil_texture_df$CLAY)
soil_texture_df <- soil_texture_df %>%
  mutate(SAND=SAND*100,
         SILT=SILT*100,
         CLAY=CLAY*100)

soil_type_ar <- TT.points.in.classes(tri.data=soil_texture_df,class.sys="USDA.TT")

## find the non-zero column which is the soil type
find_col <- names(which(colSums(soil_type_ar)==1))
soil_type_code <- toupper(if_else(find_col=="Cl","clay",
                          if_else(find_col=="Lo","loam",
                          if_else(find_col=="Si","silt",
                          if_else(find_col=="Sa","sand",
                          if_else(find_col=="SiClLo","slcl",
                          if_else(find_col=="SaClLo","sncl",
                                  find_col))))))
                          )


####* NOTE: Will need to address issue of negative values in lower limit,
####* as well as LL15-deltamin resulting in a negative value
####* 
####* 
####*

# notes
#?check_apsimx_soil_profile
#> ?compare_apsim_soil_profile - can return diffs between profiles
#> # cmp created from example - shows bias, etc. between two sites

