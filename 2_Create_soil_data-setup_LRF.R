#######################################
# Script: "1_Create_soil_data-setup_LRF"
# Author: "Ellen Maas"
# Date: "July 11, 2022"
# Description: This procedure generates soil data for the site. 
#######################################
# Audit Log
# 2/13/2023: Updated for LRF site. WOSIS soil profile data were manually 
# downloaded from https://soilgrids.org/. Hydrologic data were calculated from
# Saxton and Rawls (2006).
#######################################

#!!!!!!!!!! Note: 
######## Limit to 100 cm depth to avoid data issues, and this is also the depth
######## limit on the study ########### 
#!!!!!!

library(readr)
library(magrittr)
library(dplyr)
library(apsimx)
library(stringr)
library(tidyverse)
library(soiltexture)
#library(xml2)
#library(lubridate)


###########################
# import and manually set up 0-5, 5-15 cm depth
###########################

bd_raw<-read_csv(paste0(obs_soil_path,"wosis_latest_bd.csv"),show_col_types = FALSE)
sand_raw<-read_csv(paste0(obs_soil_path,"wosis_latest_sand.csv"),show_col_types = FALSE)
silt_raw<-read_csv(paste0(obs_soil_path,"wosis_latest_silt.csv"),show_col_types = FALSE)
clay_raw<-read_csv(paste0(obs_soil_path,"wosis_latest_clay.csv"),show_col_types = FALSE)
oc_raw<-read_csv(paste0(obs_soil_path,"wosis_latest_orgc.csv"),show_col_types = FALSE)
ph_raw<-read_csv(paste0(obs_soil_path,"wosis_latest_ph.csv"),show_col_types = FALSE)
tc_raw<-read_csv(paste0(obs_soil_path,"wosis_latest_totc.csv"),show_col_types = FALSE)

bd_df <- bd_raw[bd_raw$profile_id==161429,c("X","Y","profile_id",
                                            "upper_depth","lower_depth",
                                            "bdfiod_value_avg")]
sand_df <- sand_raw[sand_raw$profile_id==161429,c("X","Y","profile_id",
                                                  "upper_depth","lower_depth",
                                                  "sand_value_avg")]
silt_df <- silt_raw[silt_raw$profile_id==161429,c("X","Y","profile_id",
                                                  "upper_depth","lower_depth",
                                                  "silt_value_avg")]
clay_df <- clay_raw[clay_raw$profile_id==161429,c("X","Y","profile_id",
                                                  "upper_depth","lower_depth",
                                                  "clay_value_avg")]
oc_df <- oc_raw[oc_raw$profile_id==161429,c("X","Y","profile_id",
                                            "upper_depth","lower_depth",
                                            "orgc_value_avg")] %>%
  mutate(orgc_value_avg_pct=orgc_value_avg/10)
ph_df <- ph_raw[ph_raw$profile_id==161429,c("X","Y","profile_id",
                                            "upper_depth","lower_depth",
                                            "phaq_value_avg")]
tc_df <- tc_raw[tc_raw$profile_id==161429,c("X","Y","profile_id",
                                            "upper_depth","lower_depth",
                                            "totc_value_avg")]

soil_prof_mrg <- merge(bd_df,sand_df,by=c("X","Y","profile_id",
                                          "upper_depth","lower_depth"),
                       all=TRUE) %>%
  merge(silt_df,by=c("X","Y","profile_id",
                     "upper_depth","lower_depth"),
        all=TRUE) %>%
  merge(clay_df,by=c("X","Y","profile_id",
                     "upper_depth","lower_depth"),
        all=TRUE) %>%
  merge(oc_df[,c("X","Y","profile_id",
                 "upper_depth","lower_depth","orgc_value_avg_pct")],
        by=c("X","Y","profile_id",
             "upper_depth","lower_depth"),
        all=TRUE) %>%
  merge(ph_df,by=c("X","Y","profile_id",
                   "upper_depth","lower_depth"),
        all=TRUE) #%>%
# merge(tc_df,by=c("X","Y","profile_id",
#                  "upper_depth","lower_depth"),
#       all=TRUE)

top_0_5 <- data.frame(X="-101.8211",
                      Y="33.69083",
                      profile_id="161429",
                      upper_depth=0,
                      lower_depth=5,
                      bdfiod_value_avg=1.272,
                      sand_value_avg=67,
                      silt_value_avg=16,
                      clay_value_avg=17,
                      orgc_value_avg_pct=0.49,
                      phaq_value_avg=7.43)
top_5_15 <- data.frame(X="-101.8211",
                       Y="33.69083",
                       profile_id="161429",
                       upper_depth=5,
                       lower_depth=15,
                       bdfiod_value_avg=1.356,
                       sand_value_avg=67,
                       silt_value_avg=16,
                       clay_value_avg=17,
                       orgc_value_avg_pct=0.49,
                       phaq_value_avg=7.43)

soil_prof_df <- rbind(top_0_5,top_5_15,soil_prof_mrg[soil_prof_mrg$upper_depth>=15,])
soil_prof_df <- soil_prof_df[order(soil_prof_df$upper_depth),]
# fill in missing bulk density at deepest depth with layer above
soil_prof_df[nrow(soil_prof_df),"bdfiod_value_avg"] <- soil_prof_df[nrow(soil_prof_df)-1,"bdfiod_value_avg"] 


###########################
# calculate soil parameters
###########################

delta_min_vr <- c(0.008, 0.006, 0.004, 0.002, 0, 0, 0, 0, 0)

# hydrological, from Saxton and Rawls (2006)
full_soil_prof_df <- soil_prof_df %>%
  mutate(sand_frac = sand_value_avg/100,
         silt_frac = silt_value_avg/100,
         clay_frac = clay_value_avg/100,
         OM_frac = orgc_value_avg_pct*1.724/100, # organic matter fraction
         O1500t = -0.024*sand_frac + 0.487*clay_frac + 0.006*OM_frac +
          0.005*sand_frac*OM_frac - 0.013*clay_frac*OM_frac +
          0.068*sand_frac*clay_frac + 0.031, # helper equation to LL15
         LL15 = O1500t + (0.14 * O1500t - 0.02), # permanent wilting point, %
         O33t = -0.251*sand_frac + 0.195*clay_frac + 0.011*OM_frac +
           0.006*sand_frac*OM_frac - 0.027*clay_frac*OM_frac +
           0.452*sand_frac*clay_frac + 0.299, # helper equation to DUL
         DUL = O33t + (1.283*O33t^2 - 0.374*O33t - 0.015), # field capacity, %
         SAT = 1 - bdfiod_value_avg/2.65, # moisture at saturation, %; 2.65 = assumed particle density
         B = (log(1500) - log(33))/(log(DUL) - log(LL15)), # moisture-tension coefficient
         lamda = 1/B, # slope of tension-moisture curve
         Ks = 1930*(SAT-DUL)^(3-lamda), # saturated conductivity (mm h-1)
         Ks_mmday = Ks*24,
         Ks_cmsec = Ks/10/60/60,
         Ks_cmhr = Ks/10
          )

# add model-specific formatted elements
full_soil_prof_df <- full_soil_prof_df %>%
  mutate(Depth=paste0("",upper_depth,'-',lower_depth,""),
         Thickness=(lower_depth-upper_depth)*10, # in mm
         AirDry=LL15 - delta_min_vr
         )


###########################
# get APSIMX object and update with manual/calculated data
###########################

# in order to get manual data into APSIM, will need to pull down a SSURGO
# record for the format, then update all the values
sp_raw <- get_isric_soil_profile(lonlat = c(longitude, latitude))
sps <- sp_raw

# limit observed soil profile number of layers to the isric data
full_soil_prof_df <- full_soil_prof_df[1:length(sps),]

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
sps$soil$Depth <- full_soil_prof_df$Depth
sps$soil$Thickness <- full_soil_prof_df$Thickness
sps$soil$BD <- full_soil_prof_df$bdfiod_value_avg
sps$soil$AirDry <- full_soil_prof_df$AirDry
sps$soil$LL15 <- full_soil_prof_df$LL15
sps$soil$DUL <- full_soil_prof_df$DUL
sps$soil$SAT <- full_soil_prof_df$SAT
sps$soil$KS <- full_soil_prof_df$Ks_mmday
sps$soil$Carbon <- full_soil_prof_df$orgc_value_avg_pct
sps$soil$SoilCNRatio <- rep(10,length(sps))
sps$soil$PH <- full_soil_prof_df$phaq_value_avg
sps$soil$ParticleSizeClay <- full_soil_prof_df$clay_value_avg
sps$soil$ParticleSizeSilt <- full_soil_prof_df$sand_value_avg
sps$soil$ParticleSizeSand <- full_soil_prof_df$sand_value_avg
sps$soil$Cotton.KL <- sps$soil$Maize.KL
sps$soil$Cotton.LL <- full_soil_prof_df$LL15
sps$soil$Cotton.XF <- sps$soil$Maize.XF
sps$soil$Sorghum.KL <- sps$soil$Maize.KL
sps$soil$Sorghum.LL <- full_soil_prof_df$LL15
sps$soil$Sorghum.XF <- sps$soil$Maize.XF
if(mgmt_scenario_grp %in% c(3,8)) {
sps$soil$Rye.KL <- sps$soil$Maize.KL
sps$soil$Rye.LL <- full_soil_prof_df$LL15
sps$soil$Rye.XF <- sps$soil$Maize.XF
}
# remove unneeded default list elements
sps$soil$Maize.KL <- NULL
sps$soil$Maize.LL <- NULL
sps$soil$Maize.XF <- NULL
sps$soil$Soybean.KL <- NULL
sps$soil$Soybean.LL <- NULL
sps$soil$Soybean.XF <- NULL
sps$soil$Wheat.KL <- NULL
sps$soil$Wheat.LL <- NULL
sps$soil$Wheat.XF <- NULL

# adjust crops, soil type
if(mgmt_scenario_grp %in% c(3,8)) {
sps$crops <- c("Cotton","Sorghum","Ryegrass")
} else {
  sps$crops <- c("Cotton","Sorghum")
}

sps$metadata$SoilType <- "SoilType = sandy loam"

# extract just soil data into a dataframe
soil_df_raw <- sps$soil

# break up depths at the top (for Daycent, recommended for trace gas subroutines),
# then add new columns which Daycent also needs

three_layers <- rbind(soil_df_raw[1,], soil_df_raw[1,], soil_df_raw[2,])
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
  rbind(soil_df_raw[2:nrow(soil_df_raw),])
soil_df[4,"Depth"] <- "10-20"
soil_df[4,"Thickness"] <- 100
soil_df[4,3:28] <- (soil_df[3,3:28]+soil_df[5,3:28])/2
soil_df[5,"Depth"] <- "20-36"
soil_df[5,"Thickness"] <- 160

soil_df <- soil_df %>%
  mutate(upper_depth_cm = as.numeric(word(Depth, 1, sep="-")),
         lower_depth_cm = as.numeric(word(Depth, 2, sep="-")),
         root_fraction = c(0.01, 0.04, 0.15, 0.15, 0.10, 0.05, 0.05, 0.04),
         sand_fraction = ParticleSizeSand/100,
         clay_fraction = ParticleSizeClay/100,
         OM_fraction = Carbon*1.724/100,
         deltamin = delta_min_vr[1:nrow(soil_df)],
         ksat_cmsec = KS/(10*24*60*60),
         evap_coef = c(0.8, 0.2, 0, 0, 0, 0, 0, 0))


# include additional units
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

