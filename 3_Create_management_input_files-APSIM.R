#######################################
# Function: 3_Create_management_input_files-APSIM
# Author: Ellen Maas
# Date: Sept 23, 2022
# Output: It creates files in the appropriate folder for each model.
# Description: "This procedure generates management data - including crop,
# fertilizer, irrigation, fertilizer, harvest, etc. - for every model in the 
# format needed by each. For APSIM, fertilizer is collected as kg N/ha of the N 
# in the fertilizer (not kg/ha of the whole fertilizer)."
#######################################
# Audit Log
# 9/30/2022: Modified to include T3 data for scenario 3.
# 10/3/2022: Changed hairy vetch crop type to white clover
#######################################

print("Starting 3_Create_management_input_files-APSIM.R")

library(stringr)

if(mgmt_scenario_num!=6) {
  
  
#########################
## experimental period ##
#########################


# Fertilizer units: amount (kg/ha), depth (mm) per https://github.com/APSIMInitiative/ApsimX/blob/279731bdadf36084b21ab40f6d90cf157024fb06/Models/Management/FertiliserApplicationType.cs
# Fertilizer types are limited to specific values per https://github.com/APSIMInitiative/ApsimX/blob/b3d7dffcf0c4905fbb1b25907d0a9f0b9babe4da/Models/Management/Fertiliser.cs
# Plant population is number of plants per m^2, which is available in list of parameters when "[crop name].Sow(" is typed into the APSIM Operations screen. It is also documented in the parent Plant object, per https://github.com/APSIMInitiative/ApsimX/blob/22dee1d9f5a9488932f2197df15ddd414996930a/Models/PMF/Plant.cs

# Build input records
# input_recs <- sapply(full_ops_ext_adj, function(i){
#   if_else(full_ops_ext_adj$observation_type=="Harvest",
#           paste(as.character(full_ops_ext_adj$date),"[",full_ops_ext_adj$crop,"].Harvest"), "NA")
#   },
#   simplify = "array"
#   )

# remove duplicates created by DayCent coding
APSIM_data <- unique(full_ops_ext_adj[,c(1:22,24)])

# select treatment
temp_conv <- APSIM_data[APSIM_data$treatment==treatment & 
                                  !is.na(APSIM_data$obs_code),]
APSIM_conv <- temp_conv[rowSums(is.na(temp_conv)) != ncol(temp_conv),]

#APSIM_ops <- data.frame()

if(file.exists(paste0(apsim_path,"mgmt_",
                    clim_scenario_num,"_",mgmt_scenario_num,".txt"))) {
  file.remove(paste0(apsim_path,"mgmt_",
                    clim_scenario_num,"_",mgmt_scenario_num,".txt")) }

file.create(paste0(apsim_path,"mgmt_",
                    clim_scenario_num,"_",mgmt_scenario_num,".txt")) 

for (i in 1:nrow(APSIM_conv)) {
  current_op <- APSIM_conv[i,"observation_type"]
  if(current_op=="Fertilizer application") {
    cat(paste0(as.character(APSIM_conv[i,"date"])," [Fertiliser].Apply(Amount: ",
               ifelse(str_sub(APSIM_conv[i,"obs_code"],-1,-1)=='N', APSIM_conv[i,"n_rate_kg_ha"],
               ifelse(str_sub(APSIM_conv[i,"obs_code"],-1,-1)=='P', APSIM_conv[i,"p_rate_kg_ha"],
                      APSIM_conv[i,"rate_kg_ha"])),
               ", Type: ", APSIM_conv[i,"obs_code"], ")"),
        file=paste0(apsim_path,"mgmt_",
                    clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE) 
  } else if(current_op=="Harvest" & APSIM_conv[i,"obs_code"]=="grain") {
    cat(paste0(as.character(APSIM_conv[i,"date"])," [",APSIM_conv[i,"crop"],"].Harvest"),
        file=paste0(apsim_path,"mgmt_",
                    clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE) 
    cat(paste0(APSIM_conv[i,"date"],
               " [",APSIM_conv[i,"crop"],"].EndCrop"),
        file=paste0(apsim_path,"mgmt_",
                    clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE)
  } else if(current_op=="Harvest" & APSIM_conv[i,"obs_code"]=="stover") {
    #9/27/2022: changed fraction from 0.9 to 0.75, to leave 25% on surface
    cat(paste0(APSIM_conv[i,"date"], " [SurfaceOrganicMatter].Incorporate(fraction: 0.75, depth: 0)"),
        file=paste0(apsim_path,"mgmt_",
                    clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE) 
  } else if(current_op=="Harvest" & APSIM_conv[i,"obs_code"]=="grain-stover") {
    cat(paste0(as.character(APSIM_conv[i,"date"])," [",APSIM_conv[i,"crop"],"].Harvest"),
        file=paste0(apsim_path,"mgmt_",
                    clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE)
    cat(paste0(APSIM_conv[i,"date"],
               " [",APSIM_conv[i,"crop"],"].EndCrop"),
        file=paste0(apsim_path,"mgmt_",
                    clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE)
    #9/27/2022: changed fraction from 0.9 to 0.75, to leave 25% on surface
    cat(paste0(APSIM_conv[i,"date"], " [SurfaceOrganicMatter].Incorporate(fraction: 0.75, depth: 0)"),
        file=paste0(apsim_path,"mgmt_",
                    clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE) 
  } else if(current_op=="Harvest" & APSIM_conv[i,"obs_code"]=="winterkill") {
    cat(paste0(as.character(APSIM_conv[i,"date"])," [",APSIM_conv[i,"crop"],"].Harvest"),
        file=paste0(apsim_path,"mgmt_",
                    clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE) 
    cat(paste0(APSIM_conv[i,"date"],
               " [",APSIM_conv[i,"crop"],"].EndCrop"),
        file=paste0(apsim_path,"mgmt_",
                    clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE)
    cat(paste0(APSIM_conv[i,"date"], " [SurfaceOrganicMatter].Incorporate(fraction: 0, depth: 0)"),
        file=paste0(apsim_path,"mgmt_",
                    clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE) 
  } else if(current_op=="Harvest" & APSIM_conv[i,"obs_code"]=="mow") {
    cat(paste0(as.character(APSIM_conv[i,"date"])," [",APSIM_conv[i,"crop"],"].Harvest"),
        file=paste0(apsim_path,"mgmt_",
                    clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE) 
    cat(paste0(APSIM_conv[i,"date"],
               " [",APSIM_conv[i,"crop"],"].EndCrop"),
        file=paste0(apsim_path,"mgmt_",
                    clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE)
    cat(paste0(APSIM_conv[i,"date"], " [SurfaceOrganicMatter].Incorporate(fraction: 0, depth: 0)"),
        file=paste0(apsim_path,"mgmt_",
                    clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE) 
  } else if(current_op=="Harvest" & APSIM_conv[i,"obs_code"]=="hoe") {
    cat(paste0(APSIM_conv[i,"date"], " [SurfaceOrganicMatter].Incorporate(fraction: 0, depth: 30)"),
        file=paste0(apsim_path,"mgmt_",
                    clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE) 
  } else if(current_op=="Planting") {
    cat(paste0(as.character(APSIM_conv[i,"date"])," [",APSIM_conv[i,"crop"],
               "].Sow(cultivar: ", APSIM_conv[i,"cultivar"], 
               ", population: ", APSIM_conv[i,"rate_seeds_m2"], 
               ", depth: ",APSIM_conv[i,"seed_depth_mm"],
               ", rowSpacing: ", APSIM_conv[i,"row_spacing_mm"], ")"),
        file=paste0(apsim_path,"mgmt_",
                    clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE)  
  } else if(current_op=="Soil Preparation") {
    cat(paste0(APSIM_conv[i,"date"]," [SurfaceOrganicMatter].Incorporate(fraction: ",
               ifelse(APSIM_conv[i,"obs_code"]=="plow",0.9,
                      ifelse(APSIM_conv[i,"obs_code"] %in% c("disk","hoe"),0.5,
                             0.1))
               ,", depth: ", APSIM_conv[i,"till_depth"]
               ,")"),
        file=paste0(apsim_path,"mgmt_",
                    clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE) 
  } else "Error"
  
} # end for loop



#########################
## APSIM future period ##
#########################


APSIM_ops <- read.delim(paste0(apsim_path,"mgmt_",
                    clim_scenario_num,"_",mgmt_scenario_num,".txt"),header=FALSE)

APSIM_ops_2100 <- data.frame()
APSIM_ops_3yr <- as.data.frame(APSIM_ops[substr(APSIM_ops[,"V1"],1,4) %in% 2019:experiment_end_year,])
colnames(APSIM_ops_3yr) <- "V1"

if(mgmt_scenario_grp==4) {
  APSIM_ops_3yr <- APSIM_ops_3yr %>% 
    mutate(x = as.character(as.numeric(str_extract(V1, "(?i)(?<=Amount:\\D)\\d+"))*fert_adjust),
           V1 = ifelse(str_detect(V1, "Fertiliser"), str_replace(V1,"(?i)(?<=Amount:\\D)\\d+", x), V1)) %>% 
    select(-x)
} else if(mgmt_scenario_grp==5) {
  endcrop_dat <- APSIM_ops_3yr[which(str_detect(APSIM_ops_3yr$V1, "EndCrop")),]
  for (i in 1:length(endcrop_dat)) {
    APSIM_ops_3yr <- insertRows(APSIM_ops_3yr,
                                which(str_detect(APSIM_ops_3yr$V1, "EndCrop"))[i]+1,
                                new=paste0(substr(endcrop_dat[i],1,10),
                                           " [SurfaceOrganicMatter].Incorporate(fraction: ",resid_adjust,", depth: 0)"))
  }
} # end if

repeat_times <- ceiling((2100-experiment_end_year)/3)

for (i in 1:repeat_times) {
substr(APSIM_ops_3yr[,"V1"],1,4) <- as.character(as.integer(substr(APSIM_ops_3yr[,"V1"],1,4)) + 3)
APSIM_ops_2100 <- rbind(APSIM_ops_2100, APSIM_ops_3yr)
}

write.table(APSIM_ops_2100, file=paste0(apsim_path,"mgmt_",
                                        clim_scenario_num,"_",mgmt_scenario_num,".txt"),              
            quote = F, row.names = F, col.names = F, append=TRUE)

} else {  # mgmt_scenario_num=6
  
  #***************************************************
  #*biochar scenarios
  #***************************************************
  
  #########################
  ## experimental period ##
  #########################
  
  
  # remove duplicates created by DayCent coding
  APSIM_data <- unique(full_ops_ext_adj[,c(1:22,24)])
  
  # select treatment
  temp_conv <- APSIM_data[APSIM_data$treatment==treatment & 
                            !is.na(APSIM_data$obs_code),]
  APSIM_conv <- temp_conv[rowSums(is.na(temp_conv)) != ncol(temp_conv),]
  
  #APSIM_ops <- data.frame()
  
  if(file.exists(paste0(apsim_path,"mgmt_",
                        clim_scenario_num,"_",mgmt_scenario_num,".txt"))) {
    file.remove(paste0(apsim_path,"mgmt_",
                       clim_scenario_num,"_",mgmt_scenario_num,".txt")) }
  
  file.create(paste0(apsim_path,"mgmt_",
                     clim_scenario_num,"_",mgmt_scenario_num,".txt")) 
  
  for (i in 1:nrow(APSIM_conv)) {
    current_op <- APSIM_conv[i,"observation_type"]
    if(current_op=="Fertilizer application") {
      cat(paste0(as.character(format(APSIM_conv[i,"date"],"%d/%m/%Y")),
                 "\tfertiliser apply amount = ",
                 ifelse(str_sub(APSIM_conv[i,"obs_code"],-1,-1)=='N', APSIM_conv[i,"n_rate_kg_ha"],
                 ifelse(str_sub(APSIM_conv[i,"obs_code"],-1,-1)=='P', APSIM_conv[i,"p_rate_kg_ha"],
                 APSIM_conv[i,"rate_kg_ha"])),
                 " (kg/ha), depth = 50 (mm), type = ", 
                 ifelse(APSIM_conv[i,"obs_code"]=="NO3N","NO3_N",
                 ifelse(APSIM_conv[i,"obs_code"]=="CalciteCA","Calcite_CA",
                 ifelse(APSIM_conv[i,"obs_code"]=="RockP","Rock_P",
                        APSIM_conv[i,"obs_code"])))),
          file=paste0(apsim_path,"mgmt_",
                      clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE) 
    } else if(current_op=="Harvest" & APSIM_conv[i,"obs_code"]=="grain") {
      NULL # do nothing because there is a rule set in APSIM Classic for harvesting
    } else if(current_op=="Harvest" & APSIM_conv[i,"obs_code"]=="stover") {
      #9/27/2022: changed fraction from 0.9 to 0.75, to leave 25% on surface
      cat(paste0(format(APSIM_conv[i,"date"],"%d/%m/%Y"), 
                 "\tsurfaceorganicmatter tillage type = user_defined, f_incorp = 0.75, tillage_depth = 0)"),
          file=paste0(apsim_path,"mgmt_",
                      clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE) 
    } else if(current_op=="Harvest" & APSIM_conv[i,"obs_code"]=="mow") {
      cat(paste0(format(APSIM_conv[i,"date"],"%d/%m/%Y"), 
                 "\tsurfaceorganicmatter tillage type = user_defined, f_incorp = 0, tillage_depth = 0)"),
          file=paste0(apsim_path,"mgmt_",
                      clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE) 
    } else if(current_op=="Harvest" & APSIM_conv[i,"obs_code"]=="hoe") {
      cat(paste0(format(APSIM_conv[i,"date"],"%d/%m/%Y"), 
                 "\tsurfaceorganicmatter tillage type = user_defined, f_incorp = 0, tillage_depth = 30)"),
          file=paste0(apsim_path,"mgmt_",
                      clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE) 
    } else if(current_op=="Planting") {
      cat(paste0(as.character(format(APSIM_conv[i,"date"],"%d/%m/%Y")),
                 "\t",tolower(APSIM_conv[i,"crop"]), " sow ",
                 "plants = ",APSIM_conv[i,"rate_seeds_m2"], 
                 ", sowing_depth = ",APSIM_conv[i,"seed_depth_mm"],
                 ", cultivar = ", ifelse(APSIM_conv[i,"cultivar"]=="Generic_MG2","mg_2",
                                         tolower(APSIM_conv[i,"cultivar"])),
                 ", row_spacing = ", APSIM_conv[i,"row_spacing_mm"],
                 ", crop_class = plant"),
          file=paste0(apsim_path,"mgmt_",
                      clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE)
    } else if(current_op=="Soil Preparation") {
      cat(paste0(format(APSIM_conv[i,"date"],"%d/%m/%Y"),
                 "\tsurfaceorganicmatter tillage type = user_defined, f_incorp = ",
                 ifelse(APSIM_conv[i,"obs_code"]=="plow",0.9,
                        ifelse(APSIM_conv[i,"obs_code"] %in% c("disk","hoe"),0.5,
                               0.1))
                 ,", tillage_depth = ", APSIM_conv[i,"till_depth"]),
          file=paste0(apsim_path,"mgmt_",
                      clim_scenario_num,"_",mgmt_scenario_num,".txt"), sep="\n",append=TRUE) 
    } else "Error"
    
  } # end for loop
  
  
  
  #########################
  ## APSIM future period ##
  #########################
  
  
  APSIM_ops <- read.delim(paste0(apsim_path,"mgmt_",
                                 clim_scenario_num,"_",mgmt_scenario_num,".txt"),header=FALSE)
  
  APSIM_ops_2100 <- data.frame()
  APSIM_ops_3yr <- as.data.frame(APSIM_ops[str_sub(APSIM_ops[,"V1"],-4,-1) %in% 2019:experiment_end_year,])
  colnames(APSIM_ops_3yr) <- c("V1","V2")
  
  # if(mgmt_scenario_grp==4) {
  #   APSIM_ops_3yr <- APSIM_ops_3yr %>% 
  #     mutate(x = as.character(as.numeric(str_extract(V1, "(?i)(?<=Amount:\\D)\\d+"))*fert_adjust),
  #            V1 = ifelse(str_detect(V1, "Fertiliser"), str_replace(V1,"(?i)(?<=Amount:\\D)\\d+", x), V1)) %>% 
  #     select(-x)
  # } else if(mgmt_scenario_grp==5) {
  #   endcrop_dat <- APSIM_ops_3yr[which(str_detect(APSIM_ops_3yr$V1, "EndCrop")),]
  #   for (i in 1:length(endcrop_dat)) {
  #     APSIM_ops_3yr <- insertRows(APSIM_ops_3yr,
  #                                 which(str_detect(APSIM_ops_3yr$V1, "EndCrop"))[i]+1,
  #                                 new=paste0(substr(endcrop_dat[i],1,10),
  #                                            " [SurfaceOrganicMatter].Incorporate(fraction: ",resid_adjust,", depth: 0)"))
  #   }
  # } # end if
  
  repeat_times <- ceiling((2100-experiment_end_year)/3)
  
  for (i in 1:repeat_times) {
    str_sub(APSIM_ops_3yr[,"V1"],-4,-1) <- as.character(as.integer(str_sub(APSIM_ops_3yr[,"V1"],-4,-1)) + 3)
    APSIM_ops_2100 <- rbind(APSIM_ops_2100, APSIM_ops_3yr)
  }
  
  # Add one-time biochar addition
  biochar_ops <- data.frame(V1="12/08/2022",
                            V2="biochar5v3 tillage  type = user_defined, f_incorp = 0.75, tillage_depth = 300")
  APSIM_ops_2100_biochar <- rbind(APSIM_ops_2100,biochar_ops) %>%
    arrange(as.Date(V1,"%d/%m/%Y"))
  APSIM_ops_2100_biochar_formatted <- paste0(APSIM_ops_2100_biochar$V1,"\t",
                                             APSIM_ops_2100_biochar$V2)
  
  write.table(APSIM_ops_2100_biochar_formatted, file=paste0(apsim_path,"mgmt_",
                                          clim_scenario_num,"_",mgmt_scenario_num,".txt"),              
              quote = F, row.names = F, col.names = F, append=TRUE)
  
} # end if mgmt_scenario_num!=6

###########
# notes with Fernando
# edit_apsimx has pre-set nodes. Other allows editing of anything
# give him files I'm using for management and he'll work on how to do it
