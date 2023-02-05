#######################################
# Name: "p_Results_analysis.R"
# Author: "Ellen Maas"
# Date: "Nov. 4, 2022"
# Output: This script calculates summary data for analysis after a model run and
# inserts or updates the output file."
#######################################
# Calls:
# p_Edit_output_file.R
#######################################
# Audit Log
# 
#######################################

print("Starting p_Results_analysis.R")

#p_Results_analysis <- function(model_name) {

SOC_end_year <- Cstock_Mgha[nrow(Cstock_Mgha),"year"]

SOC_diff_fut <- Cstock_Mgha[Cstock_Mgha$year==SOC_end_year,model_name] - 
  Cstock_Mgha[Cstock_Mgha$year==experiment_end_year,model_name]

MaizeYld_diff_fut <- NA

SoyYld_diff_fut <- NA

WheatYld_diff_fut <- NA

N2O_total_fut <- NA

CH4_total_fut <- NA

if(model_name %in% c("APSIM","Daycent","LDNDC")) {
  MY_end_year <- MaizeYld_Mgha[nrow(MaizeYld_Mgha),"year"]
  SY_end_year <- SoyYld_Mgha[nrow(SoyYld_Mgha),"year"]
  WY_end_year <- WheatYld_Mgha[nrow(WheatYld_Mgha),"year"]
  N2O_end_year <- MaizeYld_Mgha[nrow(MaizeYld_Mgha),"year"]
  
  MaizeYld_diff_fut <- round(MaizeYld_Mgha[MaizeYld_Mgha$year==MY_end_year,model_name] - 
                               MaizeYld_Mgha[MaizeYld_Mgha$year==
                                               max(MaizeYld_Mgha[MaizeYld_Mgha$year %in% experiment_year_range,"year"],na.rm=T)
                                             ,model_name],1)
  
  SoyYld_diff_fut <- round(SoyYld_Mgha[SoyYld_Mgha$year==SY_end_year,model_name] - 
                             SoyYld_Mgha[SoyYld_Mgha$year==
                                           max(SoyYld_Mgha[SoyYld_Mgha$year %in% experiment_year_range,"year"],na.rm=T)
                                         ,model_name],1)
  
  WheatYld_diff_fut <- round(WheatYld_Mgha[WheatYld_Mgha$year==WY_end_year,model_name] - 
                               WheatYld_Mgha[WheatYld_Mgha$year==
                                               max(WheatYld_Mgha[WheatYld_Mgha$year %in% experiment_year_range,"year"],na.rm=T)
                                             ,model_name],1)
  
  N2O_total_fut <- sum(N2O_ghaday[N2O_ghaday$date>experiment_end_date,model_name])/1000
  
} # end if - all but SOC and methane

if(model_name %in% c("Daycent","LDNDC")) {
  
  CH4_end_year <- MaizeYld_Mgha[nrow(MaizeYld_Mgha),"year"]
  CH4_total_fut <- sum(CH4_ghaday[CH4_ghaday$date>experiment_end_date,model_name])/1000
  
} # end if - methane



summary_tab <- cbind(model_name,clim_scenario_num,mgmt_scenario_num,
                     scenario_name) %>% 
  cbind(MaizeYld_diff_fut,SoyYld_diff_fut,WheatYld_diff_fut,
        SOC_diff_fut,N2O_total_fut,CH4_total_fut)

# write.table(summary_tab,file=paste0("Summary_data_",scenario_name,"_",model_name,".csv"),
#             col.names=c("Model","Climate_Scenario","Mgmt_Scenario","Scenario_Name",
#                         "SOC_Diff_Mgha","Total_N2O_kgha","Total_CH4_kgha")
#             ,row.names=F,sep=",")

# call function to edit the output file
source("p_Edit_output_file.R")
p_Edit_output_file(summary_tab,model_name,scenario_name)

#} # end function
