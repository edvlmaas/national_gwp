#######################################
# Function: 3_Create_management_input_files-APSIM
# Author: Ellen Maas
# Date: Aug 30, 2022
# Output: It creates files in the appropriate folder for each model.
# Description: "This procedure generates management data - including crop,
# fertilizer, irrigation, fertilizer, harvest, etc. - for every model in the 
# format needed by each. For APSIM, fertilizer is collected as kg N/ha of the N 
# in the fertilizer (not kg/ha of the whole fertilizer)."
#######################################
# Audit Log
# 8/30/2022: Created script
# 12/14/2022: Added logging code to end.
#######################################

suppressMessages({
  
print("Starting 9_Results_RothC-calibration.R")

library(readxl)
#library(plotly)
library(magrittr)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(ggplot2)


#**********************************************************************

# Temporal graphs

## historical-experimental period

Cfit_RothC <- coef(lm(RothC ~ year, data = Cstock_Mgha[Cstock_Mgha$year >= experiment_start_year,]))

if(mgmt_scenario_grp!=3) { 
Cfit_Obs <- coef(lm(Observed ~ year, data = Cstock_Mgha[Cstock_Mgha$year >= experiment_start_year,]))
} else {
  Cfit_Obs <- coef(lm(Observed ~ year, data = Cstock_Mgha[Cstock_Mgha$year!=1998 &
                                                            Cstock_Mgha$year >= experiment_start_year,]))
}


gC <- Cstock_Mgha_piv[Cstock_Mgha_piv$year < experiment_end_year,] %>%
ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
  geom_point(show.legend=TRUE) +
  xlab("Year") +
  ylab(expression('SOC stock (Mg C ha' ^-1*')')) +
#  ylim(15, 65) +
  ggtitle(paste(site_name,"Soil Organic Carbon"),
          paste0("Scenario: ",scenario_descriptor)) +
  ggtitle(paste(site_name,"Soil Organic Carbon: Scenario ",clim_scenario_num,"_",mgmt_scenario_num)) +
#  geom_abline(intercept=Cfit_RothC[1], slope=Cfit_RothC[2], color="orange") +
  geom_abline(intercept=Cfit_Obs[1], slope=Cfit_Obs[2], color="black") +
  scale_color_manual(labels=c("Observed","RothC"),
                     values=cbPalette9[c(1,8)]) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gC 


ggsave(filename=paste0(results_path,"calib_SOC_comparison_his_",
                              clim_scenario_num,"_",mgmt_scenario_num,"_RothC.jpg"),
                       plot=gC)



#**********************************************************************

## experimental period


#Cfit_RothC <- coef(lm(RothC ~ year, data = Cstock_Mgha))
#Cfit_Obs <- coef(lm(Observed ~ year, data = Cstock_Mgha))

gC <- Cstock_Mgha_piv[Cstock_Mgha_piv$year %in% experiment_year_range,] %>%
ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
  geom_point(show.legend=TRUE) +
  xlab("Year") +
  ylab(expression('SOC stock (Mg C ha' ^-1*')')) +
  ggtitle(paste(site_name,"Soil Organic Carbon: Scenario ",clim_scenario_num,"_",mgmt_scenario_num)) +
#  geom_abline(intercept=Cfit_RothC[1], slope=Cfit_RothC[2], color="orange") +
#  geom_abline(intercept=Cfit_Obs[1], slope=Cfit_Obs[2], color="black") +
  scale_color_manual(labels=c("RothC","Observed"),
                     values=cbPalette9[c(1,8)]) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gC 


ggsave(filename=paste0(results_path,"calib_SOC_comparison_exp_",
                       clim_scenario_num,"_",mgmt_scenario_num,"_RothC.jpg"),
       plot=gC)

#**********************************************************************

# 1:1 graph

##  SOC
if(mgmt_scenario_grp==3) {
  Cfit <- lm(RothC ~ Observed, data = Cstock_Mgha[Cstock_Mgha$year!=1998 &
                                                    Cstock_Mgha$year!=1850,])
  Cfit_coef <- coef(Cfit)
  Cfit_r2 <- round(summary(Cfit)$r.squared,2)
  
  C_rmse_error <- Cstock_Mgha$Observed-Cstock_Mgha$RothC
  C_rmse <- round(sqrt(mean(C_rmse_error^2,na.rm=TRUE)),2)
} else {
  Cfit <- lm(RothC ~ Observed, data = Cstock_Mgha[Cstock_Mgha$year!=1850,])
  Cfit_coef <- coef(Cfit)
  Cfit_r2 <- round(summary(Cfit)$r.squared,2)
  
  C_rmse_error <- Cstock_Mgha$Observed-Cstock_Mgha$RothC
  C_rmse <- round(sqrt(mean(C_rmse_error^2,na.rm=TRUE)),2)
}

gC_121 <- Cstock_Mgha %>%
  ggplot(aes(x=Observed, y=RothC,
             xmin=min(Observed, RothC, na.rm=T), xmax=max(Observed, RothC, na.rm=T),
             ymin=min(Observed, RothC, na.rm=T), ymax=max(Observed, RothC, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=Cfit_coef[1], slope=Cfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(Cstock_Mgha$Observed, Cstock_Mgha$RothC, na.rm=T)*1.1,
           y=max(Cstock_Mgha$Observed, Cstock_Mgha$RothC, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(Cfit_coef[2],4))~"x" ~+ ~.(round(Cfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(Cstock_Mgha$Observed, Cstock_Mgha$RothC, na.rm=T)*1.1,
           y=max(Cstock_Mgha$Observed, Cstock_Mgha$RothC, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(Cfit_r2))) +
  annotate("text", # RMSE
           x=min(Cstock_Mgha$Observed, Cstock_Mgha$RothC, na.rm=T)*1.1,
           y=max(Cstock_Mgha$Observed, Cstock_Mgha$RothC, na.rm=T)*0.89,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(C_rmse))) +
  ggtitle("SOC stock") +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(),
        plot.title = element_text(hjust = 0.5))

gC_121

#**********************************************************************

# add this run's results to a log file
calib_log_tab <- cbind(as.character(Sys.time()),
                       model_name,clim_scenario_num,mgmt_scenario_num, scenario_name,
                       NA, NA, NA, NA,
                       NA, NA, NA, NA,
                       NA, NA, NA, NA,
                       Cfit_coef[2], Cfit_coef[1], Cfit_r2, C_rmse,
                       NA, NA, NA, NA,
                       NA, NA, NA, NA,
                       NA, NA, NA, NA,
                       NA, NA, NA, NA)
write.table(calib_log_tab,file=paste0(results_path,"Calibration_log_RothC.csv"),
            append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")

# make separate file with column headers (empty table with NA row)
dummy<-data.frame(matrix(ncol=37))
colnames(dummy) <- c("Date_time",
                     "Model","Climate_Scenario","Mgmt_Scenario","Scenario_Name",
                     "Maize_slope","Maize_yint","Maize_R2","Maize_RMSE",
                     "Soy_slope","Soy_yint","Soy_R2","Soy_RMSE",
                     "Wheat_slope","Wheat_yint","Wheat_R2","Wheat_RMSE",
                     "SOC_slope","SOC_yint","SOC_R2","SOC_RMSE",
                     "Temp_slope","Temp_yint","Temp_R2","Temp_RMSE",
                     "Moist_slope","Moist_yint","Moist_R2","Moist_RMSE",
                     "N2O_slope","N2O_yint","N2O_R2","N2O_RMSE",
                     "CH4_slope","CH4_yint","CH4_R2","CH4_RMSE")
write.table(dummy,file=paste0(results_path,"Calibration_log_columns.csv"),
            append=FALSE,col.names=TRUE,row.names=FALSE,sep=",")


# add/replace this run's results to a file collecting all final models/runs
calib_summary_tab <- cbind(as.character(Sys.time()),
                           model_name,clim_scenario_num,mgmt_scenario_num, scenario_name,
                           NA, NA, NA, NA,
                           NA, NA, NA, NA,
                           NA, NA, NA, NA,
                           Cfit_coef[2], Cfit_coef[1], Cfit_r2, C_rmse,
                           NA, NA, NA, NA,
                           NA, NA, NA, NA,
                           NA, NA, NA, NA,
                           NA, NA, NA, NA)
## call function to edit the summary output file
source("p_Edit_calib_file.R")
p_Edit_calib_file(calib_summary_tab,model_name,scenario_name)


}) # end suppressMessages
