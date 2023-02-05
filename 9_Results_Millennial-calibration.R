# ---
# title: 5_Results_Millennial-calibration.R
# author: Ellen Maas
# date: 8/30/2022
# output: html_document

suppressMessages({
  
print("Starting 5_Results_Millennial-calibration.R")

library(readxl)
#library(plotly)
library(magrittr)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)

# 9-color palette with grey and black. Colors in order are:
#[1]black, [2]dark blue, [3]green, [4]light blue, [5]grey,
#[6]pink, [7]red, [8]orange, [9]yellow                       


#*************************************************************

# Temporal graphs
## carbon

#### full 1 m depth, full time span
gC1 <- Cstock_Mgha_piv[Cstock_Mgha_piv$year <= experiment_end_year,] %>%
ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
  geom_point(show.legend=TRUE) +
  geom_point(data=ObsCdeep_Mgha, aes(x=year, y=cstock, color="black")) +
  xlab("Year") +
  ylab(expression('SOC stock (Mg C ha' ^-1*')')) +
  ggtitle(paste(site_name,"Soil Organic Carbon (1 m depth): Scenario ",scenario_name)) +
#  geom_abline(intercept=Cfit_RothC[1], slope=Cfit_RothC[2], color="orange") +
#  geom_abline(intercept=Cfit_Obs[1], slope=Cfit_Obs[2], color="black") +
  scale_color_manual(labels=c("Observed-100cm","Millennial","Observed-25cm"),
                     values=cbPalette9[c(6,8,1)]) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gC1 

#### 25 cm depth, full time span
Cfit_Obs <- coef(lm(Observed ~ year, data = Cstock_Mgha[Cstock_Mgha$year >= experiment_start_year,]))

gC2 <- Cstock_Mgha_piv_25cm[Cstock_Mgha_piv_25cm$year <= experiment_end_year,] %>%
ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
  geom_point(show.legend=TRUE) +
  geom_point(data=ObsCdeep_Mgha, aes(x=year, y=cstock, color="blue")) +
  geom_abline(intercept=Cfit_Obs[1], slope=Cfit_Obs[2], color="black") +
  xlab("Year") +
  ylab(expression('SOC stock (Mg C ha' ^-1*')')) +
  ggtitle(paste(site_name,"Soil Organic Carbon (25 cm depth): Scenario ",scenario_name)) +
#  geom_abline(intercept=Cfit_RothC[1], slope=Cfit_RothC[2], color="orange") +
#  geom_abline(intercept=Cfit_Obs[1], slope=Cfit_Obs[2], color="black") +
  scale_color_manual(labels=c("Observed-100cm","Millennial","Observed-25cm"),
                     values=cbPalette9[c(6,8,1)]) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gC2 


#### 25 cm depth, base through experimental
if(scenario_name=="1_3") {
  # remove 1998 observation from regression, looks like bad data
  Cfit_Obs <- coef(lm(Observed ~ year, data = Cstock_Mgha_25cm[Cstock_Mgha_25cm$year >= experiment_start_year &
                                                                     Cstock_Mgha_25cm$year != 1998,]))
} else {
  Cfit_Obs <- coef(lm(Observed ~ year, data = Cstock_Mgha_25cm[Cstock_Mgha_25cm$year >= experiment_start_year,]))
}


gC3 <- Cstock_Mgha_piv_25cm[Cstock_Mgha_piv_25cm$year <= experiment_end_year,] %>%
ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
  geom_point(show.legend=TRUE) +
  geom_abline(intercept=Cfit_Obs[1], slope=Cfit_Obs[2], color="black") +
  xlab("Year") +
  ylab(expression('SOC stock (Mg C ha ' ^-1*')')) +
  ggtitle(paste(site_name,"Soil Organic Carbon: Scenario ",scenario_name)) +
#  geom_abline(intercept=Cfit_RothC[1], slope=Cfit_RothC[2], color="orange") +
#  geom_abline(intercept=Cfit_Obs[1], slope=Cfit_Obs[2], color="black") +
  scale_color_manual(labels=c("Millennial","Observed"),
                     values=cbPalette9[c(8,1)]) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gC3


 ggsave(filename=paste0("calib_SOC_comparison_1m_exp_",scenario_name,"_Millennial.jpg"),plot=gC1)
 ggsave(filename=paste0("calib_SOC_comparison_25cm_exp_",scenario_name,"_Millennial.jpg"),plot=gC2)
 ggsave(filename=paste0("calib_SOC_comparison_exp_",scenario_name,"_Millennial.jpg"),plot=gC3)

 #*************************************************************
 

## microbial biomass

 
 # look at only the points when observations were made
gMB1 <- mbio_gm2[,c("date","Millennial")] %>%
ggplot(aes(x=date, y=Millennial, color=cbPalette9[6])) +
  geom_point() +
  geom_point(data=mbio_gm2[,c("date","Observed")],
             aes(x=date,y=Observed, color=cbPalette9[8])) +
  xlab("Year") +
  ylab(expression('Microbial biomass (g C m' ^-2*')')) +
  ggtitle(paste(site_name,"Microbial Biomass: Scenario ",scenario_name)) +
#  geom_abline(intercept=Cfit_RothC[1], slope=Cfit_RothC[2], color="orange") +
#  geom_abline(intercept=Cfit_Obs[1], slope=Cfit_Obs[2], color="black") +
  scale_color_manual(labels=c("Millennial","Observed"),
                     values=cbPalette9[c(6,1)]) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank(),
        legend.title = element_blank())

gMB1

# look at all Millennial data over whole model run
gMB2 <- mbio_gm2_all[,c("date","Millennial")] %>%
ggplot(aes(x=date, y=Millennial, color=cbPalette9[6])) +
  geom_line() +
  geom_point(data=mbio_gm2[,c("date","Observed")],
             aes(x=date,y=Observed, color=cbPalette9[8])) +
  xlab("Year") +
  ylab(expression('Microbial biomass (g C m' ^-2*')')) +
  ggtitle(paste(site_name,"Microbial Biomass: Scenario ",scenario_name)) +
#  geom_abline(intercept=Cfit_RothC[1], slope=Cfit_RothC[2], color="orange") +
#  geom_abline(intercept=Cfit_Obs[1], slope=Cfit_Obs[2], color="black") +
  scale_color_manual(labels=c("Millennial","Observed"),
                     values=cbPalette9[c(6,1)]) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank(),
        legend.title = element_blank())

gMB2

# look at all Millennial data within date range when observations were made
gMB3 <- mbio_gm2_all[mbio_gm2_all$date %in% min(mbio_gm2$date):max(mbio_gm2$date)
                     ,c("date","Millennial")] %>%
ggplot(aes(x=date, y=Millennial, color=cbPalette9[6])) +
  geom_line() +
  geom_point(data=mbio_gm2_all[mbio_gm2_all$date %in% min(mbio_gm2$date):max(mbio_gm2$date),
                           c("date","Observed")],
             aes(x=date,y=Observed, color=cbPalette9[8])) +
  xlab("Year") +
  ylab(expression('Microbial biomass (g C m' ^-2*')')) +
  ggtitle(paste(site_name,"Microbial Biomass: Scenario ",scenario_name)) +
#  geom_abline(intercept=Cfit_RothC[1], slope=Cfit_RothC[2], color="orange") +
#  geom_abline(intercept=Cfit_Obs[1], slope=Cfit_Obs[2], color="black") +
  scale_color_manual(labels=c("Millennial","Observed"),
                     values=cbPalette9[c(6,1)]) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank(),
        legend.title = element_blank())

gMB3


ggsave(filename=paste0(results_path,"calib_MBio_comparison_points_exp_",scenario_name,"_Millennial.jpg"),plot=gMB1)
ggsave(filename=paste0(results_path,"calib_MBio_comparison_allMill_exp_",scenario_name,"_Millennial.jpg"),plot=gMB2)
ggsave(filename=paste0(results_path,"calib_MBio_comparison_allexp_exp_",scenario_name,"_Millennial.jpg"),plot=gMB3)

#*************************************************************

# 1:1 

##  SOC
if(mgmt_scenario_grp==3) {
  Cfit <- lm(Millennial ~ Observed, data = Cstock_Mgha[Cstock_Mgha$year!=1998 &
                                                    Cstock_Mgha$year!=1850,])
  Cfit_coef <- coef(Cfit)
  Cfit_r2 <- round(summary(Cfit)$r.squared,2)
  
  C_rmse_error <- Cstock_Mgha$Observed-Cstock_Mgha$Millennial
  C_rmse <- round(sqrt(mean(C_rmse_error^2,na.rm=TRUE)),2)
} else {
  Cfit <- lm(Millennial ~ Observed, data = Cstock_Mgha[Cstock_Mgha$year!=1850,])
  Cfit_coef <- coef(Cfit)
  Cfit_r2 <- round(summary(Cfit)$r.squared,2)
  
  C_rmse_error <- Cstock_Mgha$Observed-Cstock_Mgha$Millennial
  C_rmse <- round(sqrt(mean(C_rmse_error^2,na.rm=TRUE)),2)
}

gC_121 <- Cstock_Mgha %>%
  ggplot(aes(x=Observed, y=Millennial,
             xmin=min(Observed, Millennial, na.rm=T), xmax=max(Observed, Millennial, na.rm=T),
             ymin=min(Observed, Millennial, na.rm=T), ymax=max(Observed, Millennial, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=Cfit_coef[1], slope=Cfit_coef[2], color="blue") +
  annotate("text", # line equation
           x=min(Cstock_Mgha$Observed, Cstock_Mgha$Millennial, na.rm=T)*1.1,
           y=max(Cstock_Mgha$Observed, Cstock_Mgha$Millennial, na.rm=T)*1,
           hjust=0, family="serif", color="gray31",
           label=bquote("y =" ~.(round(Cfit_coef[2],4))~"x" ~+ ~.(round(Cfit_coef[1],4)))) +
  annotate("text", # R^2
           x=min(Cstock_Mgha$Observed, Cstock_Mgha$Millennial, na.rm=T)*1.1,
           y=max(Cstock_Mgha$Observed, Cstock_Mgha$Millennial, na.rm=T)*0.95,
           hjust=0, family="serif", color="gray31",
           label=bquote(R^2 ~"=" ~.(Cfit_r2))) +
  annotate("text", # RMSE
           x=min(Cstock_Mgha$Observed, Cstock_Mgha$Millennial, na.rm=T)*1.1,
           y=max(Cstock_Mgha$Observed, Cstock_Mgha$Millennial, na.rm=T)*0.89,
           hjust=0, family="serif", color="gray31",
           label=bquote("RMSE =" ~.(C_rmse))) +
  ggtitle("SOC stock") +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(),
        plot.title = element_text(hjust = 0.5))

gC_121

# microbial biomass
MBfit <- coef(lm(Millennial ~ Observed, data = mbio_gm2))

gMB_121 <- mbio_gm2 %>%
  ggplot(aes(x=Observed, y=Millennial,
             xmin=min(Observed, Millennial, na.rm=T), xmax=max(Observed, Millennial, na.rm=T),
             ymin=min(Observed, Millennial, na.rm=T), ymax=max(Observed, Millennial, na.rm=T))) +
  geom_point() +
  geom_abline() +
  geom_abline(intercept=MBfit[1], slope=MBfit[2], color="blue") +
  ggtitle(paste0('Microbial biomass: Scenario ',scenario_name)) +
  scale_color_manual(labels=c("Millennial","Observed"),
                     values=cbPalette9[c(6,1)]) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right",
        legend.key = element_blank())

gMB_121

ggsave(filename=paste0(results_path,"calib_MBio_comparison_1to1_",
                       scenario_name,"_Millennial.jpg"),
       plot=gMB_121)

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
write.table(calib_log_tab,file=paste0(results_path,"Calibration_log_Millennial.csv")
            ,append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")

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