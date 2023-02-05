#title: "9_Results_APSIM-future.R"
#author: "Ellen Maas"
#date: "7/22/2022"
#output: html_document
#description: "Runs all graphs for the APSIM simulation at KBS, MI."

suppressMessages({
  
  print("Starting 9_Results_APSIM-future.R")
  
library(apsimx)
library(readxl)
library(magrittr)
library(lubridate)
library(tidyverse)
library(graphics)
library(ggplot2)

# Temporal graphs

## experimental -> future period


gMY <- MaizeYld_Mgha_piv %>%
ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
  geom_point() +
  xlab("Year") +
  ylab(expression('Maize Yield (Mg ha ' ^-1*')')) +
  ggtitle(paste(site_name,"Maize Yield"),paste0("Scenario: ",scenario_descriptor_full)) +
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=cbPalette9[c(8,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gMY

gSY <- SoyYld_Mgha_piv %>%
ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
  geom_point() +
  xlab("Year") +
  ylab(expression('Soybean Yield (Mg ha ' ^-1*')')) +
  ggtitle(paste(site_name," Soybean Yield"),paste0("Scenario: ",scenario_descriptor_full)) +
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=cbPalette9[c(8,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gSY

gWY <- WheatYld_Mgha_piv %>%
ggplot(aes(x=year, y=yield_val, color=source, show.legend=TRUE)) +
  geom_point() +
  xlab("Year") +
  ylab(expression('Wheat Yield (Mg ha ' ^-1*')')) +
  ggtitle(paste(site_name," Wheat Yield"),paste0("Scenario: ",scenario_descriptor_full)) +
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=cbPalette9[c(8,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gWY

if(mgmt_scenario_grp==3) {
  Cfit_APSIM <- coef(lm(APSIM ~ year, data = Cstock_Mgha))
  Cfit_Obs <- coef(lm(Observed ~ year, data = Cstock_Mgha[Cstock_Mgha$year!=1998 &
                                                            Cstock_Mgha$year >= experiment_start_year,]))
} else {
  Cfit_APSIM <- coef(lm(APSIM ~ year, data = Cstock_Mgha))
  Cfit_Obs <- coef(lm(Observed ~ year, data = Cstock_Mgha[Cstock_Mgha$year >= experiment_start_year,]))
}

gC <- Cstock_Mgha_piv %>%
ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
  geom_point() +
  xlab("Year") +
  ylab(expression('SOC stock (Mg C ha' ^-1*')')) +
#  geom_abline(intercept=Cfit_APSIM[1], slope=Cfit_APSIM[2], color="orange") +
  geom_abline(intercept=Cfit_Obs[1], slope=Cfit_Obs[2], color="black") +
  ggtitle(paste(site_name,"Soil Organic Carbon"),paste0("Scenario: ",scenario_descriptor_full)) +
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=cbPalette9[c(8,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gC 

gT <- SoilTemp_C_piv[SoilTemp_C_piv$source=='APSIM' & SoilTemp_C_piv$year %in% ObsTemp$year,] %>%
ggplot(aes(x=date, y=temp_val, color=source, show.legend=TRUE)) +
  geom_point() +
  geom_point(data=SoilTemp_C_piv[SoilTemp_C_piv$source=='Observed' & SoilTemp_C_piv$year %in% ObsTemp$year,],
             aes(x=date, y=temp_val, color=source)) +
  xlab("Year") +
  ylab(expression('Soil temperature (' ^o*'C)')) +
  ggtitle(paste0(site_name," Soil Temperature"),paste0("Scenario: ",scenario_descriptor_full)) +
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=cbPalette9[c(8,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gT

gT_calib <- SoilTemp_C_piv_calib[SoilTemp_C_piv_calib$source=='APSIM' & SoilTemp_C_piv$year %in% ObsTemp$year,] %>%
ggplot(aes(x=date, y=temp_val, color=source, show.legend=TRUE)) +
  geom_point() +
  geom_point(data=SoilTemp_C_piv_calib[SoilTemp_C_piv_calib$source=='Observed' & SoilTemp_C_piv$year %in% ObsTemp$year,],
             aes(x=date, y=temp_val, color=source)) +
  ggtitle(paste0("APSIM and Observed Soil Temperature with ",soil_temp_bias,
              " deg C correction")) +
  xlab("Year") +
  ylab(expression('Soil temperature (' ^o*'C)')) +
  ggtitle(paste0(site_name," Soil Temperature with ",soil_temp_bias,
                 " deg C correction"),paste0("Scenario: ",scenario_descriptor_full)) +
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=cbPalette9[c(8,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gT_calib

gM <- SoilMoist_VSM_piv[SoilMoist_VSM_piv$source=='APSIM' & SoilMoist_VSM_piv$year %in% ObsVSM$year,] %>%
ggplot(aes(x=date, y=h2o_val, color=source, show.legend=TRUE)) +
  geom_point() +
  geom_point(data=SoilMoist_VSM_piv[SoilMoist_VSM_piv$source=='Observed' & SoilMoist_VSM_piv$year %in% ObsVSM$year,],
             aes(x=date, y=h2o_val, color=source)) +
  xlab("Year") +
  ylab("Volumetric soil moisture (%)") +
  ggtitle(paste0(site_name," Soil Moisture"),paste0("Scenario: ",scenario_descriptor_full)) +
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=cbPalette9[c(8,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gM

gM_calib <- SoilMoist_VSM_piv_calib[SoilMoist_VSM_piv_calib$source=='APSIM' & SoilMoist_VSM_piv$year %in% ObsVSM$year,] %>%
ggplot(aes(x=date, y=h2o_val, color=source, show.legend=TRUE)) +
  geom_point() +
  geom_point(data=SoilMoist_VSM_piv_calib[SoilMoist_VSM_piv_calib$source=='Observed' & SoilMoist_VSM_piv$year %in% ObsVSM$year,],
             aes(x=date, y=h2o_val, color=source)) +
  xlab("Year") +
  ylab("Volumetric soil moisture (%)") +
  ggtitle(paste0(site_name," Volumetric soil moisture with ",soil_moist_bias,"% correction"),
          paste0("Scenario: ",scenario_descriptor_full)) +
  scale_color_manual(labels=c("APSIM","Observed"),
                     values=cbPalette9[c(8,1)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gM_calib

gB <- SoilBD_gcc_piv %>%
  ggplot(aes(x=source,y=bd_val, fill=factor(year))) +
  geom_col(position="dodge") +
  ylab(expression('Bulk density (g cc' ^-1*')')) +
  labs(fill="Year") +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())

gB

gNG <- N2O_ghaday_piv[N2O_ghaday_piv$source=='APSIM',] %>%
  ggplot(aes(x=date, y=n2o_val, color=source)) +
  geom_line(show.legend=TRUE) +
  geom_point(data=N2O_ghaday_piv[N2O_ghaday_piv$source=='Observed',],
             aes(x=date, y=n2o_val, color=source)) +
  geom_segment(data=Fert[Fert$treatment==treatment & Fert$n_rate_kg_ha>10,],
               aes(x = date, y = 200,
                   xend = date, yend = 175),
                   colour=cbPalette9[7],
                   show.legend=F,
                   lineend = "round",
                   linejoin = "round",
                   arrow = arrow(length = unit(0.3, "cm"))
   # colour = "black" 
                   ) + 
  xlab("Year") +
  ylab(expression('N'[2]*'O Emissions (g ha ' ^-1*'day ' ^-1*')')) +
  ggtitle(bquote(.(site_name)~"N"["2"]*"O Emissions"),
          paste0("Scenario: ",scenario_descriptor_full)) +
  scale_color_manual(labels=c("APSIM","Observed","Fertilizer"),
                     values=cbPalette9[c(8,1,7)]) +
  theme_classic(base_family = "serif", base_size = 15) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gNG

 ggsave(filename=paste0(results_path,"Maize_yield_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),plot=gMY)
 ggsave(filename=paste0(results_path,"Soybean_yield_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),plot=gSY)
 ggsave(filename=paste0(results_path,"Wheat_yield_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),plot=gWY)
 ggsave(filename=paste0(results_path,"SOC_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),plot=gC)
 ggsave(filename=paste0(results_path,"Soil_Temp_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),plot=gT)
 ggsave(filename=paste0(results_path,"Soil_Temp_comparison_calib_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),plot=gT_calib)
 ggsave(filename=paste0(results_path,"Soil_Moist_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),plot=gM)
 ggsave(filename=paste0(results_path,"Soil_Moist_comparison_calib_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),plot=gM_calib)
 ggsave(filename=paste0(results_path,"N2O_comparison_fut_",clim_scenario_num,"_",mgmt_scenario_num,"_APSIM.jpg"),plot=gNG)

 
}) # end suppressMessages