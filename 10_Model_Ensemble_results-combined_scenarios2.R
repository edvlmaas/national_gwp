#######################################
# File: 10_Model_Ensemble_Results-combined_scenarios2
# Author: Ellen Maas
# Date: 8/30/2022
# Description: Assembles individual model runs and calculates
# annual and daily mean values, then graphs them. Also 
# calculates the global warming potential by scenario and 
# graphs it.
#######################################
# Calls:
# f_model_coef
#######################################
# Audit Log
# 8/30/2022: Created script.
# 12/22/2022: Reworked linear models to use f_model_coef, 
# loop for climate scenarios.
#######################################

suppressMessages({
  
print("Starting 10_Model_Ensemble_results-combined_scenarios2.R")

library(readxl)
library(magrittr)
library(lubridate)
library(tidyverse)
library(graphics)
library(ggplot2)

source("f_model_coef.R")

#*************************************************************
# Import summarized data

# global warming potential
## calculated by mean of SOC, N2O, CH4 among all models by 2100,
## then CO2 equivalent in 100-yr timeframe.
## then calculate emissions - sequestration: N2O + CH4 - SOC
## CH4 is usually negative, so will offset emissions

## read in model summary output
summary_output_raw <- read.csv(paste0(results_path,"Summary_output.csv")) 

## get means for N2O and CH4 to calculate separately for change-over-time
scenario_gas_means <- summary_output_raw %>%
  group_by(Climate_Scenario,Mgmt_Scenario,Scenario_Name) %>%
  summarize(mean_N2O_Mgha=round(mean(Total_N2O_kgha/1000,na.rm=T),4),
            mean_CH4_Mgha=round(mean(Total_CH4_kgha/1000,na.rm=T),4),
            sd_N2O_Mgha=round(sd(Total_N2O_kgha/1000,na.rm=T),4),
            sd_CH4_Mgha=round(sd(Total_CH4_kgha/1000,na.rm=T),4)
  ) %>%
  left_join(scenario_df[,c("scenario_name","scenario_abbrev")],by=c("Scenario_Name"="scenario_name"))


## get means for N2O and CH4 to fill in for each model which doesn't include it,
## in order to calculate GWP:
## N2O - RothC and Millennial
## CH4 - APSIM, RothC, and Millennial

ghg_means <- summary_output_raw %>%
  group_by(Climate_Scenario,Mgmt_Scenario,Scenario_Name) %>%
  summarize(model_mean_N2O_kgha=mean(Total_N2O_kgha,na.rm=T),
            model_mean_CH4_kgha=mean(Total_CH4_kgha,na.rm=T)
  )

## now fill in missing data
summary_fillin <- left_join(summary_output_raw,
                            ghg_means,
                            by=c("Climate_Scenario","Mgmt_Scenario","Scenario_Name")) 
summary_fillin[is.na(summary_fillin$Total_N2O_kgha),"Total_N2O_kgha"] <-
  summary_fillin[is.na(summary_fillin$Total_N2O_kgha),"model_mean_N2O_kgha"]
summary_fillin[is.na(summary_fillin$Total_CH4_kgha),"Total_CH4_kgha"] <-
  summary_fillin[is.na(summary_fillin$Total_CH4_kgha),"model_mean_CH4_kgha"]

summary_output <- summary_fillin[,-which(names(summary_fillin) %in% 
                                           c("model_mean_N2O_kgha","model_mean_CH4_kgha"))] %>%
  mutate(N2O_Mgha=Total_N2O_kgha/1000,
         CH4_Mgha=Total_CH4_kgha/1000,
         CO2e_SOC=-SOC_Diff_Mgha/0.2727, # convert SOC to CO2e
         CO2e_N2O=N2O_Mgha*298, # convert N2O to CO2e
         CO2e_CH4=CH4_Mgha*28 # convert CH4 to CO2e
  )
summary_output$GWP=rowSums(summary_output[grep("^CO2e", names(summary_output))], na.rm=TRUE) # SOC negative for sequestration

scenario_means <- summary_output %>%
  group_by(Climate_Scenario,Mgmt_Scenario,Scenario_Name) %>%
  summarize(mean_MaizeYld_Mgha=round(mean(Maize_Diff_Mgha,na.rm=T),1),
            mean_SoyYld_Mgha=round(mean(Soybean_Diff_Mgha,na.rm=T),1),
            mean_WheatYld_Mgha=round(mean(Wheat_Diff_Mgha,na.rm=T),1),
            mean_SOC_Mgha=round(mean(SOC_Diff_Mgha,na.rm=T),2),
            mean_N2O_Mgha=round(mean(N2O_Mgha,na.rm=T),2),
            mean_CH4_Mgha=round(mean(CH4_Mgha,na.rm=T),2),
            mean_CO2e_SOC=round(mean(CO2e_SOC,na.rm=T),2),
            mean_CO2e_N2O=round(mean(CO2e_N2O,na.rm=T),2),
            mean_CO2e_CH4=round(mean(CO2e_CH4,na.rm=T),2),
            mean_GWP=round(mean(GWP,na.rm=T),1),
            sd_MaizeYld_Mgha=round(sd(Maize_Diff_Mgha,na.rm=T),1),
            sd_SoyYld_Mgha=round(sd(Soybean_Diff_Mgha,na.rm=T),1),
            sd_WheatYld_Mgha=round(sd(Wheat_Diff_Mgha,na.rm=T),1),
            sd_SOC_Mgha=round(sd(SOC_Diff_Mgha,na.rm=T),2),
            sd_N2O_Mgha=round(sd(N2O_Mgha,na.rm=T),2),
            sd_CH4_Mgha=round(sd(CH4_Mgha,na.rm=T),2),
            sd_CO2e_SOC=round(sd(CO2e_SOC),2),
            sd_CO2e_N2O=round(sd(CO2e_N2O),2),
            sd_CO2e_CH4=round(sd(CO2e_CH4),2),
            sd_GWP=round(sd(GWP,na.rm=T),1)
  ) %>%
  left_join(scenario_df[,c("scenario_name","scenario_abbrev")],by=c("Scenario_Name"="scenario_name"))



# write out scenario means
write.csv(scenario_means, file=paste0(results_path,"scenario_means.csv"))

gwp_means_piv <- pivot_longer(scenario_means,c(-Climate_Scenario,
                                               -Mgmt_Scenario,
                                               -Scenario_Name,
                                               -scenario_abbrev,
),
names_to="source",
values_to="vals")

annual_results <- data.frame()
# loop through csvs to get the scenarios with options (like groups 4 and 5)
for (i in clim_nums) { # climate scenarios
  for (j in mgmt_grps) { # management groups
    max_options <- if_else(j==4,4,
                   if_else(j==5,3,
                   if_else(j==6,5,1)))
    for (k in 1:max_options) { # management options
      if(j!=6) {
        for (l in c("APSIM","Daycent","Millennial","RothC")) {
          scen_nm <- paste0(i,"_",j,if(max_options==1) "" else k)
          read_dat <- read.csv(paste0(results_path,"Annual_results_compilation_",scen_nm,
                                      "_",l,".csv"))
          annual_results <- rbind(annual_results,read_dat)
        } # end for - models
      } else {
        scen_nm <- paste0(i,"_",j,if(max_options==1) "" else k)
        read_dat <- read.csv(paste0(results_path,"Annual_results_compilation_",scen_nm,
                                    "_APSIM.csv"))
        annual_results <- rbind(annual_results,read_dat)
      } # end if - management group != 6
    } # end for - management group options
  } # end for - management groups
} # end for-climate scenarios

# fill-in NA yields with 0
# annual_results[is.na(annual_results$MaizeYld_Mgha),"MaizeYld_Mgha"] <- 0
# annual_results[is.na(annual_results$SoyYld_Mgha),"SoyYld_Mgha"] <- 0
# annual_results[is.na(annual_results$WheatYld_Mgha),"WheatYld_Mgha"] <- 0

# add scenario abbreviation
annual_results <- left_join(annual_results, 
                            scenario_df[,c("scenario_name","scenario_abbrev")],
                            by="scenario_name")

# mean annual results, by scenario
mean_annual_results <- annual_results[annual_results<2100,] %>%
  group_by(year,scenario_name,climate_scenario_num,mgmt_scenario_grp_num,
           mgmt_scenario_opt_num,scenario_abbrev) %>%
  summarize(MaizeYld_Mgha=round(mean(MaizeYld_Mgha,na.rm=T),3),
            SoyYld_Mgha=round(mean(SoyYld_Mgha,na.rm=T),3),
            WheatYld_Mgha=round(mean(WheatYld_Mgha,na.rm=T),3),
            SOC_Mgha=round(mean(SOC_Mgha,na.rm=T),3)
  )
mean_annual_results <- mean_annual_results[!is.na(mean_annual_results$year),]

write.csv(mean_annual_results, file=paste0(results_path,"mean_annual_results.csv"))


daily_results <- data.frame()
# loop through csvs to get the scenarios with options (like groups 4 and 5)
for (i in clim_nums) { # climate scenarios
  for (j in mgmt_grps) { # management groups
    max_options <- if_else(j==4,4,
                           if_else(j==5,3,
                                   if_else(j==6,5,1)))
    for (k in 1:max_options) { # management options
      if(j!=6) {
        for (l in c("APSIM","Daycent")) {
          scen_nm <- paste0(i,"_",j,if(max_options==1) "" else k)
          read_dat <- read.csv(paste0(results_path,"Daily_results_compilation_",scen_nm,
                                      "_",l,".csv"))
          daily_results <- rbind(daily_results,read_dat)
        } # end for - models
      } else {
        scen_nm <- paste0(i,"_",j,if(max_options==1) "" else k)
        read_dat <- read.csv(paste0(results_path,"Daily_results_compilation_",scen_nm,
                                    "_APSIM.csv"))
        daily_results <- rbind(daily_results,read_dat)
      } # end if - management group != 6
    } # end for - mgmt options
  } # end for - management groups
} # end for - climate scenarios

# add N2O and CH4 conversions to kgha
daily_results$N2O_cum_kgha <- daily_results$N2O_cum_gha/1000
daily_results$CH4_cum_kgha <- daily_results$CH4_cum_gha/1000

# add scenario abbreviation
daily_results <- left_join(daily_results, 
                           scenario_df[,c("scenario_name","scenario_abbrev")],
                           by="scenario_name")

# mean daily results, by scenario
mean_daily_results <- daily_results[daily_results$year<2100,] %>%
  group_by(year,date,dayofyear,scenario_name,climate_scenario_num,
           mgmt_scenario_grp_num,mgmt_scenario_opt_num,scenario_abbrev) %>%
  summarize(N2O_emit_gha=round(mean(N2O_emit_gha,na.rm=T),5),
            N2O_cum_gha=round(mean(N2O_cum_gha,na.rm=T),5),
            N2O_cum_kgha=N2O_cum_gha/1000,
            CH4_net_gha=round(mean(CH4_net_gha,na.rm=T),5),
            CH4_cum_gha=round(mean(CH4_cum_gha,na.rm=T),5),
            CH4_cum_kgha=CH4_cum_gha/1000
  )


write.csv(mean_daily_results, file=paste0(results_path,"mean_daily_results.csv"))

#*************************************************************

# Combine scenarios with multiple options - one set of graphs
# per climate scenario

for(clim_num in 1:5) {
  climate_desc <-   if_else(clim_num=="1","Baseline",
                    if_else(clim_num=="2","GFDL_ESM4 Low",
                    if_else(clim_num=="3","GFDL_ESM4 High",
                    if_else(clim_num=="4","UKESM1-0-LL Low",
                    if_else(clim_num=="5","UKESM1-0-LL High",
                    "Missing Descriptor")))))
  

  ## Annual graphs-combined scenarios

  ### Mean of all scenarios

  #### Maize

  gAllMYexp <- mean_annual_results[mean_annual_results$year>=experiment_start_year &
                                     mean_annual_results$MaizeYld_Mgha != 0 &
                                     mean_annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=MaizeYld_Mgha, color=scenario_abbrev,
               shape=as.factor(mgmt_scenario_grp_num))) +
    geom_point() +
    xlab("Year") +
    ylab(expression('Maize Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Maize Yield: All Scenarios, Model Means"),
            paste("Climate Scenario:",climate_desc)) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gAllMYexp


  # get the slope and intercept for all mgmt scenarios for one climate scenario
  model_mean_coefs <- f_model_coef(df_in=mean_annual_results,
                                   modeled_element_in="MaizeYld_Mgha",
                                   model_name_in="All",
                                   climate_scen_in=clim_num,
                                   mgmt_group_in=NA,
                                   result_name_in="MYfit")

  # # use this to get the colors used:
  g <- ggplot_build(gAllMYexp)
  # colors will repeat, so take the first x number of values, equal to x
  # number of scenarios

  gAllMYfut <- mean_annual_results[mean_annual_results$year>=experiment_end_year &
                                     mean_annual_results$MaizeYld_Mgha != 0 &
                                     mean_annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=MaizeYld_Mgha, color=scenario_abbrev,
               shape=as.factor(mgmt_scenario_grp_num))) +
    geom_point() +
    xlab("Year") +
    ylab(expression('Maize Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future Maize Yield: All Scenarios, Model Means"),
            paste("Climate Scenario:",climate_desc)) +
    geom_abline(intercept=model_mean_coefs[1,3], slope=model_mean_coefs[1,4],
                color=g$data[[1]]$colour[1]) +
    geom_abline(intercept=model_mean_coefs[2,3], slope=model_mean_coefs[2,4],
                color=g$data[[1]]$colour[2]) +
    geom_abline(intercept=model_mean_coefs[3,3], slope=model_mean_coefs[3,4],
                color=g$data[[1]]$colour[3]) +
    geom_abline(intercept=model_mean_coefs[4,3], slope=model_mean_coefs[4,4],
                color=g$data[[1]]$colour[4]) +
    geom_abline(intercept=model_mean_coefs[5,3], slope=model_mean_coefs[5,4],
                color=g$data[[1]]$colour[5]) +
    geom_abline(intercept=model_mean_coefs[6,3], slope=model_mean_coefs[6,4],
                color=g$data[[1]]$colour[6]) +
    geom_abline(intercept=model_mean_coefs[7,3], slope=model_mean_coefs[7,4],
                color=g$data[[1]]$colour[7]) +
    geom_abline(intercept=model_mean_coefs[8,3], slope=model_mean_coefs[8,4],
                color=g$data[[1]]$colour[8]) +
    geom_abline(intercept=model_mean_coefs[9,3], slope=model_mean_coefs[9,4],
                color=g$data[[1]]$colour[9]) +
    geom_abline(intercept=model_mean_coefs[10,3], slope=model_mean_coefs[10,4],
                color=g$data[[1]]$colour[10]) +
    geom_abline(intercept=model_mean_coefs[11,3], slope=model_mean_coefs[11,4],
                color=g$data[[1]]$colour[11]) +
    geom_abline(intercept=model_mean_coefs[12,3], slope=model_mean_coefs[12,4],
                color=g$data[[1]]$colour[12]) +
    geom_abline(intercept=model_mean_coefs[13,3], slope=model_mean_coefs[13,4],
                color=g$data[[1]]$colour[13]) +
    geom_abline(intercept=model_mean_coefs[14,3], slope=model_mean_coefs[14,4],
                color=g$data[[1]]$colour[14]) +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())


  gAllMYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_mean_Maize_exp_",clim_num,".jpg"),
         plot=gAllMYexp)
  ggsave(filename=paste0(results_path,"scenario_comparison_mean_Maize_fut_",clim_num,".jpg"),
         plot=gAllMYfut)


  #### Soybean

  gAllSYexp <- mean_annual_results[mean_annual_results$year>=experiment_start_year &
                                     mean_annual_results$SoyYld_Mgha != 0 &
                                     mean_annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SoyYld_Mgha, color=scenario_abbrev,
               shape=as.factor(mgmt_scenario_grp_num))) +
    geom_point() +
    xlab("Year") +
    ylab(expression('Soybean Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Soybean Yield: All Scenarios, Model Means"),
            paste("Climate Scenario:",climate_desc)) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gAllSYexp


  # get the slope and intercept for all mgmt scenarios for one climate scenario
  model_mean_coefs <- f_model_coef(df_in=mean_annual_results,
                                   modeled_element_in="SoyYld_Mgha",
                                   model_name_in="All",
                                   climate_scen_in=clim_num,
                                   mgmt_group_in=NA,
                                   result_name_in="SYfit")

  # # use this to get the colors used:
  g <- ggplot_build(gAllSYexp)
  # colors will repeat, so take the first x number of values, equal to x
  # number of scenarios

    gAllSYfut <- mean_annual_results[mean_annual_results$year>=experiment_end_year &
                                     mean_annual_results$SoyYld_Mgha != 0 &
                                     mean_annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SoyYld_Mgha, color=scenario_abbrev,
               shape=as.factor(mgmt_scenario_grp_num))) +
    geom_point() +
    xlab("Year") +
    ylab(expression('Soybean Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future Soybean Yield: All Scenarios, Model Means"),
            paste("Climate Scenario:",climate_desc)) +
      geom_abline(intercept=model_mean_coefs[1,3], slope=model_mean_coefs[1,4],
                  color=g$data[[1]]$colour[1]) +
      geom_abline(intercept=model_mean_coefs[2,3], slope=model_mean_coefs[2,4],
                  color=g$data[[1]]$colour[2]) +
      geom_abline(intercept=model_mean_coefs[3,3], slope=model_mean_coefs[3,4],
                  color=g$data[[1]]$colour[3]) +
      geom_abline(intercept=model_mean_coefs[4,3], slope=model_mean_coefs[4,4],
                  color=g$data[[1]]$colour[4]) +
      geom_abline(intercept=model_mean_coefs[5,3], slope=model_mean_coefs[5,4],
                  color=g$data[[1]]$colour[5]) +
      geom_abline(intercept=model_mean_coefs[6,3], slope=model_mean_coefs[6,4],
                  color=g$data[[1]]$colour[6]) +
      geom_abline(intercept=model_mean_coefs[7,3], slope=model_mean_coefs[7,4],
                  color=g$data[[1]]$colour[7]) +
      geom_abline(intercept=model_mean_coefs[8,3], slope=model_mean_coefs[8,4],
                  color=g$data[[1]]$colour[8]) +
      geom_abline(intercept=model_mean_coefs[9,3], slope=model_mean_coefs[9,4],
                  color=g$data[[1]]$colour[9]) +
      geom_abline(intercept=model_mean_coefs[10,3], slope=model_mean_coefs[10,4],
                  color=g$data[[1]]$colour[10]) +
      geom_abline(intercept=model_mean_coefs[11,3], slope=model_mean_coefs[11,4],
                  color=g$data[[1]]$colour[11]) +
      geom_abline(intercept=model_mean_coefs[12,3], slope=model_mean_coefs[12,4],
                  color=g$data[[1]]$colour[12]) +
      geom_abline(intercept=model_mean_coefs[13,3], slope=model_mean_coefs[13,4],
                  color=g$data[[1]]$colour[13]) +
      geom_abline(intercept=model_mean_coefs[14,3], slope=model_mean_coefs[14,4],
                  color=g$data[[1]]$colour[14]) +
      # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gAllSYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_mean_Soy_exp_",clim_num,".jpg"),plot=gAllSYexp)
  ggsave(filename=paste0(results_path,"scenario_comparison_mean_Soy_fut_",clim_num,".jpg"),plot=gAllSYfut)


  ####Wheat

  gAllWYexp <- mean_annual_results[mean_annual_results$year>=experiment_start_year &
                                     mean_annual_results$WheatYld_Mgha != 0 &
                                     mean_annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=WheatYld_Mgha, color=scenario_abbrev,
               shape=as.factor(mgmt_scenario_grp_num))) +
    geom_point() +
    xlab("Year") +
    ylab(expression('Wheat Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Wheat Yield: All Scenarios, Model Means"),
            paste("Climate Scenario:",climate_desc)) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gAllWYexp


  # get the slope and intercept for all mgmt scenarios for one climate scenario
  model_mean_coefs <- f_model_coef(df_in=mean_annual_results,
                                   modeled_element_in="WheatYld_Mgha",
                                   model_name_in="All",
                                   climate_scen_in=clim_num,
                                   mgmt_group_in=NA,
                                   result_name_in="WYfit")

  # # use this to get the colors used:
  g <- ggplot_build(gAllWYexp)
  # colors will repeat, so take the first x number of values, equal to x
  # number of scenarios

    gAllWYfut <- mean_annual_results[mean_annual_results$year>=experiment_end_year &
                                     mean_annual_results$WheatYld_Mgha != 0 &
                                     mean_annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=WheatYld_Mgha, color=scenario_abbrev,
               shape=as.factor(mgmt_scenario_grp_num))) +
    geom_point() +
    xlab("Year") +
    ylab(expression('Wheat Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future Wheat Yield: All Scenarios, Model Means"),
            paste("Climate Scenario:",climate_desc)) +
      geom_abline(intercept=model_mean_coefs[1,3], slope=model_mean_coefs[1,4],
                  color=g$data[[1]]$colour[1]) +
      geom_abline(intercept=model_mean_coefs[2,3], slope=model_mean_coefs[2,4],
                  color=g$data[[1]]$colour[2]) +
      geom_abline(intercept=model_mean_coefs[3,3], slope=model_mean_coefs[3,4],
                  color=g$data[[1]]$colour[3]) +
      geom_abline(intercept=model_mean_coefs[4,3], slope=model_mean_coefs[4,4],
                  color=g$data[[1]]$colour[4]) +
      geom_abline(intercept=model_mean_coefs[5,3], slope=model_mean_coefs[5,4],
                  color=g$data[[1]]$colour[5]) +
      geom_abline(intercept=model_mean_coefs[6,3], slope=model_mean_coefs[6,4],
                  color=g$data[[1]]$colour[6]) +
      geom_abline(intercept=model_mean_coefs[7,3], slope=model_mean_coefs[7,4],
                  color=g$data[[1]]$colour[7]) +
      geom_abline(intercept=model_mean_coefs[8,3], slope=model_mean_coefs[8,4],
                  color=g$data[[1]]$colour[8]) +
      geom_abline(intercept=model_mean_coefs[9,3], slope=model_mean_coefs[9,4],
                  color=g$data[[1]]$colour[9]) +
      geom_abline(intercept=model_mean_coefs[10,3], slope=model_mean_coefs[10,4],
                  color=g$data[[1]]$colour[10]) +
      geom_abline(intercept=model_mean_coefs[11,3], slope=model_mean_coefs[11,4],
                  color=g$data[[1]]$colour[11]) +
      geom_abline(intercept=model_mean_coefs[12,3], slope=model_mean_coefs[12,4],
                  color=g$data[[1]]$colour[12]) +
      geom_abline(intercept=model_mean_coefs[13,3], slope=model_mean_coefs[13,4],
                  color=g$data[[1]]$colour[13]) +
      geom_abline(intercept=model_mean_coefs[14,3], slope=model_mean_coefs[14,4],
                  color=g$data[[1]]$colour[14]) +
      # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gAllWYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_mean_Wheat_exp_",clim_num,".jpg"),plot=gAllWYexp)
  ggsave(filename=paste0(results_path,"scenario_comparison_mean_Wheat_fut_",clim_num,".jpg"),plot=gAllWYfut)


  ## SOC

  gAllCexp <- mean_annual_results[mean_annual_results$year>=experiment_start_year &
                                    mean_annual_results$SOC_Mgha != 0 &
                                    mean_annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SOC_Mgha, color=scenario_abbrev,
               shape=as.factor(mgmt_scenario_grp_num))) +
    geom_line(linewidth=1) +
    xlab("Year") +
    ylab(expression('Soil Organic C (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future SOC: All Scenarios, Model Means"),
            paste("Climate Scenario:",climate_desc)) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gAllCexp


  # get the slope and intercept for all mgmt scenarios for one climate scenario
  model_mean_coefs <- f_model_coef(df_in=mean_annual_results,
                                   modeled_element_in="SOC_Mgha",
                                   model_name_in="All",
                                   climate_scen_in=clim_num,
                                   mgmt_group_in=NA,
                                   result_name_in="Cfit")

  # # use this to get the colors used:
  g <- ggplot_build(gAllCexp)
  # colors will repeat, so take the first x number of values, equal to x
  # number of scenarios

    gAllCfut <- mean_annual_results[mean_annual_results$year>=experiment_end_year &
                                    mean_annual_results$SOC_Mgha != 0 &
                                    mean_annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SOC_Mgha, color=scenario_abbrev,
               shape=as.factor(mgmt_scenario_grp_num))) +
    geom_line(linewidth=1) +
    xlab("Year") +
    ylab(expression('Soil Organic C (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future SOC: All Scenarios, Model Means"),
            paste("Climate Scenario:",climate_desc)) +
      # geom_abline(intercept=model_mean_coefs[1,3], slope=model_mean_coefs[1,4],
      #             color=g$data[[1]]$colour[1]) +
      # geom_abline(intercept=model_mean_coefs[2,3], slope=model_mean_coefs[2,4],
      #             color=g$data[[1]]$colour[2]) +
      # geom_abline(intercept=model_mean_coefs[3,3], slope=model_mean_coefs[3,4],
      #             color=g$data[[1]]$colour[3]) +
      # geom_abline(intercept=model_mean_coefs[4,3], slope=model_mean_coefs[4,4],
      #             color=g$data[[1]]$colour[4]) +
      # geom_abline(intercept=model_mean_coefs[5,3], slope=model_mean_coefs[5,4],
      #             color=g$data[[1]]$colour[5]) +
      # geom_abline(intercept=model_mean_coefs[6,3], slope=model_mean_coefs[6,4],
      #             color=g$data[[1]]$colour[6]) +
      # geom_abline(intercept=model_mean_coefs[7,3], slope=model_mean_coefs[7,4],
      #             color=g$data[[1]]$colour[7]) +
      # geom_abline(intercept=model_mean_coefs[8,3], slope=model_mean_coefs[8,4],
      #             color=g$data[[1]]$colour[8]) +
      # geom_abline(intercept=model_mean_coefs[9,3], slope=model_mean_coefs[9,4],
      #             color=g$data[[1]]$colour[9]) +
      # geom_abline(intercept=model_mean_coefs[10,3], slope=model_mean_coefs[10,4],
      #             color=g$data[[1]]$colour[10]) +
      # geom_abline(intercept=model_mean_coefs[11,3], slope=model_mean_coefs[11,4],
      #             color=g$data[[1]]$colour[11]) +
      # geom_abline(intercept=model_mean_coefs[12,3], slope=model_mean_coefs[12,4],
      #             color=g$data[[1]]$colour[12]) +
      # geom_abline(intercept=model_mean_coefs[13,3], slope=model_mean_coefs[13,4],
      #             color=g$data[[1]]$colour[13]) +
      # geom_abline(intercept=model_mean_coefs[14,3], slope=model_mean_coefs[14,4],
      #             color=g$data[[1]]$colour[14]) +
      # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  # # use this to get the colors used:
  # g <- ggplot_build(gAllfut)
  # g$data[[1]]$colour
  # [1] "#F8766D" "#D39200" "#93AA00" "#00BA38" "#00C19F" "#00B9E3" "#FF61C3" "#DB72FB" "#619CFF"

  gAllCfut

  ggsave(filename=paste0(results_path,"scenario_comparison_mean_SOC_exp_",clim_num,".jpg"),plot=gAllCexp)
  ggsave(filename=paste0(results_path,"scenario_comparison_mean_SOC_fut_",clim_num,".jpg"),plot=gAllCfut)


  #*************************************************************

  ### Scen grp 4

  #### Maize yield

  gMYexp <- annual_results[annual_results$year>=experiment_start_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==4 &
                             annual_results$MaizeYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=MaizeYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Maize Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Maize Yield: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gMYexp

  # get the slope and intercept for all mgmt scenarios for one climate scenario
  APSIM_coefs <- f_model_coef(df_in=annual_results,
                                   modeled_element_in="MaizeYld_Mgha",
                                   model_name_in="APSIM",
                                   climate_scen_in=clim_num,
                                   mgmt_group_in=4,
                                   result_name_in="MYfit")
  Daycent_coefs <- f_model_coef(df_in=annual_results,
                              modeled_element_in="MaizeYld_Mgha",
                              model_name_in="Daycent",
                              climate_scen_in=clim_num,
                              mgmt_group_in=4,
                              result_name_in="MYfit")

    gMYfut <- annual_results[annual_results$year>=experiment_end_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==4 &
                             annual_results$MaizeYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=MaizeYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Maize Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Maize Yield: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
      geom_abline(intercept=APSIM_coefs[1,3], slope=APSIM_coefs[1,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[1,3], slope=Daycent_coefs[1,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[2,3], slope=APSIM_coefs[2,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[2,3], slope=Daycent_coefs[2,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[3,3], slope=APSIM_coefs[3,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[3,3], slope=Daycent_coefs[3,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[4,3], slope=APSIM_coefs[4,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[4,3], slope=Daycent_coefs[4,4],
                  color=cbPalette9[2]) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gMYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_Maize_exp_grp_4_",clim_num,".jpg"),plot=gMYexp)
  ggsave(filename=paste0(results_path,"scenario_comparison_Maize_fut_grp_4_",clim_num,".jpg"),plot=gMYfut)



  #### Soybean yield

  gSYexp <- annual_results[annual_results$year>=experiment_start_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==4 &
                             annual_results$SoyYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SoyYld_Mgha, color=model_name,  shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Soybean Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Soybean Yield: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gSYexp


  # get the slope and intercept for all mgmt scenarios for one climate scenario
  APSIM_coefs <- f_model_coef(df_in=annual_results,
                              modeled_element_in="SoyYld_Mgha",
                              model_name_in="APSIM",
                              climate_scen_in=clim_num,
                              mgmt_group_in=4,
                              result_name_in="SYfit")
  Daycent_coefs <- f_model_coef(df_in=annual_results,
                                modeled_element_in="SoyYld_Mgha",
                                model_name_in="Daycent",
                                climate_scen_in=clim_num,
                                mgmt_group_in=4,
                                result_name_in="SYfit")

    gSYfut <- annual_results[annual_results$year>=experiment_end_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==4 &
                             annual_results$SoyYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SoyYld_Mgha, color=model_name,  shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Soybean Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Soybean Yield: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
      geom_abline(intercept=APSIM_coefs[1,3], slope=APSIM_coefs[1,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[1,3], slope=Daycent_coefs[1,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[2,3], slope=APSIM_coefs[2,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[2,3], slope=Daycent_coefs[2,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[3,3], slope=APSIM_coefs[3,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[3,3], slope=Daycent_coefs[3,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[4,3], slope=APSIM_coefs[4,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[4,3], slope=Daycent_coefs[4,4],
                  color=cbPalette9[2]) +
      scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gSYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_Soybean_exp_grp_4_",clim_num,".jpg"),plot=gSYexp)
  ggsave(filename=paste0(results_path,"scenario_comparison_Soybean_fut_grp_4_",clim_num,".jpg"),plot=gSYfut)


  #### Wheat yield

  gWYexp <- annual_results[annual_results$year>=experiment_start_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==4 &
                             annual_results$WheatYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=WheatYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Wheat Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Wheat Yield: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gWYexp

  # get the slope and intercept for all mgmt scenarios for one climate scenario
  APSIM_coefs <- f_model_coef(df_in=annual_results,
                              modeled_element_in="WheatYld_Mgha",
                              model_name_in="APSIM",
                              climate_scen_in=clim_num,
                              mgmt_group_in=4,
                              result_name_in="WYfit")
  Daycent_coefs <- f_model_coef(df_in=annual_results,
                                modeled_element_in="WheatYld_Mgha",
                                model_name_in="Daycent",
                                climate_scen_in=clim_num,
                                mgmt_group_in=4,
                                result_name_in="WYfit")

    gWYfut <- annual_results[annual_results$year>=experiment_end_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==4 &
                             annual_results$WheatYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=WheatYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Wheat Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Wheat Yield: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
      geom_abline(intercept=APSIM_coefs[1,3], slope=APSIM_coefs[1,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[1,3], slope=Daycent_coefs[1,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[2,3], slope=APSIM_coefs[2,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[2,3], slope=Daycent_coefs[2,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[3,3], slope=APSIM_coefs[3,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[3,3], slope=Daycent_coefs[3,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[4,3], slope=APSIM_coefs[4,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[4,3], slope=Daycent_coefs[4,4],
                  color=cbPalette9[2]) +
      scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gWYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_Wheat_exp_grp_4_",clim_num,".jpg"),plot=gWYexp)
  ggsave(filename=paste0(results_path,"scenario_comparison_Wheat_fut_grp_4_",clim_num,".jpg"),plot=gWYfut)


  #### SOC

  newC <- ObsC_Mgha[ObsC_Mgha$year > land_conversion_year,] %>%
    mutate(site="Observed")

  gCexp <- ggplot() +
    geom_point(data=annual_results[annual_results$year>=experiment_start_year &
                                     annual_results$model_name %in% c("APSIM","Daycent","RothC","Millennial") &
                                     annual_results$mgmt_scenario_grp_num==4 &
                                     annual_results$climate_scenario_num==clim_num,],
               aes(x=year, y=SOC_Mgha, color=model_name, shape=scenario_abbrev)) +
    geom_point(data=newC,
               aes(x=year, y=cstock,
                   color=site, shape=site)) +
    xlab("Year") +
    ylab(expression('Soil Organic C (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future SOC: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent","Millennial","Observed","RothC"),
                       values=cbPalette9[c(8,2,6,1,3)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCexp

  # get the slope and intercept for all mgmt scenarios for one climate scenario
  APSIM_coefs <- f_model_coef(df_in=annual_results,
                              modeled_element_in="SOC_Mgha",
                              model_name_in="APSIM",
                              climate_scen_in=clim_num,
                              mgmt_group_in=4,
                              result_name_in="Cfit")
  Daycent_coefs <- f_model_coef(df_in=annual_results,
                                modeled_element_in="SOC_Mgha",
                                model_name_in="Daycent",
                                climate_scen_in=clim_num,
                                mgmt_group_in=4,
                                result_name_in="Cfit")
  RothC_coefs <- f_model_coef(df_in=annual_results,
                                modeled_element_in="SOC_Mgha",
                                model_name_in="RothC",
                                climate_scen_in=clim_num,
                                mgmt_group_in=4,
                                result_name_in="Cfit")
  Millennial_coefs <- f_model_coef(df_in=annual_results,
                                modeled_element_in="SOC_Mgha",
                                model_name_in="Millennial",
                                climate_scen_in=clim_num,
                                mgmt_group_in=4,
                                result_name_in="Cfit")

    gCfut <- annual_results[annual_results$year>=experiment_end_year &
                            annual_results$model_name %in% c("APSIM","Daycent","RothC","Millennial") &
                            annual_results$mgmt_scenario_grp_num==4 &
                            annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SOC_Mgha, color=model_name, shape=scenario_abbrev)) +
    geom_line() +
    xlab("Year") +
    ylab(expression('Soil Organic C (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future SOC: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
    # geom_abline(intercept=Cfit_APSIM_1_41[1], slope=Cfit_APSIM_1_41[2],
    #             color=cbPalette9[8]) +
    # geom_abline(intercept=Cfit_Daycent_1_41[1], slope=Cfit_Daycent_1_41[2],
    #             color=cbPalette9[2]) +
    # geom_abline(intercept=Cfit_RothC_1_41[1], slope=Cfit_RothC_1_41[2],
    #             color=cbPalette9[3]) +
    # geom_abline(intercept=Cfit_APSIM_1_42[1], slope=Cfit_APSIM_1_42[2],
    #             color=cbPalette9[8]) +
    # geom_abline(intercept=Cfit_Daycent_1_42[1], slope=Cfit_Daycent_1_42[2],
    #             color=cbPalette9[2]) +
    # geom_abline(intercept=Cfit_RothC_1_42[1], slope=Cfit_RothC_1_42[2],
  #             color=cbPalette9[3]) +
  # geom_abline(intercept=Cfit_APSIM_1_43[1], slope=Cfit_APSIM_1_43[2],
  #             color=cbPalette9[8]) +
  # geom_abline(intercept=Cfit_Daycent_1_43[1], slope=Cfit_Daycent_1_43[2],
  #             color=cbPalette9[2]) +
  # geom_abline(intercept=Cfit_RothC_1_43[1], slope=Cfit_RothC_1_43[2],
  #             color=cbPalette9[3]) +
  # geom_abline(intercept=Cfit_APSIM_1_44[1], slope=Cfit_APSIM_1_44[2],
  #             color=cbPalette9[8]) +
  # geom_abline(intercept=Cfit_Daycent_1_44[1], slope=Cfit_Daycent_1_44[2],
  #             color=cbPalette9[2]) +
  # geom_abline(intercept=Cfit_RothC_1_44[1], slope=Cfit_RothC_1_44[2],
  #             color=cbPalette9[3]) +
  scale_color_manual(labels=c("APSIM","Daycent","Millennial","RothC"),
                     values=cbPalette9[c(8,2,6,3)]) +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCfut

  ggsave(filename=paste0(results_path,"scenario_comparison_SOC_exp_grp_4_",clim_num,".jpg"),plot=gCexp)
  ggsave(filename=paste0(results_path,"scenario_comparison_SOC_fut_grp_4_",clim_num,".jpg"),plot=gCfut)


  #*************************************************************

  ### Scen grp 5

  #### Maize yield

  gMYexp <- annual_results[annual_results$year>=experiment_start_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==5 &
                             annual_results$MaizeYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=MaizeYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Maize Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Maize Yield: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gMYexp

  # get the slope and intercept for all mgmt scenarios for one climate scenario
  APSIM_coefs <- f_model_coef(df_in=annual_results,
                              modeled_element_in="MaizeYld_Mgha",
                              model_name_in="APSIM",
                              climate_scen_in=clim_num,
                              mgmt_group_in=5,
                              result_name_in="MYfit")
  Daycent_coefs <- f_model_coef(df_in=annual_results,
                                modeled_element_in="MaizeYld_Mgha",
                                model_name_in="Daycent",
                                climate_scen_in=clim_num,
                                mgmt_group_in=5,
                                result_name_in="MYfit")

    gMYfut <- annual_results[annual_results$year>=experiment_end_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==5 &
                             annual_results$MaizeYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=MaizeYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Maize Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Maize Yield: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
      geom_abline(intercept=APSIM_coefs[1,3], slope=APSIM_coefs[1,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[1,3], slope=Daycent_coefs[1,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[2,3], slope=APSIM_coefs[2,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[2,3], slope=Daycent_coefs[2,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[3,3], slope=APSIM_coefs[3,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[3,3], slope=Daycent_coefs[3,4],
                  color=cbPalette9[2]) +
      scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gMYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_Maize_exp_grp_5_",clim_num,".jpg"),plot=gMYexp)
  ggsave(filename=paste0(results_path,"scenario_comparison_Maize_fut_grp_5_",clim_num,".jpg"),plot=gMYfut)



  #### Soybean yield

  gSYexp <- annual_results[annual_results$year>=experiment_start_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==5 &
                             annual_results$SoyYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SoyYld_Mgha, color=model_name,  shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Soybean Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Soybean Yield: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gSYexp

  # get the slope and intercept for all mgmt scenarios for one climate scenario
  APSIM_coefs <- f_model_coef(df_in=annual_results,
                              modeled_element_in="SoyYld_Mgha",
                              model_name_in="APSIM",
                              climate_scen_in=clim_num,
                              mgmt_group_in=5,
                              result_name_in="SYfit")
  Daycent_coefs <- f_model_coef(df_in=annual_results,
                                modeled_element_in="SoyYld_Mgha",
                                model_name_in="Daycent",
                                climate_scen_in=clim_num,
                                mgmt_group_in=5,
                                result_name_in="SYfit")

  gSYfut <- annual_results[annual_results$year>=experiment_end_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==5 &
                             annual_results$SoyYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SoyYld_Mgha, color=model_name,  shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Soybean Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Soybean Yield: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    geom_abline(intercept=APSIM_coefs[1,3], slope=APSIM_coefs[1,4],
                color=cbPalette9[8]) +
    geom_abline(intercept=Daycent_coefs[1,3], slope=Daycent_coefs[1,4],
                color=cbPalette9[2]) +
    geom_abline(intercept=APSIM_coefs[2,3], slope=APSIM_coefs[2,4],
                color=cbPalette9[8]) +
    geom_abline(intercept=Daycent_coefs[2,3], slope=Daycent_coefs[2,4],
                color=cbPalette9[2]) +
    geom_abline(intercept=APSIM_coefs[3,3], slope=APSIM_coefs[3,4],
                color=cbPalette9[8]) +
    geom_abline(intercept=Daycent_coefs[3,3], slope=Daycent_coefs[3,4],
                color=cbPalette9[2]) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gSYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_Soybean_exp_grp_5_",clim_num,".jpg"),plot=gSYexp)
  ggsave(filename=paste0(results_path,"scenario_comparison_Soybean_fut_grp_5_",clim_num,".jpg"),plot=gSYfut)


  #### Wheat yield

  gWYexp <- annual_results[annual_results$year>=experiment_start_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==5 &
                             annual_results$WheatYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=WheatYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Wheat Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Wheat Yield: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gWYexp

  # get the slope and intercept for all mgmt scenarios for one climate scenario
  APSIM_coefs <- f_model_coef(df_in=annual_results,
                              modeled_element_in="WheatYld_Mgha",
                              model_name_in="APSIM",
                              climate_scen_in=clim_num,
                              mgmt_group_in=5,
                              result_name_in="WYfit")
  Daycent_coefs <- f_model_coef(df_in=annual_results,
                                modeled_element_in="WheatYld_Mgha",
                                model_name_in="Daycent",
                                climate_scen_in=clim_num,
                                mgmt_group_in=5,
                                result_name_in="WYfit")

    gWYfut <- annual_results[annual_results$year>=experiment_end_year &
                             annual_results$model_name %in% c("APSIM","Daycent") &
                             annual_results$mgmt_scenario_grp_num==5 &
                             annual_results$WheatYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=WheatYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Wheat Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Wheat Yield: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
      geom_abline(intercept=APSIM_coefs[1,3], slope=APSIM_coefs[1,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[1,3], slope=Daycent_coefs[1,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[2,3], slope=APSIM_coefs[2,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[2,3], slope=Daycent_coefs[2,4],
                  color=cbPalette9[2]) +
      geom_abline(intercept=APSIM_coefs[3,3], slope=APSIM_coefs[3,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=Daycent_coefs[3,3], slope=Daycent_coefs[3,4],
                  color=cbPalette9[2]) +
      scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gWYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_Wheat_exp_grp_5_",clim_num,".jpg"),plot=gWYexp)
  ggsave(filename=paste0(results_path,"scenario_comparison_Wheat_fut_grp_5_",clim_num,".jpg"),plot=gWYfut)


  #### SOC
  Cfit_RothC_1_53 <- coef(lm(SOC_Mgha ~ year,
                             data = annual_results[annual_results$year>=experiment_end_year &
                                                     annual_results$model_name=="RothC" &
                                                     annual_results$scenario_name=="1_53",]))


  gCexp <- annual_results[annual_results$year>=experiment_start_year &
                            annual_results$model_name %in% c("APSIM","Daycent","RothC","Millennial") &
                            annual_results$mgmt_scenario_grp_num==5 &
                            annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SOC_Mgha, color=model_name, shape=scenario_abbrev)) +
    geom_point() +
    geom_point(data=newC,
               aes(x=year, y=cstock,
                   color=site, shape=site)) +
    xlab("Year") +
    ylab(expression('Soil Organic C (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future SOC: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent","Millennial","Observed","RothC"),
                       values=cbPalette9[c(8,2,6,1,3)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCexp

  gCfut <- annual_results[annual_results$year>=experiment_end_year &
                            annual_results$model_name %in% c("APSIM","Daycent","RothC","Millennial") &
                            annual_results$mgmt_scenario_grp_num==5 &
                            annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SOC_Mgha, color=model_name, shape=scenario_abbrev)) +
    geom_line() +
    xlab("Year") +
    ylab(expression('Soil Organic C (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future SOC: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent","Millennial","RothC"),
                     values=cbPalette9[c(8,2,6,3)]) +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCfut

  ggsave(filename=paste0(results_path,"scenario_comparison_SOC_exp_grp_5_",clim_num,".jpg"),plot=gCexp)
  ggsave(filename=paste0(results_path,"scenario_comparison_SOC_fut_grp_5_",clim_num,".jpg"),plot=gCfut)


  #*************************************************************

  ### Scen grp 6

  #### Maize yield
  MYfit_APSIM_1_61 <- coef(lm(MaizeYld_Mgha ~ year,
                              data = annual_results[annual_results$year>=experiment_end_year &
                                                      annual_results$MaizeYld_Mgha != 0 &
                                                      annual_results$model_name=="APSIM" &
                                                      annual_results$scenario_name=="1_61",]))
  MYfit_APSIM_1_62 <- coef(lm(MaizeYld_Mgha ~ year,
                              data = annual_results[annual_results$year>=experiment_end_year &
                                                      annual_results$MaizeYld_Mgha != 0 &
                                                      annual_results$model_name=="APSIM" &
                                                      annual_results$scenario_name=="1_62",]))
  MYfit_APSIM_1_63 <- coef(lm(MaizeYld_Mgha ~ year,
                              data = annual_results[annual_results$year>=experiment_end_year &
                                                      annual_results$MaizeYld_Mgha != 0 &
                                                      annual_results$model_name=="APSIM" &
                                                      annual_results$scenario_name=="1_63",]))
  MYfit_APSIM_1_64 <- coef(lm(MaizeYld_Mgha ~ year,
                              data = annual_results[annual_results$year>=experiment_end_year &
                                                      annual_results$MaizeYld_Mgha != 0 &
                                                      annual_results$model_name=="APSIM" &
                                                      annual_results$scenario_name=="1_64",]))
  MYfit_APSIM_1_65 <- coef(lm(MaizeYld_Mgha ~ year,
                              data = annual_results[annual_results$year>=experiment_end_year &
                                                      annual_results$MaizeYld_Mgha != 0 &
                                                      annual_results$model_name=="APSIM" &
                                                      annual_results$scenario_name=="1_65",]))

  gMYexp <- annual_results[annual_results$year>=experiment_start_year &
                             annual_results$model_name %in% c("APSIM") &
                             annual_results$mgmt_scenario_grp_num==6 &
                             annual_results$MaizeYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=MaizeYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Maize Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Maize Yield: Biochar Addition"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM"),
                       values=cbPalette9[c(8)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gMYexp

  # get the slope and intercept for all mgmt scenarios for one climate scenario
  APSIM_coefs <- f_model_coef(df_in=annual_results,
                              modeled_element_in="MaizeYld_Mgha",
                              model_name_in="APSIM",
                              climate_scen_in=clim_num,
                              mgmt_group_in=6,
                              result_name_in="MYfit")

    gMYfut <- annual_results[annual_results$year>=experiment_end_year &
                             annual_results$model_name %in% c("APSIM") &
                             annual_results$mgmt_scenario_grp_num==6 &
                             #annual_results$scenario_name=="1_61" &
                             annual_results$MaizeYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=MaizeYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Maize Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Maize Yield: Biochar Addition"),
            paste("Climate Scenario:",climate_desc)) +
      geom_abline(intercept=APSIM_coefs[1,3], slope=APSIM_coefs[1,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[2,3], slope=APSIM_coefs[2,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[3,3], slope=APSIM_coefs[3,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[4,3], slope=APSIM_coefs[4,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[5,3], slope=APSIM_coefs[5,4],
                  color=cbPalette9[8]) +
      scale_color_manual(labels=c("APSIM"),
                       values=cbPalette9[c(8)]) +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gMYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_Maize_exp_grp_6_",clim_num,".jpg"),plot=gMYexp)
  ggsave(filename=paste0(results_path,"scenario_comparison_Maize_fut_grp_6_",clim_num,".jpg"),plot=gMYfut)



  #### Soybean yield

  gSYexp <- annual_results[annual_results$year>=experiment_start_year &
                             annual_results$model_name %in% c("APSIM") &
                             annual_results$mgmt_scenario_grp_num==6 &
                             annual_results$SoyYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SoyYld_Mgha, color=model_name,  shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Soybean Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Soybean Yield: Biochar Addition"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM"),
                       values=cbPalette9[c(8)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gSYexp


  APSIM_coefs <- f_model_coef(df_in=annual_results,
                              modeled_element_in="SoyYld_Mgha",
                              model_name_in="APSIM",
                              climate_scen_in=clim_num,
                              mgmt_group_in=6,
                              result_name_in="SYfit")

    gSYfut <- annual_results[annual_results$year>=experiment_end_year &
                             annual_results$model_name %in% c("APSIM") &
                             annual_results$mgmt_scenario_grp_num==6 &
                             annual_results$SoyYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SoyYld_Mgha, color=model_name,  shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Soybean Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Soybean Yield: Biochar Addition"),
            paste("Climate Scenario:",climate_desc)) +
      geom_abline(intercept=APSIM_coefs[1,3], slope=APSIM_coefs[1,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[2,3], slope=APSIM_coefs[2,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[3,3], slope=APSIM_coefs[3,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[4,3], slope=APSIM_coefs[4,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[5,3], slope=APSIM_coefs[5,4],
                  color=cbPalette9[8]) +
      scale_color_manual(labels=c("APSIM"),
                       values=cbPalette9[c(8)]) +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gSYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_Soybean_exp_grp_6_",clim_num,".jpg"),plot=gSYexp)
  ggsave(filename=paste0(results_path,"scenario_comparison_Soybean_fut_grp_6_",clim_num,".jpg"),plot=gSYfut)


  #### Wheat yield

  gWYexp <- annual_results[annual_results$year>=experiment_start_year &
                             annual_results$model_name %in% c("APSIM") &
                             annual_results$mgmt_scenario_grp_num==6 &
                             annual_results$WheatYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=WheatYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Wheat Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Wheat Yield: Biochar Addition"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM"),
                       values=cbPalette9[c(8)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gWYexp

  APSIM_coefs <- f_model_coef(df_in=annual_results,
                              modeled_element_in="WheatYld_Mgha",
                              model_name_in="APSIM",
                              climate_scen_in=clim_num,
                              mgmt_group_in=6,
                              result_name_in="WYfit")

    gWYfut <- annual_results[annual_results$year>=experiment_end_year &
                             annual_results$model_name %in% c("APSIM") &
                             annual_results$mgmt_scenario_grp_num==6 &
                             annual_results$WheatYld_Mgha != 0 &
                             annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=WheatYld_Mgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('Wheat Yield (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name,"Future Wheat Yield: Biochar Addition"),
            paste("Climate Scenario:",climate_desc)) +
      geom_abline(intercept=APSIM_coefs[1,3], slope=APSIM_coefs[1,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[2,3], slope=APSIM_coefs[2,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[3,3], slope=APSIM_coefs[3,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[4,3], slope=APSIM_coefs[4,4],
                  color=cbPalette9[8]) +
      geom_abline(intercept=APSIM_coefs[5,3], slope=APSIM_coefs[5,4],
                  color=cbPalette9[8]) +
      scale_color_manual(labels=c("APSIM"),
                       values=cbPalette9[c(8)]) +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gWYfut

  ggsave(filename=paste0(results_path,"scenario_comparison_Wheat_exp_grp_6_",clim_num,".jpg"),plot=gWYexp)
  ggsave(filename=paste0(results_path,"scenario_comparison_Wheat_fut_grp_6_",clim_num,".jpg"),plot=gWYfut)


  #### SOC

    newC <- ObsC_Mgha[ObsC_Mgha$year > land_conversion_year,] %>%
    mutate(site="Observed")

  gCexp <- ggplot() +
    geom_point(data=annual_results[annual_results$year>=experiment_start_year &
                                     annual_results$model_name %in% c("APSIM") &
                                     annual_results$mgmt_scenario_grp_num==6 &
                                     annual_results$climate_scenario_num==clim_num,],
               aes(x=year, y=SOC_Mgha, color=model_name, shape=scenario_abbrev)) +
    geom_point(data=newC,
               aes(x=year, y=cstock,
                   color=site, shape=site)) +
    xlab("Year") +
    ylab(expression('Soil Organic C (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future SOC: Biochar Addition"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Observed"),
                       values=cbPalette9[c(8,1)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCexp

  gCfut <- annual_results[annual_results$year>=experiment_end_year &
                            annual_results$model_name %in% c("APSIM") &
                            annual_results$mgmt_scenario_grp_num==6 &
                            annual_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=SOC_Mgha, color=model_name, shape=scenario_abbrev)) +
    geom_line() +
    xlab("Year") +
    ylab(expression('Soil Organic C (Mg ha' ^-1*')')) +
    ggtitle(paste(site_name," Future SOC: Biochar Addition"),
            paste("Climate Scenario:",climate_desc)) +
    # geom_abline(intercept=Cfit_APSIM_1_41[1], slope=Cfit_APSIM_1_41[2],
    #             color=cbPalette9[8]) +
    # geom_abline(intercept=Cfit_Daycent_1_41[1], slope=Cfit_Daycent_1_41[2],
    #             color=cbPalette9[2]) +
    # geom_abline(intercept=Cfit_RothC_1_41[1], slope=Cfit_RothC_1_41[2],
    #             color=cbPalette9[3]) +
    # geom_abline(intercept=Cfit_APSIM_1_42[1], slope=Cfit_APSIM_1_42[2],
    #             color=cbPalette9[8]) +
    # geom_abline(intercept=Cfit_Daycent_1_42[1], slope=Cfit_Daycent_1_42[2],
    #             color=cbPalette9[2]) +
    # geom_abline(intercept=Cfit_RothC_1_42[1], slope=Cfit_RothC_1_42[2],
  #             color=cbPalette9[3]) +
  # geom_abline(intercept=Cfit_APSIM_1_43[1], slope=Cfit_APSIM_1_43[2],
  #             color=cbPalette9[8]) +
  # geom_abline(intercept=Cfit_Daycent_1_43[1], slope=Cfit_Daycent_1_43[2],
  #             color=cbPalette9[2]) +
  # geom_abline(intercept=Cfit_RothC_1_43[1], slope=Cfit_RothC_1_43[2],
  #             color=cbPalette9[3]) +
  # geom_abline(intercept=Cfit_APSIM_1_44[1], slope=Cfit_APSIM_1_44[2],
  #             color=cbPalette9[8]) +
  # geom_abline(intercept=Cfit_Daycent_1_44[1], slope=Cfit_Daycent_1_44[2],
  #             color=cbPalette9[2]) +
  # geom_abline(intercept=Cfit_RothC_1_44[1], slope=Cfit_RothC_1_44[2],
  #             color=cbPalette9[3]) +
  scale_color_manual(labels=c("APSIM"),
                     values=cbPalette9[c(8)]) +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCfut

  ggsave(filename=paste0(results_path,"scenario_comparison_SOC_exp_grp_6_",clim_num,".jpg"),plot=gCexp)
  ggsave(filename=paste0(results_path,"scenario_comparison_SOC_fut_grp_6_",clim_num,".jpg"),plot=gCfut)


  #*************************************************************


  ## Daily graphs-combined scenarios

  ### Mean of all scenarios

  gAllNexp <- mean_daily_results[mean_daily_results$year>=experiment_start_year &
                                   mean_daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=N2O_cum_kgha, color=scenario_abbrev)) +
    geom_line(size=1) +
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (kg ha ' ^-1*')')) +
    ggtitle(paste(site_name," Future N2O Cumulative Emissions: All Scenarios, Model Mean"),
            paste("Climate Scenario:",climate_desc)) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gAllNexp

  gAllNfut <- mean_daily_results[mean_daily_results$year>=experiment_end_year &
                                   mean_daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=N2O_cum_kgha, color=scenario_abbrev)) +
    geom_line(size=1) +
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (kg ha ' ^-1*')')) +
    ggtitle(paste(site_name," Future N2O Cumulative Emissions: All Scenarios, Model Mean"),
            paste("Climate Scenario:",climate_desc)) +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gAllNfut

  ggsave(filename=paste0(results_path,"scenario_comparison_mean_N2O_exp_",clim_num,".jpg"),plot=gAllNexp)
  ggsave(filename=paste0(results_path,"scenario_comparison_mean_N2O_fut_",clim_num,".jpg"),plot=gAllNfut)


  gAllCHexp <- mean_daily_results[mean_daily_results$year>=experiment_start_year &
                                    mean_daily_results$climate_scenario_num==clim_num &
                                    mean_daily_results$mgmt_scenario_grp_num!=6,] %>%
    ggplot(aes(x=year, y=CH4_cum_kgha, color=scenario_abbrev)) +
    geom_line(size=1) +
    xlab("Year") +
    ylab(expression('CH'[4]*' Emissions (kg ha ' ^-1*')')) +
    ggtitle(paste(site_name," Future CH4 Cumulative Emissions: All Scenarios, Model Mean"),
            paste("Climate Scenario:",climate_desc)) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gAllCHexp

  gAllCHfut <- mean_daily_results[mean_daily_results$year>=experiment_end_year &
                                    mean_daily_results$climate_scenario_num==clim_num &
                                    mean_daily_results$mgmt_scenario_grp_num!=6,] %>%
    ggplot(aes(x=year, y=CH4_cum_kgha, color=scenario_abbrev)) +
    geom_line(size=1) +
    xlab("Year") +
    ylab(expression('CH'[4]*' Emissions (kg ha ' ^-1*')')) +
    ggtitle(paste(site_name," Future CH4 Cumulative Emissions: All Scenarios, Model Mean"),
            paste("Climate Scenario:",climate_desc)) +
    # scale_color_manual(labels=c("APSIM","Daycent","Observed"),
    #                    values=cbPalette9[c(8,2,1)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gAllCHfut

  ggsave(filename=paste0(results_path,"scenario_comparison_mean_CH4_exp_",clim_num,".jpg"),plot=gAllCHexp)
  ggsave(filename=paste0(results_path,"scenario_comparison_mean_CH4_fut_",clim_num,".jpg"),plot=gAllCHfut)


  #*************************************************************

  ### Scen grp 4


  # N2O

  gNexp <- daily_results[daily_results$year>=experiment_start_year &
                           daily_results$model_name %in% c("APSIM","Daycent") &
                           daily_results$mgmt_scenario_grp_num==4 &
                           daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=N2O_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (kg ha ' ^-1*')')) +
    ggtitle(paste(site_name," Cumulative N2O Emissions: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gNexp

  gNfut <- daily_results[daily_results$year>=experiment_end_year &
                           daily_results$model_name %in% c("APSIM","Daycent") &
                           daily_results$mgmt_scenario_grp_num==4 &
                           daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=N2O_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (kg ha ' ^-1*')')) +
    ggtitle(paste(site_name," Cumulative N2O Emissions: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                     values=cbPalette9[c(8,2)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gNfut

  ggsave(filename=paste0(results_path,"scenario_comparison_N2O_exp_grp_4_",clim_num,".jpg"),plot=gNexp)
  ggsave(filename=paste0(results_path,"scenario_comparison_N2O_fut_grp_4_",clim_num,".jpg"),plot=gNfut)


  # CH4

  gCHexp <- daily_results[daily_results$year>=experiment_start_year &
                            daily_results$model_name %in% c("Daycent") &
                            daily_results$mgmt_scenario_grp_num==4 &
                            daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=CH4_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('CH'[4]*' Emissions (kg ha ' ^-1*')')) +
    ggtitle(paste(site_name," Cumulative Net CH4 Emissions: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("Daycent"),
                       values=cbPalette9[c(2)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCHexp

  gCHfut <- daily_results[daily_results$year>=experiment_end_year &
                            daily_results$model_name %in% c("Daycent") &
                            daily_results$mgmt_scenario_grp_num==4 &
                            daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=CH4_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('CH'[4]*' Emissions (kg ha ' ^-1*')')) +
    ggtitle(paste(site_name," Cumulative Net CH4 Emissions: Reducing Fertilizer Input"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("Daycent"),
                     values=cbPalette9[c(2)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCHfut

  ggsave(filename=paste0(results_path,"scenario_comparison_CH4_exp_grp_4_",clim_num,".jpg"),plot=gCHexp)
  ggsave(filename=paste0(results_path,"scenario_comparison_CH4_fut_grp_4_",clim_num,".jpg"),plot=gCHfut)


  #*************************************************************


  ### Scen grp 5

  # N2O

  gNexp <- daily_results[daily_results$year>=experiment_start_year &
                           daily_results$model_name %in% c("APSIM","Daycent") &
                           daily_results$mgmt_scenario_grp_num==5 &
                           daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=N2O_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (kg ha ' ^-1*')')) +
    ggtitle(paste(site_name," Cumulative N2O Emissions: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                       values=cbPalette9[c(8,2)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gNexp

  gNfut <- daily_results[daily_results$year>=experiment_end_year &
                           daily_results$model_name %in% c("APSIM","Daycent") &
                           daily_results$mgmt_scenario_grp_num==5 &
                           daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=N2O_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (kg ha ' ^-1*')')) +
    ggtitle(paste(site_name," Cumulative N2O Emissions: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM","Daycent"),
                     values=cbPalette9[c(8,2)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gNfut

  ggsave(filename=paste0(results_path,"scenario_comparison_N2O_exp_grp_5_",clim_num,".jpg"),plot=gNexp)
  ggsave(filename=paste0(results_path,"scenario_comparison_N2O_fut_grp_5_",clim_num,".jpg"),plot=gNfut)


  # CH4

  gCHexp <- daily_results[daily_results$year>=experiment_start_year &
                            daily_results$model_name %in% c("Daycent") &
                            daily_results$mgmt_scenario_grp_num==5 &
                            daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=CH4_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('CH'[4]*' Emissions (kg ha ' ^-1*')')) +
    ggtitle(paste(site_name," Cumulative Net CH4 Emissions: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("Daycent"),
                       values=cbPalette9[c(2)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCHexp

  gCHfut <- daily_results[daily_results$year>=experiment_end_year &
                            daily_results$model_name %in% c("Daycent") &
                            daily_results$mgmt_scenario_grp_num==5 &
                            daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=CH4_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('CH'[4]*' Emissions (kg ha ' ^-1*')')) +
    ggtitle(paste(site_name," Cumulative Net CH4 Emissions: Reducing Residue Removal"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("Daycent"),
                     values=cbPalette9[c(2)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gCHfut

  ggsave(filename=paste0(results_path,"scenario_comparison_CH4_exp_grp_5_",clim_num,".jpg"),plot=gCHexp)
  ggsave(filename=paste0(results_path,"scenario_comparison_CH4_fut_grp_5_",clim_num,".jpg"),plot=gCHfut)


  #*************************************************************


  ### Scen grp 6

  # N2O

  gNexp <- daily_results[daily_results$year>=experiment_start_year &
                           daily_results$model_name %in% c("APSIM") &
                           daily_results$mgmt_scenario_grp_num==6 &
                           daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=N2O_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (kg ha ' ^-1*')')) +
    ggtitle(paste(site_name," Cumulative N2O Emissions: Biochar Addition"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM"),
                       values=cbPalette9[c(8)]) +
    geom_vline(xintercept=experiment_end_year,linetype="dashed",color="darkgrey") +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gNexp

  gNfut <- daily_results[daily_results$year>=experiment_end_year &
                           daily_results$model_name %in% c("APSIM") &
                           daily_results$mgmt_scenario_grp_num==6 &
                           daily_results$climate_scenario_num==clim_num,] %>%
    ggplot(aes(x=year, y=N2O_cum_kgha, color=model_name, shape=scenario_abbrev, show.legend=TRUE)) +
    geom_point(show.legend=TRUE) +
    xlab("Year") +
    ylab(expression('N'[2]*'O Emissions (kg ha ' ^-1*')')) +
    ggtitle(paste(site_name," Cumulative N2O Emissions: Biochar Addition"),
            paste("Climate Scenario:",climate_desc)) +
    scale_color_manual(labels=c("APSIM"),
                     values=cbPalette9[c(8)]) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          legend.position = "right",
          legend.key = element_blank())

  gNfut

  ggsave(filename=paste0(results_path,"scenario_comparison_N2O_exp_grp_6_clim_",clim_num,".jpg"),
         plot=gNexp)
  ggsave(filename=paste0(results_path,"scenario_comparison_N2O_fut_grp_6_clim_",clim_num,".jpg"),
         plot=gNfut)
  
  
  #*************************************************************
  
# GWP Bar charts

y_breaks <- seq(-50, 150, by = 10)
#y_labels <- sprintf("%.1f", y_breaks)
#y_labels[c(2,4,6,8)] <- ""

gGWP <- scenario_means[scenario_means$Climate_Scenario==clim_num,] %>%
  ggplot(aes(x=scenario_abbrev, y=mean_GWP, fill=factor(scenario_abbrev))) +
  geom_col(position="dodge") +
  geom_errorbar(aes(ymin=mean_GWP-sd_GWP, ymax=mean_GWP+sd_GWP),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  xlab("") +
  ggtitle(paste(site_name," Global Warming Potential-Model Means"),
          paste("Climate Scenario:",climate_desc)) +
  labs(fill = "Scenario") +
 # scale_y_continuous(breaks=y_breaks) +
  theme(panel.background = element_blank(),
        #        text = element_text(size=16),
        axis.line = element_line(colour = "black"), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

gGWP

gGWPmm <- gwp_means_piv[gwp_means_piv$source %in% c("mean_CO2e_SOC","mean_CO2e_N2O","mean_CO2e_CH4") &
                          gwp_means_piv$Climate_Scenario==clim_num,] %>%
  ggplot(aes(x=scenario_abbrev, y=vals, fill=source)) +
  geom_col(position="stack") +
  # geom_errorbar(aes(ymin=mean_GWP-sd_GWP, ymax=mean_GWP+sd_GWP),
  #               width=.2,                    # Width of the error bars
  #               position=position_dodge(.9)) +
  ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
  xlab("") +
  ggtitle(paste(site_name," Global Warming Potential by Source-Model Means"),
          paste("Climate Scenario:",climate_desc)) +
  labs(fill = "Source") +
  theme(panel.background = element_blank(),
        #        text = element_text(size=16),
        axis.line = element_line(colour = "black"), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

gGWPmm

#####  Not working yet: split GWP bar graphs with error bars
#####  (see KBS_results/Archive/split_bar_graph_with_error_bars_example.R)
#
# gGWPmmeb <- 
#   ggplot(gwp_means_piv[gwp_means_piv$source == "mean_CO2e_SOC" &
#                          gwp_means_piv$Climate_Scenario==clim_num,],
#          aes(x=scenario_abbrev, y=vals, fill=source)) +
#   geom_col(position="stack") +
#   geom_errorbar(data=gwp_means_piv[gwp_means_piv$source %in% c("mean_CO2e_SOC","sd_CO2c_SOC") &
#                                 gwp_means_piv$Climate_Scenario==clim_num,],
#                 aes(x=source,
#                     y=vals,
#                       ymin=gwp_means_piv[gwp_means_piv$source=="mean_CO2e_SOC","vals"]-
#                       ,
#                     ymax=gwp_means_piv[gwp_means_piv$source=="mean_CO2e_SOC","vals"]+
#                         gwp_means_piv[gwp_means_piv$source=="sd_CO2e_SOC","vals"]),
#                 width=.2,                    # Width of the error bars
#                 position=position_dodge(.9)) +
#   ylab(expression('CO'[2]*'e (Mg ha ' ^-1*')')) +
#   xlab("") +
#   ggtitle(paste(site_name," Global Warming Potential by Source-Model Means"),
#           paste("Climate Scenario:",climate_desc)) +
#   labs(fill = "Source") +
#   theme(panel.background = element_blank(),
#         #        text = element_text(size=16),
#         axis.line = element_line(colour = "black"), 
#         axis.ticks.x = element_blank(),
#         axis.text.x = element_text(angle = 45,
#                                    hjust = 1))
# 
# gGWPmmeb

ggsave(filename=paste0(results_path,"GWP_all_scenarios_",clim_num,".jpg"),plot=gGWP)
ggsave(filename=paste0(results_path,"GWP_by_source_all_scenarios_",clim_num,".jpg"),plot=gGWPmm)

#*************************************************************

# Change over time charts

## Maize

gMYchg <- scenario_means[scenario_means$Climate_Scenario==clim_num,] %>%
  ggplot(aes(x=scenario_abbrev, y=mean_MaizeYld_Mgha, fill=factor(scenario_abbrev))) +
  geom_col(position="dodge") +
  geom_errorbar(aes(ymin=mean_MaizeYld_Mgha-sd_MaizeYld_Mgha, 
                    ymax=mean_MaizeYld_Mgha+sd_MaizeYld_Mgha),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  ylab(expression('Maize Yield (Mg ha ' ^-1*')')) +
  xlab("") +
  ggtitle(paste(site_name,"Change in Maize Yield-Model Means"),
          paste("Climate Scenario:",climate_desc)) +
  labs(fill = "Scenario") +
  # scale_y_continuous(breaks=y_breaks) +
  theme(panel.background = element_blank(),
        #        text = element_text(size=16),
        axis.line = element_line(colour = "black"), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

gMYchg

## Soybean

gSYchg <- scenario_means[scenario_means$Climate_Scenario==clim_num,] %>%
  ggplot(aes(x=scenario_abbrev, y=mean_SoyYld_Mgha, fill=factor(scenario_abbrev))) +
  geom_col(position="dodge") +
  geom_errorbar(aes(ymin=mean_SoyYld_Mgha-sd_SoyYld_Mgha, 
                    ymax=mean_SoyYld_Mgha+sd_SoyYld_Mgha),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  ylab(expression('Soybean Yield (Mg ha ' ^-1*')')) +
  xlab("") +
  ggtitle(paste(site_name,"Change in Soybean Yield-Model Means"),
          paste("Climate Scenario:",climate_desc)) +
  labs(fill = "Scenario") +
  # scale_y_continuous(breaks=y_breaks) +
  theme(panel.background = element_blank(),
        #        text = element_text(size=16),
        axis.line = element_line(colour = "black"), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

gSYchg

## Wheat

gWYchg <- scenario_means[scenario_means$Climate_Scenario==clim_num,] %>%
  ggplot(aes(x=scenario_abbrev, y=mean_WheatYld_Mgha, fill=factor(scenario_abbrev))) +
  geom_col(position="dodge") +
  geom_errorbar(aes(ymin=mean_WheatYld_Mgha-sd_WheatYld_Mgha, 
                    ymax=mean_WheatYld_Mgha+sd_WheatYld_Mgha),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  ylab(expression('Wheat Yield (Mg ha ' ^-1*')')) +
  xlab("") +
  ggtitle(paste(site_name,"Change in Wheat Yield-Model Means"),
          paste("Climate Scenario:",climate_desc)) +
  labs(fill = "Scenario") +
  # scale_y_continuous(breaks=y_breaks) +
  theme(panel.background = element_blank(),
        #        text = element_text(size=16),
        axis.line = element_line(colour = "black"), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

gWYchg

## SOC

gSOCchg <- scenario_means[scenario_means$Climate_Scenario==clim_num,] %>%
  ggplot(aes(x=scenario_abbrev, y=mean_SOC_Mgha, fill=factor(scenario_abbrev))) +
  geom_col(position="dodge") +
  geom_errorbar(aes(ymin=mean_SOC_Mgha-sd_SOC_Mgha, 
                    ymax=mean_SOC_Mgha+sd_SOC_Mgha),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  ylab(expression('SOC (Mg ha ' ^-1*')')) +
  xlab("") +
  ggtitle(paste(site_name,"Change in Soil Organic Carbon-Model Means"),
          paste("Climate Scenario:",climate_desc)) +
  labs(fill = "Scenario") +
  # scale_y_continuous(breaks=y_breaks) +
  theme(panel.background = element_blank(),
        #        text = element_text(size=16),
        axis.line = element_line(colour = "black"), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

gSOCchg

## N2O

gN2Ochg <- scenario_gas_means[scenario_gas_means$Climate_Scenario==clim_num,] %>%
  ggplot(aes(x=scenario_abbrev, y=mean_N2O_Mgha, fill=factor(scenario_abbrev))) +
  geom_col(position="dodge") +
  geom_errorbar(aes(ymin=mean_N2O_Mgha-sd_N2O_Mgha, 
                    ymax=mean_N2O_Mgha+sd_N2O_Mgha),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  ylab(expression('N2O (Mg ha ' ^-1*')')) +
  xlab("") +
  ggtitle(paste(site_name,"Cumulative N2O Emissions-Model Means"),
          paste("Climate Scenario:",climate_desc)) +
  labs(fill = "Scenario") +
  # scale_y_continuous(breaks=y_breaks) +
  theme(panel.background = element_blank(),
        #        text = element_text(size=16),
        axis.line = element_line(colour = "black"), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

gN2Ochg

## CH4

gCH4chg <- scenario_gas_means[scenario_gas_means$Climate_Scenario==clim_num,] %>%
  ggplot(aes(x=scenario_abbrev, y=mean_CH4_Mgha, fill=factor(scenario_abbrev))) +
  geom_col(position="dodge") +
  geom_errorbar(aes(ymin=mean_CH4_Mgha-sd_CH4_Mgha, 
                    ymax=mean_CH4_Mgha+sd_CH4_Mgha),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  ylab(expression('CH4 (Mg ha ' ^-1*')')) +
  xlab("") +
  ggtitle(paste(site_name,"Cumulative CH4 Emissions-Model Means"),
          paste("Climate Scenario:",climate_desc)) +
  labs(fill = "Scenario") +
  # scale_y_continuous(breaks=y_breaks) +
  theme(panel.background = element_blank(),
        #        text = element_text(size=16),
        axis.line = element_line(colour = "black"), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

gCH4chg

ggsave(filename=paste0(results_path,"change_in_maize_all_scenarios_",clim_num,".jpg"),plot=gMYchg)
ggsave(filename=paste0(results_path,"change_in_soybean_all_scenarios_",clim_num,".jpg"),plot=gSYchg)
ggsave(filename=paste0(results_path,"change_in_wheat_all_scenarios_",clim_num,".jpg"),plot=gWYchg)
ggsave(filename=paste0(results_path,"change_in_soc_all_scenarios_",clim_num,".jpg"),plot=gSOCchg)
ggsave(filename=paste0(results_path,"change_in_n2o_all_scenarios_",clim_num,".jpg"),plot=gN2Ochg)
ggsave(filename=paste0(results_path,"change_in_ch4_all_scenarios_",clim_num,".jpg"),plot=gCH4chg)

} # end for loop through climate scenarios

}) # end suppressMessages