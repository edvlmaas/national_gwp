#######################################
# File: "p_Daycent_Cinput3.R"
# Author: "Ellen Maas"
# Date: "Nov 6, 2022"
# Description: This script takes C output from Daycent for use by
# RothC and Millennial."
#
#######################################
# Calls:
#
#######################################
# Audit Log
# 11/5/2022: Created script.
#
#######################################

suppressMessages({
  
print("Starting p_Daycent_Cinput3")

library(datetime)

# the .lis file provides the annual soil c input (clitad(2))
lis_output <- read.table(paste0(daycent_path,paste0("sched_fut_",scenario_name,".lis")),
                         #lis_output <- read.table(paste0(daycent_path,paste0("sched_exp_",scenario_name,".lis")),
                         col.names = c("time","somsc_gm2","somtc","somte_1",
                                       "crpval","cinput","somse_1","petann",
                                       "tminrl_1","minerl_1_1","minerl_2_1",
                                       "minerl_3_1","minerl_4_1","minerl_5_1",
                                       "minerl_6_1","minerl_7_1","minerl_8_1",
                                       "aglivc","bglivcj","bglivcm","cgrain",
                                       "crmvst","hi","clitad_1","clitad_2",
                                       "elitad_1_1","elitad_2_1"),
                         colClasses=c("numeric","numeric","numeric","numeric",
                                      "character","numeric","numeric","numeric",
                                      "numeric","numeric","numeric","numeric",
                                      "numeric","numeric","numeric","numeric",
                                      "numeric","numeric","numeric","numeric",
                                      "numeric","numeric","numeric","numeric",
                                      "numeric","numeric","numeric"),
                         skip=45)
lis_output <- lis_output[lis_output$time==land_conversion_year | lis_output$clitad_2!=0,]

# the livec.out file gives cumulative daily above- and below-ground C input
livec_output_base <- read.table(paste0(daycent_path,paste0("livec_base_",scenario_name,".out")),
                                col.names = c("time","dayofyr","aglivc","bglivcj","bglivcm","rleavc",
                                              "frootcj","frootcm","fbrchc","rlwodc","crootc","frnutc"),
                                colClasses=c("numeric","numeric","numeric","numeric","numeric",
                                             "numeric","numeric","numeric","numeric","numeric",
                                             "numeric","numeric"),
                                skip=1)
livec_output_exp <- read.table(paste0(daycent_path,paste0("livec_exp_",scenario_name,".out")),
                               col.names = c("time","dayofyr","aglivc","bglivcj","bglivcm","rleavc",
                                             "frootcj","frootcm","fbrchc","rlwodc","crootc","frnutc"),
                               colClasses=c("numeric","numeric","numeric","numeric","numeric",
                                            "numeric","numeric","numeric","numeric","numeric",
                                            "numeric","numeric"),
                               skip=1)
livec_output_fut <- read.table(paste0(daycent_path,paste0("livec_fut_",scenario_name,".out")),
                               col.names = c("time","dayofyr","aglivc","bglivcj","bglivcm","rleavc",
                                             "frootcj","frootcm","fbrchc","rlwodc","crootc","frnutc"),
                               colClasses=c("numeric","numeric","numeric","numeric","numeric",
                                            "numeric","numeric","numeric","numeric","numeric",
                                            "numeric","numeric"),
                               skip=1)


livec_output <- rbind(livec_output_base,livec_output_exp,livec_output_fut) %>%
  mutate(year=floor(time),
         date=as.Date(dayofyr-1, origin = paste0(year,"-01-01")),
         tot_plt_growth=aglivc+bglivcj+bglivcm,
         daily_NPP=ifelse(tot_plt_growth>lag(tot_plt_growth,default=1000000),
                          tot_plt_growth-lag(tot_plt_growth,default=1000000),
                          0))


# collect plant/harvest dates
all_years <- data.frame(year=land_conversion_year:2100)

## assemble annual planting and harvest dates for crops - build list from schedule files
planting_base <- data.frame(year=all_years[all_years$year >= land_conversion_year &
                             all_years$year < experiment_start_year,"year"]) %>%
  mutate(observation_type="Planting",
         dayofyr=if_else(year >= land_conversion_year & year <= 1949,124,
                 if_else(year >= 1950 & year < experiment_start_year & (year %% 2) == 0,149, #even years
                 if_else(year >= 1950 & year < experiment_start_year & (year %% 2) == 1,124, #odd years
                         0
                 ))),
         date=as.Date(dayofyr-1, origin = paste0(year,"-01-01")),
         crop=if_else(year >= land_conversion_year & year <= 1949,"Maize",
              if_else(year >= 1950 & year < experiment_start_year & (year %% 2) == 0,"Soybean", #even years
              if_else(year >= 1950 & year < experiment_start_year & (year %% 2) == 1,"Maize", #odd years
                      "Error"
              )))
  )

harvest_base <- data.frame(year=all_years[all_years$year >= land_conversion_year &
                                             all_years$year < experiment_start_year,"year"]) %>%
  mutate(observation_type="Harvest",
         dayofyr=if_else(year >= land_conversion_year & year <= 1949,296-1,
                 if_else(year >= 1950 & year < experiment_start_year & (year %% 2) == 0,290-1, #even years
                 if_else(year >= 1950 & year < experiment_start_year & (year %% 2) == 1,296-1, #odd years
                         0
                 ))),
         date=as.Date(dayofyr-1, origin = paste0(year,"-01-01")),
         crop=if_else(year >= land_conversion_year & year <= 1949,"Maize",
              if_else(year >= 1950 & year < experiment_start_year & (year %% 2) == 0,"Soybean", #even years
              if_else(year >= 1950 & year < experiment_start_year & (year %% 2) == 1,"Maize", #odd years
                      "Error"
              )))
         )

# add field ops data - reduce to unique rows
### NOTE: There MUST only be one Planting and one Harvest per crop (vs. separate for grain
###       and stover)
field_ops_exp <- unique(full_ops_ext_adj[full_ops_ext_adj$observation_type %in% c("Planting","Harvest"),
                 c("year","observation_type","date","crop")]) %>%
  mutate(date = if_else(observation_type=="Harvest", date-1, date))#,
         #dayofyear=yday(date))
#field_ops_exp <- field_ops_exp[,c("year","observation_type","dayofyear","date","crop")]

# set planting and harvest dates by crop
## corn
planting_corn_fut <- data.frame(year=all_years[all_years$year > experiment_end_year &
                                                 (all_years$year %% 3) == 1,"year"]) %>%
  mutate(observation_type="Planting",
         dayofyr=147,
         date=as.Date(dayofyr-1, origin = paste0(year,"-01-01")),
         crop="Maize")
harvest_corn_fut <- data.frame(year=all_years[all_years$year > experiment_end_year &
                                                (all_years$year %% 3) == 1,"year"]) %>%
  mutate(observation_type="Harvest",
         dayofyr=302,
         date=as.Date(dayofyr-1, origin = paste0(year,"-01-01")),
         crop="Maize")

## oats following corn
planting_corncc_fut <- data.frame(year=all_years[all_years$year > experiment_end_year &
                                                   (all_years$year %% 3) == 1,"year"]) %>%
  mutate(observation_type="Planting",
         dayofyr=321,
         date=as.Date(dayofyr-1, origin = paste0(year,"-01-01")),
         crop=covercrop_aftercorn)
harvest_corncc_fut <- data.frame(year=all_years[all_years$year > experiment_end_year &
                                                      (all_years$year %% 3) == 2,"year"]) %>%
  mutate(observation_type="Harvest",
         dayofyr=31,
         date=as.Date(dayofyr-1, origin = paste0(year,"-01-01")),
         crop=covercrop_aftercorn)

## soybean following corn/clover
planting_soybean_fut <- data.frame(year=all_years[all_years$year > experiment_end_year &
                                                 (all_years$year %% 3) == 2,"year"]) %>%
  mutate(observation_type="Planting",
         dayofyr=168,
         date=as.Date(dayofyr-1, origin = paste0(year,"-01-01")),
         crop="Soybean")
harvest_soybean_fut <- data.frame(year=all_years[all_years$year > experiment_end_year &
                                                    (all_years$year %% 3) == 2,"year"]) %>%
  mutate(observation_type="Harvest",
         dayofyr=291,
         date=as.Date(dayofyr-1, origin = paste0(year,"-01-01")),
         crop="Soybean")

## wheat following soybean
planting_wheat_fut <- data.frame(year=all_years[all_years$year > experiment_end_year &
                                                 (all_years$year %% 3) == 2,"year"]) %>%
  mutate(observation_type="Planting",
         dayofyr=311,
         date=as.Date(dayofyr-1, origin = paste0(year,"-01-01")),
         crop="Wheat")
harvest_wheat_fut <- data.frame(year=all_years[all_years$year > experiment_end_year &
                                                  (all_years$year %% 3) == 0,"year"]) %>%
  mutate(observation_type="Harvest",
         dayofyr=204,
         date=as.Date(dayofyr-1, origin = paste0(year,"-01-01")),
         crop="Wheat")

## clover following wheat
planting_wheatcc_fut <- data.frame(year=all_years[all_years$year > experiment_end_year &
                                                  (all_years$year %% 3) == 0,"year"]) %>%
  mutate(observation_type="Planting",
         dayofyr=234,
         date=as.Date(dayofyr-1, origin = paste0(year,"-01-01")),
         crop=covercrop_afterwheat)
harvest_wheatcc_fut <- data.frame(year=all_years[all_years$year > experiment_end_year &
                                                        (all_years$year %% 3) == 0,"year"]) %>%
  mutate(observation_type="Harvest",
         dayofyr=300,
         date=as.Date(dayofyr-1, origin = paste0(year,"-01-01")),
         crop=covercrop_afterwheat)

# combine
if(mgmt_scenario_grp!=3) {
  planting_fut <- rbind(planting_corn_fut,planting_soybean_fut,planting_wheat_fut)
  
  harvest_fut <- rbind(harvest_corn_fut,harvest_soybean_fut,harvest_wheat_fut)
} else {
  planting_fut <- rbind(planting_corn_fut,planting_corncc_fut,planting_soybean_fut,
                        planting_wheat_fut,planting_wheatcc_fut)
  
  harvest_fut <- rbind(harvest_corn_fut,harvest_corncc_fut,harvest_soybean_fut,
                       harvest_wheat_fut,harvest_wheatcc_fut)
}

all_field_ops <- rbind(planting_base,harvest_base,
                       planting_fut,harvest_fut) %>%
  select(-dayofyr) %>%
  rbind(field_ops_exp) 
all_field_ops <- all_field_ops[order(all_field_ops$date),]

# join livec with field ops
livec_output <- merge(livec_output, all_field_ops, by=c("date","year"), all=TRUE)


# calculate the % C each day during plant growth

## loop once per plant/harvest cycle
for(i in which(c(1:nrow(all_field_ops))%%2==1)) {
  planting_date <- all_field_ops[i,"date"]
  harvest_date <- all_field_ops[i+1,"date"]
  # get total plant growth for the season (at harvest)
  livec_output[livec_output$date>=planting_date & 
                 livec_output$date<=harvest_date,"tot_NPP"] <- livec_output[livec_output$date==harvest_date, "tot_plt_growth"]
  # calculate the fraction of each day's growth between planting-harvest of total (tot_NPP)
  # need to calculate the amount of each day's growth as change from the day before (because
  # it's cumulative); this is the plant growth curve we need to approximate soil C input
  # throughout the growing season
  livec_output[livec_output$date>=planting_date & 
                 livec_output$date<=harvest_date,"frct_NPP"] <- livec_output[livec_output$date>=planting_date & 
                            livec_output$date<=harvest_date,"daily_NPP"]/livec_output[livec_output$date>=planting_date & 
                                                                                        livec_output$date<=harvest_date,"tot_NPP"]
  # calculate the actual daily C input due to roots/exudates according to clitad(2)
  livec_output[livec_output$date>=planting_date & 
                 livec_output$date<=harvest_date,"clitad_2"] <- lis_output[lis_output$time==all_field_ops[i,"year"],"clitad_2"]
  livec_output[livec_output$date>=planting_date & 
                 livec_output$date<=harvest_date,"daily_soilC_gm2"] <- round(livec_output[livec_output$date>=planting_date & 
                                                                                        livec_output$date<=harvest_date,"frct_NPP"] * 
                                                                           lis_output[lis_output$time==all_field_ops[i,"year"],"clitad_2"],6)
  # jump a record to start at the next planting row
  i <- i+1 
}

## fill in NA with 0
livec_output[is.na(livec_output$daily_soilC),"daily_soilC_gm2"] <- 0.00
livec_output$daily_soilC_Mgha <- round(livec_output$daily_soilC_gm2/100,6)
livec_output$month <- month(livec_output$date)

## fill in crop type from planting-planting
for(i in which(c(1:nrow(all_field_ops[all_field_ops$year<2099,]))%%2==1)) {
  planting_date <- all_field_ops[i,"date"]
  next_planting_date <- all_field_ops[i+2,"date"]
  livec_output[livec_output$date>=planting_date & 
                 livec_output$date<next_planting_date,"crop"] <- all_field_ops[i,"crop"]
}
### clean up the edges
livec_output[livec_output$year==land_conversion_year,"crop"] <- "Maize"
livec_output[livec_output$year == 2099 & is.na(livec_output$crop),"crop"] <- "Wheat"
livec_output <- livec_output[livec_output$year < 2100,]


write.table(livec_output[,c("date","year","dayofyr","month","aglivc","bglivcj","bglivcm","tot_plt_growth",
                            "daily_NPP","observation_type","crop","tot_NPP","frct_NPP","clitad_2",
                            "daily_soilC_gm2","daily_soilC_Mgha")],
            file=paste0(mgmt_path,"Daycent_Cinput_",clim_scenario_num,"_",mgmt_scenario_num,".csv"),
            col.names=c("date","year","dayofyr","month","aglivc","bglivcj","bglivcm","tot_plt_growth",
                        "daily_NPP","observation_type","crop","tot_NPP","frct_NPP","clitad_2",
                        "daily_soilC_gm2","daily_soilC_Mgha")
            ,row.names=F,
            sep=",")

}) # end suppressMessages