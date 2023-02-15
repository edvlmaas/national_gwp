setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(readr)
library(magrittr)
library(dplyr)

bd_raw<-read_csv("wosis_latest_bd.csv",show_col_types = FALSE)
sand_raw<-read_csv("wosis_latest_sand.csv",show_col_types = FALSE)
silt_raw<-read_csv("wosis_latest_silt.csv",show_col_types = FALSE)
clay_raw<-read_csv("wosis_latest_clay.csv",show_col_types = FALSE)
oc_raw<-read_csv("wosis_latest_orgc.csv",show_col_types = FALSE)
ph_raw<-read_csv("wosis_latest_ph.csv",show_col_types = FALSE)
tc_raw<-read_csv("wosis_latest_totc.csv",show_col_types = FALSE)

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
              
              
# in order to get manual data into APSIM, will need to pull down a SSURGO
# record for the format, then update all the values
library(apsimx)
sp <- get_isric_soil_profile(lonlat = c(-101.8211, 33.69083))


setwd(master_path)
