#######################################
# Script: 1_Create_weather_input_files-APSIM2.R
# Author: Ellen Maas
# Date: July 11, 2022
# Output: Function doesn't return any data, hence the "p" (procedure) naming
# convention. It creates files in the appropriate folder for each model.
# Description: "This procedure generates weather input files for every model in the 
# format needed by each. There are some gaps of missing data on various days, so 
# it fills in those values with NASA Power data (which includes radiation data). 
# Calls custom nasapower_download function."
#######################################
# Audit Log:
# 2022: Created script.
# 1/31/2023: Modified to use .met files for APSIM Classic.
#######################################





suppressMessages({
  
  print("Starting 1_Create_weather_input_files-APSIM3.R")
  
  if(clim_scenario_num==1) {
    
    # Convert into an APSIM met-formatted file
    APSIM_basic <- as_apsim_met(new_dat[,c("year","dayofyear","radn_MJm2",
                              "maxt_C","mint_C","rain_mm")],
                              filename="basic_wth_exp.met",
                              site=site_name,
                              latitude=latitude,
                              longitude=longitude,
                              )

    # find any columns with NA cells
    try(
      APSIM_basic <- napad_apsim_met(APSIM_basic),
      silent=TRUE
    )
    
  write_apsim_met(APSIM_basic, wrt.dir=apsim_path, filename="basic_wth_exp.met")

    ###########
    
    # baseline future period to 2100
    
    # Select year, dayofyear, radiation (MJ/m^2), maxt, mint, precip (mm)
    APSIM_basic_2100 <- as_apsim_met(new_dat_2100[,c("year","dayofyear","radn_MJm2",
                                                     "maxt_C","mint_C","rain_mm")],
                                     filename="basic_wth_",clim_scenario_num,".met",
                                     site=site_name,
                                     latitude=latitude,
                                     longitude=longitude,
    )
  
  # find any columns with NA cells
  try(
    APSIM_basic_2100 <- napad_apsim_met(APSIM_basic_2100),
    silent=TRUE
  )
  
  write_apsim_met(APSIM_basic, wrt.dir=apsim_path, filename="basic_wth_",clim_scenario_num,".met")
  
  } else if(clim_scenario_num>1) {
    
    fut_dat <- read.csv(file=paste0(fut_weather_path,"fut_clim_scenario_",clim_scenario_num,'.csv'))
    
    # Get experimental period and bind to future
    
    ## Select year, dayofyear, radiation (MJ/m^2), maxt, mint, precip (mm)
    APSIM_basic <- new_dat[,c("year","dayofyear","radn_MJm2.x",
                              "maxt_C.x","mint_C.x","rain_mm.x","month")]
    colnames(APSIM_basic) <- c("year","day","radn","maxt","mint","rain","month")
    APSIM_fut <- fut_dat[,c("year","dayofyear","radn_MJm2",
                            "maxt_C","mint_C","rain_mm","month")]
    colnames(APSIM_fut) <- c("year","day","radn","maxt","mint","rain","month")
    APSIM_basic_esm <- rbind(APSIM_basic,APSIM_fut)
    
    # find any columns with NA cells
    na_find <- names(which(colSums(is.na(fut_dat[c(3:12053),]))>0))
    
  } else { # clim_scenario_num not found
    
    print(paste0("Unknown clim_scenario_num=",clim_scenario_num,"in 1_Create_weather_input_files-APSIM.R"))
    
  } # if clim_scenario_num == 1
  
  rm(list = c("APSIM_basic","APSIM_basic_2100","na_find","latlon_rows","APSIM_fut",
              "fut_dat","APSIM_basic_esm", "monthly_avg","tav","amp"))
  
}) # end suppressMessages