#######################################
# File: "3_Create_additional_files-RothC2.R"
# Author: "Ellen Maas"
# Date: "Nov 7, 2022"
# Description: This script creates the schedule files for RothC."
#
#### Assumes that the weather files have already been generated. ####
#
#######################################
# Calls:
#
#######################################
# Audit Log
# 11/7/2022: Replaced APSIM with Daycent as source of C. No longer need to
#            calculate the C input by month, as it's summarized from Daycent.
#
#######################################

print("Starting 3_Create_additional_files-RothC2.R")

library(readxl)
library(dplyr)

# import observations and global constants


# local constants

spth <- paste0("RothC/",site_name,"/scenario/")
lpth <- paste0("RothC/",site_name,"/landman/")

# scenario files

# create one management file for weather and management scenario: 
#c=1 is baseline climate
#c=2 is high emissions climate
#m=1 is baseline management (conventional tillage, conventional fertilizer)
#m=2 is no tillage
#m=3 incorporates cover crops

  # set file names
  scenario_file <- paste(scenario_name,".SET",sep="")
  output_file <- scenario_name # RothC appends ".263" to this for "output" and "graph" folders
  scenario_path <- paste(spth,scenario_file, sep="")
  weather_eql <- "Eqil"
  weather_eql_dat <- paste(weather_eql,".dat",sep="")
  landman_eql_file <- "Eqil.dat"
  ### Equilibrium segment, to 1850
  cat(output_file,weather_eql,"2","0","0","0","0","5.2059","0","0","0","0","2",landman_eql_file,
      "2","1","2\n", file=scenario_path, sep="\n",append=FALSE)
  ### "Short term" segments, annual through 2100
  for(k in land_conversion_year:(2100-1)) { 
    if (k == land_conversion_year) # first short term segment uses same eql weather file, landman alternates even years corn, odd years wheat
      cat("1","2","1","2","2","1",paste(substr(as.character(k),2,4),"_",scenario_name,".dat",sep=""),"1850",
          "1","1","0","2\n", file=scenario_path, sep="\n", append=TRUE)
    else if (k >= 1851 & k <= 1899) # uses same eql weather file, landman alternates even years corn, odd years wheat
      if ((k %% 2) == 0) { # even years
        cat("1","2","1","1",paste(substr(as.character(k),2,4),"_",scenario_name,".dat",sep=""),
            "1","1","0","2\n", file=scenario_path, sep="\n", append=TRUE)
      } else { # odd years
        cat("1","2","1","1",paste(substr(as.character(k),2,4),"_",scenario_name,".dat",sep=""),
            "1","1","0","2\n", file=scenario_path, sep="\n", append=TRUE)
      }
    else if (k >= 1900 & k <= 1953)  # new weather files each year, landman alternates even years corn, odd years wheat
      if((k %% 2) == 0) { # even years
        cat("2","2",paste(clim_scenario_num,substr(as.character(k),2,4), sep=""),
            "1","1",paste(substr(as.character(k),2,4),"_",scenario_name,".dat",sep=""),
            "1","1","0","2\n", file=scenario_path, sep="\n", append=TRUE)
      } else { # odd years
        cat("2","2",paste(clim_scenario_num,substr(as.character(k),2,4), sep=""),
            "1","1",paste(substr(as.character(k),2,4),"_",scenario_name,".dat",sep=""),
            "1","1","0","2\n", file=scenario_path, sep="\n", append=TRUE)
      }
    else if (k >= 1954 & k <= 1988) # new weather files each year, landman alternates even years soybean, odd years corn
      if((k %% 2) == 0) { # soybeans even years
        cat("2","2",paste(clim_scenario_num,substr(as.character(k),2,4), sep=""),
            "1","1",paste(substr(as.character(k),2,4),"_",scenario_name,".dat",sep=""),
            "1","1","0","2\n", file=scenario_path, sep="\n", append=TRUE)
      } else { # corn odd years
        cat("2","2",paste(clim_scenario_num,substr(as.character(k),2,4), sep=""),
            "1","1",paste(substr(as.character(k),2,4),"_",scenario_name,".dat",sep=""),
            "1","1","0","2\n", file=scenario_path, sep="\n", append=TRUE)
      }
    
    ######################################
    ## start experimental period
    ######################################
    
    else if (k >= 1989 & k <= 1994) # new weather files each year, landman alternates even years soybean, odd years corn
      if((k %% 2) == 0) { # soybeans even years
        cat("2","2",paste(clim_scenario_num,substr(as.character(k),2,4), sep=""),
            "1","1",paste(substr(as.character(k),2,4),"_",scenario_name,".dat",sep=""),
            "1","1","0","2\n", file=scenario_path, sep="\n", append=TRUE)
      } else { # corn odd years
        cat("2","2",paste(clim_scenario_num,substr(as.character(k),2,4), sep=""),
            "1","1",paste(substr(as.character(k),2,4),"_",scenario_name,".dat",sep=""),
            "1","1","0","2\n", file=scenario_path, sep="\n", append=TRUE)
      }
    
    else if (k >= 1995 & k <= 2021) # new weather files each year, landman alternates wheat, corn, soybean
      if((k %% 3) == 0) { # wheat
        cat("2","2",paste(clim_scenario_num,substr(as.character(k),2,4), sep=""),
            "1","1",paste(substr(as.character(k),2,4),"_",scenario_name,".dat",sep=""),
            "1","1","0","2\n", file=scenario_path, sep="\n", append=TRUE)
      } else if((k %% 3) == 1) { # corn
        cat("2","2",paste(clim_scenario_num,substr(as.character(k),2,4), sep=""),
            "1","1",paste(substr(as.character(k),2,4),"_",scenario_name,".dat",sep=""),
            "1","1","0","2\n", file=scenario_path, sep="\n", append=TRUE)
      } else { # soybeans
        cat("2","2",paste(clim_scenario_num,substr(as.character(k),2,4), sep=""),
            "1","1",paste(substr(as.character(k),2,4),"_",scenario_name,".dat",sep=""),
            "1","1","0","2\n", file=scenario_path, sep="\n", append=TRUE)
      }

    ######################################
    ## start experimental period
    ######################################
    
    else if (k >= 2022 & k <= 2100-2) # future scenarios, new weather and landman files each year
      if((k %% 3) == 0) { # wheat
        cat("2","2",paste(clim_scenario_num,substr(as.character(k),2,4), sep=""),
            "1","1",paste(substr(as.character(k),2,4),"_",scenario_name,".dat",sep=""),
            "1","1","0","2\n", file=scenario_path, sep="\n", append=TRUE)
      } else if((k %% 3) == 1) { # corn
        cat("2","2",paste(clim_scenario_num,substr(as.character(k),2,4), sep=""),
            "1","1",paste(substr(as.character(k),2,4),"_",scenario_name,".dat",sep=""),
            "1","1","0","2\n", file=scenario_path, sep="\n", append=TRUE)
      } else { # soybeans
        cat("2","2",paste(clim_scenario_num,substr(as.character(k),2,4), sep=""),
            "1","1",paste(substr(as.character(k),2,4),"_",scenario_name,".dat",sep=""),
            "1","1","0","2\n", file=scenario_path, sep="\n", append=TRUE)
      }
    else { # we've reached 2099, so last year needs end of file indicator ("3" below)
      cat("2","2",paste(clim_scenario_num,substr(as.character(k),2,4), sep=""),
          "1","1",paste(substr(as.character(k),2,4),"_",scenario_name,".dat",sep=""),
          "1","1","0","3", file=scenario_path, sep="\n", append=TRUE)
    }
  } # end for land_conversion_year-2100
  