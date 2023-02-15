# ---
# title: "KBS"
# author: "Ellen Maas"
# date: "9/6/2022"
# output:
#   html_notebook: default
#   html_document:
#     df_print: paged
#   pdf_document: default
# ---

print("Starting run_Millennial.R")

library(FME)
library(dplyr)
library(lubridate)

# Set the model path to the location of files
setwd(mill_path)


#*************************************************************

# local constants

meas_initC <- 9500 # g C/m^2
#Define path to the Millennial directory
rootdir <- "../../../"
# the base scenario name reflects the experimental treatment that the future
# scenario is extending (such as conventional tillage)
base_scenario_name <- if_else(mgmt_scenario_num==1 | mgmt_scenario_grp %in% c(4:7), 
                              "1_1",
                      if_else(mgmt_scenario_num==2, "1_2",
                      if_else(mgmt_scenario_num==3, "1_3",
                      "Error")))

#*************************************************************

#Read in equilibrium input data - Described in Table A1 of Abramoff et al. (2021)
##forc_st: soil temperature in degrees Celsius
##forc_sw: volumetric soil moisture in mm3/mm3
##forc_npp: daily plant inputs in gC/m2/day
equil_input_raw <- read.table("siteenviron_eq_in.txt",header=T)
equil_inputdata <- select(equil_input_raw,-c("month","day"))

#Read in base-experimental period data
base_input_raw <- read.table("siteenviron_base_in.txt",header=T)
base_inputdata <- select(base_input_raw,-c("date","crop"))

#Read in future scenario period data, using actual scenario input file
scen_input_raw <- read.table(paste0("siteenviron_in_",scenario_name,".txt"),header=T)
scen_inputdata <- select(scen_input_raw,-c("date","crop"))

#Read in parameters, using experimental data the scenario is based on
##Described in Table A1 of Abramoff et al. (2021)
parameters.file <- read.table(paste0("soilparams_in_",base_scenario_name,".txt"))
parameters <- as.list(parameters.file$V2)
names(parameters) <- parameters.file$V1

#Define site-level parameters
## add site attributes to params 
site_parameters.file <- read.table(file="siteparams_in.txt")
site_parameters <- as.list(site_parameters.file$V2)
names(site_parameters) <- site_parameters.file$V1

parameters <- c(parameters,site_parameters)

#Read in functions
source("run_functions.R") #R script that contains calls to run model
source(paste0(rootdir, "R/models/derivs_V2_MM.R")) #The official version of Millennial V2


#*************************************************************

# Run model
## Equilibrium

#### Solve a steady state for spin-up purposes

##This function takes as arguments:
##1 input data
##2 model equations
##3 parameters
##4 whether or not to calculate the eigenvalues (optional; default = 0; 0 = no, 1 = yes)
##5 initial states of pools (optional; default = 1 for all pools and 0 for CO2 flux)
steadystate_raw <- Solve_Model(equil_inputdata,derivs_V2_MM,parameters,calc_eigens=1)

## because the Millennial model calculates C to 1 m (but does not differentiate
## between layers), reduce the total C to 69% to reflect the stratification of
## SOC in the top 0-25 cm layer (though Poeplau et al. 2020 note it is more like
## 53%)

steadystate <- steadystate_raw
#steadystate$y <- steadystate_raw$y * 0.69

summary_ss <- steadystate$y
summary_ss["TOTC"] <- sum(steadystate$y)
calc_initC <- sum(steadystate$y)

write.table(as.data.frame(summary_ss), file="steadystate.txt")


#*************************************************************

## Base-experimental

### Run the "official" version of the Millenial V2 model, which uses Michaelis-Menten 
### kinetics. The name of the function that holds these equations is *derivs_V2_MM.R* 
### which we imported earlier. 

### If we want to use the steady states calculated above to speed up the spin-up 
### of the model, we just replace the state variable arguments to Run_Model with 
### steadystate$y. 

#Run the model
##This function takes as arguments:
##1 input data
##2 model equations
##3 parameters
##4 [EDM modified] number of times to repeat the data brought in as inputs. For example, if 
##  data are one year's worth of data and you want to run the model on the same
##  data repeated 10 times, then repeat.times = 10, but if every year to be simulated
##  is already represented in the input data, then repeat.times = 1. (optional; default = 100)
##5 initial states of pools (optional; default = 1 for all pools and 0 for CO2 flux)

#steadystate <- read.delim("steadystate.txt",sep=" ")

base_output <- as.data.frame(Run_Model(base_inputdata,derivs_V2_MM,parameters,
                                  repeat.times=1,
                                  state=c(steadystate$y, CO2=0)))
base_output$year <- year(base_input_raw$date)
base_output$date <- base_input_raw$date
base_output$TOC <- base_output$POM+base_output$LMWC+base_output$AGG+base_output$MIC+base_output$MAOM

write.csv(base_output, file=paste0("base_out_",scenario_name,".csv"),row.names = F)


#*************************************************************

### Plot model results

#Define function which plots the evolution of Millennial model pools over time
plot.pools <- function(output){
plot(output[,"year"],output[,"POM"], xlab="year", ylab="pool (gC m-2)", col=1, 
     type="l", ylim=c(0,max(output[,-1], na.rm=T)))
lines(output[,"year"],output[,"LMWC"], col=2)
lines(output[,"year"],output[,"AGG"], col=3)
lines(output[,"year"],output[,"MIC"], col=4)
lines(output[,"year"],output[,"MAOM"], col=5)
legend("topleft", c("POM","LMWC","AGG","MIC","MAOM"), col=1:5, lwd=1)
}

#Define function which plots the modeled CO2 emissions over time
##CO2 emissions are cumulative, so we have to take the difference between 
##timesteps to plot the CO2 emission rate
plot.co2 <- function(base_output){
plot(base_output[1:(dim(base_output)[1]-1),"year"], diff(base_output[,"CO2"]), 
     xlab="year", ylab="co2 (gC m-2 d-1)", col=1, type="l")
}

#Make the plots
plot.pools(base_output[,c("year","POM","LMWC","AGG","MIC","MAOM")])
plot.co2(base_output)
plot(base_output$year,base_output$TOC, xlab="year", ylab="pool (gC m-2)", 
     col=1, type="l", ylim=c(0,max(base_output$TOC, na.rm=T)))


#*************************************************************

## Future scenario

### Use the state variable arguments from the last record in base_output.


### save end state of last segment as a named numeric array
basestate <- as.numeric(tail(base_output[,c("POM","LMWC","AGG","MIC","MAOM")],n=1))
names(basestate) <- c("POM","LMWC","AGG","MIC","MAOM")

scen_output <- as.data.frame(Run_Model(scen_inputdata,derivs_V2_MM,parameters,
                                  repeat.times=1,
                                  state=c(basestate, CO2=0)))
scen_output$year <- year(scen_input_raw$date)
scen_output$date <- scen_input_raw$date
scen_output$TOC <- scen_output$POM+scen_output$LMWC+scen_output$AGG+scen_output$MIC+scen_output$MAOM

write.csv(scen_output, file=paste0("scenario_out_",scenario_name,".csv"),row.names = F)


#*************************************************************

### Plot


### Make the plots
plot.pools(scen_output[,c("year","POM","LMWC","AGG","MIC","MAOM")])
plot.co2(scen_output)
plot(scen_output$year,scen_output$TOC, xlab="year", ylab="pool (gC m-2)",
     col=1, type="l", ylim=c(0,max(scen_output$TOC, na.rm=T)))



#*************************************************************

# Set the model path back to the master folder
setwd(master_path)

