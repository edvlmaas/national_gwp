#######################################
# Script: 3_Create_management_input_files-Millennial2.R
# Author: Ellen Maas
# Date: Sept 23, 2022
# Output: It creates files in the appropriate folder for each model.
# Description: Creates input files with soil temperature, moisture, 
# and daily C input as calculated via Daycent.
#######################################
# Audit Log
# 9/23/2022: Created script.
#######################################

print("Starting 3_Create_management_input_files-Millennial2.R")

## equilibrium

#Because the Millennial model does not use climate data, and soil temperature and
#moisture data at the site are not available daily, the 1-year global average data supplied
#with the model is used for equilibrium and the base period (pre-experimental).


# create one year's worth of input data for spin-up
eq_inputdata <- read.table(paste0(mill_path,"../",mill_init_filename))
# decrease soil temp since air temps on average were lower in 1850
eq_inputdata[1] <- eq_inputdata[1]-0.5

# Alternatively, using the daily percentages in the equilibrium initialization file, assign 
# g C/m^2 per day of the annual total determined by RothC inverse mode to reach 
# equilibrium at this site
equil_C_input_adj <- equil_C_input # try adjusting the C input for calibration
equil_init <- read.table(paste0(mill_path,"../",mill_equilinit_filename),header=T)
eq_inputdata[3] <- round(equil_init*equil_C_input_adj,5)
eq_inputdata[,4:5] <- Cin_daily_gm2[1:365,c("month","day")] # add date elements to help with readability
names(eq_inputdata) <- c("forc_st","forc_sw","forc_npp","month","day") 

# how much total C are we adding, to 1 meter?
eq_annual_C <- sum(eq_inputdata[3])

write.table(eq_inputdata, file=paste0(mill_path,mill_eqilinput_filename),
            row.names=F,col.names = T,append=F)



## base-experimental

# Reuse global average data for each year in the base period up to the experimental
# period for soil temperature and moisture. Annual C input comes from the 
# historical estimates calculated via Bolinder and output from APSIM.
# All years are 365 days.


# start data file for base run with soil temp and moisture from Daycent
mill_inputdata <- merge(Tin_daily_C[,c("date","soil_T")],
                        Min_daily_V[,c("date","soil_M")],
                        by="date") %>%
  merge (Cin_daily_gm2[,c("date","daily_soilC_gm2")],
         by="date") %>%
  mutate(year=year(date))

# increase the C input after land conversion as needed to account for the fact
# that the C input data is coming from Daycent, which is only to 25 cm, but
# Millennial is to 1 m. This adjustment is set purely through trial and error,
# to reproduce historical/experimental measurements.
soc_adj <- if_else(mgmt_scenario_grp==1,2,
           if_else(mgmt_scenario_grp==2,1.2,1))
mill_inputdata$daily_soilC_gm2 <- mill_inputdata$daily_soilC_gm2*soc_adj
base_inputdata = data.frame()

for(i in 1850:1988) { 
  if (i == 1850) { # start with fresh file creation; all other write.table calls appends data
    # need to limit this to 365 records (can't use 366 in leap years)
    base_inputdata <- head(mill_inputdata[mill_inputdata$year==i,
                                               c("soil_T","soil_M","daily_soilC_gm2")],365)
    base_inputdata[,4] <- seq(as.Date(paste0(i,"-01-01")),
                              length.out=365,by="day")
    base_inputdata[,5] <- "corn"
    write.table(base_inputdata, file=paste0(mill_path,mill_baseinput_filename),
                col.names=c("forc_st","forc_sw","forc_npp","date","crop"), 
                row.names=FALSE, sep="\t", 
                quote=FALSE, append=FALSE)
  } # end 1850 loop initialization
  if (i >= 1851 & i <= 1953) { # continuous corn
    base_inputdata[1:3] <- head(mill_inputdata[mill_inputdata$year==i,
                                               c("soil_T","soil_M","daily_soilC_gm2")],365)
    base_inputdata[,4] <- seq(as.Date(paste0(i,"-01-01")),
                              length.out=365,by="day")
    base_inputdata[,5] <- "corn"
    write.table(base_inputdata, file=paste0(mill_path,mill_baseinput_filename),
                col.names=FALSE, row.names=FALSE, sep="\t", 
                quote=FALSE, append=TRUE)
  } #end 1850-1953
  else   if (i >= 1954 & i < experiment_start_year) { # landman alternates 2 years: soybeans and corn, but C distribution is same
    base_inputdata[1:3] <- head(mill_inputdata[mill_inputdata$year==i,
                                               c("soil_T","soil_M","daily_soilC_gm2")],365)
    base_inputdata[,4] <- seq(as.Date(paste0(i,"-01-01")),
                              length.out=365,by="day")
    base_inputdata[,5] <- if ((i %% 2) == 0) "soybeans" else "corn"
    write.table(base_inputdata, file=paste0(mill_path,mill_baseinput_filename),
                col.names=FALSE, row.names=FALSE, sep="\t", 
                quote=FALSE, append=TRUE)
  } #end 1954-one year before start year
} #end 1850-1988

##########################################
####### start experimental period ########
##########################################

exp_inputdata <- mill_inputdata[mill_inputdata$year >= experiment_start_year &
                                  mill_inputdata$year <= experiment_end_year,
                                c("date","soil_T","soil_M","daily_soilC_gm2")] %>% 
  mutate(year=year(date),
         crop=if_else(year >= 1989 & year <= 1994 & (year %% 2)==0, "soybeans",
              if_else(year >= 1989 & year <= 1994 & (year %% 2)==1, "corn",
              if_else(year >= 1995 & year <= 2021 & (year %% 3)==0, "wheat",
              if_else(year >= 1995 & year <= 2100 & (year %% 3)==1, "corn",
                      "soybeans"))))
  )

write.table(exp_inputdata[,c("soil_T","soil_M","daily_soilC_gm2","date","crop")], 
            file=paste0(mill_path,mill_baseinput_filename),
            col.names=FALSE, row.names=FALSE, sep="\t", 
            quote=FALSE, append=TRUE)



## future

fut_inputdata <-mill_inputdata[mill_inputdata$year > experiment_end_year,
                               c("date","soil_T","soil_M","daily_soilC_gm2")]  %>%
  mutate(year=year(date),
         crop=if_else((year %% 3)==0, "wheat",
              if_else((year %% 3)==1, "corn",
              "soybeans"))
  )


write.table(fut_inputdata[,c("soil_T","soil_M","daily_soilC_gm2","date","crop")], 
            file=paste0(mill_path,mill_futinput_filename,
                        "_",clim_scenario_num,"_",mgmt_scenario_num,".txt"),
            col.names=c("forc_st","forc_sw","forc_npp","date","crop"),
            row.names=FALSE, sep="\t", 
            quote=FALSE, append=FALSE)


# #*******************************************************************
# ### For testing
# #*******************************************************************
# 
#
# plot(mill_inputdata$year,mill_inputdata$soil_T)
# plot(mill_inputdata$year,mill_inputdata$soil_M)
# plot(mill_inputdata$year,mill_inputdata$daily_soilC_gm2)
#
# Cin_daily <- read.csv(file=paste0(mgmt_path,"Daycent_Cinput_",scenario_name,".csv")) %>%
#   mutate(date=as.Date(date))
# plot(Cin_daily$date,Cin_daily$daily_soilC_gm2,data=Cin_daily)
# 
# Tin_daily_C <-   merge(ObsTemp,
#                        DayT_C[,c("date","year","mean_3_4")],
#                        by=c("date","year"),
#                        all=TRUE) %>%
#   mutate(soil_T=ifelse(is.na(soil_temperature),mean_3_4,soil_temperature))
# plot(Tin_daily_C$year,Tin_daily_C$mean_3_4)
# 
# 
# eq_in <- read.delim(file=paste0(mill_path,"siteenviron_eq_in.txt"),sep=" ")
# eq_in$dayofyr <- 1:365
# plot(eq_in$dayofyr,eq_input$forc_npp)
# plot(eq_in$dayofyr,eq_input$forc_sw)
# plot(eq_in$dayofyr,eq_input$forc_st)
# 
# base_in <- read.delim(file=paste0(mill_path,mill_baseinput_filename),sep="\t")
# base_in <- base_in[,1:4]
# base_in$date <- as.Date(base_in$date)
# plot(as.Date(base_in$date),base_in$forc_npp)
# plot(as.Date(base_in$date),base_in$forc_sw)
# plot(as.Date(base_in$date),base_in$forc_st)
# 
# exp_in <- merge(Tin_daily_C[Tin_daily_C$year %in% experiment_year_range,c("date","soil_T")],
#                 Min_daily_V[Min_daily_V$year %in% experiment_year_range,c("date","soil_M")],
#                 by="date") %>%
#   merge (Cin_daily_gm2[Cin_daily_gm2$year %in% experiment_year_range,c("date","daily_soilC_gm2")],
#          by="date")
# exp_in <- exp_in[exp_in$date<"2021-01-01",c(2,3,4,1)]
# exp_in$date <- as.Date(exp_in$date)
# colnames(exp_in) <- c("forc_st","forc_sw","forc_npp","date")
# plot(exp_in$date,exp_in$forc_npp)
# plot(exp_in$date,exp_in$forc_sw)
# plot(exp_in$date,exp_in$forc_st)
# 
# full_in <- rbind(base_in,exp_in)
# full_in <- full_in[full_in$date>="1980-01-01",]
# plot(as.Date(full_in$date),full_in$forc_npp)
# plot(as.Date(full_in$date),full_in$forc_sw)
# plot(as.Date(full_in$date),full_in$forc_st)
# 

