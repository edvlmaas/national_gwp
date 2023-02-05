#######################################
# Function: "3_Create_management_input_files-Daycent4.R"
# Author: "Ellen Maas"
# Date: "Sept 23, 2022"
# Output: .sch files for the scenario number.
# Description: This procedure generates scenario file for each phase of
# Daycent processing: base, experimental, and future periods. Weather
# and management are tailored to each scenario."
#######################################
# Audit Log
# 9/30/2022: Modified to include T3 data for scenario 3.
# 10/3/2022: Changed hairy vetch crop type to white clover
# 12/13/2022: Added climate scenario number to future weather file name.
#######################################

print("Starting 3_Create_management_input_files-Daycent4.R")


###########################
# Daycent 
###########################

## equilibrium

#Note: Daycent spin-up schedule file was assembled manually: sched_eq.sch

## base

schedule_file <- paste0(daycent_path,"sched_base.sch")

### 1850-1875 - start with continuous C1 corn ###

yrs_1850to1875 <- c("1850          Starting year ## start with assumed ground-breaking for agriculture until intensification",
                    "1988          Last year",
                    "site.100  Site file name",
                    "0             Labeling type ## all defaults turned off",
                    "-1            Labeling year",
                    "-1.00         Microcosm",
                    "-1            CO2 Systems",
                    "-1            pH effect",
                    "-1            Soil warming",
                    "0             N input scalar option (0 or 1)",
                    "0             OMAD scalar option (0 or 1)",
                    "0             Climate scalar option",
                    "1             Initial system",
                    "C1            Initial crop ## low-yield corn",
                    "              Initial tree",
                    "",
                    "Year Month Option",
                    "1       Block ## Corn, low yield, no fertilizer",
                    "1875    Last year",
                    "1       Repeats # of years",
                    "1850    Output starting year",
                    "12       Output month",
                    "1  Output interval",
                    "F       Weather choice",
                    "basic_eq.wth",
                    "1 111 CULT K     ## April 21 Moldboard plow",
                    "1 121 CULT H     ## May 1 Disk",
                    "1 121 CULT D     ## May 1 Cultivate",
                    "1 124 CROP C1    ## May 4",
                    "1 124 PLTM       ## May 4 Plant corn",
                    "1 177 FERT (0.75N)	## June 26",
                    "1 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw",
                    "-999 -999 X")

writeLines(yrs_1850to1875,schedule_file)

### 1875-1900 - corn-soybean 2-year rotation, with gradually increasing productivity (in cultivars)

yrs_1875to1900 <- c("2       Block ## Higher-yielding corn with fertilizer",
                    "1900    Last year",
                    "1       Repeats # of years",
                    "1876    Output starting year",
                    "12       Output month",
                    "1  Output interval",
                    "C       Weather choice ## Continue",
                    "1 111 CULT K     ## April 21 Moldboard plow",
                    "1 121 CULT H     ## May 1 Disk",
                    "1 121 CULT D     ## May 1 Cultivate",
                    "1 124 CROP C3    ## May 4",
                    "1 124 PLTM       ## May 4",
                    "1 177 FERT (1.5N)	## June 26",
                    "1 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw",
                    "-999 -999 X")

cat(yrs_1875to1900,sep="\n",file=schedule_file,append=TRUE)

### 1901-1920 - corn-soybean 2-year rotation, with gradually increasing productivity (in cultivars)

yrs_1901to1920 <- c("3       Block ## Higher-yielding corn with fertilizer",
                    "1920    Last year",
                    "1       Repeats # of years",
                    "1901    Output starting year",
                    "12       Output month",
                    "1  Output interval",
                    "C       Weather choice ## Continue",
                    "1 111 CULT K     ## April 21 Moldboard plow",
                    "1 121 CULT H     ## May 1 Disk",
                    "1 121 CULT D     ## May 1 Cultivate",
                    "1 124 CROP C3    ## May 4",
                    "1 124 PLTM       ## May 4",
                    "1 177 FERT (2.2N)	## June 26",
                    "1 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw",
                    "-999 -999 X")

cat(yrs_1901to1920,sep="\n",file=schedule_file,append=TRUE)

### 1921-1949 - corn-soybean 2-year rotation, with gradually increasing productivity (in cultivars)

yrs_1921to1949 <- c("4       Block ## Higher-yielding corn with fertilizer",
                    "1949    Last year",
                    "1       Repeats # of years",
                    "1921    Output starting year",
                    "12       Output month",
                    "1  Output interval",
                    "C       Weather choice ## Continue",
                    "1 111 CULT K     ## April 21 Moldboard plow",
                    "1 121 CULT H     ## May 1 Disk",
                    "1 121 CULT D     ## May 1 Cultivate",
                    "1 124 CROP C4    ## May 4",
                    "1 124 PLTM       ## May 4",
                    "1 177 FERT (2.2N)	## June 26",
                    "1 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw",
                    "-999 -999 X")

cat(yrs_1921to1949,sep="\n",file=schedule_file,append=TRUE)

### 1950-1988 - corn-soybean 2-year rotation, with gradually increasing productivity (in cultivars)

yrs_1950to1959 <- c("5       Block ## Higher-yielding corn with fertilizer, add soybean to rotation",
                    "1959    Last year",
                    "2       Repeats # of years",
                    "1950    Output starting year",
                    "12       Output month",
                    "1  Output interval",
                    "F       Weather choice ## Start weather over at 1950",
                    "basic_eq.wth",
                    "1 138 CULT K     ## May 18 Moldboard plow",
                    "1 145 CULT H     ## May 25 Disc",
                    "1 145 CULT D     ## May 25 Cultivate",
                    "1 149 CROP SYBN3 ## May 29",
                    "1 149 PLTM       ## May 29 Plant soybean",
                    "1 290 HARV G90S  ## Oct 17 - Harvest grains and 90% straw",
                    "2 111 CULT K     ## April 21 Moldboard plow",
                    "2 121 CULT H     ## May 1 Disk",
                    "2 121 CULT D     ## May 1 Cultivate",
                    "2 124 CROP C4    ## May 4",
                    "2 124 PLTM       ## May 4",
                    "2 177 FERT (3.5N)	## June 26",
                    "2 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw",
                    "-999 -999 X")

cat(yrs_1950to1959,sep="\n",file=schedule_file,append=TRUE)

yrs_1960to1969 <- c("6       Block ## Higher-yielding corn with fertilizer, continue soybean",
                    "1969    Last year",
                    "2       Repeats # of years",
                    "1960    Output starting year",
                    "12       Output month",
                    "1  Output interval",
                    "C       Weather choice ## Continue weather from previous block",
                    "1 138 CULT K     ## May 18 Moldboard plow",
                    "1 145 CULT H     ## May 25 Disc",
                    "1 145 CULT D     ## May 25 Cultivate",
                    "1 149 CROP SYBN3 ## May 29",
                    "1 149 PLTM       ## May 29 Plant soybean",
                    "1 290 HARV G90S  ## Oct 17 - Harvest grains and 90% straw",
                    "2 111 CULT K     ## April 21 Moldboard plow",
                    "2 121 CULT H     ## May 1 Disk",
                    "2 121 CULT D     ## May 1 Cultivate",
                    "2 124 CROP C6    ## May 4",
                    "2 124 PLTM       ## May 4",
                    "2 177 FERT (4.5N)	## June 26",
                    "2 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw",
                    "-999 -999 X")

cat(yrs_1960to1969,sep="\n",file=schedule_file,append=TRUE)

yrs_1970to1979 <- c("7       Block ## Higher-yielding corn with fertilizer, continue soybean",
                    "1979    Last year",
                    "2       Repeats # of years",
                    "1970    Output starting year",
                    "12       Output month",
                    "1  Output interval",
                    "C       Weather choice ## Continue weather from previous block",
                    "1 138 CULT K     ## May 18 Moldboard plow",
                    "1 145 CULT H     ## May 25 Disc",
                    "1 145 CULT D     ## May 25 Cultivate",
                    "1 149 CROP SYBN2 ## May 29",
                    "1 149 PLTM       ## May 29 Plant soybean",
                    "1 290 HARV G90S  ## Oct 17 - Harvest grains and 90% straw",
                    "2 111 CULT K     ## April 21 Moldboard plow",
                    "2 121 CULT H     ## May 1 Disk",
                    "2 121 CULT D     ## May 1 Cultivate",
                    "2 124 CROP C6    ## May 4",
                    "2 124 PLTM       ## May 4",
                    "2 177 FERT (6N)	## June 26",
                    "2 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw",
                    "-999 -999 X")

cat(yrs_1970to1979,sep="\n",file=schedule_file,append=TRUE)

yrs_1980to1987 <- c("8       Block ## Higher-yielding corn with fertilizer, continue soybean",
                    "1987    Last year",
                    "2       Repeats # of years",
                    "1980    Output starting year",
                    "12       Output month",
                    "1  Output interval",
                    "C       Weather choice ## Continue weather from previous block",
                    "1 138 CULT K     ## May 18 Moldboard plow",
                    "1 145 CULT H     ## May 25 Disc",
                    "1 145 CULT D     ## May 25 Cultivate",
                    "1 149 CROP SYBN2 ## May 29",
                    "1 149 PLTM       ## May 29 Plant soybean",
                    "1 290 HARV G90S  ## Oct 17 - Harvest grains and 90% straw",
                    "2 111 CULT K     ## April 21 Moldboard plow",
                    "2 121 CULT H     ## May 1 Disk",
                    "2 121 CULT D     ## May 1 Cultivate",
                    "2 124 CROP C6    ## May 4",
                    "2 124 PLTM       ## May 4",
                    "2 177 FERT (8N)	## June 26",
                    "2 296 HARV G90S	## Oct 23 - Harvest grains and 90% straw",
                    "-999 -999 X")

cat(yrs_1980to1987,sep="\n",file=schedule_file,append=TRUE)

yrs_1988 <- c("9       Block ## Just soybean",
                    "1988    Last year",
                    "1       Repeats # of years",
                    "1988    Output starting year",
                    "12       Output month",
                    "1  Output interval",
                    "C       Weather choice ## Continue weather from previous block",
                    "1 138 CULT K     ## May 18 Moldboard plow",
                    "1 145 CULT H     ## May 25 Disc",
                    "1 145 CULT D     ## May 25 Cultivate",
                    "1 149 CROP SYBN1 ## May 29",
                    "1 149 PLTM       ## May 29 Plant soybean",
                    "1 290 HARV G90S  ## Oct 17 - Harvest grains and 90% straw",
                    "-999 -999 X")

cat(yrs_1988,sep="\n",file=schedule_file,append=TRUE)



###########################
## Daycent experimental



schedule_file_exp <- paste0(daycent_path,"sched_exp_",scenario_name,".sch")

# remove duplicate/NA records
Daycent_data <- full_ops_ext_adj[!is.na(full_ops_ext_adj$daycent_mgmt_code),]

# Scenario treatment
temp_conv <- Daycent_data[Daycent_data$treatment==treatment,]

Daycent_conv <- temp_conv[rowSums(is.na(temp_conv)) != ncol(temp_conv),] %>%
  mutate(dayofyear=yday(date))

block_num <- 10

fileheader_txt <- c("1989          Starting year ## start of experimental period",
                    "2021          Last year",
                    "site.100  Site file name",
                    "0             Labeling type ## all defaults turned off",
                    "-1            Labeling year",
                    "-1.00         Microcosm",
                    "-1            CO2 Systems",
                    "-1            pH effect",
                    "-1            Soil warming",
                    "0             N input scalar option (0 or 1)",
                    "0             OMAD scalar option (0 or 1)",
                    "0             Climate scalar option",
                    "1             Initial system",
                    "SYBN1         Initial crop ## soybean",
                    "              Initial tree",
                    "",
                    "Year Month Option")

cat(fileheader_txt,sep="\n",file=schedule_file_exp,append=FALSE)  

for (i in experiment_start_year:experiment_end_year) {
  ## first year starts weather file; subsequent years continue weather file
  if (i==experiment_start_year) {
    curr_yr_ops <- Daycent_conv[Daycent_conv$year==i,c("date","daycent_mgmt_code","dayofyear")] %>%
      mutate(ops_line=paste0("1 ",dayofyear," ",daycent_mgmt_code))
    
    header_txt <- c(
      paste(block_num,"Block ## Experimental period",sep="\t"),
      paste(i,"Last year",sep="\t"),
      "1  Repeats # of years",
      paste(i,"Output starting year",sep="\t"),
      "12  Output starting month",
      "1  Output interval",
      "F 			 Weather choice",
      "basic_exp.wth")
    
    ops_txt <- curr_yr_ops$ops_line
    
    footer_txt <- "-999 -999 X"
    
    block_txt <- c(header_txt, ops_txt, footer_txt)  
    
    cat(block_txt,sep="\n",file=schedule_file_exp,append=TRUE)  
    block_num <- block_num + 1  
  } # if year=experiment_start_year
  else {
    curr_yr_ops <- Daycent_conv[Daycent_conv$year==i,c("date","daycent_mgmt_code","dayofyear")] %>%
      mutate(ops_line=paste0("1 ",dayofyear," ",daycent_mgmt_code))
    
    header_txt <- c(
      paste(block_num,"Block ## Experimental period",sep="\t"),
      paste(i,"Last year",sep="\t"),
      "1  Repeats # of years",
      paste(i,"Output starting year",sep="\t"),
      "12  Output starting month",
      "1  Output interval",
      "C 			 Weather choice")
    
    ops_txt <- curr_yr_ops$ops_line
    
    footer_txt <- "-999 -999 X"
    
    block_txt <- c(header_txt, ops_txt, footer_txt)  
    
    cat(block_txt,sep="\n",file=schedule_file_exp,append=TRUE)  
    block_num <- block_num + 1  
  } # else year is not 1989
  
} # for loop 1989-2021



## future


###########################
## Daycent future

# baseline climate

schedule_file_2100 <- paste0(daycent_path,"sched_fut_",scenario_name,".sch")

Daycent_conv_2100 <- Daycent_conv[Daycent_conv$year %in% 2019:2021,
                                c("date","daycent_mgmt_code","dayofyear")]  %>%
  mutate(
    # daycent_mgmt_code=if_else(daycent_mgmt_code=="CROP C8","CROP C7",
    #                                if_else(daycent_mgmt_code=="CROP W4EG","CROP W3EG",
    #                                        if_else(daycent_mgmt_code=="CROP SYBN","CROP SYBN1",
    #                                                daycent_mgmt_code))),
    ops_line=paste0(if_else(year(date)==2019,"1 ",
                    if_else(year(date)==2020,"2 ",
                                             "3 ")),dayofyear," ",daycent_mgmt_code))

header_txt <- c("2022          Starting year ## start with assumed ground-breaking for agriculture until intensification",
                "2099          Last year",
                "site.100      Site file name",
                "0             Labeling type ## all defaults turned off",
                "-1            Labeling year",
                "-1.00         Microcosm",
                "-1            CO2 Systems",
                "-1            pH effect",
                "-1            Soil warming",
                "0             N input scalar option (0 or 1)",
                "0             OMAD scalar option (0 or 1)",
                "0             Climate scalar option",
                "1             Initial system",
                "W4EG          Initial crop ## custom enhanced-growth wheat",
                "              Initial tree",
                "",
                "Year Month Option",
                "1       Block ## Corn, low yield, no fertilizer",
                "2099    Last year",
                "3       Repeats # of years",
                "2022    Output starting year",
                "12       Output month",
                "1  Output interval",
                "F       Weather choice",
                paste0("basic_",clim_scenario_num,".wth"))

ops_txt <- if(mgmt_scenario_grp==3) { 
  c(
    "1 85 FERT (3.4N)",
    "1 98 FERT (2.3N)",
    "1 98 FERT (5.8P)",
    "1 112 FERT (0.1N)",
    "1 126 FERT (3N)",
    "1 205 HARV G75S",
    "1 224 FERT (0.1N)",
    paste0("1 234 CROP ",covercrop_afterwheat_Daycent),
    "1 234 PLTM",
    "2 31 HARV KILL",
    "2 128 CULT K",
    "2 144 CULT D",
    "2 147 FERT (3.4N)",
    "2 147 FERT (3.4P)",
    "2 147 CROP C8",
    "2 147 PLTM",
    "2 177 FERT (0.1N)",
    "2 303 HARV G75S",
    paste0("2 321 CROP ",covercrop_aftercorn_Daycent),
    "2 321 PLTM",
    "3 32 HARV KILL",
    "3 128 CULT K",
    "3 130 FERT (1N)",
    "3 130 FERT (2.7P)",
    "3 139 CULT D",
    "3 168 CROP SYBN",
    "3 168 PLTM",
    "3 175 FERT (0.1N)",
    "3 292 HARV G75S",
    "3 311 CROP W4EG",
    "3 311 PLTM"
  )
} else {
  c(
    paste0("1 85 FERT (",round(3.4*fert_adjust,2),"N)"),
    paste0("1 87 FERT (",round(2.4*fert_adjust,2),"N)"),
    paste0("1 87 FERT (",round(5*fert_adjust,2),"P)"),
    paste0("1 107 FERT (",round(0.1*fert_adjust,2),"N)"),
    paste0("1 125 FERT (",round(1*fert_adjust,2),"N)"),
    paste0("1 125 FERT (",round(3.6*fert_adjust,2),"N)"),
    paste0("1 205 HARV G",resid_adjust_chr,"S"),
    paste0("1 225 FERT (",round(0.1*fert_adjust,2),"N)"),
    paste0("1 291 FERT (",round(0.1*fert_adjust,2),"N)"),
    if_else(mgmt_scenario_grp==5, paste0("2 108 CULT H",resid_adjust_chr),
            "2 108 CULT H"),
    if_else(mgmt_scenario_grp==5, paste0("2 117 CULT D",resid_adjust_chr),
            "2 117 CULT D"),
    paste0("2 147 FERT (",round(3.3*fert_adjust,2),"N)"),
    paste0("2 147 FERT (",round(1.3*fert_adjust,2),"P)"),
    "2 147 CROP C8",
    "2 147 PLTM",
    paste0("2 160 FERT (",round(0.1*fert_adjust,2),"N)"),
    paste0("2 172 FERT (",round(13.8*fert_adjust,2),"N)"),
    paste0("2 303 HARV G",resid_adjust_chr,"S"),
    paste0("3 110 FERT (",round(0.8*fert_adjust,2),"N)"),
    paste0("3 110 FERT (",round(1.7*fert_adjust,2),"P)"),
    if_else(mgmt_scenario_grp==5, paste0("3 115 CULT H",resid_adjust_chr),
            "3 115 CULT H"),
    if_else(mgmt_scenario_grp==5, paste0("3 138 CULT D",resid_adjust_chr),
            "3 138 CULT D"),
    "3 168 CROP SYBN",
    "3 168 PLTM",
    paste0("3 174 FERT (",round(0.1*fert_adjust,2),"N)"),
    paste0("3 292 HARV G",resid_adjust_chr,"S"),
    "3 312 CROP W4EG",
    "3 312 PLTM"
  )
}
    
footer_txt <- "-999 -999 X"

block_txt <- c(header_txt, ops_txt, footer_txt)

writeLines(block_txt,schedule_file_2100)


