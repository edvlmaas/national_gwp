#######################################################################################################
#
# compile_data_for_cleaning.R
#
# Author: Ellen Maas Sept 25, 2022
#
# Description: Compiles field operation data from multiple sources into one data frame which is then
# written out to a file ("clean_ops_ext.csv"). This file is then further cleaned manually and saved
# as "clean_ops_ext_adj.csv". This last version is then used as the source of management data for
# the KBS model scenarios.
#
#######################################################################################################

# Clear memory before rerunning the script. 
rm(list=ls())

library(readr)
library(tidyverse)
library(lubridate)
library(dplyr)

# Set up environment
modelPath <- paste0("C:/Users/edmaas/Documents/Modeling/Data/KBS/Management/")
setwd(modelPath)

# Set constants
operations_filename <- "162-activity+report+1657744914.csv"
operations_ext_filename <- "150-expanded+agronomic+log+1656512591.csv"
yield_filename <- "51-agronomic+yields+annual+crops+1656512632.csv"
year_range <- 1989:2021


###########################
#import_and_clean}
###########################

# this has the main field operations
raw_ops <- read_csv(operations_filename,
                    skip=21, col_names = TRUE, show_col_types = F) 
# this has addtl field operation details
raw_ops_ext <- read_csv(operations_ext_filename,
                        skip=26, col_names = T, show_col_types = F)
# this has the crop each year
raw_yield <- read_csv(yield_filename,
                      skip=31, col_names = T, show_col_types = F)

#save_probs <- problems()

# Inspect data for consistency
unique(raw_ops$Observation_type)
unique(raw_ops_ext$observation_type)
unique(raw_yield$Crop)

# Reverse rows so oldest data is first, modify date columns, add crop
temp_ops <- raw_ops[order(nrow(raw_ops):1),] %>%
  mutate(date=as.Date(Obs_date,format="%m/%d/%Y"),
         year=year(date)) 
temp_ops_ext <- raw_ops_ext[order(nrow(raw_ops_ext):1),] %>%
  mutate(date=as.Date(obs_date,format="%m/%d/%Y"),
         year=year(date))
temp_yield <- raw_yield[order(nrow(raw_yield):1),] %>%
  mutate(date=as.Date(Date,format="%m/%d/%Y"),
         year=year(date),
         crop=ifelse(Crop=="Zea mays L. (*)", "Maize", 
                     ifelse(Crop=="Glycine max L. (*)", "Soybean",
                            ifelse(Crop=="Triticum aestivum L. (*)", "Wheat", NA)
                     )
         )
  )

# save yield (kg_ha) for later
save_yield <- temp_yield[temp_yield$year %in% year_range,] %>%
  group_by(year,Treatment,crop) %>%
  summarize(mean_yield = round(mean(crop_only_yield_kg_ha,na.rm=T),0)) %>%
  mutate(treatment=Treatment)
#write.table(save_yield, file="Site and Region Info/Calibration/Yield by Year and Treatment.txt",
#            row.names=F, quote=F, col.names=T, sep=',')

# Only take tilling, planting, harvesting,
# and fertilizing, for the year range, using only the first date for the plot
temp2_ops <- temp_ops[temp_ops$year %in% year_range,]
temp2_ops_ext <- temp_ops_ext[which(temp_ops_ext$year %in% 
                                      year_range & temp_ops_ext$observation_type %in% 
                                      c("Soil Preparation","Planting",
                                        "Fertilizer application","Harvest")),] %>%
  group_by(year,treatment,observation_type,material,rate,units,equipment) %>%
  summarize(date=min(date)) 

# final versions
clean_yield <- save_yield
## merge ops_ext and yield data
clean_ops_ext <- left_join(temp2_ops_ext[order(temp2_ops_ext$date),],
                           clean_yield[,c(1,3,5)],by=c("year","treatment"))
## merge ops and yield data
clean_ops <- left_join(temp2_ops,clean_yield[,c(1,2,3,5)],by=c("year","Treatment"))

# write out data for manual manipulation
write.csv(clean_ops_ext, file="clean_ops_ext.csv", 
          quote=T, row.names = F)
