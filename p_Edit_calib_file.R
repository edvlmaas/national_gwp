#######################################
# Function: "p_Edit_calib_file"
# Author: "Ellen Maas"
# Date: "Nov. 23, 2022"
# Output: Function doesn't return any data, hence the "p" (procedure) naming
# convention. It creates a new output file with results from calibration of
# each model and scenario. If it already exists, it checks to see if a record
# already exists for the model and scenario. If it does, it replaces the data,
# otherwise it appends it to the end."
#
#######################################
# Called by:
# p_Calib_analysis.R
#
#######################################
# Audit Log
# 11/23/2022 EDM Created function.
#
#######################################

p_Edit_calib_file <- function(data_mtx,model_name,scenario_name) {
  
  print("Starting p_Edit_calib_file")
  
  outfile_name <- paste0(results_path,"Calibration_summary.csv")
  colnames(data_mtx) <- c("Date_time","Model","Climate_Scenario","Mgmt_Scenario","Scenario_Name",
                          "Maize_slope","Maize_yint","Maize_R2","Maize_RMSE",
                          "Soy_slope","Soy_yint","Soy_R2","Soy_RMSE",
                          "Wheat_slope","Wheat_yint","Wheat_R2","Wheat_RMSE",
                          "SOC_slope","SOC_yint","SOC_R2","SOC_RMSE",
                          "Temp_slope","Temp_yint","Temp_R2","Temp_RMSE",
                          "Moist_slope","Moist_yint","Moist_R2","Moist_RMSE",
                          "N2O_slope","N2O_yint","N2O_R2","N2O_RMSE",
                          "CH4_slope","CH4_yint","CH4_R2","CH4_RMSE")
  
  # if the output file already exists, check if the model/scenario already has
  # an entry and replace it; otherwise, create a new file
  if(file.exists(outfile_name)) {
    existing_data <- read.table(file=outfile_name,header=TRUE,sep=",")
    existing_rec <- which(existing_data$Model==model_name & existing_data$Scenario_Name==scenario_name)
    if(length(existing_rec!=0)) {
      existing_data[existing_rec,"Date_time"] <- as.character(Sys.time())
      existing_data[existing_rec,"Maize_slope"] <- data_mtx[,5]
      existing_data[existing_rec,"Maize_yint"] <- data_mtx[,6]
      existing_data[existing_rec,"Maize_R2"] <- data_mtx[,7]
      existing_data[existing_rec,"Maize_RMSE"] <- data_mtx[,8]
      existing_data[existing_rec,"Soy_slope"] <- data_mtx[,9]
      existing_data[existing_rec,"Soy_yint"] <- data_mtx[,10]
      existing_data[existing_rec,"Soy_R2"] <- data_mtx[,11]
      existing_data[existing_rec,"Soy_RMSE"] <- data_mtx[,12]
      existing_data[existing_rec,"Wheat_slope"] <- data_mtx[,13]
      existing_data[existing_rec,"Wheat_yint"] <- data_mtx[,14]
      existing_data[existing_rec,"Wheat_R2"] <- data_mtx[,15]
      existing_data[existing_rec,"Wheat_RMSE"] <- data_mtx[,16]
      existing_data[existing_rec,"SOC_slope"] <- data_mtx[,17]
      existing_data[existing_rec,"SOC_yint"] <- data_mtx[,18]
      existing_data[existing_rec,"SOC_R2"] <- data_mtx[,19]
      existing_data[existing_rec,"SOC_RMSE"] <- data_mtx[,20]
      existing_data[existing_rec,"Temp_slope"] <- data_mtx[,21]
      existing_data[existing_rec,"Temp_yint"] <- data_mtx[,22]
      existing_data[existing_rec,"Temp_R2"] <- data_mtx[,23]
      existing_data[existing_rec,"Temp_RMSE"] <- data_mtx[,24]
      existing_data[existing_rec,"Moist_slope"] <- data_mtx[,25]
      existing_data[existing_rec,"Moist_yint"] <- data_mtx[,26]
      existing_data[existing_rec,"Moist_R2"] <- data_mtx[,27]
      existing_data[existing_rec,"Moist_RMSE"] <- data_mtx[,28]
      existing_data[existing_rec,"N2O_slope"] <- data_mtx[,29]
      existing_data[existing_rec,"N2O_yint"] <- data_mtx[,30]
      existing_data[existing_rec,"N2O_R2"] <- data_mtx[,31]
      existing_data[existing_rec,"N2O_RMSE"] <- data_mtx[,32]
      existing_data[existing_rec,"CH4_slope"] <- data_mtx[,33]
      existing_data[existing_rec,"CH4_yint"] <- data_mtx[,34]
      existing_data[existing_rec,"CH4_R2"] <- data_mtx[,35]
      existing_data[existing_rec,"CH4_RMSE"] <- data_mtx[,36]
      data_mtx <- existing_data
    } else {
      data_mtx <- rbind(existing_data,data_mtx)
    } # end if record exists
  } # end if file exists
  
  write.table(data_mtx,file=outfile_name,
              col.names=T,
              row.names=F,sep=",")
  
}
