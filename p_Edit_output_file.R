#######################################
# Function: "p_Edit_output_file"
# Author: "Ellen Maas"
# Date: "Nov. 4, 2022"
# Output: Function doesn't return any data, hence the "p" (procedure) naming
# convention. It creates a new output file with summary data from a model
# run and writes the data. If it already exists, it checks to see if a record
# already exists for the model and scenario. If it does, it replaces the data,
# otherwise it appends it to the end."
#
#######################################
# Called by:
# 9_Results_Daycent-analysis.R
#
#######################################
# Audit Log
# 11/4/2022 EDM Created function.
#
#######################################

p_Edit_output_file <- function(data_mtx,model_name,scenario_name) {
  
  print("Starting p_Edit_output_file")
  
  outfile_name <- paste0("Summary_output.csv")
  colnames(data_mtx) <- c("Model","Climate_Scenario","Mgmt_Scenario","Scenario_Name",
                          "Maize_Diff_Mgha","Soybean_Diff_Mgha","Wheat_Diff_Mgha",
                          "SOC_Diff_Mgha","Total_N2O_kgha","Total_CH4_kgha")
  
  # if the output file already exists, check if the model/scenario already has
  # an entry and replace it; otherwise, create a new file
  if(file.exists(outfile_name)) {
    existing_data <- read.table(file=outfile_name,header=TRUE,sep=",")
    existing_rec <- which(existing_data$Model==model_name & existing_data$Scenario_Name==scenario_name)
    if(length(existing_rec!=0)) {
      existing_data[existing_rec,"Maize_Diff_Mgha"] <- data_mtx[,5]
      existing_data[existing_rec,"Soybean_Diff_Mgha"] <- data_mtx[,6]
      existing_data[existing_rec,"Wheat_Diff_Mgha"] <- data_mtx[,7]
      existing_data[existing_rec,"SOC_Diff_Mgha"] <- data_mtx[,8]
      existing_data[existing_rec,"Total_N2O_kgha"] <- data_mtx[,9]
      existing_data[existing_rec,"Total_CH4_kgha"] <- data_mtx[,10]
      data_mtx <- existing_data
    } else {
      data_mtx <- rbind(existing_data,data_mtx)
    } # end if record exists
  } # end if file exists
  
  write.table(data_mtx,file=paste0(results_path,outfile_name),
              col.names=c("Model","Climate_Scenario","Mgmt_Scenario","Scenario_Name",
                          "Maize_Diff_Mgha","Soybean_Diff_Mgha","Wheat_Diff_Mgha",
                          "SOC_Diff_Mgha","Total_N2O_kgha","Total_CH4_kgha"),
              row.names=F,sep=",")
  
}