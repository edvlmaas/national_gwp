f_model_coef <- function(df_in,modeled_element_in,model_name_in,
                         climate_scen_in,mgmt_group_in,result_name_in) {
  
  print("Starting f_model_coef.R")
  
  fm <- as.formula(paste(modeled_element_in, "~ year"))
  return_df <- data.frame()
  
  if(model_name_in=="All") {
    
    if(is.na(mgmt_group_in)) {
      scenario_list <- unique(df_in[df_in$climate_scenario_num==climate_scen_in,
                                    "scenario_name"])
      for(i in scenario_list$scenario_name) {
        one_rec <- coef(lm(fm, 
                           data = df_in[df_in$year>=experiment_end_year &
                                          df_in[[modeled_element_in]] != 0 & 
                                          df_in$scenario_name==i,]))
        return_df <- rbind(return_df,data.frame(result_name_in,i,one_rec[1],one_rec[2]))
      } # for each scenario_name
      
    } # if mgmt_group_in is NA
    
  } else { # model name isn't "All"
    
    scenario_list <- unique(df_in[df_in$climate_scenario_num==climate_scen_in &
                                    df_in$mgmt_scenario_grp_num==mgmt_group_in,
                                  "scenario_name"])
    for(i in scenario_list) {
      one_rec <- coef(lm(fm, 
                         data = df_in[df_in$year>=experiment_end_year &
                                        df_in[[modeled_element_in]] != 0 &
                                        df_in$model_name==model_name_in &
                                        df_in$scenario_name==i,]))
      return_df <- rbind(return_df,data.frame(result_name_in,i,one_rec[1],one_rec[2]))
    } # for each scenario_name
    
  } # if model_name_in = "All"
  
  colnames(return_df) <- c("fit_name","scenario_name","intercept","slope")
  return(return_df)
  
} # end function
