#######################################
# Function: "p_Create_soil_data"
# Author: "Ellen Maas"
# Date: "July 11, 2022"
# Output: Function doesn't return any data, hence the "p" (procedure) naming
# convention. It creates files in the appropriate folder for each model.
# Description: "This procedure generates soil data for the site."
#######################################

###########################
# LDNDC
###########################

library(xml2)

## this is the top level
doc <- xml_new_root("site",.version="1.0", .encoding="UTF-8")
xml_set_attr(doc, "id", site_id)

## these are the second level
xml_add_child(doc, "description") 
xml_add_child(doc, "soil")

## these are the third level
description_nodes <- xml_find_all(doc, "//description")
xml_add_child(description_nodes, "author", "Ellen Maas")
xml_add_child(description_nodes, "date", as.character(Sys.Date()))
xml_add_child(description_nodes, "dataset", site_name)

soil_nodes <- xml_find_all(doc, "//soil")
xml_add_child(soil_nodes, "general")
general_node <- xml_find_all(doc, "//general")
xml_set_attrs(general_node,c("usehistory" = "arable",
                             "humus" = "HUMUS",
                             "soil" = soil_type_code))
xml_add_child(soil_nodes, "layers")

layer_nodes <- xml_find_all(doc, "//layers")

## flip order of soil layers (bug in xml2?)
flipped_soil_df <- soil_df[order(nrow(soil_df):1),]

xml_add_child((layer_nodes), paste("layer ",
                                   #paste0("wcmin=\"",flipped_soil_df$LL15,"\""),
                                   #paste0("wcmax=\"",flipped_soil_df$DUL,"\""),
                                   #paste0("wcmin=\"",flipped_soil_df$LL15_dm3m3,"\""),
                                   #paste0("wcmax=\"",flipped_soil_df$DUL_dm3m3,"\""),
                                   paste0("wcmin=\"",flipped_soil_df$LL15_dm3m3,"\""),
                                   paste0("wcmax=\"",flipped_soil_df$DUL_dm3m3,"\""),
                                   paste0("ph=\"",flipped_soil_df$PH,"\""),
                                   paste0("corg=\"",flipped_soil_df$Carbon,"\""),
                                   paste0("clay=\"",flipped_soil_df$clay_fraction,"\""),
                                   paste0("sand=\"",flipped_soil_df$sand_fraction,"\""),
                                   paste0("bd=\"",flipped_soil_df$BD,"\""),
                                   paste0("depth=\"",flipped_soil_df$Thickness,"\""),
                                   paste0("sks=\"",flipped_soil_df$KS_cmmin,"\"")
                                   )
              )

#doc

write_xml(doc,file=paste0(dndc_path,"/",site_name,"_site_",
                          clim_scenario_num,"_",mgmt_scenario_num,".xml"),
          append=T)

