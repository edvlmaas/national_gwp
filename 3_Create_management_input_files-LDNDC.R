#######################################
# Function: 3_Create_management_input_files-LDNDC.R
# Author: Ellen Maas
# Date: Sept 23, 2022
# Output: It creates files in the appropriate folder for each model.
# Description: "This procedure generates management data - including crop,
# fertilizer, irrigation, fertilizer, harvest, etc. - for every model in the 
# format needed by each. For APSIM, fertilizer is collected as kg N/ha of the N 
# in the fertilizer (not kg/ha of the whole fertilizer)."
#######################################
# Audit Log
# 9/30/2022: Modified to include T3 data for scenario 3.
# 10/3/2022: Changed hairy vetch crop type to white clover
#######################################

print("Starting 3_Create_management_input_files-LDNDC.R")

###########################
# LDNDC 
###########################

## experimental


# rename data frame to save coding space and time
df <- unique(full_ops_ext_adj[full_ops_ext_adj$treatment==treatment,
                       c("year","treatment","observation_type","date","crop",
                         "obs_code","n_rate_kg_ha","till_depth")])
df$dndc_event <- if_else(df$observation_type=="Harvest","harvest",
                         if_else(df$observation_type=="Planting","plant",
                                 if_else(df$observation_type=="Fertilizer application", "fertilize",
                                         if_else(df$obs_code %in% c("plow","disk","cultivate",
                                                                    "cultipack","finish","hoe"),
                                                 "till",NULL))))
df$dndc_plant_type <- if_else(df$crop=="Maize","FOCO",
                              if_else(df$crop=="Soybean","SOYB",
                                      if_else(df$crop=="Wheat","WIWH",NULL)))



# this is the top level - "event" encapsulates the whole doc
doc <- newXMLDoc()
root = newXMLNode("event", doc=doc)


# this is the second level - also "event" tags, with attributes and sub-child nodes
for(i in 1:nrow(df)) {
  event_node <- newXMLNode("event", parent=root)
  addAttributes(event_node, type = df[i,"dndc_event"],
                time = as.character(df[i,"date"])
  # addAttributes(event_node, type = paste0("\"",df[i,"dndc_event"],"\""),
  #               time = paste0("\"",df[i,"date"],"\"")
  )
  sub_node <- newXMLNode(df[i,"dndc_event"],parent=event_node)
  switch(df[i,"dndc_event"],
         # "plant" = addAttributes(sub_node, type = paste0("\"",df[i,"dndc_plant_type"],"\"")),
         # "till" = addAttributes(sub_node, depth = paste0("\"",df[i,"till_depth"],"\"")),
         # "fertilize" = addAttributes(sub_node, amount = paste0("\"",df[i,"n_rate_kg_ha"],"\""),
         #                             type = "no3"),
         # "harvest" = addAttributes(sub_node, remains = "\"0.25\"",
         #                           name = paste0("\"",df[i,"dndc_plant_type"],"\""))
         "plant" = addAttributes(sub_node, type = df[i,"dndc_plant_type"]),
         "till" = addAttributes(sub_node, depth = df[i,"till_depth"]),
         "fertilize" = addAttributes(sub_node, amount = df[i,"n_rate_kg_ha"],
                                     type = "no3"),
         "harvest" = addAttributes(sub_node, remains = "0.25",
                                   name = df[i,"dndc_plant_type"])
  )
}


###########################
## LDNDC future


DNDC_2100 <- df
DNDC_3yr <- df[df$year %in% 2019:2021,]

for (i in 1:27) {
DNDC_3yr$year <- as.character(as.integer(DNDC_3yr[,"year"]) + 3)
DNDC_3yr$date <- DNDC_3yr$date + years(3)
DNDC_2100 <- rbind(DNDC_2100, DNDC_3yr)
}

# this is the top level - "event" encapsulates the whole doc
doc_2100 <- newXMLDoc()
root_2100 = newXMLNode("event", doc=doc_2100)


# this is the second level - also "event" tags, with attributes and sub-child nodes
for(i in 1:nrow(DNDC_2100)) {
  event_node <- newXMLNode("event", parent=root_2100)
  addAttributes(event_node, type = DNDC_2100[i,"dndc_event"],
                time = as.character(DNDC_2100[i,"date"])
  # addAttributes(event_node, type = paste0("\"",DNDC_2100[i,"dndc_event"],"\""),
  #               time = paste0("\"",DNDC_2100[i,"date"],"\"")
  )
  sub_node <- newXMLNode(DNDC_2100[i,"dndc_event"],parent=event_node)
  switch(DNDC_2100[i,"dndc_event"],
         # "plant" = addAttributes(sub_node, type = paste0("\"",DNDC_2100[i,"dndc_plant_type"],"\"")),
         # "till" = addAttributes(sub_node, depth = paste0("\"",DNDC_2100[i,"till_depth"],"\"")),
         # "fertilize" = addAttributes(sub_node, amount = paste0("\"",DNDC_2100[i,"n_rate_kg_ha"],"\""),
         #                             type = "no3"),
         # "harvest" = addAttributes(sub_node, remains = "\"0.25\"",
         #                           name = paste0("\"",DNDC_2100[i,"dndc_plant_type"],"\""))
         "plant" = addAttributes(sub_node, type = DNDC_2100[i,"dndc_plant_type"]),
         "till" = addAttributes(sub_node, depth = DNDC_2100[i,"till_depth"]),
         "fertilize" = addAttributes(sub_node, amount = DNDC_2100[i,"n_rate_kg_ha"],
                                     type = "no3"),
         "harvest" = addAttributes(sub_node, remains = "0.25",
                                   name = DNDC_2100[i,"dndc_plant_type"])
  )
}

###########################
# LDNDC write files


# experimental
saveXML(doc,file=paste0(dndc_path,"mana.xml"))

# future
saveXML(doc_2100,file=paste0(dndc_path,"mana_",clim_scenario_num,"_",mgmt_scenario_num,".xml"))


