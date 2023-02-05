#######################################
# Function: 4_Create_additional_files-LDNDC.R
# Author: Ellen Maas
# Date: Sept 23, 2022
# Output: It creates setup and project files and writes the contents of
# batch files.
# Description: "This procedure generates management data - including crop,
# fertilizer, irrigation, fertilizer, harvest, etc. - for every model in the 
# format needed by each. For APSIM, fertilizer is collected as kg N/ha of the N 
# in the fertilizer (not kg/ha of the whole fertilizer)."
#######################################
# Audit Log
# 9/23/2022: Created file.
#######################################

print("Starting 4_Create_additional_files-LDNDC.R")

library(xml2)

dndc_path <- "LDNDC/ldndc-1.30.4.win64/projects/"
dndc_setup_filename <- paste0(dndc_path,site_name,"/",site_name,"_setup.xml")
dndc_project_filename <- paste0(dndc_path,site_name,"/",site_name,".ldndc")
dndc_airchem_filename <- paste0(dndc_path,site_name,"/",site_name,"_airchem.txt")
dndc_batch_filename <- paste0(dndc_path,site_name,"/",site_name,".bat")
dndc_project_filename_2100 <- paste0(dndc_path,site_name,"/",site_name,"_2100.ldndc")
dndc_batch_filename_2100 <- paste0(dndc_path,site_name,"/",site_name,"_2100.bat")

# DNDC
## setup file


doc_setup <- read_xml(paste0(
  "<?xml version=\"1.0\" ?>",
  "<ldndcsetup>",
  "<setup id=\"",site_id,"\" name=\"",site_name,"\" >",
  "<location elevation=\"",elevation_m,"\" latitude=\"",latitude,"\" longitude=\"",longitude,"\" />",
  "<models>",
  "<model id=\"_MoBiLE\" />",
  "</models>",
  "<mobile>",
  "<modulelist>",
  "<module id=\"airchemistry:airchemistrydndc\" timemode=\"daily\" />",
  "<module id=\"microclimate:canopyecm\" timemode=\"daily\" />",
  "<module id=\"physiology:arabledndc\" timemode=\"daily\" />",
  "<module id=\"soilchemistry:metrx\" timemode=\"daily\" />",
  "<module id=\"watercycle:watercycledndc\" timemode=\"daily\" />",
  "<!-- outputs -->",
  "<module id=\"output:soilchemistry-layer:daily\" />",
  "<module id=\"output:soilchemistry:yearly\" />",
  "<module id=\"output:microclimate:daily\" />",
  "<module id=\"output:physiology:daily\" />",
  "<module id=\"output:watercycle:daily\" />",
  "<module id=\"output:report:arable:harvest\" timemode=\"daily\" />",
  "</modulelist>",
  "</mobile>",
  "</setup>",
  "</ldndcsetup>"
))

write_xml(doc_setup,file=dndc_setup_filename)


## project file


doc_proj <- read_xml(paste0("<?xml version=\"1.0\" ?><ldndcproject PackageMinimumVersionRequired=\"1.30.4\">",
                 paste0('<schedule time=\"',start_date,'/1 -> ',end_date,'\" />'),
                  "<input>",
                 paste0("<sources sourceprefix=\"",site_name,"\\",site_name,"_\" >"),
                 "<setup source=\"setup.xml\" />",
                 "<site source=\"site.xml\" />",
                 "<airchemistry source=\"airchem.txt\" format=\"txt\" />",
                 "<climate source=\"climate.txt\" />",
                 "<event source=\"mana.xml\" />",
                 "</sources>",
                  "</input>",
                  "<output>",
                 paste0("<sinks sinkprefix=\"",site_name,"\\",site_name,"_output\\",site_name,"_\" >"),
                 "<soilchemistrydaily sink=\"soil-chem.txt\" format=\"txt\" />",
                 "<microclimatedaily sink=\"soil-temp.txt\" format=\"txt\" />",
                 "<physiologydaily sink=\"physiology.txt\" format=\"txt\" />",
                 "<harvest sink=\"harvest.txt\" format=\"txt\" />",
                 "<watercycledaily sink=\"soil-water.txt\" format=\"txt\" />",
                 "</sinks>",
                  "</output>",
                 "</ldndcproject>"))

#print(x)

write_xml(doc_proj,file=dndc_project_filename)
          

doc_proj_2100 <- read_xml(paste0("<?xml version=\"1.0\" ?><ldndcproject PackageMinimumVersionRequired=\"1.30.4\">",
                 paste0('<schedule time=\"',start_date,'/1 -> ',end_date_2100,'\" />'),
                  "<input>",
                 paste0("<sources sourceprefix=\"",site_name,"\\",site_name,"_\" >"),
                 "<setup source=\"setup.xml\" />",
                 "<site source=\"site.xml\" />",
                 "<airchemistry source=\"airchem.txt\" format=\"txt\" />",
                 "<climate source=\"climate_2100.txt\" />",
                 "<event source=\"mana_2100.xml\" />",
                 "</sources>",
                  "</input>",
                  "<output>",
                 paste0("<sinks sinkprefix=\"",site_name,"\\",site_name,"_output\\",site_name,"_\" >"),
                 "<soilchemistrydaily sink=\"soil-chem_2100.txt\" format=\"txt\" />",
                 "<microclimatedaily sink=\"soil-temp_2100.txt\" format=\"txt\" />",
                 "<physiologydaily sink=\"physiology_2100.txt\" format=\"txt\" />",
                 "<harvest sink=\"harvest_2100.txt\" format=\"txt\" />",
                 "<watercycledaily sink=\"soil-water_2100.txt\" format=\"txt\" />",
                 "</sinks>",
                  "</output>",
                 "</ldndcproject>"))

#print(x)

write_xml(doc_proj_2100,file=dndc_project_filename_2100)
          

## air chemistry file


# # output header data
# 
# DNDC_airchem_file <- paste0(dndc_path,site_name,"/",site_name,"_airchem.txt")
# 
# airchem_txt <- c("%global",
#                 paste0("        time = \"",start_date,"/1\"\n"),
#                 "%airchemistry",
#                 paste0("        id = \"",as.character(site_id),"\""),
#                 "%attributes",
#                 paste0("        co2 = \"353\"") #,
# #                "\n",
# #                "%data",
# #                "*\t *\t prec\t tavg\t tmax\t tmin\t grad\t wind"
# )
# 
# writeLines(airchem_txt,dndc_airchem_filename)
# 
# # # add data
# # write.table(DNDC_basic,sep="\t",
# #             file=DNDC_wth_file,
# #             append=TRUE,
# #             row.names = F,
# #             col.names = F)


## batch file


# contents of batch file
batch_txt <- paste0("%cd%\\..\\..\\bin\\ldndc.exe ",site_name,".ldndc")
                    
writeLines(batch_txt,dndc_batch_filename)

# contents of batch file
batch_txt_2100 <- paste0("%cd%\\..\\..\\bin\\ldndc_2100.exe ",site_name,"_2100.ldndc")
                    
writeLines(batch_txt_2100,dndc_batch_filename_2100)

