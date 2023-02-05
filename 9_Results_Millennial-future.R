# ---
# title: 9_Results_Millennial-future.R
# author: Ellen Maas
# date: 8/30/2022
# output: html_document

suppressMessages({
  
print("Starting 9_Results_Millennial-future.R")

library(readxl)
#library(plotly)
library(magrittr)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)


# 9-color palette with grey and black. Colors in order are:
#[1]black, [2]dark blue, [3]green, [4]light blue, [5]grey,
#[6]pink, [7]red, [8]orange, [9]yellow

#*************************************************************

# Temporal graphs
## carbon

if(scenario_name=="1_3") {
  # remove 1998 observation from regression, looks like bad data
  Cfit_Obs <- coef(lm(Observed ~ year, data = Cstock_Mgha_25cm[Cstock_Mgha_25cm$year >= experiment_start_year &
                                                                 Cstock_Mgha_25cm$year != 1998,]))
} else {
  Cfit_Obs <- coef(lm(Observed ~ year, data = Cstock_Mgha_25cm[Cstock_Mgha_25cm$year >= experiment_start_year,]))
}


input_data <- if(scenario_name=="1_3") { Cstock_Mgha_piv_25cm[Cstock_Mgha_piv_25cm$year>=1989 & 
                                                         Cstock_Mgha_piv_25cm$year!=1998,] 
} else { Cstock_Mgha_piv_25cm[Cstock_Mgha_piv_25cm$year>=1989,] }

# 25 cm experimental through future 
gC4 <- input_data %>%
  ggplot(aes(x=year, y=C_val, color=source, show.legend=TRUE)) +
  geom_point(show.legend=TRUE) +
  geom_abline(intercept=Cfit_Obs[1], slope=Cfit_Obs[2], color="black") +
  #geom_point(data=ObsCdeep_Mgha, aes(x=ObsCdeep_Mgha$year, y=ObsCdeep_Mgha$cstock, color="blue")) +
  xlab("Year") +
  ylab(expression('SOC stock (Mg C ha' ^-1*')')) +
  ggtitle(paste(site_name,"Soil Organic Carbon: Scenario ",scenario_name)) +
  #  geom_abline(intercept=Cfit_RothC[1], slope=Cfit_RothC[2], color="orange") +
  #  geom_abline(intercept=Cfit_Obs[1], slope=Cfit_Obs[2], color="black") +
  scale_color_manual(labels=c("Millennial","Observed-25cm"),
                     values=cbPalette9[c(8,1)]) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank(),
        legend.title = element_blank())

gC4

ggsave(filename=paste0(results_path,"SOC_comparison_fut_",scenario_name,"_Millennial.jpg"),plot=gC4)


}) # end suppressMessages