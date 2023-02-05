# title: "9_Results_Daycent-manual_analysis.R"
# author: "Ellen Maas"
# date: "8/8/2022"
# output: html_document
# description: compare results of scenarios for troubleshooting

library(readxl)
library(magrittr)
library(lubridate)
library(tidyverse)
library(graphics)
library(ggplot2)
library(broom)

#**********************************************************************

# import Daycent modeled points

lis_output_1_1 <- read.table(paste0(daycent_path,paste0("sched_fut_1_1.lis")),
                             col.names = c("time","somsc_gm2","somtc","somte(1)",
                                           "crpval","cinput","somse(1)","petann",
                                           "tminrl(1)","minerl(1,1)","minerl(2,1)",
                                           "minerl(3,1)","minerl(4,1)","minerl(5,1)",
                                           "minerl(6,1)","minerl(7,1)","minerl(8,1)",
                                           "aglivc","bglivcj","bglivcm","cgrain",
                                           "crmvst","hi","clitad(1)","clitad(2)",
                                           "elitad(1,1)","elitad(2,1)"),
                             colClasses=c("numeric","numeric","numeric","numeric",
                                          "character","numeric","numeric","numeric",
                                          "numeric","numeric","numeric","numeric",
                                          "numeric","numeric","numeric","numeric",
                                          "numeric","numeric","numeric","numeric",
                                          "numeric","numeric","numeric","numeric",
                                          "numeric","numeric","numeric"),
                             skip=45)

lis_output_1_2 <- read.table(paste0(daycent_path,paste0("sched_fut_1_2.lis")),
                             col.names = c("time","somsc_gm2","somtc","somte(1)",
                                           "crpval","cinput","somse(1)","petann",
                                           "tminrl(1)","minerl(1,1)","minerl(2,1)",
                                           "minerl(3,1)","minerl(4,1)","minerl(5,1)",
                                           "minerl(6,1)","minerl(7,1)","minerl(8,1)",
                                           "aglivc","bglivcj","bglivcm","cgrain",
                                           "crmvst","hi","clitad(1)","clitad(2)",
                                           "elitad(1,1)","elitad(2,1)"),
                             colClasses=c("numeric","numeric","numeric","numeric",
                                          "character","numeric","numeric","numeric",
                                          "numeric","numeric","numeric","numeric",
                                          "numeric","numeric","numeric","numeric",
                                          "numeric","numeric","numeric","numeric",
                                          "numeric","numeric","numeric","numeric",
                                          "numeric","numeric","numeric"),
                             skip=45)

lis_output_1_3 <- read.table(paste0(daycent_path,paste0("sched_fut_1_3.lis")),
                                                  col.names = c("time","somsc_gm2","somtc","somte(1)",
                                       "crpval","cinput","somse(1)","petann",
                                       "tminrl(1)","minerl(1,1)","minerl(2,1)",
                                       "minerl(3,1)","minerl(4,1)","minerl(5,1)",
                                       "minerl(6,1)","minerl(7,1)","minerl(8,1)",
                                       "aglivc","bglivcj","bglivcm","cgrain",
                                       "crmvst","hi","clitad(1)","clitad(2)",
                                       "elitad(1,1)","elitad(2,1)"),
                         colClasses=c("numeric","numeric","numeric","numeric",
                                      "character","numeric","numeric","numeric",
                                      "numeric","numeric","numeric","numeric",
                                      "numeric","numeric","numeric","numeric",
                                      "numeric","numeric","numeric","numeric",
                                      "numeric","numeric","numeric","numeric",
                                      "numeric","numeric","numeric"),
                         skip=45)

DayCI_gm2yr_1_1 <- lis_output_1_1[!((lis_output_1_1$cinput == 0 & 
                               (lis_output_1_1$time == experiment_start_year | lis_output_1_1$time == experiment_end_year+1)) |
                              lis_output_1_1$time == 2100),c("time","clitad.2.")] %>%  
  mutate(year=floor(time),
         base=`clitad.2.`
  )

DayCI_gm2yr_1_2 <- lis_output_1_2[!((lis_output_1_2$cinput == 0 & 
                                       (lis_output_1_2$time == experiment_start_year | lis_output_1_2$time == experiment_end_year+1)) |
                                      lis_output_1_2$time == 2100),c("time","clitad.2.")] %>%  
  mutate(year=floor(time),
         base=`clitad.2.`
  )

DayCI_gm2yr_1_3 <- lis_output_1_3[!((lis_output_1_3$cinput == 0 & 
                                       (lis_output_1_3$time == experiment_start_year | lis_output_1_3$time == experiment_end_year+1)) |
                                      lis_output_1_3$time == 2100),c("time","clitad.2.")] %>%  
  mutate(year=floor(time),
         base=`clitad.2.`
  )
  
DayNI_gm2yr_1_1 <- lis_output_1_1[!((lis_output_1_1$cinput == 0 & 
                               (lis_output_1_1$time == experiment_start_year | lis_output_1_1$time == experiment_end_year+1)) |
                              lis_output_1_1$time == 2100),c("time","elitad.2.1.")] %>%  
  mutate(year=floor(time),
         base=`elitad.2.1.`
  )

DayNI_gm2yr_1_2 <- lis_output_1_2[!((lis_output_1_2$cinput == 0 & 
                                       (lis_output_1_2$time == experiment_start_year | lis_output_1_2$time == experiment_end_year+1)) |
                                      lis_output_1_2$time == 2100),c("time","elitad.2.1.")] %>%  
  mutate(year=floor(time),
         base=`elitad.2.1.`
  )

DayNI_gm2yr_1_3 <- lis_output_1_3[!((lis_output_1_3$cinput == 0 & 
                                       (lis_output_1_3$time == experiment_start_year | lis_output_1_3$time == experiment_end_year+1)) |
                                      lis_output_1_3$time == 2100),c("time","elitad.2.1.")] %>%  
  mutate(year=floor(time),
         base=`elitad.2.1.`
  )

#**********************************************************************

gCI <- DayCI_gm2yr_1_1[DayCI_gm2yr_1_1$year <= experiment_end_year,] %>%
  ggplot(aes(x=year, y=base, color=cbPalette9[1]), show.legend=TRUE) +
#  geom_line(show.legend=TRUE) +
  geom_line(data=DayCI_gm2yr_1_3[DayCI_gm2yr_1_3$year <= experiment_end_year,],
            aes(x=year, y=base, color=cbPalette9[8])) +
#  geom_line(data=DayCI_gm2yr_1_2[DayCI_gm2yr_1_2$year <= experiment_end_year,],
#            aes(x=year, y=base, color=cbPalette9[3])) +
  xlab("Year") +
  ylab(expression('C input (g C m' ^-2*' yr' ^-1*')')) +
  ggtitle("Baseline and cover scenario C input (g m^2)") +
  scale_color_manual(labels=c("Cover Crops"),
                     values=cbPalette9[c(8)]) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gCI

gNI <- DayNI_gm2yr_1_1[DayNI_gm2yr_1_1$year <= experiment_end_year,] %>%
  ggplot(aes(x=year, y=base, color=cbPalette9[1]), show.legend=TRUE) +
#  geom_line(show.legend=TRUE) +
  geom_line(data=DayNI_gm2yr_1_3[DayNI_gm2yr_1_3$year <= experiment_end_year,],
            aes(x=year, y=base, color=cbPalette9[8])) +
  # geom_line(data=DayNI_gm2yr_1_2[DayNI_gm2yr_1_2$year <= experiment_end_year,],
  #           aes(x=year, y=base, color=cbPalette9[3])) +
  xlab("Year") +
  ylab(expression('N input (g C m' ^-2*' yr' ^-1*')')) +
  ggtitle("Baseline and cover scenario N input (g m^2)") +
  scale_color_manual(labels=c("Cover Crops"),
                     values=cbPalette9[c(8)]) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "right",
        legend.key = element_blank())

gNI

ggsave(filename=paste0(results_path,"C_input_",scenario_name,"_Daycent.jpg"),plot=gCI)
ggsave(filename=paste0(results_path,"N_input_",scenario_name,"_Daycent.jpg"),plot=gNI)
