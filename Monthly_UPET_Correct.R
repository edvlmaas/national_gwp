#####################################################################
# Function: Monthly_UPET_Correct.R
# Author: Ellen Maas
# Description: Monthly correction factor needed for Thornthwaite
#              and Mather (1955) calculation. 
#####################################################################

Monthly_UPET_Correct <- function(UPET_raw, month) {
  correction <-  UPET_raw*ifelse(month==1, 24.9,
                                 ifelse(month==2, 24.9,
                                 ifelse(month==3, 30.9,
                                 ifelse(month==4, 33.3,
                                 ifelse(month==5, 37.5,
                                 ifelse(month==6, 37.8,
                                 ifelse(month==7, 38.1,
                                 ifelse(month==8, 35.7,
                                 ifelse(month==9, 31.2,
                                 ifelse(month==10, 28.8,
                                 ifelse(month==11, 24.6, 24)))))))))))
  return(correction)
}