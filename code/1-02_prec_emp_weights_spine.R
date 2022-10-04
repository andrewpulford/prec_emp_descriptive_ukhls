################################################################################

# Persistent precarious employment and health - Understanding Society
# 1-02 - Create weight spine for descriptive analysis
# Andrew Pulford

# Data source:
# University of Essex, Institute for Social and Economic Research. (2021). 
# Understanding Society: Waves 1-10, 2009-2019 and Harmonised BHPS: Waves 1-18, 
# 1991-2009. [data collection]. 13th Edition. UK Data Service. SN: 6614, 
# http://doi.org/10.5255/UKDA-SN-6614-14

#### What this script does:
# (a) Creates weight spines for analytic samples a and b

################################################################################

## remove any existing objects from global environment
rm(list=ls()) 


################################################################################
#####                            install packages                          #####
################################################################################

library(readxl) # for reading excel files
library(foreign) # for reading SPSS files
library(tidyverse) # all kinds of stuff 
library(janitor) # cleaning up


################################################################################
#####                         load and prepare data                        #####
################################################################################

var_path <- "C:/Users/0510028p/Documents/prec_emp_descriptive_ukhls/variables/"
data_path <- "C:/Users/0510028p/Documents/UKDA-6614-spss/spss/spss25/"

################################################################################
#####                         weights for waves 3-6                        ##### 
################################################################################

temp_df <- read.spss(paste0(data_path,"ukhls_w6/f_indresp.sav"), 
                     to.data.frame=TRUE, use.value.labels=TRUE) %>% 
  as_tibble()

weight_spine_a  <- temp_df %>% 
  dplyr::select(pidp, f_indinub_xw) %>% 
  mutate(weight_flag = ifelse(f_indinub_xw!=0,1,0))

rm(temp_df)

## save spine for analytic sample a
write_rds(weight_spine_a, "./look_ups/weights_spine_a.rds")

################################################################################
#####                        weights for waves 7-10                        ##### 
################################################################################

temp_df <- read.spss(paste0(data_path,"ukhls_w10/j_indresp.sav"), 
                     to.data.frame=TRUE, use.value.labels=TRUE) %>% 
  as_tibble()

weight_spine_b  <- temp_df %>% 
  dplyr::select(pidp, j_indinui_xw) %>% 
  mutate(weight_flag = ifelse(j_indinui_xw!=0,1,0))

rm(temp_df)

## save spine for analytic sample b
write_rds(weight_spine_b, "./look_ups/weights_spine_b.rds")

