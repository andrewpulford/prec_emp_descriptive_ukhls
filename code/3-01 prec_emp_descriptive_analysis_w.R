################################################################################

# Persistent precarious employment and health - Understanding Society
# 3-01 - descriptive analysis - weighted
# Andrew Pulford

# Data source:
# University of Essex, Institute for Social and Economic Research. (2021). 
# Understanding Society: Waves 1-10, 2009-2019 and Harmonised BHPS: Waves 1-18, 
# 1991-2009. [data collection]. 13th Edition. UK Data Service. SN: 6614, 
# http://doi.org/10.5255/UKDA-SN-6614-14

#### What this script does:
# (a) Split into two samples (waves 3-6 and 7-10)
#
# (b) 
#
# (c) 
#
# (d) 
#       
# (e) 

################################################################################

## remove any existing objects from global environment
rm(list=ls()) 


################################################################################
#####                            install packages                          #####
################################################################################

library(tidyverse) # all kinds of stuff 
library(broom) # for working with statistical outputs
library(TraMineR) # for sequence analysis
library(poLCA) # for latent class analysis
#library(randomLCA) # for repeated measures latent class analysis
library(survey) # for applying survey weights to analysis
options(survey.lonely.psu="adjust") # single-PSU strata are centred at the sample mean
library(tableone) # for creating Table 1

#library(srvyr) # for applying survey weights to analysis

#citation("TraMineR")

################################################################################
#####                         load and prepare data                        #####
################################################################################

#### load dataframes --------------------

### analytic sample 1a - waves 3-6
dfas1a <- readRDS("./analytic_sample_data/dfas1a.rds") %>% 
  mutate(sample_group = "a")# %>% select(-valid_6)

## endpoint only <<<<<< change to read
dfas1a_end <- readRDS("./working_data/dfas1a_end.rds")

# drop unused levels, reorder etc
dfas1a_end$sex_dv <- droplevels(dfas1a_end$sex_dv)
dfas1a_end$hiqual_dv <- droplevels(dfas1a_end$hiqual_dv)
dfas1a_end$gor_dv <- droplevels(dfas1a_end$gor_dv)
dfas1a_end$fimnnet_dv <- as.character(dfas1a_end$fimnnet_dv)
dfas1a_end$fimnnet_dv <- as.numeric(dfas1a_end$fimnnet_dv)

dfas1a_end$srh_dv <- factor(dfas1a_end$srh_dv, 
                           levels = c("excellent", "very good", "good",
                                      "fair", "poor"))

### analytic sample 1b - waves 7-10
dfas1b <- readRDS("./analytic_sample_data/dfas1b.rds") %>% 
  mutate(sample_group = "b")# %>% select(-valid_10)

## endpoint only <<<<<< change to read
dfas1b_end <- readRDS("./working_data/dfas1b_end.rds")


# drop unused levels, reorder factors etc
dfas1b_end$sex_dv <- droplevels(dfas1b_end$sex_dv)
dfas1b_end$hiqual_dv <- droplevels(dfas1b_end$hiqual_dv)
dfas1b_end$gor_dv <- droplevels(dfas1b_end$gor_dv)
dfas1b_end$fimnnet_dv <- as.character(dfas1b_end$fimnnet_dv)
dfas1b_end$fimnnet_dv <- as.numeric(dfas1b_end$fimnnet_dv)

dfas1b_end$srh_dv <- factor(dfas1b_end$srh_dv, 
                            levels = c("excellent", "very good", "good",
                                       "fair", "poor"))


#trim any white space
dfas1a_end$sf12pcs_dv <- trimws(dfas1a_end$sf12pcs_dv)
dfas1a_end$sf12mcs_dv <- trimws(dfas1a_end$sf12mcs_dv)

##sort out SF12 variables 
dfas1a_end <- dfas1a_end %>% 
  mutate(sf12pcs_dv = ifelse(sf12pcs_dv %in% c("missing", "inapplicable", 
                                               "proxy", "refusal", "don't know"),
                             NA,sf12pcs_dv),
         sf12mcs_dv = ifelse(sf12mcs_dv %in% c("missing", "inapplicable", 
                                               "proxy", "refusal", "don't know"),
                             NA,sf12mcs_dv)) 


# change to numeric
dfas1a_end$sf12pcs_dv <- as.numeric(dfas1a_end$sf12pcs_dv,  na.rm = TRUE)
dfas1a_end$sf12mcs_dv <- as.numeric(dfas1a_end$sf12mcs_dv,  na.rm = TRUE)

### sample B ---------
dfas1b_end <- readRDS("./working_data/dfas1b_end.rds")

#trim any white space
dfas1b_end$sf12pcs_dv <- trimws(dfas1b_end$sf12pcs_dv)
dfas1b_end$sf12mcs_dv <- trimws(dfas1b_end$sf12mcs_dv)

##sort out SF12 variables 
dfas1b_end <- dfas1b_end %>% 
  mutate(sf12pcs_dv = ifelse(sf12pcs_dv %in% c("missing", "inapplicable", 
                                               "proxy", "refusal", "don't know"),
                             NA,sf12pcs_dv),
         sf12mcs_dv = ifelse(sf12mcs_dv %in% c("missing", "inapplicable", 
                                               "proxy", "refusal", "don't know"),
                             NA,sf12pcs_dv)) 

# change to char then numeric
dfas1b_end$sf12pcs_dv <- as.numeric(dfas1b_end$sf12pcs_dv,  na.rm = TRUE)
dfas1b_end$sf12mcs_dv <- as.numeric(dfas1b_end$sf12mcs_dv,  na.rm = TRUE)


#### load weight spines ---------------------------------

weight_spine_a <- readRDS("./look_ups/weights_spine_a.rds") %>% 
  dplyr::select(pidp, strata, psu)

weight_spine_b <- readRDS("./look_ups/weights_spine_b.rds") %>% 
  dplyr::select(pidp, strata, psu)

#### join dfs and weight spines -------------------------

dfas1a_end <- dfas1a_end %>% 
  left_join(weight_spine_a)

dfas1b_end <- dfas1b_end %>% 
  left_join(weight_spine_b)

#### combined study endpoint df <<< prob can't do as different weights/samples <<<
#dfas1_end <- dfas1a_end %>% bind_rows(dfas1b_end)


#### create complex sample design dfs -------------------

svy_dfas1a_end <- svydesign(id=~psu, strata=~strata,
                         weights=~indinub_xw, data=dfas1a_end)

svy_dfas1b_end <- svydesign(id=~psu, strata=~strata,
                            weights=~indinui_xw, data=dfas1b_end)

### check missing values are set to NA

#missval <- c(-9, -8, -7, -2, -1)
#
#for (i in 1:5) {
#  dfas1a_end <- dfas1a_end %>% mutate_all(., list(~na_if(.,
#                                                       missval[i])))
#}

# seems like this could be done easier with mutate() and ifelse()

#dfas1a_end %>% mutate(across(ifelse(. %in% c(-9, -8, -7, -2, -1), 1,0)))
# doesn't work either

################################################################################
#####                sample characteristics at study endpoint              #####
################################################################################


#####----------------------------------------------------------------------#####
#####                     Personal characteristics                         #####
#####----------------------------------------------------------------------#####

#### sex -----------------------------------------------------------------------

### sample A ------------

## calculate proportions
sex_a <- data.frame(svymean(~sex_dv, svy_dfas1a_end))
sex_a <- cbind(rownames(sex_a),sex_a, row.names=NULL)
sex_a$`rownames(sex_a)` <- str_replace(sex_a$`rownames(sex_a)`, "sex_dv","")
sex_a <- sex_a %>% rename(measure = `rownames(sex_a)`)
names(sex_a) <- tolower(names(sex_a)) # change all col names to lower case

## calculate totals
sex2_a <- data.frame(svytotal(~sex_dv, svy_dfas1a_end))
sex2_a <- sex2_a %>% dplyr::select(-SE)
sex2_a <- cbind(rownames(sex2_a),sex2_a, row.names=NULL)
sex2_a$`rownames(sex2_a)` <- str_replace(sex2_a$`rownames(sex2_a)`, "sex_dv","")
sex2_a <- sex2_a %>% rename(measure = `rownames(sex2_a)`)
sex2_a$total <- as.integer(sex2_a$total)

## join together and format
sex_a <- sex_a %>%
  left_join(sex2_a) %>% 
  mutate(est = mean*100,
         var="Sex",
         wv_n=6) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, measure, n, est, se) %>% 
  arrange(wv_n, factor(measure, levels = c("Female","Male")))

sample_chars_endpoint <- sex_a

### sample B ------------

## calculate proportions
sex_b <- data.frame(svymean(~sex_dv, svy_dfas1b_end))
sex_b <- cbind(rownames(sex_b),sex_b, row.names=NULL)
sex_b$`rownames(sex_b)` <- str_replace(sex_b$`rownames(sex_b)`, "sex_dv","")
sex_b <- sex_b %>% rename(measure = `rownames(sex_b)`)
names(sex_b) <- tolower(names(sex_b)) # change all col names to lower case

## calculate totals
sex2_b <- data.frame(svytotal(~sex_dv, svy_dfas1b_end))
sex2_b <- sex2_b %>% dplyr::select(-SE)
sex2_b <- cbind(rownames(sex2_b),sex2_b, row.names=NULL)
sex2_b$`rownames(sex2_b)` <- str_replace(sex2_b$`rownames(sex2_b)`, "sex_dv","")
sex2_b <- sex2_b %>% rename(measure = `rownames(sex2_b)`)
sex2_b$total <- as.integer(sex2_b$total)

## join together and format
sex_b <- sex_b %>%
  left_join(sex2_b) %>% 
  mutate(est = mean*100,
         var="Sex",
         wv_n=10) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, measure, n, est, se) %>% 
  arrange(wv_n, factor(measure, levels = c("Female","Male")))

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(sex_b)

#### age -----------------------------------------------------------------------

### sample A ------------

age_mean_a <- svyby(~age_dv, ~wv_n,svy_dfas1a_end, svymean, na.rm=TRUE)

age_mean_a <- age_mean_a %>% 
  rename(est = age_dv) %>% 
  mutate(var="Age", measure="Mean", n=NA) %>% 
  dplyr::select(wv_n, var, measure, n, est, se)

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(age_mean_a)

### sample B ------------

age_mean_b <- svyby(~age_dv, ~wv_n,svy_dfas1b_end, svymean, na.rm=TRUE)

age_mean_b <- age_mean_b %>% 
  rename(est = age_dv) %>% 
  mutate(var="Age", measure="Mean", n=NA) %>% 
  dplyr::select(wv_n, var, measure, n, est, se)

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(age_mean_b)

#### ethnicity -----------------------------------------------------------------
## white/non-white

### sample A ------------

## calculate proportions
white_non_a <- data.frame(svymean(~non_white, svy_dfas1a_end))
white_non_a <- cbind(rownames(white_non_a),white_non_a, row.names=NULL)
white_non_a$`rownames(white_non_a)` <- str_replace(white_non_a$`rownames(white_non_a)`, "non_white","")
white_non_a <- white_non_a %>% rename(measure = `rownames(white_non_a)`)
names(white_non_a) <- tolower(names(white_non_a)) # change all col names to lower case

## calculate totals
white_non2_a <- data.frame(svytotal(~non_white, svy_dfas1a_end))
white_non2_a <- white_non2_a %>% dplyr::select(-SE)
white_non2_a <- cbind(rownames(white_non2_a),white_non2_a, row.names=NULL)
white_non2_a$`rownames(white_non2_a)` <- str_replace(white_non2_a$`rownames(white_non2_a)`, "non_white","")
white_non2_a <- white_non2_a %>% rename(measure = `rownames(white_non2_a)`)
white_non2_a$total <- as.integer(white_non2_a$total)

## join together and format
white_non_a <- white_non_a %>%
  left_join(white_non2_a) %>% 
  mutate(est = mean*100,
         var="Ethnicity",
         wv_n=6) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, measure, n, est, se) %>% 
  arrange(wv_n, factor(measure, levels = c("White","Non-white","Missing")))

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(white_non_a)

### sample B ------------

## calculate proportions
white_non_b <- data.frame(svymean(~non_white, svy_dfas1b_end))
white_non_b <- cbind(rownames(white_non_b),white_non_b, row.names=NULL)
white_non_b$`rownames(white_non_b)` <- str_replace(white_non_b$`rownames(white_non_b)`, "non_white","")
white_non_b <- white_non_b %>% rename(measure = `rownames(white_non_b)`)
names(white_non_b) <- tolower(names(white_non_b)) # change all col names to lower case

## calculate totals
white_non2_b <- data.frame(svytotal(~non_white, svy_dfas1b_end))
white_non2_b <- white_non2_b %>% dplyr::select(-SE)
white_non2_b <- cbind(rownames(white_non2_b),white_non2_b, row.names=NULL)
white_non2_b$`rownames(white_non2_b)` <- str_replace(white_non2_b$`rownames(white_non2_b)`, "non_white","")
white_non2_b <- white_non2_b %>% rename(measure = `rownames(white_non2_b)`)
white_non2_b$total <- as.integer(white_non2_b$total)

## join together and format
white_non_b <- white_non_b %>%
  left_join(white_non2_b) %>% 
  mutate(est = mean*100,
         var="Ethnicity",
         wv_n=10) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, measure, n, est, se)  %>% 
  arrange(wv_n, factor(measure, levels = c("White","Non-white","Missing")))

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(white_non_b)


#### Marital status -------------------------------------------------------------
### sample A ------------

## calculate proportions
marital_a <- data.frame(svymean(~marital_status, svy_dfas1a_end))
marital_a <- cbind(rownames(marital_a),marital_a, row.names=NULL)
marital_a$`rownames(marital_a)` <- str_replace(marital_a$`rownames(marital_a)`, "marital_status","")
marital_a <- marital_a %>% rename(measure = `rownames(marital_a)`)
names(marital_a) <- tolower(names(marital_a)) # change all col names to lower case

## calculate totals
marital2_a <- data.frame(svytotal(~marital_status, svy_dfas1a_end))
marital2_a <- marital2_a %>% dplyr::select(-SE)
marital2_a <- cbind(rownames(marital2_a),marital2_a, row.names=NULL)
marital2_a$`rownames(marital2_a)` <- str_replace(marital2_a$`rownames(marital2_a)`, "marital_status","")
marital2_a <- marital2_a %>% rename(measure = `rownames(marital2_a)`)
marital2_a$total <- as.integer(marital2_a$total)

## join together and format
marital_a <- marital_a %>%
  left_join(marital2_a) %>% 
  mutate(est = mean*100,
         var="Marital status",
         wv_n=6) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, measure, n, est, se) %>% 
  arrange(wv_n, factor(measure, levels = c("married/civil partnership","divorced/separated/widowed","single","missing")))

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(marital_a)

### sample B ------------

## calculate proportions
marital_b <- data.frame(svymean(~marital_status, svy_dfas1b_end))
marital_b <- cbind(rownames(marital_b),marital_b, row.names=NULL)
marital_b$`rownames(marital_b)` <- str_replace(marital_b$`rownames(marital_b)`, "marital_status","")
marital_b <- marital_b %>% rename(measure = `rownames(marital_b)`)
names(marital_b) <- tolower(names(marital_b)) # change all col names to lower case

## calculate totals
marital2_b <- data.frame(svytotal(~marital_status, svy_dfas1b_end))
marital2_b <- marital2_b %>% dplyr::select(-SE)
marital2_b <- cbind(rownames(marital2_b),marital2_b, row.names=NULL)
marital2_b$`rownames(marital2_b)` <- str_replace(marital2_b$`rownames(marital2_b)`, "marital_status","")
marital2_b <- marital2_b %>% rename(measure = `rownames(marital2_b)`)
marital2_b$total <- as.integer(marital2_b$total)

## join together and format
marital_b <- marital_b %>%
  left_join(marital2_b) %>% 
  mutate(est = mean*100,
         var="Marital status",
         wv_n=10) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, measure, n, est, se)  %>% 
  arrange(wv_n, factor(measure, levels = c("married/civil partnership","divorced/separated/widowed","single","missing")))

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(marital_b)


#### Educational attainment ----------------------------------------------------

### sample A ------------

## calculate proportions
ed_attain_a <- data.frame(svymean(~hiqual_dv, svy_dfas1a_end))
ed_attain_a <- cbind(rownames(ed_attain_a),ed_attain_a, row.names=NULL)
ed_attain_a$`rownames(ed_attain_a)` <- str_replace(ed_attain_a$`rownames(ed_attain_a)`, "hiqual_dv","")
ed_attain_a <- ed_attain_a %>% rename(measure = `rownames(ed_attain_a)`)
names(ed_attain_a) <- tolower(names(ed_attain_a)) # change all col names to lower case

## calculate totals
ed_attain2_a <- data.frame(svytotal(~hiqual_dv, svy_dfas1a_end))
ed_attain2_a <- ed_attain2_a %>% dplyr::select(-SE)
ed_attain2_a <- cbind(rownames(ed_attain2_a),ed_attain2_a, row.names=NULL)
ed_attain2_a$`rownames(ed_attain2_a)` <- str_replace(ed_attain2_a$`rownames(ed_attain2_a)`, "hiqual_dv","")
ed_attain2_a <- ed_attain2_a %>% rename(measure = `rownames(ed_attain2_a)`)
ed_attain2_a$total <- as.integer(ed_attain2_a$total)

## join together and format
ed_attain_a <- ed_attain_a %>%
  left_join(ed_attain2_a) %>% 
  mutate(est = mean*100,
         var="Educational attainment",
         wv_n=6) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, measure, n, est, se) %>% 
  arrange(wv_n, factor(measure, levels = c("degree",
                                           "other higher degree",
                                           "a-level etc",
                                           "gcse etc",
                                           "other qualification",
                                           "no qualification")))

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(ed_attain_a)

### sample B ------------

## calculate proportions
ed_attain_b <- data.frame(svymean(~hiqual_dv, svy_dfas1b_end))
ed_attain_b <- cbind(rownames(ed_attain_b),ed_attain_b, row.names=NULL)
ed_attain_b$`rownames(ed_attain_b)` <- str_replace(ed_attain_b$`rownames(ed_attain_b)`, "hiqual_dv","")
ed_attain_b <- ed_attain_b %>% rename(measure = `rownames(ed_attain_b)`)
names(ed_attain_b) <- tolower(names(ed_attain_b)) # change all col names to lower case

## calculate totals
ed_attain2_b <- data.frame(svytotal(~hiqual_dv, svy_dfas1b_end))
ed_attain2_b <- ed_attain2_b %>% dplyr::select(-SE)
ed_attain2_b <- cbind(rownames(ed_attain2_b),ed_attain2_b, row.names=NULL)
ed_attain2_b$`rownames(ed_attain2_b)` <- str_replace(ed_attain2_b$`rownames(ed_attain2_b)`, "hiqual_dv","")
ed_attain2_b <- ed_attain2_b %>% rename(measure = `rownames(ed_attain2_b)`)
ed_attain2_b$total <- as.integer(ed_attain2_b$total)

## join together and format
ed_attain_b <- ed_attain_b %>%
  left_join(ed_attain2_b) %>% 
  mutate(est = mean*100,
         var="Educational attainment",
         wv_n=10) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, measure, n, est, se)  %>% 
  arrange(wv_n, factor(measure, levels = c("degree",
                                           "other higher degree",
                                           "a-level etc",
                                           "gcse etc",
                                           "other qualification",
                                           "no qualification")))

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(ed_attain_b)


#### Region --------------------------------------------------------------------

### sample A ------------

## calculate proportions
region_a <- data.frame(svymean(~gor_dv, svy_dfas1a_end))
region_a <- cbind(rownames(region_a),region_a, row.names=NULL)
region_a$`rownames(region_a)` <- str_replace(region_a$`rownames(region_a)`, "gor_dv","")
region_a <- region_a %>% rename(measure = `rownames(region_a)`)
names(region_a) <- tolower(names(region_a)) # change all col names to lower case

## calculate totals
region2_a <- data.frame(svytotal(~gor_dv, svy_dfas1a_end))
region2_a <- region2_a %>% dplyr::select(-SE)
region2_a <- cbind(rownames(region2_a),region2_a, row.names=NULL)
region2_a$`rownames(region2_a)` <- str_replace(region2_a$`rownames(region2_a)`, "gor_dv","")
region2_a <- region2_a %>% rename(measure = `rownames(region2_a)`)
region2_a$total <- as.integer(region2_a$total)

## join together and format
region_a <- region_a %>%
  left_join(region2_a) %>% 
  mutate(est = mean*100,
         var="Region",
         wv_n=6) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, measure, n, est, se) #%>% 
#  arrange(wv_n, factor(measure, levels = c("degree",
#                                           "other higher degree",
#                                           "a-level etc",
#                                           "gcse etc",
#                                           "other qualification",
#                                           "no qualification")))

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(region_a)

### sample B ------------

## calculate proportions
region_b <- data.frame(svymean(~gor_dv, svy_dfas1b_end))
region_b <- cbind(rownames(region_b),region_b, row.names=NULL)
region_b$`rownames(region_b)` <- str_replace(region_b$`rownames(region_b)`, "gor_dv","")
region_b <- region_b %>% rename(measure = `rownames(region_b)`)
names(region_b) <- tolower(names(region_b)) # change all col names to lower case

## calculate totals
region2_b <- data.frame(svytotal(~gor_dv, svy_dfas1b_end))
region2_b <- region2_b %>% dplyr::select(-SE)
region2_b <- cbind(rownames(region2_b),region2_b, row.names=NULL)
region2_b$`rownames(region2_b)` <- str_replace(region2_b$`rownames(region2_b)`, "gor_dv","")
region2_b <- region2_b %>% rename(measure = `rownames(region2_b)`)
region2_b$total <- as.integer(region2_b$total)

## join together and format
region_b <- region_b %>%
  left_join(region2_b) %>% 
  mutate(est = mean*100,
         var="Region",
         wv_n=10) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, measure, n, est, se)  #%>% 
#  arrange(wv_n, factor(measure, levels = c("degree",
#                                           "other higher degree",
#                                           "a-level etc",
#                                           "gcse etc",
#                                           "other qualification",
#                                           "no qualification")))

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(region_b)


#####----------------------------------------------------------------------#####
#####               Employment and income characteristics                  #####
#####----------------------------------------------------------------------#####

#### permanent or temporary ----------------------------------------------------

### sample A ------------

## calculate proportions
perm_emp_a <- data.frame(svymean(~emp_contract, svy_dfas1a_end))
perm_emp_a <- cbind(rownames(perm_emp_a),perm_emp_a, row.names=NULL)
perm_emp_a$`rownames(perm_emp_a)` <- str_replace(perm_emp_a$`rownames(perm_emp_a)`, "emp_contract","")
perm_emp_a <- perm_emp_a %>% rename(measure = `rownames(perm_emp_a)`)
names(perm_emp_a) <- tolower(names(perm_emp_a)) # change all col names to lower case

## calculate totals
perm_emp2_a <- data.frame(svytotal(~emp_contract, svy_dfas1a_end))
perm_emp2_a <- perm_emp2_a %>% dplyr::select(-SE)
perm_emp2_a <- cbind(rownames(perm_emp2_a),perm_emp2_a, row.names=NULL)
perm_emp2_a$`rownames(perm_emp2_a)` <- str_replace(perm_emp2_a$`rownames(perm_emp2_a)`, "emp_contract","")
perm_emp2_a <- perm_emp2_a %>% rename(measure = `rownames(perm_emp2_a)`)
perm_emp2_a$total <- as.integer(perm_emp2_a$total)

## join together and format
perm_emp_a <- perm_emp_a %>%
  left_join(perm_emp2_a) %>% 
  mutate(est = mean*100,
         var="Marital status",
         wv_n=6) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, measure, n, est, se) %>% 
  arrange(wv_n, factor(measure, levels = c("fixed-term",
                                           "permanent",
                                           "unemployed/not in employment")))

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(perm_emp_a)

### sample B ------------

## calculate proportions
perm_emp_b <- data.frame(svymean(~emp_contract, svy_dfas1b_end))
perm_emp_b <- cbind(rownames(perm_emp_b),perm_emp_b, row.names=NULL)
perm_emp_b$`rownames(perm_emp_b)` <- str_replace(perm_emp_b$`rownames(perm_emp_b)`, "emp_contract","")
perm_emp_b <- perm_emp_b %>% rename(measure = `rownames(perm_emp_b)`)
names(perm_emp_b) <- tolower(names(perm_emp_b)) # change all col names to lower case

## calculate totals
perm_emp2_b <- data.frame(svytotal(~emp_contract, svy_dfas1b_end))
perm_emp2_b <- perm_emp2_b %>% dplyr::select(-SE)
perm_emp2_b <- cbind(rownames(perm_emp2_b),perm_emp2_b, row.names=NULL)
perm_emp2_b$`rownames(perm_emp2_b)` <- str_replace(perm_emp2_b$`rownames(perm_emp2_b)`, "emp_contract","")
perm_emp2_b <- perm_emp2_b %>% rename(measure = `rownames(perm_emp2_b)`)
perm_emp2_b$total <- as.integer(perm_emp2_b$total)

## join together and format
perm_emp_b <- perm_emp_b %>%
  left_join(perm_emp2_b) %>% 
  mutate(est = mean*100,
         var="Marital status",
         wv_n=10) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, measure, n, est, se)  %>% 
  arrange(wv_n, factor(measure, levels = c("fixed-term",
                                           "permanent",
                                           "unemployed/not in employment")))

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(perm_emp_b)


#### Employment spells since last interview ------------------------------------

### sample A ------------

## calculate proportions
emp_broken_a <- data.frame(svymean(~broken_emp, svy_dfas1a_end))
emp_broken_a <- cbind(rownames(emp_broken_a),emp_broken_a, row.names=NULL)
emp_broken_a$`rownames(emp_broken_a)` <- str_replace(emp_broken_a$`rownames(emp_broken_a)`, "broken_emp","")
emp_broken_a <- emp_broken_a %>% rename(measure = `rownames(emp_broken_a)`)
names(emp_broken_a) <- tolower(names(emp_broken_a)) # change all col names to lower case

## calculate totals
emp_broken2_a <- data.frame(svytotal(~broken_emp, svy_dfas1a_end))
emp_broken2_a <- emp_broken2_a %>% dplyr::select(-SE)
emp_broken2_a <- cbind(rownames(emp_broken2_a),emp_broken2_a, row.names=NULL)
emp_broken2_a$`rownames(emp_broken2_a)` <- str_replace(emp_broken2_a$`rownames(emp_broken2_a)`, "broken_emp","")
emp_broken2_a <- emp_broken2_a %>% rename(measure = `rownames(emp_broken2_a)`)
emp_broken2_a$total <- as.integer(emp_broken2_a$total)

## join together and format
emp_broken_a <- emp_broken_a %>%
  left_join(emp_broken2_a) %>% 
  mutate(est = mean*100,
         var="Broken employment",
         wv_n=6) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, measure, n, est, se) %>% 
  arrange(wv_n, factor(measure, levels = c("unbroken employment",
                                           "broken employment",
                                           "no employment spells")))

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(emp_broken_a)

### sample B ------------

## calculate proportions
emp_broken_b <- data.frame(svymean(~broken_emp, svy_dfas1b_end))
emp_broken_b <- cbind(rownames(emp_broken_b),emp_broken_b, row.names=NULL)
emp_broken_b$`rownames(emp_broken_b)` <- str_replace(emp_broken_b$`rownames(emp_broken_b)`, "broken_emp","")
emp_broken_b <- emp_broken_b %>% rename(measure = `rownames(emp_broken_b)`)
names(emp_broken_b) <- tolower(names(emp_broken_b)) # change all col names to lower case

## calculate totals
emp_broken2_b <- data.frame(svytotal(~broken_emp, svy_dfas1b_end))
emp_broken2_b <- emp_broken2_b %>% dplyr::select(-SE)
emp_broken2_b <- cbind(rownames(emp_broken2_b),emp_broken2_b, row.names=NULL)
emp_broken2_b$`rownames(emp_broken2_b)` <- str_replace(emp_broken2_b$`rownames(emp_broken2_b)`, "broken_emp","")
emp_broken2_b <- emp_broken2_b %>% rename(measure = `rownames(emp_broken2_b)`)
emp_broken2_b$total <- as.integer(emp_broken2_b$total)

## join together and format
emp_broken_b <- emp_broken_b %>%
  left_join(emp_broken2_b) %>% 
  mutate(est = mean*100,
         var="Broken employment",
         wv_n=10) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, measure, n, est, se)  %>% 
  arrange(wv_n, factor(measure, levels = c("unbroken employment",
                                           "broken employment",
                                           "no employment spells")))

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(emp_broken_b)


#### perceived job security in the next 12 months ------------------------------
# even # waves only

# reorder jbsec_dv variable
#dfas1_end$jbsec_dv <- factor(dfas1_end$jbsec_dv, 
#                             levels = c("very likely",
#                                        "likely",      
#                                        "unlikely",
#                                        "very unlikely", 
#                                        "missing"))

# summary df
#job_sec <- dfas1_end %>% group_by(wv_n, jbsec_dv) %>% summarise(n=n()) %>% 
#  mutate(est = n/sum(n)*100) %>% 
#  mutate(var="Perceived job security") %>% 
#  rename("measure"= "jbsec_dv") %>% 
#  dplyr::select(wv_n,var, measure, n, est)

#sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(job_sec)

#### Multiple jobs -------------------------------------------------------------
### has a 2nd job ----
### sample A ------------

## calculate proportions
emp_2nd_a <- data.frame(svymean(~j2has_dv2, svy_dfas1a_end))
emp_2nd_a <- cbind(rownames(emp_2nd_a),emp_2nd_a, row.names=NULL)
emp_2nd_a$`rownames(emp_2nd_a)` <- str_replace(emp_2nd_a$`rownames(emp_2nd_a)`, "j2has_dv2","")
emp_2nd_a <- emp_2nd_a %>% rename(measure = `rownames(emp_2nd_a)`)
names(emp_2nd_a) <- tolower(names(emp_2nd_a)) # change all col names to lower case

## calculate totals
emp_2nd2_a <- data.frame(svytotal(~j2has_dv2, svy_dfas1a_end))
emp_2nd2_a <- emp_2nd2_a %>% dplyr::select(-SE)
emp_2nd2_a <- cbind(rownames(emp_2nd2_a),emp_2nd2_a, row.names=NULL)
emp_2nd2_a$`rownames(emp_2nd2_a)` <- str_replace(emp_2nd2_a$`rownames(emp_2nd2_a)`, "j2has_dv2","")
emp_2nd2_a <- emp_2nd2_a %>% rename(measure = `rownames(emp_2nd2_a)`)
emp_2nd2_a$total <- as.integer(emp_2nd2_a$total)

## join together and format
emp_2nd_a <- emp_2nd_a %>%
  left_join(emp_2nd2_a) %>% 
  mutate(est = mean*100,
         var="Multiple jobs",
         wv_n=6) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, measure, n, est, se) %>% 
  arrange(wv_n, factor(measure, levels = c("multiple jobs",
                                           "one job",
                                           "unemployed/not in employment")))

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(emp_2nd_a)

### sample B ------------

## calculate proportions
emp_2nd_b <- data.frame(svymean(~j2has_dv2, svy_dfas1b_end))
emp_2nd_b <- cbind(rownames(emp_2nd_b),emp_2nd_b, row.names=NULL)
emp_2nd_b$`rownames(emp_2nd_b)` <- str_replace(emp_2nd_b$`rownames(emp_2nd_b)`, "j2has_dv2","")
emp_2nd_b <- emp_2nd_b %>% rename(measure = `rownames(emp_2nd_b)`)
names(emp_2nd_b) <- tolower(names(emp_2nd_b)) # change all col names to lower case

## calculate totals
emp_2nd2_b <- data.frame(svytotal(~j2has_dv2, svy_dfas1b_end))
emp_2nd2_b <- emp_2nd2_b %>% dplyr::select(-SE)
emp_2nd2_b <- cbind(rownames(emp_2nd2_b),emp_2nd2_b, row.names=NULL)
emp_2nd2_b$`rownames(emp_2nd2_b)` <- str_replace(emp_2nd2_b$`rownames(emp_2nd2_b)`, "j2has_dv2","")
emp_2nd2_b <- emp_2nd2_b %>% rename(measure = `rownames(emp_2nd2_b)`)
emp_2nd2_b$total <- as.integer(emp_2nd2_b$total)

## join together and format
emp_2nd_b <- emp_2nd_b %>%
  left_join(emp_2nd2_b) %>% 
  mutate(est = mean*100,
         var="Multiple jobs",
         wv_n=10) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, measure, n, est, se)  %>% 
  arrange(wv_n, factor(measure, levels = c("multiple jobs",
                                           "one job",
                                           "unemployed/not in employment")))

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(emp_2nd_b)


#### income --------------------------------------------------------------------
## total net personal income (check what used in COVID modelling)
# check monthly?

### sample A ------------

inc_quantile_a <- data.frame(svyquantile(~fimnnet_dv,svy_dfas1a_end, 
                                         quantile=c(0.25,0.5,0.75), ci=FALSE))

inc_quantile_a <- cbind(rownames(inc_quantile_a),inc_quantile_a, row.names=NULL)
inc_quantile_a <- inc_quantile_a %>% 
  rename(var = `rownames(inc_quantile_a)`) %>% 
  pivot_longer(cols=2:4, names_to = "measure") %>% 
  rename(est = value) %>% 
  mutate(wv_n = 6,
         n=NA,
         se=NA,
         var="Monthly net income (£)",
         measure = ifelse(measure == "X0.25","25% quantile",
                          ifelse(measure == "X0.5","Median",
                                 ifelse(measure == "X0.75", "75% quantile",
                                        "CHECK")))) %>% 
  dplyr::select(wv_n, var, measure, n, est, se)




sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(inc_quantile_a)

### sample B ------------

inc_quantile_b <- data.frame(svyquantile(~fimnnet_dv,svy_dfas1b_end, 
                                         quantile=c(0.25,0.5,0.75), ci=FALSE))

inc_quantile_b <- cbind(rownames(inc_quantile_b),inc_quantile_b, row.names=NULL)
inc_quantile_b <- inc_quantile_b %>% 
  rename(var = `rownames(inc_quantile_b)`) %>% 
  pivot_longer(cols=2:4, names_to = "measure") %>% 
  rename(est = value) %>% 
  mutate(wv_n = 10,
         n=NA,
         se=NA,
         var="Monthly net income (£)",
         measure = ifelse(measure == "X0.25","25% quantile",
                          ifelse(measure == "X0.5","Median",
                                 ifelse(measure == "X0.75", "75% quantile",
                                        "CHECK")))) %>% 
  dplyr::select(wv_n, var, measure, n, est, se)




sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(inc_quantile_b)

#####----------------------------------------------------------------------#####
#####                        Health characteristics                        #####
#####----------------------------------------------------------------------#####


#### self-rated health ---------------------------------------------------------

### sample A ------------

## calculate proportions
srh_a <- data.frame(svymean(~srh_dv, svy_dfas1a_end))
srh_a <- cbind(rownames(srh_a),srh_a, row.names=NULL)
srh_a$`rownames(srh_a)` <- str_replace(srh_a$`rownames(srh_a)`, "srh_dv","")
srh_a <- srh_a %>% rename(measure = `rownames(srh_a)`)
names(srh_a) <- tolower(names(srh_a)) # change all col names to lower case

## calculate totals
srh2_a <- data.frame(svytotal(~srh_dv, svy_dfas1a_end))
srh2_a <- srh2_a %>% dplyr::select(-SE)
srh2_a <- cbind(rownames(srh2_a),srh2_a, row.names=NULL)
srh2_a$`rownames(srh2_a)` <- str_replace(srh2_a$`rownames(srh2_a)`, "srh_dv","")
srh2_a <- srh2_a %>% rename(measure = `rownames(srh2_a)`)
srh2_a$total <- as.integer(srh2_a$total)

## join together and format
srh_a <- srh_a %>%
  left_join(srh2_a) %>% 
  mutate(est = mean*100,
         var="Self-rated health",
         wv_n=6) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, measure, n, est, se)

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(srh_a)

### sample B ------------

## calculate proportions
srh_b <- data.frame(svymean(~srh_dv, svy_dfas1b_end))
srh_b <- cbind(rownames(srh_b),srh_b, row.names=NULL)
srh_b$`rownames(srh_b)` <- str_replace(srh_b$`rownames(srh_b)`, "srh_dv","")
srh_b <- srh_b %>% rename(measure = `rownames(srh_b)`)
names(srh_b) <- tolower(names(srh_b)) # change all col names to lower case

## calculate totals
srh2_b <- data.frame(svytotal(~srh_dv, svy_dfas1b_end))
srh2_b <- srh2_b %>% dplyr::select(-SE)
srh2_b <- cbind(rownames(srh2_b),srh2_b, row.names=NULL)
srh2_b$`rownames(srh2_b)` <- str_replace(srh2_b$`rownames(srh2_b)`, "srh_dv","")
srh2_b <- srh2_b %>% rename(measure = `rownames(srh2_b)`)
srh2_b$total <- as.integer(srh2_b$total)

## join together and format
srh_b <- srh_b %>%
  left_join(srh2_b) %>% 
  mutate(est = mean*100,
         var="Self-rated health",
         wv_n=10) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, measure, n, est, se)

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(srh_b)

#### SF-12 physical component summary -------------------------------------------- 
### sample A ------------

sf12pcs_a <- svyby(~sf12pcs_dv, ~wv_n,svy_dfas1a_end, svymean, na.rm=TRUE)

sf12pcs_a <- sf12pcs_a %>% 
  rename(est = sf12pcs_dv) %>% 
  mutate(var="SF-12 physical component score", measure="Mean", n=NA) %>% 
  dplyr::select(wv_n, var, measure, n, est, se)

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(sf12pcs_a)


### sample B ------------

sf12pcs_b <- svyby(~sf12pcs_dv, ~wv_n,svy_dfas1b_end, svymean, na.rm=TRUE)

sf12pcs_b <- sf12pcs_b %>% 
  rename(est = sf12pcs_dv) %>% 
  mutate(var="SF-12 physical component score", measure="Mean", n=NA) %>% 
  dplyr::select(wv_n, var, measure, n, est, se)

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(sf12pcs_b)

#### GHQ-12 --------------------------------------------------------------------

### sample A ------------

## calculate proportions
ghq4_a <- data.frame(svymean(~ghq_case4, svy_dfas1a_end))
ghq4_a <- cbind(rownames(ghq4_a),ghq4_a, row.names=NULL)
ghq4_a$`rownames(ghq4_a)` <- str_replace(ghq4_a$`rownames(ghq4_a)`, "ghq_case4","")
ghq4_a <- ghq4_a %>% rename(measure = `rownames(ghq4_a)`)
names(ghq4_a) <- tolower(names(ghq4_a)) # change all col names to lower case

## calculate totals
ghq42_a <- data.frame(svytotal(~ghq_case4, svy_dfas1a_end))
ghq42_a <- ghq42_a %>% dplyr::select(-SE)
ghq42_a <- cbind(rownames(ghq42_a),ghq42_a, row.names=NULL)
ghq42_a$`rownames(ghq42_a)` <- str_replace(ghq42_a$`rownames(ghq42_a)`, "ghq_case4","")
ghq42_a <- ghq42_a %>% rename(measure = `rownames(ghq42_a)`)
ghq42_a$total <- as.integer(ghq42_a$total)

## join together and format
ghq4_a <- ghq4_a %>%
  left_join(ghq42_a) %>% 
  mutate(est = mean*100,
         var="GHQ12 score",
         wv_n=6) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, measure, n, est, se) %>% 
  arrange(wv_n, factor(measure, levels = c("0-2",
                                           "3 or more")))

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(ghq4_a)

### sample B ------------

## calculate proportions
ghq4_b <- data.frame(svymean(~ghq_case4, svy_dfas1b_end))
ghq4_b <- cbind(rownames(ghq4_b),ghq4_b, row.names=NULL)
ghq4_b$`rownames(ghq4_b)` <- str_replace(ghq4_b$`rownames(ghq4_b)`, "ghq_case4","")
ghq4_b <- ghq4_b %>% rename(measure = `rownames(ghq4_b)`)
names(ghq4_b) <- tolower(names(ghq4_b)) # change all col names to lower case

## calculate totals
ghq42_b <- data.frame(svytotal(~ghq_case4, svy_dfas1b_end))
ghq42_b <- ghq42_b %>% dplyr::select(-SE)
ghq42_b <- cbind(rownames(ghq42_b),ghq42_b, row.names=NULL)
ghq42_b$`rownames(ghq42_b)` <- str_replace(ghq42_b$`rownames(ghq42_b)`, "ghq_case4","")
ghq42_b <- ghq42_b %>% rename(measure = `rownames(ghq42_b)`)
ghq42_b$total <- as.integer(ghq42_b$total)

## join together and format
ghq4_b <- ghq4_b %>%
  left_join(ghq42_b) %>% 
  mutate(est = mean*100,
         var="GHQ12 score",
         wv_n=10) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, measure, n, est, se) %>% 
  arrange(wv_n, factor(measure, levels = c("0-2",
                                           "3 or more"))) 

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(ghq4_b)


#### SF-12 mental component summary -------------------------------------------- 
### sample A ------------

sf12mcs_a <- svyby(~sf12mcs_dv, ~wv_n,svy_dfas1a_end, svymean, na.rm=TRUE)

sf12mcs_a <- sf12mcs_a %>% 
  rename(est = sf12mcs_dv) %>% 
  mutate(var="SF-12 mental component score", measure="Mean", n=NA) %>% 
  dplyr::select(wv_n, var, measure, n, est, se)

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(sf12mcs_a)


### sample B ------------

sf12mcs_b <- svyby(~sf12mcs_dv, ~wv_n,svy_dfas1b_end, svymean, na.rm=TRUE)

sf12mcs_b <- sf12mcs_b %>% 
  rename(est = sf12mcs_dv) %>% 
  mutate(var="SF-12 mental component score", measure="Mean", n=NA) %>% 
  dplyr::select(wv_n, var, measure, n, est, se)

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(sf12mcs_b)

##### remove missing rows
'%ni%' <- Negate("%in%")

sample_chars_endpoint <- sample_chars_endpoint %>%
  filter(measure %ni% c("missing", "inapplicable", 
                               "proxy", "refusal", "don't know",
                        "inconsistent"))

#####----------------------------------------------------------------------#####
#####                     Save sample endpoint chars data                  #####
#####----------------------------------------------------------------------#####

## as dataframe
write_rds(sample_chars_endpoint, "./working_data/weighted/sample_chars_endpoint.rds")

## as output
write_csv(sample_chars_endpoint, "./output/weighted/sample_chars_endpoint.rds")


################################################################################
#####                             Create Table 1                           #####
################################################################################

### leave for now - can't combine data if using different weights
