################################################################################

# Persistent precarious employment and health - Understanding Society
# 1-04 - create eligible pop and analytic sample dataframes
# Andrew Pulford

# Data source:
# University of Essex, Institute for Social and Economic Research. (2021). 
# Understanding Society: Waves 1-10, 2009-2019 and Harmonised BHPS: Waves 1-18, 
# 1991-2009. [data collection]. 13th Edition. UK Data Service. SN: 6614, 
# http://doi.org/10.5255/UKDA-SN-6614-14

#### What this script does:
# (a) Creates spine for non-working age individuals
# (b) Creates spine for individuals with no valid response at study endpoint (wave 6 or 10)
# (c) Creates eligible population dataframe for descriptive analysis
# (d) Creates spine for censoring deceased individuals (at wave of death)
# (e) Creates spine for censoring retired individuals (at wave of retirement)
# (f) Checks non-response across survey waves (move from analytic spine script)
# (g) Checks data completeness across waves
# (h) Creates spine for censoring incomplete responses
# (i) Creates analytic sample dataset for descriptive analysis

#### Data output: spine for non-working age individuals; 
# spine for individuals with no valid response at study endpoint; 
# eligible population dataframe; wave of death spine look-up, 
# incomplete data spine; sequence analysis of non-response (waves 3-6 and 7-10); 
# sequence analysis of non-response (waves 1-10); analytic sample 1 dataset


################################################################################

## remove any existing objects from global environment
rm(list=ls()) 


################################################################################
#####                            install packages                          #####
################################################################################

library(tidyverse) # all kinds of stuff 
library(foreign) # for reading SPSS files


################################################################################
#####                         load and prepare data                        #####
################################################################################

## load master raw dataframe
master_raw1 <- readRDS("./working_data/master_raw1_clean.rds")

## convert age var to numeric to allow filtering
master_raw1$age_dv <- as.numeric(master_raw1$age_dv)

## split into master a (waves 3-6) and master b (waves 7-10)
master_raw1a <- master_raw1 %>% filter(wv_n %in% c(3:6)) %>% 
  dplyr::select(-indinui_xw) # drop sample B weights

master_raw1b <- master_raw1 %>% filter(wv_n %in% c(7:10)) %>% 
  dplyr::select(-indinub_xw) # drop sample A weights

# check number of indivs in raw data -- relates to node A in flowchart --
master_indivs_a <- length(unique(master_raw1a$pidp))
master_indivs_b <- length(unique(master_raw1b$pidp))

## load weight spines
weight_spine_a <- readRDS("./look_ups/weights_spine_a.rds") %>% 
  dplyr::select(pidp, weight_flag)


weight_spine_b <- readRDS("./look_ups/weights_spine_b.rds") %>% 
  dplyr::select(pidp, weight_flag)

##### create eligible pop dataframe for descriptive analysis -------------------

## keep only working age for study period (>=19 and <=64 at study endpoint (waves 6 and 10))
# create non-working age spines
non_working_age_spine_a <- master_raw1a %>% 
  filter(wv_n==6) %>% 
  mutate(non_work_age_flag=ifelse(age_dv <20 | age_dv >64, 1, 0)) %>% 
  dplyr::select(pidp,age_dv,non_work_age_flag) %>% 
  filter(non_work_age_flag==1) %>% 
  group_by(pidp) %>% 
  slice(1) %>% 
  ungroup() %>% 
  dplyr::select(pidp)

non_working_age_spine_b <- master_raw1b %>% 
  filter(wv_n == 10) %>% 
  mutate(non_work_age_flag=ifelse(age_dv <20 | age_dv >64, 1, 0)) %>% 
  dplyr::select(pidp,age_dv,non_work_age_flag) %>% 
  filter(non_work_age_flag==1) %>% 
  group_by(pidp) %>% 
  slice(1) %>% 
  ungroup() %>% 
  dplyr::select(pidp)

# create working age dfs
working_age_a <- master_raw1a %>% 
  anti_join(non_working_age_spine_a)

working_age_b <- master_raw1b %>% 
  anti_join(non_working_age_spine_b)

# check number of individuals working age
working_age_indivs_a <- length(unique(working_age_a$pidp))
working_age_indivs_b <- length(unique(working_age_b$pidp))

# check number of individuals not working age -- relates to flowchart node AA
non_work_age_indivs_a <- length(unique(non_working_age_spine_a$pidp))
non_work_age_indivs_b <- length(unique(non_working_age_spine_b$pidp))


## keep only cases with valid weights
# create no valid weight spines
no_weight_spine_a <- working_age_a %>% 
  left_join(weight_spine_a) %>% 
  # recode missing values as zero 
  mutate(weight_flag = ifelse(is.na(weight_flag),0,weight_flag)) %>% 
  filter(weight_flag==0) %>% 
  group_by(pidp) %>% 
  slice(1) %>% 
  ungroup() %>% 
  dplyr::select(pidp)

no_weight_spine_b <- working_age_b %>% 
  left_join(weight_spine_b) %>% 
  # recode missing values as zero 
  mutate(weight_flag = ifelse(is.na(weight_flag),0,weight_flag)) %>% 
  filter(weight_flag==0) %>% 
  group_by(pidp) %>% 
  slice(1) %>% 
  ungroup() %>% 
  dplyr::select(pidp)

# create eligible pop df by including only valid weights
eligible_pop_a <- working_age_a %>% 
  left_join(weight_spine_a) %>% 
  filter(weight_flag==1)


eligible_pop_b <- working_age_b %>% 
  left_join(weight_spine_b) %>% 
  filter(weight_flag==1)

# check number of individuals with valid weight -- relates to flowchart node B
eligible_pop_indivs_a <- length(unique(eligible_pop_a$pidp))
eligible_pop_indivs_b <- length(unique(eligible_pop_b$pidp))


# check number of individuals no valid weight -- relates to flowchart node AA
no_weight_indivs_a <- length(unique(no_weight_spine_a$pidp))
no_weight_indivs_b <- length(unique(no_weight_spine_b$pidp))


## check number of observations and individuals
# observations
nrow(eligible_pop_a)
nrow(eligible_pop_b)
# individuals
length(unique(eligible_pop_a$pidp))
length(unique(eligible_pop_b$pidp))
## check min and max ages are correct
min(eligible_pop_a$age_dv)
max(eligible_pop_a$age_dv)
min(eligible_pop_b$age_dv)
max(eligible_pop_b$age_dv)

## save dataframes
write_rds(eligible_pop_a, "./working_data/eligible_pop_a.rds")
write_rds(eligible_pop_b, "./working_data/eligible_pop_b.rds")

write_rds(no_weight_spine_a, "./look_ups/no_weight_spine_a.rds")
write_rds(no_weight_spine_b, "./look_ups/no_weight_spine_b.rds")

write_rds(non_working_age_spine_a, "./look_ups/non_working_age_spine_a.rds")
write_rds(non_working_age_spine_b, "./look_ups/non_working_age_spine_b.rds")

################################################################################
###### create spine for censoring deceased participants ------------------------
################################################################################

#### NOTE: don't think deaths required for this analysis as based on valid
#### response at endpoint; keep for other analyses though

## read in xwaveid data files by wave
#
#data_path <- "C:/Users/0510028p/Documents/UKDA-6614-spss/spss/spss25/"
#
#### waves 3-6
#death_spine_a <- read.spss(paste0(data_path,"ukhls_wx/xwaveid.sav"), 
#                       to.data.frame=TRUE, use.value.labels=TRUE) %>% 
#    as_tibble() %>% 
#  filter(dcsedw_dv != "inapplicable") %>% 
#  mutate(wave_died = as.numeric(gsub("[^0-9.-]", "", dcsedw_dv))) %>% 
#  dplyr::select(pidp, wave_died) %>% 
#  filter(pidp %in% eligible_pop_a$pidp) # keep only eligible pop
#
#  
#write_rds(death_spine_a,("./look_ups/death_spine_a.rds"))

#table(death_spine_a$wave_died)

#### waves 7-10

#death_spine_b <- read.spss(paste0(data_path,"ukhls_wx/xwaveid.sav"), 
#                           to.data.frame=TRUE, use.value.labels=TRUE) %>% 
#  as_tibble() %>% 
#  filter(dcsedw_dv != "inapplicable") %>% 
#  mutate(wave_died = as.numeric(gsub("[^0-9.-]", "", dcsedw_dv))) %>% 
#  dplyr::select(pidp, wave_died) %>% 
#  filter(pidp %in% eligible_pop_b$pidp)# keep only eligible pop

#write_rds(death_spine_b,("./look_ups/death_spine_b.rds"))
#
#table(death_spine_b$wave_died)

################################################################################
###### create spine for censoring retired participants -------------------------
################################################################################

#### waves 3-6
retired_spine_a <- eligible_pop_a %>% 
  dplyr::select(pidp, wv_n, jbstat) %>% 
  arrange(pidp, wv_n) %>% 
  filter(jbstat=="retired" | jbstat=="Retired") %>% 
  group_by(pidp) %>% 
  slice(1) %>% # keep only first occurrence 
  mutate(retired = 1) %>% 
  dplyr::select(pidp, retired) %>% 
  ungroup()

write_rds(retired_spine_a, "./look_ups/retired_spine_a.rds")

#### waves 7-10
retired_spine_b <- eligible_pop_b %>% 
  dplyr::select(pidp, wv_n, jbstat) %>% 
  arrange(pidp, wv_n) %>% 
  filter(jbstat=="retired" | jbstat=="Retired") %>% 
  group_by(pidp) %>% 
  slice(1) %>% # keep only first occurrence 
  mutate(retired = 1) %>% 
  dplyr::select(pidp, retired) %>% 
  ungroup()

write_rds(retired_spine_b, "./look_ups/retired_spine_b.rds")


################################################################################
###### create spine for censoring incomplete responses  ------------------------
################################################################################

#### waves 3-6
incomplete_spine_a <- eligible_pop_a %>% 
  anti_join(retired_spine_a) %>% 
  filter(wv_n==6) %>% 
  mutate(no_age = ifelse(is.na(age_dv),1,0), 
         no_sex = ifelse(sex_dv %in% c("missing", "inapplicable", "refusal", "don't know", 
                         "inconsistent"),1,0),
         no_hiqual = ifelse(hiqual_dv %in% c("missing", "inapplicable", "refusal", 
                            "don't know"),1,0),
         no_emp_contract = ifelse(emp_contract == "missing",1,0),
         no_broken_emp = ifelse(broken_emp == "missing",1,0),
         no_j2has_dv = ifelse(j2has_dv == "missing",1,0),
         no_srh = ifelse(srh_dv %in% c("missing", "inapplicable", "refusal", "don't know"),1,0),
         no_ghq = ifelse(ghq_case4 %in% c("missing", "inapplicable", "proxy","refusal", 
                            "don't know"),1,0)) %>% 
  dplyr::select(pidp, no_age, no_sex, no_hiqual, no_emp_contract, no_broken_emp, no_j2has_dv, 
         no_srh, no_ghq)


#### waves 7-10
incomplete_spine_b <- eligible_pop_b %>% 
  anti_join(retired_spine_b) %>% 
  filter(wv_n==10) %>% 
  mutate(no_age = ifelse(is.na(age_dv),1,0), 
         no_sex = ifelse(sex_dv %in% c("missing", "inapplicable", "refusal", "don't know", 
                                       "inconsistent"),1,0),
         no_hiqual = ifelse(hiqual_dv %in% c("missing", "inapplicable", "refusal", 
                                             "don't know"),1,0),
         no_emp_contract = ifelse(emp_contract == "missing",1,0),
         no_broken_emp = ifelse(broken_emp == "missing",1,0),
         no_j2has_dv = ifelse(j2has_dv == "missing",1,0),
         no_srh = ifelse(srh_dv %in% c("missing", "inapplicable", "refusal", "don't know"),1,0),
         no_ghq = ifelse(ghq_case3 %in% c("missing", "inapplicable", "proxy","refusal", 
                                          "don't know"),1,0)) %>% 
  dplyr::select(pidp, no_age, no_sex, no_hiqual, no_emp_contract, no_broken_emp, no_j2has_dv, 
         no_srh, no_ghq)


### calculate totals

## overall all totals by key variables
incomplete_spine_a %>% dplyr::select(-pidp) %>% 
  summarise_all(funs(sum)) %>% 
  pivot_longer(cols=1:8, names_to = "variable", values_to = "n") %>% 
  mutate(pc = (n/nrow(incomplete_spine_a)*100))

incomplete_spine_b %>% dplyr::select(-pidp) %>% 
  summarise_all(funs(sum)) %>% 
  pivot_longer(cols=1:8, names_to = "variable", values_to = "n") %>% 
  mutate(pc = (n/nrow(incomplete_spine_b)*100))

## check overlaps
# demographics
incomplete_spine_a %>% dplyr::select(no_age, no_sex, no_hiqual) %>% 
  mutate(n_demogs_incompete = no_age + no_sex + no_hiqual) %>% 
  filter(n_demogs_incompete!=0) %>% 
  group_by(n_demogs_incompete) %>% 
  summarise(n=n())

incomplete_spine_a %>% dplyr::select(no_age, no_sex, no_hiqual) %>% 
  summarise_all(funs(sum))

incomplete_spine_b %>% dplyr::select(no_age, no_sex, no_hiqual) %>% 
  mutate(n_demogs_incompete = no_age + no_sex + no_hiqual) %>% 
  filter(n_demogs_incompete!=0) %>% 
  group_by(n_demogs_incompete) %>% 
  summarise(n=n())

incomplete_spine_b %>% dplyr::select(no_age, no_sex, no_hiqual) %>% 
  summarise_all(funs(sum))


### exposures
incomplete_spine_a %>% dplyr::select(no_emp_contract, no_broken_emp, no_j2has_dv) %>% 
  mutate(n_exp_incompete = no_emp_contract + no_broken_emp + no_j2has_dv) %>% 
  filter(n_exp_incompete!=0) %>% 
  group_by(n_exp_incompete) %>% 
  summarise(n=n())

incomplete_spine_a %>% dplyr::select(no_emp_contract, no_broken_emp, no_j2has_dv) %>% 
  summarise_all(funs(sum))

incomplete_spine_b %>% dplyr::select(no_emp_contract, no_broken_emp, no_j2has_dv) %>% 
  mutate(n_exp_incompete = no_emp_contract + no_broken_emp + no_j2has_dv) %>% 
  filter(n_exp_incompete!=0) %>% 
  group_by(n_exp_incompete) %>% 
  summarise(n=n())

incomplete_spine_b %>% dplyr::select(no_emp_contract, no_broken_emp, no_j2has_dv) %>% 
  summarise_all(funs(sum))

### outcomes
incomplete_spine_a %>% dplyr::select(no_srh, no_ghq) %>% 
  mutate(n_outcomes_incompete = no_srh + no_ghq) %>% 
  filter(n_outcomes_incompete!=0) %>% 
  group_by(n_outcomes_incompete) %>% 
  summarise(n=n())


## save for use in sample characteristics table as missing data
write_rds(incomplete_spine_a, "./working_data/incomplete_spine_a.rds")
write_rds(incomplete_spine_b, "./working_data/incomplete_spine_b.rds")

## create long format spine with only one flag for incomplete data
incomplete_spine_a_long <- incomplete_spine_a %>% 
  pivot_longer(names_to = "censor_reason", cols = 2:9, 
               values_to = "incomplete") %>% 
  filter(incomplete==1) %>% 
  group_by(pidp) %>% 
  slice(1) %>% 
  ungroup() %>% 
  dplyr::select(-censor_reason) 

incomplete_spine_b_long <- incomplete_spine_b %>% 
  pivot_longer(names_to = "censor_reason", cols = 2:9, 
               values_to = "incomplete") %>% 
  filter(incomplete==1) %>% 
  group_by(pidp) %>% 
  slice(1) %>% 
  ungroup() %>% 
  dplyr::select(-censor_reason) 

################################################################################
###### create a combined spine to indicate the reason for censoring ------------
################################################################################

### join censoring spines together 
## waves 3-6
censor_combined_a <- retired_spine_a %>% 
  full_join(incomplete_spine_a_long) %>%   
  # covert to long format 
  pivot_longer(names_to = "censor_reason", cols = 2:3, values_to = "censored_flag") %>% 
  filter(censored_flag==1) %>% 
  # keep only first reason for censoring based on order below
  arrange(pidp, match(censor_reason, c("retired","incomplete"))) %>% 
  group_by(pidp) %>% 
  slice(1) %>% 
  ungroup()

## waves 7-10
censor_combined_b <- retired_spine_b %>% 
  full_join(incomplete_spine_b_long) %>%   
  # covert to long format 
  pivot_longer(names_to = "censor_reason", cols = 2:3, values_to = "censored_flag") %>% 
  filter(censored_flag==1) %>% 
  # keep only first reason for censoring based on order below
  arrange(pidp, match(censor_reason, c("retired","incomplete"))) %>% 
  group_by(pidp) %>% 
  slice(1) %>% 
  ungroup()


# check number of indivs by reason for censoring
table(censor_combined_a$censor_reason)
table(censor_combined_b$censor_reason)

## save combined censoring spine for flowchart
write_rds(censor_combined_a, "./look_ups/censor_combined_a.rds")
write_rds(censor_combined_b, "./look_ups/censor_combined_b.rds")


################################################################################
###### Create descriptive analytic sample --------------------------------------
################################################################################

### remove censored cases from eligible population to create analytic sample 1
## (a) waves 3-6
dfas1a <- eligible_pop_a %>% 
  anti_join(censor_combined_a)

write_rds(dfas1a, "./analytic_sample_data/dfas1a.rds")

## (b) waves 7-10
dfas1b <- eligible_pop_b %>% 
  anti_join(censor_combined_b)

write_rds(dfas1b, "./analytic_sample_data/dfas1b.rds")

