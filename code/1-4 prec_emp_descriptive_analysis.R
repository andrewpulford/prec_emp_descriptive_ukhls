################################################################################

# Persistent precarious employment and health - Understanding Society
# 1-4 - descriptive analysis
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
library(TraMineR) # for sequence analysis

citation("TraMineR")

################################################################################
#####                         load and prepare data                        #####
################################################################################


### analytic sample 1a - waves 3-6
dfas1a <- readRDS("./analytic_sample_data/dfas1a.rds") %>% 
  mutate(sample_group = "a") %>% select(-valid_6)

## endpoint only
dfas1a_end <- dfas1a %>% filter(wv_n==6)

### analytic sample 1b - waves 7-10
dfas1b <- readRDS("./analytic_sample_data/dfas1b.rds") %>% 
  mutate(sample_group = "b") %>% select(-valid_10)

## endpoint only
dfas1b_end <- dfas1b %>% filter(wv_n==10) 

### combined study endpoint df
dfas1_end <- dfas1a_end %>% bind_rows(dfas1b_end)

################################################################################
#####                sample characteristics at study endpoint              #####
################################################################################


#####----------------------------------------------------------------------#####
#####                     Personal characteristics                         #####
#####----------------------------------------------------------------------#####

#### sex -----------------------------------------------------------------------
sex <- dfas1_end %>% group_by(wv_n,sex_dv) %>% summarise(n=n()) %>% 
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Sex") %>% 
  rename("measure"= "sex_dv") %>% 
  select(wv_n, var, measure, n, est)

sample_chars_endpoint <- sex


#### age -----------------------------------------------------------------------
age_mean <- dfas1_end %>% group_by(wv_n) %>% 
  summarise(est = mean(as.numeric(as.numeric(age_dv)), na.rm = TRUE)) %>% 
  mutate(var="Age", measure="Mean", n=NA) %>% 
  select(wv_n, var, measure, n, est)

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(age_mean)

#age_min <- dfas1_end %>% group_by(wv_n,sex_dv) %>% 
#  summarise(est = min(as.numeric(age_dv), na.rm = TRUE)) %>% 
#  mutate(var="Age", measure="Min", n=NA) %>% 
#  select(wv_n, var, measure, n, est)
#
#sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(age_min)
#
#
#age_max <- dfas1_end %>% group_by(wv_n,sex_dv) %>%   
#  summarise(est = max(as.numeric(age_dv), na.rm = TRUE)) %>% 
#  mutate(var="Age", measure="Max", n=NA) %>% 
#  select(wv_n, var, measure, n, est)
#
#sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(age_max)

#### ethnicity -----------------------------------------------------------------
## full ethnicity coding
ethnicity <- dfas1_end %>% group_by(wv_n,ethn_dv) %>% 
  summarise(n=n()) %>% 
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Ethnicity") %>% 
  rename("measure"= "ethn_dv") %>% 
  select(wv_n, var, measure, n, est)

## white/non-white
dfas1_end <- dfas1_end %>% 
  mutate(non_white = ifelse(ethn_dv=="british/english/scottish/welsh/northern irish", "White",
                            ifelse(ethn_dv=="irish", "White",
                                   ifelse(ethn_dv=="any other white background", "White",
                                          ifelse(ethn_dv=="missing", "Missing",
                                                 "Non-white")))))

white_non <- dfas1_end %>% group_by(wv_n,non_white) %>% 
  summarise(n=n()) %>%  
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Ethnicity") %>% 
  rename("measure"= "non_white") %>% 
  select(wv_n, var, measure, n, est)

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(white_non)

#### Marital status -------------------------------------------------------------
marital <- dfas1_end %>% group_by(wv_n,mlstat) %>% summarise(n=n()) %>%  
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Marital status") %>% 
  rename("measure"= "mlstat") %>% 
  select(wv_n, var, measure, n, est)
### need to sort inapplicable issue by including wv1 values<<<<<<<<<<<<<<<<<<<<<

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(marital)

#### Educational attainment ----------------------------------------------------
ed_attain <- dfas1_end %>% group_by(wv_n,hiqual_dv) %>% summarise(n=n()) %>% 
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Educational attainment") %>% 
  rename("measure"= "hiqual_dv") %>% 
  select(wv_n, var, measure, n, est)

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(ed_attain)


#####----------------------------------------------------------------------#####
#####               Employment and income characteristics                  #####
#####----------------------------------------------------------------------#####

#### Current employment status -------------------------------------------------
emp <-  dfas1_end %>% 
  group_by(wv_n,employ) %>% summarise(n=n()) %>%  
  mutate(pc = n/sum(n)*100)

#### current labour force status -----------------------------------------------
lab_status <- dfas1_end %>% group_by(wv_n,jbstat) %>% summarise(n=n()) %>%  
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Labour status") %>% 
  rename("measure"= "jbstat") %>% 
  select(wv_n,var, measure, n, est)

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(lab_status)


#### Current job: Three Class NS-SEC -------------------------------------------
nssec3 <- dfas1_end %>% group_by(wv_n,jbnssec3_dv) %>% summarise(n=n()) %>% 
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="NS-SEC3") %>% 
  rename("measure"= "jbnssec3_dv") %>% 
  select(wv_n,var, measure, n, est)
# recode inapplicable based on emp status? <<<<<<<<<

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(nssec3)

#### RG Social Class: present job ----------------------------------------------
rg_class <- dfas1_end %>% group_by(wv_n,jbrgsc_dv) %>% summarise(n=n()) %>% 
  mutate(pc = n/sum(n)*100)

# don't add for now

#### permanent or temporary ----------------------------------------------------
perm_emp <- dfas1_end %>% group_by(wv_n,jbterm1) %>% summarise(n=n()) %>% 
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Employment contract") %>% 
  rename("measure"= "jbterm1") %>% 
  select(wv_n,var, measure, n, est)

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(perm_emp)


#### Employment spells since last interview ------------------------------------

### employment spells -----
# create numeric version
dfas1_end <- dfas1_end %>% 
  mutate(nmpsp_dv=ifelse(nmpsp_dv=="none","0",nmpsp_dv)) %>% 
  mutate(nmpsp_dvn=as.numeric(nmpsp_dv))

# create binary version (one spell or more)
dfas1_end <- dfas1_end %>% mutate(emp_spells_bin=ifelse(nmpsp_dvn<1,"no",
                                                ifelse(nmpsp_dvn>=1,"yes", NA)))

### non-employment spells -----
# create numeric version
dfas1_end <- dfas1_end %>% 
  mutate(nnmpsp_dv=ifelse(nnmpsp_dv=="none","0",nnmpsp_dv)) %>% 
  mutate(nnmpsp_dvn=as.numeric(nnmpsp_dv))

# create binary version (one or more spell)
dfas1_end <- dfas1_end %>% mutate(nonemp_spells_bin=ifelse(nnmpsp_dvn<1,"no",
                                                   ifelse(nnmpsp_dvn>=1,"yes", 
                                                          NA)))

### unemployment spells -----
# create numeric version
dfas1_end <- dfas1_end %>% 
  mutate(nunmpsp_dv=ifelse(nunmpsp_dv=="none","0",nunmpsp_dv)) %>% 
  mutate(nunmpsp_dvn=as.numeric(nunmpsp_dv))


# create binary version (one or more spell)
dfas1_end <- dfas1_end %>% mutate(unemp_spells_bin=ifelse(nunmpsp_dvn<1,"no",
                                                  ifelse(nunmpsp_dvn>=1,"yes", 
                                                         NA)))

### create broken employment variable ------
dfas1_end <- dfas1_end %>% 
  mutate(broken_emp = ifelse(emp_spells_bin=="no","No employment spells", 
                             ifelse(emp_spells_bin=="yes" & 
                                      nonemp_spells_bin=="no" & 
                                      unemp_spells_bin=="no",
                                    "Unbroken employment",
                                    ifelse(emp_spells_bin=="yes" & 
                                             (nonemp_spells_bin=="yes" |
                                                unemp_spells_bin=="yes"),
                                           "Broken employment","check"))))


emp_broken <- dfas1_end %>% group_by(wv_n,broken_emp) %>% 
  summarise(n=n()) %>%  
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Broken employment") %>% 
  rename("measure"= "broken_emp") %>% 
  select(wv_n,var, measure, n, est)

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(emp_broken)

#### perceived job security in the next 12 months ------------------------------
# even # waves only

# summary df
job_sec <- dfas1_end %>% group_by(wv_n, jbsec) %>% summarise(n=n()) %>% 
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Perceived job security") %>% 
  rename("measure"= "jbsec") %>% 
  select(wv_n,var, measure, n, est)# recode inapplicable etc?

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(job_sec)

#### Multiple jobs -------------------------------------------------------------
### has a 2nd job ----
emp_2nd <- dfas1_end %>% group_by(wv_n, j2has) %>% summarise(n=n()) %>%
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Multiple jobs") %>% 
  rename("measure"= "j2has") %>% 
  select(wv_n,var, measure, n, est)# recode inapplicable etc?
# recode inapplicable etc?

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(emp_2nd)

#### income --------------------------------------------------------------------
## total net personal income (check what used in COVID modelling)
# check monthly?
dfas1_end$fimnnet_dv <- as.numeric(as.character(dfas1_end$fimnnet_dv))


inc_quantile <- dfas1_end %>% group_by(wv_n) %>% 
  summarise(enframe(quantile(fimnnet_dv, c(0.25, 0.5, 0.75)), "measure", "est")) %>%
#  mutate(measure=factor(measure)) %>% 
  mutate(measure = ifelse(measure=="25%","25% quantile", 
                        ifelse(measure=="50%","Median","75% quantile"))) %>% 
  mutate(var="Monthly net income (£)",
         n=NA) %>% 
  select(wv_n,var, measure, n, est)

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(inc_quantile)


#####----------------------------------------------------------------------#####
#####                   Baseline health characteristics                    #####
#####----------------------------------------------------------------------#####

#### long-standing illness or impairment ---------------------------------------
# risk of reverse causation - need to be incident from previous wave?
ltc <- dfas1_end %>% group_by(wv_n, health) %>% summarise(n=n()) %>%  
  mutate(pc = n/sum(n)*100)

# don't add for now

#### self-rated health ---------------------------------------------------------

srh <- dfas1_end %>% 
  # recode self-rated health variables into one
  mutate(sf1 = as.character(sf1),
         scsf1 = as.character(scsf1)) %>% 
  mutate(srh_dv = ifelse(sf1=="inapplicable",scsf1, sf1)) %>% 
  group_by(wv_n, srh_dv) %>% summarise(n=n()) %>%  
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Self-rated health") %>% 
  rename("measure"= "srh_dv") %>% 
  select(wv_n,var, measure, n, est)

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(srh)


#### GHQ-12 --------------------------------------------------------------------
## calculate caseness (cut point = 3)
dfas1_end <- dfas1_end %>% mutate(ghq_case3 = ifelse(grepl("0",as.character(scghq2_dv)),0,
                                             ifelse(grepl("1",as.character(scghq2_dv)),0,
                                                    ifelse(grepl("2",as.character(scghq2_dv)),0,
                                                           ifelse(grepl("3",as.character(scghq2_dv)),1,
                                                                  ifelse(grepl("4",as.character(scghq2_dv)),1,
                                                                         ifelse(grepl("5",as.character(scghq2_dv)),1,
                                                                                ifelse(grepl("6",as.character(scghq2_dv)),1,
                                                                                       ifelse(grepl("7",as.character(scghq2_dv)),1,
                                                                                              ifelse(grepl("8",as.character(scghq2_dv)),1,
                                                                                                     ifelse(grepl("9",as.character(scghq2_dv)),1,
                                                                                                            ifelse(grepl("10",as.character(scghq2_dv)),1,
                                                                                                                   ifelse(grepl("11",as.character(scghq2_dv)),1,
                                                                                                                          ifelse(grepl("12",as.character(scghq2_dv)),1,
                                                                                                                                 as.character(scghq2_dv))))))))))))))) 

ghq3 <- dfas1_end %>% group_by(wv_n, ghq_case3) %>% summarise(n=n()) %>%  
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="GHQ12 score") %>% 
  rename("measure"= "ghq_case3") %>% 
  mutate(measure = ifelse(measure=="0","0-2",
                          ifelse(measure=="1","3 or more",measure))) %>% 
  select(wv_n,var, measure, n, est)

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(ghq3)

#### SF-12 mental component summary -------------------------------------------- 
## leave for now

# convert to numeric (to character stirng first to actual value is retained)
#dfas1_end$sf12mcs_dv <- as.character(dfas1_end$sf12mcs_dv)
#dfas1_end$sf12mcs_dv <- as.numeric(dfas1_end$sf12mcs_dv)
#
## distribution measures
#mean(dfas1_end$sf12mcs_dv, na.rm = TRUE)
#median(dfas1_end$sf12mcs_dv, na.rm = TRUE)
#min(dfas1_end$sf12mcs_dv, na.rm = TRUE)
#max(dfas1_end$sf12mcs_dv, na.rm = TRUE)
#sf12_quantile <- quantile(dfas1_end$sf12mcs_dv, na.rm = TRUE)


#### Job-related Wellbeing scale -------------
# Higher values on the scale represent lower levels of anxiety

## Anxiety subscale
# originally devised by Warr (1990): Anxiety subscale 
# (from Warr’s “Anxiety-Contentment” scale). 

# covert to numeric
#dfas1_end <- dfas1_end %>% 
#  mutate(jwbs1_dv = ifelse(jwbs1_dv=="least anxious",15,
#                           ifelse(jwbs1_dv%in%c("missing","don't know",
#                                                "inapplicable","refusal", 
#                                                "proxy"),"NA",jwbs1_dv))) %>% 
#  mutate(jwbs1_dv = as.numeric(jwbs1_dv))
#
## distribution measures
#mean(dfas1_end$jwbs1_dv, na.rm = TRUE)
#median(dfas1_end$jwbs1_dv, na.rm = TRUE)
#min(dfas1_end$jwbs1_dv, na.rm = TRUE)
#max(dfas1_end$jwbs1_dv, na.rm = TRUE)
#jb_anx <- quantile(dfas1_end$jwbs1_dv, na.rm = TRUE)
#


## Depression subscale
# originally devised by Warr (1990): Depression subscale 
# (from Warr’s "Depression-Enthusiasm" scale)

# covert to numeric
#dfas1_end <- dfas1_end %>% 
#  mutate(jwbs2_dv = ifelse(jwbs2_dv=="least depressed",15,
#                           ifelse(jwbs2_dv%in%c("missing","don't know",
#                                                "inapplicable","refusal", 
#                                                "proxy"),"NA",jwbs2_dv))) %>% 
#  mutate(jwbs2_dv = as.numeric(jwbs2_dv))
#
#
## distribution measures
#mean(dfas1_end$jwbs2_dv, na.rm = TRUE)
#median(dfas1_end$jwbs2_dv, na.rm = TRUE)
#min(dfas1_end$jwbs2_dv, na.rm = TRUE)
#max(dfas1_end$jwbs2_dv, na.rm = TRUE)
#jb_dep <- quantile(dfas1_end$jwbs2_dv, na.rm = TRUE)


#####----------------------------------------------------------------------#####
#####                     Save sample endpoint chars data                  #####
#####----------------------------------------------------------------------#####

## as dataframe
write_rds(sample_chars_endpoint, "./working_data/sample_chars_endpoint.rds")

################################################################################
#####                  sample characteristics at baseline                  ##### 
#####                  by exposure (fixed-term employment)                 #####
################################################################################

### select wave 2 only
dfas1_end_exp1 <- dfas1_end %>% 
  filter(wv_n==2)

#####----------------------------------------------------------------------#####
#####                     Personal characteristics                         #####
#####----------------------------------------------------------------------#####

#### sex -----------------------------------------------------------------------
sex_exp1 <- dfas1_end_exp1 %>% group_by(jbterm1, sex_dv) %>% summarise(n=n()) %>% 
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Sex") %>% 
  rename("measure"= "sex_dv") %>% 
  select(var, measure, jbterm1, n, est)

sample_chars_endpoint_exp1 <- sex_exp1

#### age -----------------------------------------------------------------------
age_mean_exp1 <- dfas1_end_exp1 %>% 
  group_by(jbterm1) %>% 
  summarise(est = mean(as.numeric(as.numeric(age_dv)), na.rm = TRUE)) %>% 
  mutate(var="Age", measure="Mean", n=NA) %>% 
  select(var, measure, jbterm1, n, est)

sample_chars_endpoint_exp1 <- sample_chars_endpoint_exp1 %>% bind_rows(age_mean_exp1)

age_min_exp1 <- dfas1_end_exp1 %>%  
  group_by(jbterm1) %>% 
  summarise(est = min(as.numeric(age_dv), na.rm = TRUE)) %>% 
  mutate(var="Age", measure="Min", n=NA) %>% 
  select(var, measure, jbterm1, n, est)

sample_chars_endpoint_exp1 <- sample_chars_endpoint_exp1 %>% bind_rows(age_min_exp1)


age_max_exp1 <- dfas1_end_exp1 %>%  
  group_by(jbterm1) %>% 
  summarise(est = max(as.numeric(age_dv), na.rm = TRUE)) %>% 
  mutate(var="Age", measure="Max", n=NA) %>% 
  select(var, measure, jbterm1, n, est)

sample_chars_endpoint_exp1 <- sample_chars_endpoint_exp1 %>% bind_rows(age_max_exp1)

#### ethnicity -----------------------------------------------------------------
## full ethnicity coding
ethnicity_exp1 <- dfas1_end_exp1 %>% group_by(jbterm1, ethn_dv) %>% 
  summarise(n=n()) %>% 
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Ethnicity") %>% 
  rename("measure"= "ethn_dv") %>% 
  select(var, measure, jbterm1, n, est)

## white/non-white
dfas1_end_exp1 <- dfas1_end_exp1 %>% 
  mutate(non_white = ifelse(ethn_dv=="british/english/scottish/welsh/northern irish", "White",
                            ifelse(ethn_dv=="irish", "White",
                                   ifelse(ethn_dv=="any other white background", "White",
                                          ifelse(ethn_dv=="missing", "Missing",
                                                 "Non-white")))))

white_non_exp1 <- dfas1_end_exp1 %>% group_by(jbterm1, non_white) %>% summarise(n=n()) %>%  
  mutate(est = n/sum(n)*100) %>% ungroup() %>%
  mutate(var="Ethnicity") %>% 
  rename("measure"= "non_white") %>% 
  select(var, measure, jbterm1, n, est)

sample_chars_endpoint_exp1 <- sample_chars_endpoint_exp1 %>% bind_rows(white_non_exp1)

#### Marital status -------------------------------------------------------------
marital_exp1 <- dfas1_end_exp1 %>% group_by(jbterm1, mlstat) %>% summarise(n=n()) %>%  
  mutate(est = n/sum(n)*100) %>% ungroup() %>%
  mutate(var="Marital status") %>% 
  rename("measure"= "mlstat") %>% 
  select(var, measure, jbterm1, n, est)
### need to sort inapplicable issue by including wv1 values<<<<<<<<<<<<<<<<<<<<<

sample_chars_endpoint_exp1 <- sample_chars_endpoint_exp1 %>% bind_rows(marital_exp1)

#### Educational attainment ----------------------------------------------------
ed_attain_exp1 <- dfas1_end_exp1 %>% group_by(jbterm1, hiqual_dv) %>% summarise(n=n()) %>%  
  mutate(est = n/sum(n)*100) %>% ungroup() %>%
  mutate(var="Educational attainment") %>% 
  rename("measure"= "hiqual_dv") %>% 
  select(var, measure, jbterm1, n, est)

sample_chars_endpoint_exp1 <- sample_chars_endpoint_exp1 %>% bind_rows(ed_attain_exp1)


#####----------------------------------------------------------------------#####
#####               Employment and income characteristics                  #####
#####----------------------------------------------------------------------#####

#### Current employment status -------------------------------------------------
emp_exp1 <-  dfas1_end_exp1 %>% 
  group_by(jbterm1, employ) %>% summarise(n=n()) %>% 
  mutate(pc = n/sum(n)*100) %>% ungroup()

#### current labour force status -----------------------------------------------
lab_status_exp1 <- dfas1_end_exp1 %>% group_by(jbterm1, jbstat) %>% summarise(n=n()) %>%  
  mutate(est = n/sum(n)*100) %>% ungroup() %>%
  mutate(var="Labour status") %>% 
  rename("measure"= "jbstat") %>% 
  select(var, measure, jbterm1, n, est)

sample_chars_endpoint_exp1 <- sample_chars_endpoint_exp1 %>% bind_rows(lab_status_exp1)

#### Current job: Three Class NS-SEC -------------------------------------------
nssec3_exp1 <- dfas1_end_exp1 %>% group_by(jbterm1, jbnssec3_dv) %>% summarise(n=n()) %>% 
  mutate(est = n/sum(n)*100) %>% ungroup() %>% 
  mutate(var="NS-SEC3") %>% 
  rename("measure"= "jbnssec3_dv") %>% 
  select(var, measure, jbterm1, n, est)
# recode inapplicable based on emp status? <<<<<<<<<

sample_chars_endpoint_exp1 <- sample_chars_endpoint_exp1 %>% bind_rows(nssec3_exp1)

#### RG Social Class: present job ----------------------------------------------
rg_class_exp1 <- dfas1_end_exp1 %>% group_by(jbterm1, jbrgsc_dv) %>% summarise(n=n()) %>%  
  mutate(pc = n/sum(n)*100) %>%ungroup()


#### Employment spells since last interview ------------------------------------

### employment spells -----
# create numeric version
dfas1_end_exp1 <- dfas1_end_exp1 %>% 
  mutate(nmpsp_dv=ifelse(nmpsp_dv=="none","0",nmpsp_dv)) %>% 
  mutate(nmpsp_dvn=as.numeric(nmpsp_dv))

# create collapsed version (zero, one or more more than one spell)
#dfas1_end <- dfas1_end %>% mutate(emp_spells_cat=ifelse(nmpsp_dvn>1,"more than one",nmpsp_dvn))

# create binary version (one spell or more)
dfas1_end_exp1 <- dfas1_end_exp1 %>% mutate(emp_spells_bin=ifelse(nmpsp_dvn<1,"no",
                                                        ifelse(nmpsp_dvn>=1,"yes", NA)))

# create binary version (more than one spell)
#dfas1_end <- dfas1_end %>% mutate(multi_emp_spells=ifelse(nmpsp_dvn<=1,"no",
#                                                  ifelse(nmpsp_dvn>1,"yes",
#                                                         NA)))

### non-employment spells -----
# create numeric version
dfas1_end_exp1 <- dfas1_end_exp1 %>% 
  mutate(nnmpsp_dv=ifelse(nnmpsp_dv=="none","0",nnmpsp_dv)) %>% 
  mutate(nnmpsp_dvn=as.numeric(nnmpsp_dv))

# create binary version (one or more spell)
dfas1_end_exp1 <- dfas1_end_exp1 %>% mutate(nonemp_spells_bin=ifelse(nnmpsp_dvn<1,"no",
                                                           ifelse(nnmpsp_dvn>=1,"yes", 
                                                                  NA)))

### unemployment spells -----
# create numeric version
dfas1_end_exp1 <- dfas1_end_exp1 %>% 
  mutate(nunmpsp_dv=ifelse(nunmpsp_dv=="none","0",nunmpsp_dv)) %>% 
  mutate(nunmpsp_dvn=as.numeric(nunmpsp_dv))


# create binary version (one or more spell)
dfas1_end_exp1 <- dfas1_end_exp1 %>% mutate(unemp_spells_bin=ifelse(nunmpsp_dvn<1,"no",
                                                          ifelse(nunmpsp_dvn>=1,"yes", 
                                                                 NA)))

### create broken employment variable ------
dfas1_end_exp1 <- dfas1_end_exp1 %>% 
  mutate(broken_emp = ifelse(emp_spells_bin=="no","No employment spells", 
                             ifelse(emp_spells_bin=="yes" & 
                                      nonemp_spells_bin=="no" & 
                                      unemp_spells_bin=="no",
                                    "Unbroken employment",
                                    ifelse(emp_spells_bin=="yes" & 
                                             (nonemp_spells_bin=="yes" |
                                                unemp_spells_bin=="yes"),
                                           "Broken employment","check"))))


emp_broken_exp1 <- dfas1_end_exp1 %>% group_by(jbterm1, broken_emp) %>% 
  summarise(n=n()) %>%  
  mutate(est = n/sum(n)*100) %>% ungroup() %>%
  mutate(var="Broken employment") %>% 
  rename("measure"= "broken_emp") %>% 
  select(var, measure, jbterm1, n, est)

sample_chars_endpoint_exp1 <- sample_chars_endpoint_exp1 %>% bind_rows(emp_broken_exp1)

#### perceived job security in the next 12 months ------------------------------
# even # waves only

# summary df
job_sec_exp1 <- dfas1_end_exp1 %>% group_by(jbterm1, jbsec) %>% 
  summarise(n=n()) %>% 
  mutate(est = n/sum(n)*100) %>% ungroup() %>% 
  mutate(var="Perceived job security") %>% 
  rename("measure"= "jbsec") %>% 
  select(var, measure, jbterm1, n, est)# recode inapplicable etc?

sample_chars_endpoint_exp1 <- sample_chars_endpoint_exp1 %>% bind_rows(job_sec_exp1)

#### Multiple jobs -------------------------------------------------------------
### has a 2nd job ----
emp_2nd_exp1 <- dfas1_end_exp1 %>% group_by(jbterm1, j2has) %>% summarise(n=n()) %>% 
  mutate(est = n/sum(n)*100) %>%  ungroup() %>%
  mutate(var="Multiple jobs") %>% 
  rename("measure"= "j2has") %>% 
  select(var, measure, jbterm1, n, est)# recode inapplicable etc?
# recode inapplicable etc?
# include?

sample_chars_endpoint_exp1 <- sample_chars_endpoint_exp1 %>% bind_rows(emp_2nd_exp1)

#### income --------------------------------------------------------------------
## total net personal income (check what used in COVID modelling)
# check monthly?
dfas1_end_exp1$fimnnet_dv <- as.numeric(as.character(dfas1_end_exp1$fimnnet_dv))

# descriptives
#inc_mean <- mean(dfas1_end$fimnnet_dv)
#inc_median <- median(dfas1_end$fimnnet_dv)
#inc_min <- min(dfas1_end$fimnnet_dv)
#inc_max <- max(dfas1_end$fimnnet_dv)
#inc_quantile <- quantile(dfas1_end$fimnnet_dv)
# present 25, 50, 75% quantiles in table?

inc_quantile_exp1 <- dfas1_end_exp1 %>% group_by(jbterm1) %>% 
  summarise(enframe(quantile(fimnnet_dv, c(0.25, 0.5, 0.75)), "measure", "est")) %>%
  #  mutate(measure=factor(measure)) %>% 
  mutate(measure = ifelse(measure=="25%","25% quantile", 
                          ifelse(measure=="50%","Median","75% quantile"))) %>% 
  mutate(var="Monthly net income (£)",
         n=NA) %>% 
  select(var, measure, jbterm1, n, est)

sample_chars_endpoint_exp1 <- sample_chars_endpoint_exp1 %>% bind_rows(inc_quantile_exp1)


#####----------------------------------------------------------------------#####
#####                   Baseline health characteristics                    #####
#####----------------------------------------------------------------------#####

#### long-standing illness or impairment ---------------------------------------
# risk of reverse causation - need to be incident from previous wave?
ltc_exp1 <- dfas1_end_exp1 %>% group_by(jbterm1, health) %>% summarise(n=n()) %>% 
  mutate(pc = n/sum(n)*100) %>% ungroup() 

#### self-rated health ---------------------------------------------------------
# for waves 2-5 all responses are included in sf1 == use sf1
srh_exp1 <- dfas1_end_exp1 %>% group_by(jbterm1, sf1) %>% summarise(n=n()) %>% 
  mutate(est = n/sum(n)*100) %>% ungroup() %>% 
  mutate(var="Self-rated health") %>% 
  rename("measure"= "sf1") %>% 
  select(var, measure, jbterm1, n, est)

sample_chars_endpoint_exp1 <- sample_chars_endpoint_exp1 %>% bind_rows(srh_exp1)


#### GHQ-12 --------------------------------------------------------------------
## calculate caseness (cut point = 3)
dfas1_end_exp1 <- dfas1_end_exp1 %>% mutate(ghq_case3 = ifelse(grepl("0",as.character(scghq2_dv)),0,
                                                     ifelse(grepl("1",as.character(scghq2_dv)),0,
                                                            ifelse(grepl("2",as.character(scghq2_dv)),0,
                                                                   ifelse(grepl("3",as.character(scghq2_dv)),1,
                                                                          ifelse(grepl("4",as.character(scghq2_dv)),1,
                                                                                 ifelse(grepl("5",as.character(scghq2_dv)),1,
                                                                                        ifelse(grepl("6",as.character(scghq2_dv)),1,
                                                                                               ifelse(grepl("7",as.character(scghq2_dv)),1,
                                                                                                      ifelse(grepl("8",as.character(scghq2_dv)),1,
                                                                                                             ifelse(grepl("9",as.character(scghq2_dv)),1,
                                                                                                                    ifelse(grepl("10",as.character(scghq2_dv)),1,
                                                                                                                           ifelse(grepl("11",as.character(scghq2_dv)),1,
                                                                                                                                  ifelse(grepl("12",as.character(scghq2_dv)),1,
                                                                                                                                         as.character(scghq2_dv))))))))))))))) 

ghq3_exp1 <- dfas1_end_exp1 %>% group_by(jbterm1, ghq_case3) %>% summarise(n=n()) %>% 
  mutate(est = n/sum(n)*100) %>% ungroup() %>% 
  mutate(var="GHQ12 score") %>% 
  rename("measure"= "ghq_case3") %>% 
  mutate(measure = ifelse(measure=="0","0-2",
                          ifelse(measure=="1","3 or more",measure))) %>% 
  select(var, measure, jbterm1, n, est)

sample_chars_endpoint_exp1 <- sample_chars_endpoint_exp1 %>% bind_rows(ghq3_exp1)

#### SF-12 mental component summary -------------------------------------------- 

# covnvert to numeric (to character stirng first to actual value is retained)
dfas1_end_exp1$sf12mcs_dv <- as.character(dfas1_end_exp1$sf12mcs_dv)
dfas1_end_exp1$sf12mcs_dv <- as.numeric(dfas1_end_exp1$sf12mcs_dv)

# distribution measures
#mean(dfas1_end_exp1$sf12mcs_dv, na.rm = TRUE)
#median(dfas1_end_exp1$sf12mcs_dv, na.rm = TRUE)
#min(dfas1_end_exp1$sf12mcs_dv, na.rm = TRUE)
#max(dfas1_end_exp1$sf12mcs_dv, na.rm = TRUE)
#sf12_quantile_exp1 <- quantile(dfas1_end_exp1$sf12mcs_dv, na.rm = TRUE)

#### Job-related Wellbeing scale -------------
# Higher values on the scale represent lower levels of anxiety

## Anxiety subscale
# originally devised by Warr (1990): Anxiety subscale 
# (from Warr’s “Anxiety-Contentment” scale). 

# covert to numeric
dfas1_end_exp1 <- dfas1_end_exp1 %>% 
  mutate(jwbs1_dv = ifelse(jwbs1_dv=="least anxious",15,
                           ifelse(jwbs1_dv%in%c("missing","don't know",
                                                "inapplicable","refusal", 
                                                "proxy"),"NA",jwbs1_dv))) %>% 
  mutate(jwbs1_dv = as.numeric(jwbs1_dv))

# distribution measures
#mean(dfas1_end_exp1$jwbs1_dv, na.rm = TRUE)
#median(dfas1_end_exp1$jwbs1_dv, na.rm = TRUE)
#min(dfas1_end_exp1$jwbs1_dv, na.rm = TRUE)
#max(dfas1_end_exp1$jwbs1_dv, na.rm = TRUE)
#jb_anx_exp1 <- quantile(dfas1_end_exp1$jwbs1_dv, na.rm = TRUE)

## Depression subscale
# originally devised by Warr (1990): Depression subscale 
# (from Warr’s "Depression-Enthusiasm" scale)

# covert to numeric
dfas1_end_exp1 <- dfas1_end_exp1 %>% 
  mutate(jwbs2_dv = ifelse(jwbs2_dv=="least depressed",15,
                           ifelse(jwbs2_dv%in%c("missing","don't know",
                                                "inapplicable","refusal", 
                                                "proxy"),"NA",jwbs2_dv))) %>% 
  mutate(jwbs2_dv = as.numeric(jwbs2_dv))


# distribution measures
#mean(dfas1_end_exp1$jwbs2_dv, na.rm = TRUE)
#median(dfas1_end_exp1$jwbs2_dv, na.rm = TRUE)
#min(dfas1_end_exp1$jwbs2_dv, na.rm = TRUE)
#max(dfas1_end_exp1$jwbs2_dv, na.rm = TRUE)
#jb_dep_exp1 <- quantile(dfas1_end_exp1$jwbs2_dv, na.rm = TRUE)

#####----------------------------------------------------------------------#####
#####                  Create dataframe in format for table                #####
#####----------------------------------------------------------------------#####

temp1 <- sample_chars_endpoint_exp1 %>% 
  ungroup() %>% 
  filter(jbterm1=="a permanent job") %>% 
  rename("permanent_n"="n", "permanent_est"="est") %>% 
  select(-jbterm1)

temp2 <- sample_chars_endpoint_exp1 %>% 
  ungroup() %>% 
  filter(jbterm1=="not permanent job") %>% 
  rename("not_permanent_n"="n", "not_permanent_est"="est") %>% 
  select(-jbterm1)

temp3 <-  sample_chars_endpoint_exp1 %>% 
  ungroup() %>% 
  filter(jbterm1=="inapplicable") %>% 
  rename("inapplicable_n"="n", "inapplicable_est"="est") %>% 
  select(-jbterm1)
  
temp4 <- sample_chars_endpoint_exp1 %>% 
  ungroup() %>% 
  filter(jbterm1=="don't know") %>% 
  rename("dk_n"="n", "dk_est"="est") %>% 
  select(-jbterm1)

sample_chars_endpoint_exp1_wide <- temp1 %>% 
  full_join(temp2, by = c("var","measure")) %>% 
  full_join(temp3, by = c("var","measure")) %>% 
  full_join(temp4, by = c("var","measure")) 

rm(temp1, temp2, temp3, temp4)

#####----------------------------------------------------------------------#####
#####                         Save sample chars data                       #####
#####----------------------------------------------------------------------#####

## as dataframe
# long
write_rds(sample_chars_endpoint_exp1, 
          "./working_data/sample_chars_endpoint_exp1.rds")

# wide
write_rds(sample_chars_endpoint_exp1_wide, 
          "./working_data/sample_chars_endpoint_exp1_wide.rds")




################################################################################
#####                           sequence analysis                          #####
################################################################################

dfas1_seq <- dfas1 %>% 
  mutate(emp_contract = ifelse(jbterm1 %in% c("not permanent job", 
                                              "or is there some way that it is not permanent?",
                                              "Or is there some way that it is not permanent?"),
                               "fixed-term",
                               ifelse(jbterm1 %in% c("a permanent job",
                                                     "A permanent job"),
                                      "permanent",
                                      ifelse(jbstat %in% c("Unemployed",
                                                           "unemployed"),
                                             "unemployed",
                                             ifelse(jbstat %in% c("on maternity leave",
                                                                  "On maternity leave",
                                                                  "Family care or home",
                                                    "full-time student",
                                                    "Full-time student",
                                                    "LT sick or disabled",
                                                    "Govt training scheme", # or fixed-term?
                                                    "On apprenticeship",    # or fixed-term?
                                                    "Unpaid, family business",
                                                    "doing something else",
                                                    "Doing something else"), 
                                                    "not in employment",
                                                    "inapplicable")))))


## data checks
# cases 
dfas1_seq %>% filter(jbstat %in% c("Unemployed",
                                    "unemployed") & 
                               jbterm1 %in% c("not permanent job", 
                                              "or is there some way that it is not permanent?",
                                              "Or is there some way that it is not permanent?"))
# 76 cases where jbstat is unemployed but jbterm1 reports non-permanent job
# jbterm2 usually seems to have additional emp info
# propose using jbterm1 as primary coding source for emp_contract


## wide format for creating sequence data
dfas1_seq_wide  <-  dfas1_seq %>% 
  select(pidp,wv_n,emp_contract) %>% 
  mutate(wv=paste0("wv_",wv_n)) %>% 
  select(-wv_n) %>% 
  pivot_wider(names_from = wv, values_from = emp_contract, values_fill = "missing")

### problems with these <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
emp_contract_labs <- c("fixed term", "inapplicable", "not in employment", 
                "permanent", "unemployed", "missing" )
emp_contract_code <- c("FT", "IN", "NE", "PE", "UE", "NA")
emp_contract.seq <- seqdef(dfas1_seq_wide, 2:10, states = emp_contract_code,
                    labels = emp_contract_labs)

## first 10 sequences
#png("./output/descriptive/seqiplot100_emp_contract.png", width = 960, height = 960)
seqiplot(emp_contract.seq,
         idxs=1:100, # to add more lines
         with.legend = F, 
         main = "Index plot (100 first sequences)",
         border = NA)
#dev.off()

## all sequences
#png("output/descriptive/seqiplot_emp_contract.png", width = 960, height = 1920)
seqIplot(emp_contract.seq,
         with.legend = F, 
         main = "Index plot (all sequences)",
         border = NA)
#dev.off()

# sequence frequency plot (all common sequences)
#png("output/descriptive/seqfplot_emp_contract.png", width = 960, height = 1920)
seqfplot(emp_contract.seq, 
         idxs=1:900, # to add more lines
         with.legend = F, 
         border = NA, 
         main = "Sequence frequency plot")
#dev.off()

# state distribution plot
#png("output/descriptive/seqdplot_emp_contract.png", width = 960, height = 960)
seqdplot(emp_contract.seq, 
         with.legend = F, 
         border = NA, 
         main = "State distribution plot")
#dev.off()

# legend
#png("output/descriptive/legend_emp_contract.png", width = 240, height = 190)
seqlegend(emp_contract.seq, cex = 1.3)
#dev.off()

################################################################################
#####                           cluster analysis                           #####
################################################################################

################################################################################
#####                           outcome analysis                           #####
################################################################################

