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
#library(srvyr) # for applying survey weights to analysis

citation("TraMineR")

################################################################################
#####                         load and prepare data                        #####
################################################################################


### analytic sample 1a - waves 3-6
dfas1a <- readRDS("./analytic_sample_data/dfas1a.rds") %>% 
  mutate(sample_group = "a")# %>% select(-valid_6)

## endpoint only
dfas1a_end <- dfas1a %>% filter(wv_n==6)
write_rds(dfas1a_end, "./working_data/dfas1a_end.rds")

### analytic sample 1b - waves 7-10
dfas1b <- readRDS("./analytic_sample_data/dfas1b.rds") %>% 
  mutate(sample_group = "b")# %>% select(-valid_10)

## endpoint only
dfas1b_end <- dfas1b %>% filter(wv_n==10) 
write_rds(dfas1b_end, "./working_data/dfas1b_end.rds")

### combined study endpoint df
dfas1_end <- dfas1a_end %>% bind_rows(dfas1b_end)

################################################################################
#####                sample characteristics at study endpoint              #####
################################################################################


#####----------------------------------------------------------------------#####
#####                     Personal characteristics                         #####
#####----------------------------------------------------------------------#####

#### sex -----------------------------------------------------------------------
#dfas1_end$sex_dv <- droplevels(dfas1_end$sex_dv)

sex <- dfas1_end %>% group_by(wv_n,sex_dv) %>% summarise(n=n()) %>% 
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Sex") %>% 
  rename("measure"= "sex_dv") %>% 
  dplyr::select(wv_n, var, measure, n, est) %>% 
  arrange(wv_n, factor(measure, levels = c("Female","Male")))

sample_chars_endpoint <- sex


#### age -----------------------------------------------------------------------
age_mean <- dfas1_end %>% group_by(wv_n) %>% 
  summarise(est = mean(as.numeric(as.numeric(age_dv)), na.rm = TRUE)) %>% 
  mutate(var="Age", measure="Mean", n=NA) %>% 
  dplyr::select(wv_n, var, measure, n, est)

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(age_mean)


#### ethnicity -----------------------------------------------------------------
## full ethnicity coding
ethnicity <- dfas1_end %>% group_by(wv_n,ethn_dv) %>% 
  summarise(n=n()) %>% 
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Ethnicity") %>% 
  rename("measure"= "ethn_dv") %>% 
  dplyr::select(wv_n, var, measure, n, est)

## white/non-white
white_non <- dfas1_end %>% group_by(wv_n,non_white) %>% 
  summarise(n=n()) %>%  
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Ethnicity") %>% 
  rename("measure"= "non_white") %>% 
  dplyr::select(wv_n, var, measure, n, est) %>% 
  arrange(wv_n, factor(measure, levels = c("White","Non-white","Missing")))


sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(white_non)

#### Marital status -------------------------------------------------------------
marital <- dfas1_end %>% group_by(wv_n,marital_status) %>% summarise(n=n()) %>%  
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Marital status") %>% 
  rename("measure"= "marital_status") %>% 
  dplyr::select(wv_n, var, measure, n, est) %>% 
  arrange(wv_n, factor(measure, levels = c("married/civil partnership","divorced/separated/widowed","single","missing")))


sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(marital)

#### Educational attainment ----------------------------------------------------
ed_attain <- dfas1_end %>% group_by(wv_n,hiqual_dv) %>% summarise(n=n()) %>% 
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Educational attainment") %>% 
  rename("measure"= "hiqual_dv") %>% 
  dplyr::select(wv_n, var, measure, n, est) %>% 
  arrange(wv_n, factor(measure, levels = c("degree",
                                           "other higher degree",
                                           "a-level etc",
                                           "gcse etc",
                                           "other qualification",
                                           "no qualification")))


sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(ed_attain)

#### Region --------------------------------------------------------------------
region <- dfas1_end %>% group_by(wv_n,gor_dv) %>% summarise(n=n()) %>% 
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Region") %>% 
  rename("measure"= "gor_dv") %>% 
  dplyr::select(wv_n, var, measure, n, est)

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(region)

#####----------------------------------------------------------------------#####
#####               Employment and income characteristics                  #####
#####----------------------------------------------------------------------#####

#### Current employment status -------------------------------------------------
#emp <-  dfas1_end %>% 
#  group_by(wv_n,employ) %>% summarise(n=n()) %>%  
#  mutate(pc = n/sum(n)*100)
#
##### current labour force status -----------------------------------------------
#lab_status <- dfas1_end %>% group_by(wv_n,jbstat) %>% summarise(n=n()) %>%  
#  mutate(est = n/sum(n)*100) %>% 
#  mutate(var="Labour status") %>% 
#  rename("measure"= "jbstat") %>% 
#  select(wv_n,var, measure, n, est)
#
#sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(lab_status)


#### Current job: Three Class NS-SEC -------------------------------------------
#nssec3 <- dfas1_end %>% group_by(wv_n,jbnssec3_dv) %>% summarise(n=n()) %>% 
#  mutate(est = n/sum(n)*100) %>% 
#  mutate(var="NS-SEC3") %>% 
#  rename("measure"= "jbnssec3_dv") %>% 
#  select(wv_n,var, measure, n, est)
## recode inapplicable based on emp status? <<<<<<<<<
#
#sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(nssec3)

#### RG Social Class: present job ----------------------------------------------
#rg_class <- dfas1_end %>% group_by(wv_n,jbrgsc_dv) %>% summarise(n=n()) %>% 
#  mutate(pc = n/sum(n)*100)

# don't add for now

#### permanent or temporary ----------------------------------------------------
perm_emp <- dfas1_end %>% group_by(wv_n,emp_contract) %>% summarise(n=n()) %>% 
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Employment contract") %>% 
  rename("measure"= "emp_contract") %>% 
  dplyr::select(wv_n,var, measure, n, est) %>% 
  arrange(wv_n, factor(measure, levels = c("fixed-term",
                                           "permanent",
                                           "unemployed/not in employment")))



sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(perm_emp)


#### Employment spells since last interview ------------------------------------

### employment spells -----
# create numeric version
#dfas1_end <- dfas1_end %>% 
#  mutate(nmpsp_dv=ifelse(nmpsp_dv=="none","0",nmpsp_dv)) %>% 
#  mutate(nmpsp_dvn=as.numeric(nmpsp_dv))
#
## create binary version (one spell or more)
#dfas1_end <- dfas1_end %>% mutate(emp_spells_bin=ifelse(nmpsp_dvn<1,"no",
#                                                ifelse(nmpsp_dvn>=1,"yes", NA)))
#
#### non-employment spells -----
## create numeric version
#dfas1_end <- dfas1_end %>% 
#  mutate(nnmpsp_dv=ifelse(nnmpsp_dv=="none","0",nnmpsp_dv)) %>% 
#  mutate(nnmpsp_dvn=as.numeric(nnmpsp_dv))
#
## create binary version (one or more spell)
#dfas1_end <- dfas1_end %>% mutate(nonemp_spells_bin=ifelse(nnmpsp_dvn<1,"no",
#                                                   ifelse(nnmpsp_dvn>=1,"yes", 
#                                                          NA)))
#
#### unemployment spells -----
## create numeric version
#dfas1_end <- dfas1_end %>% 
#  mutate(nunmpsp_dv=ifelse(nunmpsp_dv=="none","0",nunmpsp_dv)) %>% 
#  mutate(nunmpsp_dvn=as.numeric(nunmpsp_dv))
#
#
## create binary version (one or more spell)
#dfas1_end <- dfas1_end %>% mutate(unemp_spells_bin=ifelse(nunmpsp_dvn<1,"no",
#                                                  ifelse(nunmpsp_dvn>=1,"yes", 
#                                                         NA)))
#
#### create broken employment variable ------
#dfas1_end <- dfas1_end %>% 
#  mutate(broken_emp = ifelse(emp_spells_bin=="no","No employment spells", 
#                             ifelse(emp_spells_bin=="yes" & 
#                                      nonemp_spells_bin=="no" & 
#                                      unemp_spells_bin=="no",
#                                    "Unbroken employment",
#                                    ifelse(emp_spells_bin=="yes" & 
#                                             (nonemp_spells_bin=="yes" |
#                                                unemp_spells_bin=="yes"),
#                                           "Broken employment","check"))))


emp_broken <- dfas1_end %>% group_by(wv_n,broken_emp) %>% 
  summarise(n=n()) %>%  
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Broken employment") %>% 
  rename("measure"= "broken_emp") %>% 
  dplyr::select(wv_n,var, measure, n, est)  %>% 
  arrange(wv_n, factor(measure, levels = c("unbroken employment",
                                           "broken employment",
                                           "no employment spells")))


sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(emp_broken)

#### perceived job security in the next 12 months ------------------------------
# even # waves only

# reorder jbsec_dv variable
dfas1_end$jbsec_dv <- factor(dfas1_end$jbsec_dv, 
                             levels = c("very likely",
                                        "likely",      
                                        "unlikely",
                                        "very unlikely", 
                                        "missing"))

# summary df
job_sec <- dfas1_end %>% group_by(wv_n, jbsec_dv) %>% summarise(n=n()) %>% 
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Perceived job security") %>% 
  rename("measure"= "jbsec_dv") %>% 
  dplyr::select(wv_n,var, measure, n, est)

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(job_sec)

#### Multiple jobs -------------------------------------------------------------
### has a 2nd job ----
emp_2nd <- dfas1_end %>% group_by(wv_n, j2has) %>% summarise(n=n()) %>%
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Multiple jobs") %>% 
  rename("measure"= "j2has") %>% 
  dplyr::select(wv_n,var, measure, n, est)  %>% 
  arrange(wv_n, factor(measure, levels = c("no",
                                           "yes")))



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
  dplyr::select(wv_n,var, measure, n, est)

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(inc_quantile)


#####----------------------------------------------------------------------#####
#####                        Health characteristics                        #####
#####----------------------------------------------------------------------#####

#### long-standing illness or impairment ---------------------------------------
# risk of reverse causation - need to be incident from previous wave?
#ltc <- dfas1_end %>% group_by(wv_n, health) %>% summarise(n=n()) %>%  
#  mutate(pc = n/sum(n)*100)
#
# don't add for now

#### self-rated health ---------------------------------------------------------

dfas1_end$srh_dv <- factor(dfas1_end$srh_dv, 
                           levels = c("excellent", "very good", "good",
                                      "fair", "poor"))

srh <- dfas1_end %>% 
  group_by(wv_n, srh_dv) %>% summarise(n=n()) %>%  
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Self-rated health") %>% 
  rename("measure"= "srh_dv") %>% 
  dplyr::select(wv_n,var, measure, n, est)

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(srh)


#### GHQ-12 --------------------------------------------------------------------

ghq3 <- dfas1_end %>% group_by(wv_n, ghq_case3) %>% summarise(n=n()) %>%  
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="GHQ12 score") %>% 
  rename("measure"= "ghq_case3") %>% 
  dplyr::select(wv_n,var, measure, n, est) %>% 
  arrange(wv_n, factor(measure, levels = c("0-2",
                                           "3 or more")))


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
#####           Descriptive analysis for exposure variables                #####
################################################################################

### cross tab for employment contract and broken employment spells
## create df for cross tab
expos_df1 <- dfas1_end %>% 
  dplyr::select(emp_contract, broken_emp) %>% 
  group_by(emp_contract, broken_emp) %>% 
  summarise(total=n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = broken_emp, values_from = total, values_fill = 0) 

## chi square test
expos_df1 %>% 
  dplyr::select(-emp_contract) %>% 
  chisq.test() %>% 
  glance() #%>% 
#  pull(p.value) # add this is you want to extract p value only

### cross tab for employment contract and multiple jobs
expos_df2 <- dfas1_end %>% 
  dplyr::select(emp_contract, j2has_dv) %>% 
  group_by(emp_contract, j2has_dv) %>% 
  summarise(total=n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = j2has_dv, values_from = total, values_fill = 0) 

## chi square test
expos_df2 %>% 
  dplyr::select(-emp_contract) %>% 
  chisq.test() %>% 
  glance()  #%>% 
#  pull(p.value) # add this is you want to extract p value only

### cross tab for broken employment spells and multiple jobs
expos_df3 <- dfas1_end %>% 
  dplyr::select(broken_emp, j2has_dv) %>% 
  group_by(broken_emp, j2has_dv) %>% 
  summarise(total=n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = j2has_dv, values_from = total, values_fill = 0) 

## chi square test
expos_df3 %>% 
  dplyr::select(-broken_emp) %>% 
  chisq.test() %>% 
  glance()  #%>% 
#  pull(p.value) # add this is you want to extract p value only


#test <- dfas1_end %>% 
#  filter(emp_contract!="unemployed/not in employment" & 
#           broken_emp == "No employment spells") %>% 
#  select(pidp,wv_n,emp_contract,broken_emp, emp_spells_bin, nmpsp_dv, 
#         unemp_spells_bin, nunmpsp_dv, nonemp_spells_bin, nnmpsp_dv, jbterm1,
#         employ,jbstat)


