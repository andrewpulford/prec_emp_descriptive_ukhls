################################################################################

# Persistent precarious employment and health - Understanding Society
# 1-5 - descriptive analysis
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


#### ethnicity -----------------------------------------------------------------
## full ethnicity coding
ethnicity <- dfas1_end %>% group_by(wv_n,ethn_dv) %>% 
  summarise(n=n()) %>% 
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Ethnicity") %>% 
  rename("measure"= "ethn_dv") %>% 
  select(wv_n, var, measure, n, est)

## white/non-white
white_non <- dfas1_end %>% group_by(wv_n,non_white) %>% 
  summarise(n=n()) %>%  
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Ethnicity") %>% 
  rename("measure"= "non_white") %>% 
  select(wv_n, var, measure, n, est)

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(white_non)

#### Marital status -------------------------------------------------------------
marital <- dfas1_end %>% group_by(wv_n,marital_status) %>% summarise(n=n()) %>%  
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Marital status") %>% 
  rename("measure"= "marital_status") %>% 
  select(wv_n, var, measure, n, est)

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
  select(wv_n,var, measure, n, est)

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
  select(wv_n,var, measure, n, est)

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(emp_broken)

#### perceived job security in the next 12 months ------------------------------
# even # waves only
### here - reorder jbsec_dv <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# summary df
job_sec <- dfas1_end %>% group_by(wv_n, jbsec_dv) %>% summarise(n=n()) %>% 
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Perceived job security") %>% 
  rename("measure"= "jbsec_dv") %>% 
  select(wv_n,var, measure, n, est)

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
#####                        Health characteristics                        #####
#####----------------------------------------------------------------------#####

#### long-standing illness or impairment ---------------------------------------
# risk of reverse causation - need to be incident from previous wave?
ltc <- dfas1_end %>% group_by(wv_n, health) %>% summarise(n=n()) %>%  
  mutate(pc = n/sum(n)*100)

# don't add for now

#### self-rated health ---------------------------------------------------------

srh <- dfas1_end %>% 
  # recode self-rated health variables into one
#  mutate(sf1 = as.character(sf1),
#         scsf1 = as.character(scsf1)) %>% 
#  mutate(srh_dv = ifelse(sf1=="inapplicable",scsf1, sf1)) %>% 
  group_by(wv_n, srh_dv) %>% summarise(n=n()) %>%  
  mutate(est = n/sum(n)*100) %>% 
  mutate(var="Self-rated health") %>% 
  rename("measure"= "srh_dv") %>% 
  select(wv_n,var, measure, n, est)

sample_chars_endpoint <- sample_chars_endpoint %>% bind_rows(srh)


#### GHQ-12 --------------------------------------------------------------------
## calculate caseness (cut point = 3)
#dfas1_end <- dfas1_end %>% mutate(ghq_case3 = ifelse(grepl("0",as.character(scghq2_dv)),0,
#                                             ifelse(grepl("1",as.character(scghq2_dv)),0,
#                                                    ifelse(grepl("2",as.character(scghq2_dv)),0,
#                                                           ifelse(grepl("3",as.character(scghq2_dv)),1,
#                                                                  ifelse(grepl("4",as.character(scghq2_dv)),1,
#                                                                         ifelse(grepl("5",as.character(scghq2_dv)),1,
#                                                                                ifelse(grepl("6",as.character(scghq2_dv)),1,
#                                                                                       ifelse(grepl("7",as.character(scghq2_dv)),1,
#                                                                                              ifelse(grepl("8",as.character(scghq2_dv)),1,
#                                                                                                     ifelse(grepl("9",as.character(scghq2_dv)),1,
#                                                                                                            ifelse(grepl("10",as.character(scghq2_dv)),1,
#                                                                                                                   ifelse(grepl("11",as.character(scghq2_dv)),1,
#                                                                                                                          ifelse(grepl("12",as.character(scghq2_dv)),1,
#                                                                                                                                 as.character(scghq2_dv))))))))))))))) 
#
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
#####           Descriptive analysis for exposure variables                #####
################################################################################

### cross tab for employment contract and broken employment spells
## create df for cross tab
expos_df1 <- dfas1_end %>% 
  select(emp_contract, broken_emp) %>% 
  group_by(emp_contract, broken_emp) %>% 
  summarise(total=n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = broken_emp, values_from = total, values_fill = 0) 

## chi square test
expos_df1 %>% 
  select(-emp_contract) %>% 
  chisq.test() %>% 
  glance() #%>% 
#  pull(p.value) # add this is you want to extract p value only

### cross tab for employment contract and multiple jobs
expos_df2 <- dfas1_end %>% 
  select(emp_contract, j2has_dv) %>% 
  group_by(emp_contract, j2has_dv) %>% 
  summarise(total=n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = j2has_dv, values_from = total, values_fill = 0) 

## chi square test
expos_df2 %>% 
  select(-emp_contract) %>% 
  chisq.test() %>% 
  glance()  #%>% 
#  pull(p.value) # add this is you want to extract p value only

### cross tab for broken employment spells and multiple jobs
expos_df3 <- dfas1_end %>% 
  select(broken_emp, j2has_dv) %>% 
  group_by(broken_emp, j2has_dv) %>% 
  summarise(total=n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = j2has_dv, values_from = total, values_fill = 0) 

## chi square test
expos_df3 %>% 
  select(-broken_emp) %>% 
  chisq.test() %>% 
  glance()  #%>% 
#  pull(p.value) # add this is you want to extract p value only


################################################################################
#####                           sequence analysis                          #####
################################################################################

#### ---------------------------------------------------------------------------
#### Employment contract
#### ---------------------------------------------------------------------------

### recode employment status variables to create an employment contract variable
dfas1a_seq <- dfas1a

dfas1b_seq <- dfas1b 

### wide format for creating sequence data
dfas1a_seq_wide  <-  dfas1a_seq %>% 
  select(pidp,wv_n,emp_contract) %>% 
  mutate(wv=paste0("wv_",wv_n)) %>% 
  select(-wv_n) %>% 
  pivot_wider(names_from = wv, values_from = emp_contract, values_fill = "missing")

dfas1b_seq_wide  <-  dfas1b_seq %>% 
  select(pidp,wv_n,emp_contract) %>% 
  mutate(wv=paste0("wv_",wv_n)) %>% 
  select(-wv_n) %>% 
  pivot_wider(names_from = wv, values_from = emp_contract, values_fill = "missing")

### define labels and codes for sequence analysis
## retaining missing values for now but plan to imput
emp_contract_labs <- c("fixed term", "missing", "not in employment", 
                "permanent", "unemployed" )
emp_contract_code <- c("FT", "NE", "PE", "UE", "NA")

### create sequence data
emp_contract.seq.a <- seqdef(dfas1a_seq_wide, 2:5, states = emp_contract_code,
                    labels = emp_contract_labs)

emp_contract.seq.b <- seqdef(dfas1b_seq_wide, 2:5, states = emp_contract_code,
                             labels = emp_contract_labs)

## first 10 sequences
#png("./output/descriptive/seqiplot100_emp_contract.png", width = 960, height = 960)
par(mfrow=c(1,3))
seqiplot(emp_contract.seq.a,
         idxs=1:100, # to add more lines
         with.legend = F, 
         main = "Index plot (100 first sequences)",
         border = NA)
seqiplot(emp_contract.seq.b,
         idxs=1:100, # to add more lines
         with.legend = F, 
         main = "Index plot (100 first sequences)",
         border = NA)
seqlegend(emp_contract.seq.a, cex = 1.3)
#dev.off()

## all sequences
#png("output/descriptive/seqiplot_emp_contract.png", width = 960, height = 1920)
par(mfrow=c(1,3))
seqIplot(emp_contract.seq.a,
         with.legend = F, 
         main = "Index plot (all sequences)",
         border = NA)
seqIplot(emp_contract.seq.b,
         with.legend = F, 
         main = "Index plot (all sequences)",
         border = NA)
seqlegend(emp_contract.seq.a, cex = 1.3)
#dev.off()

# sequence frequency plot (all common sequences)
#png("output/descriptive/seqfplot_emp_contract.png", width = 960, height = 1920)
par(mfrow=c(1,3))
seqfplot(emp_contract.seq.a, 
         idxs=1:900, # to add more lines
         with.legend = F, 
         border = NA, 
         main = "Sequence frequency plot")
seqfplot(emp_contract.seq.b, 
         idxs=1:900, # to add more lines
         with.legend = F, 
         border = NA, 
         main = "Sequence frequency plot")
seqlegend(emp_contract.seq.a, cex = 1.3)
#dev.off()

# state distribution plot
#png("output/descriptive/seqdplot_emp_contract.png", width = 960, height = 960)
par(mfrow=c(1,3))
seqdplot(emp_contract.seq.a, 
         with.legend = F, 
         border = NA, 
         main = "State distribution plot")
seqdplot(emp_contract.seq.b, 
         with.legend = F, 
         border = NA, 
         main = "State distribution plot")
seqlegend(emp_contract.seq.a, cex = 1.3)
#dev.off()

# legend
#png("output/descriptive/legend_emp_contract.png", width = 240, height = 190)
seqlegend(emp_contract.seq.a, cex = 1.3)
#dev.off()

#### ---------------------------------------------------------------------------
#### Broken employment
#### ---------------------------------------------------------------------------

### employment spells -----
# create numeric version
dfas1a_seq2 <- dfas1a

dfas1b_seq2 <- dfas1b 

### wide format for creating sequence data
dfas1a_seq_wide2  <-  dfas1a_seq2 %>% 
  select(pidp,wv_n,broken_emp) %>% 
  mutate(wv=paste0("wv_",wv_n)) %>% 
  select(-wv_n) %>% 
  pivot_wider(names_from = wv, values_from = broken_emp, values_fill = "missing")

dfas1b_seq_wide2  <-  dfas1b_seq2 %>% 
  select(pidp,wv_n,broken_emp) %>% 
  mutate(wv=paste0("wv_",wv_n)) %>% 
  select(-wv_n) %>% 
  pivot_wider(names_from = wv, values_from = broken_emp, values_fill = "missing")

### define labels and codes for sequence analysis
## retaining missing values for now but plan to impute
broken_emp_labs <- c("Broken employment", "Missing", "No employment spells", "Unbroken employment" )
broken_emp_code <- c("BE","NA", "NE", "UE")

### create sequence data
broken_emp.seq.a <- seqdef(dfas1a_seq_wide2, 2:5, states = broken_emp_code,
                             labels = broken_emp_labs)

broken_emp.seq.b <- seqdef(dfas1b_seq_wide2, 2:5, states = broken_emp_code,
                           labels = broken_emp_labs)

## first 10 sequences
#png("./output/descriptive/seqiplot100_emp_contract.png", width = 960, height = 960)
par(mfrow=c(1,3))
seqiplot(broken_emp.seq.a,
         idxs=1:100, # to add more lines
         with.legend = F, 
         main = "Index plot (100 first sequences)",
         border = NA)
seqiplot(broken_emp.seq.b,
         idxs=1:100, # to add more lines
         with.legend = F, 
         main = "Index plot (100 first sequences)",
         border = NA)
seqlegend(broken_emp.seq.a, cex = 1.3)
#dev.off()

## all sequences
#png("output/descriptive/seqiplot_emp_contract.png", width = 960, height = 1920)
par(mfrow=c(1,3))
seqIplot(broken_emp.seq.a,
         with.legend = F, 
         main = "Index plot (all sequences)",
         border = NA)
seqIplot(broken_emp.seq.b,
         with.legend = F, 
         main = "Index plot (all sequences)",
         border = NA)
seqlegend(broken_emp.seq.a, cex = 1.3)
#dev.off()

# sequence frequency plot (all common sequences)
#png("output/descriptive/seqfplot_emp_contract.png", width = 960, height = 1920)
par(mfrow=c(1,3))
seqfplot(broken_emp.seq.a, 
         idxs=1:900, # to add more lines
         with.legend = F, 
         border = NA, 
         main = "Sequence frequency plot")
seqfplot(broken_emp.seq.b, 
         idxs=1:900, # to add more lines
         with.legend = F, 
         border = NA, 
         main = "Sequence frequency plot")
seqlegend(broken_emp.seq.a, cex = 1.3)
#dev.off()

# state distribution plot
#png("output/descriptive/seqdplot_emp_contract.png", width = 960, height = 960)
par(mfrow=c(1,3))
seqdplot(broken_emp.seq.a, 
         with.legend = F, 
         border = NA, 
         main = "State distribution plot")
seqdplot(broken_emp.seq.b, 
         with.legend = F, 
         border = NA, 
         main = "State distribution plot")
seqlegend(broken_emp.seq.a, cex = 1.3)
#dev.off()

# legend
#png("output/descriptive/legend_emp_contract.png", width = 240, height = 190)
seqlegend(broken_emp.seq.a, cex = 1.3)
#dev.off()

#### ---------------------------------------------------------------------------
#### Multiple jobs
#### ---------------------------------------------------------------------------

dfas1a_seq3 <- dfas1a
dfas1b_seq3 <- dfas1b

### wide format for creating sequence data
dfas1a_seq_wide3  <-  dfas1a_seq3 %>% 
  select(pidp,wv_n,j2has_dv) %>% 
  mutate(wv=paste0("wv_",wv_n)) %>% 
  select(-wv_n) %>% 
  pivot_wider(names_from = wv, values_from = j2has_dv, values_fill = "missing")

dfas1b_seq_wide3  <-  dfas1b_seq3 %>% 
  select(pidp,wv_n,j2has_dv) %>% 
  mutate(wv=paste0("wv_",wv_n)) %>% 
  select(-wv_n) %>% 
  pivot_wider(names_from = wv, values_from = j2has_dv, values_fill = "missing")

### define labels and codes for sequence analysis
## retaining missing values for now but plan to impute
multi_jobs_labs <- c("Missing", "No", "not in employment", "unemployed", "Yes")
multi_jobs_code <- c("NA","N", "NE", "UE", "Y")

### create sequence data
multi_jobs.seq.a <- seqdef(dfas1a_seq_wide3, 2:5, states = multi_jobs_code,
                           labels = multi_jobs_labs)

multi_jobs.seq.b <- seqdef(dfas1b_seq_wide3, 2:5, states = multi_jobs_labs,
                           labels = multi_jobs_code)

## first 10 sequences
#png("./output/descriptive/seqiplot100_emp_contract.png", width = 960, height = 960)
par(mfrow=c(1,3))
seqiplot(multi_jobs.seq.a,
         idxs=1:100, # to add more lines
         with.legend = F, 
         main = "Index plot (100 first sequences)",
         border = NA)
seqiplot(multi_jobs.seq.b,
         idxs=1:100, # to add more lines
         with.legend = F, 
         main = "Index plot (100 first sequences)",
         border = NA)
seqlegend(multi_jobs.seq.a, cex = 1.3)
#dev.off()

## all sequences
#png("output/descriptive/seqiplot_emp_contract.png", width = 960, height = 1920)
par(mfrow=c(1,3))
seqIplot(multi_jobs.seq.a,
         with.legend = F, 
         main = "Index plot (all sequences)",
         border = NA)
seqIplot(multi_jobs.seq.b,
         with.legend = F, 
         main = "Index plot (all sequences)",
         border = NA)
seqlegend(multi_jobs.seq.a, cex = 1.3)
#dev.off()

# sequence frequency plot (all common sequences)
#png("output/descriptive/seqfplot_emp_contract.png", width = 960, height = 1920)
par(mfrow=c(1,3))
seqfplot(multi_jobs.seq.a, 
         idxs=1:900, # to add more lines
         with.legend = F, 
         border = NA, 
         main = "Sequence frequency plot")
seqfplot(multi_jobs.seq.b, 
         idxs=1:900, # to add more lines
         with.legend = F, 
         border = NA, 
         main = "Sequence frequency plot")
seqlegend(multi_jobs.seq.a, cex = 1.3)
#dev.off()

# state distribution plot
#png("output/descriptive/seqdplot_emp_contract.png", width = 960, height = 960)
par(mfrow=c(1,3))
seqdplot(multi_jobs.seq.a, 
         with.legend = F, 
         border = NA, 
         main = "State distribution plot")
seqdplot(multi_jobs.seq.b, 
         with.legend = F, 
         border = NA, 
         main = "State distribution plot")
seqlegend(multi_jobs.seq.a, cex = 1.3)
#dev.off()

# legend
#png("output/descriptive/legend_emp_contract.png", width = 240, height = 190)
seqlegend(multi_jobs.seq.a, cex = 1.3)
#dev.off()

################################################################################
#####                           cluster analysis                           #####
################################################################################

################################################################################
#####                           outcome analysis                           #####
################################################################################

