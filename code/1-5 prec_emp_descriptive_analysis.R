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

### analytic sample 1b - waves 7-10
dfas1b <- readRDS("./analytic_sample_data/dfas1b.rds") %>% 
  mutate(sample_group = "b")# %>% select(-valid_10)

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
  dplyr::select(pidp,wv_n,emp_contract) %>% 
  mutate(wv=paste0("wv_",wv_n)) %>% 
  dplyr::select(-wv_n) %>% 
  pivot_wider(names_from = wv, values_from = emp_contract, values_fill = "missing")

dfas1b_seq_wide  <-  dfas1b_seq %>% 
  dplyr::select(pidp,wv_n,emp_contract) %>% 
  mutate(wv=paste0("wv_",wv_n)) %>% 
  dplyr::select(-wv_n) %>% 
  pivot_wider(names_from = wv, values_from = emp_contract, values_fill = "missing")

### define labels and codes for sequence analysis
## retaining missing values for now but plan to imput
emp_contract_labs <- c("fixed term", "missing", "permanent", "unemployed/not in employment" )
emp_contract_code <- c("FT", "NA", "PE", "UE")

### create sequence data
emp_contract.seq.a <- seqdef(dfas1a_seq_wide, 2:5, states = emp_contract_code,
                    labels = emp_contract_labs)

emp_contract.seq.b <- seqdef(dfas1b_seq_wide, 2:5, states = emp_contract_code,
                             labels = emp_contract_labs)

## first 100 sequences
tiff("./output/descriptive/emp_contract_seqiplot100.tiff", width = 960, height = 960)
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
#seqlegend(emp_contract.seq.a, cex = 1.3)
dev.off()

## all sequences
tiff("output/descriptive/emp_contract_seqiplot.tiff", width = 960, height = 1920)
par(mfrow=c(1,3))
seqIplot(emp_contract.seq.a,
         with.legend = F, 
         main = "Index plot (all sequences)",
         border = NA)
seqIplot(emp_contract.seq.b,
         with.legend = F, 
         main = "Index plot (all sequences)",
         border = NA)
#seqlegend(emp_contract.seq.a, cex = 1.3)
dev.off()

# sequence frequency plot (all common sequences)
tiff("output/descriptive/emp_contract_seqfplot.tiff", width = 960, height = 1920)
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
#seqlegend(emp_contract.seq.a, cex = 1.3)
dev.off()

# state distribution plot
tiff("output/descriptive/emp_contract_seqdplot.tiff", width = 960, height = 960)
par(mfrow=c(1,3))
seqdplot(emp_contract.seq.a, 
         with.legend = F, 
         border = NA, 
         main = "State distribution plot")
seqdplot(emp_contract.seq.b, 
         with.legend = F, 
         border = NA, 
         main = "State distribution plot")
#seqlegend(emp_contract.seq.a, cex = 1.3)
dev.off()

# legend
tiff("output/descriptive/emp_contract_legend.tiff", width = 400, height = 250)
seqlegend(emp_contract.seq.a, cex = 1.3)
dev.off()

#### ---------------------------------------------------------------------------
#### Broken employment
#### ---------------------------------------------------------------------------

### employment spells -----
# create numeric version
dfas1a_seq2 <- dfas1a

dfas1b_seq2 <- dfas1b 

### wide format for creating sequence data
dfas1a_seq_wide2  <-  dfas1a_seq2 %>% 
  dplyr::select(pidp,wv_n,broken_emp) %>% 
  mutate(wv=paste0("wv_",wv_n)) %>% 
#  mutate(broken_emp = ifelse(is.na(broken_emp),"missing",broken_emp)) %>% 
  dplyr::select(-wv_n) %>% 
  pivot_wider(names_from = wv, values_from = broken_emp, values_fill = "missing")

dfas1b_seq_wide2  <-  dfas1b_seq2 %>% 
  dplyr::select(pidp,wv_n,broken_emp) %>% 
  mutate(broken_emp = ifelse(is.na(broken_emp),"missing",broken_emp)) %>% 
  mutate(wv=paste0("wv_",wv_n)) %>% 
  dplyr::select(-wv_n) %>% 
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
tiff("./output/descriptive/broken_emp_seqiplot100.tiff", width = 960, height = 960)
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
#seqlegend(broken_emp.seq.a, cex = 1.3)
dev.off()

## all sequences
tiff("output/descriptive/broken_emp_seqiplot.tiff", width = 960, height = 1920)
par(mfrow=c(1,3))
seqIplot(broken_emp.seq.a,
         with.legend = F, 
         main = "Index plot (all sequences)",
         border = NA)
seqIplot(broken_emp.seq.b,
         with.legend = F, 
         main = "Index plot (all sequences)",
         border = NA)
#seqlegend(broken_emp.seq.a, cex = 1.3)
dev.off()

# sequence frequency plot (all common sequences)
tiff("output/descriptive/broken_emp_seqfplot.tiff", width = 960, height = 1920)
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
#seqlegend(broken_emp.seq.a, cex = 1.3)
dev.off()

# state distribution plot
tiff("output/descriptive/broken_emp_seqdplot.tiff", width = 960, height = 960)
par(mfrow=c(1,3))
seqdplot(broken_emp.seq.a, 
         with.legend = F, 
         border = NA, 
         main = "State distribution plot")
seqdplot(broken_emp.seq.b, 
         with.legend = F, 
         border = NA, 
         main = "State distribution plot")
#seqlegend(broken_emp.seq.a, cex = 1.3)
dev.off()

# legend
tiff("output/descriptive/broken_emp_legend.tiff", width = 400, height = 250)
seqlegend(broken_emp.seq.a, cex = 1.3)
dev.off()

#### ---------------------------------------------------------------------------
#### Multiple jobs
#### ---------------------------------------------------------------------------

dfas1a_seq3 <- dfas1a
dfas1b_seq3 <- dfas1b

### wide format for creating sequence data
dfas1a_seq_wide3  <-  dfas1a_seq3 %>% 
  dplyr::select(pidp,wv_n,j2has_dv) %>% 
  mutate(wv=paste0("wv_",wv_n)) %>% 
  dplyr::select(-wv_n) %>% 
  pivot_wider(names_from = wv, values_from = j2has_dv, values_fill = "missing")

dfas1b_seq_wide3  <-  dfas1b_seq3 %>% 
  dplyr::select(pidp,wv_n,j2has_dv) %>% 
  mutate(wv=paste0("wv_",wv_n)) %>% 
  dplyr::select(-wv_n) %>% 
  pivot_wider(names_from = wv, values_from = j2has_dv, values_fill = "missing")

### define labels and codes for sequence analysis
## retaining missing values for now but plan to impute
multi_jobs_labs <- c("Missing", "No", "Yes")
multi_jobs_code <- c("NA","NO", "YES")

### create sequence data
multi_jobs.seq.a <- seqdef(dfas1a_seq_wide3, 2:5, states = multi_jobs_code,
                           labels = multi_jobs_labs)

multi_jobs.seq.b <- seqdef(dfas1b_seq_wide3, 2:5, states = multi_jobs_code,
                           labels = multi_jobs_labs)

## first 10 sequences
tiff("./output/descriptive/multi_emp_seqiplot100.tiff", width = 960, height = 960)
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
#seqlegend(multi_jobs.seq.a, cex = 1.3)
dev.off()

## all sequences
tiff("output/descriptive/multi_emp_seqiplot.tiff", width = 960, height = 1920)
par(mfrow=c(1,3))
seqIplot(multi_jobs.seq.a,
         with.legend = F, 
         main = "Index plot (all sequences)",
         border = NA)
seqIplot(multi_jobs.seq.b,
         with.legend = F, 
         main = "Index plot (all sequences)",
         border = NA)
#seqlegend(multi_jobs.seq.a, cex = 1.3)
dev.off()

# sequence frequency plot (all common sequences)
tiff("output/descriptive/multi_emp_seqfplot.tiff", width = 960, height = 1920)
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
#seqlegend(multi_jobs.seq.a, cex = 1.3)
dev.off()

# state distribution plot
tiff("output/descriptive/multi_emp_seqdplot.tiff", width = 960, height = 960)
par(mfrow=c(1,3))
seqdplot(multi_jobs.seq.a, 
         with.legend = F, 
         border = NA, 
         main = "State distribution plot")
seqdplot(multi_jobs.seq.b, 
         with.legend = F, 
         border = NA, 
         main = "State distribution plot")
#seqlegend(multi_jobs.seq.a, cex = 1.3)
dev.off()

# legend
tiff("output/descriptive/multi_emp_legend.tiff", width = 400, height = 250)
seqlegend(multi_jobs.seq.a, cex = 1.3)
dev.off()

####save wide sequence dfs

saveRDS(dfas1a_seq_wide, "./working_data/dfas1a_seq_wide.rds")


################################################################################
#####                            Manual grouping                          #####
################################################################################

#### ---------------------------------------------------------------------------
#### Employment contract
#### ---------------------------------------------------------------------------

### create vars to calculate groupings
## create df and single sequence variable
emp_contract_group_dfa <- data.frame(emp_contract.seq.a) %>% mutate(seq_list = paste0(wv_3,"-",wv_4,"-",wv_5,"-",wv_6))
emp_contract_group_dfb <- data.frame(emp_contract.seq.b) %>% mutate(seq_list = paste0(wv_7,"-",wv_8,"-",wv_9,"-",wv_10))

## count number of occurrences of each state in sequence
emp_contract_group_dfa$pe_n <- str_count(emp_contract_group_dfa$seq_list,"PE")
emp_contract_group_dfb$pe_n <- str_count(emp_contract_group_dfb$seq_list,"PE")

emp_contract_group_dfa$ft_n <- str_count(emp_contract_group_dfa$seq_list,"FT")
emp_contract_group_dfb$ft_n <- str_count(emp_contract_group_dfb$seq_list,"FT")

emp_contract_group_dfa$ue_n <- str_count(emp_contract_group_dfa$seq_list,"UE")
emp_contract_group_dfb$ue_n <- str_count(emp_contract_group_dfb$seq_list,"UE")

emp_contract_group_dfa$na_n <- str_count(emp_contract_group_dfa$seq_list,"NA")
emp_contract_group_dfb$na_n <- str_count(emp_contract_group_dfb$seq_list,"NA")

### code groupings as new variable 
emp_contract_group_dfa <- emp_contract_group_dfa %>% 
  mutate(emp_contract_group = ifelse(pe_n==4,"steady: permanent",
                              ifelse(pe_n==3&na_n==1, "steady: permanent",
                              ifelse(ft_n==4,"steady: non-permanent",
                              ifelse(ft_n==3&na_n==1,"steady: non-permanent",
                              ifelse(ue_n==4,"steady: un/non-employed",
                              ifelse(ue_n==3&na_n==1,"steady: un/non-employed",
                              ifelse(pe_n==3&ft_n==1,"blip: non-permanent",
                              ifelse(pe_n==3&ue_n==1,"blip: un/non-employed",
                              ifelse(na_n>1,"incomplete",
                                     "churn"))))))))))

emp_contract_group_dfb <- emp_contract_group_dfb %>% 
  mutate(emp_contract_group = ifelse(pe_n==4,"steady: permanent",
                              ifelse(pe_n==3&na_n==1, "steady: permanent",
                              ifelse(ft_n==4,"steady: non-permanent",
                              ifelse(ft_n==3&na_n==1,"steady: non-permanent",
                              ifelse(ue_n==4,"steady: un/non-employed",
                              ifelse(ue_n==3&na_n==1,"steady: un/non-employed",
                              ifelse(pe_n==3&ft_n==1,"blip: non-permanent",
                              ifelse(pe_n==3&ue_n==1,"blip: un/non-employed",
                              ifelse(na_n>1,"incomplete",
                                     "churn"))))))))))

### create long versions of dfs
emp_contract_group_dfa_long <- emp_contract_group_dfa %>% 
  group_by(emp_contract_group) %>% 
  summarise(n_a=n()) %>% 
  ungroup() %>% 
  mutate(pc_a=n_a/sum(n_a)*100) %>% 
  arrange(factor(emp_contract_group, levels = c("steady: permanent",
                                                "steady: non-permanent",
                                                "steady: un/non-employed",
                                                "churn",
                                                "blip: non-permanent",
                                                "blip: un/non-employed",
                                                "incomplete")))
                                     
emp_contract_group_dfb_long <- emp_contract_group_dfb %>% 
  group_by(emp_contract_group) %>% 
  summarise(n_b=n()) %>% 
  ungroup() %>% 
  mutate(pc_b=n_b/sum(n_b)*100) %>% 
  arrange(factor(emp_contract_group, levels = c("steady: permanent",
                                                "steady: non-permanent",
                                                "steady: un/non-employed",
                                                "churn",
                                                "blip: non-permanent",
                                                "blip: un/non-employed",
                                                "incomplete")))

### create single df and save

emp_contract_group_df <- emp_contract_group_dfa_long %>% 
  left_join(emp_contract_group_dfb_long)

write_rds(emp_contract_group_df, "./working_data/emp_contract_group_df.rds")

#### ---------------------------------------------------------------------------
#### Employment spells
#### ---------------------------------------------------------------------------

### create vars to calculate groupings
## create df and single sequence variable
emp_spells_group_dfa <- data.frame(broken_emp.seq.a) %>% mutate(seq_list = paste0(wv_3,"-",wv_4,"-",wv_5,"-",wv_6))
emp_spells_group_dfb <- data.frame(broken_emp.seq.b) %>% mutate(seq_list = paste0(wv_7,"-",wv_8,"-",wv_9,"-",wv_10))

## count number of occurrences of each state in sequence
emp_spells_group_dfa$ue_n <- str_count(emp_spells_group_dfa$seq_list,"UE")
emp_spells_group_dfb$ue_n <- str_count(emp_spells_group_dfb$seq_list,"UE")

emp_spells_group_dfa$be_n <- str_count(emp_spells_group_dfa$seq_list,"BE")
emp_spells_group_dfb$be_n <- str_count(emp_spells_group_dfb$seq_list,"BE")

emp_spells_group_dfa$ne_n <- str_count(emp_spells_group_dfa$seq_list,"NE")
emp_spells_group_dfb$ne_n <- str_count(emp_spells_group_dfb$seq_list,"NE")

emp_spells_group_dfa$na_n <- str_count(emp_spells_group_dfa$seq_list,"NA")
emp_spells_group_dfb$na_n <- str_count(emp_spells_group_dfb$seq_list,"NA")

### code groupings as new variable 
emp_spells_group_dfa <- emp_spells_group_dfa %>% 
  mutate(emp_spells_group = ifelse(ue_n==4,"steady: unbroken",
                                     ifelse(ue_n==3&na_n==1, "steady: unbroken",
                                            ifelse(be_n==4,"steady: broken",
                                                   ifelse(be_n==3&na_n==1,"steady: broken",
                                                          ifelse(ne_n==4,"steady: no employment",
                                                                 ifelse(ne_n==3&na_n==1,"steady: no employment",
                                                                        ifelse(ue_n==3&be_n==1,"blip: broken",
                                                                               ifelse(ue_n==3&ne_n==1,"blip: no employment",
                                                                                      ifelse(na_n>1,"incomplete",
                                                                                             "churn"))))))))))

emp_spells_group_dfb <- emp_spells_group_dfb %>% 
  mutate(emp_spells_group = ifelse(ue_n==4,"steady: unbroken",
                                   ifelse(ue_n==3&na_n==1, "steady: unbroken",
                                          ifelse(be_n==4,"steady: broken",
                                                 ifelse(be_n==3&na_n==1,"steady: broken",
                                                        ifelse(ne_n==4,"steady: no employment",
                                                               ifelse(ne_n==3&na_n==1,"steady: no employment",
                                                                      ifelse(ue_n==3&be_n==1,"blip: broken",
                                                                             ifelse(ue_n==3&ne_n==1,"blip: no employment",
                                                                                    ifelse(na_n>1,"incomplete",
                                                                                           "churn"))))))))))

### create long versions of dfs
emp_spells_group_dfa_long <- emp_spells_group_dfa %>% 
  group_by(emp_spells_group) %>% 
  summarise(n_a=n()) %>% 
  ungroup() %>% 
  mutate(pc_a=n_a/sum(n_a)*100) %>% 
  arrange(factor(emp_spells_group, levels=c("steady: unbroken",
                                            "steady: broken",
                                            "steady: no employment",
                                            "churn",
                                            "blip: broken",
                                            "blip: no employment",
                                            "incomplete")))

emp_spells_group_dfb_long <- emp_spells_group_dfb %>% 
  group_by(emp_spells_group) %>% 
  summarise(n_b=n()) %>% 
  ungroup() %>% 
  mutate(pc_b=n_b/sum(n_b)*100) %>% 
  arrange(factor(emp_spells_group, levels=c("steady: unbroken",
                                            "steady: broken",
                                            "steady: no employment",
                                            "churn",
                                            "blip: broken",
                                            "blip: no employment",
                                            "incomplete")))

### create single df and save

emp_spells_group_df <- emp_spells_group_dfa_long %>% 
  left_join(emp_spells_group_dfb_long)

write_rds(emp_spells_group_df, "./working_data/emp_spells_group_df.rds")

#### ---------------------------------------------------------------------------
#### Multiple employment
#### ---------------------------------------------------------------------------

### create vars to calculate groupings
## create df and single sequence variable
multi_jobs_group_dfa <- data.frame(multi_jobs.seq.a) %>% mutate(seq_list = paste0(wv_3,"-",wv_4,"-",wv_5,"-",wv_6))
multi_jobs_group_dfb <- data.frame(multi_jobs.seq.b) %>% mutate(seq_list = paste0(wv_7,"-",wv_8,"-",wv_9,"-",wv_10))

## count number of occurrences of each state in sequence
multi_jobs_group_dfa$yes_n <- str_count(multi_jobs_group_dfa$seq_list,"YES")
multi_jobs_group_dfb$yes_n <- str_count(multi_jobs_group_dfb$seq_list,"YES")

multi_jobs_group_dfb$no_n <- str_count(multi_jobs_group_dfb$seq_list,"NO")
multi_jobs_group_dfa$no_n <- str_count(multi_jobs_group_dfa$seq_list,"NO")

multi_jobs_group_dfa$na_n <- str_count(multi_jobs_group_dfa$seq_list,"NA")
multi_jobs_group_dfb$na_n <- str_count(multi_jobs_group_dfb$seq_list,"NA")

### code groupings as new variable 
multi_jobs_group_dfa <- multi_jobs_group_dfa %>% 
  mutate(multi_jobs_group = ifelse(no_n==4,"steady: no multiple jobs",
                                     ifelse(no_n==3&na_n==1, "steady: no multiple jobs",
                                            ifelse(yes_n==4,"steady: multiple jobs",
                                                   ifelse(yes_n==3&na_n==1,"steady: multiple jobs",
                                                          ifelse(no_n==3&yes_n==1,"blip: multiple jobs",
                                                                               ifelse(na_n>1,"incomplete",
                                                                                             "churn")))))))

multi_jobs_group_dfb <- multi_jobs_group_dfb %>% 
  mutate(multi_jobs_group = ifelse(no_n==4,"steady: no multiple jobs",
                                   ifelse(no_n==3&na_n==1, "steady: no multiple jobs",
                                          ifelse(yes_n==4,"steady: multiple jobs",
                                                 ifelse(yes_n==3&na_n==1,"steady: multiple jobs",
                                                        ifelse(no_n==3&yes_n==1,"blip: multiple jobs",
                                                               ifelse(na_n>1,"incomplete",
                                                                      "churn")))))))

### create long versions of dfs
multi_jobs_group_dfa_long <- multi_jobs_group_dfa %>% 
  group_by(multi_jobs_group) %>% 
  summarise(n_a=n()) %>% 
  ungroup() %>% 
  mutate(pc_a=n_a/sum(n_a)*100) %>% 
  arrange(factor(multi_jobs_group, levels = c("steady: no multiple jobs",
                                              "steady: multiple jobs",
                                              "churn",
                                              "blip: multiple jobs",
                                              "incomplete")))

multi_jobs_group_dfb_long <- multi_jobs_group_dfb %>% 
  group_by(multi_jobs_group) %>% 
  summarise(n_b=n()) %>% 
  ungroup() %>% 
  mutate(pc_b=n_b/sum(n_b)*100) %>% 
  arrange(factor(multi_jobs_group, levels = c("steady: no multiple jobs",
                                              "steady: multiple jobs",
                                              "churn",
                                              "blip: multiple jobs",
                                              "incomplete")))

### create single df and save

multi_jobs_group_df <- multi_jobs_group_dfa_long %>% 
  left_join(multi_jobs_group_dfb_long)

write_rds(multi_jobs_group_df, "./working_data/multi_jobs_group_df.rds")

################################################################################
#####                         Latent class analysis                        #####
################################################################################

### use poLCA package for LCA
### use randomLCA for longitudinal/repeated measures LCA?

## recode missing values to NA
dfas1a_seq_wide <- dfas1a_seq_wide %>%  mutate(across(where(is.character), ~na_if(., "missing")))
dfas1b_seq_wide <- dfas1b_seq_wide %>%  mutate(across(where(is.character), ~na_if(., "missing")))


## convert variables to factors so that they have numeric values
dfas1a_seq_wide$wv_3 <- factor(dfas1a_seq_wide$wv_3, levels = c("fixed-term", "permanent", "unemployed/not in employment"))
dfas1a_seq_wide$wv_4 <- factor(dfas1a_seq_wide$wv_4, levels = c("fixed-term", "permanent", "unemployed/not in employment"))
dfas1a_seq_wide$wv_5 <- factor(dfas1a_seq_wide$wv_5, levels = c("fixed-term", "permanent", "unemployed/not in employment"))
dfas1a_seq_wide$wv_6 <- factor(dfas1a_seq_wide$wv_6, levels = c("fixed-term", "permanent", "unemployed/not in employment"))

dfas1b_seq_wide$wv_7 <- factor(dfas1b_seq_wide$wv_7, levels = c("fixed-term", "permanent", "unemployed/not in employment"))
dfas1b_seq_wide$wv_8 <- factor(dfas1b_seq_wide$wv_8, levels = c("fixed-term", "permanent", "unemployed/not in employment"))
dfas1b_seq_wide$wv_9 <- factor(dfas1b_seq_wide$wv_9, levels = c("fixed-term", "permanent", "unemployed/not in employment"))
dfas1b_seq_wide$wv_10 <- factor(dfas1b_seq_wide$wv_10, levels = c("fixed-term", "permanent", "unemployed/not in employment"))

# check levels
levels(dfas1a_seq_wide$wv_3)
levels(dfas1a_seq_wide$wv_4)
levels(dfas1a_seq_wide$wv_5)
levels(dfas1a_seq_wide$wv_6)

levels(dfas1b_seq_wide$wv_7)
levels(dfas1b_seq_wide$wv_8)
levels(dfas1b_seq_wide$wv_9)
levels(dfas1b_seq_wide$wv_10)

### start with basic formulation (no covariates) --------------------

f <- cbind(wv_3, wv_4,wv_5, wv_6) ~ 1
f2 <- cbind(wv_7, wv_8,wv_9, wv_10) ~ 1

### no covariates, 1 class --------------------
empcontract_lca1a <- poLCA(f, dfas1a_seq_wide, nclass = 1, maxiter = 4000,graphs = TRUE, na.rm = FALSE)

empcontract_lca1b <- poLCA(f2, dfas1b_seq_wide, nclass = 1, maxiter = 4000,graphs = TRUE, na.rm = FALSE)

# entropy -- NOTE: poLCA uses non-normalised entropy, how to interpret?
empcontract_lca1a_ent <- poLCA.entropy(empcontract_lca1a)

empcontract_lca1b_ent <- poLCA.entropy(empcontract_lca1b)

## bind the predicted class for each case back onto wide df
dfas1a_pred_class <-cbind(dfas1a_seq_wide, "pred_class1" = empcontract_lca1a$predclass)

dfas1b_pred_class <-cbind(dfas1b_seq_wide, "pred_class1" = empcontract_lca1b$predclass)


## create totals for class membership
empcontract_lca1a_memtotals <- dfas1a_pred_class %>% 
  group_by(pred_class1) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

empcontract_lca1b_memtotals <- dfas1b_pred_class %>% 
  group_by(pred_class1) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

### no covariates, 2 classes --------------------
empcontract_lca2a <- poLCA(f, dfas1a_seq_wide, nclass = 2, maxiter = 4000,graphs = TRUE, na.rm = FALSE)

empcontract_lca2b <- poLCA(f2, dfas1b_seq_wide, nclass = 2, maxiter = 4000,graphs = TRUE, na.rm = FALSE)

# entropy -- NOTE: poLCA uses non-normalised entropy, how to interpret?
empcontract_lca2a_ent <- poLCA.entropy(empcontract_lca2a)

empcontract_lca2b_ent <- poLCA.entropy(empcontract_lca2b)

## bind the predicted class for each case back onto wide df
dfas1a_pred_class <-cbind(dfas1a_seq_wide, "pred_class2" = empcontract_lca2a$predclass)

dfas1b_pred_class <-cbind(dfas1b_seq_wide, "pred_class2" = empcontract_lca2b$predclass)

## create totals for class membership
empcontract_lca2a_memtotals <- dfas1a_pred_class %>% 
  group_by(pred_class2) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

empcontract_lca2b_memtotals <- dfas1b_pred_class %>% 
  group_by(pred_class2) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

### no covariates, 3 classes --------------------
empcontract_lca3a <- poLCA(f, dfas1a_seq_wide, nclass = 3, maxiter = 4000, graphs = TRUE, na.rm = FALSE)

empcontract_lca3b <- poLCA(f2, dfas1b_seq_wide, nclass = 3, maxiter = 4000, graphs = TRUE, na.rm = FALSE)

# entropy -- NOTE: poLCA uses non-normalised entropy, how to interpret?
empcontract_lca3a_ent <- poLCA.entropy(empcontract_lca3a)

empcontract_lca3b_ent <- poLCA.entropy(empcontract_lca3b)

## bind the predicted class for each case back onto wide df
dfas1a_pred_class <-cbind(dfas1a_pred_class, "pred_class3" = empcontract_lca3a$predclass)

dfas1b_pred_class <-cbind(dfas1b_pred_class, "pred_class3" = empcontract_lca3b$predclass)

## create totals for class membership
empcontract_lca3a_memtotals <- dfas1a_pred_class %>% 
  group_by(pred_class3) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

empcontract_lca3b_memtotals <- dfas1b_pred_class %>% 
  group_by(pred_class3) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

### no covariates, 4 classes --------------------
empcontract_lca4a <- poLCA(f, dfas1a_seq_wide, nclass = 4, maxiter = 8000, graphs = TRUE, na.rm = FALSE)

empcontract_lca4b <- poLCA(f2, dfas1b_seq_wide, nclass = 4, maxiter = 8000, graphs = TRUE, na.rm = FALSE)

# entropy -- NOTE: poLCA uses non-normalised entropy, how to interpret?
empcontract_lca4a_ent <- poLCA.entropy(empcontract_lca4a)

empcontract_lca4b_ent <- poLCA.entropy(empcontract_lca4b)

## bind the predicted class for each case back onto wide df
dfas1a_pred_class <-cbind(dfas1a_pred_class, "pred_class4" = empcontract_lca4a$predclass)

dfas1b_pred_class <-cbind(dfas1b_pred_class, "pred_class4" = empcontract_lca4b$predclass)

## create totals for class membership
empcontract_lca4a_memtotals <- dfas1a_pred_class %>% 
  group_by(pred_class4) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

empcontract_lca4b_memtotals <- dfas1b_pred_class %>% 
  group_by(pred_class4) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))


### no covariates, 5 classes--------------------
empcontract_lca5a <- poLCA(f, dfas1a_seq_wide, nclass = 5, maxiter = 8000, graphs = TRUE, na.rm = FALSE)

empcontract_lca5b <- poLCA(f2, dfas1b_seq_wide, nclass = 5, maxiter = 8000, graphs = TRUE, na.rm = FALSE)

# entropy -- NOTE: poLCA uses non-normalised entropy, how to interpret?
empcontract_lca5a_ent <- poLCA.entropy(empcontract_lca5a)

empcontract_lca5b_ent <- poLCA.entropy(empcontract_lca5b)

## bind the predicted class for each case back onto wide df
dfas1a_pred_class <-cbind(dfas1a_pred_class, "pred_class5" = empcontract_lca5a$predclass)

dfas1b_pred_class <-cbind(dfas1b_pred_class, "pred_class5" = empcontract_lca5b$predclass)

## create totals for class membership
empcontract_lca5a_memtotals <- dfas1a_pred_class %>% 
  group_by(pred_class5) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

empcontract_lca5b_memtotals <- dfas1b_pred_class %>% 
  group_by(pred_class5) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

### no covariates, 6 classes --------------------
empcontract_lca6a <- poLCA(f, dfas1a_seq_wide, nclass = 6, maxiter = 8000, graphs = FALSE, na.rm = FALSE)

empcontract_lca6b <- poLCA(f2, dfas1b_seq_wide, nclass = 6, maxiter = 8000, graphs = FALSE, na.rm = FALSE)


# entropy -- NOTE: poLCA uses non-normalised entropy, how to interpret?
empcontract_lca6a_ent <- poLCA.entropy(empcontract_lca6a)

empcontract_lca6b_ent <- poLCA.entropy(empcontract_lca6b)

## bind the predicted class for each case back onto wide df
dfas1a_pred_class <-cbind(dfas1a_pred_class, "pred_class6" = empcontract_lca6a$predclass)

dfas1b_pred_class <-cbind(dfas1b_pred_class, "pred_class6" = empcontract_lca6b$predclass)

## create totals for class membership
empcontract_lca6a_memtotals <- dfas1a_pred_class %>% 
  group_by(pred_class6) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

empcontract_lca6b_memtotals <- dfas1b_pred_class %>% 
  group_by(pred_class6) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))


### no covariates, 7 classes --------------------
empcontract_lca7a <- poLCA(f, dfas1a_seq_wide, nclass = 7, maxiter = 8000, graphs = FALSE, na.rm = FALSE)#, nrep = 10)

empcontract_lca7b <- poLCA(f2, dfas1b_seq_wide, nclass = 7, maxiter = 8000, graphs = FALSE, na.rm = FALSE)#, nrep = 10)

# entropy -- NOTE: poLCA uses non-normalised entropy, how to interpret?
empcontract_lca7a_ent <- poLCA.entropy(empcontract_lca7a)

empcontract_lca7b_ent <- poLCA.entropy(empcontract_lca7b)

## bind the predicted class for each case back onto wide df
dfas1a_pred_class <-cbind(dfas1a_pred_class, "pred_class7" = empcontract_lca7a$predclass)

dfas1b_pred_class <-cbind(dfas1b_pred_class, "pred_class7" = empcontract_lca7b$predclass)

## create totals for class membership
empcontract_lca7a_memtotals <- dfas1a_pred_class %>% 
  group_by(pred_class7) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

empcontract_lca7b_memtotals <- dfas1b_pred_class %>% 
  group_by(pred_class7) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))


### no covariates, 8 classes --------------------
empcontract_lca8a <- poLCA(f, dfas1a_seq_wide, nclass = 8, maxiter = 8000, graphs = FALSE, na.rm = FALSE)#, nrep = 10)

empcontract_lca8b <- poLCA(f2, dfas1b_seq_wide, nclass = 8, maxiter = 8000, graphs = FALSE, na.rm = FALSE)#, nrep = 10)

# entropy -- NOTE: poLCA uses non-normalised entropy, how to interpret?
empcontract_lca8a_ent <- poLCA.entropy(empcontract_lca8a)

empcontract_lca8b_ent <- poLCA.entropy(empcontract_lca8b)

## bind the predicted class for each case back onto wide df
dfas1a_pred_class <-cbind(dfas1a_pred_class, "pred_class8" = empcontract_lca8a$predclass)

dfas1b_pred_class <-cbind(dfas1b_pred_class, "pred_class8" = empcontract_lca8b$predclass)

## create totals for class membership
empcontract_lca8a_memtotals <- dfas1a_pred_class %>% 
  group_by(pred_class8) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

empcontract_lca8b_memtotals <- dfas1b_pred_class %>% 
  group_by(pred_class8) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

### model fit stats --------------------

nclass_vector <- c(2:8)
bic_vector <- c(empcontract_lca2a$bic, empcontract_lca3a$bic, empcontract_lca4a$bic, 
                empcontract_lca5a$bic, empcontract_lca6a$bic, empcontract_lca7a$bic, 
                empcontract_lca8a$bic)
aic_vector <- c(empcontract_lca2a$aic, empcontract_lca3a$aic, empcontract_lca4a$aic, 
                empcontract_lca5a$aic, empcontract_lca6a$aic, empcontract_lca7a$aic, 
                empcontract_lca8a$aic)
Gsq_vector <- c(empcontract_lca2a$Gsq, empcontract_lca3a$Gsq, empcontract_lca4a$Gsq, 
                empcontract_lca5a$Gsq, empcontract_lca6a$Gsq, empcontract_lca7a$Gsq, 
                empcontract_lca8a$Gsq)
Chisq_vector <- c(empcontract_lca2a$Chisq, empcontract_lca3a$Chisq, 
                  empcontract_lca4a$Chisq, empcontract_lca5a$Chisq, 
                  empcontract_lca6a$Chisq, empcontract_lca7a$Chisq, 
                  empcontract_lca8a$Chisq)
entropy_vector <- c(empcontract_lca2a_ent,empcontract_lca3a_ent, 
                    empcontract_lca4a_ent, empcontract_lca5a_ent, 
                    empcontract_lca6a_ent, empcontract_lca7a_ent, 
                    empcontract_lca8a_ent)

bic_vector_b <- c(empcontract_lca2b$bic, empcontract_lca3b$bic, empcontract_lca4b$bic, 
                empcontract_lca5b$bic, empcontract_lca6b$bic, empcontract_lca7b$bic, 
                empcontract_lca8b$bic)
aic_vector_b <- c(empcontract_lca2b$aic, empcontract_lca3b$aic, empcontract_lca4b$aic, 
                empcontract_lca5b$aic, empcontract_lca6b$aic, empcontract_lca7b$aic, 
                empcontract_lca8b$aic)
Gsq_vector_b <- c(empcontract_lca2b$Gsq, empcontract_lca3b$Gsq, empcontract_lca4b$Gsq, 
                empcontract_lca5b$Gsq, empcontract_lca6b$Gsq, empcontract_lca7b$Gsq, 
                empcontract_lca8b$Gsq)
Chisq_vector_b <- c(empcontract_lca2b$Chisq, empcontract_lca3b$Chisq, 
                  empcontract_lca4b$Chisq, empcontract_lca5b$Chisq, 
                  empcontract_lca6b$Chisq, empcontract_lca7b$Chisq, 
                  empcontract_lca8b$Chisq)
entropy_vector_b <- c(empcontract_lca2b_ent,empcontract_lca3b_ent, 
                    empcontract_lca4b_ent, empcontract_lca5b_ent, 
                    empcontract_lca6b_ent, empcontract_lca7b_ent, 
                    empcontract_lca8b_ent)

## create df for model fit stats
empcontracta_lca_fit_stats <- data.frame(cbind(nclass_vector, bic_vector, aic_vector,Gsq_vector, Chisq_vector, entropy_vector))
names(empcontracta_lca_fit_stats) <- c("nclass", "bic", "aic", "Gsq", "Chisq", "entropy")

empcontractb_lca_fit_stats <- data.frame(cbind(nclass_vector, bic_vector_b, aic_vector_b,Gsq_vector_b, Chisq_vector_b, entropy_vector_b))
names(empcontractb_lca_fit_stats) <- c("nclass", "bic", "aic", "Gsq", "Chisq", "entropy")

# model with lowest bic
empcontracta_lca_fit_stats %>%  filter(bic==min(bic))

empcontractb_lca_fit_stats %>%  filter(bic==min(bic))

# bic elbow plot
tiff("./output/descriptive/empcontracta_lca_elbow.tiff", width = 400, height = 400)
empcontracta_lca_fit_stats %>% dplyr::select(nclass, bic) %>% 
  ggplot(aes(x=nclass,y=bic)) + 
  geom_line()
dev.off()

tiff("./output/descriptive/empcontractb_lca_elbow.tiff", width = 400, height = 400)
empcontractb_lca_fit_stats %>% dplyr::select(nclass, bic) %>% 
  ggplot(aes(x=nclass,y=bic)) + 
  geom_line()
dev.off()

# model with lowest aic
empcontracta_lca_fit_stats %>%  filter(aic==min(aic))

empcontractb_lca_fit_stats %>%  filter(aic==min(aic))

# aic elbow plot
empcontracta_lca_fit_stats %>% dplyr::select(nclass, aic) %>% 
  ggplot(aes(x=nclass,y=aic)) + 
  geom_line()

empcontractb_lca_fit_stats %>% dplyr::select(nclass, aic) %>% 
  ggplot(aes(x=nclass,y=aic)) + 
  geom_line()

## save fit_stats
write.csv(empcontracta_lca_fit_stats, "./output/descriptive/empcontracta_lca_fit_stats.csv")

write.csv(empcontractb_lca_fit_stats, "./output/descriptive/empcontractb_lca_fit_stats.csv")

#### Most plausible model ------------------------------------------------------

### 5 classes for both samples

emp_contracta_lca_final <- dfas1a_pred_class %>% 
  mutate(across(where(is.factor), as.character)) %>% 
  dplyr::select(pidp, wv_3,wv_4,wv_5,wv_6,pred_class5) %>% 
  mutate_all(~ ifelse(is.na(.),"missing",.))

emp_contractb_lca_final <- dfas1b_pred_class %>% 
  mutate(across(where(is.factor), as.character)) %>% 
  dplyr::select(pidp, wv_7,wv_8,wv_9,wv_10,pred_class5) %>% 
  mutate_all(~ ifelse(is.na(.),"missing",.))

## reorder the classes so there appear in a set order
probs.start<-empcontract_lca5a$probs.start
#new.probs.start <- poLCA.reorder(probs.start, c(5,1,2,4,3))

probs.start.b <-empcontract_lca5b$probs.start

#save output
emp_contracta_lca_final_model <- poLCA(f, dfas1a_seq_wide, nclass = 5, maxiter = 8000, probs.start=probs.start, 
               graphs = FALSE, na.rm = FALSE)
saveRDS(emp_contracta_lca_final_model, "./output/descriptive/emp_contracta_lca_final_model.rds")

emp_contractb_lca_final_model <- poLCA(f2, dfas1b_seq_wide, nclass = 5, maxiter = 8000, probs.start=probs.start.b, 
                                       graphs = FALSE, na.rm = FALSE)
saveRDS(emp_contractb_lca_final_model, "./output/descriptive/emp_contractb_lca_final_model.rds")

# save plot
tiff("./output/descriptive/emp_contracta_lca_final.tiff", width = 400, height = 400)
poLCA(f, dfas1a_seq_wide, nclass = 5, maxiter = 8000, probs.start=probs.start, 
      graphs = TRUE, na.rm = FALSE)
dev.off()

tiff("./output/descriptive/emp_contractb_lca_final.tiff", width = 400, height = 400)
poLCA(f2, dfas1b_seq_wide, nclass = 5, maxiter = 8000, probs.start=probs.start.b, 
      graphs = TRUE, na.rm = FALSE)
dev.off()

###### sequence analysis for 5 class solution

### create sequence data
emp_contract2.seq.a <- seqdef(emp_contracta_lca_final, 2:5, states = emp_contract_code,
                             labels = emp_contract_labs)


emp_contract2.seq.b <- seqdef(emp_contractb_lca_final, 2:5, states = emp_contract_code,
                             labels = emp_contract_labs)

## first 100 sequences
tiff("./output/descriptive/emp_contracta_grouped_seqiplot100.tiff")#, width = 960, height = 960)
seqiplot(emp_contract2.seq.a,
         idxs=1:100, # to add more lines
         with.legend = T, 
         main = "Index plot (100 first sequences)",
         group = emp_contracta_lca_final$pred_class5,
         border = NA)
dev.off()

tiff("./output/descriptive/emp_contractb_grouped_seqiplot100.tiff")#, width = 960, height = 960)
seqiplot(emp_contract2.seq.b,
         idxs=1:100, # to add more lines
         with.legend = T, 
         main = "Index plot (100 first sequences)",
         group = emp_contractb_lca_final$pred_class5,
         border = NA)
dev.off()

## all sequences
tiff("output/descriptive/emp_contracta_grouped_seqiplot.tiff")#, width = 960, height = 1920)
seqIplot(emp_contract2.seq.a,
         with.legend = T, 
         main = "Index plot (all sequences)",
         group = emp_contracta_lca_final$pred_class5,
         border = NA)
dev.off()

tiff("output/descriptive/emp_contractb_grouped_seqiplot.tiff")#, width = 960, height = 1920)
seqIplot(emp_contract2.seq.b,
         with.legend = T, 
         main = "Index plot (all sequences)",
         group = emp_contractb_lca_final$pred_class5,
         border = NA)
dev.off()

# sequence frequency plot (all common sequences)
tiff("./output/descriptive/emp_contracta_grouped_seqfplot.tiff")#, width = 960, height = 1920)
seqfplot(emp_contract2.seq.a, 
         idxs=1:900, # to add more lines
         with.legend = T, 
         group = emp_contracta_lca_final$pred_class5,
         border = NA, 
         main = "Sequence frequency plot")
dev.off()

tiff("output/descriptive/emp_contractb_grouped_seqfplot.tiff")#, width = 960, height = 1920)
seqfplot(emp_contract2.seq.b, 
         idxs=1:900, # to add more lines
         with.legend = T, 
         border = NA, 
         group = emp_contractb_lca_final$pred_class5,
         main = "Sequence frequency plot")
dev.off()

# state distribution plot
tiff("output/descriptive/emp_contracta_grouped_seqdplot.tiff")#, width = 960, height = 960)
seqdplot(emp_contract2.seq.a, 
         with.legend = T, 
         border = NA, 
         group = emp_contracta_lca_final$pred_class5,
         main = "State distribution plot")
dev.off()

tiff("output/descriptive/emp_contractb_grouped_seqdplot.tiff")#, width = 960, height = 960)
seqdplot(emp_contract2.seq.b, 
         with.legend = T, 
         border = NA, 
         group = emp_contractb_lca_final$pred_class5,
         main = "State distribution plot")
dev.off()

# legend
#tiff("output/descriptive/emp_contract_legend.tiff", width = 400, height = 250)
seqlegend(emp_contract2.seq.a, cex = 1.3)
seqlegend(emp_contract2.seq.b, cex = 1.3)
#dev.off()

################################################################################
#####                           outcome analysis                           #####
################################################################################

# create spine for pidp and LCA membership

# join back on to end point df

# calculate % for outcomes by LCA membership
