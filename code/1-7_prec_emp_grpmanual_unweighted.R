################################################################################

# Persistent precarious employment and health - Understanding Society
# 1-7 - manual grouping - unweighted
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

emp_contract.seq.a <- readRDS("./working_data/emp_contract.seq.a.rds")
emp_contract.seq.b <- readRDS("./working_data/emp_contract.seq.b.rds")

broken_emp.seq.a <- readRDS("./working_data/broken_emp.seq.a.rds")
broken_emp.seq.b <- readRDS("./working_data/broken_emp.seq.b.rds")

multi_jobs.seq.a <- readRDS("./working_data/multi_jobs.seq.a.rds")
multi_jobs.seq.b <- readRDS("./working_data/multi_jobs.seq.b.rds")


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

