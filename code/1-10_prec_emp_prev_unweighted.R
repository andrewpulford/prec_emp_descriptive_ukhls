################################################################################

# Persistent precarious employment and health - Understanding Society
# 1-9 - grouped sequence analysis - unweighted
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

### end point data
dfas1a_end <- readRDS("./working_data/dfas1a_end.rds")
dfas1b_end <- readRDS("./working_data/dfas1b_end.rds")

### employment contract LCA spine
emp_contracta_5class_spine <- readRDS("./working_data/emp_contracta_5class_spine.rds")
emp_contractb_5class_spine <- readRDS("./working_data/emp_contractb_5class_spine.rds")

### employment spells LCA spine


### multiple employment LCA spine
multi_empa_5class_spine <- readRDS("./working_data/multi_empa_5class_spine.rds")
multi_empb_5class_spine <- readRDS("./working_data/multi_empb_5class_spine.rds")


################################################################################
#####                          employment contract                         #####
################################################################################


### join class spine back on to end point df

dfas1a_end_class1 <- dfas1a_end %>% left_join(emp_contracta_5class_spine, by="pidp")
dfas1b_end_class1 <- dfas1b_end %>% left_join(emp_contractb_5class_spine, by="pidp")

#### calculate % for outcomes by LCA membership


### self-rated health
srh_prev_a <- dfas1a_end_class1 %>% group_by(emp_contract_class, srh_bin) %>% 
  summarise(a_srh_bin_n=n()) %>% 
  ungroup() %>% 
  group_by(emp_contract_class) %>% 
  mutate(d=sum(a_srh_bin_n),
         p=a_srh_bin_n/d,
         a_srh_bin_pc=p*100,
         margin = qnorm(0.975)*sqrt(p*(1-p)/d)*100,
         a_srh_lci = a_srh_bin_pc-margin,
         a_srh_uci = a_srh_bin_pc+margin) %>% 
  ungroup() %>% 
  dplyr::select(-c(d,p,margin))


srh_prev_b <- dfas1b_end_class1 %>% group_by(emp_contract_class, srh_bin) %>% 
  summarise(b_srh_bin_n=n()) %>% 
  ungroup() %>% 
  group_by(emp_contract_class) %>% 
  mutate(d=sum(b_srh_bin_n),
         p=b_srh_bin_n/d,
         b_srh_bin_pc=p*100,
         margin = qnorm(0.975)*sqrt(p*(1-p)/d)*100,
         b_srh_lci = b_srh_bin_pc-margin,
         b_srh_uci = b_srh_bin_pc+margin) %>% 
  ungroup() %>% 
  dplyr::select(-c(d,p,margin))

srh_combined <- srh_prev_a %>% left_join(srh_prev_b, by=c("emp_contract_class",
                                                          "srh_bin"))

## plots
tiff("./output/descriptive/empcontract_srh_prev_grouped_a.tiff")
srh_prev_a %>% 
  filter(srh_bin=="good/fair/poor") %>% 
  mutate(emp_contract_class=fct_reorder(emp_contract_class,a_srh_bin_pc)) %>% 
  ggplot(aes(x=emp_contract_class, y=a_srh_bin_pc)) +
  geom_col() +
  geom_errorbar(aes(ymin=a_srh_lci, ymax=a_srh_uci), colour="black", width=.1)+
  coord_flip() +
  theme_bw() 
dev.off()

tiff("./output/descriptive/empcontract_srh_prev_grouped_b.tiff")
srh_prev_b %>% 
  filter(srh_bin=="good/fair/poor") %>% 
  mutate(emp_contract_class=fct_reorder(emp_contract_class,b_srh_bin_pc)) %>% 
  ggplot(aes(x=emp_contract_class, y=b_srh_bin_pc)) +
  geom_col() +
  geom_errorbar(aes(ymin=b_srh_lci, ymax=b_srh_uci), colour="black", width=.1)+
  coord_flip() +
  theme_bw() 
dev.off()


### mental health symptoms
ghq_prev_a <- dfas1a_end_class1 %>% group_by(emp_contract_class, ghq_case3) %>% 
  summarise(a_ghq_case3_n=n()) %>% 
  ungroup() %>% 
  group_by(emp_contract_class) %>% 
  mutate(d=sum(a_ghq_case3_n),
         p=a_ghq_case3_n/d,
         a_ghq_case3_pc=p*100,
         margin = qnorm(0.975)*sqrt(p*(1-p)/d)*100,
         a_ghq_lci = a_ghq_case3_pc-margin,
         a_ghq_uci = a_ghq_case3_pc+margin) %>% 
  ungroup() %>% 
  dplyr::select(-c(d,p,margin))



ghq_prev_b <- dfas1b_end_class1 %>% group_by(emp_contract_class, ghq_case3) %>% 
  summarise(b_ghq_case3_n=n()) %>% 
  ungroup() %>% 
  group_by(emp_contract_class) %>% 
  mutate(d=sum(b_ghq_case3_n),
         p=b_ghq_case3_n/d,
         b_ghq_case3_pc=p*100,
         margin = qnorm(0.975)*sqrt(p*(1-p)/d)*100,
         b_ghq_lci = b_ghq_case3_pc-margin,
         b_ghq_uci = b_ghq_case3_pc+margin) %>% 
  ungroup() %>% 
  dplyr::select(-c(d,p,margin))


ghq_combined <- ghq_prev_a %>% left_join(ghq_prev_b)

## plots

tiff("./output/descriptive/empcontract_ghq_prev_grouped_a.tiff")
ghq_prev_a %>% 
  filter(ghq_case3=="3 or more") %>% 
  mutate(emp_contract_class=fct_reorder(emp_contract_class,a_ghq_case3_pc)) %>% 
  ggplot(aes(x=emp_contract_class, y=a_ghq_case3_pc)) +
  geom_col() +
  geom_errorbar(aes(ymin=a_ghq_lci, ymax=a_ghq_uci), colour="black", width=.1)+
  coord_flip() +
  theme_bw()
dev.off()

tiff("./output/descriptive/empcontract_ghq_prev_grouped_b.tiff")
ghq_prev_b %>% 
  filter(ghq_case3=="3 or more") %>% 
  mutate(emp_contract_class=fct_reorder(emp_contract_class,b_ghq_case3_pc)) %>% 
  ggplot(aes(x=emp_contract_class, y=b_ghq_case3_pc)) +
  geom_col() +
  geom_errorbar(aes(ymin=b_ghq_lci, ymax=b_ghq_uci), colour="black", width=.1)+
  coord_flip() +
  theme_bw()
dev.off()

################################################################################
#####                           employment spells                          #####
################################################################################

#### to be added ---------------------------------------------------------------

################################################################################
#####                          multiple employment                         #####
################################################################################

### join class spine back on to end point df

dfas1a_end_class3 <- dfas1a_end %>% left_join(multi_empa_5class_spine, by="pidp")
dfas1b_end_class3 <- dfas1b_end %>% left_join(multi_empb_5class_spine, by="pidp")

#### calculate % for outcomes by LCA membership


### self-rated health
srh_prev_a <- dfas1a_end_class3 %>% group_by(multi_emp_class, srh_bin) %>% 
  summarise(a_srh_bin_n=n()) %>% 
  ungroup() %>% 
  group_by(multi_emp_class) %>% 
  mutate(d=sum(a_srh_bin_n),
         p=a_srh_bin_n/d,
         a_srh_bin_pc=p*100,
         margin = qnorm(0.975)*sqrt(p*(1-p)/d)*100,
         a_srh_lci = a_srh_bin_pc-margin,
         a_srh_uci = a_srh_bin_pc+margin) %>% 
  ungroup() %>% 
  dplyr::select(-c(d,p,margin))


srh_prev_b <- dfas1b_end_class3 %>% group_by(multi_emp_class, srh_bin) %>% 
  summarise(b_srh_bin_n=n()) %>% 
  ungroup() %>% 
  group_by(multi_emp_class) %>% 
  mutate(d=sum(b_srh_bin_n),
         p=b_srh_bin_n/d,
         b_srh_bin_pc=p*100,
         margin = qnorm(0.975)*sqrt(p*(1-p)/d)*100,
         b_srh_lci = b_srh_bin_pc-margin,
         b_srh_uci = b_srh_bin_pc+margin) %>% 
  ungroup() %>% 
  dplyr::select(-c(d,p,margin))

srh_combined <- srh_prev_a %>% left_join(srh_prev_b, by=c("multi_emp_class",
                                                          "srh_bin"))

## plots
tiff("./output/descriptive/multi_emp_srh_prev_grouped_a.tiff")
srh_prev_a %>% 
  filter(srh_bin=="good/fair/poor") %>% 
  mutate(multi_emp_class=fct_reorder(multi_emp_class,a_srh_bin_pc)) %>% 
  ggplot(aes(x=multi_emp_class, y=a_srh_bin_pc)) +
  geom_col() +
  geom_errorbar(aes(ymin=a_srh_lci, ymax=a_srh_uci), colour="black", width=.1)+
  coord_flip() +
  theme_bw() 
dev.off()

tiff("./output/descriptive/multi_emp_srh_prev_grouped_b.tiff")
srh_prev_b %>% 
  filter(srh_bin=="good/fair/poor") %>% 
  mutate(multi_emp_class=fct_reorder(multi_emp_class,b_srh_bin_pc)) %>% 
  ggplot(aes(x=multi_emp_class, y=b_srh_bin_pc)) +
  geom_col() +
  geom_errorbar(aes(ymin=b_srh_lci, ymax=b_srh_uci), colour="black", width=.1)+
  coord_flip() +
  theme_bw() 
dev.off()


### mental health symptoms
ghq_prev_a <- dfas1a_end_class3 %>% group_by(multi_emp_class, ghq_case3) %>% 
  summarise(a_ghq_case3_n=n()) %>% 
  ungroup() %>% 
  group_by(multi_emp_class) %>% 
  mutate(d=sum(a_ghq_case3_n),
         p=a_ghq_case3_n/d,
         a_ghq_case3_pc=p*100,
         margin = qnorm(0.975)*sqrt(p*(1-p)/d)*100,
         a_ghq_lci = a_ghq_case3_pc-margin,
         a_ghq_uci = a_ghq_case3_pc+margin) %>% 
  ungroup() %>% 
  dplyr::select(-c(d,p,margin))



ghq_prev_b <- dfas1b_end_class3 %>% group_by(multi_emp_class, ghq_case3) %>% 
  summarise(b_ghq_case3_n=n()) %>% 
  ungroup() %>% 
  group_by(multi_emp_class) %>% 
  mutate(d=sum(b_ghq_case3_n),
         p=b_ghq_case3_n/d,
         b_ghq_case3_pc=p*100,
         margin = qnorm(0.975)*sqrt(p*(1-p)/d)*100,
         b_ghq_lci = b_ghq_case3_pc-margin,
         b_ghq_uci = b_ghq_case3_pc+margin) %>% 
  ungroup() %>% 
  dplyr::select(-c(d,p,margin))


ghq_combined <- ghq_prev_a %>% left_join(ghq_prev_b)

## plots

tiff("./output/descriptive/multi_emp_ghq_prev_grouped_a.tiff")
ghq_prev_a %>% 
  filter(ghq_case3=="3 or more") %>% 
  mutate(multi_emp_class=fct_reorder(multi_emp_class,a_ghq_case3_pc)) %>% 
  ggplot(aes(x=multi_emp_class, y=a_ghq_case3_pc)) +
  geom_col() +
  geom_errorbar(aes(ymin=a_ghq_lci, ymax=a_ghq_uci), colour="black", width=.1)+
  coord_flip() +
  theme_bw()
dev.off()

tiff("./output/descriptive/multi_emp_ghq_prev_grouped_b.tiff")
ghq_prev_b %>% 
  filter(ghq_case3=="3 or more") %>% 
  mutate(multi_emp_class=fct_reorder(multi_emp_class,b_ghq_case3_pc)) %>% 
  ggplot(aes(x=multi_emp_class, y=b_ghq_case3_pc)) +
  geom_col() +
  geom_errorbar(aes(ymin=b_ghq_lci, ymax=b_ghq_uci), colour="black", width=.1)+
  coord_flip() +
  theme_bw()
dev.off()

