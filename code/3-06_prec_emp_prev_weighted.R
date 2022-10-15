################################################################################

# Persistent precarious employment and health - Understanding Society
# 3-06 - grouped sequence analysis - weighted
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

citation("TraMineR")

################################################################################
#####                         load and prepare data                        #####
################################################################################

#### end point data ------------------
dfas1a_end <- readRDS("./working_data/dfas1a_end.rds")
dfas1b_end <- readRDS("./working_data/dfas1b_end.rds")

#### latent class membership spines
### employment contract LCA spine
emp_contracta_5class_spine <- readRDS("./working_data/emp_contracta_5class_spine.rds")
emp_contractb_5class_spine <- readRDS("./working_data/emp_contractb_5class_spine.rds")

### employment spells LCA spine
# to be added

### multiple employment LCA spine
multi_empa_5class_spine <- readRDS("./working_data/multi_empa_5class_spine.rds")
multi_empb_5class_spine <- readRDS("./working_data/multi_empb_5class_spine.rds")

#### load weight spines ---------------------------------

weight_spine_a <- readRDS("./look_ups/weights_spine_a.rds") %>% 
  dplyr::select(pidp, strata, psu)

weight_spine_b <- readRDS("./look_ups/weights_spine_b.rds") %>% 
  dplyr::select(pidp, strata, psu)


#### join class and weight spines back on to end point df ------------

### employment contract
dfas1a_end_class1 <- dfas1a_end %>% 
  left_join(emp_contracta_5class_spine, by="pidp") %>% 
  left_join(weight_spine_a, by="pidp")
  
dfas1b_end_class1 <- dfas1b_end %>% 
  left_join(emp_contractb_5class_spine, by="pidp") %>% 
  left_join(weight_spine_b, by="pidp")

### broken employment spells
# to be added

### multiple employment
dfas1a_end_class3 <- dfas1a_end %>% 
  left_join(multi_empa_5class_spine, by="pidp") %>% 
  left_join(weight_spine_a, by="pidp")
  
dfas1b_end_class3 <- dfas1b_end %>% 
  left_join(multi_empb_5class_spine, by="pidp") %>% 
  left_join(weight_spine_b, by="pidp")

#### create weighted samples -----------------

### employment contract
svy_emp_contract_a <- svydesign(id=~psu, strata=~strata,
                                weights=~indinub_xw, data=dfas1a_end_class1)

svy_emp_contract_b <- svydesign(id=~psu, strata=~strata,
                                weights=~indinui_xw, data=dfas1b_end_class1)

### broken employment spells
# to be added

### multiple employment
svy_multi_emp_a <- svydesign(id=~psu, strata=~strata,
                                weights=~indinub_xw, data=dfas1a_end_class3)

svy_multi_emp_b <- svydesign(id=~psu, strata=~strata,
                                weights=~indinui_xw, data=dfas1b_end_class3)


################################################################################
#####                          employment contract                         #####
################################################################################



#### self-rated health ---------------------------------------------------------

### sample A ------------

## calculate proportions
srh1_a <- data.frame(svyby(~srh_bin, ~emp_contract_class,svy_emp_contract_a, svymean, na.rm=TRUE))

srh1_a %>% pivot_longer(2:5,names_to = "measure",values_to = "est")

### done to here - need separate col for se and get rid of junk text in measure strings

#srh1_a <- cbind(rownames(srh1_a),srh1_a, row.names=NULL) <<< repurpose
#srh1_a$`rownames(srh1_a)` <- str_replace(srh1_a$`rownames(srh1_a)`, "emp_contract_class","")
#srh1_a <- srh1_a %>% rename(measure = `rownames(srh1_a)`)
#names(srh1_a) <- tolower(names(srh1_a)) # change all col names to lower case

## calculate totals
emp_contract2_a <- data.frame(svytotal(~emp_contract_class, svy_emp_contract_a))
emp_contract2_a <- emp_contract2_a %>% dplyr::select(-SE)
emp_contract2_a <- cbind(rownames(emp_contract2_a),emp_contract2_a, row.names=NULL)
emp_contract2_a$`rownames(emp_contract2_a)` <- str_replace(emp_contract2_a$`rownames(emp_contract2_a)`, "emp_contract_class","")
emp_contract2_a <- emp_contract2_a %>% rename(measure = `rownames(emp_contract2_a)`)
emp_contract2_a$total <- as.integer(emp_contract2_a$total)

## join together and format
emp_contract_a <- emp_contract_a %>%
  left_join(emp_contract2_a) %>% 
  mutate(est = mean*100,
         var="Employment contract class",
         wv_n=6) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, measure, n, est, se) %>% 
  arrange(wv_n, factor(measure, levels = c("non-permanent employment",
                                           "permanent employment",
                                           "unemployed",
                                           "into employment",
                                           "out of employment")))

class_mem <- emp_contract_a


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
tiff("./output/weighted/empcontract_srh_prev_grouped_a.tiff")
srh_prev_a %>% 
  filter(srh_bin=="good/fair/poor") %>% 
  mutate(emp_contract_class=fct_reorder(emp_contract_class,a_srh_bin_pc)) %>% 
  ggplot(aes(x=emp_contract_class, y=a_srh_bin_pc)) +
  geom_col() +
  geom_errorbar(aes(ymin=a_srh_lci, ymax=a_srh_uci), colour="black", width=.1)+
  coord_flip() +
  theme_bw() 
dev.off()

tiff("./output/weighted/empcontract_srh_prev_grouped_b.tiff")
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

tiff("./output/weighted/empcontract_ghq_prev_grouped_a.tiff")
ghq_prev_a %>% 
  filter(ghq_case3=="3 or more") %>% 
  mutate(emp_contract_class=fct_reorder(emp_contract_class,a_ghq_case3_pc)) %>% 
  ggplot(aes(x=emp_contract_class, y=a_ghq_case3_pc)) +
  geom_col() +
  geom_errorbar(aes(ymin=a_ghq_lci, ymax=a_ghq_uci), colour="black", width=.1)+
  coord_flip() +
  theme_bw()
dev.off()

tiff("./output/weighted/empcontract_ghq_prev_grouped_b.tiff")
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
tiff("./output/weighted/multi_emp_srh_prev_grouped_a.tiff")
srh_prev_a %>% 
  filter(srh_bin=="good/fair/poor") %>% 
  mutate(multi_emp_class=fct_reorder(multi_emp_class,a_srh_bin_pc)) %>% 
  ggplot(aes(x=multi_emp_class, y=a_srh_bin_pc)) +
  geom_col() +
  geom_errorbar(aes(ymin=a_srh_lci, ymax=a_srh_uci), colour="black", width=.1)+
  coord_flip() +
  theme_bw() 
dev.off()

tiff("./output/weighted/multi_emp_srh_prev_grouped_b.tiff")
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

tiff("./output/weighted/multi_emp_ghq_prev_grouped_a.tiff")
ghq_prev_a %>% 
  filter(ghq_case3=="3 or more") %>% 
  mutate(multi_emp_class=fct_reorder(multi_emp_class,a_ghq_case3_pc)) %>% 
  ggplot(aes(x=multi_emp_class, y=a_ghq_case3_pc)) +
  geom_col() +
  geom_errorbar(aes(ymin=a_ghq_lci, ymax=a_ghq_uci), colour="black", width=.1)+
  coord_flip() +
  theme_bw()
dev.off()

tiff("./output/weighted/multi_emp_ghq_prev_grouped_b.tiff")
ghq_prev_b %>% 
  filter(ghq_case3=="3 or more") %>% 
  mutate(multi_emp_class=fct_reorder(multi_emp_class,b_ghq_case3_pc)) %>% 
  ggplot(aes(x=multi_emp_class, y=b_ghq_case3_pc)) +
  geom_col() +
  geom_errorbar(aes(ymin=b_ghq_lci, ymax=b_ghq_uci), colour="black", width=.1)+
  coord_flip() +
  theme_bw()
dev.off()

