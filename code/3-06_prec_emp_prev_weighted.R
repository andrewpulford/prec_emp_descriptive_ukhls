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

srh1_a_long <- srh1_a %>% pivot_longer(2:3,names_to = "measure",values_to = "est") %>% 
  rename("se" = "se.srh_binexcellent.very.good") %>% 
  dplyr::select(-se.srh_bingood.fair.poor)

## calculate totals
srh12_a <- data.frame(svyby(~srh_bin, ~emp_contract_class,svy_emp_contract_a, svytotal, na.rm=TRUE))

srh12_a_long <- srh12_a %>% pivot_longer(2:3,names_to = "measure",values_to = "total") %>% 
  dplyr::select(-c(se.srh_binexcellent.very.good, se.srh_bingood.fair.poor))

#join
srh1_a_long <- srh1_a_long %>% left_join(srh12_a_long)

### get rid of junk text in measure strings
srh1_a_long$emp_contract_class <- str_to_title(srh1_a_long$emp_contract_class)
srh1_a_long$measure <- str_replace(srh1_a_long$measure,"srh_bin","")
srh1_a_long$measure <- str_replace(srh1_a_long$measure,"excellent.very.good","Excellent/very good")
srh1_a_long$measure <- str_replace(srh1_a_long$measure,"good.fair.poor","Good/fair/poor")

## join together and format
srh_prev_a <- srh1_a_long %>%
  mutate(est = est*100,
         var="Self-reported health",
         wv_n=6) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, emp_contract_class,measure, n, est, se) %>% 
  arrange(wv_n, factor(measure, levels = c("Excellent/very good",
                                           "Good/fair/poor"))) %>% 
  arrange(emp_contract_class)

emp_contract_prev <- srh_prev_a

### sample B ------------

## calculate proportions
srh1_b <- data.frame(svyby(~srh_bin, ~emp_contract_class,svy_emp_contract_b, svymean, na.rm=TRUE))

srh1_b_long <- srh1_b %>% pivot_longer(2:3,names_to = "measure",values_to = "est") %>% 
  rename("se" = "se.srh_binexcellent.very.good") %>% 
  dplyr::select(-se.srh_bingood.fair.poor)

## calculate totals
srh12_b <- data.frame(svyby(~srh_bin, ~emp_contract_class,svy_emp_contract_b, svytotal, na.rm=TRUE))

srh12_b_long <- srh12_b %>% pivot_longer(2:3,names_to = "measure",values_to = "total") %>% 
  dplyr::select(-c(se.srh_binexcellent.very.good, se.srh_bingood.fair.poor))

#join
srh1_b_long <- srh1_b_long %>% left_join(srh12_b_long)

### get rid of junk text in measure strings
srh1_b_long$emp_contract_class <- str_to_title(srh1_b_long$emp_contract_class)
srh1_b_long$measure <- str_replace(srh1_b_long$measure,"srh_bin","")
srh1_b_long$measure <- str_replace(srh1_b_long$measure,"excellent.very.good","Excellent/very good")
srh1_b_long$measure <- str_replace(srh1_b_long$measure,"good.fair.poor","Good/fair/poor")




## join together and format
srh_prev_b <- srh1_b_long %>%
  mutate(est = est*100,
         var="Self-reported health",
         wv_n=10) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, emp_contract_class,measure, n, est, se) %>% 
  arrange(wv_n, factor(measure, levels = c("Excellent/very good",
                                           "Good/fair/poor"))) %>% 
  arrange(emp_contract_class)

emp_contract_prev <- emp_contract_prev %>% bind_rows(srh_prev_b)


#### GHQ-12 --------------------------------------------------------------------

### sample A ------------

## calculate proportions
ghq1_a <- data.frame(svyby(~ghq_case3, ~emp_contract_class,svy_emp_contract_a, svymean, na.rm=TRUE))

ghq1_a_long <- ghq1_a %>% pivot_longer(2:3,names_to = "measure",values_to = "est") %>% 
  rename("se" = "se.ghq_case30.2") %>% 
  dplyr::select(-se.ghq_case33.or.more)

## calculate totals
ghq12_a <- data.frame(svyby(~ghq_case3, ~emp_contract_class,svy_emp_contract_a, svytotal, na.rm=TRUE))

ghq12_a_long <- ghq12_a %>% pivot_longer(2:3,names_to = "measure",values_to = "total") %>% 
  dplyr::select(-c(se.ghq_case30.2, se.ghq_case33.or.more))

#join
ghq1_a_long <- ghq1_a_long %>% left_join(ghq12_a_long)

### get rid of junk text in measure strings
ghq1_a_long$emp_contract_class <- str_to_title(ghq1_a_long$emp_contract_class)
ghq1_a_long$measure <- str_replace(ghq1_a_long$measure,"ghq_case3","")
ghq1_a_long$measure <- str_replace(ghq1_a_long$measure,"0.2","0-2")
ghq1_a_long$measure <- str_replace(ghq1_a_long$measure,"3.or.more","3 or more")

## join together and format
ghq_prev_a <- ghq1_a_long %>%
  mutate(est = est*100,
         var="GHQ-12 caseness",
         wv_n=6) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, emp_contract_class,measure, n, est, se) %>% 
  arrange(wv_n, factor(measure, levels = c("0-2",
                                           "3 or more"))) %>% 
  arrange(emp_contract_class)

emp_contract_prev <- emp_contract_prev %>% bind_rows(ghq_prev_a)

### sample B ------------

## calculate proportions
ghq1_b <- data.frame(svyby(~ghq_case3, ~emp_contract_class,svy_emp_contract_b, svymean, na.rm=TRUE))

ghq1_b_long <- ghq1_b %>% pivot_longer(2:3,names_to = "measure",values_to = "est") %>% 
  rename("se" = "se.ghq_case30.2") %>% 
  dplyr::select(-se.ghq_case33.or.more)

## calculate totals
ghq12_b <- data.frame(svyby(~ghq_case3, ~emp_contract_class,svy_emp_contract_b, svytotal, na.rm=TRUE))

ghq12_b_long <- ghq12_b %>% pivot_longer(2:3,names_to = "measure",values_to = "total") %>% 
  dplyr::select(-c(se.ghq_case30.2, se.ghq_case33.or.more))

#join
ghq1_b_long <- ghq1_b_long %>% left_join(ghq12_b_long)

### get rid of junk text in measure strings
ghq1_b_long$emp_contract_class <- str_to_title(ghq1_b_long$emp_contract_class)
ghq1_b_long$measure <- str_replace(ghq1_b_long$measure,"ghq_case3","")
ghq1_b_long$measure <- str_replace(ghq1_b_long$measure,"0.2","0-2")
ghq1_b_long$measure <- str_replace(ghq1_b_long$measure,"3.or.more","3 or more")

## join together and format
ghq_prev_b <- ghq1_b_long %>%
  mutate(est = est*100,
         var="GHQ-12 caseness",
         wv_n=10) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, emp_contract_class,measure, n, est, se) %>% 
  arrange(wv_n, factor(measure, levels = c("0-2",
                                           "3 or more"))) %>% 
  arrange(emp_contract_class)

emp_contract_prev <- emp_contract_prev %>% bind_rows(ghq_prev_b)

#### add confidence intervals --------------------------------------------------
emp_contract_prev <- emp_contract_prev %>%
  group_by(var, emp_contract_class) %>% 
  mutate(d=sum(n),
         p=n/d,
         margin = qnorm(0.975)*sqrt(p*(1-p)/d)*100,
         lci = est-margin,
         uci = est+margin) %>% 
  ungroup() %>% 
  dplyr::select(-c(d,p,margin)) %>% 
  mutate(sample_grp = ifelse(wv_n==6, "A","B")) %>% 
  mutate(exp_flag = factor(ifelse(emp_contract_class=="Non-Permanent Employment",
                                  1,0)))

#### plots ---------------------------------------------------------------------

### binary self-reported health ----------------------------
tiff("./output/weighted/emp_contract_srh_prev_grouped.tiff")
emp_contract_prev %>% 
  filter(var=="Self-reported health" & measure=="Good/fair/poor") %>% 
  mutate(emp_contract_class=fct_reorder(emp_contract_class,est)) %>% 
  ggplot(aes(x=emp_contract_class, y=est,
             fill=exp_flag)) +
  geom_col(show.legend = FALSE) +
  geom_errorbar(aes(ymin=lci, ymax=uci), colour="black", width=.1)+
  coord_flip() +
  theme_bw() +
  scale_fill_manual(name = "exp_flag", values=c("grey50","red")) +
  facet_wrap(~sample_grp, ncol = 1)
dev.off()

### GHQ-12 caseness ------------------------------------------------------------

tiff("./output/weighted/emp_contract_ghq_prev_grouped.tiff")
emp_contract_prev %>% 
  filter(var=="GHQ-12 caseness" & measure=="3 or more") %>% 
  mutate(emp_contract_class=fct_reorder(emp_contract_class,est)) %>% 
  ggplot(aes(x=emp_contract_class, y=est,
             fill=exp_flag)) +
  geom_col(show.legend = FALSE) +
  geom_errorbar(aes(ymin=lci, ymax=uci), colour="black", width=.1)+
  coord_flip() +
  theme_bw() +
  scale_fill_manual(name = "exp_flag", values=c("grey50","red")) +
  facet_wrap(~sample_grp, ncol = 1)
dev.off()




################################################################################
#####                           employment spells                          #####
################################################################################

#### to be added ---------------------------------------------------------------

################################################################################
#####                          multiple employment                         #####
################################################################################

#### self-rated health ---------------------------------------------------------

### sample A ------------

## calculate proportions
srh1_a <- data.frame(svyby(~srh_bin, ~multi_emp_class,svy_multi_emp_a, svymean, na.rm=TRUE))

srh1_a_long <- srh1_a %>% pivot_longer(2:3,names_to = "measure",values_to = "est") %>% 
  rename("se" = "se.srh_binexcellent.very.good") %>% 
  dplyr::select(-se.srh_bingood.fair.poor)

## calculate totals
srh12_a <- data.frame(svyby(~srh_bin, ~multi_emp_class,svy_multi_emp_a, svytotal, na.rm=TRUE))

srh12_a_long <- srh12_a %>% pivot_longer(2:3,names_to = "measure",values_to = "total") %>% 
  dplyr::select(-c(se.srh_binexcellent.very.good, se.srh_bingood.fair.poor))

#join
srh1_a_long <- srh1_a_long %>% left_join(srh12_a_long)

### get rid of junk text in measure strings
srh1_a_long$multi_emp_class <- str_to_title(srh1_a_long$multi_emp_class)
srh1_a_long$measure <- str_replace(srh1_a_long$measure,"srh_bin","")
srh1_a_long$measure <- str_replace(srh1_a_long$measure,"excellent.very.good","Excellent/very good")
srh1_a_long$measure <- str_replace(srh1_a_long$measure,"good.fair.poor","Good/fair/poor")

## join together and format
srh_prev_a <- srh1_a_long %>%
  mutate(est = est*100,
         var="Self-reported health",
         wv_n=6) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, multi_emp_class,measure, n, est, se) %>% 
  arrange(wv_n, factor(measure, levels = c("Excellent/very good",
                                           "Good/fair/poor"))) %>% 
  arrange(multi_emp_class)

multi_emp_prev <- srh_prev_a

### sample B ------------

## calculate proportions
srh1_b <- data.frame(svyby(~srh_bin, ~multi_emp_class,svy_multi_emp_b, svymean, na.rm=TRUE))

srh1_b_long <- srh1_b %>% pivot_longer(2:3,names_to = "measure",values_to = "est") %>% 
  rename("se" = "se.srh_binexcellent.very.good") %>% 
  dplyr::select(-se.srh_bingood.fair.poor)

## calculate totals
srh12_b <- data.frame(svyby(~srh_bin, ~multi_emp_class,svy_multi_emp_b, svytotal, na.rm=TRUE))

srh12_b_long <- srh12_b %>% pivot_longer(2:3,names_to = "measure",values_to = "total") %>% 
  dplyr::select(-c(se.srh_binexcellent.very.good, se.srh_bingood.fair.poor))

#join
srh1_b_long <- srh1_b_long %>% left_join(srh12_b_long)

### get rid of junk text in measure strings
srh1_b_long$multi_emp_class <- str_to_title(srh1_b_long$multi_emp_class)
srh1_b_long$measure <- str_replace(srh1_b_long$measure,"srh_bin","")
srh1_b_long$measure <- str_replace(srh1_b_long$measure,"excellent.very.good","Excellent/very good")
srh1_b_long$measure <- str_replace(srh1_b_long$measure,"good.fair.poor","Good/fair/poor")




## join together and format
srh_prev_b <- srh1_b_long %>%
  mutate(est = est*100,
         var="Self-reported health",
         wv_n=10) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, multi_emp_class,measure, n, est, se) %>% 
  arrange(wv_n, factor(measure, levels = c("Excellent/very good",
                                           "Good/fair/poor"))) %>% 
  arrange(multi_emp_class)

multi_emp_prev <- multi_emp_prev %>% bind_rows(srh_prev_b)


#### GHQ-12 --------------------------------------------------------------------

### sample A ------------

## calculate proportions
ghq1_a <- data.frame(svyby(~ghq_case3, ~multi_emp_class,svy_multi_emp_a, svymean, na.rm=TRUE))

ghq1_a_long <- ghq1_a %>% pivot_longer(2:3,names_to = "measure",values_to = "est") %>% 
  rename("se" = "se.ghq_case30.2") %>% 
  dplyr::select(-se.ghq_case33.or.more)

## calculate totals
ghq12_a <- data.frame(svyby(~ghq_case3, ~multi_emp_class,svy_multi_emp_a, svytotal, na.rm=TRUE))

ghq12_a_long <- ghq12_a %>% pivot_longer(2:3,names_to = "measure",values_to = "total") %>% 
  dplyr::select(-c(se.ghq_case30.2, se.ghq_case33.or.more))

#join
ghq1_a_long <- ghq1_a_long %>% left_join(ghq12_a_long)

### get rid of junk text in measure strings
ghq1_a_long$multi_emp_class <- str_to_title(ghq1_a_long$multi_emp_class)
ghq1_a_long$measure <- str_replace(ghq1_a_long$measure,"ghq_case3","")
ghq1_a_long$measure <- str_replace(ghq1_a_long$measure,"0.2","0-2")
ghq1_a_long$measure <- str_replace(ghq1_a_long$measure,"3.or.more","3 or more")

## join together and format
ghq_prev_a <- ghq1_a_long %>%
  mutate(est = est*100,
         var="GHQ-12 caseness",
         wv_n=6) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, multi_emp_class,measure, n, est, se) %>% 
  arrange(wv_n, factor(measure, levels = c("0-2",
                                           "3 or more"))) %>% 
  arrange(multi_emp_class)

multi_emp_prev <- multi_emp_prev %>% bind_rows(ghq_prev_a)

### sample B ------------

## calculate proportions
ghq1_b <- data.frame(svyby(~ghq_case3, ~multi_emp_class,svy_multi_emp_b, svymean, na.rm=TRUE))

ghq1_b_long <- ghq1_b %>% pivot_longer(2:3,names_to = "measure",values_to = "est") %>% 
  rename("se" = "se.ghq_case30.2") %>% 
  dplyr::select(-se.ghq_case33.or.more)

## calculate totals
ghq12_b <- data.frame(svyby(~ghq_case3, ~multi_emp_class,svy_multi_emp_b, svytotal, na.rm=TRUE))

ghq12_b_long <- ghq12_b %>% pivot_longer(2:3,names_to = "measure",values_to = "total") %>% 
  dplyr::select(-c(se.ghq_case30.2, se.ghq_case33.or.more))

#join
ghq1_b_long <- ghq1_b_long %>% left_join(ghq12_b_long)

### get rid of junk text in measure strings
ghq1_b_long$multi_emp_class <- str_to_title(ghq1_b_long$multi_emp_class)
ghq1_b_long$measure <- str_replace(ghq1_b_long$measure,"ghq_case3","")
ghq1_b_long$measure <- str_replace(ghq1_b_long$measure,"0.2","0-2")
ghq1_b_long$measure <- str_replace(ghq1_b_long$measure,"3.or.more","3 or more")

## join together and format
ghq_prev_b <- ghq1_b_long %>%
  mutate(est = est*100,
         var="GHQ-12 caseness",
         wv_n=10) %>% 
  rename(n=total) %>% 
  dplyr::select(wv_n, var, multi_emp_class,measure, n, est, se) %>% 
  arrange(wv_n, factor(measure, levels = c("0-2",
                                           "3 or more"))) %>% 
  arrange(multi_emp_class)

multi_emp_prev <- multi_emp_prev %>% bind_rows(ghq_prev_b)

#### add confidence intervals --------------------------------------------------
multi_emp_prev <- multi_emp_prev %>%
  group_by(var, multi_emp_class) %>% 
  mutate(d=sum(n),
         p=n/d,
         margin = qnorm(0.975)*sqrt(p*(1-p)/d)*100,
         lci = est-margin,
         uci = est+margin) %>% 
  ungroup() %>% 
  dplyr::select(-c(d,p,margin)) %>% 
  mutate(sample_grp = ifelse(wv_n==6, "A","B")) %>% 
  mutate(exp_flag = factor(ifelse(multi_emp_class=="Multiple Employment",
                                  1,0)))

#### plots ---------------------------------------------------------------------

### binary self-reported health ----------------------------
tiff("./output/weighted/multi_emp_srh_prev_grouped.tiff")
multi_emp_prev %>% 
  filter(var=="Self-reported health" & measure=="Good/fair/poor") %>% 
  mutate(multi_emp_class=fct_reorder(multi_emp_class,est)) %>% 
  ggplot(aes(x=multi_emp_class, y=est,
             fill=exp_flag)) +
  geom_col(show.legend = FALSE) +
  geom_errorbar(aes(ymin=lci, ymax=uci), colour="black", width=.1)+
  coord_flip() +
  theme_bw() +
  scale_fill_manual(name = "exp_flag", values=c("grey50","red")) +
  facet_wrap(~sample_grp, ncol = 1)
dev.off()

### GHQ-12 caseness ------------------------------------------------------------

tiff("./output/weighted/multi_emp_ghq_prev_grouped.tiff")
multi_emp_prev %>% 
  filter(var=="GHQ-12 caseness" & measure=="3 or more") %>% 
  mutate(multi_emp_class=fct_reorder(multi_emp_class,est)) %>% 
  ggplot(aes(x=multi_emp_class, y=est,
             fill=exp_flag)) +
  geom_col(show.legend = FALSE) +
  geom_errorbar(aes(ymin=lci, ymax=uci), colour="black", width=.1)+
  coord_flip() +
  theme_bw() +
  scale_fill_manual(name = "exp_flag", values=c("grey50","red")) +
  facet_wrap(~sample_grp, ncol = 1)
dev.off()

