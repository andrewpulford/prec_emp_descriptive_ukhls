################################################################################

# Persistent precarious employment and health - Understanding Society
# 3-07 - grouped prevalence - weighted
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

#citation("TraMineR")

################################################################################
#####                         load and prepare data                        #####
################################################################################

#### load European standard population ------------------
# https://www.opendata.nhs.scot/dataset/4dd86111-7326-48c4-8763-8cc4aa190c3e/resource/29ce4cda-a831-40f4-af24-636196e05c1a/download/european_standard_population_by_sex.csv

esp <- read.csv("./look_ups/european_standard_population_by_sex.csv")

### tidy up and make consistent with analytic data
esp <- esp %>% 
  mutate(AgeGroup = str_remove(AgeGroup," years"),
  Sex = tolower(Sex)) %>% 
  rename(age_dv_grp = AgeGroup,
         sex_dv = Sex,
         euro_std_pop = EuropeanStandardPopulation) %>% 
  filter(age_dv_grp %in% c("20-24",  "25-29",  "30-34",  "35-39",  "40-44",  
                           "45-49",  "50-54",  "55-59",  "60-64"))

esp$age_dv_grp <- factor(esp$age_dv_grp)
esp$age_dv_grp <- ordered(esp$age_dv_grp, 
                          levels = c("20-24",  "25-29",  "30-34",  "35-39",  
                                     "40-44",  "45-49",  "50-54",  "55-59",  
                                     "60-64"))



#### end point data ------------------
### sample A ---------
dfas1a_end <- readRDS("./working_data/dfas1a_end.rds")

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

#### latent class membership spines
### employment contract LCA spine
emp_contracta_5class_spine <- readRDS("./working_data/emp_contracta_5class_spine.rds")
emp_contractb_5class_spine <- readRDS("./working_data/emp_contractb_5class_spine.rds")

### employment spells LCA spine
emp_spellsa_3class_spine <- readRDS("./working_data/emp_spellsa_3class_spine.rds")
emp_spellsb_3class_spine <- readRDS("./working_data/emp_spellsb_3class_spine.rds")

### multiple employment LCA spine
multi_empa_5class_spine <- readRDS("./working_data/multi_empa_5class_spine.rds")
multi_empb_5class_spine <- readRDS("./working_data/multi_empb_5class_spine.rds")

#### create five year age groups from 20-24 to 60-64 ---------------------------

### load the PHS age group function
source("./functions/age_groups.R")

### sample A
dfas1a_end$age_dv_grp <- create_age_groups(dfas1a_end$age_dv, 
                                           from = 20, to = 64, by = 5,
                                           as_factor = TRUE)

# correct label for 60-64 level
levels(dfas1a_end$age_dv_grp)[levels(dfas1a_end$age_dv_grp)=='60+'] <- '60-64'

# add on the European standard population for each age group
dfas1a_end <- dfas1a_end %>% 
  left_join(esp, by = c("age_dv_grp", "sex_dv"))

### Sample B
dfas1b_end$age_dv_grp <- create_age_groups(dfas1b_end$age_dv, 
                                           from = 20, to = 64, by = 5,
                                           as_factor = TRUE)

# correct label for 60-64 level
levels(dfas1b_end$age_dv_grp)[levels(dfas1b_end$age_dv_grp)=='60+'] <- '60-64'


##### VVVVVVV DO THIS LATER VVVVVVV
# add on the European standard population for each age group
#dfas1b_end <- dfas1b_end %>% 
#  left_join(esp, by = c("age_dv_grp", "sex_dv"))

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
dfas1a_end_class2 <- dfas1a_end %>% 
  left_join(emp_spellsa_3class_spine, by="pidp") %>% 
  left_join(weight_spine_a, by="pidp")

dfas1b_end_class2 <- dfas1b_end %>% 
  left_join(emp_spellsb_3class_spine, by="pidp") %>% 
  left_join(weight_spine_b, by="pidp")

### multiple employment
dfas1a_end_class3 <- dfas1a_end %>% 
  left_join(multi_empa_5class_spine, by="pidp") %>% 
  left_join(weight_spine_a, by="pidp")
  
dfas1b_end_class3 <- dfas1b_end %>% 
  left_join(multi_empb_5class_spine, by="pidp") %>% 
  left_join(weight_spine_b, by="pidp")


################################################################################
#####                          age standardisation                         #####
################################################################################

## 
ghq_df <- dfas1a_end_class1 %>% 
  dplyr::select(pidp, indinub_xw, psu, strata,
                emp_contract_class, age_dv_grp, 
                sex_dv, ghq_case3) %>% 
  mutate(emp_contract_class = factor(emp_contract_class)) 


## create a list for each exposure class
ghq_splits <- split(ghq_df, ghq_df$emp_contract_class)



#### create denominator variable (number of individuals in age-sex group) ------

# these are just tests to check code before converting into functions
test <- svydesign(id=~psu, strata=~strata,
          weights=~indinub_xw, data=ghq_splits[[1]])

test2 <- data.frame(svyby(~sex_dv, ~age_dv_grp,test, svytotal, na.rm=TRUE))

names(test2) <- str_remove(names(test2), "sex_dv")
  

### define function
svydesign_function <- function(x){
  svydesign(id=~psu, strata=~strata,
            weights=~indinub_xw, data=x)
  }

### call function to create weighted dataframes for each exposure class
svy_ghq_splits <- lapply(ghq_splits, svydesign_function)

### define function
svytotal_function <- function(x){
  data.frame(svyby(~sex_dv, ~age_dv_grp, x, svytotal, na.rm=TRUE))

}

### call function to create weighted denominators for each exposure class
svy_ghq_splits_d <- lapply(svy_ghq_splits, svytotal_function) 

# drop SE columns
svy_ghq_splits_d <- map(svy_ghq_splits_d, ~ (.x %>% dplyr::select(-c(4:5))))

#  fix row names
col_vec <- c("age_dv_grp","male","female")
svy_ghq_splits_d <- lapply(svy_ghq_splits_d, setNames, col_vec)

#### create numerator variable (number of cases in age-sex group) ------

data.frame(svyby(~sex_dv, ~age_dv_grp, ~ghq_case3, svy_ghq_splits, svytotal, na.rm=TRUE))


################################################################################
#####                           weighted samples                           #####
################################################################################

#### create weighted samples -----------------

### employment contract
svy_emp_contract_a <- svydesign(id=~psu, strata=~strata,
                                weights=~indinub_xw, data=dfas1a_end_class1)

svy_emp_contract_b <- svydesign(id=~psu, strata=~strata,
                                weights=~indinui_xw, data=dfas1b_end_class1)

### broken employment spells
svy_broken_emp_a <- svydesign(id=~psu, strata=~strata,
                                weights=~indinub_xw, data=dfas1a_end_class2)

svy_broken_emp_b <- svydesign(id=~psu, strata=~strata,
                                weights=~indinui_xw, data=dfas1b_end_class2)

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
  group_by(var, emp_contract_class) %>% 
  mutate(d=sum(n),
         p=n/d,
         margin = qnorm(0.975)*sqrt(p*(1-p)/d)*100,
         lci = est-margin,
         uci = est+margin) %>% 
  ungroup() %>% 
  dplyr::select(-c(d,p,margin)) %>% 
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
  group_by(var, emp_contract_class) %>% 
  mutate(d=sum(n),
         p=n/d,
         margin = qnorm(0.975)*sqrt(p*(1-p)/d)*100,
         lci = est-margin,
         uci = est+margin) %>% 
  ungroup() %>% 
  dplyr::select(-c(d,p,margin)) %>% 
  arrange(emp_contract_class)

emp_contract_prev <- emp_contract_prev %>% bind_rows(srh_prev_b)

#### SF-12 PCS -----------------------------------------------------------------

### sample A ------------

sf12pcs_a <- data.frame(svyby(~sf12pcs_dv, ~emp_contract_class,svy_emp_contract_a, svymean, na.rm=TRUE))

sf12pcs_a <- sf12pcs_a %>% 
  rename("est"="sf12pcs_dv") %>% 
  mutate(wv_n= 6,
         var = "SF-12 physical component",
         measure = "Mean",
         n=NA)

emp_contract_prev <- emp_contract_prev %>% bind_rows(sf12pcs_a)

### sample B ------------

sf12pcs_b <- data.frame(svyby(~sf12pcs_dv, ~emp_contract_class,svy_emp_contract_b, svymean, na.rm=TRUE))

sf12pcs_b <- sf12pcs_b %>% 
  rename("est"="sf12pcs_dv") %>% 
  mutate(wv_n= 10,
         var = "SF-12 physical component",
         measure = "Mean",
         n=NA)

emp_contract_prev <- emp_contract_prev %>% bind_rows(sf12pcs_b)

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
  group_by(var, emp_contract_class) %>% 
  mutate(d=sum(n),
         p=n/d,
         margin = qnorm(0.975)*sqrt(p*(1-p)/d)*100,
         lci = est-margin,
         uci = est+margin) %>% 
  ungroup() %>% 
  dplyr::select(-c(d,p,margin)) %>% 
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
  group_by(var, emp_contract_class) %>% 
  mutate(d=sum(n),
         p=n/d,
         margin = qnorm(0.975)*sqrt(p*(1-p)/d)*100,
         lci = est-margin,
         uci = est+margin) %>% 
  ungroup() %>% 
  dplyr::select(-c(d,p,margin)) %>% 
  arrange(emp_contract_class)

emp_contract_prev <- emp_contract_prev %>% bind_rows(ghq_prev_b)

#### SF-12 MCS -----------------------------------------------------------------

### sample A ------------

sf12mcs_a <- data.frame(svyby(~sf12mcs_dv, ~emp_contract_class,svy_emp_contract_a, svymean, na.rm=TRUE))

sf12mcs_a <- sf12mcs_a %>% 
  rename("est"="sf12mcs_dv") %>% 
  mutate(wv_n= 6,
         var = "SF-12 mental component",
         measure = "Mean",
         n=NA)

emp_contract_prev <- emp_contract_prev %>% bind_rows(sf12mcs_a)

### sample B ------------

sf12mcs_b <- data.frame(svyby(~sf12mcs_dv, ~emp_contract_class,svy_emp_contract_b, svymean, na.rm=TRUE))

sf12mcs_b <- sf12mcs_b %>% 
  rename("est"="sf12mcs_dv") %>% 
  mutate(wv_n= 10,
         var = "SF-12 mental component",
         measure = "Mean",
         n=NA)

emp_contract_prev <- emp_contract_prev %>% bind_rows(sf12mcs_b)




#### add confidence intervals --------------------------------------------------

emp_contract_prev$emp_contract_class <- str_to_sentence(emp_contract_prev$emp_contract_class)


emp_contract_prev <- emp_contract_prev %>% 
  mutate(sample_grp = ifelse(wv_n==6, "A","B")) %>% 
  mutate(exp_flag = factor(ifelse(emp_contract_class=="Non-permanent employment",
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

### SF-12 PCS ----------------------------
tiff("./output/weighted/emp_contract_sf12-pcs_prev_grouped.tiff")
emp_contract_prev %>% 
  filter(var=="SF-12 physical component") %>% 
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

### SF-12 MCS ----------------------------
tiff("./output/weighted/emp_contract_sf12-mcs_prev_grouped.tiff")
emp_contract_prev %>% 
  filter(var=="SF-12 mental component") %>% 
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

#### SF-12 PCS -----------------------------------------------------------------

### sample A ------------

sf12pcs_a <- data.frame(svyby(~sf12pcs_dv, ~multi_emp_class,svy_multi_emp_a, svymean, na.rm=TRUE))

sf12pcs_a <- sf12pcs_a %>% 
  rename("est"="sf12pcs_dv") %>% 
  mutate(wv_n= 6,
         var = "SF-12 physical component",
         measure = "Mean",
         n=NA)

multi_emp_prev <- multi_emp_prev %>% bind_rows(sf12pcs_a)

### sample B ------------

sf12pcs_b <- data.frame(svyby(~sf12pcs_dv, ~multi_emp_class,svy_multi_emp_b, svymean, na.rm=TRUE))

sf12pcs_b <- sf12pcs_b %>% 
  rename("est"="sf12pcs_dv") %>% 
  mutate(wv_n= 10,
         var = "SF-12 physical component",
         measure = "Mean",
         n=NA)

multi_emp_prev <- multi_emp_prev %>% bind_rows(sf12pcs_b)

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

#### SF-12 MCS -----------------------------------------------------------------

### sample A ------------

sf12mcs_a <- data.frame(svyby(~sf12mcs_dv, ~multi_emp_class,svy_multi_emp_a, svymean, na.rm=TRUE))

sf12mcs_a <- sf12mcs_a %>% 
  rename("est"="sf12mcs_dv") %>% 
  mutate(wv_n= 6,
         var = "SF-12 mental component",
         measure = "Mean",
         n=NA)

multi_emp_prev <- multi_emp_prev %>% bind_rows(sf12mcs_a)

### sample B ------------

sf12mcs_b <- data.frame(svyby(~sf12mcs_dv, ~multi_emp_class,svy_multi_emp_b, svymean, na.rm=TRUE))

sf12mcs_b <- sf12mcs_b %>% 
  rename("est"="sf12mcs_dv") %>% 
  mutate(wv_n= 10,
         var = "SF-12 mental component",
         measure = "Mean",
         n=NA)

multi_emp_prev <- multi_emp_prev %>% bind_rows(sf12mcs_b)



#### add confidence intervals --------------------------------------------------
multi_emp_prev$multi_emp_class <- str_to_sentence(multi_emp_prev$multi_emp_class)

multi_emp_prev <- multi_emp_prev %>% 
  mutate(sample_grp = ifelse(wv_n==6, "A","B")) %>% 
  mutate(exp_flag = factor(ifelse(multi_emp_class=="Multiple employment",
                                  1,0)))


multi_emp_prev <- multi_emp_prev %>%
  group_by(var, multi_emp_class) %>% 
  mutate(d=sum(n),
         p=n/d,
         margin = qnorm(0.975)*sqrt(p*(1-p)/d)*100,
         lci = est-margin,
         uci = est+margin) %>% 
  ungroup() %>% 
  dplyr::select(-c(d,p,margin)) %>% 
  mutate(sample_grp = ifelse(wv_n==6, "A","B")) #%>% 
#  mutate(exp_flag = factor(ifelse(multi_emp_class=="Multiple Employment",
 #                                 1,0)))

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

### SF-12 PCS ----------------------------
tiff("./output/weighted/multi_emp_sf12-pcs_prev_grouped.tiff")
multi_emp_prev %>% 
  filter(var=="SF-12 physical component") %>% 
  mutate(multi_emp_class=fct_reorder(multi_emp_class,est)) %>% 
  ggplot(aes(x=multi_emp_class, y=est,
             fill=exp_flag)) +
  geom_col(show.legend = FALSE) +
#  geom_errorbar(aes(ymin=lci, ymax=uci), colour="black", width=.1)+
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

### SF-12 MCS ----------------------------
tiff("./output/weighted/multi_emp_sf12-mcs_prev_grouped.tiff")
multi_emp_prev %>% 
  filter(var=="SF-12 mental component") %>% 
  mutate(multi_emp_class=fct_reorder(multi_emp_class,est)) %>% 
  ggplot(aes(x=multi_emp_class, y=est,
             fill=exp_flag)) +
  geom_col(show.legend = FALSE) +
  #  geom_errorbar(aes(ymin=lci, ymax=uci), colour="black", width=.1)+
  coord_flip() +
  theme_bw() +
  scale_fill_manual(name = "exp_flag", values=c("grey50","red")) +
  facet_wrap(~sample_grp, ncol = 1)
dev.off()

################################################################################
######              regression models - employment contract               ######
################################################################################

#### self-rated health ---------------------------------------------------------
### Sample A
srh_reg_a <- dfas1a_end_class1 %>% 
  mutate(srh_bin = ifelse(srh_bin == "good/fair/poor",1,0))

srh_reg_a$emp_contract_class <- factor(srh_reg_a$emp_contract_class)

srh_model_a <- glm(srh_bin ~ relevel(emp_contract_class, ref = 4)+sex_dv+age_dv+hiqual_dv,
    family=binomial,data=srh_reg_a)

srh_model_a_df <- data.frame(exp(cbind(OR = coef(srh_model_a), round(confint(srh_model_a),2))))
srh_model_a_df <- srh_model_a_df %>%
  rename("lci"="X2.5..",
         "uci"="X97.5..")

### Sample B
srh_reg_b <- dfas1b_end_class1 %>% 
  mutate(srh_bin = ifelse(srh_bin == "good/fair/poor",1,0))

srh_reg_b$emp_contract_class <- factor(srh_reg_b$emp_contract_class)

srh_model_b <- glm(srh_bin ~ relevel(emp_contract_class, ref = 4)+sex_dv+age_dv+hiqual_dv,
                   family=binomial,data=srh_reg_b)

srh_model_b_df <- data.frame(exp(cbind(OR = coef(srh_model_b), round(confint(srh_model_b),2))))
srh_model_b_df <- srh_model_b_df %>%
  rename("lci"="X2.5..",
         "uci"="X97.5..")

#### GHQ12 ---------------------------------------------------------------------
### Sample A
ghq_reg_a <- dfas1a_end_class1 %>% 
  mutate(ghq_case3 = ifelse(ghq_case3 == "3 or more",1,0))

ghq_reg_a$emp_contract_class <- factor(ghq_reg_a$emp_contract_class)

ghq_model_a <- glm(ghq_case3 ~ relevel(emp_contract_class, ref = 4)+sex_dv+age_dv+hiqual_dv,
                   family=binomial,data=ghq_reg_a)

ghq_model_a_df <- data.frame(exp(cbind(OR = coef(ghq_model_a), round(confint(ghq_model_a),2))))
ghq_model_a_df <- ghq_model_a_df %>%
  rename("lci"="X2.5..",
         "uci"="X97.5..")

### Sample B
ghq_reg_b <- dfas1b_end_class1 %>% 
  mutate(ghq_case3 = ifelse(ghq_case3 == "3 or more",1,0))

ghq_reg_b$emp_contract_class <- factor(ghq_reg_b$emp_contract_class)

ghq_model_b <- glm(ghq_case3 ~ relevel(emp_contract_class, ref = 4)+sex_dv+age_dv+hiqual_dv,
                   family=binomial,data=ghq_reg_b)

ghq_model_b_df <- data.frame(exp(cbind(OR = coef(ghq_model_b), round(confint(ghq_model_b),2))))
ghq_model_b_df <- ghq_model_b_df %>%
  rename("lci"="X2.5..",
         "uci"="X97.5..")

