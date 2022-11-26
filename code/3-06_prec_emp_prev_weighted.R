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


## disable scientific notation
options(scipen=999)


################################################################################
#####                            install packages                          #####
################################################################################

library(tidyverse) # all kinds of stuff 
library(broom) # for working with statistical outputs
#library(TraMineR) # for sequence analysis
#library(poLCA) # for latent class analysis
#library(randomLCA) # for repeated measures latent class analysis
library(survey) # for applying survey weights to analysis
options(survey.lonely.psu="adjust") # single-PSU strata are centred at the sample mean
library(epitools)
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
#####               age standardisation  - employment contract             #####
################################################################################


### select only the required variables
## Self-rated health
srh_a_df <- dfas1a_end_class1 %>% 
  dplyr::select(pidp, indinub_xw, psu, strata,
                emp_contract_class, age_dv_grp, 
                sex_dv, srh_bin) %>% 
  mutate(emp_contract_class = factor(emp_contract_class)) 

srh_b_df <- dfas1b_end_class1 %>% 
  dplyr::select(pidp, indinub_xw, psu, strata,
                emp_contract_class, age_dv_grp, 
                sex_dv, srh_bin) %>% 
  mutate(emp_contract_class = factor(emp_contract_class)) 

## GHQ-12
ghq_a_df <- dfas1a_end_class1 %>% 
  dplyr::select(pidp, indinub_xw, psu, strata,
                emp_contract_class, age_dv_grp, 
                sex_dv, ghq_case3) %>% 
  mutate(emp_contract_class = factor(emp_contract_class)) 

ghq_b_df <- dfas1b_end_class1 %>% 
  dplyr::select(pidp, indinub_xw, psu, strata,
                emp_contract_class, age_dv_grp, 
                sex_dv, ghq_case3) %>% 
  mutate(emp_contract_class = factor(emp_contract_class)) 

### create a list for each exposure class
srh_splits_a <- split(srh_a_df, paste0(srh_a_df$emp_contract_class,"$",srh_a_df$sex_dv))
srh_splits_b <- split(srh_b_df, paste0(srh_b_df$emp_contract_class,"$",srh_b_df$sex_dv))
ghq_splits_a <- split(ghq_a_df, paste0(ghq_a_df$emp_contract_class,"$",ghq_a_df$sex_dv))
ghq_splits_b <- split(ghq_b_df, paste0(ghq_b_df$emp_contract_class,"$",ghq_b_df$sex_dv))

### create denominator variable (number of individuals in age-sex group)

## these are just tests to check code before converting into functions
#test <- svydesign(id=~psu, strata=~strata,
#          weights=~indinub_xw, data=ghq_splits[[1]])
#test2 <- data.frame(svytotal(~age_dv_grp,test, na.rm=TRUE))
#rm(test, test2)

## define function to create weighted df
svydesign_function <- function(x){
  svydesign(id=~psu, strata=~strata,
            weights=~indinub_xw, data=x)
  }

### call function to create weighted dataframes for each exposure class
svy_srh_splits_a <- lapply(srh_splits_a, svydesign_function)
svy_srh_splits_b <- lapply(srh_splits_b, svydesign_function)
svy_ghq_splits_a <- lapply(ghq_splits_a, svydesign_function)
svy_ghq_splits_b <- lapply(ghq_splits_b, svydesign_function)

### define function to calculate denominators
svytotal_function <- function(x){
  data.frame(svytotal(~age_dv_grp,x, na.rm=TRUE))

}

### call function to create weighted denominators for each exposure class
svy_srh_splits_a_d <- lapply(svy_srh_splits_a, svytotal_function) 
svy_srh_splits_b_d <- lapply(svy_srh_splits_b, svytotal_function) 
svy_ghq_splits_a_d <- lapply(svy_ghq_splits_a, svytotal_function) 
svy_ghq_splits_b_d <- lapply(svy_ghq_splits_b, svytotal_function) 

## drop SE columns
svy_srh_splits_a_d <- map(svy_srh_splits_a_d, ~ (.x %>% dplyr::select(-SE)))
svy_srh_splits_b_d <- map(svy_srh_splits_b_d, ~ (.x %>% dplyr::select(-SE)))
svy_ghq_splits_a_d <- map(svy_ghq_splits_a_d, ~ (.x %>% dplyr::select(-SE)))
svy_ghq_splits_b_d <- map(svy_ghq_splits_b_d, ~ (.x %>% dplyr::select(-SE)))

## add in age band column
svy_srh_splits_a_d <- map(svy_srh_splits_a_d, ~ (cbind(rownames(.x), .x, row.names=NULL)))
svy_srh_splits_b_d <- map(svy_srh_splits_b_d, ~ (cbind(rownames(.x), .x, row.names=NULL)))
svy_ghq_splits_a_d <- map(svy_ghq_splits_a_d, ~ (cbind(rownames(.x), .x, row.names=NULL)))
svy_ghq_splits_b_d <- map(svy_ghq_splits_b_d, ~ (cbind(rownames(.x), .x, row.names=NULL)))

##  fix row names
col_vec <- c("age_dv_grp","d")
svy_srh_splits_a_d <- lapply(svy_srh_splits_a_d, setNames, col_vec)
svy_srh_splits_b_d <- lapply(svy_srh_splits_b_d, setNames, col_vec)
svy_ghq_splits_a_d <- lapply(svy_ghq_splits_a_d, setNames, col_vec)
svy_ghq_splits_b_d <- lapply(svy_ghq_splits_b_d, setNames, col_vec)

## sort out age band column
svy_srh_splits_a_d <- map(svy_srh_splits_a_d, ~ (.x %>% mutate(age_dv_grp = str_remove(age_dv_grp, "age_dv_grp"))))
svy_srh_splits_b_d <- map(svy_srh_splits_b_d, ~ (.x %>% mutate(age_dv_grp = str_remove(age_dv_grp, "age_dv_grp"))))
svy_ghq_splits_a_d <- map(svy_ghq_splits_a_d, ~ (.x %>% mutate(age_dv_grp = str_remove(age_dv_grp, "age_dv_grp"))))
svy_ghq_splits_b_d <- map(svy_ghq_splits_b_d, ~ (.x %>% mutate(age_dv_grp = str_remove(age_dv_grp, "age_dv_grp"))))

## add in cols for class and sex
svy_srh_splits_a_d <- lapply(names(svy_srh_splits_a_d), function(current_name) 
  transform(svy_srh_splits_a_d[[current_name]],
            new_column = current_name))

svy_srh_splits_b_d <- lapply(names(svy_srh_splits_b_d), function(current_name) 
  transform(svy_ghq_splits_b_d[[current_name]],
            new_column = current_name))

svy_ghq_splits_a_d <- lapply(names(svy_ghq_splits_a_d), function(current_name) 
  transform(svy_ghq_splits_a_d[[current_name]],
            new_column = current_name))

svy_ghq_splits_b_d <- lapply(names(svy_ghq_splits_b_d), function(current_name) 
  transform(svy_ghq_splits_b_d[[current_name]],
            new_column = current_name))



# create_class_mem var
svy_srh_splits_a_d <- map(svy_srh_splits_a_d, ~ (.x %>% mutate(class_mem = str_extract(new_column, "[^$]+"))))
svy_srh_splits_b_d <- map(svy_srh_splits_b_d, ~ (.x %>% mutate(class_mem = str_extract(new_column, "[^$]+"))))
svy_ghq_splits_a_d <- map(svy_ghq_splits_a_d, ~ (.x %>% mutate(class_mem = str_extract(new_column, "[^$]+"))))
svy_ghq_splits_b_d <- map(svy_ghq_splits_b_d, ~ (.x %>% mutate(class_mem = str_extract(new_column, "[^$]+"))))

# create sex var
svy_srh_splits_a_d <- map(svy_srh_splits_a_d, ~ (.x %>% mutate(sex_dv = str_extract(new_column, "\\b\\w+$"))))
svy_srh_splits_b_d <- map(svy_srh_splits_b_d, ~ (.x %>% mutate(sex_dv = str_extract(new_column, "\\b\\w+$"))))
svy_ghq_splits_a_d <- map(svy_ghq_splits_a_d, ~ (.x %>% mutate(sex_dv = str_extract(new_column, "\\b\\w+$"))))
svy_ghq_splits_b_d <- map(svy_ghq_splits_b_d, ~ (.x %>% mutate(sex_dv = str_extract(new_column, "\\b\\w+$"))))

# drop new_column var
svy_srh_splits_a_d <- map(svy_srh_splits_a_d, ~ (.x %>% dplyr::select(-new_column)))
svy_srh_splits_b_d <- map(svy_srh_splits_b_d, ~ (.x %>% dplyr::select(-new_column)))
svy_ghq_splits_a_d <- map(svy_ghq_splits_a_d, ~ (.x %>% dplyr::select(-new_column)))
svy_ghq_splits_b_d <- map(svy_ghq_splits_b_d, ~ (.x %>% dplyr::select(-new_column)))


### create numerator variable (number of cases in age-sex group)

# test
#data.frame(svyby(~age_dv_grp, ~ghq_case3, svy_ghq_splits[[1]], svytotal, na.rm=TRUE))

## function to calculate numerators
# for self-rated health
svy_numerator_srh <- function(x){
data.frame(svyby(~age_dv_grp, ~srh_bin, x, svytotal, na.rm=TRUE))
}
# for GHQ-12
svy_numerator_ghq <- function(x){
  data.frame(svyby(~age_dv_grp, ~ghq_case3, x, svytotal, na.rm=TRUE))
}

# call function
svy_srh_splits_a_n <- lapply(svy_srh_splits_a, svy_numerator_srh)
svy_srh_splits_b_n <- lapply(svy_srh_splits_b, svy_numerator_srh)
svy_ghq_splits_a_n <- lapply(svy_ghq_splits_a, svy_numerator_ghq)
svy_ghq_splits_b_n <- lapply(svy_ghq_splits_b, svy_numerator_ghq)

# keep cols 1:10
svy_srh_splits_a_n <- map(svy_srh_splits_a_n, ~ (.x %>% dplyr::select(1:10)))
svy_srh_splits_b_n <- map(svy_srh_splits_b_n, ~ (.x %>% dplyr::select(1:10)))
svy_ghq_splits_a_n <- map(svy_ghq_splits_a_n, ~ (.x %>% dplyr::select(1:10)))
svy_ghq_splits_b_n <- map(svy_ghq_splits_b_n, ~ (.x %>% dplyr::select(1:10)))

# pivot long
svy_srh_splits_a_n <- map(svy_srh_splits_a_n, ~ (.x %>% pivot_longer(cols = 2:10, names_to = "age_dv_grp",
                                             values_to = "n")))
svy_srh_splits_b_n <- map(svy_srh_splits_b_n, ~ (.x %>% pivot_longer(cols = 2:10, names_to = "age_dv_grp",
                                                                 values_to = "n")))
svy_ghq_splits_a_n <- map(svy_ghq_splits_a_n, ~ (.x %>% pivot_longer(cols = 2:10, names_to = "age_dv_grp",
                                                                 values_to = "n")))
svy_ghq_splits_b_n <- map(svy_ghq_splits_b_n, ~ (.x %>% pivot_longer(cols = 2:10, names_to = "age_dv_grp",
                                                                 values_to = "n")))

# sort out age band column
svy_srh_splits_a_n <- map(svy_srh_splits_a_n, ~ (.x %>% mutate(age_dv_grp = str_remove(age_dv_grp, "age_dv_grp"))))
svy_srh_splits_a_n <- map(svy_srh_splits_a_n, ~ (.x %>% mutate(age_dv_grp = str_replace(age_dv_grp, "\\.","-"))))
svy_srh_splits_b_n <- map(svy_srh_splits_b_n, ~ (.x %>% mutate(age_dv_grp = str_remove(age_dv_grp, "age_dv_grp"))))
svy_srh_splits_b_n <- map(svy_srh_splits_b_n, ~ (.x %>% mutate(age_dv_grp = str_replace(age_dv_grp, "\\.","-"))))
svy_ghq_splits_a_n <- map(svy_ghq_splits_a_n, ~ (.x %>% mutate(age_dv_grp = str_remove(age_dv_grp, "age_dv_grp"))))
svy_ghq_splits_a_n <- map(svy_ghq_splits_a_n, ~ (.x %>% mutate(age_dv_grp = str_replace(age_dv_grp, "\\.","-"))))
svy_ghq_splits_b_n <- map(svy_ghq_splits_b_n, ~ (.x %>% mutate(age_dv_grp = str_remove(age_dv_grp, "age_dv_grp"))))
svy_ghq_splits_b_n <- map(svy_ghq_splits_b_n, ~ (.x %>% mutate(age_dv_grp = str_replace(age_dv_grp, "\\.","-"))))

# select only cases
svy_srh_splits_a_n <- map(svy_srh_splits_a_n, ~ (.x %>% filter(srh_bin=="good/fair/poor")))
svy_srh_splits_b_n <- map(svy_srh_splits_b_n, ~ (.x %>% filter(srh_bin=="good/fair/poor")))
svy_ghq_splits_a_n <- map(svy_ghq_splits_a_n, ~ (.x %>% filter(ghq_case3=="3 or more")))
svy_ghq_splits_b_n <- map(svy_ghq_splits_b_n, ~ (.x %>% filter(ghq_case3=="3 or more")))


# add in cols for class and sex
svy_srh_splits_a_n <- lapply(names(svy_srh_splits_a_n), function(current_name) 
  transform(svy_srh_splits_a_n[[current_name]],
            new_column = current_name))

svy_srh_splits_b_n <- lapply(names(svy_srh_splits_b_n), function(current_name) 
  transform(svy_srh_splits_b_n[[current_name]],
            new_column = current_name))

svy_ghq_splits_a_n <- lapply(names(svy_ghq_splits_a_n), function(current_name) 
  transform(svy_ghq_splits_a_n[[current_name]],
            new_column = current_name))

svy_ghq_splits_b_n <- lapply(names(svy_ghq_splits_b_n), function(current_name) 
  transform(svy_ghq_splits_b_n[[current_name]],
            new_column = current_name))

svy_srh_splits_a_n <- map(svy_srh_splits_a_n, ~ (.x %>% mutate(class_mem = str_extract(new_column, "[^$]+"))))
svy_srh_splits_b_n <- map(svy_srh_splits_b_n, ~ (.x %>% mutate(class_mem = str_extract(new_column, "[^$]+"))))
svy_ghq_splits_a_n <- map(svy_ghq_splits_a_n, ~ (.x %>% mutate(class_mem = str_extract(new_column, "[^$]+"))))
svy_ghq_splits_b_n <- map(svy_ghq_splits_b_n, ~ (.x %>% mutate(class_mem = str_extract(new_column, "[^$]+"))))


svy_srh_splits_a_n <- map(svy_srh_splits_a_n, ~ (.x %>% mutate(sex_dv = str_extract(new_column, "\\b\\w+$"))))
svy_srh_splits_b_n <- map(svy_srh_splits_b_n, ~ (.x %>% mutate(sex_dv = str_extract(new_column, "\\b\\w+$"))))
svy_ghq_splits_a_n <- map(svy_ghq_splits_a_n, ~ (.x %>% mutate(sex_dv = str_extract(new_column, "\\b\\w+$"))))
svy_ghq_splits_b_n <- map(svy_ghq_splits_b_n, ~ (.x %>% mutate(sex_dv = str_extract(new_column, "\\b\\w+$"))))


svy_srh_splits_a_n <- map(svy_srh_splits_a_n, ~ (.x %>% dplyr::select(-new_column)))
svy_srh_splits_b_n <- map(svy_srh_splits_b_n, ~ (.x %>% dplyr::select(-new_column)))
svy_ghq_splits_a_n <- map(svy_ghq_splits_a_n, ~ (.x %>% dplyr::select(-new_column)))
svy_ghq_splits_b_n <- map(svy_ghq_splits_b_n, ~ (.x %>% dplyr::select(-new_column)))

#### join denominator,  numerator dfs and standard pop ------------

# note - this converts list back into a single dataframe
svy_srh_a_df <- map2_df(svy_srh_splits_a_d, svy_srh_splits_a_n, left_join, by = c("age_dv_grp",
                                                              "sex_dv",
                                                              "class_mem"))

svy_srh_b_df <- map2_df(svy_srh_splits_b_d, svy_srh_splits_b_n, left_join, by = c("age_dv_grp",
                                                                                  "sex_dv",
                                                                                  "class_mem"))


svy_ghq_a_df <- map2_df(svy_ghq_splits_a_d, svy_ghq_splits_a_n, left_join, by = c("age_dv_grp",
                                                                            "sex_dv",
                                                                            "class_mem"))

svy_ghq_b_df <- map2_df(svy_ghq_splits_b_d, svy_ghq_splits_b_n, left_join, by = c("age_dv_grp",
                                                                                  "sex_dv",
                                                                                  "class_mem"))


# add in outcome label var and sample group
svy_srh_a_df <- svy_srh_a_df %>% 
  mutate(sample_grp = "A",
         outcome_lab = "poor self-rated health") %>% 
  dplyr::select(-srh_bin)

svy_srh_b_df <- svy_srh_b_df %>% 
  mutate(sample_grp = "B",
         outcome_lab = "poor self-rated health") %>% 
  dplyr::select(-srh_bin)

svy_ghq_a_df <- svy_ghq_a_df %>% 
  mutate(sample_grp = "A",
         outcome_lab = "poor mental health") %>% 
  dplyr::select(-ghq_case3)

svy_ghq_b_df <- svy_ghq_b_df %>% 
  mutate(sample_grp = "B",
         outcome_lab = "poor mental health") %>% 
  dplyr::select(-ghq_case3)

### put back into a list of df's
svy_df_list <- list(svy_srh_a_df, svy_srh_b_df, svy_ghq_a_df, svy_ghq_b_df)

# change sex_dv to lowercase
svy_df_list <- map(svy_df_list, ~(.x %>% 
                                    mutate(sex_dv = ifelse(sex_dv=="Male","male",
                                                                ifelse(sex_dv=="Female", "female",
                                                                       "CHECK")))))

# join on the European std pop df
svy_df_list <- map(svy_df_list, ~ (.x %>% 
                                     left_join(esp, by = c("age_dv_grp", "sex_dv"))))

# reorder df cols
svy_df_list <- map(svy_df_list, ~.x %>% 
                     dplyr::select(sample_grp, outcome_lab, class_mem, sex_dv, 
                                   age_dv_grp, n, d, euro_std_pop) %>% 
                     arrange(sample_grp, outcome_lab, class_mem, sex_dv, age_dv_grp))


## convert numerator and denominator to integers
svy_df_list <- map(svy_df_list, ~(.x %>%
                     mutate(n = as.integer(n),
                            d = as.integer(d))))

## convert zero denominators to 1 so that division possible
svy_df_list <- map(svy_df_list, ~(.x %>% mutate(d = ifelse(d==0,1,d))))


## back to single df -- keep for now, might not need
#svy_df <- bind_rows(svy_df_list)

#### calculate standardised rates -----------------------

## calculate for females and meals separately
svy_dsr_list <- map(svy_df_list, 
                    ~(.x %>%
                        group_by(outcome_lab, sample_grp, class_mem, sex_dv) %>%
                        PHEindicatormethods::phe_dsr(
                          x = n,                 # column with observed number of events
                          n = d,                 # column with non-standard pops for each stratum
                          stdpop = euro_std_pop, # standard populations for each stratum
                          stdpoptype = "field",  # either "vector" for a standalone vector or "field" meaning std populations are in the data  
                          multiplier = 100) %>%       # set to per 100 pop
                        ungroup()))

# convert back into singl df
svy_dsr_df <- bind_rows(svy_dsr_list)  

## calculate age-sex standardised rates by averaging male and female rates
temp_list <- map(svy_dsr_list, 
            ~(.x %>% 
                 group_by(outcome_lab, sample_grp, class_mem) %>% 
                 summarise(total_count = sum(total_count),
                           total_pop = sum(total_pop),
                           value = mean(value),
                           lowercl = mean(lowercl),
                           uppercl = mean(uppercl),
                           confidence = first(confidence),
                           statistic = first(statistic),
                           method = first(method)) %>% 
                 mutate(sex_dv = "both") %>% 
                 ungroup() %>% 
                 dplyr::select(c(outcome_lab, sample_grp, class_mem, sex_dv, everything()))))
  

temp_df <- bind_rows(temp_list)  

### bind age-sex dsr df onto main df
svy_dsr_df1 <- svy_dsr_df %>% bind_rows(temp_df) %>% 
  arrange(outcome_lab, sample_grp,class_mem, sex_dv)

rm(temp_list, temp_df)

################################################################################
#####                age standardisation  - employment spells              #####
################################################################################


### select only the required variables
## Self-rated health
srh_a_df <- dfas1a_end_class2 %>% 
  dplyr::select(pidp, indinub_xw, psu, strata,
                emp_spells_class, age_dv_grp, 
                sex_dv, srh_bin) %>% 
  mutate(emp_spells_class = factor(emp_spells_class)) 

srh_b_df <- dfas1b_end_class2 %>% 
  dplyr::select(pidp, indinub_xw, psu, strata,
                emp_spells_class, age_dv_grp, 
                sex_dv, srh_bin) %>% 
  mutate(emp_spells_class = factor(emp_spells_class)) 

## GHQ-12
ghq_a_df <- dfas1a_end_class2 %>% 
  dplyr::select(pidp, indinub_xw, psu, strata,
                emp_spells_class, age_dv_grp, 
                sex_dv, ghq_case3) %>% 
  mutate(emp_spells_class = factor(emp_spells_class)) 

ghq_b_df <- dfas1b_end_class2 %>% 
  dplyr::select(pidp, indinub_xw, psu, strata,
                emp_spells_class, age_dv_grp, 
                sex_dv, ghq_case3) %>% 
  mutate(emp_spells_class = factor(emp_spells_class)) 

### create a list for each exposure class
srh_splits_a <- split(srh_a_df, paste0(srh_a_df$emp_spells_class,"$",srh_a_df$sex_dv))
srh_splits_b <- split(srh_b_df, paste0(srh_b_df$emp_spells_class,"$",srh_b_df$sex_dv))
ghq_splits_a <- split(ghq_a_df, paste0(ghq_a_df$emp_spells_class,"$",ghq_a_df$sex_dv))
ghq_splits_b <- split(ghq_b_df, paste0(ghq_b_df$emp_spells_class,"$",ghq_b_df$sex_dv))

### create denominator variable (number of individuals in age-sex group)

## these are just tests to check code before converting into functions
#test <- svydesign(id=~psu, strata=~strata,
#          weights=~indinub_xw, data=ghq_splits[[1]])
#test2 <- data.frame(svytotal(~age_dv_grp,test, na.rm=TRUE))
#rm(test, test2)

## define function to create weighted df
svydesign_function <- function(x){
  svydesign(id=~psu, strata=~strata,
            weights=~indinub_xw, data=x)
}

### call function to create weighted dataframes for each exposure class
svy_srh_splits_a <- lapply(srh_splits_a, svydesign_function)
svy_srh_splits_b <- lapply(srh_splits_b, svydesign_function)
svy_ghq_splits_a <- lapply(ghq_splits_a, svydesign_function)
svy_ghq_splits_b <- lapply(ghq_splits_b, svydesign_function)

### define function to calculate denominators
svytotal_function <- function(x){
  data.frame(svytotal(~age_dv_grp,x, na.rm=TRUE))
  
}

### call function to create weighted denominators for each exposure class
svy_srh_splits_a_d <- lapply(svy_srh_splits_a, svytotal_function) 
svy_srh_splits_b_d <- lapply(svy_srh_splits_b, svytotal_function) 
svy_ghq_splits_a_d <- lapply(svy_ghq_splits_a, svytotal_function) 
svy_ghq_splits_b_d <- lapply(svy_ghq_splits_b, svytotal_function) 

## drop SE columns
svy_srh_splits_a_d <- map(svy_srh_splits_a_d, ~ (.x %>% dplyr::select(-SE)))
svy_srh_splits_b_d <- map(svy_srh_splits_b_d, ~ (.x %>% dplyr::select(-SE)))
svy_ghq_splits_a_d <- map(svy_ghq_splits_a_d, ~ (.x %>% dplyr::select(-SE)))
svy_ghq_splits_b_d <- map(svy_ghq_splits_b_d, ~ (.x %>% dplyr::select(-SE)))

## add in age band column
svy_srh_splits_a_d <- map(svy_srh_splits_a_d, ~ (cbind(rownames(.x), .x, row.names=NULL)))
svy_srh_splits_b_d <- map(svy_srh_splits_b_d, ~ (cbind(rownames(.x), .x, row.names=NULL)))
svy_ghq_splits_a_d <- map(svy_ghq_splits_a_d, ~ (cbind(rownames(.x), .x, row.names=NULL)))
svy_ghq_splits_b_d <- map(svy_ghq_splits_b_d, ~ (cbind(rownames(.x), .x, row.names=NULL)))

##  fix row names
col_vec <- c("age_dv_grp","d")
svy_srh_splits_a_d <- lapply(svy_srh_splits_a_d, setNames, col_vec)
svy_srh_splits_b_d <- lapply(svy_srh_splits_b_d, setNames, col_vec)
svy_ghq_splits_a_d <- lapply(svy_ghq_splits_a_d, setNames, col_vec)
svy_ghq_splits_b_d <- lapply(svy_ghq_splits_b_d, setNames, col_vec)

## sort out age band column
svy_srh_splits_a_d <- map(svy_srh_splits_a_d, ~ (.x %>% mutate(age_dv_grp = str_remove(age_dv_grp, "age_dv_grp"))))
svy_srh_splits_b_d <- map(svy_srh_splits_b_d, ~ (.x %>% mutate(age_dv_grp = str_remove(age_dv_grp, "age_dv_grp"))))
svy_ghq_splits_a_d <- map(svy_ghq_splits_a_d, ~ (.x %>% mutate(age_dv_grp = str_remove(age_dv_grp, "age_dv_grp"))))
svy_ghq_splits_b_d <- map(svy_ghq_splits_b_d, ~ (.x %>% mutate(age_dv_grp = str_remove(age_dv_grp, "age_dv_grp"))))

## add in cols for class and sex
svy_srh_splits_a_d <- lapply(names(svy_srh_splits_a_d), function(current_name) 
  transform(svy_srh_splits_a_d[[current_name]],
            new_column = current_name))

svy_srh_splits_b_d <- lapply(names(svy_srh_splits_b_d), function(current_name) 
  transform(svy_ghq_splits_b_d[[current_name]],
            new_column = current_name))

svy_ghq_splits_a_d <- lapply(names(svy_ghq_splits_a_d), function(current_name) 
  transform(svy_ghq_splits_a_d[[current_name]],
            new_column = current_name))

svy_ghq_splits_b_d <- lapply(names(svy_ghq_splits_b_d), function(current_name) 
  transform(svy_ghq_splits_b_d[[current_name]],
            new_column = current_name))



# create_class_mem var
svy_srh_splits_a_d <- map(svy_srh_splits_a_d, ~ (.x %>% mutate(class_mem = str_extract(new_column, "[^$]+"))))
svy_srh_splits_b_d <- map(svy_srh_splits_b_d, ~ (.x %>% mutate(class_mem = str_extract(new_column, "[^$]+"))))
svy_ghq_splits_a_d <- map(svy_ghq_splits_a_d, ~ (.x %>% mutate(class_mem = str_extract(new_column, "[^$]+"))))
svy_ghq_splits_b_d <- map(svy_ghq_splits_b_d, ~ (.x %>% mutate(class_mem = str_extract(new_column, "[^$]+"))))

# create sex var
svy_srh_splits_a_d <- map(svy_srh_splits_a_d, ~ (.x %>% mutate(sex_dv = str_extract(new_column, "\\b\\w+$"))))
svy_srh_splits_b_d <- map(svy_srh_splits_b_d, ~ (.x %>% mutate(sex_dv = str_extract(new_column, "\\b\\w+$"))))
svy_ghq_splits_a_d <- map(svy_ghq_splits_a_d, ~ (.x %>% mutate(sex_dv = str_extract(new_column, "\\b\\w+$"))))
svy_ghq_splits_b_d <- map(svy_ghq_splits_b_d, ~ (.x %>% mutate(sex_dv = str_extract(new_column, "\\b\\w+$"))))

# drop new_column var
svy_srh_splits_a_d <- map(svy_srh_splits_a_d, ~ (.x %>% dplyr::select(-new_column)))
svy_srh_splits_b_d <- map(svy_srh_splits_b_d, ~ (.x %>% dplyr::select(-new_column)))
svy_ghq_splits_a_d <- map(svy_ghq_splits_a_d, ~ (.x %>% dplyr::select(-new_column)))
svy_ghq_splits_b_d <- map(svy_ghq_splits_b_d, ~ (.x %>% dplyr::select(-new_column)))


### create numerator variable (number of cases in age-sex group)

# test
#data.frame(svyby(~age_dv_grp, ~ghq_case3, svy_ghq_splits[[1]], svytotal, na.rm=TRUE))

## function to calculate numerators
# for self-rated health
svy_numerator_srh <- function(x){
  data.frame(svyby(~age_dv_grp, ~srh_bin, x, svytotal, na.rm=TRUE))
}
# for GHQ-12
svy_numerator_ghq <- function(x){
  data.frame(svyby(~age_dv_grp, ~ghq_case3, x, svytotal, na.rm=TRUE))
}

# call function
svy_srh_splits_a_n <- lapply(svy_srh_splits_a, svy_numerator_srh)
svy_srh_splits_b_n <- lapply(svy_srh_splits_b, svy_numerator_srh)
svy_ghq_splits_a_n <- lapply(svy_ghq_splits_a, svy_numerator_ghq)
svy_ghq_splits_b_n <- lapply(svy_ghq_splits_b, svy_numerator_ghq)

# keep cols 1:10
svy_srh_splits_a_n <- map(svy_srh_splits_a_n, ~ (.x %>% dplyr::select(1:10)))
svy_srh_splits_b_n <- map(svy_srh_splits_b_n, ~ (.x %>% dplyr::select(1:10)))
svy_ghq_splits_a_n <- map(svy_ghq_splits_a_n, ~ (.x %>% dplyr::select(1:10)))
svy_ghq_splits_b_n <- map(svy_ghq_splits_b_n, ~ (.x %>% dplyr::select(1:10)))

# pivot long
svy_srh_splits_a_n <- map(svy_srh_splits_a_n, ~ (.x %>% pivot_longer(cols = 2:10, names_to = "age_dv_grp",
                                                                     values_to = "n")))
svy_srh_splits_b_n <- map(svy_srh_splits_b_n, ~ (.x %>% pivot_longer(cols = 2:10, names_to = "age_dv_grp",
                                                                     values_to = "n")))
svy_ghq_splits_a_n <- map(svy_ghq_splits_a_n, ~ (.x %>% pivot_longer(cols = 2:10, names_to = "age_dv_grp",
                                                                     values_to = "n")))
svy_ghq_splits_b_n <- map(svy_ghq_splits_b_n, ~ (.x %>% pivot_longer(cols = 2:10, names_to = "age_dv_grp",
                                                                     values_to = "n")))

# sort out age band column
svy_srh_splits_a_n <- map(svy_srh_splits_a_n, ~ (.x %>% mutate(age_dv_grp = str_remove(age_dv_grp, "age_dv_grp"))))
svy_srh_splits_a_n <- map(svy_srh_splits_a_n, ~ (.x %>% mutate(age_dv_grp = str_replace(age_dv_grp, "\\.","-"))))
svy_srh_splits_b_n <- map(svy_srh_splits_b_n, ~ (.x %>% mutate(age_dv_grp = str_remove(age_dv_grp, "age_dv_grp"))))
svy_srh_splits_b_n <- map(svy_srh_splits_b_n, ~ (.x %>% mutate(age_dv_grp = str_replace(age_dv_grp, "\\.","-"))))
svy_ghq_splits_a_n <- map(svy_ghq_splits_a_n, ~ (.x %>% mutate(age_dv_grp = str_remove(age_dv_grp, "age_dv_grp"))))
svy_ghq_splits_a_n <- map(svy_ghq_splits_a_n, ~ (.x %>% mutate(age_dv_grp = str_replace(age_dv_grp, "\\.","-"))))
svy_ghq_splits_b_n <- map(svy_ghq_splits_b_n, ~ (.x %>% mutate(age_dv_grp = str_remove(age_dv_grp, "age_dv_grp"))))
svy_ghq_splits_b_n <- map(svy_ghq_splits_b_n, ~ (.x %>% mutate(age_dv_grp = str_replace(age_dv_grp, "\\.","-"))))

# select only cases
svy_srh_splits_a_n <- map(svy_srh_splits_a_n, ~ (.x %>% filter(srh_bin=="good/fair/poor")))
svy_srh_splits_b_n <- map(svy_srh_splits_b_n, ~ (.x %>% filter(srh_bin=="good/fair/poor")))
svy_ghq_splits_a_n <- map(svy_ghq_splits_a_n, ~ (.x %>% filter(ghq_case3=="3 or more")))
svy_ghq_splits_b_n <- map(svy_ghq_splits_b_n, ~ (.x %>% filter(ghq_case3=="3 or more")))


# add in cols for class and sex
svy_srh_splits_a_n <- lapply(names(svy_srh_splits_a_n), function(current_name) 
  transform(svy_srh_splits_a_n[[current_name]],
            new_column = current_name))

svy_srh_splits_b_n <- lapply(names(svy_srh_splits_b_n), function(current_name) 
  transform(svy_srh_splits_b_n[[current_name]],
            new_column = current_name))

svy_ghq_splits_a_n <- lapply(names(svy_ghq_splits_a_n), function(current_name) 
  transform(svy_ghq_splits_a_n[[current_name]],
            new_column = current_name))

svy_ghq_splits_b_n <- lapply(names(svy_ghq_splits_b_n), function(current_name) 
  transform(svy_ghq_splits_b_n[[current_name]],
            new_column = current_name))

svy_srh_splits_a_n <- map(svy_srh_splits_a_n, ~ (.x %>% mutate(class_mem = str_extract(new_column, "[^$]+"))))
svy_srh_splits_b_n <- map(svy_srh_splits_b_n, ~ (.x %>% mutate(class_mem = str_extract(new_column, "[^$]+"))))
svy_ghq_splits_a_n <- map(svy_ghq_splits_a_n, ~ (.x %>% mutate(class_mem = str_extract(new_column, "[^$]+"))))
svy_ghq_splits_b_n <- map(svy_ghq_splits_b_n, ~ (.x %>% mutate(class_mem = str_extract(new_column, "[^$]+"))))


svy_srh_splits_a_n <- map(svy_srh_splits_a_n, ~ (.x %>% mutate(sex_dv = str_extract(new_column, "\\b\\w+$"))))
svy_srh_splits_b_n <- map(svy_srh_splits_b_n, ~ (.x %>% mutate(sex_dv = str_extract(new_column, "\\b\\w+$"))))
svy_ghq_splits_a_n <- map(svy_ghq_splits_a_n, ~ (.x %>% mutate(sex_dv = str_extract(new_column, "\\b\\w+$"))))
svy_ghq_splits_b_n <- map(svy_ghq_splits_b_n, ~ (.x %>% mutate(sex_dv = str_extract(new_column, "\\b\\w+$"))))


svy_srh_splits_a_n <- map(svy_srh_splits_a_n, ~ (.x %>% dplyr::select(-new_column)))
svy_srh_splits_b_n <- map(svy_srh_splits_b_n, ~ (.x %>% dplyr::select(-new_column)))
svy_ghq_splits_a_n <- map(svy_ghq_splits_a_n, ~ (.x %>% dplyr::select(-new_column)))
svy_ghq_splits_b_n <- map(svy_ghq_splits_b_n, ~ (.x %>% dplyr::select(-new_column)))

#### join denominator,  numerator dfs and standard pop ------------

# note - this converts list back into a single dataframe
svy_srh_a_df <- map2_df(svy_srh_splits_a_d, svy_srh_splits_a_n, left_join, by = c("age_dv_grp",
                                                                                  "sex_dv",
                                                                                  "class_mem"))

svy_srh_b_df <- map2_df(svy_srh_splits_b_d, svy_srh_splits_b_n, left_join, by = c("age_dv_grp",
                                                                                  "sex_dv",
                                                                                  "class_mem"))


svy_ghq_a_df <- map2_df(svy_ghq_splits_a_d, svy_ghq_splits_a_n, left_join, by = c("age_dv_grp",
                                                                                  "sex_dv",
                                                                                  "class_mem"))

svy_ghq_b_df <- map2_df(svy_ghq_splits_b_d, svy_ghq_splits_b_n, left_join, by = c("age_dv_grp",
                                                                                  "sex_dv",
                                                                                  "class_mem"))


# add in outcome label var and sample group
svy_srh_a_df <- svy_srh_a_df %>% 
  mutate(sample_grp = "A",
         outcome_lab = "poor self-rated health") %>% 
  dplyr::select(-srh_bin)

svy_srh_b_df <- svy_srh_b_df %>% 
  mutate(sample_grp = "B",
         outcome_lab = "poor self-rated health") %>% 
  dplyr::select(-srh_bin)

svy_ghq_a_df <- svy_ghq_a_df %>% 
  mutate(sample_grp = "A",
         outcome_lab = "poor mental health") %>% 
  dplyr::select(-ghq_case3)

svy_ghq_b_df <- svy_ghq_b_df %>% 
  mutate(sample_grp = "B",
         outcome_lab = "poor mental health") %>% 
  dplyr::select(-ghq_case3)

### put back into a list of df's
svy_df_list <- list(svy_srh_a_df, svy_srh_b_df, svy_ghq_a_df, svy_ghq_b_df)

# change sex_dv to lowercase
svy_df_list <- map(svy_df_list, ~(.x %>% 
                                    mutate(sex_dv = ifelse(sex_dv=="Male","male",
                                                           ifelse(sex_dv=="Female", "female",
                                                                  "CHECK")))))

# join on the European std pop df
svy_df_list <- map(svy_df_list, ~ (.x %>% 
                                     left_join(esp, by = c("age_dv_grp", "sex_dv"))))

# reorder df cols
svy_df_list <- map(svy_df_list, ~.x %>% 
                     dplyr::select(sample_grp, outcome_lab, class_mem, sex_dv, 
                                   age_dv_grp, n, d, euro_std_pop) %>% 
                     arrange(sample_grp, outcome_lab, class_mem, sex_dv, age_dv_grp))


## convert numerator and denominator to integers
svy_df_list <- map(svy_df_list, ~(.x %>%
                                    mutate(n = as.integer(n),
                                           d = as.integer(d))))

## convert zero denominators to 1 so that division possible
svy_df_list <- map(svy_df_list, ~(.x %>% mutate(d = ifelse(d==0,1,d))))


## back to single df -- keep for now, might not need
#svy_df <- bind_rows(svy_df_list)

#### calculate standardised rates -----------------------

## calculate for females and meals separately
svy_dsr_list <- map(svy_df_list, 
                    ~(.x %>%
                        group_by(outcome_lab, sample_grp, class_mem, sex_dv) %>%
                        PHEindicatormethods::phe_dsr(
                          x = n,                 # column with observed number of events
                          n = d,                 # column with non-standard pops for each stratum
                          stdpop = euro_std_pop, # standard populations for each stratum
                          stdpoptype = "field",  # either "vector" for a standalone vector or "field" meaning std populations are in the data  
                          multiplier = 100) %>%       # set to per 100 pop
                        ungroup()))

# convert back into singl df
svy_dsr_df <- bind_rows(svy_dsr_list)  

## calculate age-sex standardised rates by averaging male and female rates
temp_list <- map(svy_dsr_list, 
                 ~(.x %>% 
                     group_by(outcome_lab, sample_grp, class_mem) %>% 
                     summarise(total_count = sum(total_count),
                               total_pop = sum(total_pop),
                               value = mean(value),
                               lowercl = mean(lowercl),
                               uppercl = mean(uppercl),
                               confidence = first(confidence),
                               statistic = first(statistic),
                               method = first(method)) %>% 
                     mutate(sex_dv = "both") %>% 
                     ungroup() %>% 
                     dplyr::select(c(outcome_lab, sample_grp, class_mem, sex_dv, everything()))))


temp_df <- bind_rows(temp_list)  

### bind age-sex dsr df onto main df
svy_dsr_df2 <- svy_dsr_df %>% bind_rows(temp_df) %>% 
  arrange(outcome_lab, sample_grp,class_mem, sex_dv)

rm(temp_list, temp_df)

################################################################################
#####               age standardisation  - multiple employment             #####
################################################################################


### select only the required variables
## Self-rated health
srh_a_df <- dfas1a_end_class3 %>% 
  dplyr::select(pidp, indinub_xw, psu, strata,
                multi_emp_class, age_dv_grp, 
                sex_dv, srh_bin) %>% 
  mutate(emp_spells_class = factor(multi_emp_class)) 

srh_b_df <- dfas1b_end_class3 %>% 
  dplyr::select(pidp, indinub_xw, psu, strata,
                multi_emp_class, age_dv_grp, 
                sex_dv, srh_bin) %>% 
  mutate(multi_emp_class = factor(multi_emp_class)) 

## GHQ-12
ghq_a_df <- dfas1a_end_class3 %>% 
  dplyr::select(pidp, indinub_xw, psu, strata,
                multi_emp_class, age_dv_grp, 
                sex_dv, ghq_case3) %>% 
  mutate(multi_emp_class = factor(multi_emp_class)) 

ghq_b_df <- dfas1b_end_class3 %>% 
  dplyr::select(pidp, indinub_xw, psu, strata,
                multi_emp_class, age_dv_grp, 
                sex_dv, ghq_case3) %>% 
  mutate(multi_emp_class = factor(multi_emp_class)) 

### create a list for each exposure class
srh_splits_a <- split(srh_a_df, paste0(srh_a_df$multi_emp_class,"$",srh_a_df$sex_dv))
srh_splits_b <- split(srh_b_df, paste0(srh_b_df$multi_emp_class,"$",srh_b_df$sex_dv))
ghq_splits_a <- split(ghq_a_df, paste0(ghq_a_df$multi_emp_class,"$",ghq_a_df$sex_dv))
ghq_splits_b <- split(ghq_b_df, paste0(ghq_b_df$multi_emp_class,"$",ghq_b_df$sex_dv))

### create denominator variable (number of individuals in age-sex group)

## these are just tests to check code before converting into functions
#test <- svydesign(id=~psu, strata=~strata,
#          weights=~indinub_xw, data=ghq_splits[[1]])
#test2 <- data.frame(svytotal(~age_dv_grp,test, na.rm=TRUE))
#rm(test, test2)

## define function to create weighted df
svydesign_function <- function(x){
  svydesign(id=~psu, strata=~strata,
            weights=~indinub_xw, data=x)
}

### call function to create weighted dataframes for each exposure class
svy_srh_splits_a <- lapply(srh_splits_a, svydesign_function)
svy_srh_splits_b <- lapply(srh_splits_b, svydesign_function)
svy_ghq_splits_a <- lapply(ghq_splits_a, svydesign_function)
svy_ghq_splits_b <- lapply(ghq_splits_b, svydesign_function)

### define function to calculate denominators
svytotal_function <- function(x){
  data.frame(svytotal(~age_dv_grp,x, na.rm=TRUE))
  
}

### call function to create weighted denominators for each exposure class
svy_srh_splits_a_d <- lapply(svy_srh_splits_a, svytotal_function) 
svy_srh_splits_b_d <- lapply(svy_srh_splits_b, svytotal_function) 
svy_ghq_splits_a_d <- lapply(svy_ghq_splits_a, svytotal_function) 
svy_ghq_splits_b_d <- lapply(svy_ghq_splits_b, svytotal_function) 

## drop SE columns
svy_srh_splits_a_d <- map(svy_srh_splits_a_d, ~ (.x %>% dplyr::select(-SE)))
svy_srh_splits_b_d <- map(svy_srh_splits_b_d, ~ (.x %>% dplyr::select(-SE)))
svy_ghq_splits_a_d <- map(svy_ghq_splits_a_d, ~ (.x %>% dplyr::select(-SE)))
svy_ghq_splits_b_d <- map(svy_ghq_splits_b_d, ~ (.x %>% dplyr::select(-SE)))

## add in age band column
svy_srh_splits_a_d <- map(svy_srh_splits_a_d, ~ (cbind(rownames(.x), .x, row.names=NULL)))
svy_srh_splits_b_d <- map(svy_srh_splits_b_d, ~ (cbind(rownames(.x), .x, row.names=NULL)))
svy_ghq_splits_a_d <- map(svy_ghq_splits_a_d, ~ (cbind(rownames(.x), .x, row.names=NULL)))
svy_ghq_splits_b_d <- map(svy_ghq_splits_b_d, ~ (cbind(rownames(.x), .x, row.names=NULL)))

##  fix row names
col_vec <- c("age_dv_grp","d")
svy_srh_splits_a_d <- lapply(svy_srh_splits_a_d, setNames, col_vec)
svy_srh_splits_b_d <- lapply(svy_srh_splits_b_d, setNames, col_vec)
svy_ghq_splits_a_d <- lapply(svy_ghq_splits_a_d, setNames, col_vec)
svy_ghq_splits_b_d <- lapply(svy_ghq_splits_b_d, setNames, col_vec)

## sort out age band column
svy_srh_splits_a_d <- map(svy_srh_splits_a_d, ~ (.x %>% mutate(age_dv_grp = str_remove(age_dv_grp, "age_dv_grp"))))
svy_srh_splits_b_d <- map(svy_srh_splits_b_d, ~ (.x %>% mutate(age_dv_grp = str_remove(age_dv_grp, "age_dv_grp"))))
svy_ghq_splits_a_d <- map(svy_ghq_splits_a_d, ~ (.x %>% mutate(age_dv_grp = str_remove(age_dv_grp, "age_dv_grp"))))
svy_ghq_splits_b_d <- map(svy_ghq_splits_b_d, ~ (.x %>% mutate(age_dv_grp = str_remove(age_dv_grp, "age_dv_grp"))))

## add in cols for class and sex
svy_srh_splits_a_d <- lapply(names(svy_srh_splits_a_d), function(current_name) 
  transform(svy_srh_splits_a_d[[current_name]],
            new_column = current_name))

svy_srh_splits_b_d <- lapply(names(svy_srh_splits_b_d), function(current_name) 
  transform(svy_ghq_splits_b_d[[current_name]],
            new_column = current_name))

svy_ghq_splits_a_d <- lapply(names(svy_ghq_splits_a_d), function(current_name) 
  transform(svy_ghq_splits_a_d[[current_name]],
            new_column = current_name))

svy_ghq_splits_b_d <- lapply(names(svy_ghq_splits_b_d), function(current_name) 
  transform(svy_ghq_splits_b_d[[current_name]],
            new_column = current_name))



# create_class_mem var
svy_srh_splits_a_d <- map(svy_srh_splits_a_d, ~ (.x %>% mutate(class_mem = str_extract(new_column, "[^$]+"))))
svy_srh_splits_b_d <- map(svy_srh_splits_b_d, ~ (.x %>% mutate(class_mem = str_extract(new_column, "[^$]+"))))
svy_ghq_splits_a_d <- map(svy_ghq_splits_a_d, ~ (.x %>% mutate(class_mem = str_extract(new_column, "[^$]+"))))
svy_ghq_splits_b_d <- map(svy_ghq_splits_b_d, ~ (.x %>% mutate(class_mem = str_extract(new_column, "[^$]+"))))

# create sex var
svy_srh_splits_a_d <- map(svy_srh_splits_a_d, ~ (.x %>% mutate(sex_dv = str_extract(new_column, "\\b\\w+$"))))
svy_srh_splits_b_d <- map(svy_srh_splits_b_d, ~ (.x %>% mutate(sex_dv = str_extract(new_column, "\\b\\w+$"))))
svy_ghq_splits_a_d <- map(svy_ghq_splits_a_d, ~ (.x %>% mutate(sex_dv = str_extract(new_column, "\\b\\w+$"))))
svy_ghq_splits_b_d <- map(svy_ghq_splits_b_d, ~ (.x %>% mutate(sex_dv = str_extract(new_column, "\\b\\w+$"))))

# drop new_column var
svy_srh_splits_a_d <- map(svy_srh_splits_a_d, ~ (.x %>% dplyr::select(-new_column)))
svy_srh_splits_b_d <- map(svy_srh_splits_b_d, ~ (.x %>% dplyr::select(-new_column)))
svy_ghq_splits_a_d <- map(svy_ghq_splits_a_d, ~ (.x %>% dplyr::select(-new_column)))
svy_ghq_splits_b_d <- map(svy_ghq_splits_b_d, ~ (.x %>% dplyr::select(-new_column)))


### create numerator variable (number of cases in age-sex group)

# test
#data.frame(svyby(~age_dv_grp, ~ghq_case3, svy_ghq_splits[[1]], svytotal, na.rm=TRUE))

## function to calculate numerators
# for self-rated health
svy_numerator_srh <- function(x){
  data.frame(svyby(~age_dv_grp, ~srh_bin, x, svytotal, na.rm=TRUE))
}
# for GHQ-12
svy_numerator_ghq <- function(x){
  data.frame(svyby(~age_dv_grp, ~ghq_case3, x, svytotal, na.rm=TRUE))
}

# call function
svy_srh_splits_a_n <- lapply(svy_srh_splits_a, svy_numerator_srh)
svy_srh_splits_b_n <- lapply(svy_srh_splits_b, svy_numerator_srh)
svy_ghq_splits_a_n <- lapply(svy_ghq_splits_a, svy_numerator_ghq)
svy_ghq_splits_b_n <- lapply(svy_ghq_splits_b, svy_numerator_ghq)

# keep cols 1:10
svy_srh_splits_a_n <- map(svy_srh_splits_a_n, ~ (.x %>% dplyr::select(1:10)))
svy_srh_splits_b_n <- map(svy_srh_splits_b_n, ~ (.x %>% dplyr::select(1:10)))
svy_ghq_splits_a_n <- map(svy_ghq_splits_a_n, ~ (.x %>% dplyr::select(1:10)))
svy_ghq_splits_b_n <- map(svy_ghq_splits_b_n, ~ (.x %>% dplyr::select(1:10)))

# pivot long
svy_srh_splits_a_n <- map(svy_srh_splits_a_n, ~ (.x %>% pivot_longer(cols = 2:10, names_to = "age_dv_grp",
                                                                     values_to = "n")))
svy_srh_splits_b_n <- map(svy_srh_splits_b_n, ~ (.x %>% pivot_longer(cols = 2:10, names_to = "age_dv_grp",
                                                                     values_to = "n")))
svy_ghq_splits_a_n <- map(svy_ghq_splits_a_n, ~ (.x %>% pivot_longer(cols = 2:10, names_to = "age_dv_grp",
                                                                     values_to = "n")))
svy_ghq_splits_b_n <- map(svy_ghq_splits_b_n, ~ (.x %>% pivot_longer(cols = 2:10, names_to = "age_dv_grp",
                                                                     values_to = "n")))

# sort out age band column
svy_srh_splits_a_n <- map(svy_srh_splits_a_n, ~ (.x %>% mutate(age_dv_grp = str_remove(age_dv_grp, "age_dv_grp"))))
svy_srh_splits_a_n <- map(svy_srh_splits_a_n, ~ (.x %>% mutate(age_dv_grp = str_replace(age_dv_grp, "\\.","-"))))
svy_srh_splits_b_n <- map(svy_srh_splits_b_n, ~ (.x %>% mutate(age_dv_grp = str_remove(age_dv_grp, "age_dv_grp"))))
svy_srh_splits_b_n <- map(svy_srh_splits_b_n, ~ (.x %>% mutate(age_dv_grp = str_replace(age_dv_grp, "\\.","-"))))
svy_ghq_splits_a_n <- map(svy_ghq_splits_a_n, ~ (.x %>% mutate(age_dv_grp = str_remove(age_dv_grp, "age_dv_grp"))))
svy_ghq_splits_a_n <- map(svy_ghq_splits_a_n, ~ (.x %>% mutate(age_dv_grp = str_replace(age_dv_grp, "\\.","-"))))
svy_ghq_splits_b_n <- map(svy_ghq_splits_b_n, ~ (.x %>% mutate(age_dv_grp = str_remove(age_dv_grp, "age_dv_grp"))))
svy_ghq_splits_b_n <- map(svy_ghq_splits_b_n, ~ (.x %>% mutate(age_dv_grp = str_replace(age_dv_grp, "\\.","-"))))

# select only cases
svy_srh_splits_a_n <- map(svy_srh_splits_a_n, ~ (.x %>% filter(srh_bin=="good/fair/poor")))
svy_srh_splits_b_n <- map(svy_srh_splits_b_n, ~ (.x %>% filter(srh_bin=="good/fair/poor")))
svy_ghq_splits_a_n <- map(svy_ghq_splits_a_n, ~ (.x %>% filter(ghq_case3=="3 or more")))
svy_ghq_splits_b_n <- map(svy_ghq_splits_b_n, ~ (.x %>% filter(ghq_case3=="3 or more")))


# add in cols for class and sex
svy_srh_splits_a_n <- lapply(names(svy_srh_splits_a_n), function(current_name) 
  transform(svy_srh_splits_a_n[[current_name]],
            new_column = current_name))

svy_srh_splits_b_n <- lapply(names(svy_srh_splits_b_n), function(current_name) 
  transform(svy_srh_splits_b_n[[current_name]],
            new_column = current_name))

svy_ghq_splits_a_n <- lapply(names(svy_ghq_splits_a_n), function(current_name) 
  transform(svy_ghq_splits_a_n[[current_name]],
            new_column = current_name))

svy_ghq_splits_b_n <- lapply(names(svy_ghq_splits_b_n), function(current_name) 
  transform(svy_ghq_splits_b_n[[current_name]],
            new_column = current_name))

svy_srh_splits_a_n <- map(svy_srh_splits_a_n, ~ (.x %>% mutate(class_mem = str_extract(new_column, "[^$]+"))))
svy_srh_splits_b_n <- map(svy_srh_splits_b_n, ~ (.x %>% mutate(class_mem = str_extract(new_column, "[^$]+"))))
svy_ghq_splits_a_n <- map(svy_ghq_splits_a_n, ~ (.x %>% mutate(class_mem = str_extract(new_column, "[^$]+"))))
svy_ghq_splits_b_n <- map(svy_ghq_splits_b_n, ~ (.x %>% mutate(class_mem = str_extract(new_column, "[^$]+"))))


svy_srh_splits_a_n <- map(svy_srh_splits_a_n, ~ (.x %>% mutate(sex_dv = str_extract(new_column, "\\b\\w+$"))))
svy_srh_splits_b_n <- map(svy_srh_splits_b_n, ~ (.x %>% mutate(sex_dv = str_extract(new_column, "\\b\\w+$"))))
svy_ghq_splits_a_n <- map(svy_ghq_splits_a_n, ~ (.x %>% mutate(sex_dv = str_extract(new_column, "\\b\\w+$"))))
svy_ghq_splits_b_n <- map(svy_ghq_splits_b_n, ~ (.x %>% mutate(sex_dv = str_extract(new_column, "\\b\\w+$"))))


svy_srh_splits_a_n <- map(svy_srh_splits_a_n, ~ (.x %>% dplyr::select(-new_column)))
svy_srh_splits_b_n <- map(svy_srh_splits_b_n, ~ (.x %>% dplyr::select(-new_column)))
svy_ghq_splits_a_n <- map(svy_ghq_splits_a_n, ~ (.x %>% dplyr::select(-new_column)))
svy_ghq_splits_b_n <- map(svy_ghq_splits_b_n, ~ (.x %>% dplyr::select(-new_column)))

#### join denominator,  numerator dfs and standard pop ------------

# note - this converts list back into a single dataframe
svy_srh_a_df <- map2_df(svy_srh_splits_a_d, svy_srh_splits_a_n, left_join, by = c("age_dv_grp",
                                                                                  "sex_dv",
                                                                                  "class_mem"))

svy_srh_b_df <- map2_df(svy_srh_splits_b_d, svy_srh_splits_b_n, left_join, by = c("age_dv_grp",
                                                                                  "sex_dv",
                                                                                  "class_mem"))


svy_ghq_a_df <- map2_df(svy_ghq_splits_a_d, svy_ghq_splits_a_n, left_join, by = c("age_dv_grp",
                                                                                  "sex_dv",
                                                                                  "class_mem"))

svy_ghq_b_df <- map2_df(svy_ghq_splits_b_d, svy_ghq_splits_b_n, left_join, by = c("age_dv_grp",
                                                                                  "sex_dv",
                                                                                  "class_mem"))


# add in outcome label var and sample group
svy_srh_a_df <- svy_srh_a_df %>% 
  mutate(sample_grp = "A",
         outcome_lab = "poor self-rated health") %>% 
  dplyr::select(-srh_bin)

svy_srh_b_df <- svy_srh_b_df %>% 
  mutate(sample_grp = "B",
         outcome_lab = "poor self-rated health") %>% 
  dplyr::select(-srh_bin)

svy_ghq_a_df <- svy_ghq_a_df %>% 
  mutate(sample_grp = "A",
         outcome_lab = "poor mental health") %>% 
  dplyr::select(-ghq_case3)

svy_ghq_b_df <- svy_ghq_b_df %>% 
  mutate(sample_grp = "B",
         outcome_lab = "poor mental health") %>% 
  dplyr::select(-ghq_case3)

### put back into a list of df's
svy_df_list <- list(svy_srh_a_df, svy_srh_b_df, svy_ghq_a_df, svy_ghq_b_df)

# change sex_dv to lowercase
svy_df_list <- map(svy_df_list, ~(.x %>% 
                                    mutate(sex_dv = ifelse(sex_dv=="Male","male",
                                                           ifelse(sex_dv=="Female", "female",
                                                                  "CHECK")))))

# join on the European std pop df
svy_df_list <- map(svy_df_list, ~ (.x %>% 
                                     left_join(esp, by = c("age_dv_grp", "sex_dv"))))

# reorder df cols
svy_df_list <- map(svy_df_list, ~.x %>% 
                     dplyr::select(sample_grp, outcome_lab, class_mem, sex_dv, 
                                   age_dv_grp, n, d, euro_std_pop) %>% 
                     arrange(sample_grp, outcome_lab, class_mem, sex_dv, age_dv_grp))


## convert numerator and denominator to integers
svy_df_list <- map(svy_df_list, ~(.x %>%
                                    mutate(n = as.integer(n),
                                           d = as.integer(d))))

## convert zero denominators to 1 so that division possible
svy_df_list <- map(svy_df_list, ~(.x %>% mutate(d = ifelse(d==0,1,d))))


## back to single df -- keep for now, might not need
#svy_df <- bind_rows(svy_df_list)

#### calculate standardised rates -----------------------

## calculate for females and meals separately
svy_dsr_list <- map(svy_df_list, 
                    ~(.x %>%
                        group_by(outcome_lab, sample_grp, class_mem, sex_dv) %>%
                        PHEindicatormethods::phe_dsr(
                          x = n,                 # column with observed number of events
                          n = d,                 # column with non-standard pops for each stratum
                          stdpop = euro_std_pop, # standard populations for each stratum
                          stdpoptype = "field",  # either "vector" for a standalone vector or "field" meaning std populations are in the data  
                          multiplier = 100) %>%       # set to per 100 pop
                        ungroup()))

# convert back into singl df
svy_dsr_df <- bind_rows(svy_dsr_list)  

## calculate age-sex standardised rates by averaging male and female rates
temp_list <- map(svy_dsr_list, 
                 ~(.x %>% 
                     group_by(outcome_lab, sample_grp, class_mem) %>% 
                     summarise(total_count = sum(total_count),
                               total_pop = sum(total_pop),
                               value = mean(value),
                               lowercl = mean(lowercl),
                               uppercl = mean(uppercl),
                               confidence = first(confidence),
                               statistic = first(statistic),
                               method = first(method)) %>% 
                     mutate(sex_dv = "both") %>% 
                     ungroup() %>% 
                     dplyr::select(c(outcome_lab, sample_grp, class_mem, sex_dv, everything()))))


temp_df <- bind_rows(temp_list)  

### bind age-sex dsr df onto main df
svy_dsr_df3 <- svy_dsr_df %>% bind_rows(temp_df) %>% 
  arrange(outcome_lab, sample_grp,class_mem, sex_dv)

rm(temp_list, temp_df)

### remove objects except for the three dsr df's and class df's
rm(list = ls()[!(ls() %in% c('svy_dsr_df1','svy_dsr_df2','svy_dsr_df3',
                             "dfas1a_end_class1","dfas1b_end_class1",
                             "dfas1a_end_class2", "dfas1b_end_class2",
                             "dfas1a_end_class3", "dfas1b_end_class3"))])



################################################################################
#####                               plots                                  #####
################################################################################

svy_dsr_df1 <- svy_dsr_df1 %>% 
  mutate(exp_flag = factor(ifelse(class_mem=="non-permanent employment",1,
                                  ifelse(class_mem=="permanent employment",2,
                                         0))))

svy_dsr_df2 <- svy_dsr_df2 %>% 
  mutate(exp_flag = factor(ifelse(class_mem=="broken employment",1,
                                  ifelse(class_mem=="unbroken employment",2,
                                         0))))

svy_dsr_df3 <- svy_dsr_df3 %>% 
  mutate(exp_flag = factor(ifelse(class_mem=="multiple employment",1,
                                  ifelse(class_mem=="single employment",2,
                                         0))))

plotter <-  function(data, outcome, x=class_mem, y=value){ 
  temp <- data %>% 
  filter(outcome_lab == outcome &
           sex_dv == "both") %>%
    mutate(class_mem=tidytext::reorder_within(class_mem,value,sample_grp))
  
  ggplot(temp,aes({{x}}, {{y}},
             fill=exp_flag)) +
  geom_col(show.legend = FALSE) +
  geom_errorbar(aes(ymin=lowercl, ymax=uppercl), colour="black", width=.1) +
  coord_flip() +
  theme_bw() +
  tidytext::scale_x_reordered() +
  scale_fill_manual(name = "exp_flag", values=c("grey50","red","green")) +
  facet_wrap(~sample_grp, ncol = 1, scales = "free_y")
}

#### employment contract -------------------------------------------------------

### poor self-rated health
plotter(data = svy_dsr_df1,
        outcome = "poor self-rated health")

### poor mental health
plotter(data = svy_dsr_df1,
        outcome = "poor mental health")


#### broken employment ---------------------------------------------------------

### poor self-rated health
plotter(data = svy_dsr_df2,
        outcome = "poor self-rated health")

### poor mental health
plotter(data = svy_dsr_df2,
        outcome = "poor mental health")

#### multiple employment -------------------------------------------------------

### poor self-rated health
plotter(data = svy_dsr_df3,
        outcome = "poor self-rated health")

### poor mental health
plotter(data = svy_dsr_df3,
        outcome = "poor mental health")



################################################################################
#####                    logistic regression - data prep                   #####
################################################################################

#### prepare data --------------------------------------------------------------

### employment contract
dfas1a_end_class1 <- dfas1a_end_class1 %>% 
    mutate(srh_bin = ifelse(srh_bin == "good/fair/poor",1,0),
           ghq_case3 = ifelse(ghq_case3 == "3 or more",1,0),
           emp_contract_class = factor(emp_contract_class))


dfas1b_end_class1 <- dfas1b_end_class1 %>% 
  mutate(srh_bin = ifelse(srh_bin == "good/fair/poor",1,0),
         ghq_case3 = ifelse(ghq_case3 == "3 or more",1,0),
         emp_contract_class = factor(emp_contract_class))


### employment contract
dfas1a_end_class2 <- dfas1a_end_class2 %>% 
  mutate(srh_bin = ifelse(srh_bin == "good/fair/poor",1,0),
         ghq_case3 = ifelse(ghq_case3 == "3 or more",1,0),
         emp_spells_class = factor(emp_spells_class))


dfas1b_end_class2 <- dfas1b_end_class2 %>% 
  mutate(srh_bin = ifelse(srh_bin == "good/fair/poor",1,0),
         ghq_case3 = ifelse(ghq_case3 == "3 or more",1,0),
         emp_spells_class = factor(emp_spells_class))

### multiple employment
dfas1a_end_class3 <- dfas1a_end_class3 %>% 
  mutate(srh_bin = ifelse(srh_bin == "good/fair/poor",1,0),
         ghq_case3 = ifelse(ghq_case3 == "3 or more",1,0),
         multi_emp_class = factor(multi_emp_class))


dfas1b_end_class3 <- dfas1b_end_class3 %>% 
  mutate(srh_bin = ifelse(srh_bin == "good/fair/poor",1,0),
         ghq_case3 = ifelse(ghq_case3 == "3 or more",1,0),
         multi_emp_class = factor(multi_emp_class))

#### create weighted samples ---------------------------------------------------

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
#####                logistic regression - self-rated health               #####
################################################################################

#### employment contract -------------------------------------------------------

### sample A ------------------

## logistic regression
svy_srh_emp_contract_glm_a <- svyglm(srh_bin~relevel(emp_contract_class, ref = 4)+age_dv_grp+sex_dv, 
               family=quasibinomial, design=svy_emp_contract_a, 
               na.action = na.omit)

## model summary
svy_srh_emp_contract_glm_a_summary <- summary(svy_srh_emp_contract_glm_a)

## coefficients dataframe
svy_srh_emp_contract_glm_a_df <- data.frame(svy_srh_emp_contract_glm_a$coefficients)

# exponentiate to get ORs
svy_srh_emp_contract_glm_a_df <- svy_srh_emp_contract_glm_a_df %>% 
  mutate(est = exp(svy_srh_emp_contract_glm_a.coefficients)) %>% 
  dplyr::select(est) 

# add in row names 
svy_srh_emp_contract_glm_a_df <- cbind(rownames(svy_srh_emp_contract_glm_a_df),svy_srh_emp_contract_glm_a_df, row.names=NULL)

svy_srh_emp_contract_glm_a_df <- svy_srh_emp_contract_glm_a_df %>% 
  rename(measure = `rownames(svy_srh_emp_contract_glm_a_df)`)

## confidence intervals
svy_srh_emp_contract_glm_a_ci <- data.frame(confint(svy_srh_emp_contract_glm_a)) %>% 
  rename(lci = X2.5..,
         uci = X97.5..) %>% 
  mutate(lci = exp(lci),
         uci = exp(uci))

# add in row names
svy_srh_emp_contract_glm_a_ci <- cbind(rownames(svy_srh_emp_contract_glm_a_ci),svy_srh_emp_contract_glm_a_ci, row.names=NULL)

svy_srh_emp_contract_glm_a_ci <- svy_srh_emp_contract_glm_a_ci %>% 
  rename(measure = `rownames(svy_srh_emp_contract_glm_a_ci)`)

## join dfs together
svy_srh_emp_contract_glm_a_df <- svy_srh_emp_contract_glm_a_df %>% 
  left_join(svy_srh_emp_contract_glm_a_ci) %>% 
  mutate(measure = str_remove(measure, "relevel\\(emp_contract_class, ref = 4\\)"))

### sample B -------------------

## logistic regression
svy_srh_emp_contract_glm_b <- svyglm(srh_bin~relevel(emp_contract_class, ref = 4)+age_dv_grp+sex_dv, 
                                 family=quasibinomial, design=svy_emp_contract_b, 
                                 na.action = na.omit)

## model summary
svy_srh_emp_contract_glm_b_summary <- summary(svy_srh_emp_contract_glm_b)

## coefficients dataframe
svy_srh_emp_contract_glm_b_df <- data.frame(svy_srh_emp_contract_glm_b$coefficients)

# exponentiate to get ORs
svy_srh_emp_contract_glm_b_df <- svy_srh_emp_contract_glm_b_df %>% 
  mutate(est = exp(svy_srh_emp_contract_glm_b.coefficients)) %>% 
  dplyr::select(est) 

# add in row names 
svy_srh_emp_contract_glm_b_df <- cbind(rownames(svy_srh_emp_contract_glm_b_df),svy_srh_emp_contract_glm_b_df, row.names=NULL)

svy_srh_emp_contract_glm_b_df <- svy_srh_emp_contract_glm_b_df %>% 
  rename(measure = `rownames(svy_srh_emp_contract_glm_b_df)`)

## confidence intervals
svy_srh_emp_contract_glm_b_ci <- data.frame(confint(svy_srh_emp_contract_glm_b)) %>% 
  rename(lci = X2.5..,
         uci = X97.5..) %>% 
  mutate(lci = exp(lci),
         uci = exp(uci))

# add in row names
svy_srh_emp_contract_glm_b_ci <- cbind(rownames(svy_srh_emp_contract_glm_b_ci),svy_srh_emp_contract_glm_b_ci, row.names=NULL)

svy_srh_emp_contract_glm_b_ci <- svy_srh_emp_contract_glm_b_ci %>% 
  rename(measure = `rownames(svy_srh_emp_contract_glm_b_ci)`)

## join dfs together
svy_srh_emp_contract_glm_b_df <- svy_srh_emp_contract_glm_b_df %>% 
  left_join(svy_srh_emp_contract_glm_b_ci) %>% 
  mutate(measure = str_remove(measure, "relevel\\(emp_contract_class, ref = 4\\)"))


#### employment spells ---------------------------------------------------------

### sample A ----------------------
## logistic regression
svy_srh_emp_spells_glm_a <- svyglm(srh_bin~relevel(emp_spells_class, ref = 2)+age_dv_grp+sex_dv, 
                                 family=quasibinomial, design=svy_broken_emp_a, 
                                 na.action = na.omit)

## model summary
svy_srh_emp_spells_glm_a_summary <- summary(svy_srh_emp_spells_glm_a)

## coefficients dataframe
svy_srh_emp_spells_glm_a_df <- data.frame(svy_srh_emp_spells_glm_a$coefficients)

# exponentiate to get ORs
svy_srh_emp_spells_glm_a_df <- svy_srh_emp_spells_glm_a_df %>% 
  mutate(est = exp(svy_srh_emp_spells_glm_a.coefficients)) %>% 
  dplyr::select(est) 

# add in row names 
svy_srh_emp_spells_glm_a_df <- cbind(rownames(svy_srh_emp_spells_glm_a_df),svy_srh_emp_spells_glm_a_df, row.names=NULL)

svy_srh_emp_spells_glm_a_df <- svy_srh_emp_spells_glm_a_df %>% 
  rename(measure = `rownames(svy_srh_emp_spells_glm_a_df)`)

## confidence intervals
svy_srh_emp_spells_glm_a_ci <- data.frame(confint(svy_srh_emp_spells_glm_a)) %>% 
  rename(lci = X2.5..,
         uci = X97.5..) %>% 
  mutate(lci = exp(lci),
         uci = exp(uci))

# add in row names
svy_srh_emp_spells_glm_a_ci <- cbind(rownames(svy_srh_emp_spells_glm_a_ci),svy_srh_emp_spells_glm_a_ci, row.names=NULL)

svy_srh_emp_spells_glm_a_ci <- svy_srh_emp_spells_glm_a_ci %>% 
  rename(measure = `rownames(svy_srh_emp_spells_glm_a_ci)`)

## join dfs together
svy_srh_emp_spells_glm_a_df <- svy_srh_emp_spells_glm_a_df %>% 
  left_join(svy_srh_emp_spells_glm_a_ci) %>% 
  mutate(measure = str_remove(measure, "relevel\\(emp_spells_class, ref = 2\\)"))

### sample B ----------------------
## logistic regression
svy_srh_emp_spells_glm_b <- svyglm(srh_bin~relevel(emp_spells_class, ref = 2)+age_dv_grp+sex_dv, 
                                 family=quasibinomial, design=svy_broken_emp_b, 
                                 na.action = na.omit)

## model summary
svy_srh_emp_spells_glm_b_summary <- summary(svy_srh_emp_spells_glm_b)

## coefficients dataframe
svy_srh_emp_spells_glm_b_df <- data.frame(svy_srh_emp_spells_glm_b$coefficients)

# exponentiate to get ORs
svy_srh_emp_spells_glm_b_df <- svy_srh_emp_spells_glm_b_df %>% 
  mutate(est = exp(svy_srh_emp_spells_glm_b.coefficients)) %>% 
  dplyr::select(est) 

# add in row names 
svy_srh_emp_spells_glm_b_df <- cbind(rownames(svy_srh_emp_spells_glm_b_df),svy_srh_emp_spells_glm_b_df, row.names=NULL)

svy_srh_emp_spells_glm_b_df <- svy_srh_emp_spells_glm_b_df %>% 
  rename(measure = `rownames(svy_srh_emp_spells_glm_b_df)`)

## confidence intervals
svy_srh_emp_spells_glm_b_ci <- data.frame(confint(svy_srh_emp_spells_glm_b)) %>% 
  rename(lci = X2.5..,
         uci = X97.5..) %>% 
  mutate(lci = exp(lci),
         uci = exp(uci))

# add in row names
svy_srh_emp_spells_glm_b_ci <- cbind(rownames(svy_srh_emp_spells_glm_b_ci),svy_srh_emp_spells_glm_b_ci, row.names=NULL)

svy_srh_emp_spells_glm_b_ci <- svy_srh_emp_spells_glm_b_ci %>% 
  rename(measure = `rownames(svy_srh_emp_spells_glm_b_ci)`)

## join dfs together
svy_srh_emp_spells_glm_b_df <- svy_srh_emp_spells_glm_b_df %>% 
  left_join(svy_srh_emp_spells_glm_b_ci) %>% 
  mutate(measure = str_remove(measure, "relevel\\(emp_spells_class, ref = 2\\)"))


#### multiple employment -------------------------------------------------------

### sample A -----------------------
## logistic regression
svy_srh_multi_emp_glm_a <- svyglm(srh_bin~relevel(multi_emp_class, ref = 4)+age_dv_grp+sex_dv, 
                                 family=quasibinomial, design=svy_multi_emp_a, 
                                 na.action = na.omit)

## model summary
svy_srh_multi_emp_glm_a_summary <- summary(svy_srh_multi_emp_glm_a)

## coefficients dataframe
svy_srh_multi_emp_glm_a_df <- data.frame(svy_srh_multi_emp_glm_a$coefficients)

# exponentiate to get ORs
svy_srh_multi_emp_glm_a_df <- svy_srh_multi_emp_glm_a_df %>% 
  mutate(est = exp(svy_srh_multi_emp_glm_a.coefficients)) %>% 
  dplyr::select(est) 

# add in row names 
svy_srh_multi_emp_glm_a_df <- cbind(rownames(svy_srh_multi_emp_glm_a_df),svy_srh_multi_emp_glm_a_df, row.names=NULL)

svy_srh_multi_emp_glm_a_df <- svy_srh_multi_emp_glm_a_df %>% 
  rename(measure = `rownames(svy_srh_multi_emp_glm_a_df)`)

## confidence intervals
svy_srh_multi_emp_glm_a_ci <- data.frame(confint(svy_srh_multi_emp_glm_a)) %>% 
  rename(lci = X2.5..,
         uci = X97.5..) %>% 
  mutate(lci = exp(lci),
         uci = exp(uci))

# add in row names
svy_srh_multi_emp_glm_a_ci <- cbind(rownames(svy_srh_multi_emp_glm_a_ci),svy_srh_multi_emp_glm_a_ci, row.names=NULL)

svy_srh_multi_emp_glm_a_ci <- svy_srh_multi_emp_glm_a_ci %>% 
  rename(measure = `rownames(svy_srh_multi_emp_glm_a_ci)`)

## join dfs together
svy_srh_multi_emp_glm_a_df <- svy_srh_multi_emp_glm_a_df %>% 
  left_join(svy_srh_multi_emp_glm_a_ci) %>% 
  mutate(measure = str_remove(measure, "relevel\\(multi_emp_class, ref = 4\\)"))

### sample B ----------------------
## logistic regression
svy_srh_multi_emp_glm_b <- svyglm(srh_bin~relevel(multi_emp_class, ref = 4)+age_dv_grp+sex_dv, 
                                 family=quasibinomial, design=svy_multi_emp_b, 
                                 na.action = na.omit)

## model summary
svy_srh_multi_emp_glm_b_summary <- summary(svy_srh_multi_emp_glm_b)

## coefficients dataframe
svy_srh_multi_emp_glm_b_df <- data.frame(svy_srh_multi_emp_glm_b$coefficients)

# exponentiate to get ORs
svy_srh_multi_emp_glm_b_df <- svy_srh_multi_emp_glm_b_df %>% 
  mutate(est = exp(svy_srh_multi_emp_glm_b.coefficients)) %>% 
  dplyr::select(est) 

# add in row names 
svy_srh_multi_emp_glm_b_df <- cbind(rownames(svy_srh_multi_emp_glm_b_df),svy_srh_multi_emp_glm_b_df, row.names=NULL)

svy_srh_multi_emp_glm_b_df <- svy_srh_multi_emp_glm_b_df %>% 
  rename(measure = `rownames(svy_srh_multi_emp_glm_b_df)`)

## confidence intervals
svy_srh_multi_emp_glm_b_ci <- data.frame(confint(svy_srh_multi_emp_glm_b)) %>% 
  rename(lci = X2.5..,
         uci = X97.5..) %>% 
  mutate(lci = exp(lci),
         uci = exp(uci))

# add in row names
svy_srh_multi_emp_glm_b_ci <- cbind(rownames(svy_srh_multi_emp_glm_b_ci),svy_srh_multi_emp_glm_b_ci, row.names=NULL)

svy_srh_multi_emp_glm_b_ci <- svy_srh_multi_emp_glm_b_ci %>% 
  rename(measure = `rownames(svy_srh_multi_emp_glm_b_ci)`)

## join dfs together
svy_srh_multi_emp_glm_b_df <- svy_srh_multi_emp_glm_b_df %>% 
  left_join(svy_srh_multi_emp_glm_b_ci) %>% 
  mutate(measure = str_remove(measure, "relevel\\(multi_emp_class, ref = 4\\)"))



################################################################################
#####        logistic regression - common mental health disorders          #####
################################################################################

#### employment contract -------------------------------------------------------

### sample A ------------------

## logistic regression
svy_ghq_emp_contract_glm_a <- svyglm(ghq_case3~relevel(emp_contract_class, ref = 4)+age_dv_grp+sex_dv, 
                                 family=quasibinomial, design=svy_emp_contract_a, 
                                 na.action = na.omit)

## model summary
svy_ghq_emp_contract_glm_a_summary <- summary(svy_ghq_emp_contract_glm_a)

## coefficients dataframe
svy_ghq_emp_contract_glm_a_df <- data.frame(svy_ghq_emp_contract_glm_a$coefficients)

# exponentiate to get ORs
svy_ghq_emp_contract_glm_a_df <- svy_ghq_emp_contract_glm_a_df %>% 
  mutate(est = exp(svy_ghq_emp_contract_glm_a.coefficients)) %>% 
  dplyr::select(est) 

# add in row names 
svy_ghq_emp_contract_glm_a_df <- cbind(rownames(svy_ghq_emp_contract_glm_a_df),svy_ghq_emp_contract_glm_a_df, row.names=NULL)

svy_ghq_emp_contract_glm_a_df <- svy_ghq_emp_contract_glm_a_df %>% 
  rename(measure = `rownames(svy_ghq_emp_contract_glm_a_df)`)

## confidence intervals
svy_ghq_emp_contract_glm_a_ci <- data.frame(confint(svy_ghq_emp_contract_glm_a)) %>% 
  rename(lci = X2.5..,
         uci = X97.5..) %>% 
  mutate(lci = exp(lci),
         uci = exp(uci))

# add in row names
svy_ghq_emp_contract_glm_a_ci <- cbind(rownames(svy_ghq_emp_contract_glm_a_ci),svy_ghq_emp_contract_glm_a_ci, row.names=NULL)

svy_ghq_emp_contract_glm_a_ci <- svy_ghq_emp_contract_glm_a_ci %>% 
  rename(measure = `rownames(svy_ghq_emp_contract_glm_a_ci)`)

## join dfs together
svy_ghq_emp_contract_glm_a_df <- svy_ghq_emp_contract_glm_a_df %>% 
  left_join(svy_ghq_emp_contract_glm_a_ci) %>% 
  mutate(measure = str_remove(measure, "relevel\\(emp_contract_class, ref = 4\\)"))

### sample B -------------------

## logistic regression
svy_ghq_emp_contract_glm_b <- svyglm(ghq_case3~relevel(emp_contract_class, ref = 4)+age_dv_grp+sex_dv, 
                                 family=quasibinomial, design=svy_emp_contract_b, 
                                 na.action = na.omit)

## model summary
svy_ghq_emp_contract_glm_b_summary <- summary(svy_ghq_emp_contract_glm_b)

## coefficients dataframe
svy_ghq_emp_contract_glm_b_df <- data.frame(svy_ghq_emp_contract_glm_b$coefficients)

# exponentiate to get ORs
svy_ghq_emp_contract_glm_b_df <- svy_ghq_emp_contract_glm_b_df %>% 
  mutate(est = exp(svy_ghq_emp_contract_glm_b.coefficients)) %>% 
  dplyr::select(est) 

# add in row names 
svy_ghq_emp_contract_glm_b_df <- cbind(rownames(svy_ghq_emp_contract_glm_b_df),svy_ghq_emp_contract_glm_b_df, row.names=NULL)

svy_ghq_emp_contract_glm_b_df <- svy_ghq_emp_contract_glm_b_df %>% 
  rename(measure = `rownames(svy_ghq_emp_contract_glm_b_df)`)

## confidence intervals
svy_ghq_emp_contract_glm_b_ci <- data.frame(confint(svy_ghq_emp_contract_glm_b)) %>% 
  rename(lci = X2.5..,
         uci = X97.5..) %>% 
  mutate(lci = exp(lci),
         uci = exp(uci))

# add in row names
svy_ghq_emp_contract_glm_b_ci <- cbind(rownames(svy_ghq_emp_contract_glm_b_ci),svy_ghq_emp_contract_glm_b_ci, row.names=NULL)

svy_ghq_emp_contract_glm_b_ci <- svy_ghq_emp_contract_glm_b_ci %>% 
  rename(measure = `rownames(svy_ghq_emp_contract_glm_b_ci)`)

## join dfs together
svy_ghq_emp_contract_glm_b_df <- svy_ghq_emp_contract_glm_b_df %>% 
  left_join(svy_ghq_emp_contract_glm_b_ci) %>% 
  mutate(measure = str_remove(measure, "relevel\\(emp_contract_class, ref = 4\\)"))

#### employment spells ---------------------------------------------------------

### sample A ----------------------
## logistic regression
svy_ghq_emp_spells_glm_a <- svyglm(ghq_case3~relevel(emp_spells_class, ref = 2)+age_dv_grp+sex_dv, 
                               family=quasibinomial, design=svy_broken_emp_a, 
                               na.action = na.omit)

## model summary
svy_ghq_emp_spells_glm_a_summary <- summary(svy_ghq_emp_spells_glm_a)

## coefficients dataframe
svy_ghq_emp_spells_glm_a_df <- data.frame(svy_ghq_emp_spells_glm_a$coefficients)

# exponentiate to get ORs
svy_ghq_emp_spells_glm_a_df <- svy_ghq_emp_spells_glm_a_df %>% 
  mutate(est = exp(svy_ghq_emp_spells_glm_a.coefficients)) %>% 
  dplyr::select(est) 

# add in row names 
svy_ghq_emp_spells_glm_a_df <- cbind(rownames(svy_ghq_emp_spells_glm_a_df),svy_ghq_emp_spells_glm_a_df, row.names=NULL)

svy_ghq_emp_spells_glm_a_df <- svy_ghq_emp_spells_glm_a_df %>% 
  rename(measure = `rownames(svy_ghq_emp_spells_glm_a_df)`)

## confidence intervals
svy_ghq_emp_spells_glm_a_ci <- data.frame(confint(svy_ghq_emp_spells_glm_a)) %>% 
  rename(lci = X2.5..,
         uci = X97.5..) %>% 
  mutate(lci = exp(lci),
         uci = exp(uci))

# add in row names
svy_ghq_emp_spells_glm_a_ci <- cbind(rownames(svy_ghq_emp_spells_glm_a_ci),svy_ghq_emp_spells_glm_a_ci, row.names=NULL)

svy_ghq_emp_spells_glm_a_ci <- svy_ghq_emp_spells_glm_a_ci %>% 
  rename(measure = `rownames(svy_ghq_emp_spells_glm_a_ci)`)

## join dfs together
svy_ghq_emp_spells_glm_a_df <- svy_ghq_emp_spells_glm_a_df %>% 
  left_join(svy_ghq_emp_spells_glm_a_ci) %>% 
  mutate(measure = str_remove(measure, "relevel\\(emp_spells_class, ref = 2\\)"))

### sample B ----------------------
## logistic regression
svy_ghq_emp_spells_glm_b <- svyglm(ghq_case3~relevel(emp_spells_class, ref = 2)+age_dv_grp+sex_dv, 
                               family=quasibinomial, design=svy_broken_emp_b, 
                               na.action = na.omit)

## model summary
svy_ghq_emp_spells_glm_b_summary <- summary(svy_ghq_emp_spells_glm_b)

## coefficients dataframe
svy_ghq_emp_spells_glm_b_df <- data.frame(svy_ghq_emp_spells_glm_b$coefficients)

# exponentiate to get ORs
svy_ghq_emp_spells_glm_b_df <- svy_ghq_emp_spells_glm_b_df %>% 
  mutate(est = exp(svy_ghq_emp_spells_glm_b.coefficients)) %>% 
  dplyr::select(est) 

# add in row names 
svy_ghq_emp_spells_glm_b_df <- cbind(rownames(svy_ghq_emp_spells_glm_b_df),svy_ghq_emp_spells_glm_b_df, row.names=NULL)

svy_ghq_emp_spells_glm_b_df <- svy_ghq_emp_spells_glm_b_df %>% 
  rename(measure = `rownames(svy_ghq_emp_spells_glm_b_df)`)

## confidence intervals
svy_ghq_emp_spells_glm_b_ci <- data.frame(confint(svy_ghq_emp_spells_glm_b)) %>% 
  rename(lci = X2.5..,
         uci = X97.5..) %>% 
  mutate(lci = exp(lci),
         uci = exp(uci))

# add in row names
svy_ghq_emp_spells_glm_b_ci <- cbind(rownames(svy_ghq_emp_spells_glm_b_ci),svy_ghq_emp_spells_glm_b_ci, row.names=NULL)

svy_ghq_emp_spells_glm_b_ci <- svy_ghq_emp_spells_glm_b_ci %>% 
  rename(measure = `rownames(svy_ghq_emp_spells_glm_b_ci)`)

## join dfs together
svy_ghq_emp_spells_glm_b_df <- svy_ghq_emp_spells_glm_b_df %>% 
  left_join(svy_ghq_emp_spells_glm_b_ci) %>% 
  mutate(measure = str_remove(measure, "relevel\\(emp_spells_class, ref = 2\\)"))


#### multiple employment -------------------------------------------------------

### sample A -----------------------
## logistic regression
svy_ghq_multi_emp_glm_a <- svyglm(ghq_case3~relevel(multi_emp_class, ref = 4)+age_dv_grp+sex_dv, 
                              family=quasibinomial, design=svy_multi_emp_a, 
                              na.action = na.omit)

## model summary
svy_ghq_multi_emp_glm_a_summary <- summary(svy_ghq_multi_emp_glm_a)

## coefficients dataframe
svy_ghq_multi_emp_glm_a_df <- data.frame(svy_ghq_multi_emp_glm_a$coefficients)

# exponentiate to get ORs
svy_ghq_multi_emp_glm_a_df <- svy_ghq_multi_emp_glm_a_df %>% 
  mutate(est = exp(svy_ghq_multi_emp_glm_a.coefficients)) %>% 
  dplyr::select(est) 

# add in row names 
svy_ghq_multi_emp_glm_a_df <- cbind(rownames(svy_ghq_multi_emp_glm_a_df),svy_ghq_multi_emp_glm_a_df, row.names=NULL)

svy_ghq_multi_emp_glm_a_df <- svy_ghq_multi_emp_glm_a_df %>% 
  rename(measure = `rownames(svy_ghq_multi_emp_glm_a_df)`)

## confidence intervals
svy_ghq_multi_emp_glm_a_ci <- data.frame(confint(svy_ghq_multi_emp_glm_a)) %>% 
  rename(lci = X2.5..,
         uci = X97.5..) %>% 
  mutate(lci = exp(lci),
         uci = exp(uci))

# add in row names
svy_ghq_multi_emp_glm_a_ci <- cbind(rownames(svy_ghq_multi_emp_glm_a_ci),svy_ghq_multi_emp_glm_a_ci, row.names=NULL)

svy_ghq_multi_emp_glm_a_ci <- svy_ghq_multi_emp_glm_a_ci %>% 
  rename(measure = `rownames(svy_ghq_multi_emp_glm_a_ci)`)

## join dfs together
svy_ghq_multi_emp_glm_a_df <- svy_ghq_multi_emp_glm_a_df %>% 
  left_join(svy_ghq_multi_emp_glm_a_ci) %>% 
  mutate(measure = str_remove(measure, "relevel\\(multi_emp_class, ref = 4\\)"))

### sample B ----------------------
## logistic regression
svy_ghq_multi_emp_glm_b <- svyglm(ghq_case3~relevel(multi_emp_class, ref = 4)+age_dv_grp+sex_dv, 
                              family=quasibinomial, design=svy_multi_emp_b, 
                              na.action = na.omit)

## model summary
svy_ghq_multi_emp_glm_b_summary <- summary(svy_ghq_multi_emp_glm_b)

## coefficients dataframe
svy_ghq_multi_emp_glm_b_df <- data.frame(svy_ghq_multi_emp_glm_b$coefficients)

# exponentiate to get ORs
svy_ghq_multi_emp_glm_b_df <- svy_ghq_multi_emp_glm_b_df %>% 
  mutate(est = exp(svy_ghq_multi_emp_glm_b.coefficients)) %>% 
  dplyr::select(est) 

# add in row names 
svy_ghq_multi_emp_glm_b_df <- cbind(rownames(svy_ghq_multi_emp_glm_b_df),svy_ghq_multi_emp_glm_b_df, row.names=NULL)

svy_ghq_multi_emp_glm_b_df <- svy_ghq_multi_emp_glm_b_df %>% 
  rename(measure = `rownames(svy_ghq_multi_emp_glm_b_df)`)

## confidence intervals
svy_ghq_multi_emp_glm_b_ci <- data.frame(confint(svy_ghq_multi_emp_glm_b)) %>% 
  rename(lci = X2.5..,
         uci = X97.5..) %>% 
  mutate(lci = exp(lci),
         uci = exp(uci))

# add in row names
svy_ghq_multi_emp_glm_b_ci <- cbind(rownames(svy_ghq_multi_emp_glm_b_ci),svy_ghq_multi_emp_glm_b_ci, row.names=NULL)

svy_ghq_multi_emp_glm_b_ci <- svy_ghq_multi_emp_glm_b_ci %>% 
  rename(measure = `rownames(svy_ghq_multi_emp_glm_b_ci)`)

## join dfs together
svy_ghq_multi_emp_glm_b_df <- svy_ghq_multi_emp_glm_b_df %>% 
  left_join(svy_ghq_multi_emp_glm_b_ci) %>% 
  mutate(measure = str_remove(measure, "relevel\\(multi_emp_class, ref = 4\\)"))

################################################################################
#####                       Logistic regression plots                      #####
################################################################################

#### define function for plots -------------------------------------------------
plotter2 <- function(data, classes, y_lab, title_lab){
data %>% 
  filter(measure %in% classes) %>% 
  ggplot(aes(x = est, y = measure)) + 
  geom_point() +
  geom_vline(xintercept=1, linetype="dotted") +
  geom_errorbar(aes(xmin=lci, xmax=uci), colour="black", width=.1) +
  theme_bw() +
  scale_x_log10(breaks = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10),
                minor_breaks = NULL) +
  ggtitle(title_lab) +
  labs(y = y_lab, x = "Odds ratio")

}

#### employment contract - self-rated health -----------------------------------

### sample A
tiff("./output/weighted/emp_contract_srh_OR_grouped_a.tiff")
plotter2(data = svy_srh_emp_contract_glm_a_df, 
         classes = c("into employment", "non-permanent employment", 
                     "out of employment", "unemployed"),
         y_lab = "Employment contract class",
         title_lab = "Poor self-rated health by employment contract class\n(Sample A)")
dev.off()

### sample B
tiff("./output/weighted/emp_contract_srh_OR_grouped_b.tiff")
plotter2(data = svy_srh_emp_contract_glm_b_df, 
         classes = c("into employment", "non-permanent employment", 
                     "out of employment", "unemployed"),
         y_lab = "Employment contract class",
         title_lab = "Poor self-rated health by employment contract class\n(Sample B)")
dev.off()

#### employment contract - mental health -----------------------------------

### sample A
tiff("./output/weighted/emp_contract_ghq_OR_grouped_a.tiff")
plotter2(data = svy_ghq_emp_contract_glm_a_df, 
         classes = c("into employment", "non-permanent employment", 
                     "out of employment", "unemployed"),
         y_lab = "Employment contract class",
         title_lab = "Common mental health conditon by employment contract\nclass (Sample A)")
dev.off()

### sample B
tiff("./output/weighted/emp_contract_ghq_OR_grouped_b.tiff")
plotter2(data = svy_ghq_emp_contract_glm_b_df, 
         classes = c("into employment", "non-permanent employment", 
                     "out of employment", "unemployed"),
         y_lab = "Employment contract class",
         title_lab = "Common mental health conditon by employment contract\nclass (Sample B)")
dev.off()

#### employment spells - self-rated health -----------------------------------

### sample A
tiff("./output/weighted/emp_spells_srh_OR_grouped_a.tiff")
plotter2(data = svy_srh_emp_spells_glm_a_df, 
         classes = c("broken employment", "unemployed"),
         y_lab = "Employment spells class",
         title_lab = "Poor self-rated health  by employment spells class\n(Sample A)")
dev.off()

### sample B
tiff("./output/weighted/emp_spells_srh_OR_grouped_b.tiff")
plotter2(data = svy_srh_emp_spells_glm_b_df, 
         classes = c("broken employment", "unemployed"),
         y_lab = "Employment spells class",
         title_lab = "Poor self-rated health  by employment spells class\n(Sample B)")
dev.off()


#### employment spells - mental health -----------------------------------------

### sample A
tiff("./output/weighted/emp_spells_ghq_OR_grouped_a.tiff")
plotter2(data = svy_ghq_emp_spells_glm_a_df, 
         classes = c("broken employment", "unemployed"),
         y_lab = "Employment spells class",
         title_lab = "Common mental health conditon by employment spells\nclass (Sample A)")
dev.off()

### sample B
tiff("./output/weighted/emp_spells_ghq_OR_grouped_b.tiff")
plotter2(data = svy_ghq_emp_spells_glm_b_df, 
         classes = c("broken employment", "unemployed"),
         y_lab = "Employment spells class",
         title_lab = "Common mental health conditon by employment spells\nclass (Sample B)")
dev.off()


#### multiple employment - self-rated health -----------------------------------

### sample A
tiff("./output/weighted/multi_emp_srh_OR_grouped_a.tiff")
plotter2(data = svy_srh_multi_emp_glm_a_df, 
         classes = c("into employment", "multiple employment", 
                     "out of employment", "unemployed"),
         y_lab = "Multiple employment class",
         title_lab = "Poor self-rated health by multiple employment class\n(Sample A)")
dev.off()

### sample B
tiff("./output/weighted/multi_emp_srh_OR_grouped_b.tiff")
plotter2(data = svy_srh_multi_emp_glm_b_df, 
         classes = c("into employment", "multiple employment", 
                     "out of employment", "unemployed"),
         y_lab = "Multiple employment class",
         title_lab = "Poor self-rated health by multiple employment class\n(Sample B)")
dev.off()


#### multiple employment - mental health -----------------------------------

### sample A
tiff("./output/weighted/multi_emp_ghq_OR_grouped_a.tiff")
plotter2(data = svy_ghq_multi_emp_glm_a_df, 
         classes = c("into employment", "multiple employment", 
                     "out of employment", "unemployed"),
         y_lab = "Employment contract class",
         title_lab = "Common mental health conditon by multiple employment class (Sample A)")
dev.off()

### sample B
tiff("./output/weighted/multi_emp_ghq_OR_grouped_b.tiff")
plotter2(data = svy_ghq_multi_emp_glm_b_df, 
         classes = c("into employment", "multiple employment", 
                     "out of employment", "unemployed"),
         y_lab = "Employment contract class",
         title_lab = "Common mental health conditon by multiple employment\nclass (Sample B)")
dev.off()





#### employment contract -------------------------------------------------------

### self-rated health ---------

### sample A ------------
#
### calculate proportions
#srh1_a <- data.frame(svyby(~srh_bin, ~emp_contract_class,svy_emp_contract_a, svymean, na.rm=TRUE))
#
#srh1_a_long <- srh1_a %>% pivot_longer(2:3,names_to = "measure",values_to = "est") %>% 
#  rename("se" = "se.srh_binexcellent.very.good") %>% 
#  dplyr::select(-se.srh_bingood.fair.poor)
#
### calculate totals
#srh12_a <- data.frame(svyby(~srh_bin, ~emp_contract_class,svy_emp_contract_a, svytotal, na.rm=TRUE))
#
#srh12_a_long <- srh12_a %>% pivot_longer(2:3,names_to = "measure",values_to = "total") %>% 
#  dplyr::select(-c(se.srh_binexcellent.very.good, se.srh_bingood.fair.poor))
#
##join
#srh1_a_long <- srh1_a_long %>% left_join(srh12_a_long)
#
#### get rid of junk text in measure strings
#srh1_a_long$emp_contract_class <- str_to_title(srh1_a_long$emp_contract_class)
#srh1_a_long$measure <- str_replace(srh1_a_long$measure,"srh_bin","")
#srh1_a_long$measure <- str_replace(srh1_a_long$measure,"excellent.very.good","Excellent/very good")
#srh1_a_long$measure <- str_replace(srh1_a_long$measure,"good.fair.poor","Good/fair/poor")
#
### join together and format
#srh_prev_a <- srh1_a_long %>%
#  mutate(est = est*100,
#         var="Self-reported health",
#         wv_n=6) %>% 
#  rename(n=total) %>% 
#  dplyr::select(wv_n, var, emp_contract_class,measure, n, est, se) %>%
#  group_by(var, emp_contract_class) %>% 
#  mutate(d=sum(n),
#         p=n/d,
#         margin = qnorm(0.975)*sqrt(p*(1-p)/d)*100,
#         lci = est-margin,
#         uci = est+margin) %>% 
#  ungroup() %>% 
#  dplyr::select(-c(d,p,margin)) %>% 
#  arrange(wv_n, factor(measure, levels = c("Excellent/very good",
#                                           "Good/fair/poor"))) %>% 
#  arrange(emp_contract_class)
#
#emp_contract_prev <- srh_prev_a
#
#### sample B ------------
#
### calculate proportions
#srh1_b <- data.frame(svyby(~srh_bin, ~emp_contract_class,svy_emp_contract_b, svymean, na.rm=TRUE))
#
#srh1_b_long <- srh1_b %>% pivot_longer(2:3,names_to = "measure",values_to = "est") %>% 
#  rename("se" = "se.srh_binexcellent.very.good") %>% 
#  dplyr::select(-se.srh_bingood.fair.poor)
#
### calculate totals
#srh12_b <- data.frame(svyby(~srh_bin, ~emp_contract_class,svy_emp_contract_b, svytotal, na.rm=TRUE))
#
#srh12_b_long <- srh12_b %>% pivot_longer(2:3,names_to = "measure",values_to = "total") %>% 
#  dplyr::select(-c(se.srh_binexcellent.very.good, se.srh_bingood.fair.poor))
#
##join
#srh1_b_long <- srh1_b_long %>% left_join(srh12_b_long)
#
#### get rid of junk text in measure strings
#srh1_b_long$emp_contract_class <- str_to_title(srh1_b_long$emp_contract_class)
#srh1_b_long$measure <- str_replace(srh1_b_long$measure,"srh_bin","")
#srh1_b_long$measure <- str_replace(srh1_b_long$measure,"excellent.very.good","Excellent/very good")
#srh1_b_long$measure <- str_replace(srh1_b_long$measure,"good.fair.poor","Good/fair/poor")
#
#
#
#
### join together and format
#srh_prev_b <- srh1_b_long %>%
#  mutate(est = est*100,
#         var="Self-reported health",
#         wv_n=10) %>% 
#  rename(n=total) %>% 
#  dplyr::select(wv_n, var, emp_contract_class,measure, n, est, se) %>% 
#  arrange(wv_n, factor(measure, levels = c("Excellent/very good",
#                                           "Good/fair/poor"))) %>%
#  group_by(var, emp_contract_class) %>% 
#  mutate(d=sum(n),
#         p=n/d,
#         margin = qnorm(0.975)*sqrt(p*(1-p)/d)*100,
#         lci = est-margin,
#         uci = est+margin) %>% 
#  ungroup() %>% 
#  dplyr::select(-c(d,p,margin)) %>% 
#  arrange(emp_contract_class)
#
#emp_contract_prev <- emp_contract_prev %>% bind_rows(srh_prev_b)
#
##### SF-12 PCS -----------------------------------------------------------------
#
#### sample A ------------
#
#sf12pcs_a <- data.frame(svyby(~sf12pcs_dv, ~emp_contract_class,svy_emp_contract_a, svymean, na.rm=TRUE))
#
#sf12pcs_a <- sf12pcs_a %>% 
#  rename("est"="sf12pcs_dv") %>% 
#  mutate(wv_n= 6,
#         var = "SF-12 physical component",
#         measure = "Mean",
#         n=NA)
#
#emp_contract_prev <- emp_contract_prev %>% bind_rows(sf12pcs_a)
#
#### sample B ------------
#
#sf12pcs_b <- data.frame(svyby(~sf12pcs_dv, ~emp_contract_class,svy_emp_contract_b, svymean, na.rm=TRUE))
#
#sf12pcs_b <- sf12pcs_b %>% 
#  rename("est"="sf12pcs_dv") %>% 
#  mutate(wv_n= 10,
#         var = "SF-12 physical component",
#         measure = "Mean",
#         n=NA)
#
#emp_contract_prev <- emp_contract_prev %>% bind_rows(sf12pcs_b)
#
##### GHQ-12 --------------------------------------------------------------------
#
#### sample A ------------
#
### calculate proportions
#ghq1_a <- data.frame(svyby(~ghq_case3, ~emp_contract_class,svy_emp_contract_a, svymean, na.rm=TRUE))
#
#ghq1_a_long <- ghq1_a %>% pivot_longer(2:3,names_to = "measure",values_to = "est") %>% 
#  rename("se" = "se.ghq_case30.2") %>% 
#  dplyr::select(-se.ghq_case33.or.more)
#
### calculate totals
#ghq12_a <- data.frame(svyby(~ghq_case3, ~emp_contract_class,svy_emp_contract_a, svytotal, na.rm=TRUE))
#
#ghq12_a_long <- ghq12_a %>% pivot_longer(2:3,names_to = "measure",values_to = "total") %>% 
#  dplyr::select(-c(se.ghq_case30.2, se.ghq_case33.or.more))
#
##join
#ghq1_a_long <- ghq1_a_long %>% left_join(ghq12_a_long)
#
#### get rid of junk text in measure strings
#ghq1_a_long$emp_contract_class <- str_to_title(ghq1_a_long$emp_contract_class)
#ghq1_a_long$measure <- str_replace(ghq1_a_long$measure,"ghq_case3","")
#ghq1_a_long$measure <- str_replace(ghq1_a_long$measure,"0.2","0-2")
#ghq1_a_long$measure <- str_replace(ghq1_a_long$measure,"3.or.more","3 or more")
#
### join together and format
#ghq_prev_a <- ghq1_a_long %>%
#  mutate(est = est*100,
#         var="GHQ-12 caseness",
#         wv_n=6) %>% 
#  rename(n=total) %>% 
#  dplyr::select(wv_n, var, emp_contract_class,measure, n, est, se) %>% 
#  arrange(wv_n, factor(measure, levels = c("0-2",
#                                           "3 or more"))) %>%
#  group_by(var, emp_contract_class) %>% 
#  mutate(d=sum(n),
#         p=n/d,
#         margin = qnorm(0.975)*sqrt(p*(1-p)/d)*100,
#         lci = est-margin,
#         uci = est+margin) %>% 
#  ungroup() %>% 
#  dplyr::select(-c(d,p,margin)) %>% 
#  arrange(emp_contract_class)
#
#emp_contract_prev <- emp_contract_prev %>% bind_rows(ghq_prev_a)
#
#### sample B ------------
#
### calculate proportions
#ghq1_b <- data.frame(svyby(~ghq_case3, ~emp_contract_class,svy_emp_contract_b, svymean, na.rm=TRUE))
#
#ghq1_b_long <- ghq1_b %>% pivot_longer(2:3,names_to = "measure",values_to = "est") %>% 
#  rename("se" = "se.ghq_case30.2") %>% 
#  dplyr::select(-se.ghq_case33.or.more)
#
### calculate totals
#ghq12_b <- data.frame(svyby(~ghq_case3, ~emp_contract_class,svy_emp_contract_b, svytotal, na.rm=TRUE))
#
#ghq12_b_long <- ghq12_b %>% pivot_longer(2:3,names_to = "measure",values_to = "total") %>% 
#  dplyr::select(-c(se.ghq_case30.2, se.ghq_case33.or.more))
#
##join
#ghq1_b_long <- ghq1_b_long %>% left_join(ghq12_b_long)
#
#### get rid of junk text in measure strings
#ghq1_b_long$emp_contract_class <- str_to_title(ghq1_b_long$emp_contract_class)
#ghq1_b_long$measure <- str_replace(ghq1_b_long$measure,"ghq_case3","")
#ghq1_b_long$measure <- str_replace(ghq1_b_long$measure,"0.2","0-2")
#ghq1_b_long$measure <- str_replace(ghq1_b_long$measure,"3.or.more","3 or more")
#
### join together and format
#ghq_prev_b <- ghq1_b_long %>%
#  mutate(est = est*100,
#         var="GHQ-12 caseness",
#         wv_n=10) %>% 
#  rename(n=total) %>% 
#  dplyr::select(wv_n, var, emp_contract_class,measure, n, est, se) %>% 
#  arrange(wv_n, factor(measure, levels = c("0-2",
#                                           "3 or more"))) %>%
#  group_by(var, emp_contract_class) %>% 
#  mutate(d=sum(n),
#         p=n/d,
#         margin = qnorm(0.975)*sqrt(p*(1-p)/d)*100,
#         lci = est-margin,
#         uci = est+margin) %>% 
#  ungroup() %>% 
#  dplyr::select(-c(d,p,margin)) %>% 
#  arrange(emp_contract_class)
#
#emp_contract_prev <- emp_contract_prev %>% bind_rows(ghq_prev_b)
#
##### SF-12 MCS -----------------------------------------------------------------
#
#### sample A ------------
#
#sf12mcs_a <- data.frame(svyby(~sf12mcs_dv, ~emp_contract_class,svy_emp_contract_a, svymean, na.rm=TRUE))
#
#sf12mcs_a <- sf12mcs_a %>% 
#  rename("est"="sf12mcs_dv") %>% 
#  mutate(wv_n= 6,
#         var = "SF-12 mental component",
#         measure = "Mean",
#         n=NA)
#
#emp_contract_prev <- emp_contract_prev %>% bind_rows(sf12mcs_a)
#
#### sample B ------------
#
#sf12mcs_b <- data.frame(svyby(~sf12mcs_dv, ~emp_contract_class,svy_emp_contract_b, svymean, na.rm=TRUE))
#
#sf12mcs_b <- sf12mcs_b %>% 
#  rename("est"="sf12mcs_dv") %>% 
#  mutate(wv_n= 10,
#         var = "SF-12 mental component",
#         measure = "Mean",
#         n=NA)
#
#emp_contract_prev <- emp_contract_prev %>% bind_rows(sf12mcs_b)
#
#
#
#
##### add confidence intervals --------------------------------------------------
#
#emp_contract_prev$emp_contract_class <- str_to_sentence(emp_contract_prev$emp_contract_class)
#
#
#emp_contract_prev <- emp_contract_prev %>% 
#  mutate(sample_grp = ifelse(wv_n==6, "A","B")) %>% 
#  mutate(exp_flag = factor(ifelse(emp_contract_class=="Non-permanent employment",
#                                  1,0)))
#
#
##### plots ---------------------------------------------------------------------
#
#### binary self-reported health ----------------------------
#tiff("./output/weighted/emp_contract_srh_prev_grouped.tiff")
#emp_contract_prev %>% 
#  filter(var=="Self-reported health" & measure=="Good/fair/poor") %>% 
#  mutate(emp_contract_class=fct_reorder(emp_contract_class,est)) %>% 
#  ggplot(aes(x=emp_contract_class, y=est,
#             fill=exp_flag)) +
#  geom_col(show.legend = FALSE) +
#  geom_errorbar(aes(ymin=lci, ymax=uci), colour="black", width=.1)+
#  coord_flip() +
#  theme_bw() +
#  scale_fill_manual(name = "exp_flag", values=c("grey50","red")) +
#  facet_wrap(~sample_grp, ncol = 1)
#dev.off()
#
#### SF-12 PCS ----------------------------
#tiff("./output/weighted/emp_contract_sf12-pcs_prev_grouped.tiff")
#emp_contract_prev %>% 
#  filter(var=="SF-12 physical component") %>% 
#  mutate(emp_contract_class=fct_reorder(emp_contract_class,est)) %>% 
#  ggplot(aes(x=emp_contract_class, y=est,
#             fill=exp_flag)) +
#  geom_col(show.legend = FALSE) +
#  geom_errorbar(aes(ymin=lci, ymax=uci), colour="black", width=.1)+
#  coord_flip() +
#  theme_bw() +
#  scale_fill_manual(name = "exp_flag", values=c("grey50","red")) +
#  facet_wrap(~sample_grp, ncol = 1)
#dev.off()
#
#### GHQ-12 caseness ------------------------------------------------------------
#
#tiff("./output/weighted/emp_contract_ghq_prev_grouped.tiff")
#emp_contract_prev %>% 
#  filter(var=="GHQ-12 caseness" & measure=="3 or more") %>% 
#  mutate(emp_contract_class=fct_reorder(emp_contract_class,est)) %>% 
#  ggplot(aes(x=emp_contract_class, y=est,
#             fill=exp_flag)) +
#  geom_col(show.legend = FALSE) +
#  geom_errorbar(aes(ymin=lci, ymax=uci), colour="black", width=.1)+
#  coord_flip() +
#  theme_bw() +
#  scale_fill_manual(name = "exp_flag", values=c("grey50","red")) +
#  facet_wrap(~sample_grp, ncol = 1)
#dev.off()
#
#### SF-12 MCS ----------------------------
#tiff("./output/weighted/emp_contract_sf12-mcs_prev_grouped.tiff")
#emp_contract_prev %>% 
#  filter(var=="SF-12 mental component") %>% 
#  mutate(emp_contract_class=fct_reorder(emp_contract_class,est)) %>% 
#  ggplot(aes(x=emp_contract_class, y=est,
#             fill=exp_flag)) +
#  geom_col(show.legend = FALSE) +
#  geom_errorbar(aes(ymin=lci, ymax=uci), colour="black", width=.1)+
#  coord_flip() +
#  theme_bw() +
#  scale_fill_manual(name = "exp_flag", values=c("grey50","red")) +
#  facet_wrap(~sample_grp, ncol = 1)
#dev.off()
#
#
#
#################################################################################
######                           employment spells                          #####
#################################################################################
#
##### to be added ---------------------------------------------------------------
#
#################################################################################
######                          multiple employment                         #####
#################################################################################
#
##### self-rated health ---------------------------------------------------------
#
#### sample A ------------
#
### calculate proportions
#srh1_a <- data.frame(svyby(~srh_bin, ~multi_emp_class,svy_multi_emp_a, svymean, na.rm=TRUE))
#
#srh1_a_long <- srh1_a %>% pivot_longer(2:3,names_to = "measure",values_to = "est") %>% 
#  rename("se" = "se.srh_binexcellent.very.good") %>% 
#  dplyr::select(-se.srh_bingood.fair.poor)
#
### calculate totals
#srh12_a <- data.frame(svyby(~srh_bin, ~multi_emp_class,svy_multi_emp_a, svytotal, na.rm=TRUE))
#
#srh12_a_long <- srh12_a %>% pivot_longer(2:3,names_to = "measure",values_to = "total") %>% 
#  dplyr::select(-c(se.srh_binexcellent.very.good, se.srh_bingood.fair.poor))
#
##join
#srh1_a_long <- srh1_a_long %>% left_join(srh12_a_long)
#
#### get rid of junk text in measure strings
#srh1_a_long$multi_emp_class <- str_to_title(srh1_a_long$multi_emp_class)
#srh1_a_long$measure <- str_replace(srh1_a_long$measure,"srh_bin","")
#srh1_a_long$measure <- str_replace(srh1_a_long$measure,"excellent.very.good","Excellent/very good")
#srh1_a_long$measure <- str_replace(srh1_a_long$measure,"good.fair.poor","Good/fair/poor")
#
### join together and format
#srh_prev_a <- srh1_a_long %>%
#  mutate(est = est*100,
#         var="Self-reported health",
#         wv_n=6) %>% 
#  rename(n=total) %>% 
#  dplyr::select(wv_n, var, multi_emp_class,measure, n, est, se) %>% 
#  arrange(wv_n, factor(measure, levels = c("Excellent/very good",
#                                           "Good/fair/poor"))) %>% 
#  arrange(multi_emp_class)
#
#multi_emp_prev <- srh_prev_a
#
#### sample B ------------
#
### calculate proportions
#srh1_b <- data.frame(svyby(~srh_bin, ~multi_emp_class,svy_multi_emp_b, svymean, na.rm=TRUE))
#
#srh1_b_long <- srh1_b %>% pivot_longer(2:3,names_to = "measure",values_to = "est") %>% 
#  rename("se" = "se.srh_binexcellent.very.good") %>% 
#  dplyr::select(-se.srh_bingood.fair.poor)
#
### calculate totals
#srh12_b <- data.frame(svyby(~srh_bin, ~multi_emp_class,svy_multi_emp_b, svytotal, na.rm=TRUE))
#
#srh12_b_long <- srh12_b %>% pivot_longer(2:3,names_to = "measure",values_to = "total") %>% 
#  dplyr::select(-c(se.srh_binexcellent.very.good, se.srh_bingood.fair.poor))
#
##join
#srh1_b_long <- srh1_b_long %>% left_join(srh12_b_long)
#
#### get rid of junk text in measure strings
#srh1_b_long$multi_emp_class <- str_to_title(srh1_b_long$multi_emp_class)
#srh1_b_long$measure <- str_replace(srh1_b_long$measure,"srh_bin","")
#srh1_b_long$measure <- str_replace(srh1_b_long$measure,"excellent.very.good","Excellent/very good")
#srh1_b_long$measure <- str_replace(srh1_b_long$measure,"good.fair.poor","Good/fair/poor")
#
#
#
#
### join together and format
#srh_prev_b <- srh1_b_long %>%
#  mutate(est = est*100,
#         var="Self-reported health",
#         wv_n=10) %>% 
#  rename(n=total) %>% 
#  dplyr::select(wv_n, var, multi_emp_class,measure, n, est, se) %>% 
#  arrange(wv_n, factor(measure, levels = c("Excellent/very good",
#                                           "Good/fair/poor"))) %>% 
#  arrange(multi_emp_class)
#
#multi_emp_prev <- multi_emp_prev %>% bind_rows(srh_prev_b)
#
##### SF-12 PCS -----------------------------------------------------------------
#
#### sample A ------------
#
#sf12pcs_a <- data.frame(svyby(~sf12pcs_dv, ~multi_emp_class,svy_multi_emp_a, svymean, na.rm=TRUE))
#
#sf12pcs_a <- sf12pcs_a %>% 
#  rename("est"="sf12pcs_dv") %>% 
#  mutate(wv_n= 6,
#         var = "SF-12 physical component",
#         measure = "Mean",
#         n=NA)
#
#multi_emp_prev <- multi_emp_prev %>% bind_rows(sf12pcs_a)
#
#### sample B ------------
#
#sf12pcs_b <- data.frame(svyby(~sf12pcs_dv, ~multi_emp_class,svy_multi_emp_b, svymean, na.rm=TRUE))
#
#sf12pcs_b <- sf12pcs_b %>% 
#  rename("est"="sf12pcs_dv") %>% 
#  mutate(wv_n= 10,
#         var = "SF-12 physical component",
#         measure = "Mean",
#         n=NA)
#
#multi_emp_prev <- multi_emp_prev %>% bind_rows(sf12pcs_b)
#
##### GHQ-12 --------------------------------------------------------------------
#
#### sample A ------------
#
### calculate proportions
#ghq1_a <- data.frame(svyby(~ghq_case3, ~multi_emp_class,svy_multi_emp_a, svymean, na.rm=TRUE))
#
#ghq1_a_long <- ghq1_a %>% pivot_longer(2:3,names_to = "measure",values_to = "est") %>% 
#  rename("se" = "se.ghq_case30.2") %>% 
#  dplyr::select(-se.ghq_case33.or.more)
#
### calculate totals
#ghq12_a <- data.frame(svyby(~ghq_case3, ~multi_emp_class,svy_multi_emp_a, svytotal, na.rm=TRUE))
#
#ghq12_a_long <- ghq12_a %>% pivot_longer(2:3,names_to = "measure",values_to = "total") %>% 
#  dplyr::select(-c(se.ghq_case30.2, se.ghq_case33.or.more))
#
##join
#ghq1_a_long <- ghq1_a_long %>% left_join(ghq12_a_long)
#
#### get rid of junk text in measure strings
#ghq1_a_long$multi_emp_class <- str_to_title(ghq1_a_long$multi_emp_class)
#ghq1_a_long$measure <- str_replace(ghq1_a_long$measure,"ghq_case3","")
#ghq1_a_long$measure <- str_replace(ghq1_a_long$measure,"0.2","0-2")
#ghq1_a_long$measure <- str_replace(ghq1_a_long$measure,"3.or.more","3 or more")
#
### join together and format
#ghq_prev_a <- ghq1_a_long %>%
#  mutate(est = est*100,
#         var="GHQ-12 caseness",
#         wv_n=6) %>% 
#  rename(n=total) %>% 
#  dplyr::select(wv_n, var, multi_emp_class,measure, n, est, se) %>% 
#  arrange(wv_n, factor(measure, levels = c("0-2",
#                                           "3 or more"))) %>% 
#  arrange(multi_emp_class)
#
#multi_emp_prev <- multi_emp_prev %>% bind_rows(ghq_prev_a)
#
#### sample B ------------
#
### calculate proportions
#ghq1_b <- data.frame(svyby(~ghq_case3, ~multi_emp_class,svy_multi_emp_b, svymean, na.rm=TRUE))
#
#ghq1_b_long <- ghq1_b %>% pivot_longer(2:3,names_to = "measure",values_to = "est") %>% 
#  rename("se" = "se.ghq_case30.2") %>% 
#  dplyr::select(-se.ghq_case33.or.more)
#
### calculate totals
#ghq12_b <- data.frame(svyby(~ghq_case3, ~multi_emp_class,svy_multi_emp_b, svytotal, na.rm=TRUE))
#
#ghq12_b_long <- ghq12_b %>% pivot_longer(2:3,names_to = "measure",values_to = "total") %>% 
#  dplyr::select(-c(se.ghq_case30.2, se.ghq_case33.or.more))
#
##join
#ghq1_b_long <- ghq1_b_long %>% left_join(ghq12_b_long)
#
#### get rid of junk text in measure strings
#ghq1_b_long$multi_emp_class <- str_to_title(ghq1_b_long$multi_emp_class)
#ghq1_b_long$measure <- str_replace(ghq1_b_long$measure,"ghq_case3","")
#ghq1_b_long$measure <- str_replace(ghq1_b_long$measure,"0.2","0-2")
#ghq1_b_long$measure <- str_replace(ghq1_b_long$measure,"3.or.more","3 or more")
#
### join together and format
#ghq_prev_b <- ghq1_b_long %>%
#  mutate(est = est*100,
#         var="GHQ-12 caseness",
#         wv_n=10) %>% 
#  rename(n=total) %>% 
#  dplyr::select(wv_n, var, multi_emp_class,measure, n, est, se) %>% 
#  arrange(wv_n, factor(measure, levels = c("0-2",
#                                           "3 or more"))) %>% 
#  arrange(multi_emp_class)
#
#multi_emp_prev <- multi_emp_prev %>% bind_rows(ghq_prev_b)
#
##### SF-12 MCS -----------------------------------------------------------------
#
#### sample A ------------
#
#sf12mcs_a <- data.frame(svyby(~sf12mcs_dv, ~multi_emp_class,svy_multi_emp_a, svymean, na.rm=TRUE))
#
#sf12mcs_a <- sf12mcs_a %>% 
#  rename("est"="sf12mcs_dv") %>% 
#  mutate(wv_n= 6,
#         var = "SF-12 mental component",
#         measure = "Mean",
#         n=NA)
#
#multi_emp_prev <- multi_emp_prev %>% bind_rows(sf12mcs_a)
#
#### sample B ------------
#
#sf12mcs_b <- data.frame(svyby(~sf12mcs_dv, ~multi_emp_class,svy_multi_emp_b, svymean, na.rm=TRUE))
#
#sf12mcs_b <- sf12mcs_b %>% 
#  rename("est"="sf12mcs_dv") %>% 
#  mutate(wv_n= 10,
#         var = "SF-12 mental component",
#         measure = "Mean",
#         n=NA)
#
#multi_emp_prev <- multi_emp_prev %>% bind_rows(sf12mcs_b)
#
#
#
##### add confidence intervals --------------------------------------------------
#multi_emp_prev$multi_emp_class <- str_to_sentence(multi_emp_prev$multi_emp_class)
#
#multi_emp_prev <- multi_emp_prev %>% 
#  mutate(sample_grp = ifelse(wv_n==6, "A","B")) %>% 
#  mutate(exp_flag = factor(ifelse(multi_emp_class=="Multiple employment",
#                                  1,0)))
#
#
#multi_emp_prev <- multi_emp_prev %>%
#  group_by(var, multi_emp_class) %>% 
#  mutate(d=sum(n),
#         p=n/d,
#         margin = qnorm(0.975)*sqrt(p*(1-p)/d)*100,
#         lci = est-margin,
#         uci = est+margin) %>% 
#  ungroup() %>% 
#  dplyr::select(-c(d,p,margin)) %>% 
#  mutate(sample_grp = ifelse(wv_n==6, "A","B")) #%>% 
##  mutate(exp_flag = factor(ifelse(multi_emp_class=="Multiple Employment",
# #                                 1,0)))
#
##### plots ---------------------------------------------------------------------
#
#### binary self-reported health ----------------------------
#tiff("./output/weighted/multi_emp_srh_prev_grouped.tiff")
#multi_emp_prev %>% 
#  filter(var=="Self-reported health" & measure=="Good/fair/poor") %>% 
#  mutate(multi_emp_class=fct_reorder(multi_emp_class,est)) %>% 
#  ggplot(aes(x=multi_emp_class, y=est,
#             fill=exp_flag)) +
#  geom_col(show.legend = FALSE) +
#  geom_errorbar(aes(ymin=lci, ymax=uci), colour="black", width=.1)+
#  coord_flip() +
#  theme_bw() +
#  scale_fill_manual(name = "exp_flag", values=c("grey50","red")) +
#  facet_wrap(~sample_grp, ncol = 1)
#dev.off()
#
#### SF-12 PCS ----------------------------
#tiff("./output/weighted/multi_emp_sf12-pcs_prev_grouped.tiff")
#multi_emp_prev %>% 
#  filter(var=="SF-12 physical component") %>% 
#  mutate(multi_emp_class=fct_reorder(multi_emp_class,est)) %>% 
#  ggplot(aes(x=multi_emp_class, y=est,
#             fill=exp_flag)) +
#  geom_col(show.legend = FALSE) +
##  geom_errorbar(aes(ymin=lci, ymax=uci), colour="black", width=.1)+
#  coord_flip() +
#  theme_bw() +
#  scale_fill_manual(name = "exp_flag", values=c("grey50","red")) +
#  facet_wrap(~sample_grp, ncol = 1)
#dev.off()
#
#
#### GHQ-12 caseness ------------------------------------------------------------
#
#tiff("./output/weighted/multi_emp_ghq_prev_grouped.tiff")
#multi_emp_prev %>% 
#  filter(var=="GHQ-12 caseness" & measure=="3 or more") %>% 
#  mutate(multi_emp_class=fct_reorder(multi_emp_class,est)) %>% 
#  ggplot(aes(x=multi_emp_class, y=est,
#             fill=exp_flag)) +
#  geom_col(show.legend = FALSE) +
#  geom_errorbar(aes(ymin=lci, ymax=uci), colour="black", width=.1)+
#  coord_flip() +
#  theme_bw() +
#  scale_fill_manual(name = "exp_flag", values=c("grey50","red")) +
#  facet_wrap(~sample_grp, ncol = 1)
#dev.off()
#
#### SF-12 MCS ----------------------------
#tiff("./output/weighted/multi_emp_sf12-mcs_prev_grouped.tiff")
#multi_emp_prev %>% 
#  filter(var=="SF-12 mental component") %>% 
#  mutate(multi_emp_class=fct_reorder(multi_emp_class,est)) %>% 
#  ggplot(aes(x=multi_emp_class, y=est,
#             fill=exp_flag)) +
#  geom_col(show.legend = FALSE) +
#  #  geom_errorbar(aes(ymin=lci, ymax=uci), colour="black", width=.1)+
#  coord_flip() +
#  theme_bw() +
#  scale_fill_manual(name = "exp_flag", values=c("grey50","red")) +
#  facet_wrap(~sample_grp, ncol = 1)
#dev.off()
#
#################################################################################
#######              regression models - employment contract               ######
#################################################################################
#
##### self-rated health ---------------------------------------------------------
#### Sample A
#srh_reg_a <- dfas1a_end_class1 %>% 
#  mutate(srh_bin = ifelse(srh_bin == "good/fair/poor",1,0))
#
#srh_reg_a$emp_contract_class <- factor(srh_reg_a$emp_contract_class)
#
#srh_model_a <- glm(srh_bin ~ relevel(emp_contract_class, ref = 4)+sex_dv+age_dv+hiqual_dv,
#    family=binomial,data=srh_reg_a)
#
#srh_model_a_df <- data.frame(exp(cbind(OR = coef(srh_model_a), round(confint(srh_model_a),2))))
#srh_model_a_df <- srh_model_a_df %>%
#  rename("lci"="X2.5..",
#         "uci"="X97.5..")
#
#### Sample B
#srh_reg_b <- dfas1b_end_class1 %>% 
#  mutate(srh_bin = ifelse(srh_bin == "good/fair/poor",1,0))
#
#srh_reg_b$emp_contract_class <- factor(srh_reg_b$emp_contract_class)
#
#srh_model_b <- glm(srh_bin ~ relevel(emp_contract_class, ref = 4)+sex_dv+age_dv+hiqual_dv,
#                   family=binomial,data=srh_reg_b)
#
#srh_model_b_df <- data.frame(exp(cbind(OR = coef(srh_model_b), round(confint(srh_model_b),2))))
#srh_model_b_df <- srh_model_b_df %>%
#  rename("lci"="X2.5..",
#         "uci"="X97.5..")
#
##### GHQ12 ---------------------------------------------------------------------
#### Sample A
#ghq_reg_a <- dfas1a_end_class1 %>% 
#  mutate(ghq_case3 = ifelse(ghq_case3 == "3 or more",1,0))
#
#ghq_reg_a$emp_contract_class <- factor(ghq_reg_a$emp_contract_class)
#
#ghq_model_a <- glm(ghq_case3 ~ relevel(emp_contract_class, ref = 4)+sex_dv+age_dv+hiqual_dv,
#                   family=binomial,data=ghq_reg_a)
#
#ghq_model_a_df <- data.frame(exp(cbind(OR = coef(ghq_model_a), round(confint(ghq_model_a),2))))
#ghq_model_a_df <- ghq_model_a_df %>%
#  rename("lci"="X2.5..",
#         "uci"="X97.5..")
#
#### Sample B
#ghq_reg_b <- dfas1b_end_class1 %>% 
#  mutate(ghq_case3 = ifelse(ghq_case3 == "3 or more",1,0))
#
#ghq_reg_b$emp_contract_class <- factor(ghq_reg_b$emp_contract_class)
#
#ghq_model_b <- glm(ghq_case3 ~ relevel(emp_contract_class, ref = 4)+sex_dv+age_dv+hiqual_dv,
#                   family=binomial,data=ghq_reg_b)
#
#ghq_model_b_df <- data.frame(exp(cbind(OR = coef(ghq_model_b), round(confint(ghq_model_b),2))))
#ghq_model_b_df <- ghq_model_b_df %>%
#  rename("lci"="X2.5..",
#         "uci"="X97.5..")
#
#