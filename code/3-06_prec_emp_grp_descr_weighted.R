################################################################################

# Persistent precarious employment and health - Understanding Society
# 3-06 - grouped descriptives - weighted
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

#### end point data ------------------
### sample A ---------
dfas1a_end <- readRDS("./working_data/dfas1a_end.rds")

# drop unused levels
dfas1a_end$sex_dv <- droplevels(dfas1a_end$sex_dv)
dfas1a_end$hiqual_dv <- droplevels(dfas1a_end$hiqual_dv)


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

# drop unused levels
dfas1b_end$sex_dv <- droplevels(dfas1b_end$sex_dv)
dfas1b_end$hiqual_dv <- droplevels(dfas1b_end$hiqual_dv)

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

#### load weight spines ---------------------------------

weight_spine_a <- readRDS("./look_ups/weights_spine_a.rds") %>% 
  dplyr::select(pidp, strata, psu)

weight_spine_b <- readRDS("./look_ups/weights_spine_b.rds") %>% 
  dplyr::select(pidp, strata, psu)


#### join classes and weight spines back on to end point df ------------

dfas1a_end_class <- dfas1a_end %>% 
  left_join(emp_contracta_5class_spine, by="pidp") %>% 
  left_join(emp_spellsa_3class_spine, by="pidp") %>% 
  left_join(multi_empa_5class_spine, by="pidp") %>% 
  left_join(weight_spine_a, by="pidp")

dfas1b_end_class <- dfas1b_end %>% 
  left_join(emp_contractb_5class_spine, by="pidp") %>% 
  left_join(emp_spellsb_3class_spine, by="pidp") %>% 
  left_join(multi_empb_5class_spine, by="pidp") %>% 
  left_join(weight_spine_b, by="pidp")

#### create overlap var

dfas1a_end_class <- dfas1a_end_class %>% 
  mutate(emp_contract_class_bin = ifelse(emp_contract_class == "non-permanent employment",1,0)) %>% 
  mutate(broken_emp_class_bin = ifelse(emp_spells_class == "broken employment",1,0)) %>% 
  mutate(multi_emp_class_bin = ifelse(multi_emp_class == "multiple employment",1,0)) %>% 
  mutate(exp_overlap = paste0(emp_contract_class_bin,"-",broken_emp_class_bin,"-",multi_emp_class_bin)) %>% 
  mutate(exp_overlap2 = paste0(emp_contract_class,"-",emp_spells_class,"-",multi_emp_class))
# think first version is better for presentation, second gives detail if needed

dfas1b_end_class <- dfas1b_end_class %>% 
  mutate(emp_contract_class_bin = ifelse(emp_contract_class == "non-permanent employment",1,0)) %>% 
  mutate(broken_emp_class_bin = ifelse(emp_spells_class == "broken employment",1,0)) %>% 
  mutate(multi_emp_class_bin = ifelse(multi_emp_class == "multiple employment",1,0)) %>% 
  mutate(exp_overlap = paste0(emp_contract_class_bin,"-",broken_emp_class_bin,"-",multi_emp_class_bin)) %>% 
  mutate(exp_overlap2 = paste0(emp_contract_class,"-",emp_spells_class,"-",multi_emp_class))


#### create weighted samples -----------------

svy_dfas1a_end_class <- svydesign(id=~psu, strata=~strata,
                                weights=~indinub_xw, data=dfas1a_end_class)

svy_dfas1b_end_class <- svydesign(id=~psu, strata=~strata,
                                weights=~indinui_xw, data=dfas1b_end_class)

################################################################################
#####                             class overlap                            #####
################################################################################

## proportion
exposure_class_overlaps_pc_a <- data.frame(svymean(~exp_overlap, svy_dfas1a_end_class, na.rm=TRUE))
exposure_class_overlaps_pc_a <- cbind(rownames(exposure_class_overlaps_pc_a),exposure_class_overlaps_pc_a, row.names=NULL)
names(exposure_class_overlaps_pc_a)[1] <- "overlaps"

exposure_class_overlaps_pc_b <- data.frame(svymean(~exp_overlap, svy_dfas1b_end_class, na.rm=TRUE))
exposure_class_overlaps_pc_b <- cbind(rownames(exposure_class_overlaps_pc_b),exposure_class_overlaps_pc_b, row.names=NULL)
names(exposure_class_overlaps_pc_b)[1] <- "overlaps"

## total
exposure_class_overlaps_n_a <- data.frame(svytotal(~exp_overlap, svy_dfas1a_end_class, na.rm=TRUE))
exposure_class_overlaps_n_a <- cbind(rownames(exposure_class_overlaps_n_a),exposure_class_overlaps_n_a, row.names=NULL)
names(exposure_class_overlaps_n_a)[1] <- "overlaps"

exposure_class_overlaps_n_b <- data.frame(svytotal(~exp_overlap, svy_dfas1b_end_class, na.rm=TRUE))
exposure_class_overlaps_n_b <- cbind(rownames(exposure_class_overlaps_n_b),exposure_class_overlaps_n_b, row.names=NULL)
names(exposure_class_overlaps_n_b)[1] <- "overlaps"

### join df's together

exposure_class_overlaps_a <- exposure_class_overlaps_pc_a %>% 
  mutate(pc = mean*100) %>% 
  dplyr::select(-c(mean,SE)) %>% 
  left_join(exposure_class_overlaps_n_a) %>% 
  dplyr::select(-SE)

exposure_class_overlaps_b <- exposure_class_overlaps_pc_b %>% 
  mutate(pc = mean*100) %>% 
  dplyr::select(-c(mean,SE)) %>% 
  left_join(exposure_class_overlaps_n_b) %>% 
  dplyr::select(-SE)

## save 
write_rds(exposure_class_overlaps_a, 
          "./output/weighted/exposure_class_overlaps_a.rds")

write_rds(exposure_class_overlaps_b, 
          "./output/weighted/exposure_class_overlaps_b.rds")

################################################################################
#####                   descriptives - class totals and %s                 #####
################################################################################

#### employment contract -------------------------------------------------------

### sample A ------------
emp_contract_a <- data.frame(svymean(~emp_contract_class, svy_dfas1a_end_class, na.rm=TRUE))
emp_contract_a <- cbind(rownames(emp_contract_a),emp_contract_a, row.names=NULL)
emp_contract_a$`rownames(emp_contract_a)` <- str_replace(emp_contract_a$`rownames(emp_contract_a)`, "emp_contract_class","")
emp_contract_a <- emp_contract_a %>% rename(measure = `rownames(emp_contract_a)`)
names(emp_contract_a) <- tolower(names(emp_contract_a)) # change all col names to lower case

emp_contract_a <- emp_contract_a %>% 
  mutate(pc = as.double(mean*100),
         exp_lab = "employment contract",
         sample_grp = "A") %>% 
  dplyr::select(-c(se, mean))

### sample B ------------
emp_contract_b <- data.frame(svymean(~emp_contract_class, svy_dfas1b_end_class, na.rm=TRUE))
emp_contract_b <- cbind(rownames(emp_contract_b),emp_contract_b, row.names=NULL)
emp_contract_b$`rownames(emp_contract_b)` <- str_replace(emp_contract_b$`rownames(emp_contract_b)`, "emp_contract_class","")
emp_contract_b <- emp_contract_b %>% rename(measure = `rownames(emp_contract_b)`)
names(emp_contract_b) <- tolower(names(emp_contract_b)) # change all col names to lower case

emp_contract_b <- emp_contract_b %>% 
  mutate(pc = as.double(mean*100),
         exp_lab = "employment contract",
         sample_grp = "B") %>% 
  dplyr::select(-c(se, mean))

#### employment spells ---------------------------------------------------------

### sample A ------------
emp_spells_a <- data.frame(svymean(~emp_spells_class, svy_dfas1a_end_class, na.rm=TRUE))
emp_spells_a_n <- data.frame(svytotal(~emp_spells_class, svy_dfas1a_end_class, na.rm=TRUE))
emp_spells_a <- cbind(rownames(emp_spells_a),emp_spells_a, row.names=NULL)
emp_spells_a$`rownames(emp_spells_a)` <- str_replace(emp_spells_a$`rownames(emp_spells_a)`, "emp_spells_class","")
emp_spells_a <- emp_spells_a %>% rename(measure = `rownames(emp_spells_a)`)
names(emp_spells_a) <- tolower(names(emp_spells_a)) # change all col names to lower case

emp_spells_a <- emp_spells_a %>% 
  mutate(pc = as.double(mean*100),
         exp_lab = "employment spells",
         sample_grp = "A") %>% 
  dplyr::select(-c(se, mean))

### sample B ------------
emp_spells_b <- data.frame(svymean(~emp_spells_class, svy_dfas1b_end_class, na.rm=TRUE))
emp_spells_b <- cbind(rownames(emp_spells_b),emp_spells_b, row.names=NULL)
emp_spells_b$`rownames(emp_spells_b)` <- str_replace(emp_spells_b$`rownames(emp_spells_b)`, "emp_spells_class","")
emp_spells_b <- emp_spells_b %>% rename(measure = `rownames(emp_spells_b)`)
names(emp_spells_b) <- tolower(names(emp_spells_b)) # change all col names to lower case

emp_spells_b <- emp_spells_b %>% 
  mutate(pc = as.double(mean*100),
         exp_lab = "employment spells",
         sample_grp = "B") %>% 
  dplyr::select(-c(se, mean))

#### multiple employment  ------------------------------------------------------

### sample A ------------
multi_emp_a <- data.frame(svymean(~multi_emp_class, svy_dfas1a_end_class, na.rm=TRUE))
multi_emp_a <- cbind(rownames(multi_emp_a),multi_emp_a, row.names=NULL)
multi_emp_a$`rownames(multi_emp_a)` <- str_replace(multi_emp_a$`rownames(multi_emp_a)`, "multi_emp_class","")
multi_emp_a <- multi_emp_a %>% rename(measure = `rownames(multi_emp_a)`)
names(multi_emp_a) <- tolower(names(multi_emp_a)) # change all col names to lower case

multi_emp_a <- multi_emp_a %>% 
  mutate(pc = as.double(mean*100),
         exp_lab = "multiple employment",
         sample_grp = "A") %>% 
  dplyr::select(-c(se, mean))

### sample B ------------
multi_emp_b <- data.frame(svymean(~multi_emp_class, svy_dfas1b_end_class, na.rm=TRUE))
multi_emp_b <- cbind(rownames(multi_emp_b),multi_emp_b, row.names=NULL)
multi_emp_b$`rownames(multi_emp_b)` <- str_replace(multi_emp_b$`rownames(multi_emp_b)`, "multi_emp_class","")
multi_emp_b <- multi_emp_b %>% rename(measure = `rownames(multi_emp_b)`)
names(multi_emp_b) <- tolower(names(multi_emp_b)) # change all col names to lower case

multi_emp_b <- multi_emp_b %>% 
  mutate(pc = as.double(mean*100),
         exp_lab = "multiple employment",
         sample_grp = "B") %>% 
  dplyr::select(-c(se, mean))

#### join together and save ----------------------------------------------------

class_pc <- emp_contract_a %>% 
  bind_rows(emp_contract_b,
            emp_spells_a,
            emp_spells_b,
            multi_emp_a,
            multi_emp_b)

write_rds(class_pc, "./working_data/weighted/class_pc.rds")

################################################################################
#####                   descriptives - employment contract                 #####
################################################################################

#### sex -----------------------------------------------------------------------

### sample A ------------

svymean(~sex_dv, svy_dfas1a_end_class, na.rm=TRUE)

## calculate proportions
sex_a <- data.frame(svyby(~sex_dv, ~emp_contract_class, svy_dfas1a_end_class, svymean, na.rm=TRUE))
sex_a <- sex_a %>% rename(class_mem = emp_contract_class)
names(sex_a) <- tolower(names(sex_a)) # change all col names to lower case
sex_a <- sex_a %>% pivot_longer(cols = 2:3, names_to = "measure")
sex_a <- sex_a %>% dplyr::select(-se.sex_dvfemale) %>% 
  rename(se = se.sex_dvmale) %>%
  mutate(wv_n=6,
         var="sex",
         measure = str_remove(measure,"sex_dv"),
         est=value*100,
         se=se*100) %>% 
  dplyr::select(wv_n,var,class_mem,measure,est,se) %>% 
  arrange(factor(class_mem, levels = c("non-permanent employment",
                                       "permanent employment",
                                       "into employment",
                                       "out of employment",
                                       "unemployed")),
          factor(measure, levels = c("female","male")))

## calculate totals
sex2_a <- data.frame(svyby(~sex_dv, ~emp_contract_class, svy_dfas1a_end_class, svytotal, na.rm=TRUE))
sex2_a <- sex2_a %>% rename(class_mem = emp_contract_class)
names(sex2_a) <- tolower(names(sex2_a)) # change all col names to lower case
sex2_a <- sex2_a %>% 
  pivot_longer(cols = 2:3, names_to = "measure") %>% 
  rename(n=value) %>% 
  mutate(wv_n=6,
         var="sex",
         measure = str_remove(measure,"sex_dv")) %>% 
  dplyr::select(wv_n,var,class_mem,measure,n) %>% 
  arrange(factor(class_mem, levels = c("non-permanent employment",
                                       "permanent employment",
                                       "into employment",
                                       "out of employment",
                                       "unemployed")),
          factor(measure, levels = c("female","male")))

## join together
sex_a <- sex_a %>% left_join(sex2_a) %>% 
  dplyr::select(wv_n,var,class_mem,measure,n,est,se)

emp_contract_grp <- sex_a

### sample B ------------

svymean(~sex_dv, svy_dfas1b_end_class, na.rm=TRUE)

## calculate proportions
sex_b <- data.frame(svyby(~sex_dv, ~emp_contract_class, svy_dfas1b_end_class, svymean, na.rm=TRUE))
sex_b <- sex_b %>% rename(class_mem = emp_contract_class)
names(sex_b) <- tolower(names(sex_b)) # change all col names to lower case
sex_b <- sex_b %>% pivot_longer(cols = 2:3, names_to = "measure")
sex_b <- sex_b %>% dplyr::select(-se.sex_dvfemale) %>% 
  rename(se = se.sex_dvmale) %>%
  mutate(wv_n=10,
         var="sex",
         measure = str_remove(measure,"sex_dv"),
         est=value*100,
         se=se*100) %>% 
  dplyr::select(wv_n,var,class_mem,measure,est,se) %>% 
  arrange(factor(class_mem, levels = c("non-permanent employment",
                                       "permanent employment",
                                       "into employment",
                                       "out of employment",
                                       "unemployed")),
          factor(measure, levels = c("female","male")))

## calculate totals
sex2_b <- data.frame(svyby(~sex_dv, ~emp_contract_class, svy_dfas1b_end_class, svytotal, na.rm=TRUE))
sex2_b <- sex2_b %>% rename(class_mem = emp_contract_class)
names(sex2_b) <- tolower(names(sex2_b)) # change all col names to lower case
sex2_b <- sex2_b %>% 
  pivot_longer(cols = 2:3, names_to = "measure") %>% 
  rename(n=value) %>% 
  mutate(wv_n=10,
         var="sex",
         measure = str_remove(measure,"sex_dv")) %>% 
  dplyr::select(wv_n,var,class_mem,measure,n) %>% 
  arrange(factor(class_mem, levels = c("non-permanent employment",
                                       "permanent employment",
                                       "into employment",
                                       "out of employment",
                                       "unemployed")),
          factor(measure, levels = c("female","male")))

## join together
sex_b <- sex_b %>% left_join(sex2_b) %>% 
  dplyr::select(wv_n,var,class_mem,measure,n,est,se)

emp_contract_grp <- emp_contract_grp %>% bind_rows(sex_b)

#### age -----------------------------------------------------------------------

### sample A ------------

svymean(~age_dv, svy_dfas1a_end_class, na.rm=TRUE)

## calculate proportions
age_a <- data.frame(svyby(~age_dv, ~emp_contract_class, svy_dfas1a_end_class, svymean, na.rm=TRUE))
age_a <- age_a %>% 
  mutate(wv_n = 6,
         measure = "mean",
         var="age",
         n=NA)%>% 
  rename(class_mem = emp_contract_class,
                          est = age_dv)  %>% 
  dplyr::select(wv_n,var,class_mem,measure,n,est,se) %>% 
  arrange(factor(class_mem, levels = c("non-permanent employment",
                                       "permanent employment",
                                       "into employment",
                                       "out of employment",
                                       "unemployed")))
names(age_a) <- tolower(names(age_a)) # change all col names to lower case

emp_contract_grp <- emp_contract_grp %>% bind_rows(age_a)

### sample B ------------

svymean(~age_dv, svy_dfas1b_end_class, na.rm=TRUE)

## calculate proportions
age_b <- data.frame(svyby(~age_dv, ~emp_contract_class, svy_dfas1b_end_class, svymean, na.rm=TRUE))
age_b <- age_b %>% 
  mutate(wv_n = 10,
         var="age",
         measure = "mean")%>% 
  rename(class_mem = emp_contract_class,
         est = age_dv)  %>% 
  arrange(factor(class_mem, levels = c("non-permanent employment",
                                       "permanent employment",
                                       "into employment",
                                       "out of employment",
                                       "unemployed")))
names(age_b) <- tolower(names(age_b)) # change all col names to lower case

emp_contract_grp <- emp_contract_grp %>% bind_rows(age_b)

#### Educational attainment ----------------------------------------------------

### sample A ------------

svymean(~hiqual_dv, svy_dfas1a_end_class, na.rm=TRUE)

## calculate proportions
ed_attain_a <- data.frame(svyby(~hiqual_dv, ~emp_contract_class, svy_dfas1a_end_class, svymean, na.rm=TRUE))
ed_attain_a <- ed_attain_a %>% rename(class_mem = emp_contract_class)
names(ed_attain_a) <- tolower(names(ed_attain_a)) # change all col names to lower case

# create long format data for estimates
temp <- ed_attain_a %>% 
  dplyr::select(c(1:7)) %>% 
  pivot_longer(cols = 2:7, names_to = "measure") %>% 
  rename(est=value) %>% 
  mutate(measure = str_remove(measure,"hiqual_dv"))

# create long format data for SE
temp2 <- ed_attain_a %>% 
  dplyr::select(c(1,8:13)) %>% 
  pivot_longer(cols = 2:7, names_to = "measure") %>% 
  rename(se=value) %>% 
  mutate(measure = str_remove(measure,"se.hiqual_dv"))

# join back together
ed_attain_a <- temp %>% 
  left_join(temp2) %>% 
  mutate(wv_n=6,
         var="educational attainment",
         est=est*100,
         se=se*100,
         measure = str_replace(measure,"\\."," "), # need to do this twice!
         measure = str_replace(measure,"\\."," ")) %>% 
  dplyr::select(wv_n,var,class_mem,measure,est,se) %>% 
  arrange(factor(class_mem, levels = c("non-permanent employment",
                                       "permanent employment",
                                       "into employment",
                                       "out of employment",
                                       "unemployed")),
          factor(measure, levels = c("degree",
                                     "other higher degree",
                                     "a level etc",
                                     "gcse etc",
                                     "other qualification",
                                     "no qualification")))

## calculate totals
ed_attain2_a <- data.frame(svyby(~hiqual_dv, ~emp_contract_class, svy_dfas1a_end_class, svytotal, na.rm=TRUE))
ed_attain2_a <- ed_attain2_a %>% rename(class_mem = emp_contract_class)
names(ed_attain2_a) <- tolower(names(ed_attain2_a)) # change all col names to lower case
ed_attain2_a <- ed_attain2_a %>% 
  dplyr::select(c(1:7)) %>% 
  pivot_longer(cols = 2:7, names_to = "measure") %>% 
  mutate(measure = str_remove(measure,"hiqual_dv")) %>% 
  rename(n=value) %>% 
  mutate(wv_n=6,
         var="educational attainment",
         measure = str_replace(measure,"\\."," "), # need to do this twice!
         measure = str_replace(measure,"\\."," ")) %>% 
  dplyr::select(wv_n,var,class_mem,measure,n) %>% 
  arrange(factor(class_mem, levels = c("non-permanent employment",
                                       "permanent employment",
                                       "into employment",
                                       "out of employment",
                                       "unemployed")),
          factor(measure, levels = c("degree",
                                     "other higher degree",
                                     "a level etc",
                                     "gcse etc",
                                     "other qualification",
                                     "no qualification")))

## join together
ed_attain_a <- ed_attain_a %>% left_join(ed_attain2_a) %>% 
  dplyr::select(wv_n,var,class_mem,measure,n,est,se)

emp_contract_grp <- emp_contract_grp %>% bind_rows(ed_attain_a)

rm(temp, temp2)

### sample B ------------

svymean(~hiqual_dv, svy_dfas1b_end_class, na.rm=TRUE)

## calculate proportions
ed_attain_b <- data.frame(svyby(~hiqual_dv, ~emp_contract_class, svy_dfas1b_end_class, svymean, na.rm=TRUE))
ed_attain_b <- ed_attain_b %>% rename(class_mem = emp_contract_class)
names(ed_attain_b) <- tolower(names(ed_attain_b)) # change all col names to lower case

# create long format data for estimates
temp <- ed_attain_b %>% 
  dplyr::select(c(1:7)) %>% 
  pivot_longer(cols = 2:7, names_to = "measure") %>% 
  rename(est=value) %>% 
  mutate(measure = str_remove(measure,"hiqual_dv"))

# create long format data for SE
temp2 <- ed_attain_b %>% 
  dplyr::select(c(1,8:13)) %>% 
  pivot_longer(cols = 2:7, names_to = "measure") %>% 
  rename(se=value) %>% 
  mutate(measure = str_remove(measure,"se.hiqual_dv"))

# join back together
ed_attain_b <- temp %>% 
  left_join(temp2) %>% 
  mutate(wv_n=6,
         var="educational attainment",
         est=est*100,
         se=se*100,
         measure = str_replace(measure,"\\."," "), # need to do this twice!
         measure = str_replace(measure,"\\."," ")) %>% 
  dplyr::select(wv_n,var,class_mem,measure,est,se) %>% 
  arrange(factor(class_mem, levels = c("non-permanent employment",
                                       "permanent employment",
                                       "into employment",
                                       "out of employment",
                                       "unemployed")),
          factor(measure, levels = c("degree",
                                     "other higher degree",
                                     "a level etc",
                                     "gcse etc",
                                     "other qualification",
                                     "no qualification")))

## calculate totals
ed_attain2_b <- data.frame(svyby(~hiqual_dv, ~emp_contract_class, svy_dfas1b_end_class, svytotal, na.rm=TRUE))
ed_attain2_b <- ed_attain2_b %>% rename(class_mem = emp_contract_class)
names(ed_attain2_b) <- tolower(names(ed_attain2_b)) # change all col names to lower case
ed_attain2_b <- ed_attain2_b %>% 
  dplyr::select(c(1:7)) %>% 
  pivot_longer(cols = 2:7, names_to = "measure") %>% 
  mutate(measure = str_remove(measure,"hiqual_dv")) %>% 
  rename(n=value) %>% 
  mutate(wv_n=6,
         var="educational attainment",
         measure = str_replace(measure,"\\."," "), # need to do this twice!
         measure = str_replace(measure,"\\."," ")) %>% 
  dplyr::select(wv_n,var,class_mem,measure,n) %>% 
  arrange(factor(class_mem, levels = c("non-permanent employment",
                                       "permanent employment",
                                       "into employment",
                                       "out of employment",
                                       "unemployed")),
          factor(measure, levels = c("degree",
                                     "other higher degree",
                                     "a level etc",
                                     "gcse etc",
                                     "other qualification",
                                     "no qualification")))

## join together
ed_attain_b <- ed_attain_b %>% left_join(ed_attain2_b) %>% 
  dplyr::select(wv_n,var,class_mem,measure,n,est,se)

emp_contract_grp <- emp_contract_grp %>% bind_rows(ed_attain_b)

rm(temp, temp2)

################################################################################
#####                    descriptives - broken employment                  #####
################################################################################

#### sex -----------------------------------------------------------------------

### sample A ------------

svymean(~sex_dv, svy_dfas1a_end_class, na.rm=TRUE)

## calculate proportions
sex_a <- data.frame(svyby(~sex_dv, ~emp_spells_class, svy_dfas1a_end_class, svymean, na.rm=TRUE))
sex_a <- sex_a %>% rename(class_mem = emp_spells_class)
names(sex_a) <- tolower(names(sex_a)) # change all col names to lower case
sex_a <- sex_a %>% pivot_longer(cols = 2:3, names_to = "measure")
sex_a <- sex_a %>% dplyr::select(-se.sex_dvfemale) %>% 
  rename(se = se.sex_dvmale) %>%
  mutate(wv_n=6,
         var="sex",
         measure = str_remove(measure,"sex_dv"),
         est=value*100,
         se=se*100) %>% 
  dplyr::select(wv_n,var,class_mem,measure,est,se) %>% 
  arrange(factor(class_mem, levels = c("broken employment",
                                       "unbroken employment",
                                       "unemployed")),
          factor(measure, levels = c("female","male")))

## calculate totals
sex2_a <- data.frame(svyby(~sex_dv, ~emp_spells_class, svy_dfas1a_end_class, svytotal, na.rm=TRUE))
sex2_a <- sex2_a %>% rename(class_mem = emp_spells_class)
names(sex2_a) <- tolower(names(sex2_a)) # change all col names to lower case
sex2_a <- sex2_a %>% 
  pivot_longer(cols = 2:3, names_to = "measure") %>% 
  rename(n=value) %>% 
  mutate(wv_n=6,
         var="sex",
         measure = str_remove(measure,"sex_dv")) %>% 
  dplyr::select(wv_n,var,class_mem,measure,n) %>% 
  arrange(factor(class_mem, levels = c("broken employment",
                                       "unbroken employment",
                                       "unemployed")),
          factor(measure, levels = c("female","male")))

## join together
sex_a <- sex_a %>% left_join(sex2_a) %>% 
  dplyr::select(wv_n,var,class_mem,measure,n,est,se)

emp_spells_grp <- sex_a

### sample B ------------

svymean(~sex_dv, svy_dfas1b_end_class, na.rm=TRUE)

## calculate proportions
sex_b <- data.frame(svyby(~sex_dv, ~emp_spells_class, svy_dfas1b_end_class, svymean, na.rm=TRUE))
sex_b <- sex_b %>% rename(class_mem = emp_spells_class)
names(sex_b) <- tolower(names(sex_b)) # change all col names to lower case
sex_b <- sex_b %>% pivot_longer(cols = 2:3, names_to = "measure")
sex_b <- sex_b %>% dplyr::select(-se.sex_dvfemale) %>% 
  rename(se = se.sex_dvmale) %>%
  mutate(wv_n=10,
         var="sex",
         measure = str_remove(measure,"sex_dv"),
         est=value*100,
         se=se*100) %>% 
  dplyr::select(wv_n,var,class_mem,measure,est,se) %>% 
  arrange(factor(class_mem, levels = c("broken employment",
                                       "unbroken employment",
                                       "unemployed")),
          factor(measure, levels = c("female","male")))

## calculate totals
sex2_b <- data.frame(svyby(~sex_dv, ~emp_spells_class, svy_dfas1b_end_class, svytotal, na.rm=TRUE))
sex2_b <- sex2_b %>% rename(class_mem = emp_spells_class)
names(sex2_b) <- tolower(names(sex2_b)) # change all col names to lower case
sex2_b <- sex2_b %>% 
  pivot_longer(cols = 2:3, names_to = "measure") %>% 
  rename(n=value) %>% 
  mutate(wv_n=10,
         var="sex",
         measure = str_remove(measure,"sex_dv")) %>% 
  dplyr::select(wv_n,var,class_mem,measure,n) %>% 
  arrange(factor(class_mem, levels = c("broken employment",
                                       "unbroken employment",
                                       "unemployed")),
          factor(measure, levels = c("female","male")))

## join together
sex_b <- sex_b %>% left_join(sex2_b) %>% 
  dplyr::select(wv_n,var,class_mem,measure,n,est,se)

emp_spells_grp <- emp_spells_grp %>% bind_rows(sex_b)

#### age -----------------------------------------------------------------------

### sample A ------------

svymean(~age_dv, svy_dfas1a_end_class, na.rm=TRUE)

## calculate proportions
age_a <- data.frame(svyby(~age_dv, ~emp_spells_class, svy_dfas1a_end_class, svymean, na.rm=TRUE))
age_a <- age_a %>% 
  mutate(wv_n = 6,
         measure = "mean",
         var="age",
         n=NA)%>% 
  rename(class_mem = emp_spells_class,
         est = age_dv)  %>% 
  dplyr::select(wv_n,var,class_mem,measure,n,est,se) %>% 
  arrange(factor(class_mem, levels = c("broken employment",
                                       "unbroken employment",
                                       "unemployed")))
names(age_a) <- tolower(names(age_a)) # change all col names to lower case

emp_spells_grp <- emp_spells_grp %>% bind_rows(age_a)

### sample B ------------

svymean(~age_dv, svy_dfas1b_end_class, na.rm=TRUE)

## calculate proportions
age_b <- data.frame(svyby(~age_dv, ~emp_spells_class, svy_dfas1b_end_class, svymean, na.rm=TRUE))
age_b <- age_b %>% 
  mutate(wv_n = 10,
         var="age",
         measure = "mean")%>% 
  rename(class_mem = emp_spells_class,
         est = age_dv)  %>% 
  arrange(factor(class_mem, levels = c("broken employment",
                                       "unbroken employment",
                                       "unemployed")))
names(age_b) <- tolower(names(age_b)) # change all col names to lower case

emp_spells_grp <- emp_spells_grp %>% bind_rows(age_b)

#### Educational attainment ----------------------------------------------------

### sample A ------------

svymean(~hiqual_dv, svy_dfas1a_end_class, na.rm=TRUE)

## calculate proportions
ed_attain_a <- data.frame(svyby(~hiqual_dv, ~emp_spells_class, svy_dfas1a_end_class, svymean, na.rm=TRUE))
ed_attain_a <- ed_attain_a %>% rename(class_mem = emp_spells_class)
names(ed_attain_a) <- tolower(names(ed_attain_a)) # change all col names to lower case

# create long format data for estimates
temp <- ed_attain_a %>% 
  dplyr::select(c(1:7)) %>% 
  pivot_longer(cols = 2:7, names_to = "measure") %>% 
  rename(est=value) %>% 
  mutate(measure = str_remove(measure,"hiqual_dv"))

# create long format data for SE
temp2 <- ed_attain_a %>% 
  dplyr::select(c(1,8:13)) %>% 
  pivot_longer(cols = 2:7, names_to = "measure") %>% 
  rename(se=value) %>% 
  mutate(measure = str_remove(measure,"se.hiqual_dv"))

# join back together
ed_attain_a <- temp %>% 
  left_join(temp2) %>% 
  mutate(wv_n=6,
         var="educational attainment",
         est=est*100,
         se=se*100,
         measure = str_replace(measure,"\\."," "), # need to do this twice!
         measure = str_replace(measure,"\\."," ")) %>% 
  dplyr::select(wv_n,var,class_mem,measure,est,se) %>% 
  arrange(factor(class_mem, levels = c("broken employment",
                                       "unbroken employment",
                                       "unemployed")),
          factor(measure, levels = c("degree",
                                     "other higher degree",
                                     "a level etc",
                                     "gcse etc",
                                     "other qualification",
                                     "no qualification")))

## calculate totals
ed_attain2_a <- data.frame(svyby(~hiqual_dv, ~emp_spells_class, svy_dfas1a_end_class, svytotal, na.rm=TRUE))
ed_attain2_a <- ed_attain2_a %>% rename(class_mem = emp_spells_class)
names(ed_attain2_a) <- tolower(names(ed_attain2_a)) # change all col names to lower case
ed_attain2_a <- ed_attain2_a %>% 
  dplyr::select(c(1:7)) %>% 
  pivot_longer(cols = 2:7, names_to = "measure") %>% 
  mutate(measure = str_remove(measure,"hiqual_dv")) %>% 
  rename(n=value) %>% 
  mutate(wv_n=6,
         var="educational attainment",
         measure = str_replace(measure,"\\."," "), # need to do this twice!
         measure = str_replace(measure,"\\."," ")) %>% 
  dplyr::select(wv_n,var,class_mem,measure,n) %>% 
  arrange(factor(class_mem, levels = c("broken employment",
                                       "unbroken employment",
                                       "unemployed")),
          factor(measure, levels = c("degree",
                                     "other higher degree",
                                     "a level etc",
                                     "gcse etc",
                                     "other qualification",
                                     "no qualification")))

## join together
ed_attain_a <- ed_attain_a %>% left_join(ed_attain2_a) %>% 
  dplyr::select(wv_n,var,class_mem,measure,n,est,se)

emp_spells_grp <- emp_spells_grp %>% bind_rows(ed_attain_a)

rm(temp, temp2)

### sample B ------------

svymean(~hiqual_dv, svy_dfas1b_end_class, na.rm=TRUE)

## calculate proportions
ed_attain_b <- data.frame(svyby(~hiqual_dv, ~emp_spells_class, svy_dfas1b_end_class, svymean, na.rm=TRUE))
ed_attain_b <- ed_attain_b %>% rename(class_mem = emp_spells_class)
names(ed_attain_b) <- tolower(names(ed_attain_b)) # change all col names to lower case

# create long format data for estimates
temp <- ed_attain_b %>% 
  dplyr::select(c(1:7)) %>% 
  pivot_longer(cols = 2:7, names_to = "measure") %>% 
  rename(est=value) %>% 
  mutate(measure = str_remove(measure,"hiqual_dv"))

# create long format data for SE
temp2 <- ed_attain_b %>% 
  dplyr::select(c(1,8:13)) %>% 
  pivot_longer(cols = 2:7, names_to = "measure") %>% 
  rename(se=value) %>% 
  mutate(measure = str_remove(measure,"se.hiqual_dv"))

# join back together
ed_attain_b <- temp %>% 
  left_join(temp2) %>% 
  mutate(wv_n=6,
         var="educational attainment",
         est=est*100,
         se=se*100,
         measure = str_replace(measure,"\\."," "), # need to do this twice!
         measure = str_replace(measure,"\\."," ")) %>% 
  dplyr::select(wv_n,var,class_mem,measure,est,se) %>% 
  arrange(factor(class_mem, levels = c("broken employment",
                                       "unbroken employment",
                                       "unemployed")),
          factor(measure, levels = c("degree",
                                     "other higher degree",
                                     "a level etc",
                                     "gcse etc",
                                     "other qualification",
                                     "no qualification")))

## calculate totals
ed_attain2_b <- data.frame(svyby(~hiqual_dv, ~emp_spells_class, svy_dfas1b_end_class, svytotal, na.rm=TRUE))
ed_attain2_b <- ed_attain2_b %>% rename(class_mem = emp_spells_class)
names(ed_attain2_b) <- tolower(names(ed_attain2_b)) # change all col names to lower case
ed_attain2_b <- ed_attain2_b %>% 
  dplyr::select(c(1:7)) %>% 
  pivot_longer(cols = 2:7, names_to = "measure") %>% 
  mutate(measure = str_remove(measure,"hiqual_dv")) %>% 
  rename(n=value) %>% 
  mutate(wv_n=6,
         var="educational attainment",
         measure = str_replace(measure,"\\."," "), # need to do this twice!
         measure = str_replace(measure,"\\."," ")) %>% 
  dplyr::select(wv_n,var,class_mem,measure,n) %>% 
  arrange(factor(class_mem, levels = c("broken employment",
                                       "unbroken employment",
                                       "unemployed")),
          factor(measure, levels = c("degree",
                                     "other higher degree",
                                     "a level etc",
                                     "gcse etc",
                                     "other qualification",
                                     "no qualification")))

## join together
ed_attain_b <- ed_attain_b %>% left_join(ed_attain2_b) %>% 
  dplyr::select(wv_n,var,class_mem,measure,n,est,se)

emp_spells_grp <- emp_spells_grp %>% bind_rows(ed_attain_b)

rm(temp, temp2)

################################################################################
#####                   descriptives - multiple employment                 #####
################################################################################

#### sex -----------------------------------------------------------------------

### sample A ------------

svymean(~sex_dv, svy_dfas1a_end_class, na.rm=TRUE)

## calculate proportions
sex_a <- data.frame(svyby(~sex_dv, ~multi_emp_class, svy_dfas1a_end_class, svymean, na.rm=TRUE))
sex_a <- sex_a %>% rename(class_mem = multi_emp_class)
names(sex_a) <- tolower(names(sex_a)) # change all col names to lower case
sex_a <- sex_a %>% pivot_longer(cols = 2:3, names_to = "measure")
sex_a <- sex_a %>% dplyr::select(-se.sex_dvfemale) %>% 
  rename(se = se.sex_dvmale) %>%
  mutate(wv_n=6,
         var="sex",
         measure = str_remove(measure,"sex_dv"),
         est=value*100,
         se=se*100) %>% 
  dplyr::select(wv_n,var,class_mem,measure,est,se) %>% 
  arrange(factor(class_mem, levels = c("multiple employment",
                                       "single employment",
                                       "into employment",
                                       "out of employment",
                                       "unemployed")),
          factor(measure, levels = c("female","male")))

## calculate totals
sex2_a <- data.frame(svyby(~sex_dv, ~multi_emp_class, svy_dfas1a_end_class, svytotal, na.rm=TRUE))
sex2_a <- sex2_a %>% rename(class_mem = multi_emp_class)
names(sex2_a) <- tolower(names(sex2_a)) # change all col names to lower case
sex2_a <- sex2_a %>% 
  pivot_longer(cols = 2:3, names_to = "measure") %>% 
  rename(n=value) %>% 
  mutate(wv_n=6,
         var="sex",
         measure = str_remove(measure,"sex_dv")) %>% 
  dplyr::select(wv_n,var,class_mem,measure,n) %>% 
  arrange(factor(class_mem, levels = c("multiple employment",
                                       "single employment",
                                       "into employment",
                                       "out of employment",
                                       "unemployed")),
          factor(measure, levels = c("female","male")))

## join together
sex_a <- sex_a %>% left_join(sex2_a) %>% 
  dplyr::select(wv_n,var,class_mem,measure,n,est,se)

multi_emp_grp <- sex_a

### sample B ------------

svymean(~sex_dv, svy_dfas1b_end_class, na.rm=TRUE)

## calculate proportions
sex_b <- data.frame(svyby(~sex_dv, ~multi_emp_class, svy_dfas1b_end_class, svymean, na.rm=TRUE))
sex_b <- sex_b %>% rename(class_mem = multi_emp_class)
names(sex_b) <- tolower(names(sex_b)) # change all col names to lower case
sex_b <- sex_b %>% pivot_longer(cols = 2:3, names_to = "measure")
sex_b <- sex_b %>% dplyr::select(-se.sex_dvfemale) %>% 
  rename(se = se.sex_dvmale) %>%
  mutate(wv_n=10,
         var="sex",
         measure = str_remove(measure,"sex_dv"),
         est=value*100,
         se=se*100) %>% 
  dplyr::select(wv_n,var,class_mem,measure,est,se) %>% 
  arrange(factor(class_mem, levels = c("multiple employment",
                                       "single employment",
                                       "into employment",
                                       "out of employment",
                                       "unemployed")),
          factor(measure, levels = c("female","male")))

## calculate totals
sex2_b <- data.frame(svyby(~sex_dv, ~multi_emp_class, svy_dfas1b_end_class, svytotal, na.rm=TRUE))
sex2_b <- sex2_b %>% rename(class_mem = multi_emp_class)
names(sex2_b) <- tolower(names(sex2_b)) # change all col names to lower case
sex2_b <- sex2_b %>% 
  pivot_longer(cols = 2:3, names_to = "measure") %>% 
  rename(n=value) %>% 
  mutate(wv_n=10,
         var="sex",
         measure = str_remove(measure,"sex_dv")) %>% 
  dplyr::select(wv_n,var,class_mem,measure,n) %>% 
  arrange(factor(class_mem, levels = c("multiple employment",
                                       "single employment",
                                       "into employment",
                                       "out of employment",
                                       "unemployed")),
          factor(measure, levels = c("female","male")))

## join together
sex_b <- sex_b %>% left_join(sex2_b) %>% 
  dplyr::select(wv_n,var,class_mem,measure,n,est,se)

multi_emp_grp <- multi_emp_grp %>% bind_rows(sex_b)

#### age -----------------------------------------------------------------------

### sample A ------------

svymean(~age_dv, svy_dfas1a_end_class, na.rm=TRUE)

## calculate proportions
age_a <- data.frame(svyby(~age_dv, ~multi_emp_class, svy_dfas1a_end_class, svymean, na.rm=TRUE))
age_a <- age_a %>% 
  mutate(wv_n = 6,
         measure = "mean",
         var="age",
         n=NA)%>% 
  rename(class_mem = multi_emp_class,
         est = age_dv)  %>% 
  dplyr::select(wv_n,var,class_mem,measure,n,est,se) %>% 
  arrange(factor(class_mem, levels = c("multiple employment",
                                       "single employment",
                                       "into employment",
                                       "out of employment",
                                       "unemployed")))
names(age_a) <- tolower(names(age_a)) # change all col names to lower case

multi_emp_grp <- multi_emp_grp %>% bind_rows(age_a)

### sample B ------------

svymean(~age_dv, svy_dfas1b_end_class, na.rm=TRUE)

## calculate proportions
age_b <- data.frame(svyby(~age_dv, ~multi_emp_class, svy_dfas1b_end_class, svymean, na.rm=TRUE))
age_b <- age_b %>% 
  mutate(wv_n = 10,
         var="age",
         measure = "mean")%>% 
  rename(class_mem = multi_emp_class,
         est = age_dv)  %>% 
  arrange(factor(class_mem, levels = c("multiple employment",
                                       "single employment",
                                       "into employment",
                                       "out of employment",
                                       "unemployed")))
names(age_b) <- tolower(names(age_b)) # change all col names to lower case

multi_emp_grp <- multi_emp_grp %>% bind_rows(age_b)

#### Educational attainment ----------------------------------------------------

### sample A ------------

svymean(~hiqual_dv, svy_dfas1a_end_class, na.rm=TRUE)

## calculate proportions
ed_attain_a <- data.frame(svyby(~hiqual_dv, ~multi_emp_class, svy_dfas1a_end_class, svymean, na.rm=TRUE))
ed_attain_a <- ed_attain_a %>% rename(class_mem = multi_emp_class)
names(ed_attain_a) <- tolower(names(ed_attain_a)) # change all col names to lower case

# create long format data for estimates
temp <- ed_attain_a %>% 
  dplyr::select(c(1:7)) %>% 
  pivot_longer(cols = 2:7, names_to = "measure") %>% 
  rename(est=value) %>% 
  mutate(measure = str_remove(measure,"hiqual_dv"))

# create long format data for SE
temp2 <- ed_attain_a %>% 
  dplyr::select(c(1,8:13)) %>% 
  pivot_longer(cols = 2:7, names_to = "measure") %>% 
  rename(se=value) %>% 
  mutate(measure = str_remove(measure,"se.hiqual_dv"))

# join back together
ed_attain_a <- temp %>% 
  left_join(temp2) %>% 
  mutate(wv_n=6,
         var="educational attainment",
         est=est*100,
         se=se*100,
         measure = str_replace(measure,"\\."," "), # need to do this twice!
         measure = str_replace(measure,"\\."," ")) %>% 
  dplyr::select(wv_n,var,class_mem,measure,est,se) %>% 
  arrange(factor(class_mem, levels = c("multiple employment",
                                       "single employment",
                                       "into employment",
                                       "out of employment",
                                       "unemployed")),
          factor(measure, levels = c("degree",
                                     "other higher degree",
                                     "a level etc",
                                     "gcse etc",
                                     "other qualification",
                                     "no qualification")))

## calculate totals
ed_attain2_a <- data.frame(svyby(~hiqual_dv, ~multi_emp_class, svy_dfas1a_end_class, svytotal, na.rm=TRUE))
ed_attain2_a <- ed_attain2_a %>% rename(class_mem = multi_emp_class)
names(ed_attain2_a) <- tolower(names(ed_attain2_a)) # change all col names to lower case
ed_attain2_a <- ed_attain2_a %>% 
  dplyr::select(c(1:7)) %>% 
  pivot_longer(cols = 2:7, names_to = "measure") %>% 
  mutate(measure = str_remove(measure,"hiqual_dv")) %>% 
  rename(n=value) %>% 
  mutate(wv_n=6,
         var="educational attainment",
         measure = str_replace(measure,"\\."," "), # need to do this twice!
         measure = str_replace(measure,"\\."," ")) %>% 
  dplyr::select(wv_n,var,class_mem,measure,n) %>% 
  arrange(factor(class_mem, levels = c("multiple employment",
                                       "single employment",
                                       "into employment",
                                       "out of employment",
                                       "unemployed")),
          factor(measure, levels = c("degree",
                                     "other higher degree",
                                     "a level etc",
                                     "gcse etc",
                                     "other qualification",
                                     "no qualification")))

## join together
ed_attain_a <- ed_attain_a %>% left_join(ed_attain2_a) %>% 
  dplyr::select(wv_n,var,class_mem,measure,n,est,se)

multi_emp_grp <- multi_emp_grp %>% bind_rows(ed_attain_a)

rm(temp, temp2)

### sample B ------------

svymean(~hiqual_dv, svy_dfas1b_end_class, na.rm=TRUE)

## calculate proportions
ed_attain_b <- data.frame(svyby(~hiqual_dv, ~multi_emp_class, svy_dfas1b_end_class, svymean, na.rm=TRUE))
ed_attain_b <- ed_attain_b %>% rename(class_mem = multi_emp_class)
names(ed_attain_b) <- tolower(names(ed_attain_b)) # change all col names to lower case

# create long format data for estimates
temp <- ed_attain_b %>% 
  dplyr::select(c(1:7)) %>% 
  pivot_longer(cols = 2:7, names_to = "measure") %>% 
  rename(est=value) %>% 
  mutate(measure = str_remove(measure,"hiqual_dv"))

# create long format data for SE
temp2 <- ed_attain_b %>% 
  dplyr::select(c(1,8:13)) %>% 
  pivot_longer(cols = 2:7, names_to = "measure") %>% 
  rename(se=value) %>% 
  mutate(measure = str_remove(measure,"se.hiqual_dv"))

# join back together
ed_attain_b <- temp %>% 
  left_join(temp2) %>% 
  mutate(wv_n=6,
         var="educational attainment",
         est=est*100,
         se=se*100,
         measure = str_replace(measure,"\\."," "), # need to do this twice!
         measure = str_replace(measure,"\\."," ")) %>% 
  dplyr::select(wv_n,var,class_mem,measure,est,se) %>% 
  arrange(factor(class_mem, levels = c("multiple employment",
                                       "single employment",
                                       "into employment",
                                       "out of employment",
                                       "unemployed")),
          factor(measure, levels = c("degree",
                                     "other higher degree",
                                     "a level etc",
                                     "gcse etc",
                                     "other qualification",
                                     "no qualification")))

## calculate totals
ed_attain2_b <- data.frame(svyby(~hiqual_dv, ~multi_emp_class, svy_dfas1b_end_class, svytotal, na.rm=TRUE))
ed_attain2_b <- ed_attain2_b %>% rename(class_mem = multi_emp_class)
names(ed_attain2_b) <- tolower(names(ed_attain2_b)) # change all col names to lower case
ed_attain2_b <- ed_attain2_b %>% 
  dplyr::select(c(1:7)) %>% 
  pivot_longer(cols = 2:7, names_to = "measure") %>% 
  mutate(measure = str_remove(measure,"hiqual_dv")) %>% 
  rename(n=value) %>% 
  mutate(wv_n=6,
         var="educational attainment",
         measure = str_replace(measure,"\\."," "), # need to do this twice!
         measure = str_replace(measure,"\\."," ")) %>% 
  dplyr::select(wv_n,var,class_mem,measure,n) %>% 
  arrange(factor(class_mem, levels = c("multiple employment",
                                       "single employment",
                                       "into employment",
                                       "out of employment",
                                       "unemployed")),
          factor(measure, levels = c("degree",
                                     "other higher degree",
                                     "a level etc",
                                     "gcse etc",
                                     "other qualification",
                                     "no qualification")))

## join together
ed_attain_b <- ed_attain_b %>% left_join(ed_attain2_b) %>% 
  dplyr::select(wv_n,var,class_mem,measure,n,est,se)

multi_emp_grp <- multi_emp_grp %>% bind_rows(ed_attain_b)

rm(temp, temp2)

## add CI calculations
## save outputs


