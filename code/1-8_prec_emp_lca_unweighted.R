################################################################################

# Persistent precarious employment and health - Understanding Society
# 1-8 - latent class analysis - unweighted
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

### employment contract 
dfas1a_seq_wide <- readRDS("./working_data/dfas1a_seq_wide.rds")
dfas1b_seq_wide <- readRDS("./working_data/dfas1b_seq_wide.rds")

### broken employment spells
dfas1a_seq_wide2 <- readRDS("./working_data/dfas1a_seq_wide2.rds")
dfas1b_seq_wide2 <- readRDS("./working_data/dfas1b_seq_wide2.rds")

### multiple jobs
dfas1a_seq_wide3 <- readRDS("./working_data/dfas1a_seq_wide3.rds")
dfas1b_seq_wide3 <- readRDS("./working_data/dfas1b_seq_wide3.rds")


################################################################################
#####                         Latent class analysis                        #####
################################################################################

#### ---------------------------------------------------------------------------
#### Employment contract
#### ---------------------------------------------------------------------------

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
temp <- empcontracta_lca_fit_stats %>%  filter(bic==min(bic))
empcontract_min_bic_a <- temp[,1]
rm(temp)  

temp <- empcontract_min_bic_b <- empcontractb_lca_fit_stats %>%  filter(bic==min(bic))
empcontract_min_bic_b <- temp[,1]
rm(temp)  

# bic elbow plot
tiff("./output/descriptive/empcontracta_lca_elbow.tiff", width = 400, height = 400)
empcontracta_lca_fit_stats %>% dplyr::select(nclass, bic) %>% 
  ggplot(aes(x=nclass,y=bic)) + 
  geom_line() +
  geom_vline(xintercept=empcontract_min_bic_a, colour="dark green", linetype = "longdash") +
  theme_bw()
dev.off()

tiff("./output/descriptive/empcontractb_lca_elbow.tiff", width = 400, height = 400)
empcontractb_lca_fit_stats %>% dplyr::select(nclass, bic) %>% 
  ggplot(aes(x=nclass,y=bic)) + 
  geom_line() +
  geom_vline(xintercept=empcontract_min_bic_b, colour="dark green", linetype = "longdash") +
  theme_bw()
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


#### ---------------------------------------------------------------------------
#### Broken employment spells
#### ---------------------------------------------------------------------------

## recode missing values to NA
dfas1a_seq_wide2 <- dfas1a_seq_wide2 %>%  mutate(across(where(is.character), ~na_if(., "missing")))
dfas1b_seq_wide2 <- dfas1b_seq_wide2 %>%  mutate(across(where(is.character), ~na_if(., "missing")))


## convert variables to factors so that they have numeric values
dfas1a_seq_wide2$wv_3 <- factor(dfas1a_seq_wide2$wv_3, levels = c("Broken employment", "Unbroken employment", "No employment spells"))
dfas1a_seq_wide2$wv_4 <- factor(dfas1a_seq_wide2$wv_4, levels = c("Broken employment", "Unbroken employment", "No employment spells"))
dfas1a_seq_wide2$wv_5 <- factor(dfas1a_seq_wide2$wv_5, levels = c("Broken employment", "Unbroken employment", "No employment spells"))
dfas1a_seq_wide2$wv_6 <- factor(dfas1a_seq_wide2$wv_6, levels = c("Broken employment", "Unbroken employment", "No employment spells"))

dfas1b_seq_wide2$wv_7 <- factor(dfas1b_seq_wide2$wv_7, levels = c("Broken employment", "Unbroken employment", "No employment spells"))
dfas1b_seq_wide2$wv_8 <- factor(dfas1b_seq_wide2$wv_8, levels = c("Broken employment", "Unbroken employment", "No employment spells"))
dfas1b_seq_wide2$wv_9 <- factor(dfas1b_seq_wide2$wv_9, levels = c("Broken employment", "Unbroken employment", "No employment spells"))
dfas1b_seq_wide2$wv_10 <- factor(dfas1b_seq_wide2$wv_10, levels = c("Broken employment", "Unbroken employment", "No employment spells"))

# check levels
levels(dfas1a_seq_wide2$wv_3)
levels(dfas1a_seq_wide2$wv_4)
levels(dfas1a_seq_wide2$wv_5)
levels(dfas1a_seq_wide2$wv_6)

levels(dfas1b_seq_wide2$wv_7)
levels(dfas1b_seq_wide2$wv_8)
levels(dfas1b_seq_wide2$wv_9)
levels(dfas1b_seq_wide2$wv_10)

### start with basic formulation (no covariates) --------------------

f <- cbind(wv_3, wv_4,wv_5, wv_6) ~ 1
f2 <- cbind(wv_7, wv_8,wv_9, wv_10) ~ 1

### no covariates, 1 class --------------------
empspells_lca1a <- poLCA(f, dfas1a_seq_wide2, nclass = 1, maxiter = 4000,graphs = TRUE, na.rm = FALSE)

empspells_lca1b <- poLCA(f2, dfas1b_seq_wide2, nclass = 1, maxiter = 4000,graphs = TRUE, na.rm = FALSE)

# entropy -- NOTE: poLCA uses non-normalised entropy, how to interpret?
empspells_lca1a_ent <- poLCA.entropy(empspells_lca1a)

empspells_lca1b_ent <- poLCA.entropy(empspells_lca1b)

## bind the predicted class for each case back onto wide df
dfas1a_pred_class2 <-cbind(dfas1a_seq_wide2, "pred_class1" = empspells_lca1a$predclass)

dfas1b_pred_class2 <-cbind(dfas1b_seq_wide2, "pred_class1" = empspells_lca1b$predclass)


## create totals for class membership
empspells_lca1a_memtotals <- dfas1a_pred_class2 %>% 
  group_by(pred_class1) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

empspells_lca1b_memtotals <- dfas1b_pred_class2 %>% 
  group_by(pred_class1) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

### no covariates, 2 class --------------------
empspells_lca2a <- poLCA(f, dfas1a_seq_wide2, nclass = 2, maxiter = 4000,graphs = TRUE, na.rm = FALSE)

empspells_lca2b <- poLCA(f2, dfas1b_seq_wide2, nclass = 2, maxiter = 4000,graphs = TRUE, na.rm = FALSE)

# entropy -- NOTE: poLCA uses non-normalised entropy, how to interpret?
empspells_lca2a_ent <- poLCA.entropy(empspells_lca2a)

empspells_lca2b_ent <- poLCA.entropy(empspells_lca2b)

## bind the predicted class for each case back onto wide df
dfas1a_pred_class2 <-cbind(dfas1a_pred_class2, "pred_class2" = empspells_lca2a$predclass)

dfas1b_pred_class2 <-cbind(dfas1b_pred_class2, "pred_class2" = empspells_lca2b$predclass)


## create totals for class membership
empspells_lca2a_memtotals <- dfas1a_pred_class2 %>% 
  group_by(pred_class2) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

empspells_lca2b_memtotals <- dfas1b_pred_class2 %>% 
  group_by(pred_class2) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

### no covariates, 3 class --------------------
empspells_lca3a <- poLCA(f, dfas1a_seq_wide2, nclass = 3, maxiter = 4000,graphs = TRUE, na.rm = FALSE)

empspells_lca3b <- poLCA(f2, dfas1b_seq_wide2, nclass = 3, maxiter = 4000,graphs = TRUE, na.rm = FALSE)

# entropy -- NOTE: poLCA uses non-normalised entropy, how to interpret?
empspells_lca3a_ent <- poLCA.entropy(empspells_lca3a)

empspells_lca3b_ent <- poLCA.entropy(empspells_lca3b)

## bind the predicted class for each case back onto wide df
dfas1a_pred_class2 <-cbind(dfas1a_pred_class2, "pred_class3" = empspells_lca3a$predclass)

dfas1b_pred_class2 <-cbind(dfas1b_pred_class2, "pred_class3" = empspells_lca3b$predclass)


## create totals for class membership
empspells_lca3a_memtotals <- dfas1a_pred_class2 %>% 
  group_by(pred_class3) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

empspells_lca3b_memtotals <- dfas1b_pred_class2 %>% 
  group_by(pred_class3) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

### no covariates, 4 class --------------------
empspells_lca4a <- poLCA(f, dfas1a_seq_wide2, nclass = 4, maxiter = 4000,graphs = TRUE, na.rm = FALSE)

empspells_lca4b <- poLCA(f2, dfas1b_seq_wide2, nclass = 4, maxiter = 4000,graphs = TRUE, na.rm = FALSE)

# entropy -- NOTE: poLCA uses non-normalised entropy, how to interpret?
empspells_lca4a_ent <- poLCA.entropy(empspells_lca4a)

empspells_lca4b_ent <- poLCA.entropy(empspells_lca4b)

## bind the predicted class for each case back onto wide df
dfas1a_pred_class2 <-cbind(dfas1a_pred_class2, "pred_class4" = empspells_lca4a$predclass)

dfas1b_pred_class2 <-cbind(dfas1b_pred_class2, "pred_class4" = empspells_lca4b$predclass)


## create totals for class membership
empspells_lca4a_memtotals <- dfas1a_pred_class2 %>% 
  group_by(pred_class4) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

empspells_lca4b_memtotals <- dfas1b_pred_class2 %>% 
  group_by(pred_class4) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

### no covariates, 5 class --------------------
empspells_lca5a <- poLCA(f, dfas1a_seq_wide2, nclass = 5, maxiter = 4000,graphs = TRUE, na.rm = FALSE)

empspells_lca5b <- poLCA(f2, dfas1b_seq_wide2, nclass = 5, maxiter = 4000,graphs = TRUE, na.rm = FALSE)

# entropy -- NOTE: poLCA uses non-normalised entropy, how to interpret?
empspells_lca5a_ent <- poLCA.entropy(empspells_lca5a)

empspells_lca5b_ent <- poLCA.entropy(empspells_lca5b)

## bind the predicted class for each case back onto wide df
dfas1a_pred_class2 <-cbind(dfas1a_pred_class2, "pred_class5" = empspells_lca5a$predclass)

dfas1b_pred_class2 <-cbind(dfas1b_pred_class2, "pred_class5" = empspells_lca5b$predclass)


## create totals for class membership
empspells_lca5a_memtotals <- dfas1a_pred_class2 %>% 
  group_by(pred_class5) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

empspells_lca5b_memtotals <- dfas1b_pred_class2 %>% 
  group_by(pred_class5) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

### no covariates, 6 class --------------------
empspells_lca6a <- poLCA(f, dfas1a_seq_wide2, nclass = 6, maxiter = 4000,graphs = FALSE, na.rm = FALSE)

empspells_lca6b <- poLCA(f2, dfas1b_seq_wide2, nclass = 6, maxiter = 4000,graphs = FALSE, na.rm = FALSE)

# entropy -- NOTE: poLCA uses non-normalised entropy, how to interpret?
empspells_lca6a_ent <- poLCA.entropy(empspells_lca6a)

empspells_lca6b_ent <- poLCA.entropy(empspells_lca6b)

## bind the predicted class for each case back onto wide df
dfas1a_pred_class2 <-cbind(dfas1a_pred_class2, "pred_class6" = empspells_lca6a$predclass)

dfas1b_pred_class2 <-cbind(dfas1b_pred_class2, "pred_class6" = empspells_lca6b$predclass)


## create totals for class membership
empspells_lca6a_memtotals <- dfas1a_pred_class2 %>% 
  group_by(pred_class6) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

empspells_lca6b_memtotals <- dfas1b_pred_class2 %>% 
  group_by(pred_class6) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

### no covariates, 7 class --------------------
empspells_lca7a <- poLCA(f, dfas1a_seq_wide2, nclass = 7, maxiter = 4000,graphs = FALSE, na.rm = FALSE)

empspells_lca7b <- poLCA(f2, dfas1b_seq_wide2, nclass = 7, maxiter = 4000,graphs = FALSE, na.rm = FALSE)

# entropy -- NOTE: poLCA uses non-normalised entropy, how to interpret?
empspells_lca7a_ent <- poLCA.entropy(empspells_lca7a)

empspells_lca7b_ent <- poLCA.entropy(empspells_lca7b)

## bind the predicted class for each case back onto wide df
dfas1a_pred_class2 <-cbind(dfas1a_pred_class2, "pred_class7" = empspells_lca7a$predclass)

dfas1b_pred_class2 <-cbind(dfas1b_pred_class2, "pred_class7" = empspells_lca7b$predclass)


## create totals for class membership
empspells_lca7a_memtotals <- dfas1a_pred_class2 %>% 
  group_by(pred_class7) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

empspells_lca7b_memtotals <- dfas1b_pred_class2 %>% 
  group_by(pred_class7) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

### no covariates, 8 class --------------------
empspells_lca8a <- poLCA(f, dfas1a_seq_wide2, nclass = 8, maxiter = 4000,graphs = FALSE, na.rm = FALSE)

empspells_lca8b <- poLCA(f2, dfas1b_seq_wide2, nclass = 8, maxiter = 4000,graphs = FALSE, na.rm = FALSE)

# entropy -- NOTE: poLCA uses non-normalised entropy, how to interpret?
empspells_lca8a_ent <- poLCA.entropy(empspells_lca8a)

empspells_lca8b_ent <- poLCA.entropy(empspells_lca8b)

## bind the predicted class for each case back onto wide df
dfas1a_pred_class2 <-cbind(dfas1a_pred_class2, "pred_class8" = empspells_lca8a$predclass)

dfas1b_pred_class2 <-cbind(dfas1b_pred_class2, "pred_class8" = empspells_lca8b$predclass)


## create totals for class membership
empspells_lca8a_memtotals <- dfas1a_pred_class2 %>% 
  group_by(pred_class8) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

empspells_lca8b_memtotals <- dfas1b_pred_class2 %>% 
  group_by(pred_class8) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

### model fit stats --------------------

nclass_vector <- c(2:8)
bic_vector2 <- c(empspells_lca2a$bic, empspells_lca3a$bic, empspells_lca4a$bic, 
                empspells_lca5a$bic, empspells_lca6a$bic, empspells_lca7a$bic, 
                empspells_lca8a$bic)
aic_vector2 <- c(empspells_lca2a$aic, empspells_lca3a$aic, empspells_lca4a$aic, 
                empspells_lca5a$aic, empspells_lca6a$aic, empspells_lca7a$aic, 
                empspells_lca8a$aic)
Gsq_vector2 <- c(empspells_lca2a$Gsq, empspells_lca3a$Gsq, empspells_lca4a$Gsq, 
                empspells_lca5a$Gsq, empspells_lca6a$Gsq, empspells_lca7a$Gsq, 
                empspells_lca8a$Gsq)
Chisq_vector2 <- c(empspells_lca2a$Chisq, empspells_lca3a$Chisq, 
                  empspells_lca4a$Chisq, empspells_lca5a$Chisq, 
                  empspells_lca6a$Chisq, empspells_lca7a$Chisq, 
                  empspells_lca8a$Chisq)
entropy_vector2 <- c(empspells_lca2a_ent,empspells_lca3a_ent, 
                    empspells_lca4a_ent, empspells_lca5a_ent, 
                    empspells_lca6a_ent, empspells_lca7a_ent, 
                    empspells_lca8a_ent)

bic_vector2_b <- c(empspells_lca2b$bic, empspells_lca3b$bic, empspells_lca4b$bic, 
                  empspells_lca5b$bic, empspells_lca6b$bic, empspells_lca7b$bic, 
                  empspells_lca8b$bic)
aic_vector2_b <- c(empspells_lca2b$aic, empspells_lca3b$aic, empspells_lca4b$aic, 
                  empspells_lca5b$aic, empspells_lca6b$aic, empspells_lca7b$aic, 
                  empspells_lca8b$aic)
Gsq_vector2_b <- c(empspells_lca2b$Gsq, empspells_lca3b$Gsq, empspells_lca4b$Gsq, 
                  empspells_lca5b$Gsq, empspells_lca6b$Gsq, empspells_lca7b$Gsq, 
                  empspells_lca8b$Gsq)
Chisq_vector2_b <- c(empspells_lca2b$Chisq, empspells_lca3b$Chisq, 
                    empspells_lca4b$Chisq, empspells_lca5b$Chisq, 
                    empspells_lca6b$Chisq, empspells_lca7b$Chisq, 
                    empspells_lca8b$Chisq)
entropy_vector2_b <- c(empspells_lca2b_ent,empspells_lca3b_ent, 
                      empspells_lca4b_ent, empspells_lca5b_ent, 
                      empspells_lca6b_ent, empspells_lca7b_ent, 
                      empspells_lca8b_ent)

## create df for model fit stats
empspellsa_lca_fit_stats <- data.frame(cbind(nclass_vector, bic_vector, aic_vector,Gsq_vector, Chisq_vector, entropy_vector))
names(empspellsa_lca_fit_stats) <- c("nclass", "bic", "aic", "Gsq", "Chisq", "entropy")

empspellsb_lca_fit_stats <- data.frame(cbind(nclass_vector, bic_vector_b, aic_vector_b,Gsq_vector_b, Chisq_vector_b, entropy_vector_b))
names(empspellsb_lca_fit_stats) <- c("nclass", "bic", "aic", "Gsq", "Chisq", "entropy")

# model with lowest bic
temp <- empspellsa_lca_fit_stats %>%  filter(bic==min(bic))
empspells_min_bic_a <- temp[,1]
rm(temp)  

temp <- empspells_min_bic_b <- empspellsb_lca_fit_stats %>%  filter(bic==min(bic))
empspells_min_bic_b <- temp[,1]
rm(temp)  

# bic elbow plot
tiff("./output/descriptive/empspellsa_lca_elbow.tiff", width = 400, height = 400)
empspellsa_lca_fit_stats %>% dplyr::select(nclass, bic) %>% 
  ggplot(aes(x=nclass,y=bic)) + 
  geom_line() +
  geom_vline(xintercept=empspells_min_bic_a, colour="dark green", linetype = "longdash") +
  theme_bw()
dev.off()

tiff("./output/descriptive/empspellsb_lca_elbow.tiff", width = 400, height = 400)
empspellsb_lca_fit_stats %>% dplyr::select(nclass, bic) %>% 
  ggplot(aes(x=nclass,y=bic)) + 
  geom_line() +
  geom_vline(xintercept=empspells_min_bic_b, colour="dark green", linetype = "longdash") +
  theme_bw()
dev.off()

# model with lowest aic
empspellsa_lca_fit_stats %>%  filter(aic==min(aic))

empspellsb_lca_fit_stats %>%  filter(aic==min(aic))

# aic elbow plot
empspellsa_lca_fit_stats %>% dplyr::select(nclass, aic) %>% 
  ggplot(aes(x=nclass,y=aic)) + 
  geom_line()

empspellsb_lca_fit_stats %>% dplyr::select(nclass, aic) %>% 
  ggplot(aes(x=nclass,y=aic)) + 
  geom_line()

## save fit_stats
write.csv(empspellsa_lca_fit_stats, "./output/descriptive/empspellsa_lca_fit_stats.csv")

write.csv(empspellsb_lca_fit_stats, "./output/descriptive/empspellsb_lca_fit_stats.csv")

#### ---------------------------------------------------------------------------
#### Multiple employment
#### ---------------------------------------------------------------------------

## recode missing values to NA
dfas1a_seq_wide3 <- dfas1a_seq_wide3 %>%  mutate(across(where(is.character), ~na_if(., "missing")))
dfas1b_seq_wide3 <- dfas1b_seq_wide3 %>%  mutate(across(where(is.character), ~na_if(., "missing")))


## convert variables to factors so that they have numeric values
dfas1a_seq_wide3$wv_3 <- factor(dfas1a_seq_wide3$wv_3, levels = c("multiple jobs", "one job", 
                                                                  "unemployed/not in employment",
                                                                  "unemployed/not in employment with additional"))
dfas1a_seq_wide3$wv_4 <- factor(dfas1a_seq_wide3$wv_4, levels = c("multiple jobs", "one job", 
                                                                  "unemployed/not in employment",
                                                                  "unemployed/not in employment with additional"))
dfas1a_seq_wide3$wv_5 <- factor(dfas1a_seq_wide3$wv_5, levels = c("multiple jobs", "one job", 
                                                                  "unemployed/not in employment",
                                                                  "unemployed/not in employment with additional"))
dfas1a_seq_wide3$wv_6 <- factor(dfas1a_seq_wide3$wv_6, levels = c("multiple jobs", "one job", 
                                                                  "unemployed/not in employment",
                                                                  "unemployed/not in employment with additional"))

dfas1b_seq_wide3$wv_7 <- factor(dfas1b_seq_wide3$wv_7, levels = c("multiple jobs", "one job", 
                                                                  "unemployed/not in employment",
                                                                  "unemployed/not in employment with additional"))
dfas1b_seq_wide3$wv_8 <- factor(dfas1b_seq_wide3$wv_8, levels = c("multiple jobs", "one job", 
                                                                  "unemployed/not in employment",
                                                                  "unemployed/not in employment with additional"))
dfas1b_seq_wide3$wv_9 <- factor(dfas1b_seq_wide3$wv_9, levels = c("multiple jobs", "one job", 
                                                                  "unemployed/not in employment",
                                                                  "unemployed/not in employment with additional"))
dfas1b_seq_wide3$wv_10 <- factor(dfas1b_seq_wide3$wv_10, levels = c("multiple jobs", "one job", 
                                                                    "unemployed/not in employment",
                                                                    "unemployed/not in employment with additional"))

# check levels
levels(dfas1a_seq_wide3$wv_3)
levels(dfas1a_seq_wide3$wv_4)
levels(dfas1a_seq_wide3$wv_5)
levels(dfas1a_seq_wide3$wv_6)

levels(dfas1b_seq_wide3$wv_7)
levels(dfas1b_seq_wide3$wv_8)
levels(dfas1b_seq_wide3$wv_9)
levels(dfas1b_seq_wide3$wv_10)

### start with basic formulation (no covariates) --------------------

f <- cbind(wv_3, wv_4,wv_5, wv_6) ~ 1
f2 <- cbind(wv_7, wv_8,wv_9, wv_10) ~ 1

### no covariates, 1 class --------------------
multi_emp_lca1a <- poLCA(f, dfas1a_seq_wide3, nclass = 1, maxiter = 4000,graphs = TRUE, na.rm = FALSE)

multi_emp_lca1b <- poLCA(f2, dfas1b_seq_wide3, nclass = 1, maxiter = 4000,graphs = TRUE, na.rm = FALSE)

# entropy -- NOTE: poLCA uses non-normalised entropy, how to interpret?
multi_emp_lca1a_ent <- poLCA.entropy(multi_emp_lca1a)

multi_emp_lca1b_ent <- poLCA.entropy(multi_emp_lca1b)

## bind the predicted class for each case back onto wide df
dfas1a_pred_class3 <-cbind(dfas1a_seq_wide3, "pred_class1" = multi_emp_lca1a$predclass)

dfas1b_pred_class3 <-cbind(dfas1b_seq_wide3, "pred_class1" = multi_emp_lca1b$predclass)


## create totals for class membership
multi_emp_lca1a_memtotals <- dfas1a_pred_class3 %>% 
  group_by(pred_class1) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

multi_emp_lca1b_memtotals <- dfas1b_pred_class3 %>% 
  group_by(pred_class1) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

### no covariates, 2 class --------------------
multi_emp_lca2a <- poLCA(f, dfas1a_seq_wide3, nclass = 2, maxiter = 4000,graphs = TRUE, na.rm = FALSE)

multi_emp_lca2b <- poLCA(f2, dfas1b_seq_wide3, nclass = 2, maxiter = 4000,graphs = TRUE, na.rm = FALSE)

# entropy -- NOTE: poLCA uses non-normalised entropy, how to interpret?
multi_emp_lca2a_ent <- poLCA.entropy(multi_emp_lca2a)

multi_emp_lca2b_ent <- poLCA.entropy(multi_emp_lca2b)

## bind the predicted class for each case back onto wide df
dfas1a_pred_class3 <-cbind(dfas1a_pred_class3, "pred_class2" = multi_emp_lca2a$predclass)

dfas1b_pred_class3 <-cbind(dfas1b_pred_class3, "pred_class2" = multi_emp_lca2b$predclass)


## create totals for class membership
multi_emp_lca2a_memtotals <- dfas1a_pred_class3 %>% 
  group_by(pred_class2) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

multi_emp_lca2b_memtotals <- dfas1b_pred_class3 %>% 
  group_by(pred_class2) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

### no covariates, 3 class --------------------
multi_emp_lca3a <- poLCA(f, dfas1a_seq_wide3, nclass = 3, maxiter = 4000,graphs = TRUE, na.rm = FALSE)

multi_emp_lca3b <- poLCA(f2, dfas1b_seq_wide3, nclass = 3, maxiter = 4000,graphs = TRUE, na.rm = FALSE)

# entropy -- NOTE: poLCA uses non-normalised entropy, how to interpret?
multi_emp_lca3a_ent <- poLCA.entropy(multi_emp_lca3a)

multi_emp_lca3b_ent <- poLCA.entropy(multi_emp_lca3b)

## bind the predicted class for each case back onto wide df
dfas1a_pred_class3 <-cbind(dfas1a_pred_class3, "pred_class3" = multi_emp_lca3a$predclass)

dfas1b_pred_class3 <-cbind(dfas1b_pred_class3, "pred_class3" = multi_emp_lca3b$predclass)


## create totals for class membership
multi_emp_lca3a_memtotals <- dfas1a_pred_class3 %>% 
  group_by(pred_class3) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

multi_emp_lca3b_memtotals <- dfas1b_pred_class3 %>% 
  group_by(pred_class3) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

### no covariates, 4 class --------------------
multi_emp_lca4a <- poLCA(f, dfas1a_seq_wide3, nclass = 4, maxiter = 4000,graphs = TRUE, na.rm = FALSE)

multi_emp_lca4b <- poLCA(f2, dfas1b_seq_wide3, nclass = 4, maxiter = 4000,graphs = TRUE, na.rm = FALSE)

# entropy -- NOTE: poLCA uses non-normalised entropy, how to interpret?
multi_emp_lca4a_ent <- poLCA.entropy(multi_emp_lca4a)

multi_emp_lca4b_ent <- poLCA.entropy(multi_emp_lca4b)

## bind the predicted class for each case back onto wide df
dfas1a_pred_class3 <-cbind(dfas1a_pred_class3, "pred_class4" = multi_emp_lca4a$predclass)

dfas1b_pred_class3 <-cbind(dfas1b_pred_class3, "pred_class4" = multi_emp_lca4b$predclass)


## create totals for class membership
multi_emp_lca4a_memtotals <- dfas1a_pred_class3 %>% 
  group_by(pred_class4) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

multi_emp_lca4b_memtotals <- dfas1b_pred_class3 %>% 
  group_by(pred_class4) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

### no covariates, 5 class --------------------
multi_emp_lca5a <- poLCA(f, dfas1a_seq_wide3, nclass = 5, maxiter = 4000,graphs = TRUE, na.rm = FALSE)

multi_emp_lca5b <- poLCA(f2, dfas1b_seq_wide3, nclass = 5, maxiter = 4000,graphs = TRUE, na.rm = FALSE)

# entropy -- NOTE: poLCA uses non-normalised entropy, how to interpret?
multi_emp_lca5a_ent <- poLCA.entropy(multi_emp_lca5a)

multi_emp_lca5b_ent <- poLCA.entropy(multi_emp_lca5b)

## bind the predicted class for each case back onto wide df
dfas1a_pred_class3 <-cbind(dfas1a_pred_class3, "pred_class5" = multi_emp_lca5a$predclass)

dfas1b_pred_class3 <-cbind(dfas1b_pred_class3, "pred_class5" = multi_emp_lca5b$predclass)


## create totals for class membership
multi_emp_lca5a_memtotals <- dfas1a_pred_class3 %>% 
  group_by(pred_class5) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

multi_emp_lca5b_memtotals <- dfas1b_pred_class3 %>% 
  group_by(pred_class5) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

### no covariates, 6 class --------------------
multi_emp_lca6a <- poLCA(f, dfas1a_seq_wide3, nclass = 6, maxiter = 4000,graphs = FALSE, na.rm = FALSE)

multi_emp_lca6b <- poLCA(f2, dfas1b_seq_wide3, nclass = 6, maxiter = 4000,graphs = FALSE, na.rm = FALSE)

# entropy -- NOTE: poLCA uses non-normalised entropy, how to interpret?
multi_emp_lca6a_ent <- poLCA.entropy(multi_emp_lca6a)

multi_emp_lca6b_ent <- poLCA.entropy(multi_emp_lca6b)

## bind the predicted class for each case back onto wide df
dfas1a_pred_class3 <-cbind(dfas1a_pred_class3, "pred_class6" = multi_emp_lca6a$predclass)

dfas1b_pred_class3 <-cbind(dfas1b_pred_class3, "pred_class6" = multi_emp_lca6b$predclass)


## create totals for class membership
multi_emp_lca6a_memtotals <- dfas1a_pred_class3 %>% 
  group_by(pred_class6) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

multi_emp_lca6b_memtotals <- dfas1b_pred_class3 %>% 
  group_by(pred_class6) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

### no covariates, 7 class --------------------
multi_emp_lca7a <- poLCA(f, dfas1a_seq_wide3, nclass = 7, maxiter = 4000,graphs = FALSE, na.rm = FALSE)

multi_emp_lca7b <- poLCA(f2, dfas1b_seq_wide3, nclass = 7, maxiter = 4000,graphs = FALSE, na.rm = FALSE)

# entropy -- NOTE: poLCA uses non-normalised entropy, how to interpret?
multi_emp_lca7a_ent <- poLCA.entropy(multi_emp_lca7a)

multi_emp_lca7b_ent <- poLCA.entropy(multi_emp_lca7b)

## bind the predicted class for each case back onto wide df
dfas1a_pred_class3 <-cbind(dfas1a_pred_class3, "pred_class7" = multi_emp_lca7a$predclass)

dfas1b_pred_class3 <-cbind(dfas1b_pred_class3, "pred_class7" = multi_emp_lca7b$predclass)


## create totals for class membership
multi_emp_lca7a_memtotals <- dfas1a_pred_class3 %>% 
  group_by(pred_class7) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

multi_emp_lca7b_memtotals <- dfas1b_pred_class3 %>% 
  group_by(pred_class7) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

### no covariates, 8 class --------------------
multi_emp_lca8a <- poLCA(f, dfas1a_seq_wide3, nclass = 8, maxiter = 4000,graphs = FALSE, na.rm = FALSE)

multi_emp_lca8b <- poLCA(f2, dfas1b_seq_wide3, nclass = 8, maxiter = 4000,graphs = FALSE, na.rm = FALSE)

# entropy -- NOTE: poLCA uses non-normalised entropy, how to interpret?
multi_emp_lca8a_ent <- poLCA.entropy(multi_emp_lca8a)

multi_emp_lca8b_ent <- poLCA.entropy(multi_emp_lca8b)

## bind the predicted class for each case back onto wide df
dfas1a_pred_class3 <-cbind(dfas1a_pred_class3, "pred_class8" = multi_emp_lca8a$predclass)

dfas1b_pred_class3 <-cbind(dfas1b_pred_class3, "pred_class8" = multi_emp_lca8b$predclass)


## create totals for class membership
multi_emp_lca8a_memtotals <- dfas1a_pred_class3 %>% 
  group_by(pred_class8) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

multi_emp_lca8b_memtotals <- dfas1b_pred_class3 %>% 
  group_by(pred_class8) %>% 
  summarise(n=n()) %>% 
  mutate(pc=n/sum(n))

### model fit stats --------------------

nclass_vector <- c(2:8)
bic_vector2 <- c(multi_emp_lca2a$bic, multi_emp_lca3a$bic, multi_emp_lca4a$bic, 
                 multi_emp_lca5a$bic, multi_emp_lca6a$bic, multi_emp_lca7a$bic, 
                 multi_emp_lca8a$bic)
aic_vector2 <- c(multi_emp_lca2a$aic, multi_emp_lca3a$aic, multi_emp_lca4a$aic, 
                 multi_emp_lca5a$aic, multi_emp_lca6a$aic, multi_emp_lca7a$aic, 
                 multi_emp_lca8a$aic)
Gsq_vector2 <- c(multi_emp_lca2a$Gsq, multi_emp_lca3a$Gsq, multi_emp_lca4a$Gsq, 
                 multi_emp_lca5a$Gsq, multi_emp_lca6a$Gsq, multi_emp_lca7a$Gsq, 
                 multi_emp_lca8a$Gsq)
Chisq_vector2 <- c(multi_emp_lca2a$Chisq, multi_emp_lca3a$Chisq, 
                   multi_emp_lca4a$Chisq, multi_emp_lca5a$Chisq, 
                   multi_emp_lca6a$Chisq, multi_emp_lca7a$Chisq, 
                   multi_emp_lca8a$Chisq)
entropy_vector2 <- c(multi_emp_lca2a_ent,multi_emp_lca3a_ent, 
                     multi_emp_lca4a_ent, multi_emp_lca5a_ent, 
                     multi_emp_lca6a_ent, multi_emp_lca7a_ent, 
                     multi_emp_lca8a_ent)

bic_vector2_b <- c(multi_emp_lca2b$bic, multi_emp_lca3b$bic, multi_emp_lca4b$bic, 
                   multi_emp_lca5b$bic, multi_emp_lca6b$bic, multi_emp_lca7b$bic, 
                   multi_emp_lca8b$bic)
aic_vector2_b <- c(multi_emp_lca2b$aic, multi_emp_lca3b$aic, multi_emp_lca4b$aic, 
                   multi_emp_lca5b$aic, multi_emp_lca6b$aic, multi_emp_lca7b$aic, 
                   multi_emp_lca8b$aic)
Gsq_vector2_b <- c(multi_emp_lca2b$Gsq, multi_emp_lca3b$Gsq, multi_emp_lca4b$Gsq, 
                   multi_emp_lca5b$Gsq, multi_emp_lca6b$Gsq, multi_emp_lca7b$Gsq, 
                   multi_emp_lca8b$Gsq)
Chisq_vector2_b <- c(multi_emp_lca2b$Chisq, multi_emp_lca3b$Chisq, 
                     multi_emp_lca4b$Chisq, multi_emp_lca5b$Chisq, 
                     multi_emp_lca6b$Chisq, multi_emp_lca7b$Chisq, 
                     multi_emp_lca8b$Chisq)
entropy_vector2_b <- c(multi_emp_lca2b_ent,multi_emp_lca3b_ent, 
                       multi_emp_lca4b_ent, multi_emp_lca5b_ent, 
                       multi_emp_lca6b_ent, multi_emp_lca7b_ent, 
                       multi_emp_lca8b_ent)

## create df for model fit stats
multi_empa_lca_fit_stats <- data.frame(cbind(nclass_vector, bic_vector2, aic_vector2,Gsq_vector2, Chisq_vector2, entropy_vector2))
names(multi_empa_lca_fit_stats) <- c("nclass", "bic", "aic", "Gsq", "Chisq", "entropy")

multi_empb_lca_fit_stats <- data.frame(cbind(nclass_vector, bic_vector2_b, aic_vector2_b,Gsq_vector2_b, Chisq_vector2_b, entropy_vector2_b))
names(multi_empb_lca_fit_stats) <- c("nclass", "bic", "aic", "Gsq", "Chisq", "entropy")

# model with lowest bic
temp <- multi_empa_lca_fit_stats %>%  filter(bic==min(bic))
multi_emp_min_bic_a <- temp[,1]
rm(temp)  

temp <- multi_emp_min_bic_b <- multi_empb_lca_fit_stats %>%  filter(bic==min(bic))
multi_emp_min_bic_b <- temp[,1]
rm(temp)  

# bic elbow plot
tiff("./output/descriptive/multi_empa_lca_elbow.tiff", width = 400, height = 400)
multi_empa_lca_fit_stats %>% dplyr::select(nclass, bic) %>% 
  ggplot(aes(x=nclass,y=bic)) + 
  geom_line() +
  geom_vline(xintercept=multi_emp_min_bic_a, colour="dark green", linetype = "longdash") +
  theme_bw()
dev.off()

tiff("./output/descriptive/multi_empb_lca_elbow.tiff", width = 400, height = 400)
multi_empb_lca_fit_stats %>% dplyr::select(nclass, bic) %>% 
  ggplot(aes(x=nclass,y=bic)) + 
  geom_line() +
  geom_vline(xintercept=multi_emp_min_bic_b, colour="dark green", linetype = "longdash") +
  theme_bw()
dev.off()

# model with lowest aic
multi_empa_lca_fit_stats %>%  filter(aic==min(aic))

multi_empb_lca_fit_stats %>%  filter(aic==min(aic))

# aic elbow plot
multi_empa_lca_fit_stats %>% dplyr::select(nclass, aic) %>% 
  ggplot(aes(x=nclass,y=aic)) + 
  geom_line()

multi_empb_lca_fit_stats %>% dplyr::select(nclass, aic) %>% 
  ggplot(aes(x=nclass,y=aic)) + 
  geom_line()

## save fit_stats
write.csv(multi_empa_lca_fit_stats, "./output/descriptive/multi_empa_lca_fit_stats.csv")

write.csv(multi_empb_lca_fit_stats, "./output/descriptive/multi_empb_lca_fit_stats.csv")

################################################################################
#####                        Most plausible models                        ######
################################################################################

#### ---------------------------------------------------------------------------
#### Employment contract
#### ---------------------------------------------------------------------------
#### Most plausible model ------------------------------------------------------
### 5 classes for both samples

emp_contracta_lca_final <- dfas1a_pred_class %>% ## <<< specify for final run <<< ##
  mutate(across(where(is.factor), as.character)) %>% 
  dplyr::select(pidp, wv_3,wv_4,wv_5,wv_6,pred_class5) %>% 
  mutate_all(~ ifelse(is.na(.),"missing",.)) %>% 
  mutate(emp_contract_class= ifelse(pred_class5==1, "non-permanent employment",
                                    ifelse(pred_class5==2, "into employment",
                                           ifelse(pred_class5==3,"permanent employment",
                                                  ifelse(pred_class5==4,"unemployed",
                                                         ifelse(pred_class5==5,"out of employment",
                                                                "CHECK"))))))

write_rds(emp_contracta_lca_final, "./working_data/emp_contracta_lca_final.rds")


emp_contractb_lca_final <- dfas1b_pred_class %>% ## <<< specify for final run <<< ##
  mutate(across(where(is.factor), as.character)) %>% 
  dplyr::select(pidp, wv_7,wv_8,wv_9,wv_10,pred_class5) %>% 
  mutate_all(~ ifelse(is.na(.),"missing",.)) %>% 
  mutate(emp_contract_class= ifelse(pred_class5==1, "unemployed",
                                    ifelse(pred_class5==2, "out of employment",
                                           ifelse(pred_class5==3,"permanent employment",
                                                  ifelse(pred_class5==4,"into employment",
                                                         ifelse(pred_class5==5,"non-permanent employment",
                                                                "CHECK"))))))

write_rds(emp_contractb_lca_final, "./working_data/emp_contractb_lca_final.rds")


## create spine for allocating classes back onto survey data
emp_contracta_5class_spine <- emp_contracta_lca_final %>% 
  dplyr::select(pidp,emp_contract_class)

emp_contractb_5class_spine <- emp_contractb_lca_final %>% 
  dplyr::select(pidp,emp_contract_class)

write_rds(emp_contracta_5class_spine, "./working_data/emp_contracta_5class_spine.rds")
write_rds(emp_contractb_5class_spine, "./working_data/emp_contractb_5class_spine.rds")

## reorder the classes so there appear in a set order (FE,PE,UE,IE,OE)
probs.start.a <-empcontract_lca5a$probs.start
new.probs.start.a <- poLCA.reorder(probs.start, c(1,3,4,2,5)) ## <<< specify for final run <<< ##

probs.start.b <-empcontract_lca5b$probs.start
new.probs.start.b <- poLCA.reorder(probs.start, c(5,3,1,4,2)) ## <<< specify for final run <<< ##

#save output
emp_contracta_lca_final_model <- poLCA(f, dfas1a_seq_wide, nclass = 5, maxiter = 8000, probs.start=new.probs.start.a, 
                                       graphs = FALSE, na.rm = FALSE)
saveRDS(emp_contracta_lca_final_model, "./output/descriptive/emp_contracta_lca_final_model.rds")

emp_contractb_lca_final_model <- poLCA(f2, dfas1b_seq_wide, nclass = 5, maxiter = 8000, probs.start=new.probs.start.b, 
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


#### ---------------------------------------------------------------------------
#### Broken employment spells
#### ---------------------------------------------------------------------------
#### Most plausible model ------------------------------------------------------
### 5 classes for both samples

#emp_spellsa_lca_final <- dfas1a_pred_class2 %>% ## <<< specify for final run <<< ##
#  mutate(across(where(is.factor), as.character)) %>% 
#  dplyr::select(pidp, wv_3,wv_4,wv_5,wv_6,pred_class5) %>% 
#  mutate_all(~ ifelse(is.na(.),"missing",.)) %>% 
#  mutate(emp_spells_class= ifelse(pred_class5==1, "non-permanent employment",
#                                    ifelse(pred_class5==2, "into employment",
#                                           ifelse(pred_class5==3,"permanent employment",
#                                                  ifelse(pred_class5==4,"unemployed",
#                                                         ifelse(pred_class5==5,"out of employment",
#                                                                "CHECK"))))))
#
#write_rds(emp_spellsa_lca_final, "./working_data/emp_spellsa_lca_final.rds")
#
#
#emp_spellsb_lca_final <- dfas1b_pred_class2 %>% ## <<< specify for final run <<< ##
#  mutate(across(where(is.factor), as.character)) %>% 
#  dplyr::select(pidp, wv_7,wv_8,wv_9,wv_10,pred_class5) %>% 
#  mutate_all(~ ifelse(is.na(.),"missing",.)) %>% 
#  mutate(emp_spells_class= ifelse(pred_class5==1, "non-permanent employment",
#                                    ifelse(pred_class5==2, "permanent employment",
#                                           ifelse(pred_class5==3,"unemployed",
#                                                  ifelse(pred_class5==4,"into employment",
#                                                         ifelse(pred_class5==5,"out of employment",
#                                                                "CHECK"))))))
#
#write_rds(emp_spellsb_lca_final, "./working_data/emp_spellsb_lca_final.rds")
#
#
### create spine for allocating classes back onto survey data
#emp_spellsa_5class_spine <- emp_spellsa_lca_final %>% 
#  dplyr::select(pidp,emp_spells_class)
#
#emp_spellsb_5class_spine <- emp_spellsb_lca_final %>% 
#  dplyr::select(pidp,emp_spells_class)
#
#write_rds(emp_spellsa_5class_spine, "./working_data/emp_spellsa_5class_spine.rds")
#write_rds(emp_spellsb_5class_spine, "./working_data/emp_spellsb_5class_spine.rds")
#
### reorder the classes so there appear in a set order
#probs.start<-emp_spells_lca5a$probs.start
##new.probs.start <- poLCA.reorder(probs.start, c(5,1,2,4,3)) ## <<< specify for final run <<< ##
#
#probs.start.b <-emp_spells_lca5b$probs.start
#
##save output
#emp_spellsa_lca_final_model <- poLCA(f, dfas1a_seq_wide, nclass = 5, maxiter = 8000, probs.start=probs.start, 
#                                       graphs = FALSE, na.rm = FALSE)
#saveRDS(emp_spellsa_lca_final_model, "./output/descriptive/emp_spellsa_lca_final_model.rds")
#
#emp_spellsb_lca_final_model <- poLCA(f2, dfas1b_seq_wide, nclass = 5, maxiter = 8000, probs.start=probs.start.b, 
#                                       graphs = FALSE, na.rm = FALSE)
#saveRDS(emp_spellsb_lca_final_model, "./output/descriptive/emp_spellsb_lca_final_model.rds")
#
## save plot
#tiff("./output/descriptive/emp_spellsa_lca_final.tiff", width = 400, height = 400)
#poLCA(f, dfas1a_seq_wide2, nclass = 5, maxiter = 8000, probs.start=probs.start, 
#      graphs = TRUE, na.rm = FALSE)
#dev.off()
#
#tiff("./output/descriptive/emp_spellsb_lca_final.tiff", width = 400, height = 400)
#poLCA(f2, dfas1b_seq_wide2, nclass = 5, maxiter = 8000, probs.start=probs.start.b, 
#      graphs = TRUE, na.rm = FALSE)
#dev.off()


#### ---------------------------------------------------------------------------
#### Multiple employment
#### ---------------------------------------------------------------------------

#### Most plausible model ------------------------------------------------------
### 5 classes for both samples

multi_empa_lca_final <- dfas1a_pred_class3 %>% ## <<< specify for final run <<< ##
  mutate(across(where(is.factor), as.character)) %>% 
  dplyr::select(pidp, wv_3,wv_4,wv_5,wv_6,pred_class5) %>% 
  mutate_all(~ ifelse(is.na(.),"missing",.)) %>% 
  mutate(multi_emp_class= ifelse(pred_class5==1, "out of employment",
                                    ifelse(pred_class5==2, "single employment",
                                           ifelse(pred_class5==3,"multiple employment",
                                                  ifelse(pred_class5==4,"into employment",
                                                         ifelse(pred_class5==5,"unemployed",
                                                                "CHECK"))))))

write_rds(multi_empa_lca_final, "./working_data/multi_empa_lca_final.rds")


multi_empb_lca_final <- dfas1b_pred_class3 %>% ## <<< specify for final run <<< ##
  mutate(across(where(is.factor), as.character)) %>% 
  dplyr::select(pidp, wv_7,wv_8,wv_9,wv_10,pred_class5) %>% 
  mutate_all(~ ifelse(is.na(.),"missing",.)) %>% 
  mutate(multi_emp_class= ifelse(pred_class5==1, "out of employment",
                                 ifelse(pred_class5==2, "single employment",
                                        ifelse(pred_class5==3,"into employment",
                                               ifelse(pred_class5==4,"multiple employment",
                                                      ifelse(pred_class5==5,"unemployed",
                                                             "CHECK"))))))

write_rds(multi_empb_lca_final, "./working_data/multi_empb_lca_final.rds")


## create spine for allocating classes back onto survey data
multi_empa_5class_spine <- multi_empa_lca_final %>% 
  dplyr::select(pidp,multi_emp_class)

multi_empb_5class_spine <- multi_empb_lca_final %>% 
  dplyr::select(pidp,multi_emp_class)

write_rds(multi_empa_5class_spine, "./working_data/multi_empa_5class_spine.rds")
write_rds(multi_empb_5class_spine, "./working_data/multi_empb_5class_spine.rds")

## reorder the classes so there appear in a set order (ME,SE,UE,IE,OE)
probs.start.a <-multi_emp_lca5a$probs.start
new.probs.start.a <- poLCA.reorder(probs.start.a, c(3,2,5,4,1)) ## <<< specify for final run <<< ##

probs.start.b <-multi_emp_lca5b$probs.start
new.probs.start.b <- poLCA.reorder(probs.start.b, c(4,2,3,5,1)) ## <<< specify for final run <<< ##

#save output
multi_empa_lca_final_model <- poLCA(f, dfas1a_seq_wide3, nclass = 5, maxiter = 8000, probs.start=new.probs.start.a, 
                                       graphs = FALSE, na.rm = FALSE)
saveRDS(multi_empa_lca_final_model, "./output/descriptive/multi_empa_lca_final_model.rds")

multi_empb_lca_final_model <- poLCA(f2, dfas1b_seq_wide3, nclass = 5, maxiter = 8000, probs.start=new.probs.start.b, 
                                       graphs = FALSE, na.rm = FALSE)
saveRDS(multi_empb_lca_final_model, "./output/descriptive/multi_empb_lca_final_model.rds")

# save plot
tiff("./output/descriptive/multi_empa_lca_final.tiff", width = 400, height = 400)
poLCA(f, dfas1a_seq_wide3, nclass = 5, maxiter = 8000, probs.start=new.probs.start.a, 
      graphs = TRUE, na.rm = FALSE)
dev.off()

tiff("./output/descriptive/multi_empb_lca_final.tiff", width = 400, height = 400)
poLCA(f2, dfas1b_seq_wide3, nclass = 5, maxiter = 8000, probs.start=new.probs.start.b, 
      graphs = TRUE, na.rm = FALSE)
dev.off()
