################################################################################

# Persistent precarious employment and health - Understanding Society
# 3-02 - ungrouped sequence analysis - weighted
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

### analytic sample 1b - waves 7-10
dfas1b <- readRDS("./analytic_sample_data/dfas1b.rds") %>% 
  mutate(sample_group = "b")# %>% select(-valid_10)

### weights spines
weight_spine_a <- readRDS("./look_ups/weights_spine_a.rds") %>% 
  dplyr::select(pidp, weight_flag, indinub_xw) %>% 
  filter(weight_flag==1) %>% 
  dplyr::select(-weight_flag)

weight_spine_b <- readRDS("./look_ups/weights_spine_b.rds") %>% 
  dplyr::select(pidp, weight_flag, indinui_xw) %>% 
  filter(weight_flag==1) %>% 
  dplyr::select(-weight_flag)


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
  pivot_wider(names_from = wv, values_from = emp_contract, values_fill = "missing") %>% 
  filter(wv_6!="missing") %>% 
  dplyr::select(-wv_NA)


dfas1b_seq_wide  <-  dfas1b_seq %>% 
  dplyr::select(pidp,wv_n,emp_contract) %>% 
  mutate(wv=paste0("wv_",wv_n)) %>% 
  dplyr::select(-wv_n) %>% 
  pivot_wider(names_from = wv, values_from = emp_contract, values_fill = "missing") %>% 
  filter(wv_10!="missing")

### add on weights
dfas1a_seq_wide <- dfas1a_seq_wide %>% 
  left_join(weight_spine_a)

dfas1b_seq_wide <- dfas1b_seq_wide %>% 
  left_join(weight_spine_b)

### define labels and codes for sequence analysis
## retaining missing values for now but plan to imput
emp_contract_labs <- c("fixed term", "missing", "permanent", "unemployed/not in employment" )
emp_contract_code <- c("FT", "NA", "PE", "UE")

### create sequence data
emp_contract.seq.a <- seqdef(dfas1a_seq_wide, 2:5, states = emp_contract_code,
                             labels = emp_contract_labs, weights = dfas1a_seq_wide$indinub_xw)

emp_contract.seq.b <- seqdef(dfas1b_seq_wide, 2:5, states = emp_contract_code,
                             labels = emp_contract_labs, weights = dfas1b_seq_wide$indinui_xw)



## first 100 sequences
tiff("./output/weighted/emp_contract_seqiplot100.tiff", width = 960, height = 960)
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
tiff("output/weighted/emp_contract_seqiplot.tiff", width = 960, height = 1920)
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
tiff("output/weighted/emp_contract_seqfplot.tiff", width = 960, height = 1920)
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
tiff("output/weighted/emp_contract_seqdplot.tiff", width = 960, height = 960)
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
tiff("output/weighted/emp_contract_legend.tiff", width = 400, height = 250)
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
  pivot_wider(names_from = wv, values_from = broken_emp, values_fill = "missing") %>% 
  filter(wv_6!="missing") %>% 
  dplyr::select(-wv_NA)

write_rds(dfas1a_seq_wide2, "./working_data/weighted/dfas1a_seq_wide2.rds")

dfas1b_seq_wide2  <-  dfas1b_seq2 %>% 
  dplyr::select(pidp,wv_n,broken_emp) %>% 
  mutate(broken_emp = ifelse(is.na(broken_emp),"missing",broken_emp)) %>% 
  mutate(wv=paste0("wv_",wv_n)) %>% 
  dplyr::select(-wv_n) %>% 
  pivot_wider(names_from = wv, values_from = broken_emp, values_fill = "missing")

write_rds(dfas1b_seq_wide2, "./working_data/weighted/dfas1b_seq_wide2.rds")

### add on weights
dfas1a_seq_wide2 <- dfas1a_seq_wide2 %>% 
  left_join(weight_spine_a)

dfas1b_seq_wide2 <- dfas1b_seq_wide2 %>% 
  left_join(weight_spine_b)

### define labels and codes for sequence analysis
## retaining missing values for now but plan to impute
broken_emp_labs <- c("Broken employment", "Missing", "No employment spells", "Unbroken employment" )
broken_emp_code <- c("BE","NA", "NE", "UE")

### create sequence data
broken_emp.seq.a <- seqdef(dfas1a_seq_wide2, 2:5, states = broken_emp_code,
                           labels = broken_emp_labs, weights = dfas1a_seq_wide2$indinub_xw)

broken_emp.seq.b <- seqdef(dfas1b_seq_wide2, 2:5, states = broken_emp_code,
                           labels = broken_emp_labs, weights = dfas1b_seq_wide$indinui_xw)

## first 10 sequences
tiff("./output/weighted/broken_emp_seqiplot100.tiff", width = 960, height = 960)
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
tiff("output/weighted/broken_emp_seqiplot.tiff", width = 960, height = 1920)
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
tiff("output/weighted/broken_emp_seqfplot.tiff", width = 960, height = 1920)
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
tiff("output/weighted/broken_emp_seqdplot.tiff", width = 960, height = 960)
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
tiff("output/weighted/broken_emp_legend.tiff", width = 400, height = 250)
seqlegend(broken_emp.seq.a, cex = 1.3)
dev.off()

#### ---------------------------------------------------------------------------
#### Multiple jobs
#### ---------------------------------------------------------------------------

dfas1a_seq3 <- dfas1a
dfas1b_seq3 <- dfas1b

### wide format for creating sequence data
dfas1a_seq_wide3  <-  dfas1a_seq3 %>% 
  dplyr::select(pidp,wv_n,j2has_dv2) %>% 
  mutate(wv=paste0("wv_",wv_n)) %>% 
  dplyr::select(-wv_n) %>% 
  pivot_wider(names_from = wv, values_from = j2has_dv2, values_fill = "missing") %>% 
  filter(wv_6!="missing") %>% 
  dplyr::select(-wv_NA)

write_rds(dfas1a_seq_wide3, "./working_data/weighted/dfas1a_seq_wide3.rds")


dfas1b_seq_wide3  <-  dfas1b_seq3 %>% 
  dplyr::select(pidp,wv_n,j2has_dv2) %>% 
  mutate(wv=paste0("wv_",wv_n)) %>% 
  dplyr::select(-wv_n) %>% 
  pivot_wider(names_from = wv, values_from = j2has_dv2, values_fill = "missing")

write_rds(dfas1b_seq_wide3, "./working_data/weighted/dfas1b_seq_wide3.rds")

### add on weights
dfas1a_seq_wide3 <- dfas1a_seq_wide3 %>% 
  left_join(weight_spine_a)

dfas1b_seq_wide3 <- dfas1b_seq_wide3 %>% 
  left_join(weight_spine_b)

### define labels and codes for sequence analysis
multi_jobs_labs <- c("missing", "multiple jobs", "one job", 
                     "unemployed/not in employment", "unemployed/not in employment with additional")
multi_jobs_code <- c("NA","ME", "OE", "UE", "UA")

### create sequence data
multi_jobs.seq.a <- seqdef(dfas1a_seq_wide3, 2:5, states = multi_jobs_code,
                           labels = multi_jobs_labs, weights = dfas1a_seq_wide3$indinub_xw)

multi_jobs.seq.b <- seqdef(dfas1b_seq_wide3, 2:5, states = multi_jobs_code,
                           labels = multi_jobs_labs, weights = dfas1b_seq_wide$indinui_xw)

## first 10 sequences
tiff("./output/weighted/multi_emp_seqiplot100.tiff", width = 960, height = 960)
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
tiff("output/weighted/multi_emp_seqiplot.tiff", width = 960, height = 1920)
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
tiff("output/weighted/multi_emp_seqfplot.tiff", width = 960, height = 1920)
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
tiff("output/weighted/multi_emp_seqdplot.tiff", width = 960, height = 960)
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
tiff("output/weighted/multi_emp_legend.tiff", width = 400, height = 250)
seqlegend(multi_jobs.seq.a, cex = 1.3)
dev.off()

####save wide sequence dfs and sequence dfs

saveRDS(dfas1a_seq_wide, "./working_data/weighted/dfas1a_seq_wide.rds")
saveRDS(dfas1b_seq_wide, "./working_data/weighted/dfas1b_seq_wide.rds")

saveRDS(emp_contract.seq.a, "./working_data/weighted/emp_contract.seq.a.rds")
saveRDS(emp_contract.seq.b, "./working_data/weighted/emp_contract.seq.b.rds")

saveRDS(broken_emp.seq.a, "./working_data/weighted/broken_emp.seq.a.rds")
saveRDS(broken_emp.seq.b, "./working_data/weighted/broken_emp.seq.b.rds")

saveRDS(multi_jobs.seq.a, "./working_data/weighted/multi_jobs.seq.a.rds")
saveRDS(multi_jobs.seq.b, "./working_data/weighted/multi_jobs.seq.b.rds")

