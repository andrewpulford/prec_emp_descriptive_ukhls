################################################################################

# Persistent precarious employment and health - Understanding Society
# 3-05 - grouped sequence analysis - weighted
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

#### load wide sequence data with lca class membership attached
### employment contract
emp_contracta_lca_final <- readRDS("./working_data/emp_contracta_lca_final.rds") %>% 
  dplyr::select(-pred_class5)
emp_contractb_lca_final <- readRDS("./working_data/emp_contractb_lca_final.rds") %>% 
  dplyr::select(-pred_class5)

### broken employment spells
emp_spellsa_lca_final <- readRDS("./working_data/emp_spellsa_lca_final.rds") %>% 
  dplyr::select(-pred_class3)
emp_spellsb_lca_final <- readRDS("./working_data/emp_spellsb_lca_final.rds") %>% 
  dplyr::select(-pred_class3)

### multiple employment
multi_empa_lca_final <- readRDS("./working_data/multi_empa_lca_final.rds") %>% 
  dplyr::select(-pred_class5)

multi_empb_lca_final <- readRDS("./working_data/multi_empb_lca_final.rds") %>% 
  dplyr::select(-pred_class5)


#### weights spines ----------
weight_spine_a <- readRDS("./look_ups/weights_spine_a.rds") %>% 
  dplyr::select(pidp, weight_flag, indinub_xw) %>% 
  filter(weight_flag==1) %>% 
  dplyr::select(-weight_flag)

weight_spine_b <- readRDS("./look_ups/weights_spine_b.rds") %>% 
  dplyr::select(pidp, weight_flag, indinui_xw) %>% 
  filter(weight_flag==1) %>% 
  dplyr::select(-weight_flag)

#### join weights onto seq data -------------
### employment contract
emp_contracta_lca_final <- emp_contracta_lca_final %>% left_join(weight_spine_a)
emp_contractb_lca_final <- emp_contractb_lca_final %>% left_join(weight_spine_b)

### broken employment spells
emp_spellsa_lca_final <- emp_spellsa_lca_final  %>% left_join(weight_spine_a)
emp_spellsb_lca_final <- emp_spellsb_lca_final  %>% left_join(weight_spine_b)

### multiple employment
multi_empa_lca_final <- multi_empa_lca_final %>% left_join(weight_spine_a)
multi_empb_lca_final <- multi_empb_lca_final %>% left_join(weight_spine_b)



################################################################################
#####      sequence analysis for employment contract - 5 class solution    #####
################################################################################

### define labels and codes for sequence analysis
emp_contract_labs <- c("fixed term", "missing", "permanent", "unemployed/not in employment" )
emp_contract_code <- c("FT", "NA", "PE", "UE")

### create sequence data
emp_contract2.seq.a <- seqdef(emp_contracta_lca_final, 2:5, states = emp_contract_code,
                              labels = emp_contract_labs, 
                              weights = emp_contracta_lca_final$indinub_xw)


emp_contract2.seq.b <- seqdef(emp_contractb_lca_final, 2:5, states = emp_contract_code,
                              labels = emp_contract_labs, 
                              weights = emp_contractb_lca_final$indinui_xw)

## first 100 sequences
tiff("./output/weighted/emp_contracta_grouped_seqiplot100.tiff")#, width = 960, height = 960)
seqiplot(emp_contract2.seq.a,
         idxs=1:100, # to add more lines
         with.legend = T, 
         main = NULL,
         group = emp_contracta_lca_final$emp_contract_class,
         border = NA)
dev.off()

tiff("./output/weighted/emp_contractb_grouped_seqiplot100.tiff")#, width = 960, height = 960)
seqiplot(emp_contract2.seq.b,
         idxs=1:100, # to add more lines
         with.legend = T, 
         main = NULL,
         group = emp_contractb_lca_final$emp_contract_class,
         border = NA)
dev.off()

## all sequences
tiff("output/weighted/emp_contracta_grouped_seqiplot.tiff")#, width = 960, height = 1920)
seqIplot(emp_contract2.seq.a,
         with.legend = T, 
         main = NULL,
         group = emp_contracta_lca_final$emp_contract_class,
         border = NA)
dev.off()

tiff("output/weighted/emp_contractb_grouped_seqiplot.tiff")#, width = 960, height = 1920)
seqIplot(emp_contract2.seq.b,
         with.legend = T, 
         main = NULL,
         group = emp_contractb_lca_final$emp_contract_class,
         border = NA)
dev.off()

# sequence frequency plot (all common sequences)
tiff("./output/weighted/emp_contracta_grouped_seqfplot.tiff")#, width = 960, height = 1920)
seqfplot(emp_contract2.seq.a, 
         idxs=1:900, # to add more lines
         with.legend = T, 
         group = emp_contracta_lca_final$emp_contract_class,
         border = NA, 
         main = NULL)
dev.off()

tiff("output/weighted/emp_contractb_grouped_seqfplot.tiff")#, width = 960, height = 1920)
seqfplot(emp_contract2.seq.b, 
         idxs=1:900, # to add more lines
         with.legend = T, 
         border = NA, 
         group = emp_contractb_lca_final$emp_contract_class,
         main = NULL)
dev.off()

# state distribution plot
tiff("output/weighted/emp_contracta_grouped_seqdplot.tiff")#, width = 960, height = 960)
seqdplot(emp_contract2.seq.a, 
         with.legend = T, 
         border = NA, 
         group = emp_contracta_lca_final$emp_contract_class,
         main = NULL)
dev.off()

tiff("output/weighted/emp_contractb_grouped_seqdplot.tiff")#, width = 960, height = 960)
seqdplot(emp_contract2.seq.b, 
         with.legend = T, 
         border = NA, 
         group = emp_contractb_lca_final$emp_contract_class,
         main = NULL)
dev.off()

# legend
#tiff("output/weighted/emp_contract_legend.tiff", width = 400, height = 250)
seqlegend(emp_contract2.seq.a, cex = 1.3)
seqlegend(emp_contract2.seq.b, cex = 1.3)
#dev.off()


################################################################################
#####       sequence analysis for broken employment - 3 class solution     #####
################################################################################

### define labels and codes for sequence analysis
emp_spells_labs <- c("Broken employment", "missing", "No employment spells", "Unbroken employment" )
emp_spells_code <- c("BE", "NA", "NE", "UE")

### create sequence data
emp_spells2.seq.a <- seqdef(emp_spellsa_lca_final, 2:5, states = emp_spells_code,
                              labels = emp_spells_labs, 
                              weights = emp_spellsa_lca_final$indinub_xw)


emp_spells2.seq.b <- seqdef(emp_spellsb_lca_final, 2:5, states = emp_spells_code,
                              labels = emp_spells_labs, 
                              weights = emp_spellsb_lca_final$indinui_xw)

## first 100 sequences
tiff("./output/weighted/emp_spellsa_grouped_seqiplot100.tiff")#, width = 960, height = 960)
seqiplot(emp_spells2.seq.a,
         idxs=1:100, # to add more lines
         with.legend = T, 
         main = NULL,
         group = emp_spellsa_lca_final$emp_spells_class,
         border = NA)
dev.off()

tiff("./output/weighted/emp_spellsb_grouped_seqiplot100.tiff")#, width = 960, height = 960)
seqiplot(emp_spells2.seq.b,
         idxs=1:100, # to add more lines
         with.legend = T, 
         main = NULL,
         group = emp_spellsb_lca_final$emp_spells_class,
         border = NA)
dev.off()

## all sequences
tiff("output/weighted/emp_spellsa_grouped_seqiplot.tiff")#, width = 960, height = 1920)
seqIplot(emp_spells2.seq.a,
         with.legend = T, 
         main = NULL,
         group = emp_spellsa_lca_final$emp_spells_class,
         border = NA)
dev.off()

tiff("output/weighted/emp_spellsb_grouped_seqiplot.tiff")#, width = 960, height = 1920)
seqIplot(emp_spells2.seq.b,
         with.legend = T, 
         main = NULL,
         group = emp_spellsb_lca_final$emp_spells_class,
         border = NA)
dev.off()

# sequence frequency plot (all common sequences)
tiff("./output/weighted/emp_spellsa_grouped_seqfplot.tiff")#, width = 960, height = 1920)
seqfplot(emp_spells2.seq.a, 
         idxs=1:900, # to add more lines
         with.legend = T, 
         group = emp_spellsa_lca_final$emp_spells_class,
         border = NA, 
         main = NULL)
dev.off()

tiff("output/weighted/emp_spellsb_grouped_seqfplot.tiff")#, width = 960, height = 1920)
seqfplot(emp_spells2.seq.b, 
         idxs=1:900, # to add more lines
         with.legend = T, 
         border = NA, 
         group = emp_spellsb_lca_final$emp_spells_class,
         main = NULL)
dev.off()

# state distribution plot
tiff("output/weighted/emp_spellsa_grouped_seqdplot.tiff")#, width = 960, height = 960)
seqdplot(emp_spells2.seq.a, 
         with.legend = T, 
         border = NA, 
         group = emp_spellsa_lca_final$emp_spells_class,
         main = NULL)
dev.off()

tiff("output/weighted/emp_spellsb_grouped_seqdplot.tiff")#, width = 960, height = 960)
seqdplot(emp_spells2.seq.b, 
         with.legend = T, 
         border = NA, 
         group = emp_spellsb_lca_final$emp_spells_class,
         main = NULL)
dev.off()

# legend
#tiff("output/weighted/emp_spells_legend.tiff", width = 400, height = 250)
seqlegend(emp_spells2.seq.a, cex = 1.3)
#seqlegend(emp_spells2.seq.b, cex = 1.3)
#dev.off()

################################################################################
#####      sequence analysis for multiple employment - 5 class solution    #####
################################################################################

### define labels and codes for sequence analysis
multi_jobs_labs <- c("missing", "multiple employment", "single employment", 
                     "unemployed/not in employment", "unemployed/not in employment with additional")
multi_jobs_code <- c("NA","ME", "OE", "UE", "UA")

### create sequence data
multi_emp2.seq.a <- seqdef(multi_empa_lca_final, 2:5, states = multi_jobs_code,
                              labels = multi_jobs_labs, weights = multi_empa_lca_final$indinub_xw)


multi_emp2.seq.b <- seqdef(multi_empb_lca_final, 2:5, states = multi_jobs_code,
                              labels = multi_jobs_labs, weights = multi_empb_lca_final$indinui_xw)

## first 100 sequences
tiff("./output/weighted/multi_empa_grouped_seqiplot100.tiff")#, width = 960, height = 960)
seqiplot(multi_emp2.seq.a,
         idxs=1:100, # to add more lines
         with.legend = T, 
         main = NULL,
         group = multi_empa_lca_final$multi_emp_class,
         border = NA)
dev.off()

tiff("./output/weighted/multi_empb_grouped_seqiplot100.tiff")#, width = 960, height = 960)
seqiplot(multi_emp2.seq.b,
         idxs=1:100, # to add more lines
         with.legend = T, 
         main = NULL,
         group = multi_empb_lca_final$multi_emp_class,
         border = NA)
dev.off()

## all sequences
tiff("output/weighted/multi_empa_grouped_seqiplot.tiff")#, width = 960, height = 1920)
seqIplot(multi_emp2.seq.a,
         with.legend = T, 
         main = NULL,
         group = multi_empa_lca_final$multi_emp_class,
         border = NA)
dev.off()

tiff("output/weighted/multi_empb_grouped_seqiplot.tiff")#, width = 960, height = 1920)
seqIplot(multi_emp2.seq.b,
         with.legend = T, 
         main = NULL,
         group = multi_empb_lca_final$multi_emp_class,
         border = NA)
dev.off()

# sequence frequency plot (all common sequences)
tiff("./output/weighted/multi_empa_grouped_seqfplot.tiff")#, width = 960, height = 1920)
seqfplot(multi_emp2.seq.a, 
         idxs=1:900, # to add more lines
         with.legend = T, 
         group = multi_empa_lca_final$multi_emp_class,
         border = NA, 
         main = NULL)
dev.off()

tiff("output/weighted/multi_empb_grouped_seqfplot.tiff")#, width = 960, height = 1920)
seqfplot(multi_emp2.seq.b, 
         idxs=1:900, # to add more lines
         with.legend = T, 
         border = NA, 
         group = multi_empb_lca_final$multi_emp_class,
         main = NULL)
dev.off()

# state distribution plot
tiff("output/weighted/multi_empa_grouped_seqdplot.tiff")#, width = 960, height = 960)
seqdplot(multi_emp2.seq.a, 
         with.legend = T, 
         border = NA, 
         group = multi_empa_lca_final$multi_emp_class,
         main = NULL)
dev.off()

tiff("output/weighted/multi_empb_grouped_seqdplot.tiff")#, width = 960, height = 960)
seqdplot(multi_emp2.seq.b, 
         with.legend = T, 
         border = NA, 
         group = multi_empb_lca_final$multi_emp_class,
         main = NULL)
dev.off()

# legend
#tiff("output/weighted/multi_emp_legend.tiff", width = 400, height = 250)
seqlegend(multi_emp2.seq.a, cex = 1.3)
seqlegend(multi_emp2.seq.b, cex = 1.3)
#dev.off()
