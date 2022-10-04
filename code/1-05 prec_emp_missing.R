################################################################################

# Persistent precarious employment and health - Understanding Society
# 1-05 - Non-response and missing data 
# Andrew Pulford

# Data source:
# University of Essex, Institute for Social and Economic Research. (2021). 
# Understanding Society: Waves 1-10, 2009-2019 and Harmonised BHPS: Waves 1-18, 
# 1991-2009. [data collection]. 13th Edition. UK Data Service. SN: 6614, 
# http://doi.org/10.5255/UKDA-SN-6614-14

#### What this script does:
# (a) Checks non-response across survey waves 
# (b) Checks exposure data completeness across waves


################################################################################

## remove any existing objects from global environment
rm(list=ls()) 


################################################################################
#####                            install packages                          #####
################################################################################

library(tidyverse) # all kinds of stuff 
#library(gridExtra) # for side-by-side plots
library(TraMineR) # for sequence analysis

citation("TraMineR")

################################################################################
#####                         load and prepare data                        #####
################################################################################

## for missing vars by wave (3-6) use dfas1a
dfas1a <- readRDS("./analytic_sample_data/dfas1a.rds")

## for missing vars by wave (7-10) use dfas1b
dfas1b <- readRDS("./analytic_sample_data/dfas1b.rds")


################################################################################
#####                        check wave non-response                       #####
################################################################################

### total number of cases in each wave 16-64
## waves 3-6
spine_total_a <- dfas1a %>% group_by(wv_n) %>% 
  summarise(n=n()) %>% 
  ungroup() 

# number and % of individuals in sample based on endpoint
indivs_a <- spine_total_a[[4,2]]

spine_total_a <- spine_total_a %>% 
  mutate(total = indivs_a) %>% 
  mutate(pc = n/total*100) %>% 
  mutate(as = "a")

## waves7-10
spine_total_b <- dfas1b %>% group_by(wv_n) %>% 
  summarise(n=n()) %>% 
  ungroup() 

# number and % of individuals in sample based on endpoint
indivs_b <- spine_total_b[[4,2]]

spine_total_b <- spine_total_b %>% 
  mutate(total = indivs_b) %>% 
  mutate(pc = n/total*100) %>% 
  mutate(as = "b")

spine_total <- spine_total_a %>% bind_rows(spine_total_b)

# chart
indivs_wave_plot <- spine_total %>% 
  ggplot(aes(x=wv_n, y=pc))+
  geom_col() +
  xlab("Wave") +
  ylab("Percantage of individuals in analytic sample") +
  theme_classic() +
  facet_wrap(~as, scales = "free")


### total number of cases by number of waves reported
## waves 3-6
spine_waves_a <- dfas1a %>% 
  group_by(pidp) %>% 
  summarise(n_wvs = n()) %>% # calculate number of waves responded to
  ungroup() %>% 
  group_by(n_wvs) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pc= n/sum(n)*100) %>% 
  mutate(as = "a")

## waves 7-10
spine_waves_b <- dfas1b %>% 
  group_by(pidp) %>% 
  summarise(n_wvs = n()) %>% # calculate number of waves responded to
  ungroup() %>% 
  group_by(n_wvs) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pc= n/sum(n)*100) %>% 
  mutate(as = "b")

## join together
spine_waves <- spine_waves_a %>% bind_rows(spine_waves_b)

# chart
spine_waves %>% 
  ggplot(aes(x=n_wvs, y=pc)) +
  geom_col() +
#  scale_x_continuous(limits = c(0,5),breaks = 1:4) +
  xlab("Number of waves responded to") +
  ylab("Percentage of cases") +
  theme_classic() +
  facet_wrap(~as, scales = "free")


### sequence analysis - wave non-response

## specify labels and codes (same for both samples)
spine_labs <- c("response", "missing")
spine_code <- c("RESP", "NA")


## wide format for sequencing data
# waves 3-6
spine_wide_a <- dfas1a %>%
  dplyr::select(pidp,wv_n) %>% 
  mutate(response=1) %>% 
  mutate(wv=paste0("wv_",wv_n)) %>% 
  dplyr::select(-wv_n) %>% 
  pivot_wider(names_from = wv, values_from = response, values_fill = 99)

# waves 7-10
spine_wide_b <- dfas1b %>%
  dplyr::select(pidp,wv_n) %>% 
  mutate(response=1) %>% 
  mutate(wv=paste0("wv_",wv_n)) %>% 
  dplyr::select(-wv_n) %>% 
  pivot_wider(names_from = wv, values_from = response, values_fill = 99)


spine.seq.a <- seqdef(spine_wide_a, 2:5, states = spine_code,
                    labels = spine_labs)
spine.seq.b <- seqdef(spine_wide_b, 2:5, states = spine_code,
                    labels = spine_labs)


## Index plot - first 10 sequences
#png("./output/descriptive/seqiplot100_missing.png", width = 960, height = 960)
par(mfrow=c(1,3))
seqiplot(spine.seq.a,
         idxs=1:100, # to add more lines
         with.legend = F, 
         main = "a",
         border = NA)
seqiplot(spine.seq.b,
         idxs=1:100, # to add more lines
         with.legend = F, 
         main = "b",
         border = NA)
seqlegend(spine.seq.a, cex = 1.3)
#dev.off()

## Index plot - all sequences
#png("output/descriptive/seqiplot_missing.png", width = 960, height = 1920)
par(mfrow=c(1,3))
seqIplot(spine.seq.a,
         #         group = spine.seq$wv_1,
         with.legend = F, 
         main = "a",
         border = NA)
seqIplot(spine.seq.b,
         #         group = spine.seq$wv_1,
         with.legend = F, 
         main = "b",
         border = NA)
seqlegend(spine.seq.a, cex = 1.3)
#dev.off()

# sequence frequency plot (all common sequences)
#png("output/descriptive/seqfplot_missing.png", width = 960, height = 1920)
par(mfrow=c(1,3))
seqfplot(spine.seq.a, 
         idxs=1:900, # to add more lines
         with.legend = F, 
         border = NA, 
         main = "a")
seqfplot(spine.seq.b, 
         idxs=1:900, # to add more lines
         with.legend = F, 
         border = NA, 
         main = "b")
seqlegend(spine.seq.a, cex = 1.3)
#dev.off()

# state distribution plot
#png("output/descriptive/seqdplot_missing.png", width = 960, height = 960)
par(mfrow=c(1,3))
seqdplot(spine.seq.a, 
         with.legend = F, 
         border = NA, 
         main = "a")
seqdplot(spine.seq.b, 
         with.legend = F, 
         border = NA, 
         main = "b")
seqlegend(spine.seq.a, cex = 1.3)
#dev.off()

#png("output/descriptive/legend_missing.png", width = 240, height = 190)
seqlegend(spine.seq.a, cex = 1.3)
#dev.off()

################################################################################
#####      Missing employment history data by variable across waves        #####
################################################################################

### employment contract (jbterm1) ----------------------------------------------

## specify labels and codes (same for both samples)
jbterm1_labs <- c("response","missing")
jbterm1_code <- c("RESP","NA")


## wide format for sequencing data
# waves 3-6
jbterm1_wide_a <- dfas1a %>%
  dplyr::select(pidp,wv_n,jbterm1) %>% 
  mutate(response = ifelse(jbterm1 %in% c("missing", "inapplicable", "proxy", "refusal", 
                               "Only available for IEMB", "Not available for IEMB",
                               "don't know"),99,1)) %>% 
  mutate(wv=paste0("wv_",wv_n)) %>% 
  dplyr::select(-c(wv_n, jbterm1)) %>% 
  pivot_wider(names_from = wv, values_from = response, values_fill = 99)

# waves 7-10
jbterm1_wide_b <- dfas1b %>%
  dplyr::select(pidp,wv_n,jbterm1) %>% 
  mutate(response = ifelse(jbterm1 %in% c("missing", "inapplicable", "proxy", "refusal", 
                                          "Only available for IEMB", "Not available for IEMB",
                                          "don't know"),99,1)) %>% 
  mutate(wv=paste0("wv_",wv_n)) %>% 
  dplyr::select(-c(wv_n, jbterm1)) %>% 
  pivot_wider(names_from = wv, values_from = response, values_fill = 99)


jbterm1.seq.a <- seqdef(jbterm1_wide_a, 2:5, states = jbterm1_code,
                      labels = jbterm1_labs)
jbterm1.seq.b <- seqdef(jbterm1_wide_b, 2:5, states = jbterm1_code,
                      labels = jbterm1_labs)


## Index plot - first 10 sequences
#png("./output/descriptive/seqiplot100_missing.png", width = 960, height = 960)
par(mfrow=c(1,3))
seqiplot(jbterm1.seq.a,
         idxs=1:100, # to add more lines
         with.legend = F, 
         main = "a",
         border = NA)
seqiplot(jbterm1.seq.b,
         idxs=1:100, # to add more lines
         with.legend = F, 
         main = "b",
         border = NA)
seqlegend(jbterm1.seq.a, cex = 1.3)
#dev.off()

## Index plot - all sequences
#png("output/descriptive/seqiplot_missing.png", width = 960, height = 1920)
par(mfrow=c(1,3))
seqIplot(jbterm1.seq.a,
         #         group = spine.seq$wv_1,
         with.legend = F, 
         main = "a",
         border = NA)
seqIplot(jbterm1.seq.b,
         #         group = spine.seq$wv_1,
         with.legend = F, 
         main = "b",
         border = NA)
seqlegend(spine.seq.a, cex = 1.3)
#dev.off()

# sequence frequency plot (all common sequences)
#png("output/descriptive/seqfplot_missing.png", width = 960, height = 1920)
par(mfrow=c(1,3))
seqfplot(jbterm1.seq.a, 
         idxs=1:900, # to add more lines
         with.legend = F, 
         border = NA, 
         main = "a")
seqfplot(jbterm1.seq.b, 
         idxs=1:900, # to add more lines
         with.legend = F, 
         border = NA, 
         main = "b")
seqlegend(jbterm1.seq.a, cex = 1.3)
#dev.off()

# state distribution plot
#png("output/descriptive/seqdplot_missing.png", width = 960, height = 960)
par(mfrow=c(1,3))
seqdplot(jbterm1.seq.a, 
         with.legend = F, 
         border = NA, 
         main = "a")
seqdplot(jbterm1.seq.b, 
         with.legend = F, 
         border = NA, 
         main = "b")
seqlegend(jbterm1.seq.a, cex = 1.3)
#dev.off()

#png("output/descriptive/legend_missing.png", width = 240, height = 190)
seqlegend(jbterm1.seq.a, cex = 1.3)
#dev.off()

### employment spells (emp_spells) ---------------------------------------------

## specify labels and codes (same for both samples)
emp_spells_labs <- c("response","missing")
emp_spells_code <- c("RESP","NA")


## wide format for sequencing data
# waves 3-6
emp_spells_wide_a <- dfas1a %>%
  dplyr::select(pidp,wv_n,nmpsp_dv,nnmpsp_dv,nunmpsp_dv) %>% 
  mutate(response = ifelse(nmpsp_dv %in% c("missing", "proxy", "refusal", 
                                             "don't know"), 99,
                             ifelse(nnmpsp_dv %in% c("missing", "proxy", "refusal", "don't know"), 99,
                                    ifelse(nunmpsp_dv %in% c("missing", "proxy", "refusal", "don't know"),99,1))),
  ) %>% 
mutate(wv=paste0("wv_",wv_n)) %>% 
  dplyr::select(-c(wv_n, nmpsp_dv,nnmpsp_dv,nunmpsp_dv)) %>% 
  pivot_wider(names_from = wv, values_from = response, values_fill = 99)

# waves 7-10
emp_spells_wide_b <- dfas1b %>%
  dplyr::select(pidp,wv_n,nmpsp_dv,nnmpsp_dv,nunmpsp_dv) %>% 
  mutate(response = ifelse(nmpsp_dv %in% c("missing", "proxy", "refusal", 
                                           "don't know"), 99,
                           ifelse(nnmpsp_dv %in% c("missing", "proxy", "refusal", "don't know"), 99,
                                  ifelse(nunmpsp_dv %in% c("missing", "proxy", "refusal", "don't know"),99,1))),
  ) %>% 
  mutate(wv=paste0("wv_",wv_n)) %>% 
  dplyr::select(-c(wv_n, nmpsp_dv,nnmpsp_dv,nunmpsp_dv)) %>% 
  pivot_wider(names_from = wv, values_from = response, values_fill = 99)

emp_spells.seq.a <- seqdef(emp_spells_wide_a, 2:5, states = emp_spells_code,
                        labels = emp_spells_labs)
emp_spells.seq.b <- seqdef(emp_spells_wide_b, 2:5, states = emp_spells_code,
                        labels = emp_spells_labs)


## Index plot - first 10 sequences
#png("./output/descriptive/seqiplot100_missing.png", width = 960, height = 960)
par(mfrow=c(1,3))
seqiplot(emp_spells.seq.a,
         idxs=1:100, # to add more lines
         with.legend = F, 
         main = "a",
         border = NA)
seqiplot(emp_spells.seq.b,
         idxs=1:100, # to add more lines
         with.legend = F, 
         main = "b",
         border = NA)
seqlegend(emp_spells.seq.a, cex = 1.3)
#dev.off()

## Index plot - all sequences
#png("output/descriptive/seqiplot_missing.png", width = 960, height = 1920)
par(mfrow=c(1,3))
seqIplot(emp_spells.seq.a,
         #         group = spine.seq$wv_1,
         with.legend = F, 
         main = "a",
         border = NA)
seqIplot(emp_spells.seq.b,
         #         group = spine.seq$wv_1,
         with.legend = F, 
         main = "b",
         border = NA)
seqlegend(spine.seq.a, cex = 1.3)
#dev.off()

# sequence frequency plot (all common sequences)
#png("output/descriptive/seqfplot_missing.png", width = 960, height = 1920)
par(mfrow=c(1,3))
seqfplot(emp_spells.seq.a, 
         idxs=1:900, # to add more lines
         with.legend = F, 
         border = NA, 
         main = "a")
seqfplot(emp_spells.seq.b, 
         idxs=1:900, # to add more lines
         with.legend = F, 
         border = NA, 
         main = "b")
seqlegend(emp_spells.seq.a, cex = 1.3)
#dev.off()

# state distribution plot
#png("output/descriptive/seqdplot_missing.png", width = 960, height = 960)
par(mfrow=c(1,3))
seqdplot(emp_spells.seq.a, 
         with.legend = F, 
         border = NA, 
         main = "a")
seqdplot(emp_spells.seq.b, 
         with.legend = F, 
         border = NA, 
         main = "b")
seqlegend(emp_spells.seq.a, cex = 1.3)
#dev.off()

#png("output/descriptive/legend_missing.png", width = 240, height = 190)
seqlegend(emp_spells.seq.a, cex = 1.3)
#dev.off()

### second job (j2has) ---------------------------------------------------------

## specify labels and codes (same for both samples)
j2has_labs <- c("response","missing")
j2has_code <- c("RESP","NA")


## wide format for sequencing data
# waves 3-6
j2has_wide_a <- dfas1a %>%
  dplyr::select(pidp,wv_n,j2has) %>% 
  mutate(response = ifelse(j2has %in% c("missing", "proxy", "refusal", 
                                          "Only available for IEMB", "Not available for IEMB",
                                          "don't know"),99,1)) %>% 
  mutate(wv=paste0("wv_",wv_n)) %>% 
  dplyr::select(-c(wv_n, j2has)) %>% 
  pivot_wider(names_from = wv, values_from = response, values_fill = 99)

# waves 7-10
j2has_wide_b <- dfas1b %>%
  dplyr::select(pidp,wv_n,j2has) %>% 
  mutate(response = ifelse(j2has %in% c("missing", "proxy", "refusal", 
                                          "Only available for IEMB", "Not available for IEMB",
                                          "don't know"),99,1)) %>% 
  mutate(wv=paste0("wv_",wv_n)) %>% 
  dplyr::select(-c(wv_n, j2has)) %>% 
  pivot_wider(names_from = wv, values_from = response, values_fill = 99)


j2has.seq.a <- seqdef(j2has_wide_a, 2:5, states = j2has_code,
                        labels = j2has_labs)
j2has.seq.b <- seqdef(j2has_wide_b, 2:5, states = j2has_code,
                        labels = j2has_labs)


## Index plot - first 10 sequences
#png("./output/descriptive/seqiplot100_missing.png", width = 960, height = 960)
par(mfrow=c(1,3))
seqiplot(j2has.seq.a,
         idxs=1:100, # to add more lines
         with.legend = F, 
         main = "a",
         border = NA)
seqiplot(j2has.seq.b,
         idxs=1:100, # to add more lines
         with.legend = F, 
         main = "b",
         border = NA)
seqlegend(j2has.seq.a, cex = 1.3)
#dev.off()

## Index plot - all sequences
#png("output/descriptive/seqiplot_missing.png", width = 960, height = 1920)
par(mfrow=c(1,3))
seqIplot(j2has.seq.a,
         #         group = spine.seq$wv_1,
         with.legend = F, 
         main = "a",
         border = NA)
seqIplot(j2has.seq.b,
         #         group = spine.seq$wv_1,
         with.legend = F, 
         main = "b",
         border = NA)
seqlegend(spine.seq.a, cex = 1.3)
#dev.off()

# sequence frequency plot (all common sequences)
#png("output/descriptive/seqfplot_missing.png", width = 960, height = 1920)
par(mfrow=c(1,3))
seqfplot(j2has.seq.a, 
         idxs=1:900, # to add more lines
         with.legend = F, 
         border = NA, 
         main = "a")
seqfplot(j2has.seq.b, 
         idxs=1:900, # to add more lines
         with.legend = F, 
         border = NA, 
         main = "b")
seqlegend(j2has.seq.a, cex = 1.3)
#dev.off()

# state distribution plot
#png("output/descriptive/seqdplot_missing.png", width = 960, height = 960)
par(mfrow=c(1,3))
seqdplot(j2has.seq.a, 
         with.legend = F, 
         border = NA, 
         main = "a")
seqdplot(j2has.seq.b, 
         with.legend = F, 
         border = NA, 
         main = "b")
seqlegend(j2has.seq.a, cex = 1.3)
#dev.off()

#png("output/descriptive/legend_missing.png", width = 240, height = 190)
seqlegend(j2has.seq.a, cex = 1.3)
#dev.off()

