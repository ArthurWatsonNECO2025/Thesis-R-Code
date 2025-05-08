#### Data & Packages ####
install.packages("tidyverse")
install.packages("broom")
install.packages("gtsummary")
install.packages("gt")
install.packages("zoo")
install.packages("plotrix")
install.packages("patchwork")
library(tidyverse)
library(broom)
library(gtsummary)
library(gt)
library(zoo)
library(plotrix)
library(patchwork)

values_removed <- read.csv('https://raw.githubusercontent.com/ArthurWatsonNECO2025/Thesis-Data/refs/heads/Supplemental/Removed%20Values.csv')
normalcy <- read.csv('https://raw.githubusercontent.com/ArthurWatsonNECO2025/Miscellaneous/refs/heads/main/normalcy.csv')

#### Tables ####
## Table Showing Outlier Values Removed
# ELISA
values_removed_ELISA_table <- values_removed %>% 
  select(treatment,measure,ZT00:ZT16) %>% 
  filter(treatment == "White" |
           treatment == "Blue") %>%
  mutate(Treatment = str_to_sentence(treatment)) %>% 
  select(-measure,-treatment) %>% 
  gt(rowname_col = "Treatment") %>% 
  tab_header(title = "ELISA TYR Concentration",
             subtitle = "Number of Outliers Discarded") %>%
  cols_align(align = "center",
             columns = 1:4) %>% 
  tab_options(column_labels.font.weight = "bold",
              table.font.size = 12)
# Rate Change
values_removed_rate_table <- values_removed %>% 
  select(treatment:intervention,AC.L,VC,CT,AL) %>% 
  filter(treatment == "saline" |
         treatment == "dopamine" |
         treatment == "apomorphine") %>% 
  filter(measure == "rate") %>% 
  mutate(Treatment = str_to_sentence(treatment),
         Intervention = str_to_sentence(intervention),
         Eye = if_else(eye == "fel", # Changing labels
                       "Fellow",
                       "Experimental")) %>% 
  arrange(factor(Eye, # Reorder groups w/ levels
                 levels = c("Experimental",
                            "Fellow"))) %>% 
  select(AC.L:Eye) %>% 
  gt(rowname_col = "Treatment",
     groupname_col = "Intervention") %>% 
  tab_header(title = "Experimental & Fellow Rate of Change",
             subtitle = "Number of Outliers Discarded") %>% 
  cols_label(AC.L = "AC+L",
             VC = "VC",
             CT = "CT",
             AL = "AL") %>% 
  cols_align(align = "center",
             columns = 1:4) %>% 
  tab_options(column_labels.font.weight = "bold",
              table.font.size = 12)
# Interocular Difference
values_removed_diff_table <- values_removed %>% 
  select(treatment:intervention,AC.L,VC,CT,AL) %>% 
  filter(treatment == "saline" |
         treatment == "dopamine" |
         treatment == "apomorphine") %>% 
  filter(measure == "ratediff") %>% 
  mutate(Treatment = str_to_sentence(treatment),
         Intervention = str_to_sentence(intervention)) %>% 
  select(AC.L:Intervention) %>% 
  gt(rowname_col = "Treatment",
     groupname_col = "Intervention") %>% 
  tab_header(title = "Interocular Rate of Change Difference",
             subtitle = "Number of Outliers Discarded") %>% 
  cols_label(AC.L = "AC+L",
             VC = "VC",
             CT = "CT",
             AL = "AL") %>% 
  cols_align(align = "center",
             columns = 1:4) %>% 
  tab_options(column_labels.font.weight = "bold",
              table.font.size = 12)

## Table of Shapiro-Wilk Results of Normalcy
#ELISA
table_elisa_norm <- normalcy %>% 
    filter(experiment == "elisa") %>% 
  select(X07.00.00:X00.00.00,Treatment) %>% 
  mutate(Treatment = str_to_sentence(Treatment)) %>% 
  gt(groupname_col = "Treatment",
     row_group_as_column = TRUE) %>% 
  tab_header(title = "ELISA Concentration",
             subtitle = "Choroid Samples") %>% 
  cols_label(Treatment = "Treatment",
             starts_with("X07") ~ "ZT00",
             starts_with("X14") ~ "ZT06",
             starts_with("X19") ~ "ZT12",
             starts_with("X00") ~ "ZT16") %>% 
  cols_align(align = "center",
             columns = 1:4) %>% 
  tab_options(column_labels.font.weight = "bold",
              table.font.size = 12) %>% 
  fmt_number(decimals = 3)
# Growth Rate
table_rate_norm <- normalcy %>% 
  filter(experiment == "rate") %>% 
  select(dAC.L,dVC,dCT,dAL,Eye,Treatment,intervention) %>% 
  mutate(Treatment = str_to_sentence(Treatment),
         intervention = str_to_sentence(intervention)) %>% 
  arrange(factor(Eye, # Reorder groups w/ levels
                 levels = c("Experimental",
                            "Fellow"))) %>% 
  gt(rowname_col = "Treatment",
     groupname_col = "intervention") %>% 
  tab_header(title = "Pharmacologic Intervention",
             subtitle = "Experimental & Fellow Rate of Change") %>% 
  cols_label(dAC.L = "AC+L",
             dVC = "VC",
             dCT = "CT",
             dAL = "AL") %>% 
  cols_align(align = "center",
             columns = 1:4) %>% 
  tab_options(column_labels.font.weight = "bold",
              table.font.size = 12) %>% 
  fmt_number(decimals = 3)
# Growth Difference
table_diff_norm <- normalcy %>% 
  filter(experiment == "ratediff") %>% 
  select(dAC.L_diff,
         dVC_diff,
         dCT_diff,
         dAL_diff,
         Treatment,
         intervention) %>% 
  mutate(Treatment = str_to_sentence(Treatment),
         intervention = str_to_sentence(intervention)) %>%
  gt(rowname_col = "Treatment",
     groupname_col = "intervention") %>% 
  tab_header(title = "Pharmacologic Intervention",
             subtitle = "Rate of Change Difference") %>% 
  cols_label(dAC.L_diff = "AC+L",
             dVC_diff = "VC",
             dCT_diff = "CT",
             dAL_diff = "AL") %>% 
  cols_align(align = "center",
             columns = 1:4) %>% 
  tab_options(column_labels.font.weight = "bold",
              table.font.size = 12) %>% 
  fmt_number(decimals = 3)