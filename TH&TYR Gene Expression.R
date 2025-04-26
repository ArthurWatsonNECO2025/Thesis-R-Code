#### Packages, Data, & Data Formating ####
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
# The data is being imported from my GitHub, but was initially obtained from the supplementary file located here: https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE261232
# Import data
RNA_data <- read.csv('https://raw.githubusercontent.com/ArthurWatsonNECO2025/Miscellaneous/refs/heads/main/RNAraw_counts_DNRS_144samp.csv')
# The ensemble Gallus gallus gene IDs for Tyrosine Hydroxylase (ENSGALG00000029648) and Tyrosinase (ENSGALG00000017237) are used in this dataset
# Simplify data for TH and TYR only and lengthen data set
RNA_TH_TYR <- RNA_data %>% 
  filter(ensembl_gene_id %in% c("ENSGALG00000029648", #TH
                                "ENSGALG00000017237")) %>% #TYR
  pivot_longer(cols = starts_with("Z"),
               names_to = "tissue",
               values_to = "RNA_raw")
# Split data to Occluded and Open eyes for Choroid & Retina then bind
RNA_open_chor <- RNA_TH_TYR %>% 
  filter(grepl("_open", tissue)) %>%
  filter(grepl("_chor", tissue)) %>% 
  mutate(treatment = "open") %>% 
  mutate(location = "choroid")
RNA_open_ret <- RNA_TH_TYR %>% 
  filter(grepl("_open", tissue)) %>%
  filter(grepl("_ret", tissue)) %>% 
  mutate(treatment = "open") %>% 
  mutate(location = "retina")
RNA_occluded_chor <- RNA_TH_TYR %>% 
  filter(grepl("_occl", tissue)) %>%
  filter(grepl("_chor", tissue)) %>% 
  mutate(treatment = "occluded") %>% 
  mutate(location = "choroid")
RNA_occluded_ret <- RNA_TH_TYR %>% 
  filter(grepl("_occl", tissue)) %>%
  filter(grepl("_ret", tissue)) %>% 
  mutate(treatment = "occluded") %>% 
  mutate(location = "retina")
RNA_bound <- rbind(RNA_occluded_chor,
                   RNA_occluded_ret,
                   RNA_open_chor,
                   RNA_open_ret)
# Split into ZT times and then bind
RNA_ZT00 <- RNA_bound %>% 
  filter(grepl("Z00", tissue)) %>%
  mutate(ZT = "ZT00")
RNA_ZT04 <- RNA_bound %>% 
  filter(grepl("Z04", tissue)) %>%
  mutate(ZT = "ZT04")
RNA_ZT08 <- RNA_bound %>% 
  filter(grepl("Z08", tissue)) %>%
  mutate(ZT = "ZT08")
RNA_ZT12 <- RNA_bound %>% 
  filter(grepl("Z12", tissue)) %>%
  mutate(ZT = "ZT12")
RNA_ZT16 <- RNA_bound %>% 
  filter(grepl("Z16", tissue)) %>%
  mutate(ZT = "ZT16")
RNA_ZT20 <- RNA_bound %>% 
  filter(grepl("Z20", tissue)) %>%
  mutate(ZT = "ZT20")
RNA_bound_ZT <- rbind(RNA_ZT00,
                      RNA_ZT04,
                      RNA_ZT08,
                      RNA_ZT12,
                      RNA_ZT16,
                      RNA_ZT20)
# Split into TH & TYR then bind
RNA_TH <- RNA_bound_ZT %>% 
  filter(grepl("29648", ensembl_gene_id)) %>%
  mutate(enzyme = "TH")
RNA_TYR <- RNA_bound_ZT %>% 
  filter(grepl("17237", ensembl_gene_id)) %>%
  mutate(enzyme = "TYR")
RNA_bound_final <- rbind(RNA_TH,
                         RNA_TYR)
# Altering tissue column to become chick # column
RNA_final <- RNA_bound_final %>%
  mutate(tissue = substr(tissue,
                         nchar(tissue) - 3,
                         nchar(tissue))) %>%
  rename(chick = tissue)
# Removing ensemble ID column
RNA_final <- RNA_final %>% 
  select(-ensembl_gene_id)

#### Summary ####
# Widening Data
RNA_final_wide <- RNA_final %>%
  pivot_wider(names_from = treatment,
              values_from = RNA_raw)
# Outlining columns
columns_to_summarize <- c("open", 
                          "occluded")
# Create the summary data frames
RNA_summary_TH <- RNA_final_wide %>%
  filter(enzyme == "TH") %>% 
  group_by(location) %>%
  summarise(across(all_of(columns_to_summarize), 
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        se = ~ std.error(.x),
                        max = ~ mean(.x, na.rm = TRUE) + std.error(.x),
                        min = ~ mean(.x, na.rm = TRUE) - std.error(.x)),
                   .names = "{col}_{fn}"))

RNA_summary_TYR <- RNA_final_wide %>%
  filter(enzyme == "TYR") %>% 
  group_by(location) %>%
  summarise(across(all_of(columns_to_summarize), 
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        se = ~ std.error(.x),
                        max = ~ mean(.x, na.rm = TRUE) + std.error(.x),
                        min = ~ mean(.x, na.rm = TRUE) - std.error(.x)),
                   .names = "{col}_{fn}"))

#### Graphic Representation ####  
# TH gene levels in Retina & Choroid
RNA_TH_open_bar <- RNA_summary_TH %>%
  ggplot(aes(x = location,
             y = open_mean,
             fill = location)) +
  geom_col(color = "black",
           show.legend = FALSE) +
  geom_errorbar(aes(ymin = open_min,
                    ymax = open_max),
                width = 0.3) +
  labs(title = "TH in Open Eyes",
       x = NULL,
       y = "Gene Expression") +
  scale_x_discrete(labels = c('Choroid', #changing x labels
                              'Retina')) +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 40, by=10),
                     limits = c(0,40))

RNA_TH_occluded_bar <- RNA_summary_TH %>%
  ggplot(aes(x = location,
             y = occluded_mean,
             fill = location)) +
  geom_col(color = "black",
           show.legend = FALSE) +
  geom_errorbar(aes(ymin = occluded_min,
                    ymax = occluded_max),
                width = 0.3) +
  labs(title = "TH in Occluded Eyes",
       x = NULL,
       y = "Gene Expression") +
  scale_x_discrete(labels = c('Choroid', #changing x labels
                              'Retina')) +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 40, by=10),
                     limits = c(0,40))

# TYR gene values in Retina & Choroid
RNA_TYR_open_bar <- RNA_summary_TYR %>%
  ggplot(aes(x = location,
             y = open_mean,
             fill = location)) +
  geom_col(color = "black",
           show.legend = FALSE) +
  geom_errorbar(aes(ymin = open_min,
                    ymax = open_max),
                width = 0.3) +
  labs(title = "TYR in Open Eyes",
       x = NULL,
       y = "Gene Expression") +
  scale_x_discrete(labels = c('Choroid', #changing x labels
                              'Retina')) +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 3500, by=1000),
                     limits = c(0,3500))

RNA_TYR_occluded_bar <- RNA_summary_TYR %>%
  ggplot(aes(x = location,
             y = occluded_mean,
             fill = location)) +
  geom_col(color = "black",
           show.legend = FALSE) +
  geom_errorbar(aes(ymin = occluded_min,
                    ymax = occluded_max),
                width = 0.3) +
  labs(title = "TYR in Occluded Eyes",
       x = NULL,
       y = "Gene Expression") +
  scale_x_discrete(labels = c('Choroid', #changing x labels
                              'Retina')) +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 3500, by=1000),
                     limits = c(0,3500))

# Compiled Figure
(RNA_TH_open_bar + RNA_TH_occluded_bar) / (RNA_TYR_open_bar + RNA_TYR_occluded_bar)  +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(family = "Times",
                                size = 14,
                                face = "bold"))