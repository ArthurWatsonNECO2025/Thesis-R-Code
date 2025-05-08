#### Packages and Data ####
install.packages("tidyverse")
install.packages("broom")
install.packages("gt")
install.packages("plotrix")
install.packages("naniar")
install.packages("FSA")
library(FSA)
library(tidyverse)
library(broom)
library(gt)
library(plotrix)
library(naniar)

elisa <- read_csv('https://raw.githubusercontent.com/ArthurWatsonNECO2025/Thesis-Data/ELISA-Data/Final%20Compiled%20ELISA.csv')

#### Data Manipulation ####
# Make time a factor
elisa <- elisa %>% 
  mutate(time = as.factor(time))

# Setting levels to treatment
elisa$treatment <- elisa$treatment %>% 
  factor(levels = c("white",
                    "blue"))

#### Preliminary Data Evaluation ####
### Outliers
# Looking for outliers
elisa %>% 
  ggplot(aes(x = time,
             y = concentration,
             fill = treatment)) +
  geom_boxplot() + 
  labs(title = "Tyrosinase ELISA",
       x = "Time",
       y = "Concentration") # Note white oultiers @ 14:00, and blue outliers @ 14:00 & 19:00

# Evaluating if the WHITE outliers are >2 standard deviations
elisa %>% filter(treatment == "white",
                 time == "14:00:00") %>% 
  pull(concentration) %>% 
  summary() #mean=11.215
elisa %>% filter(treatment == "white",
                 time == "14:00:00") %>% 
  pull(concentration) %>% 
  sd() #sd=3.987635
11.215+(3.987635*2) > 21.419764 #FALSE -> remove outlier
11.215-(3.987635*2) < 4.795188 #TRUE -> keep value

# Evaluating if the BLUE outliers are >2 standard deviations
elisa %>% filter(treatment == "blue",
                 time == "14:00:00") %>% 
  pull(concentration) %>% 
  summary() #mean=8.834
elisa %>% filter(treatment == "blue",
                 time == "14:00:00") %>% 
  pull(concentration) %>% 
  sd() #sd=3.299294
8.834+(3.299294*2) > 17.7864468 #FALSE -> remove outlier
elisa %>% filter(treatment == "blue",
                 time == "19:00:00") %>% 
  pull(concentration) %>% 
  summary() #mean=1.6769
elisa %>% filter(treatment == "blue",
                 time == "19:00:00") %>% 
  pull(concentration) %>% 
  sd() #sd=1.074113
1.6769+(1.074113*2) > 4.7140367 #FALSE -> remove outlier

# Removing outliers & making files to merge
elisa_w07 <- elisa %>% 
  filter(treatment == "white",
         time == "07:00:00")

elisa_b07 <- elisa %>% 
  filter(treatment == "blue",
         time == "07:00:00")

elisa_w14 <- elisa %>% 
  filter(treatment == "white",
         time == "14:00:00") %>% 
  mutate(concentration = if_else(concentration<20,
                                 concentration,
                                 NA_real_))

elisa_b14 <- elisa %>% 
  filter(treatment == "blue",
         time == "14:00:00") %>% 
  mutate(concentration=if_else(concentration<17,
                               concentration,
                               NA_real_))

elisa_w19 <- elisa %>% 
  filter(treatment == "white",
         time == "19:00:00")

elisa_b19 <- elisa %>% 
  filter(treatment == "blue",
         time == "19:00:00") %>% 
  mutate(concentration=if_else(concentration<4,
                               concentration,
                               NA_real_))

elisa_w00 <- elisa %>% 
  filter(treatment == "white",
         time == "00:00:00")

elisa_b00 <- elisa %>% 
  filter(treatment == "blue",
         time == "00:00:00")

elisa_clean <- rbind(elisa_w07,
                     elisa_b07,
                     elisa_w14,
                     elisa_b14,
                     elisa_w19,
                     elisa_b19,
                     elisa_w00,
                     elisa_b00)

### Assessing Normality
# Column to test
columns_to_test <- c("concentration")
# Times to test
times_to_test <- c("07:00:00",
                   "14:00:00",
                   "19:00:00",  
                   "00:00:00")
  
# Data frame of conditions to test
conditions <- expand.grid(treatment = c("white",
                                        "blue"),
                          stringsAsFactors = FALSE)
# Data frame of desired combinations to test
combinations <- expand.grid(columns_to_test,
                            times_to_test,
                            conditions$treatment,
                            stringsAsFactors = FALSE)
names(combinations) <- c("variable", "time", "treatment") # Naming columns to work with the created function
# Function to perform Shapiro-Wilk test on specified combinations 
perform_shapiro_test <- function(variable, time, treatment) 
  {
    elisa_clean %>%
      filter(treatment == !!treatment) %>%
      filter(time == !!time) %>% 
      pull({{ variable }}) %>%
      shapiro.test() %>%
      tidy() %>%
      mutate(treatment = !!treatment, 
             variable = quo_name(enquo(variable)),
             time = !!time)
  }
  
# Data frame applying the function
normality_results <- pmap_dfr(combinations, ~ perform_shapiro_test(..1, ..2, ..3))

# Setting time factors that match with the ZT levels
normality_results$time <- normality_results$time %>%
  factor(levels = c("07:00:00",
                    "14:00:00",
                    "19:00:00",
                    "00:00:00"))

# Widening the data to be more readable
elisa_normality_results_wide <- normality_results %>% 
  select(-statistic) %>% #removing test statistic column
  pivot_wider(names_from = time,
              values_from = p.value)
#Adding Experiment and Interventions
elisa_norm <- elisa_normality_results_wide %>% 
  mutate(experiment = "elisa",
         Treatment = treatment)

# Table Showing Results
table_elisa_norm <- elisa_normality_results_wide %>% 
  select(-method, -variable) %>% 
  mutate(treatment = str_to_sentence(treatment)) %>% 
  gt(groupname_col = "treatment",
     row_group_as_column = TRUE) %>% 
  tab_header(title = "ELISA Concentration",
             subtitle = "Choroid Samples") %>% 
  cols_label(treatment = "Treatment",
             starts_with("07") ~ "ZT00",
             starts_with("14") ~ "ZT06", #replacing with ZT
             starts_with("19") ~ "ZT12",
             starts_with("00") ~ "ZT16") %>% 
  cols_align(align = "center",
             columns = 2:5) %>% 
  tab_options(column_labels.font.weight = "bold",
              table.font.size = 12,) %>% 
  fmt_number(decimals = 3)

print(table_elisa_norm) # Note white & blue @ ZT16 are non-parametric per the Shapiro-Wilk test

### Summary 
# Shaping data
elisa_clean_wide <- elisa_clean %>% 
  pivot_wider(names_from = time,
              values_from = concentration) # shaping data

# Summary of concentrations at different times for white & blue
elisa_summary <- elisa_clean_wide %>%
  group_by(treatment) %>% 
  summarise(across(all_of(times_to_test),
            list(mean = ~ mean(.x, na.rm = TRUE),
                 se = ~ std.error(.x),
                 max = ~ mean(.x, na.rm = TRUE) + std.error(.x),
                 min = ~ mean(.x, na.rm = TRUE) - std.error(.x)),
            .names = "{col}_{fn}")) # Renames column

#### Statistical Analysis ####
## Variation in concentration with time; Non-parametric testing
# White
elisa_clean %>% 
  filter(treatment == "white") %>% 
  kruskal.test(concentration ~ time, data=.) #p=5.398e-05

elisa_clean %>% 
  filter(treatment == "white") %>% 
  dunnTest(concentration ~ time,
           data = .,
           method = "bonferroni")
# Blue
elisa_clean %>% 
  filter(treatment == "blue") %>% 
  kruskal.test(concentration ~ time, data=.) #p=3.012e-10

elisa_clean %>% 
  filter(treatment == "blue") %>% 
  dunnTest(concentration ~ time,
           data = .,
           method = "bonferroni")

## Difference between Blue & White
# Parametric tests
elisa_clean %>% 
  filter(time == "07:00:00") %>% #ZT00
  t.test(concentration ~ treatment, data = .) #p=8.113e-05

elisa_clean %>% #ZT06
  filter(time == "14:00:00") %>% 
  t.test(concentration ~ treatment, data = .) #p=0.03065

elisa_clean %>% #ZT12
  filter(time == "19:00:00") %>% 
  t.test(concentration ~ treatment, data = .) #p=7.196e-05

# Nonparametric tests
elisa_clean %>% #ZT16
  filter(time == "00:00:00") %>% 
  wilcox.test(concentration ~ treatment, data = .) #p=0.06905

#### Graphical Representation ####
### Shaping the data
# Making dataframes for each time and removing the suffixes
sum_07 <- elisa_summary %>% 
  select(treatment, contains("07:00:00")) %>% 
  mutate(time = "07:00:00") %>% 
  rename(mean = "07:00:00_mean",
         sem = "07:00:00_se",
         max = "07:00:00_max",
         min = "07:00:00_min")
sum_14 <- elisa_summary %>% 
  select(treatment, contains("14:00:00")) %>% 
  mutate(time = "14:00:00") %>% 
  rename(mean = "14:00:00_mean",
         sem = "14:00:00_se",
         max = "14:00:00_max",
         min = "14:00:00_min")
sum_19 <- elisa_summary %>% 
  select(treatment, contains("19:00:00")) %>% 
  mutate(time = "19:00:00") %>% 
  rename(mean = "19:00:00_mean",
         sem = "19:00:00_se",
         max = "19:00:00_max",
         min = "19:00:00_min")
sum_00 <- elisa_summary %>% 
  select(treatment, contains("00:00:00")) %>% 
  mutate(time = "00:00:00") %>% 
  rename(mean = "00:00:00_mean",
         sem = "00:00:00_se",
         max = "00:00:00_max",
         min = "00:00:00_min")
# Bindings them into one
sum_bound <- rbind(sum_07,sum_14,sum_19,sum_00)

# Creating the Zeitgeber Time; Relative to local time, lights turn on at 07:00 & off at 19:00
sum_bound$ZT <- c("00","00","06","06","12","12","16","16")

# Setting levels for graphing
sum_bound$ZT <- sum_bound$ZT %>%
  factor(levels = c("00",
                    "06",
                    "12",
                    "16"))

# White Bar Graph
elisa_white <- sum_bound %>% 
  filter(treatment == "white") %>% 
  ggplot(aes(x = ZT,
             y = mean,
             fill = treatment)) +
  scale_fill_manual("Treatment",
                    values = c("cornsilk"),
                    labels = c("White")) +
  geom_col(position = position_dodge(),
           color = "black") +
  geom_errorbar(aes(ymin = min,
                    ymax = max),
                width = 0.3) +
  labs(title = "White Light",
       x = "Zeitgeber Time",
       y = "Concentration (ng/\u00B5g)") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 40, by=10),
                     limits = c(0,40))

bar_elisa_white <- elisa_white + 
  geom_line(data = tibble(x = c(1,4), y = c(37,37)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(2.5), y = c(38)),
            aes(x = x, y = y, label = "*"),
            inherit.aes = FALSE,
            size = 7) + 
  geom_line(data = tibble(x = c(1,2), y = c(32,32)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(1.5), y = c(33)),
            aes(x = x, y = y, label = "***"),
            inherit.aes = FALSE,
            size = 7)

# Blue Bar Graph
elisa_blue <- sum_bound %>% 
  filter(treatment == "blue") %>% 
  ggplot(aes(x = ZT,
             y = mean,
             fill = treatment)) +
  scale_fill_manual("Treatment",
                    values = c("darkturquoise"),
                    labels = c("Blue")) +
  geom_col(position = position_dodge(),
           color = "black") +
  geom_errorbar(aes(ymin = min,
                    ymax = max),
                width = 0.3) +
  labs(title = "Blue Evening Light",
       x = "Zeitgeber Time",
       y = "Concentration (ng/\u00B5g)") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 20, by=5),
                     limits = c(0,20))

bar_elisa_blue <- elisa_blue + 
  geom_line(data = tibble(x = c(1,4), y = c(17.5,17.5)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(2.5), y = c(18)),
            aes(x = x, y = y, label = "***"),
            inherit.aes = FALSE,
            size = 7) + 
  geom_line(data = tibble(x = c(3,4), y = c(15,15)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(3.5), y = c(15.5)),
            aes(x = x, y = y, label = "***"),
            inherit.aes = FALSE,
            size = 7) + 
  geom_line(data = tibble(x = c(1,2), y = c(10,10)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(1.5), y = c(10.5)),
            aes(x = x, y = y, label = "*"),
            inherit.aes = FALSE,
            size = 7) + 
  geom_line(data = tibble(x = c(2,3), y = c(12.5,12.5)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(2.5), y = c(13)),
            aes(x = x, y = y, label = "***"),
            inherit.aes = FALSE,
            size = 7)

# Figure with both Blue and White Bar graphs
bar_elisa_white + bar_elisa_blue +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(family = "Times",
                                size = 14,
                                face = "bold"))

# Blue vs White Bar Graph
sum_bar <- sum_bound %>% 
  ggplot(aes(x = ZT,
             y = mean,
             fill = treatment)) +
  scale_fill_manual("Treatment",
                    values = c("cornsilk",
                               "darkturquoise"),
                    labels = c("White",
                               "Blue")) +
  geom_col(position = position_dodge(),
           color = "black") +
  geom_errorbar(aes(ymin = min,
                    ymax = max),
                width = 0.3,
                position = position_dodge(width = 0.9)) + # dodging to align centrally on each bar
  labs(x = "Zeitgeber Time",
       y = "Concentration (ng/\u00B5g)") +
  theme_classic(base_size = 13)

bar_elisa <- sum_bar + 
  geom_line(data = tibble(x = c(0.75,1.25), y = c(37,37)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(1), y = c(39)),
            aes(x = x, y = y, label = "***"),
            inherit.aes = FALSE,
            size = 7) +
  geom_line(data = tibble(x = c(1.75,2.25), y = c(16,16)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(2), y = c(18)),
            aes(x = x, y = y, label = "*"),
            inherit.aes = FALSE,
            size = 7) +
  geom_line(data = tibble(x = c(2.75,3.25), y = c(23,23)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(3), y = c(25)),
            aes(x = x, y = y, label = "***"),
            inherit.aes = FALSE,
            size = 7)

# Figures saved as PDF at size 3 x 6