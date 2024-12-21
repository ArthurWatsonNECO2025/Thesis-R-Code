#### Packages and Data ####
install.packages("tidyverse")
install.packages("broom")
install.packages("gt")
install.packages("plotrix")
install.packages("naniar")
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

elisa_w23 <- elisa %>% 
  filter(treatment == "white",
         time == "23:00:00")

elisa_b23 <- elisa %>% 
  filter(treatment == "blue",
         time == "23:00:00")

elisa_clean <- rbind(elisa_w07,
                     elisa_b07,
                     elisa_w14,
                     elisa_b14,
                     elisa_w19,
                     elisa_b19,
                     elisa_w23,
                     elisa_b23)

### Assessing Normality
# Column to test
columns_to_test <- c("concentration")
# Times to test
times_to_test <- c("07:00:00",
                   "14:00:00",
                   "19:00:00",  
                   "23:00:00")
  
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
normality_results$time <- normality_results$time %>%
  factor(levels = c("14:00:00",
                    "19:00:00",
                    "23:00:00",
                    "07:00:00"))
# Widening the data to be more readable
normality_results_wide <- normality_results %>% 
  select(-statistic) %>% #removing test statistic column
  pivot_wider(names_from = time,
              values_from = p.value)
normality_results_wide <- 
  normality_results_wide[,c(1,2,3,5,6,7,4)] # reordering columns for the table 
# Table Showing Results
table_elisa_norm <- normality_results_wide %>% 
  select(-method) %>% 
  select(-variable) %>% 
  mutate(treatment = str_to_sentence(treatment)) %>% 
  gt(groupname_col = "Treatment",
     row_group_as_column = TRUE) %>% 
  tab_header(title = "Shapiro-Wilk Test of Normality",
             subtitle = "ELISA Concentrations") %>% 
  cols_label(treatment = "Treatment",
             starts_with("14") ~ "6", #replacing with ZT
             starts_with("19") ~ "11",
             starts_with("23") ~ "15",
             starts_with("07") ~ "23") %>% 
  tab_options(column_labels.font.weight = "bold",
              table.font.size = 12,) %>% 
  fmt_number(decimals = 3)

# Color any cell in the table that is <0.05
# for (column in times_to_test) {
#  table_elisa_norm <- table_elisa_norm %>%
#    tab_style(
#      style = cell_fill(color = "#FF6565"),
#      locations = cells_body(
#        columns = all_of(column),
#        rows = .data[[column]] < 0.05
#      )
#    )
#}

print(table_elisa_norm) # Note white & blue @ ZT 15 are non-parametric per the Shapiro-Wilk test


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
# Parametric tests
elisa_clean %>% 
  filter(time == "07:00:00") %>% 
  t.test(concentration ~ treatment, data = .) #p=8.113e-05

elisa_clean %>% 
  filter(time == "14:00:00") %>% 
  t.test(concentration ~ treatment, data = .) #p = 0.03065

elisa_clean %>% 
  filter(time == "19:00:00") %>% 
  t.test(concentration ~ treatment, data = .) #p = 7.196e-05

# Nonparametric tests
elisa_clean %>% 
  filter(time == "23:00:00") %>% 
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
sum_23 <- elisa_summary %>% 
  select(treatment, contains("23:00:00")) %>% 
  mutate(time = "23:00:00") %>% 
  rename(mean = "23:00:00_mean",
         sem = "23:00:00_se",
         max = "23:00:00_max",
         min = "23:00:00_min")
# Bindings them into one
sum_bound <- rbind(sum_07,sum_14,sum_19,sum_23)

# Creating the Zeitgeber Time; Relative to local time, lights turn on at 09:00 & off at 21:00
sum_bound$ZT <- c("23","23","6","6","11","11","15","15")
# Setting levels for graphing
sum_bound$ZT <- sum_bound$ZT %>%
  factor(levels = c("6",
                    "11",
                    "15",
                    "23"))

# Bar Graph
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
  theme_classic()

bar_elisa <- sum_bar + 
  geom_line(data = tibble(x = c(3.75,4.25), y = c(37,37)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(4), y = c(39)),
            aes(x = x, y = y, label = "*"),
            inherit.aes = FALSE) +
  geom_line(data = tibble(x = c(0.75,1.25), y = c(17,17)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(1), y = c(19)),
            aes(x = x, y = y, label = "*"),
            inherit.aes = FALSE) +
  geom_line(data = tibble(x = c(1.75,2.25), y = c(19,19)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(2), y = c(21)),
            aes(x = x, y = y, label = "*"),
            inherit.aes = FALSE)

print(bar_elisa)