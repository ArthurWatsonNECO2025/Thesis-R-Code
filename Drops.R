#### Packages and Data ####
install.packages("tidyverse")
install.packages("broom")
install.packages("gtsummary")
install.packages("gt")
install.packages("zoo")
install.packages("plotrix")
library(tidyverse)
library(broom)
library(gtsummary)
library(gt)
library(zoo)
library(plotrix)
saline_d <- read.csv('https://raw.githubusercontent.com/ArthurWatsonNECO2025/Thesis-Data/Ultrasound-Data/Blue%20Saline%20(Drops).csv')
dopamine_d <- read.csv('https://raw.githubusercontent.com/ArthurWatsonNECO2025/Thesis-Data/Ultrasound-Data/Blue%20Dopamine%20(Drops).csv')

#### Data Manipulation ####
### Calculating AL & Denoting Treatment Group
{
# Saline
#Calculating AL and denoting treatment group
saline_d$AL <- saline_d$AC + saline_d$L + saline_d$VC + saline_d$RT + saline_d$CT #Calculated axial length column
saline_d$Treatment <- "saline" #Indicating treatment
sal_d <- saline_d %>% select(Bird.., AC, L, VC, RT, CT, AL, Measure, Eye, Treatment) #Simplifying data & removing SEMs

# Dopamine Data
#The same manipulation done for saline is done for the dopamine data
dopamine_d$AL <- dopamine_d$AC + dopamine_d$L + dopamine_d$VC + dopamine_d$RT + dopamine_d$CT
dopamine_d$Treatment <- "dopamine"
dop_d <- dopamine_d %>% select(Bird.., AC, L, VC, RT, CT, AL, Measure, Eye, Treatment)

}
### Creating data frame w/ change in growth difference
{
# Creating an inclusive dataframe
drops <- rbind(sal_d, dop_d) 
# Altering Bird# variable to remove laterality and file type thereby allowing the function merge() to work
drops$Bird.. <- sub(".{1}\\.[^.]+$", "", drops$Bird..) 
# Creating PRE and POST data frames then merging them
PRE <- drops %>% 
  filter(Measure == "PRE") %>% 
  select(Bird.., AC, L, VC, RT, CT, AL, Eye, Treatment)
POST <- drops %>% 
  filter(Measure == "POST") %>% 
  select(Bird.., AC, L, VC, RT, CT, AL, Eye, Treatment)
drops_measure <- merge(PRE, POST,
                       by = c("Bird..", "Eye"),
                       suffixes = c("PR", "PO")) %>% 
  subset(select = -TreatmentPO) %>% #removing duplicate treatment column created by merge()
  rename(Treatment = TreatmentPR)
# Calculate Ocular Biometry Change POST-PRE
drops_measure$dAC <- drops_measure$ACPO - drops_measure$ACPR
drops_measure$dL <- drops_measure$LPO - drops_measure$LPR
drops_measure$dVC <- drops_measure$VCPO - drops_measure$VCPR
drops_measure$dRT <- drops_measure$RTPO - drops_measure$RTPR
drops_measure$dCT <- drops_measure$CTPO - drops_measure$CTPR
drops_measure$dAL <- drops_measure$ALPO - drops_measure$ALPR
#Making R & L dataframes of the change then merging them
R <- drops_measure %>% 
  filter(Eye == "R") %>% 
  select(Bird.., dAC, dL, dVC, dRT, dCT, dAL, Treatment)
L <- drops_measure %>% 
  filter(Eye == "L") %>% 
  select(Bird.., dAC, dL, dVC, dRT, dCT, dAL, Treatment)
drops_change <- merge(R, L,
                      by = c("Bird.."),
                      suffixes = c("_R", "_L")) %>% 
  subset(select = -Treatment_R) %>%
  rename(Treatment = Treatment_L)
# Calculating diff between growth change between the eyes using treated minus fellow, i.e. R-L [!!!NOTE: Right eye was treated eye for drops, this is NOT the same as for the injections groups; the following code HAS been modified to reflect that]
drops_change$dAC_diff <- drops_change$dAC_R - drops_change$dAC_L
drops_change$dL_diff <- drops_change$dL_R - drops_change$dL_L
drops_change$dVC_diff <- drops_change$dVC_R - drops_change$dVC_L
drops_change$dRT_diff <- drops_change$dRT_R - drops_change$dRT_L
drops_change$dCT_diff <- drops_change$dCT_R - drops_change$dCT_L
drops_change$dAL_diff <- drops_change$dAL_R - drops_change$dAL_L
# Creating a data frame of difference in change 
change_diff <- drops_change %>% 
  select(Bird.., 
         dAC_diff, 
         dL_diff, 
         dVC_diff, 
         dRT_diff, 
         dCT_diff, 
         dAL_diff, 
         Treatment)
# Adding in different measure combinations
change_diff$dAC.L_diff <- change_diff$dAC_diff + change_diff$dL_diff
change_diff$dRT.CT_diff <- change_diff$dRT_diff + change_diff$dCT_diff
# Setting levels to treatments
change_diff$Treatment <- change_diff$Treatment %>% 
  factor(levels = c("saline",
                    "dopamine"))
# Moving treatment column to simplify column selection
change_diff <- change_diff %>% 
  select(Bird.., 
         dAC_diff, 
         dL_diff, 
         dVC_diff, 
         dRT_diff, 
         dCT_diff,
         dAC.L_diff,
         dRT.CT_diff,
         dAL_diff,
         Treatment)
}
#### Preliminary Data Evaluation ####
### Looking for Outliers
{
# Shaping the data for manipulation
change_diff_long <- change_diff %>% 
  pivot_longer(cols = 2:9,
               names_to = "Variable",
               values_to = "Diff")
# Set variable as factor & giving levels for clarity
change_diff_long$Variable <- change_diff_long$Variable %>% 
  factor(levels = c("dAC_diff",
                    "dL_diff",
                    "dAC.L_diff",
                    "dVC_diff",
                    "dRT_diff",
                    "dCT_diff",
                    "dRT.CT_diff",
                    "dAL_diff"))
# Box plots of each difference measure
change_diff_long %>% 
  ggplot(aes(x = Variable,
             y = Diff,
             fill = Treatment)) +
  geom_boxplot() + 
  labs(title = "Difference in Rate of Change",
       x = "Variables",
       y = "Difference") # Note outliers in dRT_diff, dCT_diff, dRT.CT_diff, & dAL_diff
}
### Evaluating if outliers are >2sd & cleaning data
{
# Saline dRT_diff
change_diff_long %>% 
  filter(Treatment == "saline",
         Variable == "dRT_diff") %>% 
  summary() # mean = 0.013969
change_diff_long %>% 
  filter(Treatment == "saline",
         Variable == "dRT_diff") %>% 
  pull(Diff) %>% 
  sd() # sd = 0.01570598
0.0390 > 0.013969+(0.01570598*2) # FALSE -> Keep value
-0.0128 < 0.013969-(0.01570598*2) # FALSE -> Keep value
# dopamine dCT_diff
change_diff_long %>% 
  filter(Treatment == "dopamine",
         Variable == "dCT_diff") %>% 
  summary() # mean = -0.007074
change_diff_long %>% 
  filter(Treatment == "dopamine",
         Variable == "dCT_diff") %>% 
  pull(Diff) %>% 
  sd() # sd = 0.03209658
0.0522 > -0.007074+(0.03209658*2) # FALSE -> Keep value
-0.0560 < -0.007074-(0.03209658*2) # FALSE -> Keep value
# dopamine dRT.CT_diff
change_diff_long %>% 
  filter(Treatment == "dopamine",
         Variable == "dRT.CT_diff") %>% 
  summary() # mean = -0.002577
change_diff_long %>% 
  filter(Treatment == "dopamine",
         Variable == "dRT.CT_diff") %>% 
  pull(Diff) %>% 
  sd() # sd = 0.02847239
0.0375 > -0.002577+(0.02847239*2) # FALSE -> Keep value
-0.0569 < -0.002577-(0.02847239*2) # FALSE -> Keep value
# dopamine dRT.CT_diff
change_diff_long %>% 
  filter(Treatment == "dopamine",
         Variable == "dAL_diff") %>% 
  summary() # mean = -0.018108
change_diff_long %>% 
  filter(Treatment == "dopamine",
         Variable == "dAL_diff") %>% 
  pull(Diff) %>% 
  sd() # sd = 0.04465271
0.0429 > -0.018108+(0.04465271*2) #FALSE -> Keep value
-0.105 < -0.018108-(0.04465271*2) #FALSE -> Keep value
}
### Cleaning data - all outliers < 2sd, no removals needed
change_diff_clean <- change_diff
### Assessing Normality
{
# Data frame of columns to test
columns_to_test <- c("dAC_diff",
                     "dL_diff",
                     "dAC.L_diff",
                     "dVC_diff",
                     "dRT_diff",
                     "dCT_diff",
                     "dRT.CT_diff",
                     "dAL_diff")
# Data frame of conditions to test
conditions <- expand.grid(Treatment = c("saline",
                                        "dopamine"),
                          stringsAsFactors = FALSE)
# Data frame of desired combinations to test
combinations <- expand.grid(columns_to_test,
                            conditions$Treatment,
                            stringsAsFactors = FALSE)
names(combinations) <- c("Variable", "Treatment") # Naming columns to work with the created function
# Function to perform Shapiro-Wilk test on specified combinations 
perform_shapiro_test <- function(Variable, Treatment) {
  change_diff %>%
    filter(Treatment == !!Treatment) %>%
    pull({{ Variable }}) %>%
    shapiro.test() %>%
    tidy() %>%
    mutate(Treatment = !!Treatment, 
           Variable = quo_name(enquo(Variable)))
}

# Data frame applying the function
normality_results <- pmap_dfr(combinations, ~ perform_shapiro_test(..1, ..2))
# Widening the data to be more readable
normality_results_wide <- normality_results %>% 
  select(-statistic) %>% #removing test statistic column
  pivot_wider(names_from = Variable,
              values_from = p.value)
# Table Showing Results
table_d_norm <- normality_results_wide %>% 
  select(-method) %>% 
  mutate(Treatment = str_to_sentence(Treatment)) %>% 
  gt(groupname_col = "Treatment",
     row_group_as_column = TRUE) %>% 
  tab_header(title = "Shapiro-Wilk Test of Normality",
             subtitle = "Ophthalmic Drops") %>% 
  cols_label(dAC_diff = "AC",
             dL_diff = "L",
             dAC.L_diff = "AC&L",
             dVC_diff = "VC",
             dRT_diff = "RT",
             dCT_diff = "CT",
             dRT.CT_diff = "RT&CT",
             dAL_diff = "AL") %>% 
  cols_align(align = "center",
             columns = 2:9) %>% 
  tab_options(column_labels.font.weight = "bold",
              table.font.size = 12) %>% 
  fmt_number(decimals = 3)

print(table_d_norm) # All groups are parametric per Shapiro-Wilk test

}
### Summary
{
# Changing values from mm to um
change_diff_clean_µm <- change_diff_clean %>% 
  mutate(across(where(is.numeric), ~ . * 1000))
# Outlining columns
columns_to_summarize <- c("dAC_diff", 
                          "dL_diff", 
                          "dAC.L_diff", 
                          "dVC_diff", 
                          "dRT_diff", 
                          "dCT_diff", 
                          "dRT.CT_diff", 
                          "dAL_diff")
# Create the summary data frame
change_diff_summary <- change_diff_clean_µm %>%
  group_by(Treatment) %>%
  summarise(across(all_of(columns_to_summarize), 
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        se = ~ std.error(.x),
                        max = ~ mean(.x, na.rm = TRUE) + std.error(.x),
                        min = ~ mean(.x, na.rm = TRUE) - std.error(.x)),
                   .names = "{col}_{fn}"))
}
#### Statistical Analysis ####
### Two Sample T-test
change_diff_clean %>% 
  t.test(dAC_diff ~ Treatment, data = .) #p=0.1107

change_diff_clean %>% 
  t.test(dL_diff ~ Treatment, data = .) #p=0.1511

change_diff_clean %>% 
  t.test(dAC.L_diff ~ Treatment, data = .) #p=0.4501

change_diff_clean %>% 
  t.test(dVC_diff ~ Treatment, data = .) #p=0.9959

change_diff_clean %>% 
  t.test(dRT_diff ~ Treatment, data = .) #p=0.2166

change_diff_clean %>% 
  t.test(dCT_diff ~ Treatment, data = .) #p=0.3552

change_diff_clean %>% 
  t.test(dRT.CT_diff ~ Treatment, data = .) #p=0.2191

change_diff_clean %>% 
  t.test(dAL_diff ~ Treatment, data = .) #p=0.5369

#### Graphical Representation ####
# Shaping data
change_diff_summary_long_mean <- change_diff_summary %>% 
  select(Treatment,
         ends_with("mean")) %>% 
  pivot_longer(cols = 2:9,
               names_to = "Variable",
               values_to = "Mean")
# Setting levels for graphing
change_diff_summary_long_mean$Variable <- change_diff_summary_long_mean$Variable %>% 
  factor(levels = c("dAC_diff_mean",
                    "dL_diff_mean",
                    "dAC.L_diff_mean",
                    "dVC_diff_mean",
                    "dRT_diff_mean",
                    "dCT_diff_mean",
                    "dRT.CT_diff_mean",
                    "dAL_diff_mean"))
# Composite Graph
composite_bar <- change_diff_summary_long_mean %>% 
  ggplot(aes(x = Variable,
             y = Mean,
             fill = Treatment)) +
  scale_x_discrete(labels = c('AC', #changing x labels
                              'L',
                              'AC+L',
                              'VC',
                              'RT',
                              'CT',
                              'RT+CT',
                              'AL')) +
  geom_col(position = position_dodge(),
           color = "black") + # adds black outline
  labs(title = NULL,
       x = NULL,
       y = "Interocular Rate of Change Difference (µm)") +
  scale_fill_manual("Treatment",
                    values = c("#fde725",
                               "#21918c"),
                    labels = c("Saline",
                               "Dopamine")) +
  theme_classic() +
  scale_y_continuous(breaks = seq(-150, 100, by=50),
                     limits = c(-175,110))

ggsave(composite_bar, # save graph
       filename = "composite_bar_final_drops.pdf",
       device = "pdf",
       width = 8,
       height = 5,
       units = "in")

# AC+L
AC.L_bar <- change_diff_summary %>% 
  select(Treatment,dAC.L_diff_mean:dAC.L_diff_min) %>% 
  ggplot(aes(x = Treatment,
             y = dAC.L_diff_mean,
             fill = Treatment)) +
  geom_col(color = "black",
           show.legend = FALSE) +
  geom_errorbar(aes(ymin = dAC.L_diff_min,
                    ymax = dAC.L_diff_max),
                width = 0.3) +
  labs(x = NULL,
       y = "AC+L Interocular Rate of Change Difference (µm)") +
  scale_x_discrete(labels = c('Saline', #changing x labels
                              'Dopamine')) +
  scale_fill_manual("Treatment",
                    values = c("#fde725",
                               "#21918c")) +
  theme_classic() +
  scale_y_continuous(breaks = seq(-150, 0, by=50),
                     limits = c(-160,25))


ggsave(AC.L_bar, # save graph
       filename = "AC.L_bar_final_drops.pdf",
       device = "pdf",
       width = 8,
       height = 5,
       units = "in")

# AL
AL_bar <- change_diff_summary %>% 
  select(Treatment, dAL_diff_mean:dAL_diff_min) %>% 
  ggplot(aes(x = Treatment,
             y = dAL_diff_mean,
             fill = Treatment)) +
  geom_col(color = "black",
           show.legend = FALSE) +
  geom_errorbar(aes(ymin = dAL_diff_min,
                    ymax = dAL_diff_max),
                width = 0.3) +
  labs(x = NULL,
       y = "AL Interocular Rate of Change Difference (µm)") +
  scale_x_discrete(labels = c('Saline', #changing x labels
                              'Dopamine')) +
  scale_fill_manual("Treatment",
                    values = c("#fde725",
                               "#21918c")) +
  theme_classic() +
  scale_y_continuous(breaks = seq(-200, 0, by=50),
                     limits = c(-240,30))

ggsave(AL_bar, # save graph
       filename = "AL_bar_final_drops.pdf",
       device = "pdf",
       width = 8,
       height = 5,
       units = "in")