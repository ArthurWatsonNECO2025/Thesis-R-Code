#### Packages and Data ####
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
### Creating data frame w/ Interocular Rate of Change Difference - change_diff
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
# Adding in measure combinations
drops_measure$AC.LPR <- drops_measure$ACPR + drops_measure$LPR
drops_measure$RT.CTPR <- drops_measure$RTPR + drops_measure$CTPR
drops_measure$AC.LPO <- drops_measure$ACPO + drops_measure$LPO
drops_measure$RT.CTPO <- drops_measure$RTPO + drops_measure$CTPO
drops_measure$dAC.L <- drops_measure$dAC + drops_measure$dL
drops_measure$dRT.CT <- drops_measure$dRT + drops_measure$dCT
#Making R & L dataframes of the change then merging them
R <- drops_measure %>% 
  filter(Eye == "R") %>% 
  select(Bird.., ACPO, LPO, VCPO, RTPO, CTPO, ALPO, dAC, dL, dVC, dRT, dCT, dAL, Treatment)
L <- drops_measure %>% 
  filter(Eye == "L") %>% 
  select(Bird.., ACPO, LPO, VCPO, RTPO, CTPO, ALPO, dAC, dL, dVC, dRT, dCT, dAL, Treatment)
drops_change <- merge(R, L,
                      by = c("Bird.."),
                      suffixes = c("_R", "_L")) %>% 
  subset(select = -Treatment_R) %>%
  rename(Treatment = Treatment_L)
# Calculating diff between growth change between the eyes using experimental minus fellow, i.e. R-L [!!!NOTE: Right eye was treated eye for drops, this is NOT the same as for the injections groups; the following code HAS been modified to reflect that]
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
         dAC.L_diff,
         dVC_diff, 
         dRT_diff, 
         dCT_diff,
         dRT.CT_diff,
         dAL_diff,
         Treatment)
}
### Creating data frame w/ End Point Difference - POST_diff
{
  # Calculating diff between end point measures between the eyes using treated minus fellow, i.e. R-L
  drops_change$ACPO_diff <- drops_change$ACPO_R - drops_change$ACPO_L
  drops_change$LPO_diff <- drops_change$LPO_R - drops_change$LPO_L
  drops_change$VCPO_diff <- drops_change$VCPO_R - drops_change$VCPO_L
  drops_change$RTPO_diff <- drops_change$RTPO_R - drops_change$RTPO_L
  drops_change$CTPO_diff <- drops_change$CTPO_R - drops_change$CTPO_L
  drops_change$ALPO_diff <- drops_change$ALPO_R - drops_change$ALPO_L
  # Creating a data frame of POST difference 
  POST_diff <- drops_change %>% 
    select(Bird.., 
           ACPO_diff, 
           LPO_diff, 
           VCPO_diff, 
           RTPO_diff, 
           CTPO_diff, 
           ALPO_diff, 
           Treatment)
  # Adding in different measure combinations
  POST_diff$AC.LPO_diff <- POST_diff$ACPO_diff + POST_diff$LPO_diff
  POST_diff$RT.CTPO_diff <- POST_diff$RTPO_diff + POST_diff$CTPO_diff
  # Setting levels to treatments
  POST_diff$Treatment <- POST_diff$Treatment %>% 
    factor(levels = c("saline",
                      "dopamine"))
  # Moving treatment column to simplify column selection
  POST_diff <- POST_diff %>% 
    select(Bird.., 
           ACPO_diff, 
           LPO_diff,
           AC.LPO_diff,
           VCPO_diff, 
           RTPO_diff, 
           CTPO_diff,
           RT.CTPO_diff,
           ALPO_diff,
           Treatment)
}

#### Rate of Change Exp vs Fellow ####
### Preliminary Data Evaluation ###
## Data Frame w/ Laterality
drops_RATE <- drops_measure %>% 
  select(Bird..,
         dAC,
         dL,
         dAC.L,
         dVC,
         dRT,
         dCT,
         dRT.CT,
         dAL,
         Eye,
         Treatment)
## Looking for Outliers
{
  # Shaping the data for manipulation
  drops_RATE_long <- drops_RATE %>% 
    pivot_longer(cols = 2:9,
                 names_to = "Variable",
                 values_to = "Rate")
  # Set variable as factor & giving levels for clarity
  drops_RATE_long$Variable <- drops_RATE_long$Variable %>% 
    factor(levels = c("dAC",
                      "dL",
                      "dAC.L",
                      "dVC",
                      "dRT",
                      "dCT",
                      "dRT.CT",
                      "dAL"))
  # Box plots of each difference measure
  drops_RATE_long %>% 
    filter(Eye == "R") %>% # R = Experimental eye
    ggplot(aes(x = Variable,
               y = Rate,
               fill = Treatment)) +
    geom_boxplot() + 
    labs(title = "Experimental Rate of Change",
         x = "Variables",
         y = "Difference") # Note outliers in dRT & dRT.CT
  
  drops_RATE_long %>% 
    filter(Eye == "L") %>% # L = Fellow eye
    ggplot(aes(x = Variable,
               y = Rate,
               fill = Treatment)) +
    geom_boxplot() + 
    labs(title = "Fellow Rate of Change",
         x = "Variables",
         y = "Difference") # Note outliers in dL, dCT, & dRT.CT
  }
## Evaluating if outliers are >2sd & cleaning data
{
  # Experimental Dopamine dRT
  drops_RATE_long %>% 
    filter(Eye == "R",
           Treatment == "dopamine",
           Variable == "dRT") %>% 
    summary() # mean = -0.003419
  drops_RATE_long %>% 
    filter(Eye == "R",
           Treatment == "dopamine",
           Variable == "dRT") %>% 
    pull(Rate) %>% 
    sd() # sd = 0.01364183
  -0.02658933 < -0.003419-(0.01364183*2) # FALSE -> Keep value
  # Experimental Dopamine dRT.CT
  drops_RATE_long %>% 
    filter(Eye == "R",
           Treatment == "dopamine",
           Variable == "dRT.CT") %>% 
    summary() # mean = -0.02763 
  drops_RATE_long %>% 
    filter(Eye == "R",
           Treatment == "dopamine",
           Variable == "dRT.CT") %>% 
    pull(Rate) %>% 
    sd() # sd = 0.03851172
  0.04218500 > -0.02763+(0.03851172*2) # FALSE -> Keep value
  -0.08217857 < -0.02763-(0.03851172*2) # FALSE -> Keep value
  
  # Fellow Saline dL
  drops_RATE_long %>% 
    filter(Eye == "L",
           Treatment == "saline",
           Variable == "dL") %>% 
    summary() # mean = 0.2315 
  drops_RATE_long %>% 
    filter(Eye == "L",
           Treatment == "saline",
           Variable == "dL") %>% 
    pull(Rate) %>% 
    sd() # sd = 0.06882469
  0.11388583 < 0.2315-(0.06882469*2) # FALSE -> Keep value
  # Fellow Saline dCT
  drops_RATE_long %>% 
    filter(Eye == "L",
           Treatment == "saline",
           Variable == "dCT") %>% 
    summary() # mean = -0.03999  
  drops_RATE_long %>% 
    filter(Eye == "L",
           Treatment == "saline",
           Variable == "dCT") %>% 
    pull(Rate) %>% 
    sd() # sd = 0.03847421
  -0.12144167 < -0.03999-(0.03847421*2) # TRUE -> Remove value
  
  # Fellow Saline dRT.CT
  drops_RATE_long %>% 
    filter(Eye == "L",
           Treatment == "saline",
           Variable == "dRT.CT") %>% 
    summary() # mean = -0.047725  
  drops_RATE_long %>% 
    filter(Eye == "L",
           Treatment == "saline",
           Variable == "dRT.CT") %>% 
    pull(Rate) %>% 
    sd() # sd = 0.04200963
  -0.13039000 < -0.047725-(0.04200963*2) # FALSE -> Keep value
  
  # Cleaning data
  drops_RATE_fellow_s <- drops_RATE %>% 
    filter(Eye == "L",
           Treatment == "saline") %>% 
    mutate(dCT=if_else(dCT>-0.1,
                       dCT,
                       NA_real_))
  
  drops_RATE_fellow_d <- drops_RATE %>% 
    filter(Eye == "L",
           Treatment == "dopamine")
  
  drops_RATE_experimental <- drops_RATE %>% 
    filter(Eye == "R")
  
  drops_RATE_clean <- rbind(drops_RATE_fellow_s,
                            drops_RATE_fellow_d,
                            drops_RATE_experimental)
}
## Assessing Normality
{
  # Data frame of columns to test
  columns_to_test <- c("dAC",
                       "dL",
                       "dAC.L",
                       "dVC",
                       "dRT",
                       "dCT",
                       "dRT.CT",
                       "dAL")
  # Data frame of conditions to test
  conditions <- expand.grid(Treatment = c("saline",
                                          "dopamine"),
                            Eye = c("L",
                                    "R"),
                            stringsAsFactors = FALSE)
  # Data frame of desired combinations to test
  combinations <- merge(data.frame(Variable = columns_to_test), conditions)
  # Naming columns to work with the created function
  names(combinations) <- c("Variable", "Treatment", "Eye") 
  # Function to perform Shapiro-Wilk test on specified combinations 
  perform_shapiro_test <- function(Variable, Treatment, Eye) {
    drops_RATE_clean %>%
      filter(Eye == !!Eye,
             Treatment == !!Treatment) %>%
      pull({{ Variable }}) %>%
      shapiro.test() %>%
      tidy() %>%
      mutate(Treatment = !!Treatment,
             Eye = !!Eye,
             Variable = quo_name(enquo(Variable)))
  }
  
  # Data frame applying the function
  normality_results <- pmap_dfr(combinations, ~ perform_shapiro_test(..1, ..2, ..3))
  # Widening the data to be more readable
  d_rate_normality_results_wide <- normality_results %>% 
    select(-statistic) %>% #removing test statistic column
    pivot_wider(names_from = Variable,
                values_from = p.value)
  #Adding Experiment and Interventions
  d_rate_norm <- d_rate_normality_results_wide %>% 
    mutate(experiment = "rate",
           intervention = "drops",
           Eye = if_else(Eye == "L", # Changing labels
                         "Fellow",
                         "Experimental"))
  
  # Table Showing Results
  table_d_rate_EYE_norm <- d_rate_normality_results_wide %>% 
    select(-method) %>% 
    mutate(Treatment = str_to_sentence(Treatment),
           Eye = if_else(Eye == "L", # Changing labels
                         "Fellow",
                         "Experimental")) %>% 
    arrange(factor(Eye, # Reorder groups
                   levels = c("Experimental",
                                   "Fellow"))) %>% 
    gt(rowname_col = "Treatment",
       groupname_col = "Eye") %>% 
    tab_header(title = "Ophthalmic Drops",
               subtitle = "Experimental & Fellow Rate of Change") %>% 
    cols_label(dAC = "AC",
               dL = "L",
               dAC.L = "AC&L",
               dVC = "VC",
               dRT = "RT",
               dCT = "CT",
               dRT.CT = "RT&CT",
               dAL = "AL") %>% 
    cols_align(align = "center",
               columns = 2:9) %>% 
    tab_options(column_labels.font.weight = "bold",
                table.font.size = 12) %>% 
    fmt_number(decimals = 3)
  
  print(table_d_rate_EYE_norm) # All groups are parametric per Shapiro-Wilk test EXCEPT Experimental Saline dAL
}
## Summary
{
  # Changing values from mm to µm
  drops_RATE_clean_µm <- drops_RATE_clean %>% 
    mutate(across(where(is.numeric), ~ . * 1000))
  # Outlining columns
  columns_to_summarize <- c("dAC", 
                            "dL", 
                            "dAC.L", 
                            "dVC", 
                            "dRT", 
                            "dCT", 
                            "dRT.CT", 
                            "dAL")
  # Create the summary data frame
  drops_RATE_summary <- drops_RATE_clean_µm %>%
    group_by(Treatment, Eye) %>%
    summarise(across(all_of(columns_to_summarize), 
                     list(mean = ~ mean(.x, na.rm = TRUE),
                          se = ~ std.error(.x),
                          max = ~ mean(.x, na.rm = TRUE) + std.error(.x),
                          min = ~ mean(.x, na.rm = TRUE) - std.error(.x)),
                     .names = "{col}_{fn}"))
  # Creating Levels
  drops_RATE_summary$Treatment <- drops_RATE_summary$Treatment %>% 
    factor(levels = c("saline",
                      "dopamine"))
  drops_RATE_summary$Eye <- drops_RATE_summary$Eye %>% 
    factor(levels = c("R", # experimental
                      "L")) # control
}

### Statistical Analysis ###
## Two Sample T-test
# Saline
{
  drops_RATE_clean %>% 
    filter(Treatment == "saline") %>% 
    t.test(dAC ~ Eye, data = .) #p=0.1127
  
  drops_RATE_clean %>% 
    filter(Treatment == "saline") %>% 
    t.test(dL ~ Eye, data = .) #p=0.1193
  
  drops_RATE_clean %>% 
    filter(Treatment == "saline") %>% 
    t.test(dAC.L ~ Eye, data = .) #p=0.2601
  
  drops_RATE_clean %>% 
    filter(Treatment == "saline") %>% 
    t.test(dVC ~ Eye, data = .) #p=0.569
  
  drops_RATE_clean %>% 
    filter(Treatment == "saline") %>% 
    t.test(dRT ~ Eye, data = .) #p=0.07487
  
  drops_RATE_clean %>% 
    filter(Treatment == "saline") %>% 
    t.test(dCT ~ Eye, data = .) #p=0.722
  
  drops_RATE_clean %>% 
    filter(Treatment == "saline") %>% 
    t.test(dRT.CT ~ Eye, data = .) #p=0.1471
  
# dAL saline Experimental group was nonparametric
}
# Dopamine
{
  drops_RATE_clean %>% 
    filter(Treatment == "dopamine") %>% 
    t.test(dAC ~ Eye, data = .) #p=0.3594
  
  drops_RATE_clean %>% 
    filter(Treatment == "dopamine") %>% 
    t.test(dL ~ Eye, data = .) #p=0.4753
  
  drops_RATE_clean %>% 
    filter(Treatment == "dopamine") %>% 
    t.test(dAC.L ~ Eye, data = .) #p=0.9006
  
  drops_RATE_clean %>% 
    filter(Treatment == "dopamine") %>% 
    t.test(dVC ~ Eye, data = .) #p=0.4787
  
  drops_RATE_clean %>% 
    filter(Treatment == "dopamine") %>% 
    t.test(dRT ~ Eye, data = .) #p=0.4365
  
  drops_RATE_clean %>% 
    filter(Treatment == "dopamine") %>% 
    t.test(dCT ~ Eye, data = .) #p=0.6791
  
  drops_RATE_clean %>% 
    filter(Treatment == "dopamine") %>% 
    t.test(dRT.CT ~ Eye, data = .) #p=0.8952
  
  drops_RATE_clean %>% 
    filter(Treatment == "dopamine") %>% 
    t.test(dAL ~ Eye, data = .) #p=0.3708
}
## Mann-Whitney U-test for non-parametric data
# Saline dAL is only non-parametric evaluation
{
  drops_RATE_clean %>% 
    filter(Treatment == "saline") %>% 
    wilcox.test(dAL ~ Eye, data = .) #p=0.8048
}

### Graphical Representation ###
# AC+L
AC.L_EYE_bar <- drops_RATE_summary %>% 
  select(Treatment,Eye,dAC.L_mean:dAC.L_min) %>% 
  ggplot(aes(x = Treatment,
             y = dAC.L_mean,
             fill = Eye)) +
  geom_col(position = position_dodge(),
           color = "black") +
  geom_errorbar(aes(ymin = dAC.L_min,
                    ymax = dAC.L_max),
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  labs(x = NULL,
       y = "AC+L Rate of Change\n(µm/8days)") +
  scale_x_discrete(labels = c('Saline', #changing x labels
                              'Dopamine')) +
  scale_fill_manual("Eye",
                    labels = c("R" = "Exp",
                               "L" = "Fellow"),
                    values = c("#A6CEE3",
                               "#1F78B4")) +
  theme_classic(base_size = 13) +
  scale_y_continuous(breaks = seq(0, 400, by=100),
                     limits = c(0,400))

#VC
VC_EYE_bar <- drops_RATE_summary %>% 
  select(Treatment,Eye,dVC_mean:dVC_min) %>% 
  ggplot(aes(x = Treatment,
             y = dVC_mean,
             fill = Eye)) +
  geom_col(position = position_dodge(),
           color = "black") +
  geom_errorbar(aes(ymin = dVC_min,
                    ymax = dVC_max),
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  labs(x = NULL,
       y = "VC Rate of Change\n(µm/8days)") +
  scale_x_discrete(labels = c('Saline', #changing x labels
                              'Dopamine')) +
  scale_fill_manual("Eye",
                    labels = c("R" = "Exp",
                               "L" = "Fellow"),
                    values = c("#A6CEE3",
                               "#1F78B4")) +
  theme_classic(base_size = 13) +
  scale_y_continuous(breaks = seq(0, 500, by=100),
                     limits = c(0,500))

#CT
CT_EYE_bar <- drops_RATE_summary %>% 
  select(Treatment,Eye,dCT_mean:dCT_min) %>% 
  ggplot(aes(x = Treatment,
             y = dCT_mean,
             fill = Eye)) +
  geom_col(position = position_dodge(),
           color = "black") +
  geom_errorbar(aes(ymin = dCT_min,
                    ymax = dCT_max),
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  labs(x = NULL,
       y = "CT Rate of Change\n(µm/8days)") +
  scale_x_discrete(labels = c('Saline', #changing x labels
                              'Dopamine')) +
  scale_fill_manual("Eye",
                    labels = c("R" = "Exp",
                               "L" = "Fellow"),
                    values = c("#A6CEE3",
                               "#1F78B4")) +
  theme_classic(base_size = 13) +
  scale_y_continuous(breaks = seq(-50, 0, by=10),
                     limits = c(-50,0))

# AL
AL_EYE_bar <- drops_RATE_summary %>% 
  select(Treatment,Eye,dAL_mean:dAL_min) %>% 
  ggplot(aes(x = Treatment,
             y = dAL_mean,
             fill = Eye)) +
  geom_col(position = position_dodge(),
           color = "black") +
  geom_errorbar(aes(ymin = dAL_min,
                    ymax = dAL_max),
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  labs(x = NULL,
       y = "AL Rate of Change\n(µm/8days)") +
  scale_x_discrete(labels = c('Saline', #changing x labels
                              'Dopamine')) +
  scale_fill_manual("Eye",
                    labels = c("R" = "Exp",
                               "L" = "Fellow"),
                    values = c("#A6CEE3",
                               "#1F78B4")) +
  theme_classic(base_size = 13) +
  scale_y_continuous(breaks = seq(0, 800, by=200),
                     limits = c(0,850))

# Creating a figure with all graphs, labels, & one legend
(AC.L_EYE_bar + VC_EYE_bar) / (CT_EYE_bar + AL_EYE_bar) +
  plot_layout(guides = "collect")

#### Rate of Change Difference ####
### Preliminary Data Evaluation ###
## Looking for Outliers
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
## Evaluating if outliers are >2sd & cleaning data
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

# Cleaning data - all outliers < 2sd, no removals needed
change_diff_clean <- change_diff
}
## Assessing Normality
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
d_diff_normality_results_wide <- normality_results %>% 
  select(-statistic) %>% #removing test statistic column
  pivot_wider(names_from = Variable,
              values_from = p.value)
#Adding Experiment and Interventions
d_diff_norm <- d_diff_normality_results_wide %>% 
  mutate(experiment = "ratediff",
         intervention = "drops")

# Table Showing Results
table_d_diff_norm <- d_diff_normality_results_wide %>% 
  select(-method) %>% 
  mutate(Treatment = str_to_sentence(Treatment)) %>% 
  gt(groupname_col = "Treatment",
     row_group_as_column = TRUE) %>% 
  tab_header(title = "Ophthalmic Drops",
             subtitle = "Interocular Rate of Change Difference") %>% 
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

print(table_d_diff_norm) # All groups are parametric per Shapiro-Wilk test
}
## Summary
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

### Statistical Analysis ###
## One Sample T-test for Saline
{
change_diff_clean %>% 
  filter(Treatment == "saline") %>% 
  pull(dAC_diff) %>% 
  t.test(.) # p=0.1589

change_diff_clean %>% 
  filter(Treatment == "saline") %>% 
  pull(dL_diff) %>% 
  t.test(.) # p=0.1682

change_diff_clean %>% 
  filter(Treatment == "saline") %>% 
  pull(dAC.L_diff) %>% 
  t.test(.) # p=0.3388

change_diff_clean %>% 
  filter(Treatment == "saline") %>% 
  pull(dVC_diff) %>% 
  t.test(.) # p=0.4222

change_diff_clean %>% 
  filter(Treatment == "saline") %>% 
  pull(dRT_diff) %>% 
  t.test(.) # p=0.05681

change_diff_clean %>% 
  filter(Treatment == "saline") %>% 
  pull(dCT_diff) %>% 
  t.test(.) # p=0.4515

change_diff_clean %>% 
  filter(Treatment == "saline") %>% 
  pull(dRT.CT_diff) %>% 
  t.test(.) # p=0.2273

change_diff_clean %>% 
  filter(Treatment == "saline") %>% 
  pull(dAL_diff) %>% 
  t.test(.) # p=0.9442
}
## Two Sample T-test
{
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
}

### Graphical Representation ###
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
composite_bar_RATE <- change_diff_summary_long_mean %>% 
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
       y = "Interocular Rate Difference\n(X-F; µm/8days)") +
  scale_fill_manual("Treatment",
                    values = c("#fde725",
                               "#21918c"),
                    labels = c("Saline",
                               "Dopamine")) +
  theme_classic(base_size = 13) +
  scale_y_continuous(breaks = seq(-60, 60, by=20),
                     limits = c(-70,60))

print(composite_bar_RATE)

# AC+L
AC.L_RATE_bar <- change_diff_summary %>% 
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
       y = "AC+L Rate Difference\n(X-F; µm/8days)") +
  scale_x_discrete(labels = c('Saline', #changing x labels
                              'Dopamine')) +
  scale_fill_manual("Treatment",
                    values = c("#fde725",
                               "#21918c")) +
  theme_classic(base_size = 13) +
  scale_y_continuous(breaks = seq(-40, 40, by=20),
                     limits = c(-40,40))

print(AC.L_RATE_bar)

#VC
VC_RATE_bar <- change_diff_summary %>% 
  select(Treatment,dVC_diff_mean:dVC_diff_min) %>% 
  ggplot(aes(x = Treatment,
             y = dVC_diff_mean,
             fill = Treatment)) +
  geom_col(color = "black",
           show.legend = FALSE) +
  geom_errorbar(aes(ymin = dVC_diff_min,
                    ymax = dVC_diff_max),
                width = 0.3) +
  labs(x = NULL,
       y = "VC Rate Difference\n(X-F; µm/8days)") +
  scale_x_discrete(labels = c('Saline', #changing x labels
                              'Dopamine')) +
  scale_fill_manual("Treatment",
                    values = c("#fde725",
                               "#21918c")) +
  theme_classic(base_size = 13) +
  scale_y_continuous(breaks = seq(-60, 20, by=20),
                     limits = c(-60,20))

print(VC_RATE_bar)

#CT
CT_RATE_bar <- change_diff_summary %>% 
  select(Treatment,dCT_diff_mean:dCT_diff_min) %>% 
  ggplot(aes(x = Treatment,
             y = dCT_diff_mean,
             fill = Treatment)) +
  geom_col(color = "black",
           show.legend = FALSE) +
  geom_errorbar(aes(ymin = dCT_diff_min,
                    ymax = dCT_diff_max),
                width = 0.3) +
  labs(x = NULL,
       y = "CT Rate Difference\n(X-F; µm/8days)") +
  scale_x_discrete(labels = c('Saline', #changing x labels
                              'Dopamine')) +
  scale_fill_manual("Treatment",
                    values = c("#fde725",
                               "#21918c")) +
  theme_classic(base_size = 13) +
  scale_y_continuous(breaks = seq(-20, 60, by=20),
                     limits = c(-20,60))

print(CT_RATE_bar)

# AL
AL_RATE_bar <- change_diff_summary %>% 
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
       y = "AL Rate Difference\n(X-F; µm/8days)") +
  scale_x_discrete(labels = c('Saline', #changing x labels
                              'Dopamine')) +
  scale_fill_manual("Treatment",
                    values = c("#fde725",
                               "#21918c")) +
  theme_classic(base_size = 13) +
  scale_y_continuous(breaks = seq(-40, 40, by=20),
                     limits = c(-40,40))

print(AL_RATE_bar)

# Creating a figure with all graphs, labels, & one legend
(AC.L_RATE_bar + VC_RATE_bar) / (CT_RATE_bar + AL_RATE_bar)  +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(family = "Times",
                                size = 14,
                                face = "bold"))
