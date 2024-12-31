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
saline_i <- read.csv('https://raw.githubusercontent.com/ArthurWatsonNECO2025/Thesis-Data/Ultrasound-Data/Blue%20Saline%20(Intravitreal).csv')
dopamine_i <- read.csv('https://raw.githubusercontent.com/ArthurWatsonNECO2025/Thesis-Data/Ultrasound-Data/Blue%20Dopamine%20(Intravitreal).csv')
apomorphine_i <- read.csv('https://raw.githubusercontent.com/ArthurWatsonNECO2025/Thesis-Data/Ultrasound-Data/Blue%20Apomorphine%20(Intravitreal).csv')

#### Data Manipulation ####
### Calculating AL & Denoting Treatment Group
{
# Saline
#Calculating AL and denoting treatment group
saline_i$AL <- saline_i$AC + saline_i$L + saline_i$VC + saline_i$RT + saline_i$CT #Creating a calculated axial length column
saline_i$Treatment <- "saline" #Creating an indicator of experimental group
sal_i <- saline_i %>% select(Bird.., AC, L, VC, RT, CT, AL, Measure, Eye, Treatment) #Simplifying data & removing SEMs

# Dopamine Data
#The same manipulation done for saline is done for the dopamine data
dopamine_i$AL <- dopamine_i$AC + dopamine_i$L + dopamine_i$VC + dopamine_i$RT + dopamine_i$CT
dopamine_i$Treatment <- "dopamine"
dop_i <- dopamine_i %>% select(Bird.., AC, L, VC, RT, CT, AL, Measure, Eye, Treatment)

# Apomorphine Data
#The same manipulation done for saline is done for the apomorphine data
apomorphine_i$AL <- apomorphine_i$AC + apomorphine_i$L + apomorphine_i$VC + apomorphine_i$RT + apomorphine_i$CT
apomorphine_i$Treatment <- "apomorphine"
apo_i <- apomorphine_i %>% select(Bird.., AC, L, VC, RT, CT, AL, Measure, Eye, Treatment)
}
### Creating data frame w/ change in growth difference
{
# Creating an inclusive dataframe
intravitreal <- rbind(sal_i, dop_i, apo_i) 
# Altering Bird# variable to remove laterality and file type thereby allowing the function merge() to work
intravitreal$Bird.. <- sub(".{1}\\.[^.]+$", "", intravitreal$Bird..) 
# Creating PRE and POST data frames then merging them
PRE <- intravitreal %>% 
  filter(Measure == "PRE") %>% 
  select(Bird.., AC, L, VC, RT, CT, AL, Eye, Treatment)
POST <- intravitreal %>% 
  filter(Measure == "POST") %>% 
  select(Bird.., AC, L, VC, RT, CT, AL, Eye, Treatment)
intravitreal_measure <- merge(PRE, POST,
                              by = c("Bird..", "Eye"),
                              suffixes = c("PR", "PO")) %>% 
  subset(select = -TreatmentPO) %>% #removing duplicate treatment column created by merge()
  rename(Treatment = TreatmentPR)
# Calculate Ocular Biometry Change POST-PRE
intravitreal_measure$dAC <- intravitreal_measure$ACPO - intravitreal_measure$ACPR
intravitreal_measure$dL <- intravitreal_measure$LPO - intravitreal_measure$LPR
intravitreal_measure$dVC <- intravitreal_measure$VCPO - intravitreal_measure$VCPR
intravitreal_measure$dRT <- intravitreal_measure$RTPO - intravitreal_measure$RTPR
intravitreal_measure$dCT <- intravitreal_measure$CTPO - intravitreal_measure$CTPR
intravitreal_measure$dAL <- intravitreal_measure$ALPO - intravitreal_measure$ALPR
#Making R & L dataframes of the change then merging them
R <- intravitreal_measure %>% 
  filter(Eye == "R") %>% 
  select(Bird.., dAC, dL, dVC, dRT, dCT, dAL, Treatment)
L <- intravitreal_measure %>% 
  filter(Eye == "L") %>% 
  select(Bird.., dAC, dL, dVC, dRT, dCT, dAL, Treatment)
intravitreal_change <- merge(R, L,
                          by = c("Bird.."),
                          suffixes = c("_R", "_L")) %>% 
  subset(select = -Treatment_R) %>%
  rename(Treatment = Treatment_L)
# Calculating diff between growth change between the eyes using treated minus fellow, i.e. L-R
intravitreal_change$dAC_diff <- intravitreal_change$dAC_L - intravitreal_change$dAC_R
intravitreal_change$dL_diff <- intravitreal_change$dL_L - intravitreal_change$dL_R
intravitreal_change$dVC_diff <- intravitreal_change$dVC_L - intravitreal_change$dVC_R
intravitreal_change$dRT_diff <- intravitreal_change$dRT_L - intravitreal_change$dRT_R
intravitreal_change$dCT_diff <- intravitreal_change$dCT_L - intravitreal_change$dCT_R
intravitreal_change$dAL_diff <- intravitreal_change$dAL_L - intravitreal_change$dAL_R
# Creating a data frame of difference in change 
change_diff <- intravitreal_change %>% 
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
                    "dopamine",
                    "apomorphine"))
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
# Set variable as factor for clarity
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
       y = "Difference") # Note outliers in dAC_diff, dL_diff, & dAC.L_diff
}
### Evaluating if outliers are >2sd & cleaning data
{
# Saline dAC_diff
change_diff_long %>% 
  filter(Treatment == "saline",
         Variable == "dAC_diff") %>% 
  summary() # mean = -0.05514
change_diff_long %>% 
  filter(Treatment == "saline",
         Variable == "dAC_diff") %>% 
  pull(Diff) %>% 
  sd() # sd = 0.129799
0.229 > -0.05514+(0.129799*2) # TRUE -> discard value
0.0717 > -0.05514+(0.129799*2) # FALSE -> keep
-0.329 < -0.05514-(0.129799*2) # TRUE -> discard value
# Saline dL_diff
change_diff_long %>% 
  filter(Treatment == "saline",
         Variable == "dL_diff") %>% 
  summary() # mean = 0.07041
change_diff_long %>% 
  filter(Treatment == "saline",
         Variable == "dL_diff") %>% 
  pull(Diff) %>% 
  sd() # sd = 0.1636123
0.383 > 0.07041+(0.1636123*2) # FALSE -> Keep
0.259 > 0.07041+(0.1636123*2) # FALSE -> Keep
-0.268 < 0.07041-(0.1636123*2) # TRUE -> Discard
# Saline dAC.L_diff
change_diff_long %>% 
  filter(Treatment == "saline",
         Variable == "dAC.L_diff") %>% 
  summary() # mean = 0.01528
change_diff_long %>% 
  filter(Treatment == "saline",
         Variable == "dAC.L_diff") %>% 
  pull(Diff) %>% 
  sd() # sd = 0.0613549
0.189 > 0.01528+(0.0613549*2) # TRUE -> Discard
# Cleaning saline data
change_diff_s <- change_diff %>% 
  filter(Treatment == "saline") %>% 
  mutate(dAC_diff=if_else(dAC_diff<0.2 & dAC_diff>-0.2,
                          dAC_diff,
                          NA_real_),
         dL_diff=if_else(dL_diff>-0.2,
                         dL_diff,
                         NA_real_),
         dAC.L_diff=if_else(dAC.L_diff<0.1,
                            dAC.L_diff,
                            NA_real_))
# Dopamine dAC_diff
change_diff_long %>% 
  filter(Treatment == "dopamine",
         Variable == "dAC_diff") %>% 
  summary() # mean = -0.08668
change_diff_long %>% 
  filter(Treatment == "dopamine",
         Variable == "dAC_diff") %>% 
  pull(Diff) %>% 
  sd() # sd = 0.06180824
0.0322 > -0.08668+(0.06180824*2) # FALSE -> Keep
# Dopamine dL_diff
change_diff_long %>% 
  filter(Treatment == "dopamine",
         Variable == "dL_diff") %>% 
  summary() # mean = 0.03830
change_diff_long %>% 
  filter(Treatment == "dopamine",
         Variable == "dL_diff") %>% 
  pull(Diff) %>% 
  sd() # sd = 0.080817
-0.112 < 0.03830-(0.080817*2) # FALSE -> Keep
# Cleaning dopamine data
change_diff_d <- change_diff %>% 
  filter(Treatment == "dopamine") # no outliers removed
# Cleaning apomorphine
change_diff_a <- change_diff %>% 
  filter(Treatment == "apomorphine") # no outliers identified
# Merging into a cleaned file
change_diff_clean <- rbind(change_diff_s,
                           change_diff_d,
                           change_diff_a)
}
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
                                        "dopamine",
                                        "apomorphine"),
                          stringsAsFactors = FALSE)
# Data frame of desired combinations to test
combinations <- expand.grid(columns_to_test,
                            conditions$Treatment,
                            stringsAsFactors = FALSE)
names(combinations) <- c("Variable", "Treatment") # Naming columns to work with the created function
# Function to perform Shapiro-Wilk test on specified combinations 
perform_shapiro_test <- function(Variable, Treatment) {
  change_diff_clean %>%
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
table_i_norm <- normality_results_wide %>% 
  select(-method) %>% 
  mutate(Treatment = str_to_sentence(Treatment)) %>% 
  gt(groupname_col = "Treatment",
     row_group_as_column = TRUE) %>% 
  tab_header(title = "Shapiro-Wilk Test of Normality",
             subtitle = "Intravitreal Injections") %>% 
  cols_label(dAC_diff = "\u0394AC",
             dL_diff = "\u0394L",
             dAC.L_diff = "\u0394AC&L",
             dVC_diff = "\u0394VC",
             dRT_diff = "\u0394RT",
             dCT_diff = "\u0394CT",
             dRT.CT_diff = "\u0394RT&CT",
             dAL_diff = "\u0394AL") %>% 
  tab_options(column_labels.font.weight = "bold",
              table.font.size = 12,) %>% 
  fmt_number(decimals = 3)


print(table_i_norm) # Note that all groups are parametric per Shapiro-Wilk test
}
### Summary
{
# Changing values from mm to um
change_diff_clean_um <- change_diff_clean %>% 
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
change_diff_summary <- change_diff_clean_um %>%
  group_by(Treatment) %>%
  summarise(across(all_of(columns_to_summarize), 
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        se = ~ std.error(.x),
                        max = ~ mean(.x, na.rm = TRUE) + std.error(.x),
                        min = ~ mean(.x, na.rm = TRUE) - std.error(.x)),
                   .names = "{col}_{fn}"))
}
#### Statistical Analysis ####
### ANOVA
# ANOVA: AL & Treatment
change_diff_clean %>% 
  aov(dAC_diff ~ Treatment, data = .) %>% 
  summary() # p=0.367

change_diff_clean %>% 
  aov(dL_diff ~ Treatment, data = .) %>% 
  summary() # p=0.38

change_diff_clean %>% 
  aov(dAC.L_diff ~ Treatment, data = .) %>% 
  #summary() # p=0.00318 -> run tukey's HSD
  TukeyHSD() # p<0.05 for apomorphine vs saline group only

change_diff_clean %>% 
  aov(dVC_diff ~ Treatment, data = .) %>% 
  summary() # p=0.276

change_diff_clean %>% 
  aov(dRT_diff ~ Treatment, data = .) %>% 
  summary() # p=0.872

change_diff_clean %>% 
  aov(dCT_diff ~ Treatment, data = .) %>% 
  summary() # p=0.125

change_diff_clean %>% 
  aov(dRT.CT_diff ~ Treatment, data = .) %>% 
  summary() # p=0.462

change_diff_clean %>% 
  aov(dAL_diff ~ Treatment, data = .) %>% 
  #summary() # p=0.0195 -> run tukey's HSD
  TukeyHSD() # p<0.05 for apomorphine vs saline group only

#### Graphical Representation ####
### Bar chart of all variables
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
                               "#21918c",
                               "#440154"),
                    labels = c("Saline",
                               "Dopamine",
                               "Apomorphine")) +
  theme_classic()

composite_bar_final <- composite_bar +
  geom_text(data = tibble(x = c(3), y = c(20)),
            aes(x = x, y = y, label = "*"),
            inherit.aes = FALSE,
            size = 7) +
  geom_text(data = tibble(x = c(8), y = c(20)),
            aes(x = x, y = y, label = "*"),
            inherit.aes = FALSE,
            size = 7)

ggsave(composite_bar_final, # save graph
       filename = "composite_bar_final_injections.pdf",
       device = "pdf",
       width = 8,
       height = 5,
       units = "in")

### Significant ANOVA Bar Charts
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
                              'Dopamine',
                              'Apomorphine')) +
  scale_fill_manual("Treatment",
                    values = c("#fde725",
                               "#21918c",
                               "#440154")) +
  theme_classic()

AC.L_bar_final <- AC.L_bar +
  geom_line(data = tibble(x = c(1,2), y = c(-75,-75)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(1.5), y = c(-80)),
            aes(x = x, y = y, label = "n.s."),
            inherit.aes = FALSE) +
  geom_line(data = tibble(x = c(2,3), y = c(-125,-125)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(2.5), y = c(-130)),
            aes(x = x, y = y, label = "n.s."),
            inherit.aes = FALSE) +
  geom_line(data = tibble(x = c(1,3), y = c(-140,-140)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(2), y = c(-150)),
            aes(x = x, y = y, label = "*"),
            inherit.aes = FALSE,
            size = 7)
 
ggsave(AC.L_bar_final, # save graph
       filename = "AC.L_bar_final_injections.pdf",
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
                              'Dopamine',
                              'Apomorphine')) +
  scale_fill_manual("Treatment",
                    values = c("#fde725",
                               "#21918c",
                               "#440154")) +
  theme_classic()

AL_bar_final <- AL_bar + 
  geom_line(data = tibble(x = c(1,2), y = c(-187,-187)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(1.5), y = c(-195)),
            aes(x = x, y = y, label = "n.s."),
            inherit.aes = FALSE) +
  geom_line(data = tibble(x = c(2,3), y = c(-195,-195)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(2.5), y = c(-203)),
            aes(x = x, y = y, label = "n.s."),
            inherit.aes = FALSE) +
  geom_line(data = tibble(x = c(1,3), y = c(-215,-215)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(2), y = c(-227)),
            aes(x = x, y = y, label = "*"),
            inherit.aes = FALSE,
            size = 7)

ggsave(AL_bar_final, # save graph
       filename = "AL_bar_final_injections.pdf",
       device = "pdf",
       width = 8,
       height = 5,
       units = "in")