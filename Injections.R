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
### Creating data frame w/ Interocular Rate of Change Difference - change_diff
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
# Adding in measure combinations
intravitreal_measure$AC.LPR <- intravitreal_measure$ACPR + intravitreal_measure$LPR
intravitreal_measure$RT.CTPR <- intravitreal_measure$RTPR + intravitreal_measure$CTPR
intravitreal_measure$AC.LPO <- intravitreal_measure$ACPO + intravitreal_measure$LPO
intravitreal_measure$RT.CTPO <- intravitreal_measure$RTPO + intravitreal_measure$CTPO
intravitreal_measure$dAC.L <- intravitreal_measure$dAC + intravitreal_measure$dL
intravitreal_measure$dRT.CT <- intravitreal_measure$dRT + intravitreal_measure$dCT
#Making R & L dataframes of the change then merging them
R <- intravitreal_measure %>% 
  filter(Eye == "R") %>% 
  select(Bird.., ACPO, LPO, VCPO, RTPO, CTPO, ALPO, dAC, dL, dVC, dRT, dCT, dAL, Treatment)
L <- intravitreal_measure %>% 
  filter(Eye == "L") %>% 
  select(Bird.., ACPO, LPO, VCPO, RTPO, CTPO, ALPO, dAC, dL, dVC, dRT, dCT, dAL, Treatment)
intravitreal_change <- merge(R, L,
                          by = c("Bird.."),
                          suffixes = c("_R", "_L")) %>% 
  subset(select = -Treatment_R) %>%
  rename(Treatment = Treatment_L)
# Calculating diff between growth rate change between the eyes using treated minus fellow, i.e. L-R
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
### Creating data frame w/ End Point Difference - POST_diff
{
# Calculating diff between end point measures between the eyes using treated minus fellow, i.e. L-R
intravitreal_change$ACPO_diff <- intravitreal_change$ACPO_L - intravitreal_change$ACPO_R
intravitreal_change$LPO_diff <- intravitreal_change$LPO_L - intravitreal_change$LPO_R
intravitreal_change$VCPO_diff <- intravitreal_change$VCPO_L - intravitreal_change$VCPO_R
intravitreal_change$RTPO_diff <- intravitreal_change$RTPO_L - intravitreal_change$RTPO_R
intravitreal_change$CTPO_diff <- intravitreal_change$CTPO_L - intravitreal_change$CTPO_R
intravitreal_change$ALPO_diff <- intravitreal_change$ALPO_L - intravitreal_change$ALPO_R
# Creating a data frame of POST difference 
POST_diff <- intravitreal_change %>% 
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
                    "dopamine",
                    "apomorphine"))
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

#### Rate of Change Experimental vs Fellow ####
### Preliminary Data Evaluation ###
## Data Frame w/ Laterality
intravitreal_RATE <- intravitreal_measure %>% 
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
  intravitreal_RATE_long <- intravitreal_RATE %>% 
    pivot_longer(cols = 2:9,
                 names_to = "Variable",
                 values_to = "Rate")
  # Set variable as factor & giving levels for clarity
  intravitreal_RATE_long$Variable <- 
    intravitreal_RATE_long$Variable %>% 
    factor(levels = c("dAC",
                      "dL",
                      "dAC.L",
                      "dVC",
                      "dRT",
                      "dCT",
                      "dRT.CT",
                      "dAL"))
  # Box plots of each difference measure
  intravitreal_RATE_long %>% 
    filter(Eye == "L") %>% # L = Experimental eye
    ggplot(aes(x = Variable,
               y = Rate,
               fill = Treatment)) +
    geom_boxplot() + 
    labs(title = "Experimental Rate of Change",
         x = "Variables",
         y = "Difference") # Note outliers in dAC, dAC.L, dVC, dRT, dCT, & dRT.CT
  
  intravitreal_RATE_long %>% 
    filter(Eye == "R") %>% # R = Fellow eye
    ggplot(aes(x = Variable,
               y = Rate,
               fill = Treatment)) +
    geom_boxplot() + 
    labs(title = "Fellow Rate of Change",
         x = "Variables",
         y = "Difference") # Note outliers in dAC, dL, dAC.L, dVC, & dRT.CT
  }

## Evaluating if outliers are >2sd & cleaning data
# Experimental
{
# Saline - No outliers in Exp eye
# Dopamine dAC
intravitreal_RATE_long %>% 
  filter(Eye == "L",
         Treatment == "dopamine",
         Variable == "dAC") %>% 
  summary() # mean = 0.0537087
intravitreal_RATE_long %>% 
  filter(Eye == "L",
         Treatment == "dopamine",
         Variable == "dAC") %>% 
  pull(Rate) %>% 
  sd() # sd = 0.0607754
0.172 > 0.0537087+(0.0607754*2) # FALSE -> Keep value
# Dopamine dAC.L
intravitreal_RATE_long %>% 
  filter(Eye == "L",
         Treatment == "dopamine",
         Variable == "dAC.L") %>% 
  summary() # mean = 0.2671
intravitreal_RATE_long %>% 
  filter(Eye == "L",
         Treatment == "dopamine",
         Variable == "dAC.L") %>% 
  pull(Rate) %>% 
  sd() # sd = 0.03834847
0.341 > 0.2671+(0.03834847*2) # FALSE -> Keep value
# Dopamine dVC
intravitreal_RATE_long %>% 
  filter(Eye == "L",
         Treatment == "dopamine",
         Variable == "dVC") %>% 
  summary() # mean = 0.3653
intravitreal_RATE_long %>% 
  filter(Eye == "L",
         Treatment == "dopamine",
         Variable == "dVC") %>% 
  pull(Rate) %>% 
  sd() # sd = 0.0767318
0.507 > 0.3653+(0.0767318*2) # FALSE -> Keep value
# Dopamine dRT.CT
intravitreal_RATE_long %>% 
  filter(Eye == "L",
         Treatment == "dopamine",
         Variable == "dRT.CT") %>% 
  summary() # mean = -0.0564062
intravitreal_RATE_long %>% 
  filter(Eye == "L",
         Treatment == "dopamine",
         Variable == "dRT.CT") %>% 
  pull(Rate) %>% 
  sd() # sd = 0.04956668
-0.158 < -0.0564062-(0.04956668*2) # TRUE -> Discard value
# Apomorphine dAC
intravitreal_RATE_long %>% 
  filter(Eye == "L",
         Treatment == "apomorphine",
         Variable == "dAC") %>% 
  summary() # mean = -0.005204
intravitreal_RATE_long %>% 
  filter(Eye == "L",
         Treatment == "apomorphine",
         Variable == "dAC") %>% 
  pull(Rate) %>% 
  sd() # sd = 0.06954692
0.0949 > -0.005204+(0.06954692*2) # FALSE -> Keep value
0.156 > -0.005204+(0.06954692*2) # TRUE -> Discard value
# Apomorphine dRT
intravitreal_RATE_long %>% 
  filter(Eye == "L",
         Treatment == "apomorphine",
         Variable == "dRT") %>% 
  summary() # mean = -0.015814
intravitreal_RATE_long %>% 
  filter(Eye == "L",
         Treatment == "apomorphine",
         Variable == "dRT") %>% 
  pull(Rate) %>% 
  sd() # sd = 0.02083291
0.0221 > -0.015814+(0.02083291*2) # FALSE -> Keep value
-0.0482 < -0.015814-(0.02083291*2) # FALSE -> Keep value
-0.0585 < -0.015814-(0.02083291*2) # TRUE -> Discard value
# Apomorphine dCT
intravitreal_RATE_long %>% 
  filter(Eye == "L",
         Treatment == "apomorphine",
         Variable == "dCT") %>% 
  summary() # mean = -0.02602
intravitreal_RATE_long %>% 
  filter(Eye == "L",
         Treatment == "apomorphine",
         Variable == "dCT") %>% 
  pull(Rate) %>% 
  sd() # sd = 0.03196512
0.0470 > -0.02602+(0.03196512*2) # True -> Discard value
# Apomorphine dRT.CT
intravitreal_RATE_long %>% 
  filter(Eye == "L",
         Treatment == "apomorphine",
         Variable == "dRT.CT") %>% 
  summary() # mean = -0.04184
intravitreal_RATE_long %>% 
  filter(Eye == "L",
         Treatment == "apomorphine",
         Variable == "dRT.CT") %>% 
  pull(Rate) %>% 
  sd() # sd = 0.04759435
-0.136 < -0.04184-(0.04759435*2) # False -> Keep value
}
# Fellow
{
# Saline dAC
intravitreal_RATE_long %>% 
  filter(Eye == "R",
         Treatment == "saline",
         Variable == "dAC") %>% 
  summary() # mean = 0.1389
intravitreal_RATE_long %>% 
  filter(Eye == "R",
         Treatment == "saline",
         Variable == "dAC") %>% 
  pull(Rate) %>% 
  sd() # sd = 0.1248798
0.419 > 0.1389+(0.1248798*2) # TRUE -> Discard value
-0.114 < 0.1389-(0.1248798*2) # TRUE -> Discard value
# Saline dL
intravitreal_RATE_long %>% 
  filter(Eye == "R",
         Treatment == "saline",
         Variable == "dL") %>% 
  summary() # mean = 0.15295
intravitreal_RATE_long %>% 
  filter(Eye == "R",
         Treatment == "saline",
         Variable == "dL") %>% 
  pull(Rate) %>% 
  sd() # sd = 0.1349046
-0.175 < 0.1389-(0.1349046*2) # TRUE -> Discard value
# Saline dVC
intravitreal_RATE_long %>% 
  filter(Eye == "R",
         Treatment == "saline",
         Variable == "dVC") %>% 
  summary() # mean = 0.4317
intravitreal_RATE_long %>% 
  filter(Eye == "R",
         Treatment == "saline",
         Variable == "dVC") %>% 
  pull(Rate) %>% 
  sd() # sd = 0.06192563
0.304 < 0.4317-(0.06192563*2) # TRUE -> Discard value
# Saline dRT.CT
intravitreal_RATE_long %>% 
  filter(Eye == "R",
         Treatment == "saline",
         Variable == "dRT.CT") %>% 
  summary() # mean = 0.002489
intravitreal_RATE_long %>% 
  filter(Eye == "R",
         Treatment == "saline",
         Variable == "dRT.CT") %>% 
  pull(Rate) %>% 
  sd() # sd = 0.03910569
-0.0639 < 0.002489-(0.03910569*2) # False -> Keep value
-0.0726 < 0.002489-(0.03910569*2) # False -> Keep value
# Dopamine dL
intravitreal_RATE_long %>% 
  filter(Eye == "R",
         Treatment == "dopamine",
         Variable == "dL") %>% 
  summary() # mean = 0.002489
intravitreal_RATE_long %>% 
  filter(Eye == "R",
         Treatment == "dopamine",
         Variable == "dL") %>% 
  pull(Rate) %>% 
  sd() # sd = 0.08016382
0.0126 < 0.17508-(0.08016382*2) # TRUE -> Discard value
# Dopamine dAC.L
intravitreal_RATE_long %>% 
  filter(Eye == "R",
         Treatment == "dopamine",
         Variable == "dAC.L") %>% 
  summary() # mean = 0.3155
intravitreal_RATE_long %>% 
  filter(Eye == "R",
         Treatment == "dopamine",
         Variable == "dAC.L") %>% 
  pull(Rate) %>% 
  sd() # sd = 0.05586436
0.395 > 0.3155+(0.05586436*2) # FALSE -> Keep value
0.214 < 0.3155-(0.05586436*2) # FALSE -> Keep value
# Dopamine dVC
intravitreal_RATE_long %>% 
  filter(Eye == "R",
         Treatment == "dopamine",
         Variable == "dVC") %>% 
  summary() # mean = 0.3987
intravitreal_RATE_long %>% 
  filter(Eye == "R",
         Treatment == "dopamine",
         Variable == "dVC") %>% 
  pull(Rate) %>% 
  sd() # sd = 0.0696258
0.265 < 0.3987-(0.0696258*2) # FALSE -> Keep value
# Apomorphine - No outliers in fellow group
}
# Cleaning data
{
intravitreal_RATE_experimental_s <- intravitreal_RATE %>% 
  filter(Eye == "L",
         Treatment == "saline")

intravitreal_RATE_experimental_d <- intravitreal_RATE %>% 
  filter(Eye == "L",
         Treatment == "dopamine") %>% 
  mutate(dRT.CT=if_else(dRT.CT>-0.1,
                        dRT.CT,
                        NA_real_))

intravitreal_RATE_experimental_a <- intravitreal_RATE %>% 
  filter(Eye == "L",
         Treatment == "apomorphine") %>% 
  mutate(dAC=if_else(dAC<0.15,
                     dAC,
                     NA_real_),
         dRT=if_else(dRT>-0.05,
                     dRT,
                     NA_real_),
         dCT=if_else(dCT<0.04,
                     dCT,
                     NA_real_))

intravitreal_RATE_fellow_s <- intravitreal_RATE %>% 
  filter(Eye == "R",
         Treatment == "saline") %>% 
  mutate(dAC=if_else(dAC>-0.1 & dAC<0.4,
                     dAC,
                     NA_real_),
         dL=if_else(dL>-0.1,
                    dL,
                    NA_real_),
         dVC=if_else(dVC>0.31,
                     dVC,
                     NA_real_))

intravitreal_RATE_fellow_d <- intravitreal_RATE %>% 
  filter(Eye == "R",
         Treatment == "dopamine") %>% 
  mutate(dL=if_else(dL>0.1,
                    dL,
                    NA_real_))

intravitreal_RATE_fellow_a <- intravitreal_RATE %>% 
  filter(Eye == "R",
         Treatment == "apomorphine")

intravitreal_RATE_clean <- 
  rbind(intravitreal_RATE_experimental_s,
        intravitreal_RATE_experimental_d,
        intravitreal_RATE_experimental_a,
        intravitreal_RATE_fellow_s,
        intravitreal_RATE_fellow_d,
        intravitreal_RATE_fellow_a)
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
                                          "dopamine",
                                          "apomorphine"),
                            Eye = c("L",
                                    "R"),
                            stringsAsFactors = FALSE)
  # Data frame of desired combinations to test
  combinations <- merge(data.frame(Variable = columns_to_test), conditions)
  # Naming columns to work with the created function
  names(combinations) <- c("Variable", "Treatment", "Eye") 
  # Function to perform Shapiro-Wilk test on specified combinations 
  perform_shapiro_test <- function(Variable, Treatment, Eye) {
    intravitreal_RATE_clean %>%
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
  i_rate_normality_results_wide <- normality_results %>% 
    select(-statistic) %>% #removing test statistic column
    pivot_wider(names_from = Variable,
                values_from = p.value)
  #Adding Experiment and Intervention
  i_rate_norm <- i_rate_normality_results_wide %>% 
    mutate(experiment = "rate",
           intervention = "injections",
           Eye = if_else(Eye == "R", # Changing labels
                         "Fellow",
                         "Experimental"))
  
  # Table Showing Results
  table_i_rate_EYE_norm <- i_rate_normality_results_wide %>% 
    select(-method) %>% 
    mutate(Treatment = str_to_sentence(Treatment),
           Eye = if_else(Eye == "R", # Changing labels
                         "Fellow",
                         "Experimental")) %>% 
    arrange(factor(Eye, # Reorder groups w/ levels
                   levels = c("Experimental",
                              "Fellow"))) %>% 
    gt(rowname_col = "Treatment",
       groupname_col = "Eye") %>% 
    tab_header(title = "Intravitreal Injections",
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
  
  print(table_i_rate_EYE_norm) # All groups are parametric per Shapiro-Wilk test EXCEPT Fellow Apomorphine for dAL
}

## Summary
{
  # Changing values from mm to µm
  intravitreal_RATE_clean_µm <- 
    intravitreal_RATE_clean %>% 
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
  intravitreal_RATE_summary <- 
    intravitreal_RATE_clean_µm %>%
    group_by(Treatment, Eye) %>%
    summarise(across(all_of(columns_to_summarize), 
                     list(mean = ~ mean(.x, na.rm = TRUE),
                          se = ~ std.error(.x),
                          max = ~ mean(.x, na.rm = TRUE) + std.error(.x),
                          min = ~ mean(.x, na.rm = TRUE) - std.error(.x)),
                     .names = "{col}_{fn}"))
  # Creating Levels
  intravitreal_RATE_summary$Treatment <- 
    intravitreal_RATE_summary$Treatment %>% 
    factor(levels = c("saline",
                      "dopamine",
                      "apomorphine"))
  intravitreal_RATE_summary$Eye <- 
    intravitreal_RATE_summary$Eye %>% 
    factor(levels = c("L", # experimental
                      "R")) # control
}

### Statistical Analysis ###
## Two Sample T-test
# Saline
{
  intravitreal_RATE_clean %>% 
    filter(Treatment == "saline") %>% 
    t.test(dAC ~ Eye, data = .) #p=0.02282
  
  intravitreal_RATE_clean %>% 
    filter(Treatment == "saline") %>% 
    t.test(dL ~ Eye, data = .) #p=0.2655
  
  intravitreal_RATE_clean %>% 
    filter(Treatment == "saline") %>% 
    t.test(dAC.L ~ Eye, data = .) #p=0.5253
  
  intravitreal_RATE_clean %>% 
    filter(Treatment == "saline") %>% 
    t.test(dVC ~ Eye, data = .) #p=0.001494
  
  intravitreal_RATE_clean %>% 
    filter(Treatment == "saline") %>% 
    t.test(dRT ~ Eye, data = .) #p=0.004766
  
  intravitreal_RATE_clean %>% 
    filter(Treatment == "saline") %>% 
    t.test(dCT ~ Eye, data = .) #p=0.6444
  
  intravitreal_RATE_clean %>% 
    filter(Treatment == "saline") %>% 
    t.test(dRT.CT ~ Eye, data = .) #p=0.06502
  
  intravitreal_RATE_clean %>% 
    filter(Treatment == "saline") %>% 
    t.test(dAL ~ Eye, data = .) #p=0.01073
}
# Dopamine
{
  intravitreal_RATE_clean %>% 
    filter(Treatment == "dopamine") %>% 
    t.test(dAC ~ Eye, data = .) #p=0.01122
  
  intravitreal_RATE_clean %>% 
    filter(Treatment == "dopamine") %>% 
    t.test(dL ~ Eye, data = .) #p=0.7466
  
  intravitreal_RATE_clean %>% 
    filter(Treatment == "dopamine") %>% 
    t.test(dAC.L ~ Eye, data = .) #p=0.08649
  
  intravitreal_RATE_clean %>% 
    filter(Treatment == "dopamine") %>% 
    t.test(dVC ~ Eye, data = .) #p=0.4107
  
  intravitreal_RATE_clean %>% 
    filter(Treatment == "dopamine") %>% 
    t.test(dRT ~ Eye, data = .) #p=0.1599
  
  intravitreal_RATE_clean %>% 
    filter(Treatment == "dopamine") %>% 
    t.test(dCT ~ Eye, data = .) #p=0.04586
  
  intravitreal_RATE_clean %>% 
    filter(Treatment == "dopamine") %>% 
    t.test(dRT.CT ~ Eye, data = .) #p=0.02386
  
  intravitreal_RATE_clean %>% 
    filter(Treatment == "dopamine") %>% 
    t.test(dAL ~ Eye, data = .) #p=0.0106
}
# Apomorphine
{
  intravitreal_RATE_clean %>% 
    filter(Treatment == "apomorphine") %>% 
    t.test(dAC ~ Eye, data = .) #p=0.001082
  
  intravitreal_RATE_clean %>% 
    filter(Treatment == "apomorphine") %>% 
    t.test(dL ~ Eye, data = .) #p=0.6756
  
  intravitreal_RATE_clean %>% 
    filter(Treatment == "apomorphine") %>% 
    t.test(dAC.L ~ Eye, data = .) #p=0.0002656
  
  intravitreal_RATE_clean %>% 
    filter(Treatment == "apomorphine") %>% 
    t.test(dVC ~ Eye, data = .) #p=0.05453
  
  intravitreal_RATE_clean %>% 
    filter(Treatment == "apomorphine") %>% 
    t.test(dRT ~ Eye, data = .) #p=0.05199
  
  intravitreal_RATE_clean %>% 
    filter(Treatment == "apomorphine") %>% 
    t.test(dCT ~ Eye, data = .) #p=0.2067
  
  intravitreal_RATE_clean %>% 
    filter(Treatment == "apomorphine") %>% 
    t.test(dRT.CT ~ Eye, data = .) #p=0.1294
  
# Fellow for Apomorphine dAL group non-parametric
}

## Mann-Whitney U-test for non-parametric data
# Apomorphine dAL is only non-parametric evaluation
{
  intravitreal_RATE_clean %>% 
    filter(Treatment == "apomorphine") %>% 
    wilcox.test(dAL ~ Eye, data = .) #p=5.769e-06
}

### Graphical Representation - remember to change laterallity to L=Experimental ###
# AC+L
AC.L_EYE_bar <- intravitreal_RATE_summary %>% 
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
                              'Dopamine',
                              'Apomorphine')) +
  scale_fill_manual("Eye",
                    labels = c("L" = "Exp",
                               "R" = "Fellow"),
                    values = c("#A6CEE3",
                               "#1F78B4")) +
  theme_classic(base_size = 13) +
  scale_y_continuous(breaks = seq(0, 400, by=100),
                     limits = c(0,400))

AC.L_EYE_bar_final <- AC.L_EYE_bar +
  geom_line(data = tibble(x = c(2.75,3.25),
                          y = c(380,380)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(3), y = c(390)),
            aes(x = x, y = y, label = "*"),
            inherit.aes = FALSE,
            size = 7)

#VC
VC_EYE_bar <- intravitreal_RATE_summary %>% 
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
                              'Dopamine',
                              'Apomorphine')) +
  scale_fill_manual("Eye",
                    labels = c("L" = "Exp",
                               "R" = "Fellow"),
                    values = c("#A6CEE3",
                               "#1F78B4")) +
  theme_classic(base_size = 13) +
  scale_y_continuous(breaks = seq(0, 500, by=100),
                     limits = c(0,500))

VC_EYE_bar_final <- VC_EYE_bar +
  geom_line(data = tibble(x = c(0.75,1.25),
                          y = c(480,480)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(1), y = c(500)),
            aes(x = x, y = y, label = "*"),
            inherit.aes = FALSE,
            size = 7)

#CT
CT_EYE_bar <- intravitreal_RATE_summary %>% 
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
                              'Dopamine',
                              'Apomorphine')) +
  scale_fill_manual("Eye",
                    labels = c("L" = "Exp",
                               "R" = "Fellow"),
                    values = c("#A6CEE3",
                               "#1F78B4")) +
  theme_classic(base_size = 13) +
  scale_y_continuous(breaks = seq(-60, 40, by=20),
                     limits = c(-60,40))

CT_EYE_bar_final <- CT_EYE_bar +
  geom_line(data = tibble(x = c(1.75,2.25),
                          y = c(30,30)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(2), y = c(33)),
            aes(x = x, y = y, label = "*"),
            inherit.aes = FALSE,
            size = 7) 

# AL
AL_EYE_bar <- intravitreal_RATE_summary %>% 
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
                              'Dopamine',
                              'Apomorphine')) +
  scale_fill_manual("Eye",
                    labels = c("L" = "Exp",
                               "R" = "Fellow"),
                    values = c("#A6CEE3",
                               "#1F78B4")) +
  theme_classic(base_size = 13) +
  scale_y_continuous(breaks = seq(0, 800, by=100),
                     limits = c(0,810))

AL_EYE_bar_final <- AL_EYE_bar +
  geom_line(data = tibble(x = c(0.75,1.25),
                          y = c(780,780)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(1), y = c(790)),
            aes(x = x, y = y, label = "*"),
            inherit.aes = FALSE,
            size = 7) +
  geom_line(data = tibble(x = c(1.75,2.25),
                          y = c(780,780)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(2), y = c(790)),
            aes(x = x, y = y, label = "*"),
            inherit.aes = FALSE,
            size = 7) +
  geom_line(data = tibble(x = c(2.75,3.25),
                          y = c(780,780)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(3), y = c(810)),
            aes(x = x, y = y, label = "#"),
            inherit.aes = FALSE,
            fontface = "bold")

# Creating a figure with all graphs, labels, & one legend
(AC.L_EYE_bar_final + VC_EYE_bar_final) / (CT_EYE_bar_final + AL_EYE_bar_final) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(family = "Times",
                                size = 14,
                                face = "bold"))

#### Rate of Change Difference ####
### Preliminary Data Evaluation ###
## Looking for Outliers
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
## Evaluating if outliers are >2sd & cleaning data
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
i_diff_normality_results_wide <- normality_results %>% 
  select(-statistic) %>% #removing test statistic column
  pivot_wider(names_from = Variable,
              values_from = p.value)
#Adding Experiment and Interventions
i_diff_norm <- i_diff_normality_results_wide %>% 
  mutate(experiment = "ratediff",
         intervention = "injections")

# Table Showing Results
table_i_diff_norm <- i_diff_normality_results_wide %>% 
  select(-method) %>% 
  mutate(Treatment = str_to_sentence(Treatment)) %>% 
  gt(groupname_col = "Treatment",
     row_group_as_column = TRUE) %>% 
  tab_header(title = "Intravitreal Injections",
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

print(table_i_diff_norm) # Note that all groups are parametric per Shapiro-Wilk test
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
## One Sample t-test for Injection Artifact
{
change_diff_clean %>% 
  filter(Treatment == "saline") %>% 
  pull(dAC_diff) %>% 
  t.test(.) # p=0.01256

change_diff_clean %>% 
  filter(Treatment == "saline") %>% 
  pull(dL_diff) %>% 
  t.test(.) # p=0.02754

change_diff_clean %>% 
  filter(Treatment == "saline") %>% 
  pull(dAC.L_diff) %>% 
  t.test(.) # p=0.9571

change_diff_clean %>% 
  filter(Treatment == "saline") %>% 
  pull(dVC_diff) %>% 
  t.test(.) # p=0.0001157

change_diff_clean %>% 
  filter(Treatment == "saline") %>% 
  pull(dRT_diff) %>% 
  t.test(.) # p=0.004194

change_diff_clean %>% 
  filter(Treatment == "saline") %>% 
  pull(dCT_diff) %>% 
  t.test(.) # p=0.408

change_diff_clean %>% 
  filter(Treatment == "saline") %>% 
  pull(dRT.CT_diff) %>% 
  t.test(.) # p=0.01511

change_diff_clean %>% 
  filter(Treatment == "saline") %>% 
  pull(dAL_diff) %>% 
  t.test(.) # p=0.0006092
}
## ANOVA & Tukey's HSD
{
change_diff_clean %>% 
  aov(dAC_diff ~ Treatment, data = .) %>% 
  summary() # p=0.367

change_diff_clean %>% 
  aov(dL_diff ~ Treatment, data = .) %>% 
  summary() # p=0.38

change_diff_clean %>% 
  aov(dAC.L_diff ~ Treatment, data = .) %>% 
  #summary() # p=0.00318 -> run tukey's HSD
  TukeyHSD() # dopa vs sal p=0.2243311  
             # apo vs sal p=0.0021659 
             # apo vs dopa p=0.2854820

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
  TukeyHSD() # dopa vs sal p=0.1781607  
             # apo vs sal p=0.0167339 
             # apo vs dopa p=0.7872671
}

### Graphical Representation ###
## Bar chart of all variables
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
                               "#21918c",
                               "#440154"),
                    labels = c("Saline",
                               "Dopamine",
                               "Apomorphine")) +
  theme_classic(base_size = 13) +
  scale_y_continuous(breaks = seq(-150, 100, by=50),
                     limits = c(-175,110))

composite_bar_RATE_final <- composite_bar_RATE +
  geom_text(data = tibble(x = c(3), y = c(20)),
            aes(x = x, y = y, label = "*"),
            inherit.aes = FALSE,
            size = 7) +
  geom_text(data = tibble(x = c(8), y = c(20)),
            aes(x = x, y = y, label = "*"),
            inherit.aes = FALSE,
            size = 7)

## Bar Charts for Variables of Interest
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
                              'Dopamine',
                              'Apomorphine')) +
  scale_fill_manual("Treatment",
                    values = c("#fde725",
                               "#21918c",
                               "#440154")) +
  theme_classic(base_size = 13) +
  scale_y_continuous(breaks = seq(-150, 0, by=50),
                     limits = c(-160,25))

AC.L_RATE_bar_final <- AC.L_RATE_bar +
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
  geom_line(data = tibble(x = c(1,3), y = c(-145,-145)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(2), y = c(-160)),
            aes(x = x, y = y, label = "*"),
            inherit.aes = FALSE,
            size = 7)

#VC
VC_RATE_bar <- change_diff_summary %>% 
  select(Treatment, dVC_diff_mean:dVC_diff_min) %>% 
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
                              'Dopamine',
                              'Apomorphine')) +
  scale_fill_manual("Treatment",
                    values = c("#fde725",
                               "#21918c",
                               "#440154")) +
  theme_classic(base_size = 13) +
  scale_y_continuous(breaks = seq(-100, 0, by=20),
                     limits = c(-100,0))

# CT
CT_RATE_bar <- change_diff_summary %>% 
  select(Treatment, dCT_diff_mean:dCT_diff_min) %>% 
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
                              'Dopamine',
                              'Apomorphine')) +
  scale_fill_manual("Treatment",
                    values = c("#fde725",
                               "#21918c",
                               "#440154")) +
  theme_classic(base_size = 13) +
  scale_y_continuous(breaks = seq(-80, 0, by=20),
                     limits = c(-80,10))

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
                              'Dopamine',
                              'Apomorphine')) +
  scale_fill_manual("Treatment",
                    values = c("#fde725",
                               "#21918c",
                               "#440154")) +
  theme_classic(base_size = 13) +
  scale_y_continuous(breaks = seq(-250, 0, by=50),
                     limits = c(-250,0))

AL_RATE_bar_final <- AL_RATE_bar + 
  geom_line(data = tibble(x = c(1,2), y = c(-187,-187)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(1.5), y = c(-195)),
            aes(x = x, y = y, label = "n.s."),
            inherit.aes = FALSE) +
  geom_line(data = tibble(x = c(2,3), y = c(-197,-197)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(2.5), y = c(-205)),
            aes(x = x, y = y, label = "n.s."),
            inherit.aes = FALSE) +
  geom_line(data = tibble(x = c(1,3), y = c(-222,-222)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(2), y = c(-240)),
            aes(x = x, y = y, label = "*"),
            inherit.aes = FALSE,
            size = 7)

# Creating a figure with all graphs, labels, & one legend
(AC.L_RATE_bar_final + VC_RATE_bar) / (CT_RATE_bar + AL_RATE_bar_final)  +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(family = "Times",
                                size = 14,
                                face = "bold"))
