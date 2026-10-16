library(lme4)
library(lmerTest)
library(readr)
library(dplyr)
library(tidyr)
library(lmerTest)
library(caret)
library(sjPlot)
library(brms)
library(knitr)

#Loading the Final Merged Dataset
df <- read_csv("Csv Files/F_Merged.csv")

#Setting the Variables required for the Demographics being loaded
#Setting the Side of the Selected choice.
#Standardising the MAC scores and eye-tracking data
df <- df %>%
  mutate(
    choice = ifelse(playerChoice == 1, "active","passive"),
    swapped = as.factor(swapped),
    cond_name = factor(condition, labels = c("CM1","CM2","IM1","IM2")),
    Gender = as.factor(Gender),
    selected_side = ifelse(Interval %in% c(7,8),
                           ifelse(
                             playerChoice == 0,
                             ifelse(
                               choiceOnePosition == "Left",
                               "Left",
                               "Right"
                             ),
                             ifelse(
                               choiceOnePosition == "Left",
                               "Right",
                               "Left"
                             )
                           ),
                           ifelse(
                              playerChoice == 0,
                              ifelse(
                                choiceOnePosition == "Left",
                                "Right",
                                "Left"
                              ),
                              ifelse(
                                choiceOnePosition == "Left",
                                "Left",
                                "Right"
                              ))),
    
    z_Lb = scale(Total_duration_of_fixations_Lb)[,1],
    z_Rb = scale(Total_duration_of_fixations_Rb)[,1],
    z_LbS = scale(Total_duration_of_fixations_Lb_Score)[,1],
    z_RbS = scale(Total_duration_of_fixations_Rb_Score)[,1],
    z_MM = scale(Total_duration_of_fixations_MM)[,1],
    z_text = scale(Total_duration_of_fixations_text)[,1],
    
    z_family = scale(family_Score)[,1],
    z_bravery = scale(bravery_Score)[,1],
    z_property = scale(property_Score)[,1],
    z_loyalty = scale(loyalty_Score)[,1],
    z_respect = scale(respect_Score)[,1],
    z_recip = scale(recip_Score)[,1],
    z_fairness = scale(fairness_Score)[,1],
    
    z_RT = scale(responseTime)[,1]
  )

#Setting the specific fixation duration of the selected side's moral score AOI
df <- df %>% 
  mutate(fixation_selected_score = ifelse(selected_side == "Left", Total_duration_of_fixations_Lb_Score, Total_duration_of_fixations_Rb_Score))

#Generating the Summary Statistics Tables
table_age <- df %>%
  group_by(Interval, cond_name, choice) %>%
  summarise(mean = mean(Age, na.rm=TRUE),
            sd = sd(Age,na.rm = TRUE),
            n = n()) %>%
  arrange(Interval, cond_name, choice) %>%
  print(n = Inf)

table_gender <- df %>%
  group_by(Interval, cond_name, choice, Gender) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Gender, values_from = count, values_fill = 0) %>%
  arrange(Interval, cond_name, choice) %>%
  print(n = Inf)

table_perc_diff <- df %>%
  group_by(Interval, cond_name, choice) %>%
  summarise(mean = mean(perc_diff, na.rm=TRUE),
            sd = sd(perc_diff,na.rm = TRUE),
            n = n()) %>%
  arrange(Interval, cond_name, choice) %>%
  print(n = Inf)

table_scores <- df %>%
  group_by(Interval, cond_name) %>%
  summarise(mean_LbS = mean(Total_duration_of_fixations_Lb_Score, na.rm = TRUE),
            mean_RbS = mean(Total_duration_of_fixations_Rb_Score, na.rm = TRUE),
            sd_LbS = sd(Total_duration_of_fixations_Lb_Score, na.rm = TRUE),
            sd_RbS = sd(Total_duration_of_fixations_Rb_Score, na.rm = TRUE))

table_fixation <- df %>%
  group_by(Interval, cond_name) %>%
  summarise(mean_selected_fixation = mean(fixation_selected_score, na.rm = TRUE),
            sd_selected_fixation = sd(fixation_selected_score, na.rm = TRUE))

table_mac <- df %>%
  group_by(Interval, cond_name, choice) %>%
  summarise(m_family = mean(family_Score,na.rm=TRUE),
            m_bravery = mean(bravery_Score,na.rm=TRUE),
            m_property = mean(property_Score,na.rm=TRUE),
            m_loyalty = mean(loyalty_Score,na.rm=TRUE),
            m_respect = mean(respect_Score,na.rm=TRUE),
            m_recip = mean(recip_Score,na.rm=TRUE),
            m_fairness = mean(fairness_Score,na.rm=TRUE))

