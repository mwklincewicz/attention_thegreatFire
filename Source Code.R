library(lme4)
library(lmerTest)
library(readr)
library(dplyr)
library(lmerTest)
library(caret)
library(sjPlot)
library(brms)
library(texreg)


#Loading Final Merged Dataset
df <- read_csv("Csv Files/F_Merged.csv")

#Getting the Mean And Standard Deviations of all Ages
df %>% distinct(testerKey, Age) %>%
  summarise(
    mean_age = mean(Age, na.rm = TRUE),
    sd_age = sd(Age, na.rm = TRUE),
    n=n()
  )

#Setting the Variables for the Statistical Models
#Standardising the MAC scores and the Eye-Tracking variables
df <- df %>%
  mutate(
    choice = ifelse(playerChoice == 1, "active","passive"),
    swapped = as.factor(swapped),
    cond_name = factor(condition, labels = c("CM1","CM2","IM1","IM2")),
    Gender = as.factor(Gender),
    
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

#Running the Statistical Models
model1 <- lmer(perc_diff ~ swapped * choice + Age + Gender + z_RT + (1| testerKey), data=df)

model2 <- lmer(perc_diff ~ swapped * choice + z_LbS + z_RbS + z_Lb + z_Rb +
                 z_MM + Age + Gender + (1 | testerKey), data =df)

model3a <- lmer(z_LbS ~ swapped *choice + Age + Gender +
                  (1|testerKey), data=df)
  
model3b <- lmer(z_RbS ~ swapped *choice + Age + Gender +
                  (1|testerKey), data=df)

model4 <- lmer(perc_diff ~ swapped * choice + z_LbS + z_RbS+
                 z_property + z_recip + z_fairness + z_family + z_bravery +
                 z_respect + z_loyalty + z_RT + Age + Gender + (1 | testerKey), data =df)


#Generating Summary of Models including regression tables
summary(model1, correlation=TRUE)
summary(model2, correlation=TRUE)
summary(model3a, correlation=TRUE)
summary(model3b, correlation=TRUE)
summary(model4, correlation=TRUE)

