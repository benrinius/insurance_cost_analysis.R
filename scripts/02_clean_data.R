source("scripts/01_load_data.R")
library(tidyverse)

data_clean <- data_raw %>%
  mutate(
    #Standardisera text
    region = str_to_title(str_trim(region)),
    smoker = str_to_title(str_trim(smoker)),
    plan_type = str_to_title(str_trim(plan_type)),
    exercise_level = str_to_title(str_trim(exercise_level)),
    
    #Ersätter saknade värden med kolumnens median
    bmi = if_else(is.na(bmi), median(bmi, na.rm = TRUE), bmi),
    annual_checkups = if_else(is.na(annual_checkups), median(annual_checkups, na.rm = TRUE), annual_checkups),
    
    #Ersätter tomma stängar med "unknown"
    exercise_level = if_else(exercise_level == "" | is.na(exercise_level), "unknown", exercise_level),
    
    #Skapar nya variabler
    #Bmi kategori
    bmi_cat = case_when(
      bmi < 18.5 ~ "Low",
      bmi >= 18.5 & bmi < 25 ~ "Medium",
      bmi >= 25 ~ "High",
      .default = "Unknown"
    ),
    #åldersgrupp
    age_group = case_when(
      age <35 ~ "Young Adult (18-34)",
      age >= 35 & age < 55 ~ "Middle-Aged (35-54)",
      age >= 55 ~ "Senior (55+)",
      .default = "Unknown"
    ),
    
    #Gör om till faktor
    across(c(sex, region, smoker, plan_type, bmi_cat, age_group, chronic_condition, exercise_level), as.factor)
    
  )

glimpse(data_clean)
summary(data_clean)


