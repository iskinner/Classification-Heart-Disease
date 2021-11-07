#Ian Skinner
#2021-11-07
#Objective: take heart disease dataset and build classification models

# Attribute Information
  # Age: age of the patient [years]
  # Sex: sex of the patient [M: Male, F: Female]
  # ChestPainType: chest pain type [TA: Typical Angina, ATA: Atypical Angina, NAP: Non-Anginal Pain, ASY: Asymptomatic]
  # RestingBP: resting blood pressure [mm Hg]
  # Cholesterol: serum cholesterol [mm/dl]
  # FastingBS: fasting blood sugar [1: if FastingBS > 120 mg/dl, 0: otherwise]
  # RestingECG: resting electrocardiogram results [Normal: Normal, ST: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV), LVH: showing probable or definite left ventricular hypertrophy by Estes' criteria]
  # MaxHR: maximum heart rate achieved [Numeric value between 60 and 202]
  # ExerciseAngina: exercise-induced angina [Y: Yes, N: No]
  # Oldpeak: oldpeak = ST [Numeric value measured in depression]
  # ST_Slope: the slope of the peak exercise ST segment [Up: upsloping, Flat: flat, Down: downsloping]
  # HeartDisease: output class [1: heart disease, 0: Normal]

#setup========================================================================================================================================================================
rm(list = ls())
pacman::p_load(tidyverse, janitor, scales, rio, na.tools)

#import dataset=================================================================================================================================================================================
data = rio::import("C:/Users/gamin/Downloads/archive/heart.csv") %>% 
  clean_names()

data = data %>% 
  mutate(id = row_number(),
         across(everything(), ~ifelse(.x == "", NA, .x)),
         heart_disease = as.factor(heart_disease))

#exploration========================================================================================================================================================================
summary(data)

#na check
nas = anti_join(data, na.omit(data), by = "id")
na_rows = nrow(nas)

#need to encode these, if we end up using them
chars = names(data %>% 
  select(where(is.character)))

chars_sep = 

#count by gender
data %>% 
  count(sex) %>% 
  mutate(pct = percent(n / sum(n))) %>% 
  arrange(-n)

#summarization function for strings
counter = function(varname) {
  data %>% 
    count({{varname}}) %>% 
    mutate(pct = round(n / sum(n), 2)) %>% 
    arrange(-n)
}

counter(sex)
counter(chest_pain_type)
counter(resting_ecg)
counter(exercise_angina)
counter(st_slope)

#target var split
target_split = counter(heart_disease)
target_split

#baseline for prediction (to compare to 'just guessing')
baseline = (target_split %>% filter(heart_disease == 1))$pct

#NTS
# majority male
# most patients were asymptomatic
# 