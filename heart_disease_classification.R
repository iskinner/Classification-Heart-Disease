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
pacman::p_load(tidyverse, janitor, scales, rio, na.tools, tictoc, caret, GGally)
theme_set(theme_classic())
set.seed(255)

tic()

#import dataset=================================================================================================================================================================================
heart = rio::import("heart.csv") %>% 
  clean_names()

heart = heart %>% 
  mutate(id = row_number(),
         across(everything(), ~ifelse(.x == "", NA, .x)),
         heart_disease = as.factor(heart_disease),
         exercise_angina = ifelse(exercise_angina == "Y", 1, 0))

#exploration========================================================================================================================================================================
summary(heart)

#na check
nas = anti_join(heart, na.omit(heart), by = "id")
na_rows = nrow(nas)

#summarization function for strings
counter = function(varname) {
  heart %>% 
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

#notes to self
# majority male
# most patients were asymptomatic
# most had a normal electrocardiogram results (heart test result)

#distributions of continuous variables
hist_fn = function(varname) {
  ggplot(data = heart,
         aes(x = {{varname}})) +
    geom_histogram() +
    labs(title = "Variable distribution",
         y = "Frequency") +
    scale_y_continuous(labels = comma)
}

hist_fn(resting_bp) #one observation has a zero resting bp - not possible unless deceased - impute this
hist_fn(cholesterol) #some 0 cholesterol levels. is 0 cholesterol possible? i will assume yes...
hist_fn(log(cholesterol)) #at least it's log normal
hist_fn(max_hr) #looks OK, normal
hist_fn(oldpeak) #looks OK, lots have exactly zero

#create string var for heart disease prevalence
heart$heart_disease_char = ifelse(heart$heart_disease == 1, "Heart Disease", "No Heart Disease")

#plot comparison of variable based on outcome 
ggplot(data = heart,
       aes(x = age)) +
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~ heart_disease_char) +
  labs(title = "Heart disease by age",
       subtitle = "Those who had heart disease tend to be older",
       x = "Age")

heart = heart %>% select(!c(id, heart_disease_char))

#data imputation, rescaling==================================================================================================================================================================
#vars to impute = resting_bp
missing_data_rows = heart %>% filter(resting_bp == 0)

#since zero resting bp and zero cholesterol are impossible, NA them
heart = heart %>% 
  mutate(across(c(resting_bp), ~ifelse(.x == 0, NA, .x)))

#use caret preProcess to impute the median value for these
heart = predict(preProcess(heart %>% select(resting_bp),
                           method = "medianImpute"), 
                heart)

#one hot encode strings
dummy = c("sex", "chest_pain_type", "resting_ecg", "st_slope")

dummy_df = heart %>% select(all_of(dummy))
heart_df = heart %>% select(!all_of(dummy))

heart = data.frame(predict(dummyVars(" ~ .", 
                                     data = dummy_df),
                           newdata = dummy_df)) %>% 
  clean_names() %>% 
  bind_cols(heart_df)

#rescale and center continuous variables
heart = predict(preProcess(heart %>% select(age, resting_bp, cholesterol, max_hr, oldpeak)), heart)

#remove unnecessary df
rm(dummy_df, heart_df, missing_data_rows, nas, target_split)

#correlations

toc()