#Ian Skinner
#2021-11-07
#Objective: take heart disease dataset and build classification models to predict heart disease

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
options(scipen = 999)
pacman::p_load(tidyverse, janitor, scales, rio, na.tools, caret, RANN, gbm)
theme_set(theme_classic())
set.seed(255)

#import dataset=================================================================================================================================================================================
heart = rio::import("heart.csv") %>% 
  clean_names()

heart = heart %>% 
  mutate(id = row_number(),
         across(everything(), ~ifelse(.x == "", NA, .x)),
         exercise_angina = case_when(exercise_angina == "Y" ~ 1,
                                     exercise_angina == "N" ~ 0))

#exploration========================================================================================================================================================================
summary(heart)

#na check
nas = anti_join(heart, na.omit(heart), by = "id")
na_rows = nrow(nas)

#summarization function for strings
counter = function(df, varname) {
  df %>% 
    count({{varname}}) %>% 
    mutate(pct = round(n / sum(n), 2)) %>% 
    arrange(-n)
}

counter(heart, sex)
counter(heart, chest_pain_type)
counter(heart, resting_ecg)
counter(heart, exercise_angina)
counter(heart, st_slope)

#target var split
target_split = counter(heart, heart_disease)
target_split

#baseline for prediction (to compare to 'just guessing')
baseline = (target_split %>% filter(heart_disease == 1))$pct

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
hist_fn(cholesterol) #some 0 cholesterol levels. is 0 cholesterol possible? don't think so
hist_fn(log(cholesterol)) #it's log-normal
hist_fn(max_hr) #looks OK, normal
hist_fn(oldpeak) #looks OK, lots have exactly zero

#create ID name
heart$heart_disease_char = ifelse(heart$heart_disease == 1, "Heart Disease", "No Heart Disease")

#summary table for continuous variables
con = heart %>%
  select(heart_disease_char, age, resting_bp, cholesterol, fasting_bs, max_hr, exercise_angina, oldpeak) %>% 
  group_by(heart_disease_char) %>% 
  summarise_all(.funs = "mean")

#categorical variables
cat = heart %>% 
  select(sex, chest_pain_type, resting_ecg, st_slope)

#remove unnecessary variables
heart = heart %>% select(!c(id, heart_disease_char))

#data imputation, rescaling==================================================================================================================================================================
heart = heart %>% 
  mutate(across(c(resting_bp, cholesterol), ~ifelse(.x == 0, NA, .x)))

#use caret preProcess to impute the median value for the one missing value in resting_bp
heart = predict(preProcess(heart %>% select(resting_bp),
                           method = "medianImpute"), 
                heart)

#for cholesterol, use bag imputation since there's quite a lot of missing data
heart = predict(preProcess(heart,
                           method = "bagImpute"),
                heart)

#re-plot cholesterol
hist_fn(cholesterol)

#check out new cholestrol averages by target variable
chol = heart %>% 
  group_by(heart_disease) %>% 
  summarise(avg_cholesterol = mean(cholesterol))

#one hot encode strings
dummy = c("sex", "chest_pain_type", "resting_ecg", "st_slope")

dummy_df = heart %>% select(all_of(dummy))
heart_df = heart %>% select(!all_of(dummy))

heart = data.frame(predict(dummyVars(" ~ .", 
                                     data = dummy_df),
                           newdata = dummy_df)) %>% 
  clean_names() %>% 
  bind_cols(heart_df)

#rescale and center variables
heart = predict(preProcess(heart %>% 
                             select(age, resting_bp, cholesterol, max_hr, oldpeak),
                           method = c("center", "scale")), 
                heart)

#remove unnecessary stuff
rm(dummy_df, heart_df, nas, target_split)

#correlations
correlations = data.frame(cor(heart)) %>% 
  mutate(across(everything(), ~round(.x, 2))) 

correlations$x = row.names(correlations)
row.names(correlations) = NULL

correlations = correlations %>% 
  relocate(x) %>% 
  pivot_longer(!x,
               names_to = "y",
               values_to = "r")

high_corr = correlations %>% 
  mutate(same = ifelse(x == y, 1, 0)) %>% 
  filter(same == 0,
         abs(r) > 0.7)

#remove highly correlative / unnecessary / duplicative columns, recode outcome variable
heart = heart %>% 
  select(!c(sex_m, st_slope_up)) %>% 
  mutate(heart_disease = ifelse(heart_disease == 1, "Y", "N"),
         heart_disease = as.factor(heart_disease))

#machine learning===============================================================================================================================================================

#split data into training and test sets
split = createDataPartition(heart$heart_disease, p = .7, list = F)

train = data.frame(heart[ split, ])
test = data.frame(heart[-split, ])

#confirm class balances compared to baseline
counter(train, heart_disease)
counter(test, heart_disease)

baseline

#stochastic gradient boosting
control_gbm = trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10)

grid_gbm = expand.grid(interaction.depth = c(1, 3, 5), 
                        n.trees = (1:20) * 20, 
                        shrinkage = 0.05,
                        n.minobsinnode = 1)

model_gbm = train(heart_disease ~ ., 
                  data = heart,
                  method = "gbm",
                  tuneGrid = grid_gbm,
                  trControl = control_gbm)

#in sample results
model_gbm
ggplot(model_gbm) +
  labs(title = "Results from in-sample model training",
       subtitle = "Model type: gradient boosting") +
  scale_y_continuous(labels = percent)

model_gbm$bestTune
model_gbm$results

#feature importance
importance = varImp(model_gbm)
importance

ggplot(importance)

#out of sample predictions, results, accuracy
yhat_gbm = predict(model_gbm, test)
cm_gbm = confusionMatrix(test$heart_disease, yhat_gbm, positive = "Y")
cm_gbm