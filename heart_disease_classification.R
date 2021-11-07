#Ian Skinner
#2021-11-07
#Objective: take heart disease dataset and build classification models

#setup========================================================================================================================================================================
rm(list = ls())
pacman::p_load(tidyverse, janitor, scales, rio, na.tools)

#import dataset=================================================================================================================================================================================
data = rio::import("C:/Users/gamin/Downloads/archive/heart.csv") %>% 
  clean_names()

data = data %>% 
  mutate(across(everything(), ~ifelse(.x == "", NA, .x)),
         id = row_number())

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

pivoter = function(varname) {
  data %>% 
    count({{varname}}) %>% 
    mutate(pct = percent(n / sum(n))) %>% 
    arrange(-n)
}

pivoter(sex)
pivoter(chest_pain_type)
pivoter(resting_ecg)
pivoter(exercise_angina)
pivoter(st_slope)