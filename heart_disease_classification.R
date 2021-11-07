#Ian Skinner
#2021-11-07
#Objective: take heart disease dataset and build classification models

#clean environ
rm(list = ls())

#call pkgs
pacman::p_load(tidyverse, janitor, scales, rio)

#import dataset
data = rio::import("C:/Users/gamin/Downloads/archive/heart.csv") %>% 
  clean_names()


summary(data)

data %>% 
  count(sex) %>% 
  mutate(pct = percent(n / sum(n))) %>% 
  arrange(-n)