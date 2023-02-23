# install/load packages 
library(tidyverse)
library(tidyr)
library(dplyr)
library(janitor)
library(stringr)
library(knitr)

# read in the data 

cake_data <- read_csv("raw_data/cake-ingredients-1961.csv")

cake_codes <- read_csv("raw_data/cake_ingredient_code.csv")


# inspect the data 

# View(cake_data)
# View(cake_codes)

# clean data 

# change to long format 

cake_data_long <- cake_data %>% 
  pivot_longer(!Cake, names_to = "ingredient", values_to = "quantity")

# do remaining cleaning 


cake_clean <- 
  
  # join cake_codes to the data 
  
  left_join(cake_data_long, cake_codes, join_by(ingredient == code)) %>% 
  
  # remove ingredient column 
  select(-ingredient) %>% 
  
  # clean column names 
  clean_names() %>% 
  
  # change ingredient_y to ingredient name 
  
  rename(ingredient_name = ingredient_y) %>% 
  
  # change measure to title 
  
  mutate(measure = str_to_title(measure)) %>% 
  
  # change the order so that quantity comes after measure 
  
  select(cake,ingredient_name,measure,quantity)


# create the csv file 


write_csv(cake_clean, "clean_data/cake_clean_data.csv")



