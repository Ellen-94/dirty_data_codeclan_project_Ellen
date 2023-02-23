# install/load packages 

# install.packages("readr")

library(readr)
library(tidyverse)
library(janitor)
library(tidyr)
library(dplyr)
library(stringr)

# read in the data 


decathlon_data <- read_rds("raw_data/decathlon.rds")

# clean data -------------------------------------------------------------------

decathlon_clean <- 
  
# make row-names a column in the data 
  
  decathlon_data %>% 
  tibble::rownames_to_column(var = "name") %>% 
  
  
# change events from wide to long format 
  
  pivot_longer(cols = c("100m", 
                        "Long.jump",
                        "Shot.put", 
                        "High.jump",
                        "400m", 
                        "110m.hurdle", 
                        "Discus",     
                        "Pole.vault", 
                        "Javeline",  
                        "1500m"), 
               names_to = "event", 
               values_to = "performance") %>% 
  
  
 # change column header names 
  
  clean_names() %>% 
  
# change order of columns so that event and performance comes after name 
  
  select(name, event, performance, rank, points, competition) %>% 
  
# change the names to name format in the name column 
  
  mutate(name = str_to_lower(name), name = str_to_title(name)) %>% 
  
# replace the . in the event column with spaces 
  
  mutate(event = str_replace(event,"\\."," "))

# save the cleaned data to a csv file

write_csv(decathlon_clean, "clean_data/decathlon_clean_data.csv")


