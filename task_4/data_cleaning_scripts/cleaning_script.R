# install/load packages 
library(tidyverse)
library(readr)
# install.packages("readxl")
library(readxl)
library(tidyr)
library(lubridate)
library(stringr)
library(janitor)
library(assertr)



# read in the data 
candy_data_2015 <- read_excel("raw_data/boing-boing-candy-2015.xlsx")
candy_data_2016 <- read_excel("raw_data/boing-boing-candy-2016.xlsx")
candy_data_2017 <- read_excel("raw_data/boing-boing-candy-2017.xlsx")



# change each dataset to long format -------------------------------------------

candy_15_long <-
  candy_data_2015 %>% 
  pivot_longer(cols = starts_with("["), names_to = "item", values_to = "feeling")

candy_16_long <-
  candy_data_2016 %>% 
  pivot_longer(cols = starts_with("["), names_to = "item", values_to = "feeling")


candy_17_long <- 
  candy_data_2017 %>% 
  pivot_longer(cols = starts_with("Q6"), names_to = "item", values_to = "feeling")


# select relevant columns in appropriate order for each dataset ----------------

candy_15_cut <- 
  candy_15_long %>% 
  select("Timestamp","How old are you?", 
         "Are you going actually going trick or treating yourself?", "item", "feeling")

candy_16_cut <- 
  candy_16_long %>% 
  select("Timestamp","How old are you?", "Your gender:", "Which country do you live in?", 
         "Are you going actually going trick or treating yourself?", "item", "feeling")

candy_17_cut <- 
  candy_17_long %>% 
  select("Internal ID", "Q3: AGE", "Q2: GENDER", "Q1: GOING OUT?", "Q4: COUNTRY", "item", "feeling")


# clean columns and rows in the candy_15_cut data ------------------------------

candy_15_clean <- 
  
  # extract the year from the Timestamp column 
  
  candy_15_cut %>% 
  mutate(Timestamp = as.character(Timestamp)) %>% 
  mutate(year = if_else(str_starts(Timestamp,"2015"), "2015", Timestamp)) %>% 
  
  # remove unnecessary columns and rearrange 
  
  select("year", "How old are you?", 
         "Are you going actually going trick or treating yourself?", "item", "feeling") %>% 
  
  # change column name How old are you? to Age 
  
  rename(age = "How old are you?") %>% 
  
  # change the age variable to a numeric variable 
  
  mutate(age = as.integer(age)) %>% 
  
  # change the name of the trick or treat variable  
  
  rename(going_out = "Are you going actually going trick or treating yourself?") %>% 
  
  # remove the brackets in the items column 
  
  mutate(item = str_remove_all(item, "\\[|\\]")) %>% 
  
  # change feeling to lower case and capitalised 
  
  mutate(feeling = str_to_lower(feeling), feeling = str_to_title(feeling))




# clean columns and rows in the candy_16_cut data. save country for later. ----

candy_16_clean_except_country <- 
  
  # extract the year from the Timestamp column 
  
  candy_16_cut %>% 
  mutate(Timestamp = as.character(Timestamp)) %>% 
  mutate(year = if_else(str_starts(Timestamp,"2016"), "2016", Timestamp)) %>% 
  
  # remove unnecessary columns and rearrange  
  
  select("year", "How old are you?", "Your gender:", "Which country do you live in?", 
         "Are you going actually going trick or treating yourself?", "item", "feeling") %>% 
  
  # rename columns 
  
  rename(age = "How old are you?", 
         gender = "Your gender:", 
         country = "Which country do you live in?", 
         going_out = "Are you going actually going trick or treating yourself?") %>% 
  
  # change age to a numeric variable
  
  mutate(age = as.integer(age)) %>% 
  
  # remove the brackets in the items column 
  
  mutate(item = str_remove_all(item, "\\[|\\]")) %>% 
  
  
  # change feeling to lower case and capitalised 
  
  mutate(feeling = str_to_lower(feeling), feeling = str_to_title(feeling)) 



# clean the country variable --------------------------------------------------

# Look at distinct elements 


# create dataset with country cleaned 

candy_16_clean <- 
  
  candy_16_clean_except_country %>% 
  mutate(country = recode(country, "usa" = "US", 
                          "USA" = "US", 
                          "United States of America" = 
                            "US", "uSA" = "US", 
                          "united states" = "US", 
                          "canada" = "Canada", 
                          "United States" = "US", 
                          "us" = "US", 
                          "france" = "France", 
                          "USSA" = "US", 
                          "U.S.A." = "US", 
                          "england" = "England", 
                          "uk" = "UK", 
                          "United Kingdon" = "UK", 
                          "USA!" = "US", 
                          "Usa" = "US", 
                          "U.S." = "US",
                          "Us" = "US", 
                          "America" = "US",
                          "Units States" = "US", 
                          "belgium" = "Belgium", 
                          "croatia" = "Croatia", 
                          "United states" = "US", 
                          "England" = "UK", 
                          "USA USA USA" = "US", 
                          "the best one - usa" = "US", 
                          "USA! USA! USA!" = "US", 
                          "españa" = "Spain", 
                          "u.s." = "US", 
                          "The Yoo Ess of Aaayyyyyy" = "US", 
                          "United Kindom" = "UK", 
                          "hungary" = "Hungary", 
                          "united states of america" = "US", 
                          "USA!!!!!!" = "US", 
                          "USA! USA!" = "US", 
                          "sweden" = "Sweden", 
                          "United Sates" = "US", 
                          "Sub-Canadian North America... 'Merica" = "US", 
                          "Trumpistan" = "US", 
                          "U.s." = "US", 
                          "Merica" = "US", 
                          "germany" = "Germany", 
                          "UNited States" = "US", 
                          "kenya" = "Kenya", 
                          "Netherlands" = "The Netherlands", 
                          "United Stetes" = "US", 
                          "america" = "US", 
                          "USA USA USA USA" = "US", 
                          "United States of America" = "US", 
                          "netherlands" = "The Netherlands", 
                          "United State" = "US", 
                          "USA (I think but it's an election year so who can really tell)" = "US", 
                          "United States of America" = "US")) 



# clean the candy_17_cut data. save countries for later. ----------------------

candy_17_clean_except_country <- 
  
  # replace all values in Internal ID with 2017 
  
  candy_17_cut %>% 
  clean_names() %>% 
  mutate(internal_id = as.character(internal_id)) %>% 
  mutate(internal_id = if_else(str_starts(internal_id,"9"), "2017", internal_id)) %>% 
  rename(year = internal_id) %>% 
  
  # rename the rest of the variables 
  
  rename(age = q3_age, gender = q2_gender, going_out = q1_going_out, country = q4_country) %>% 
  
  # change age to integer
  
  mutate(age = as.integer(age)) %>% 
  
  
  # remove Q6 | from item 
  
  mutate(item = str_remove_all(item,"Q6 \\|")) %>% 
  
  # change feeling to lower case and capitalised 
  
  
  mutate(feeling = str_to_lower(feeling), feeling = str_to_title(feeling)) 


# clean the country column ----------------------------------------------------

# create a list alternative US names 

us_list <- c("Usa", 
             "Us", 
             "Unites States", 
             "United Staes", 
             "United States Of America", 
             "Uae", 
             "U.s.a.", 
             "Usausausa", 
             "America", 
             "Unhinged States", 
             "Us Of A", 
             "Unites States", 
             "The United States", 
             "North Carolina",  
             "Unied States", 
             "U S", 
             "U.s.",  
             "The United States Of America", 
             "Unite States", 
             "Usa? Hard To Tell Anymore..", 
             "Usas", 
             "United State", 
             "New York" , 
             "Trumpistan", 
             "United Sates", 
             "California", 
             "I Pretend To Be From Canada, But I Am Really From The United States." , 
             "United Stated" , 
             "Ahem....Amerca", 
             "New Jersey", 
             "United Ststes", 
             "United Statss", 
             "Usa! Usa! Usa!", 
             "Usaa", 
             "Alaska" , 
             "N. America" , 
             "Ussa" , 
             "U S A" , 
             "United Statea", 
             "Usa Usa Usa!!!!", 
             "United States", 
             "Pittsburgh", 
             "'Merica")

# create a list of alternative UK names 

uk_list <- c("Uk", "United Kingdom", "U.k.","Scotland", "Endland", "England")

# create a list of alternative names for Canada

canada_list <- c("Can", "Canae", "Canada`")

# change the alternative names to the country names and create the clean data --

candy_17_clean <- 
  
  candy_17_clean_except_country %>% 
  mutate(country = (str_to_lower(country))) %>% 
  mutate(country = (str_to_title(country))) %>% 
  mutate(country = case_when(country %in% us_list ~ "US", .default = country )) %>% 
  mutate(country = case_when(country %in% uk_list ~ "UK", .default = country)) %>% 
  mutate(country = case_when(country %in% canada_list ~ "Canada", .default = country)) 

# bind all the clean data together ---------------------------------------------

all_candy_data <- 
  bind_rows(candy_15_clean, candy_16_clean, candy_17_clean)

# do additional checks of each column in the all_candy_data 

# use assertive programming to check if the age variable is within appropriate ranges

# check that the items are all candy 

# create a vector of non-candy items or candy items that are too vague to be interesting. 


non_candy <- c("Cash, or other forms of legal tender", 
               "Dental paraphenalia", 
               "Generic Brand Acetaminophen", 
               "Glow sticks", 
               "Broken glow stick", 
               "Creepy Religious comics/Chick Tracts", 
               "Healthy Fruit", 
               "Hugs (actual physical hugs)", 
               "Kale smoothie", 
               "Lapel Pins", 
               "Mary Janes", 
               "Vicodin", 
               "Whole Wheat anything", 
               "Bonkers (the board game)", 
               "Chardonnay", 
               "Person of Interest Season 3 DVD Box Set (not including Disc 4 with hilarious outtakes)", 
               "Real Housewives of Orange County Season 9 Blue-Ray", 
               "Any full-sized candy bar", 
               "Brach products (not including candy corn)", 
               "Vials of pure high fructose corn syrup, for main-lining into your vein", 
               "Gum from baseball cards", 
               "Hard Candy", 
               "Minibags of chips", 
               "Peterson Brand Sidewalk Chalk", 
               "Sea-salt flavored stuff, probably chocolate, since this is the \"it\" flavor of the year",
               "Whatchamacallit Bars",
               "Candy that is clearly just the stuff given out for free at restaurants", 
               "White Bread", 
               "Those odd marshmallow circus peanut things", 
               "Jolly Rancher (bad flavor)", 
               "Spotted Dick", 
               "Pencils", 
               "Anonymous brown globs that come in black and orange wrappers", 
               "Jolly Rancher (bad flavor)", 
               "Jolly Ranchers (good flavor)",
               "Nerds", 
               "Anonymous brown globs that come in black and orange wrappers\t(a.k.a. Mary Janes)", 
               "Abstained from M&M'ing."
               )

# check more countries that need to be changed 

non_countries <- c("A tropical island south of the equator", 
                   "Neverland", 
                   "this one", 
                   "51.0", 
                   "47.0", 
                   "there isn't one for old men", 
                   "one of the best ones", 
                   "Somewhere", 
                   "54.0", 
                   "44.0", 
                   "god's country", 
                   "EUA", 
                   "45.0", 
                   "See above", 
                   "30.0", 
                   "Not the USA or Canada", 
                   "Denial", 
                   "35", 
                   "Europe", 
                   "Earth", 
                   "46", 
                   "Insanity Lately", 
                   "45", 
                   "32", 
                   "A", 
                   "Ud", 
                   "Atlantis", 
                   "Soviet Canuckistan", 
                   "Narnia", 
                   "1", 
                   "Subscribe To Dm4uz3 On Youtube", 		
                   "I Don't Know Anymore", 
                   "Fear And Loathing")

# check other variables 


# do final clean of the entire data --------------------------------------------

candy_data_clean <- 
  
  # make those ages that don't satisfy the condition NAs
  
  all_candy_data %>% 
  mutate(age = (if_else(age < 5 | age > 105, NA_integer_, age))) %>% 
  
  # remove spaces before the name of some items
  
  mutate(item = str_remove(item,"^( )")) %>% 
  
  
  # make those elements that are not candys NA 
  
  mutate(item = (case_when(item %in% non_candy ~ NA, .default = item))) %>% 
  
  # change the rest of the country column 
  
  mutate(country = (if_else(country == "England", "UK", country)) ) %>% 
  mutate(country = (if_else(country == "United  States of America" , "US", country))) %>% 
  mutate(country = (if_else(country == "Netherlands", "The Netherlands", country))) %>% 
  mutate(country = (if_else(country == "Murica", "US", country))) %>% 
  mutate(country = (if_else(country == "United Kingdom", "UK", country))) %>% 
  mutate(country = (if_else(country == "The republic of Cascadia", "Cascadia", country))) %>% 
  mutate(country = (if_else(country == "Murrika", "US", country))) %>% 
  
  
  # make NAs out of non countries 
  
  mutate(country = (case_when(country %in% non_countries ~ NA, .default = country))) %>% 
  
  # rename the item column to candy 
  
  rename(candy = item) %>% 
  
  # change to good column order 
  
  select(year, age, gender, country, going_out, candy, feeling)


# write to csv file  ----------------------------------------------------------

write_csv(candy_data_clean, "clean_data/candy_clean_data.csv")







