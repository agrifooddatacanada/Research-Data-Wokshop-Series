############################### README ###################################
#
# This script is part of the workshop: Agri-food Research Data Workshop Series
# given on May 10th, 2023 at the University of Guelph
# GitHub Page: https://github.com/agrifooddatacanada/Research-Data-Workshop-Series
#
# Workshop 5: Introduction to R
# Instructor: Lucas Alcantara, PhD
# Contact: alcantal@uoguelph.ca
#
######################################################################## #

# Install required packages ----
# install.packages("dplyr")
# install.packages("readr")
# install.packages("skimr")
# install.packages("tidyr")
# install.packages("lubridate")

# Load required packages ----
library(dplyr)
library(readr)
library(skimr)
library(tidyr)
library(lubridate)

# Toy data ----
## Load with readr ----
env_data <- read_csv("sample_data/environmental_data.csv") 

## Take a look at the data with skimr ----
env_data
skim(env_data)

# filter() ----
## Example ----
### Filter for lactating barn temperatures only ----
filter(env_data, barn == "lactating")

## Your turn ----
### From inside the barn ----
filter(env_data, location == "inside")
### Above 30C ----
filter(env_data, temp > 30)
### Between 0 and 10C inside the Special Needs barn ----
filter(env_data, temp >= 0, temp <= 10, location == "inside",
       barn == "sp_needs")
### From June 3, 2022, or June 4, 2022 ----
filter(env_data, date == "2022-06-04" | date == "2022-06-05")

# select() ----
## Example ----
### Select temp column ----
select(env_data, date, time, barn, location, temp)
### Select a range of columns (:) ----
select(env_data, date:location)
### Select every column but (-) ----
select(env_data, -rh)
### Select columns that start with … (starts_with()) ----
select(env_data, starts_with("t"))
### Select columns that end with … (ends_with()) ----
select(env_data, ends_with("e"))

## Your turn ----
### Relative humidity from inside the Special Needs barn ----
sp_needs <- filter(env_data, barn == "sp_needs", location == "inside")
sp_needs <- select(sp_needs, date, time, rh)
sp_needs

# arrange() ----
## Example ----
### Order by temp ----
arrange(env_data, temp)
### Order by temp descending ----
arrange(env_data, desc(temp))
### Order by temp and use rh as tie breaker ----
arrange(env_data, temp, rh)

# Pipe operator %>% ----
## Examples ----
# Relative humidity from inside the Special Needs barn, 
# ordered ascending by relative humidity
sp_needs <- filter(env_data, barn == "sp_needs", location == "inside")
sp_needs <- select(sp_needs , date, time, rh)
sp_needs <- arrange(sp_needs, rh)

sp_needs <- arrange(select(filter(env_data, barn == "sp_needs",
                                  location == "inside"), date, time, rh), rh)

filter(env_data, barn == "sp_needs", location == "inside")
env_data %>% filter(barn == "sp_needs", location == "inside")

## Your turn ----
# Use filter(), select(), arrange() and the pipe operator %>% to show only 
# relative humidity from inside the Special Needs barn, 
# ordered ascending by relative humidity
env_data %>% 
  filter(barn == "sp_needs", location == "inside") %>%
  select(date, time, rh) %>%
  arrange(rh)

# summarise() ----
## Example ----
env_data %>% 
  summarize(avg_temp = mean(temp),
            max_temp = max(temp))

env_data %>% filter(is.na(temp))

env_data %>% 
  summarize(avg_temp = mean(temp, na.rm = TRUE),
            max_temp = max(temp, na.rm = TRUE))

## Your turn ----
env_data %>% 
  na_omit() %>%
  filter(barn == "sp_needs",
         location == "inside") %>%
  summarize(min_temp = min(temp),
            avg_temp = mean(temp),
            max_temp = max(temp),
            min_rh = min(rh),
            avg_rh = mean(rh),
            max_rh = max(rh))

# group_by ----
## Example ----
# Without groups
env_data %>% 
  na_omit() %>%
  summarize(min_temp = min(temp),
            avg_temp = mean(temp),
            max_temp = max(temp))
# With groups
env_data %>% 
  na_omit() %>%
  group_by(barn, location) %>% 
  summarize(min_temp = min(temp),
            avg_temp = mean(temp),
            max_temp = max(temp))

# Sample 2 random observations from each group and save dataframe
set.seed(123)
sampled_env_data <- env_data %>% 
  group_by(barn, location) %>% 
  slice_sample(n = 2) %>% 
  select(-c(date,time)) %>%
  ungroup()

# Summarize sampled data
sampled_env_data %>% 
  na_omit() %>%
  summarize(min_temp = min(temp),
            avg_temp = mean(temp),
            max_temp = max(temp))

# Summarize sampled data by barn and location
sampled_env_data %>% 
  na_omit() %>%
  group_by(barn, location) %>% 
  summarize(min_temp = min(temp),
            avg_temp = mean(temp),
            max_temp = max(temp))

## Your turn ----
env_data %>% 
  filter(location == "inside") %>% 
  na.omit() %>% 
  group_by(barn, location) %>% 
  summarize(min_temp = min(temp),
            max_temp = max(temp),
            min_rh = min(rh),
            max_rh = max(rh))

# mutate() ----
## Example ----
env_data %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date),
         barn = dplyr::if_else(barn == "sp_needs", "special_needs", barn))

## Your turn ----
# Using the functions from today, create the following table to answer 
# our ultimate question: Are the temperatures inside the barns 
# milder than outside?
env_data %>% 
  na.omit() %>% 
  mutate(season = if_else(date >= "2021-12-31" & date <= "2022-03-20",
                          true = "winter", 
                          false = if_else(date >= "2022-06-21" & 
                                            date <= "2022-09-23",
                                          true = "summer",
                                          false = "spring/fall"))) %>%
  filter(season %in% c("summer", "winter")) %>% 
  group_by(barn, season, location) %>%
  summarise(avg_temp = mean(temp)) %>%
  arrange(desc(barn), desc(season))
