# Tyler Harm's request: How many swans didn't return within 10km of previous year's territory



library(here)
library(tidyverse)
df<-read_csv(here("output/metrics_3rd_round_manuscript_ready.csv"))

df %>% 
  drop_na(fall_mig_onset) %>% 
  count(spring_arrival_comment)

# Of the 91 swan-years that had fall departure, 48/91, 53% swan-years didn't return within 10km the next summer 

# how many swans didn't have a spring return following an autumn return for at least 1 year
df %>% 
  drop_na(fall_mig_onset) %>% 
  filter(spring_arrival_comment=='first and last intercepts not within 10 km') %>%
  summarize(count_ID=n_distinct(swan_ID))

         
df %>% 
  drop_na(fall_mig_onset) %>% 
  summarize(count_ID=n_distinct(swan_ID))

# 43 out of 64 swans: 67% of swans

# Of long-distance fall migrants that didn't have spring return, what is the breakdown of breeding classes
df %>% 
  drop_na(fall_mig_onset) %>% 
  group_by(breeding_status) %>% 
  filter(spring_arrival_comment=='first and last intercepts not within 10 km') %>%
  summarize(count_ID=n_distinct(swan_ID),
            count_swan_years=n())


# Slightly different stat, what is the overall spring return rate for each breeding class
df %>% 
  drop_na(fall_mig_onset) %>% 
  group_by(breeding_status) %>% 
  summarize(count_ID=n_distinct(swan_ID),
            count_swan_years=n(),
            num_spring_returner_years=sum(!is.na(spring_arrival)),
            spring_return_rate_years=sum(!is.na(spring_arrival))/count_swan_years)


