# Table to show the summary statistics of migration phenology
# Include the mean, median, range, s.d. for all times 
# (also grouped by breeding status and sex)
# additionally, add a coefficient of variation for all statistics among years



library(here)
library(tidyverse)
library(flextable)
library(lubridate)
library(ggpubr)
library(patchwork)

# third round (post apr/may 2023)
param_df<-read_csv(here("output/migration_metrics_3rd.csv"))

# Merge additional info onto dataframe
ids<-read_csv(here("ids.csv"))
param_df<-param_df %>% 
  left_join(., ids,
            by=c("swan_ID" = "id")) %>% 
  select(-mate_present, -'mass (kg)', -'skull (mm)',-'tarsus (mm)', -comments) %>% 
  rename(breeding_status="breeding_status(if cygnets=breeder; if mate=paired;else non-breeder or cygnet)",
         id_year=year)


# Translate back to dates from julian day
p_dates<-param_df %>% 
  mutate(across(c(fall_mig_onset, 
                  first_departure,
                  furthest_seg_arrival, 
                  furthest_seg_departure, 
                  spring_arrival),
                ~ifelse(.<186, .+181, .-185)))


p_dates<-p_dates %>% 
  mutate(across(c(fall_mig_onset, 
                  first_departure,
                  furthest_seg_arrival, 
                  furthest_seg_departure, 
                  spring_arrival),
                ~as.Date(., origin="2019-12-31")))

# Add specific years for the fall and spring events (fall_onset and spring_arrival) to track yearly variation
p_dates<-p_dates %>% 
  mutate(fall_yr=map_chr(strsplit(.$id_year, "-"), ~.x[2]),
         spring_yr=map_chr(strsplit(.$id_year, "-"), ~.x[3]))

# add column for entire year cycle
p_dates<-p_dates %>% 
  mutate(entire_yr=paste(map_chr(strsplit(.$id_year, "-"), ~.x[2]),
                         map_chr(strsplit(.$id_year, "-"), ~.x[3]), sep="-"))


# first, show a table with summary stats for fall departure and spring arrival
# all swans and years, fall departures
fall_table<-p_dates %>% 
  drop_na(fall_mig_onset) %>% 
  summarize(num_swans=length(unique(swan_ID)),
            num_swan_years=n(),
            average_fall_depart=format(mean(as.POSIXct(fall_mig_onset)), "%B %d"),
            # median_fall_depart=format(median(as.POSIXct(fall_mig_onset)), "%B %d"),
            stan_dev_fall_depart=signif(sd(fall_mig_onset), digits =0),
            earliest_fall_depart=format(range(fall_mig_onset)[1], "%B %d"),
            latest_fall_depart=format(range(fall_mig_onset)[2], "%B %d"))

# all swans and years, spring arrivals
spring_table<-p_dates %>% 
  drop_na(spring_arrival) %>% 
  summarize(num_swans=length(unique(swan_ID)),
            num_swan_years=n(),
            average_spring_arrival=format(mean(as.POSIXct(spring_arrival)), "%B %d"),
            # median_spring_arrival=format(median(as.POSIXct(spring_arrival)), "%B %d"),
            stan_dev_spring_arrival=signif(sd(spring_arrival), digits=0),
            earliest_spring_arrival=format(range(spring_arrival)[1], "%B %d"),
            latest_spring_arrival=format(range(spring_arrival)[2], "%B %d"))

fall_table %>% 
  flextable() %>% 
  set_header_labels(values=list(
    "num_swans" = "Number of Swans",
    "num_swan_years" = "Number of Swan-Years",
    "average_fall_depart" = "Average Fall Departure",
    "stan_dev_fall_depart" = "Standard Deviation (in days)",
    "earliest_fall_depart" = "Earliest Departure",
    "latest_fall_depart" = "Latest Departure")) %>% 
  set_table_properties(layout="autofit") %>% 
  vline(part="header") %>% 
  align(align ='center', part = 'all') %>% 
  bg(part="header", bg="#dbdbdb") %>% 
  bold(part="header") %>% 
  set_caption("Migration phenology of fall departures by long-distance migrants")

spring_table %>% 
  flextable() %>% 
  set_header_labels(values=list(
    "num_swans" = "Number of Swans",
    "num_swan_years" = "Number of Swan-Years",
    "average_spring_arrival" = "Average Spring Arrival",
    "stan_dev_spring_arrival" = "Standard Deviation (in days)",
    "earliest_spring_arrival" = "Earliest Arrival",
    "latest_spring_arrival" = "Latest Arrival")) %>% 
  set_table_properties(layout="autofit") %>% 
  vline(part="header") %>% 
  align(align ='center', part = 'all') %>% 
  bg(part="header", bg="#dbdbdb") %>% 
  bold(part="header") %>% 
  set_caption("Migration phenology of spring arrivals by long-distance migrants")
            
# Split out by year
fall_yearly<-p_dates %>% 
  drop_na(fall_mig_onset) %>% 
  # filter(!fall_yr==2019) %>% 
  group_by(fall_yr) %>% 
  summarize(num_swans=length(unique(swan_ID)),
            average_fall_depart=format(mean(as.POSIXct(fall_mig_onset)), "%B %d"),
            # median_fall_depart=format(median(as.POSIXct(fall_mig_onset)), "%B %d"),
            stan_dev_fall_depart=signif(sd(fall_mig_onset), digits =0),
            earliest_fall_depart=format(range(fall_mig_onset)[1], "%B %d"),
            latest_fall_depart=format(range(fall_mig_onset)[2], "%B %d"))

fall_yearly %>% 
  flextable() %>% 
  set_header_labels(values=list(
    "fall_yr" = "Year",
    "num_swans" = "Number of Swans",
    "average_fall_depart" = "Average Fall Departure",
    "stan_dev_fall_depart" = "Standard Deviation (in days)",
    "earliest_fall_depart" = "Earliest Departure",
    "latest_fall_depart" = "Latest Departure")) %>% 
      set_table_properties(layout="autofit") %>% 
  vline(part="header") %>% 
  align(align ='center', part = 'all') %>% 
  bg(part="header", bg="#dbdbdb") %>% 
  bold(part="header") %>% 
  set_caption("Fall departures of long-distance migrants by year")

spring_yearly<-p_dates %>% 
  drop_na(spring_arrival) %>% 
  group_by(spring_yr) %>% 
  summarize(num_swans=length(unique(swan_ID)),
            average_spring_arrival=format(mean(as.POSIXct(spring_arrival)), "%B %d"),
            stan_dev_spring_arrival=signif(sd(spring_arrival), digits=0),
            earliest_spring_arrival=format(range(spring_arrival)[1], "%B %d"),
            latest_spring_arrival=format(range(spring_arrival)[2], "%B %d"))

spring_yearly %>% 
  flextable() %>% 
  set_header_labels(values=list(
    "spring_yr" = "Year",
    "num_swans" = "Number of Swans",
    "average_spring_arrival" = "Average Spring Arrival",
    "stan_dev_spring_arrival" = "Standard Deviation (in days)",
    "earliest_spring_arrival" = "Earliest Arrival",
    "latest_spring_arrival" = "Latest Arrival")) %>% 
  set_table_properties(layout="autofit") %>% 
  vline(part="header") %>% 
  align(align ='center', part = 'all') %>% 
  bg(part="header", bg="#dbdbdb") %>% 
  bold(part="header") %>% 
  set_caption("Spring arrivals of long-distance migrants by year")

# Split by breeding status
# Fall
fall_breeding<-p_dates %>% 
  drop_na(fall_mig_onset) %>% 
  drop_na(breeding_status) %>% 
  group_by(breeding_status) %>% 
  summarize(num_swans=length(unique(swan_ID)),
            num_swan_years=n(),
            average_fall_depart=format(mean(as.POSIXct(fall_mig_onset)), "%B %d"),
            # median_fall_depart=format(median(as.POSIXct(fall_mig_onset)), "%B %d"),
            stan_dev_fall_depart=signif(sd(fall_mig_onset), digits =0),
            earliest_fall_depart=format(range(fall_mig_onset)[1], "%B %d"),
            latest_fall_depart=format(range(fall_mig_onset)[2], "%B %d"))

fall_breeding %>% 
  flextable() %>% 
  set_header_labels(values=list(
    "breeding_status" = "Breeding Status",
    "num_swans" = "Number of Swans",
    "num_swan_years" = "Number of Swan-Years",
    "average_fall_depart" = "Average Fall Departure",
    "stan_dev_fall_depart" = "Standard Deviation (in days)",
    "earliest_fall_depart" = "Earliest Departure",
    "latest_fall_depart" = "Latest Departure")) %>% 
  set_table_properties(layout="autofit") %>% 
  vline(part="header") %>% 
  align(align ='center', part = 'all') %>% 
  bg(part="header", bg="#dbdbdb") %>% 
  bold(part="header") %>% 
  set_caption("Fall departures of long-distance migrants by breeding status")

# Spring
spring_breeding<-p_dates %>% 
  drop_na(spring_arrival) %>% 
  drop_na(breeding_status) %>% 
  group_by(breeding_status) %>% 
  summarize(num_swans=length(unique(swan_ID)),
            num_swan_years=n(),
            average_spring_arrival=format(mean(as.POSIXct(spring_arrival)), "%B %d"),
            stan_dev_spring_arrival=signif(sd(spring_arrival), digits=0),
            earliest_spring_arrival=format(range(spring_arrival)[1], "%B %d"),
            latest_spring_arrival=format(range(spring_arrival)[2], "%B %d"))


spring_breeding %>% 
  flextable() %>% 
  set_header_labels(values=list(
    "spring_yr" = "Year",
    "num_swans" = "Number of Swans",
    "num_swan_years" = "Number of Swan-Years",
    "average_spring_arrival" = "Average Spring Arrival",
    "stan_dev_spring_arrival" = "Standard Deviation (in days)",
    "earliest_spring_arrival" = "Earliest Arrival",
    "latest_spring_arrival" = "Latest Arrival")) %>% 
  set_table_properties(layout="autofit") %>% 
  vline(part="header") %>% 
  align(align ='center', part = 'all') %>% 
  bg(part="header", bg="#dbdbdb") %>% 
  bold(part="header") %>% 
  set_caption("Spring arrivals of long-distance migrants by breeding status")
