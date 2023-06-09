# Script to try and show population-level migration strategies by year in a figure

# Pull out max displacement by hand for entire swan dataset

# package names
packages<-c("tidyverse", "here", "lubridate")

# install any packages not previously installed
installed_packages<-packages %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])
}

# load packages
invisible(lapply(packages, library, character.only = TRUE))

# This is the entire dataset (n=125) condensed to one record for day with averaged NSD
df<-read_csv(here("data/full_dataset_12_30_2022/full_daily_nsd.csv"))
ids<-unique(df$id)

# split years up each summer
df <- df %>%
  group_by(id) %>%
  mutate(swan_yr = ifelse(yday < 182, paste(id, year - 1, year, sep = "-"),
                          paste(id, year, year + 1, sep = "-")
  )) # 182 is julian day for july 1

# 125 separate collar deployments and 204 swan-year combinations


# Convert NSD to simple displacement in kilometers
df$sqrt<-sqrt(df$nsd_daily_mean)
df$rescale<-df$sqrt/1000

#########################################################################

# Pull out the maximum displacement value for each swan-year
df<-df %>% 
  group_by(swan_yr) %>% 
  mutate(max_nsd=max(rescale)) %>% 
  select(id, capture_state, state_ID, sex, swan_yr, max_nsd) %>% 
  distinct()

# add in breeding lat and other info
ids<-read_csv(here("ids.csv"))
df<-df %>% 
  left_join(., ids) %>% 
  select(-mate_present, -comments) %>% 
  rename(breeding_status="breeding_status(if cygnets=breeder; if mate=paired;else non-breeder or cygnet)",
         mass='mass (kg)', skull='skull (mm)')

# Filter out swans that:
# - had incomplete years  (6/7 of 1st yr MN collars)
# I'll consider a swan-year incomplete if it doesn't make it through Jan (unless there is already a settled winter plateau)
incomplete<-c("0H-2021-2022" ,"1P-2020-2021", "3N-2021-2022",
              "4H-2021-2022", "5E-2021-2022",
              "9J (first deployment)-2019-2020",
              "9J (swan originally collared as 5J)-2021-2022")

# - made big summer dispersal movements  n=5? (5L, 6M, 9N, 4P, 8P, 7M)
# 4P did make big movements during the summer, but they were equivalent in distance to it's winter movements, so retain
# Dropped 7M because it made a big movement NE into Pennsylvania (and then back to territory)
dispersers<-c("6M-2021-2022","5L-2020-2021" ,"5L-2021-2022",
              "7M-2021-2022", "8P-2021-2022", "9N-2021-2022")
# - were caught in Arkansas  n=4   # we could consider the lat from the 
ark<-c("0H_2nd-2021-2022", "7L-2021-2022", "8L-2021-2022",  "9L-2021-2022")
filt_vec<-c(incomplete, dispersers, ark)


df<-df %>% 
  filter(!swan_yr%in%filt_vec)

# get year info
df<-as.data.frame(df)
df<-df %>% 
  mutate(fall_yr=map_chr(strsplit(.$swan_yr, "-"), ~.x[2]),
         spring_yr=map_chr(strsplit(.$swan_yr, "-"), ~.x[3]))
  
full_list<-unique(df$swan_yr)
old<-read_csv(here("data/full_dataset_6_28_2022/full_daily_nsd.csv"))
old<-old %>% 
  group_by(id) %>% 
  mutate(swan_yr = ifelse(yday < 182, paste(id, year - 1, year, sep = "-"),
                          paste(id, year, year + 1, sep = "-")))
old_list<-unique(old$swan_yr)
new_years<-full_list[!full_list%in%old_list]
                          