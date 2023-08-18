# Updated Latent State Model
# August 2023


# Pull out max displacement by hand for entire swan dataset

# package names
packages<-c("tidyverse", "here", "lubridate", "R2jags", "rjags", 
            "mcmcplots", "loo", "MCMCvis", "viridis")
options(mc.cores=7)

# install any packages not previously installed
installed_packages<-packages %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])
}

# load packages
invisible(lapply(packages, library, character.only = TRUE))

# most updated version of dataset
df<-read_csv(here("data/full_dataset_4_28_2023/daily_nsd.csv"))
# 126 separate collar deployments

# split years up each summer
df <- df %>%
  group_by(id) %>%
  mutate(swan_yr = ifelse(yday < 182, paste(id, year - 1, year, sep = "-"),
                          paste(id, year, year + 1, sep = "-")
  )) # 182 is julian day for july 1

df<-df %>% 
  group_by(swan_yr) %>% 
  mutate(num_days=n())

# filter out swans that had years with less than 90 days
df<-df %>% 
  filter(num_days>90)

# filtered out 11 swan-years with less than 30 days; 241 swan-year combinations

# Convert NSD to simple displacement in kilometers
df$sqrt<-sqrt(df$nsd_daily_mean)
df$rescale<-df$sqrt/1000

#########################################################################

# Pull out the maximum displacement value for each swan-year
df<-df %>% 
  group_by(swan_yr) %>% 
  mutate(max_nsd=max(rescale)) %>% 
  select(id, capture_state, state_ID, sex, swan_yr, max_nsd, num_days) %>% 
  distinct()


# arkansas data separate from the overall mcp fit
# ark<-read_csv(here("output/arkansas_migration_metrics_salvaged.csv"))
# The numbers that I pulled off from AR swans actually agrees pretty closely with 
# what mcp got from the batch run, so I'll leave it for now (8/18/2023).

# others to exclude
exclude<-c(
  "1P-2020-2021", # taken into custody, year all screwy
  "9J (swan originally collared as 5J)-2021-2022", # collar died before winter
  # "5L-2020-2021" ,"5L-2021-2022", Maybe remove 5L, the cygnet that went up to Hudson Bay??
  "7M-2021-2022", # Dropped 7M because it made a big movement NE into Pennsylvania (and then back to territory)
  "8P-2021-2022", # big summer dispersal N and then collar died
  "9N-2021-2022", # big summer dispersal
  "9N-2022-2023"  # big summer dispersal
  )

# add in breeding lat and other info
ids<-read_csv(here("ids.csv"))
df<-df %>% 
  left_join(., ids) %>% 
  select(-comments) %>% 
  rename(breeding_status="breeding_status(if cygnets=breeder; if mate=paired;else non-breeder or cygnet)",
         mass='mass (kg)', skull='skull (mm)')

df<-df %>% 
  filter(!swan_yr%in%exclude)
####################################################
# Now fit latent mixture model

# Model 1: Just latitude and max_nsd 
migs<-df$max_nsd
lats<-df$breeding_lat-min(df$breeding_lat)
df$bl2<-df$breeding_lat-min(df$breeding_lat)
n_obs<-nrow(df)
