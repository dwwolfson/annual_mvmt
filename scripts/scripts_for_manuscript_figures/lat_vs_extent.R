library(here)
library(tidyverse)
library(lubridate)


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

# filtered out 21 swan-years with less than 90 days; 231 swan-year combinations

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
  "5L-2020-2021" ,"5L-2021-2022", # the cygnet that went up to Hudson Bay
  "6M-2021-2022", "6M-2022-2023", # Ohio disperser
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
# excluded 10 more swan-year datasets, from 231 to 221


dist_plot<-df %>% 
  ggplot(aes(breeding_lat,max_nsd))+
  geom_point()+
  # geom_hline(yintercept = quantile(p_dates$mig_extent, 0.5, na.rm=T), 
  #            lty=2, color="red")+
  # geom_text(aes(52, quantile(p_dates$mig_extent, 0.5, na.rm=T),
  #               label="50% quantile", vjust=-1), size=5, color="red")+
  # geom_hline(yintercept = quantile(p_dates$mig_extent, 0.25, na.rm=T), 
  #            lty=2, color="blue")+
  #  geom_text(aes(52, quantile(p_dates$mig_extent, 0.25, na.rm=T),
  #               label="25% quantile", vjust=-1), size=5, color="blue")+
  # geom_hline(yintercept = quantile(p_dates$mig_extent, 0.75, na.rm=T), 
  #            lty=2, color="darkgreen")+
  #  geom_text(aes(52, quantile(p_dates$mig_extent, 0.75, na.rm=T),
#               label="75% quantile", vjust=-1), size=5, color="darkgreen")+
labs(x="\nBreeding/Capture Latitude",
     y="Furthest Extent of Migration (in km)\n")+
  theme_pubr()

dist_plot

ggsave(here("figures/figs_for_manuscript/distances_latitude.tiff"),
dpi=300, compression="lzw")