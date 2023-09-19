# durations (vs lat and breeding status)

library(here)
library(tidyverse)
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

# remove a swan with only partial info
p_dates<-p_dates %>% 
  filter(!id_year%in%"8P-2021-2022")

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

# Or, just load this instead
# third round (post apr/may 2023)
#p_dates<-read_csv(here("output/metrics_3rd_round_manuscript_ready.csv"))

# plot for duration of migration vs breeder/non-breeder/paired

breeding_duration<-p_dates %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired")) %>% 
  mutate(breeding_status=fct_relevel(breeding_status, "breeder", "paired", "non_breeder")) %>% 
  ggplot(., aes(breeding_status, mig_duration,fill=breeding_status))+
  scale_x_discrete(labels=c("Breeder", "Paired", "Non Breeder"))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = 0.1)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  ggtitle(label="Migration Duration vs Breeding Status")+
  theme(plot.title = element_text(hjust=0.5, size=14),
        plot.subtitle = element_text(hjust=0.5, size=12))+
  labs(x="\nBreeding Status", y="Migration Duration (# of days)\n")+
  theme_pubr()+
  theme(legend.position = "none", plot.title = element_text(hjust=0.5))

migration_latitude<-p_dates %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired")) %>% 
  mutate(breeding_status=fct_relevel(breeding_status, "breeder", "paired", "non_breeder")) %>% 
  ggplot(., aes(breeding_lat, mig_duration))+
  geom_point()+
  geom_smooth(method="lm", alpha=0.3)+
  stat_cor(aes(label = paste(after_stat(rr.label))), # adds R^2 value
           r.accuracy = 0.01,
           size = 4)+
  ggtitle(label="Migration Duration vs Breeding Latitude")+
  theme(plot.title = element_text(hjust=0.5, size=14),
        plot.subtitle = element_text(hjust=0.5, size=12))+
  labs(x="\nBreeding Latitude", y="Migration Duration (# of days)\n")+
  theme_pubr()+
  theme(legend.position = "none", plot.title = element_text(hjust=0.5))


# durations<-breeding_duration+migration_latitude+
#   plot_layout(
#     nrow=1
#   )
# 
# ggsave(here("figures/figs_for_manuscript/durations.tiff"),
#             dpi=300, compression="lzw")