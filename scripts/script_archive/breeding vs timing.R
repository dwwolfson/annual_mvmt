# breeding vs migration timing

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

# plot for duration of migration vs breeder/non-breeder/paired
fall_onset<-p_dates %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired")) %>% 
  ggplot(., aes(breeding_status, fall_mig_onset,fill=breeding_status))+
  geom_boxplot()+
  geom_point()+
  scale_y_date(date_labels = "%b %d")+
  geom_jitter(width = 0.15)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  ggtitle(label="Fall Migration Onset")+
  theme(plot.title = element_text(hjust=0.5, size=14),
        plot.subtitle = element_text(hjust=0.5, size=12))+
  labs(x="\nBreeding Status", y="Date of Fall Migration Onset\n")+
  theme_pubr()+
  theme(legend.position = "none", plot.title = element_text(hjust=0.5))

spring_arrival<-p_dates %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired")) %>% 
  ggplot(., aes(breeding_status, spring_arrival,fill=breeding_status))+
  geom_boxplot()+
  geom_point()+
  scale_y_date(date_labels = "%b %d")+
  geom_jitter(width = 0.15)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  ggtitle(label="Spring Arrival")+
  theme(plot.title = element_text(hjust=0.5, size=14),
        plot.subtitle = element_text(hjust=0.5, size=12))+
  labs(x="\nBreeding Status", y="Date of Spring Arrival\n")+
  theme_pubr()+
  theme(legend.position = "none", plot.title = element_text(hjust=0.5))

fall_onset+spring_arrival

###########################################################################################
# Only the two figures above to be used in the manuscript
# Below, I went through the aggravation of also plotting the furthest segment arrival and departure
# I initially thought this would be a good way to visualize the duration of fall and spring migration,
# since it would give the timing in regards to the furthest segment, which for many swans is the same as the wintering grounds

# I think the fall-mig-onset -> furthest segment arrival might make sense to use because swans seem to stage more in the fall,
# and therefore there are additional segments and it makes sense to estimate duration.
# However, for the spring migration, swans don't stage as much (or at all), and so since mcp isn't fitting additional segments,
# it can't estimate the time spent on spring migration.

# fix 'furthest seg arrival' so that they're not all on the same year
p_dates<-p_dates %>% 
  mutate(furthest_seg_arrival=if_else(furthest_seg_arrival<as.Date("2020-10-01"),
                                      'year<-'(furthest_seg_arrival, 2021),
                                      furthest_seg_arrival))


#furthest seg arrival
# limit to only swans that had fall migration onset
seg_arr<-p_dates %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired")) %>% 
  filter(!is.na(fall_mig_onset)) %>% 
  ggplot(., aes(breeding_status, furthest_seg_arrival,fill=breeding_status))+
  geom_boxplot()+
  geom_point()+
  scale_y_date(date_labels = "%b %d")+
  geom_jitter(width = 0.15)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  ggtitle(label="Furthest Segment Arrival")+
  theme(plot.title = element_text(hjust=0.5, size=14),
        plot.subtitle = element_text(hjust=0.5, size=12))+
  labs(x="\nBreeding Status", y="Date of Spring Arrival\n")+
  theme_pubr()+
  theme(legend.position = "none", plot.title = element_text(hjust=0.5))




# param_df<-param_df %>% 
#   mutate(seg_dep_doy=ifelse(furthest_seg_departure<186, 
#                              furthest_seg_departure+181, 
#                              furthest_seg_departure-185)) %>% 
#         mutate(seg_dep_date=as.Date(seg_dep_doy, origin="2019-12-31"))

# fix 'furthest seg departure' so that they're not all on the same year
p_dates<-p_dates %>% 
  mutate(furthest_seg_departure=if_else(furthest_seg_departure>as.Date("2020-12-01"),
                                        'year<-'(furthest_seg_departure, 2019),
                                        furthest_seg_departure))
#furthest seg departure
# limit to only swans that had spring arrival
seg_dep<-p_dates %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired")) %>% 
  filter(!is.na(spring_arrival)) %>% 
  ggplot(., aes(breeding_status,furthest_seg_departure,fill=breeding_status))+
  geom_boxplot()+
  geom_point()+
  scale_y_date(date_labels = "%b %d")+
  geom_jitter(width = 0.15)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  ggtitle(label="Furthest Segment Departure")+
  theme(plot.title = element_text(hjust=0.5, size=14),
        plot.subtitle = element_text(hjust=0.5, size=12))+
  labs(x="\nBreeding Status", y="Date of Spring Arrival\n")+
  theme_pubr()+
  theme(legend.position = "none", plot.title = element_text(hjust=0.5))



# I think we probably shouldn't use these plots (seg_dep and seg_arr) because they don't explicitly track the timing of individuals, and even if they did, lots/most(?) swans don't any staging stopovers
# during the spring migration, so the date of the furthest seg dept and the spring arrival are exactly the same, because there isn't a separate segment for a spring stopover event.

# fall_onset+seg_arr
# seg_dep+spring_arrival


