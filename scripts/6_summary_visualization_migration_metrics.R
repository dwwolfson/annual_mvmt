# Summary and visualization of migration metrics


# package names
packages <- c("tidyverse", "here", "lubridate")

# install any packages not previously installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# load packages
invisible(lapply(packages, library, character.only = TRUE))
source(here("scripts/ggplot_custom_function.R"))


param_df<-read_csv(here("output/migration_metrics.csv"))

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

##########################################################################################
# fall departure by breeding lat
p_dates %>% 
  filter(!is.na(breeding_status)) %>% 
  ggplot(aes(fall_mig_onset, breeding_lat))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~fall_yr)+
  ylim(41,52)+
  ggtitle(label="Onset of Long-Distance (>100km) Fall Migration by Breeding/Capture Latitude",
          subtitle="\n It seems like swans are leaving in waves, probably timed with things freezing up \n Although maybe the smoother is deceiving, because collar failure might be non-random")+
  theme(plot.title = element_text(hjust=0.5, size=14),
        plot.subtitle = element_text(hjust=0.5, size=12))

p_dates %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired")) %>% 
  ggplot(., aes(fall_mig_onset, breeding_lat, color=breeding_status))+
  geom_point()+
  geom_smooth(method="lm")+
  ylim(41,52)+
  ggtitle(label="Fall departure by breeding status",
          subtitle="There doesn't seem to be a big effect of breeding status")+
  theme(plot.title = element_text(hjust=0.5, size=14),
        plot.subtitle = element_text(hjust=0.5, size=12))

p_dates %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired")) %>% 
  ggplot(., aes(breeding_status, fall_mig_onset))+
  geom_violin(aes(fill=breeding_status),trim=F)+
  geom_boxplot(width=0.2)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  ggtitle(label="Fall Migration Onset by Breeding Status",
          subtitle="Lots of overlap")+
  theme(plot.title = element_text(hjust=0.5, size=14),
        plot.subtitle = element_text(hjust=0.5, size=12))+
  labs(x="\nBreeding Status", y="\nDate of Fall Migration Onset")
  


# spring arrival by breeding lat
p_dates %>% 
  filter(!is.na(breeding_status)) %>% 
  ggplot(aes(spring_arrival, breeding_lat))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~spring_yr)+
  ylim(41,52)+
  ggtitle(label="Spring Arrival on previous summer territory by Breeding/Capture Latitude",
          subtitle="\n 2021 looks like everyone moves together, 2022 more punctuated in waves")+
  theme(plot.title = element_text(hjust=0.5, size=14),
        plot.subtitle = element_text(hjust=0.5, size=12))

p_dates %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired")) %>% 
  ggplot(., aes(spring_arrival, breeding_lat, color=breeding_status))+
  geom_point()+
  geom_violin()+
  ylim(41,52)+
  ggtitle(label="Spring arrival by breeding status",
          subtitle="There doesn't seem to be a big effect of breeding status")+
  theme(plot.title = element_text(hjust=0.5, size=14),
        plot.subtitle = element_text(hjust=0.5, size=12))

p_dates %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired")) %>% 
  ggplot(., aes(breeding_status, spring_arrival))+
  geom_violin(aes(fill=breeding_status),trim=F)+
  geom_boxplot(width=0.2)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  ggtitle(label="Spring arival by Breeding Status",
          subtitle="\nBreeders arrive before non-breeders.")+
  theme(plot.title = element_text(hjust=0.5, size=14),
        plot.subtitle = element_text(hjust=0.5, size=14))+
  labs(x="\nBreeding Status", y="\nDate of Spring Arrival", fill="Breeding Status")

# max extent of migration
p_dates %>% 
  filter(id_year!="9N-2021-2022" & id_year!="6M-2021-2022") %>% 
  ggplot(., aes(mig_extent, breeding_lat))+
  geom_point()+
  geom_smooth(method="lm")+
  ylim(40,52)+ # to exclude arkansas winter captures
  facet_wrap(~entire_yr)+
  ggtitle(label="Extent of Displacement from Breeding/Capture Origin throughout the annual cycle",
          subtitle="\n ")+
  theme(plot.title = element_text(hjust=0.5, size=14),
        plot.subtitle = element_text(hjust=0.5, size=12))+
  geom_hline(yintercept=42.5, linetype="dashed", color="red")+
  geom_hline(yintercept = 48, linetype="dashed", color="red")+
  geom_vline(xintercept=300, linetype="dashed", color="black")
  
p_dates %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired")) %>% 
  ggplot(., aes(breeding_status, mig_extent))+
  geom_violin(aes(fill=breeding_status),trim=F)+
  geom_boxplot(width=0.2)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  ggtitle(label="Migration Extent by Breeding Status",
          subtitle="Lots of overlap\n I wonder if distributions are influenced by non-random sampling over latitudinal range?")+
  theme(plot.title = element_text(hjust=0.5, size=14),
        plot.subtitle = element_text(hjust=0.5, size=14))+
  labs(x="\nBreeding Status", y="\nExtent of Migration (in km)", fill="Breeding Status")









# Convert dataframe from wide to long format for easier ggplotting
bob<-p_dates %>% 
  pivot_longer(cols=c(fall_mig_onset,  # Events with specific dates
                      first_departure, 
                      furthest_seg_arrival, 
                      furthest_seg_departure,
                      spring_arrival),
               names_to = 'Temporal_Event',
               values_to = "Date",
               values_drop_na = T) %>% 
  pivot_longer(cols=c(mig_extent, 
                      mig_duration),
               names_to = "Distance_Measures",
               values_to = "Distance_Values",
               values_drop_na = T ) %>% 
  pivot_longer(cols=c(spring_arrival_comment,
                      mig_duration_comment,
                      fall_mig_onset_comment,
                      num_stops_comment,
                      first_departure_comment,
                      mig_extent_comment),
               names_to = "Comment_Type",
               values_to = "Comment",
               values_drop_na = T) %>% 
  pivot_longer(cols=c(stop1_duration:stop5_duration),
               names_to = "Stop_durations",
               values_to = "Duration_Period",
               values_drop_na = T) %>% 
  pivot_longer(cols=c(fall_yr, spring_yr),
               names_to = "Fall/Spring_Year",
               values_to = "year")







bob %>% 
  filter(Temporal_Event%in%c("spring_arrival","fall_mig_onset")) %>%
  ggplot(., aes(as.factor(Temporal_Event),Date))+
  geom_violin()+
  geom_point()+
  coord_flip()


bob %>% 
  filter(Temporal_Event%in%"fall_mig_onset") %>%
  ggplot(., aes(as.factor(Temporal_Event),Date))+
  geom_point()+
  coord_flip()



p1<-ggplot(p_dates, aes(fall_mig_onset, id_year))+
  geom_point()+
  coord_flip()+
  theme(axis.text.x=element_blank())+
  ggtitle("Fall Migration Onset")+
  labs(y="Individual Swans", x="Dates") #exclude those 2 and re-run
# both of the outliers are not actual fall migrations

plotly::ggplotly(p1)


p2<-ggplot(p_dates, aes(spring_arrival, id_year))+
  geom_point()+
  coord_flip()+
  theme(axis.text.x=element_blank())+
  ggtitle("Spring Arrival")+
  labs(y="Individual Swans", x="Dates")

plotly::ggplotly(p2)

bob %>% 
  filter(Temporal_Event%in%"fall_mig_onset") %>%
  ggplot(., aes(Date, id_year))+
  geom_point()+
  coord_flip()+
  theme(axis.text.x=element_blank())

bob %>% 
  filter(Temporal_Event=="fall_mig_onset") %>%
  ggplot(., aes(Date, id_year))+
  geom_point()+
  coord_flip()+
  theme(axis.text.x=element_blank())



ggplot(p_dates, aes(first_departure, year))+
  geom_point()+
  coord_flip()+
  theme(axis.text.x=element_blank())+
  ggtitle("Fall Migration Onset")+
  labs(y="Individual Swans", x="Dates")


# add breeding latitude information
# add fall year and spring year as columns to be able to pull apart and facet
# latitude versus duration
# latitude versus fall onset
# latitude versus spring arrival
# redo model prior to abstracts with latitude vs extent?












