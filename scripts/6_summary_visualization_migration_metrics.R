# Summary and visualization of migration metrics


# package names
packages <- c("tidyverse", "here", "lubridate")

# load packages
invisible(lapply(packages, library, character.only = TRUE))
source(here("scripts/ggplot_custom_function.R"))

# first round (for conferences)
# param_df<-read_csv(here("output/migration_metrics.csv"))

# second round (pre-apr/may 2023) 
# param_df<-read_csv(here("output/migration_metrics_2nd.csv"))

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

p_dates<-p_dates %>% 
  mutate(lat_distance=(breeding_lat-40)*111)
p1<-p_dates %>% 
  filter(id_year!="9N-2021-2022" & id_year!="6M-2021-2022") %>% 
  filter(entire_yr!="2019-2020") %>% 
  ggplot(., aes(breeding_lat, mig_extent))+
  geom_point()+
  geom_smooth(method="lm")+
  xlim(40,52)+ # to exclude arkansas winter captures
  facet_wrap(~entire_yr)+
  ggtitle(label="Extent of Displacement from Breeding/Capture Origin\n throughout the annual cycle",
          subtitle="\n Trends are consistent across years")+
  theme(plot.title = element_text(hjust=0.5, size=14),
        plot.subtitle = element_text(hjust=0.5, size=12))+
  labs(x="Breeding/Capture latitude", y="Migration extent (in km)")



bob<-p_dates %>% 
  filter(id_year!="9N-2021-2022" & id_year!="6M-2021-2022") %>% 
  filter(entire_yr!="2019-2020")
p1+geom_line(data=bob, aes(x=lat_distance, y=mig_extent))

ggplot(bob, aes(breeding_lat, mig_extent))+
  geom_point()+
  geom_smooth(method="lm")+
  xlim(40,52)+ # to exclude arkansas winter captures
  facet_wrap(~entire_yr)+
  ggtitle(label="Extent of Displacement from Breeding/Capture Origin\n throughout the annual cycle",
          subtitle="\n Trends are consistent across years")+
  theme(plot.title = element_text(hjust=0.5, size=14),
        plot.subtitle = element_text(hjust=0.5, size=12))+
  labs(x="Breeding/Capture latitude", y="Migration extent (in km)")
  
  bob<-bob %>% 
  filter(lat_distance>0)
  ggplot(data=bob, aes(x=lat_distance, y=mig_extent))+geom_point()

  
  

p_dates %>% 
  ggplot(., aes(breeding_lat, mig_extent))+
  geom_point()+
  geom_smooth(method="lm")+
  xlim(40, 53)

p<-p_dates %>% 
  ggplot(., aes(breeding_lat, mig_extent, color=swan_ID))+
  geom_point()
plotly::ggplotly(p)

p<-p_dates %>% 
  ggplot(., aes(breeding_lat, mig_extent, color=as.factor(fall_mig_onset)))+
  geom_point()
plotly::ggplotly(p)

# check out 4H (super brief data period), 7L, 8L, 9L,(all L's Arkansas captures), 6M, 9N (Ohio dispersers)



p_dates<-p_dates %>% 
  mutate(capture_state=ifelse(grepl("[7-9]L", swan_ID)|
                                grepl("0H_2nd", swan_ID), "AR",
                              ifelse(grepl("9H_2nd", swan_ID)|
                                       grepl("2H_2nd", swan_ID)|
                                       grepl("0N_2nd", swan_ID)|
                                       grepl("6P", swan_ID), "MN",
                                     ifelse(grepl("A", swan_ID)|
                                              grepl("E", swan_ID)|
                                              grepl("R", swan_ID)|
                                              grepl("T",swan_ID)|
                                              grepl("L", swan_ID),"MN",
                                            ifelse(grepl("M", swan_ID)|
                                                     grepl("N", swan_ID), "OH",
                                                   ifelse(grepl("H", swan_ID), "MB",
                                                          ifelse(grepl("C", swan_ID), "IA",
                                                                 ifelse(grepl("P", swan_ID), "WI",
                                                                        ifelse(grepl("J", swan_ID)|
                                                                                 grepl("K", swan_ID), "MI",
                                                                               "flag")))))))))

p<-p_dates %>% 
  ggplot(., aes(breeding_lat, mig_extent, color=id_year))+
  geom_point()+facet_wrap(~capture_state, scales = "free")
plotly::ggplotly(p)



ggplot(p_dates, aes(first_departure, id_year))+
  geom_point()+
  coord_flip()+
  theme_classic()+
  theme(axis.text.x=element_blank())+
  ggtitle("Fall Migration Onset")+
  labs(y="Individual Swans", x="Dates")

# There is a clear split where there were no first departures during the summer, but there are 
# some Jan-May (that are just local movements)

# Number of stops vs breeding

p_dates %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired")) %>% 
  ggplot(., aes(breeding_status, num_stops, fill=breeding_status))+
  geom_boxplot(width=0.2, outlier.shape = NA)+
  geom_jitter(width=0.05,alpha=0.9)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  ggtitle(label="Number of stops during non-breeding season",
          subtitle="Breeders make less stops than non-breeders and paired swans")+
  theme(plot.title = element_text(hjust=0.5, size=14),
        plot.subtitle = element_text(hjust=0.5, size=12))+
  labs(x="\nBreeding Status", y="\nNumber of Stops")

# number of stops vs latitude

p_dates %>% 
  filter(id_year!="9N-2021-2022" & id_year!="6M-2021-2022") %>% 
  ggplot(., aes(breeding_lat,num_stops))+
  geom_point()+
  geom_smooth(method="lm")+
  xlim(40,52)+ # to exclude arkansas winter captures
  facet_wrap(~entire_yr)+
  ggtitle(label="Number of stops",
          subtitle="\n ")+
  theme(plot.title = element_text(hjust=0.5, size=14),
        plot.subtitle = element_text(hjust=0.5, size=12))+
  labs(x="Breeding/Capture latitude", y="\nNumber of stops during migration")


# first departure vs latitude

p_dates %>% 
  filter(id_year!="9N-2021-2022" & id_year!="6M-2021-2022") %>% 
  ggplot(., aes(breeding_lat,first_departure))+
  geom_point()+
  geom_smooth(method="lm")+
  xlim(40,52)+ # to exclude arkansas winter captures
  facet_wrap(~fall_yr)+
  ggtitle(label="First departure from breeding/capture origin",
          subtitle="\n No long-distance movement threshold criteria")+
  theme(plot.title = element_text(hjust=0.5, size=14),
        plot.subtitle = element_text(hjust=0.5, size=12))+
  labs(x="Breeding/Capture latitude", y="\n Departure from breeding/capture origin")

# first departure vs breeding
p_dates %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired")) %>% 
  ggplot(., aes(breeding_status, first_departure, fill=breeding_status))+
  geom_boxplot(width=0.2, outlier.shape = NA)+
  geom_jitter(width=0.05,alpha=0.9)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  ggtitle(label="Number of stops during non-breeding season",
          subtitle="Breeders make less stops than non-breeders and paired swans")+
  theme(plot.title = element_text(hjust=0.5, size=14),
        plot.subtitle = element_text(hjust=0.5, size=12))+
  labs(x="\nBreeding Status", y="\nNumber of Stops")


# date of leaving max segment vs latitude


p_dates %>% 
  filter(id_year!="9N-2021-2022" & id_year!="6M-2021-2022") %>% 
  ggplot(., aes(breeding_lat,furthest_seg_departure))+
  geom_point()+
  geom_smooth(method="lm")+
  xlim(40,52)+ # to exclude arkansas winter captures
  facet_wrap(~fall_yr)+
  ggtitle(label="Date of departure from max segment")+
  theme(plot.title = element_text(hjust=0.5, size=14),
        plot.subtitle = element_text(hjust=0.5, size=12))+
  labs(x="Breeding/Capture latitude", y="\n Departure from breeding/capture origin")

# date of leaving max segment vs breeding
p_dates %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired")) %>% 
  ggplot(., aes(breeding_status, furthest_seg_departure, fill=breeding_status))+
  geom_boxplot(width=0.2, outlier.shape = NA)+
  geom_jitter(width=0.05,alpha=0.9)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  ggtitle(label="Date of departure from max segment",
          subtitle="No big difference")+
  theme(plot.title = element_text(hjust=0.5, size=14),
        plot.subtitle = element_text(hjust=0.5, size=12))+
  labs(x="\nBreeding Status", y="\nDate of departure from max segment")
