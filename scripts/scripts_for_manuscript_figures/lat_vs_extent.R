library(here)
library(tidyverse)
library(lubridate)


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

# Filter out swans that:
# - had incomplete years  (6/7 of 1st yr MN collars)
# I'll consider a swan-year incomplete if it doesn't make it through Jan (unless there is already a settled winter plateau)
incomplete<-c("OC_2nd-2022-2023",
              "0H-2021-2022" ,
              "1P-2020-2021", 
              "3N-2021-2022",
              "4H-2021-2022", 
              "5E-2021-2022",
              "7P-2022-2023",
              "9J (first deployment)-2019-2020",
              "9J (swan originally collared as 5J)-2021-2022")

# - made big summer dispersal movements  n=5? (5L, 6M, 9N, 4P, 8P, 7M)
# 4P did make big movements during the summer, but they were equivalent in distance to it's winter movements, so retain
# Dropped 7M because it made a big movement NE into Pennsylvania (and then back to territory)
dispersers<-c("6M-2021-2022", "6M-2022-2023",
              "5L-2020-2021" ,"5L-2021-2022",
              "7M-2021-2022", 
              "8P-2021-2022", 
              "9N-2021-2022", "9N-2022-2023")

filt_vec<-c(incomplete, dispersers)


p_dates<-p_dates %>% 
  filter(!id_year%in%filt_vec)

dist_plot<-p_dates %>% 
  filter(fall_yr!=2019) %>% 
  ggplot(aes(breeding_lat,mig_extent))+
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

# dist_plot
# 
# ggsave(here("figures/figs_for_manuscript/distances_latitude.tiff"),
# dpi=300, compression="lzw")