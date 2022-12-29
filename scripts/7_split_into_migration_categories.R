# CATEGORIZE


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

# first round (for conferences)
# param_df<-read_csv(here("output/migration_metrics.csv"))

# second round
param_df<-read_csv(here("output/migration_metrics_2nd.csv"))

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
# try out categories by migration extent
p_dates<-p_dates %>% 
  mutate(mig_cat=ifelse(mig_extent<20, "resident",
                        ifelse(mig_extent>20&mig_extent<100, "local_short_distance",
                               ifelse(mig_extent>100&mig_extent<300, "regional_med_distance",
                                      ifelse(mig_extent>300, "long-distance","flag")))))

# proportion that lacked 'official' fall or spring migration



p<-p_dates %>% 
  ggplot(., aes(breeding_lat, mig_extent, color=fall_mig_onset))+
  geom_point()
plotly::ggplotly(p)

summaries<-p_dates %>% 
  drop_na(mig_cat) %>% 
  group_by(mig_cat) %>% 
  summarize(num_swan_years=n(),
            proportion_group=n()/nrow(p_dates),
            num_fall_departures=sum(!is.na(fall_mig_onset)),
            prop_with_fall_departure=sum(!is.na(fall_mig_onset))/n(),
            average_fall_depart=mean(as.POSIXct(fall_mig_onset), na.rm=T),
            average_first_depart=mean(as.POSIXct(first_departure),na.rm=T),
            average_stops=mean(num_stops, na.rm=T),
            average_stop_duration=mean(c(stop1_duration, stop2_duration, 
                                       stop3_duration, stop4_duration, stop5_duration), na.rm=T),
            num_spr_arrivals=sum(!is.na(spring_arrival)),
            average_spr_arrival=mean(as.POSIXct(spring_arrival), na.rm=T),
            prop_with_spring_arrival=sum(!is.na(spring_arrival))/n()
            )


            