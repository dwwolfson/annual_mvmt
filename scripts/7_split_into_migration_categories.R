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
# param_df<-read_csv(here("output/migration_metrics_2nd.csv"))

# third round after pulling 3rd year of migration data
# param_df<-read_csv(here("output/migration_metrics_3rd.csv"))

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

# Swans to filter out

# - made big summer dispersal movements  n=5? (5L, 6M, 9N, 4P, 8P, 7M)
# 4P did make big movements during the summer, but they were equivalent in distance to it's winter movements, so retain
# Dropped 7M because it made a big movement NE into Pennsylvania (and then back to territory)
dispersers<-c("5L-2020-2021" ,"5L-2021-2022",
              "6M-2021-2022",
              "7M-2021-2022", "8P-2021-2022", "9N-2021-2022")

# - were caught in Arkansas  n=4   # we could consider the lat from the eventual summer spot?
ark<-c("0H_2nd-2021-2022", 
       "7L-2021-2022", "7L-2022-2023", 
       "8L-2021-2022", "8L-2022-2023", 
       "9L-2021-2022", "9L-2022-2023")



# - had incomplete years  (6/7 of 1st yr MN collars)
# I'll consider a swan-year incomplete if it doesn't make it through Jan 
# (unless there is already a settled winter plateau)
incomplete<-c("0E_2nd-2021-2022", "0H-2021-2022", "0N-2020-2021",
              "1P-2020-2021", "3N-2021-2022",
              "4H-2021-2022", "5E-2021-2022",
              "9J (first deployment)-2019-2020")

filt_vec<-c(incomplete, dispersers, ark)


p_dates<-p_dates %>% 
  filter(!id_year%in%filt_vec)


p<-p_dates %>% 
  ggplot(., aes(breeding_lat, mig_extent, color=id_year))+
  geom_point()
plotly::ggplotly(p)

num_swan_ID<-length(unique(p_dates$swan_ID))

summaries<-p_dates %>% 
  drop_na(mig_cat) %>% 
  group_by(mig_cat) %>% 
  summarize(num_swan_years=n(),
            num_swans=length(unique(swan_ID)),
            proportion_group_swan_years=n()/nrow(p_dates),
            proportion_group_swans=length(unique(swan_ID))/num_swan_ID,
            num_fall_departures=sum(!is.na(fall_mig_onset)),
            prop_with_fall_departure=sum(!is.na(fall_mig_onset))/n(),
            average_fall_depart=mean(as.POSIXct(fall_mig_onset), na.rm=T),
            average_first_depart=mean(as.POSIXct(first_departure),na.rm=T),
            average_stops=mean(num_stops, na.rm=T),
            average_stop_duration=mean(c(stop1_duration, stop2_duration, 
                                       stop3_duration, stop4_duration, stop5_duration), na.rm=T),
            num_spr_arrivals=sum(!is.na(spring_arrival)),
            average_spr_arrival=mean(as.POSIXct(spring_arrival), na.rm=T),
            prop_with_spring_arrival=sum(!is.na(spring_arrival))/n())

#retain info by year
summaries1<-p_dates %>% 
  drop_na(mig_cat) %>% 
  group_by(mig_cat, fall_yr) %>% 
  summarize(num_swans=length(unique(swan_ID)),
            num_fall_departures=sum(!is.na(fall_mig_onset)),
            prop_with_fall_departure=sum(!is.na(fall_mig_onset))/n(),
            average_fall_depart=mean(as.POSIXct(fall_mig_onset), na.rm=T),
            average_first_depart=mean(as.POSIXct(first_departure),na.rm=T),
            average_stops=mean(num_stops, na.rm=T),
            average_stop_duration=mean(c(stop1_duration, stop2_duration, 
                                         stop3_duration, stop4_duration, stop5_duration), na.rm=T),
            num_spr_arrivals=sum(!is.na(spring_arrival)),
            average_spr_arrival=mean(as.POSIXct(spring_arrival), na.rm=T),
            prop_with_spring_arrival=sum(!is.na(spring_arrival))/n())


# except 2019 doesn't have good enough data
summaries1<-summaries1 %>% 
  filter(!fall_yr==2019) 


# calculate proportion properly
num_2020<-length(unique(p_dates[p_dates$fall_yr==2020,"swan_ID"]))

# for 2022 annual report report
write_csv(summaries, here("output/migration_category_summaries.csv"))
write_csv(summaries1, here("output/migration_category_summaries1.csv"))

# check on switching between years
switching<-p_dates %>% 
  drop_na(mig_cat) %>% 
  group_by(swan_ID) %>% 
  summarize(num_mig_cats=length(unique(mig_cat)))

# switchers are:
switchers<-c("1A", "2M", "4J", "5C", "6N", "7C", "7E", "9A")
swt<-p_dates %>% 
  filter(swan_ID%in%switchers) %>% 
  select(swan_ID, id_year, mig_extent, mig_cat, capture_state)

# take out the third year of 4J because there wasn't a shift in that year
swt<-swt %>% 
  filter(!id_year%in%"4J-2021-2022")

# quantify direction of switching and magnitude of difference in extent
switch_sum<-swt %>% 
  group_by(swan_ID) %>% 
  summarize(type_switch=paste(mig_cat, collapse="-"),
            distance_diff=lead(mig_extent)-mig_extent,
            capture_state=capture_state) %>% 
  drop_na()

