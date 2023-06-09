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

##################################################################
# Figures
# max extent of migration
p<-p_dates %>% 
  filter(id_year!="9N-2021-2022" & 
           id_year!="6M-2021-2022" &
           id_year!="9N-2022-2023" &
           id_year!="6M-2022-2023" & 
           id_year!="4H-2021-2022") %>% 
  ggplot(., aes(breeding_lat,mig_extent, color=id_year))+
  geom_point()+
  geom_smooth(method="lm")+
  xlim(40,52)+ # to exclude arkansas winter captures
  facet_wrap(~entire_yr)+
  ggtitle(label="Extent of Displacement from Breeding/Capture Origin throughout the annual cycle",
          subtitle="\n ")+
  theme(plot.title = element_text(hjust=0.5, size=14),
        plot.subtitle = element_text(hjust=0.5, size=12))
  # geom_hline(yintercept=42.5, linetype="dashed", color="red")+
  # geom_hline(yintercept = 48, linetype="dashed", color="red")+
  # geom_vline(xintercept=300, linetype="dashed", color="black")

plotly::ggplotly(p)
