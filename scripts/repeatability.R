# Variation and repeatability

library(here)
library(tidyverse)
library(rptR)
library(lme4)
library(lattice)
library(merTools)


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

# drop cygnets
param_df<-param_df %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired"))

# # Add specific years for the fall and spring events (fall_onset and spring_arrival) to track yearly variation
# param_df<-param_df%>% 
#   mutate(fall_yr=map_chr(strsplit(.$id_year, "-"), ~.x[2]),
#          spring_yr=map_chr(strsplit(.$id_year, "-"), ~.x[3]))
# 
# # add column for entire year cycle
# param_df<-param_df %>% 
#   mutate(entire_yr=paste(map_chr(strsplit(.$id_year, "-"), ~.x[2]),
#                          map_chr(strsplit(.$id_year, "-"), ~.x[3]), sep="-"))

# use lme4
fall_lmer<-lmer(fall_mig_onset~sex+breeding_status+
                  (1|swan_ID),
                data=param_df)
randoms<-REsim(fall_lmer, n.sims=500)
plotREsim(randoms)
f1<-broom.mixed::tidy(fall_lmer)
f1


spring_lmer<-lmer(spring_arrival~sex+breeding_status+
                  (1|swan_ID),
                data=param_df)
random_spr<-REsim(spring_lmer, n.sims=500)
plotREsim(random_spr)
s1<-broom.mixed::tidy(spring_lmer)


duration_lmer<-lmer(mig_duration~sex+breeding_status+
                      (1|swan_ID),
                    data=param_df)
rand_dur<-REsim(duration_lmer, n.sims=500)
plotREsim(rand_dur)
d1<-broom.mixed::tidy(duration_lmer)

# 3 models to run


# fall duration
fm1<-rpt(fall_mig_onset~sex+breeding_status+
           (1|swan_ID),
         data=param_df,
         grname="swan_ID",
         datatype="Gaussian",
         nboot=1000,
         ratio=T,
         ncores=7)
summary(fm1)
summary(fm1$mod)


sm1<-rpt(spring_arrival~sex+breeding_status+
           (1|swan_ID),
         data=param_df,
         grname="swan_ID",
         datatype="Gaussian",
         nboot=1000,
         ratio=T,
         ncores=7)
summary(sm1)
summary(sm1$mod)

dm1<-rpt(mig_duration~sex+breeding_status+
           (1|swan_ID),
         data=param_df,
         grname="swan_ID",
         datatype="Gaussian",
         nboot=1000,
         ratio=T,
         ncores=7)
summary(dm1)
summary(dm1$mod)
