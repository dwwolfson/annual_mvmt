# Variation and repeatability

library(here)
library(tidyverse)
library(rptR)
library(lme4)
library(flextable)
library(performance)


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


# Methodology is to fit a LMM with sex and breeding status and if the variance for the random intercept is 0,
# then drop the random effect and fit a LM instead.


# Autumn departure
fall_lmer<-lmer(fall_mig_onset~sex+breeding_status+breeding_lat+
                  (1|swan_ID),
                data=param_df)
# The random effect variance is 0, so switch to LM instead of LMM
fall_lm<-lm(fall_mig_onset~sex+breeding_status+breeding_lat,
            data=param_df)


# Spring arrival
spring_lmer<-lmer(spring_arrival~sex+breeding_status+breeding_lat+
                    (1|swan_ID), 
                     data=param_df)
# The variance of the random effect is not 0, so I'll use rptR to bootstrap for a repeatability estimate.
spring_lm<-lm(spring_arrival~sex+breeding_status+breeding_lat,
              data=param_df)

sm1<-rpt(spring_arrival~sex+breeding_status+breeding_lat+
           (1|swan_ID),
         data=param_df,
         grname="swan_ID",
         datatype="Gaussian",
         nboot=1000,
         ratio=T,
         ncores=7)
summary(sm1)
summary(sm1$mod)

# Migration duration
duration_lmer<-lmer(mig_duration~sex+breeding_status+breeding_lat+
                      (1|swan_ID),
                    data=param_df)
# The random effect variance is 0, so switch to LM instead of LMM
duration_lm<-lm(mig_duration~sex+breeding_status+breeding_lat,
                data=param_df)


# Table from all the previous estimations
rep_tab<-read_csv(here("output/repeatability/repeatability.csv"))

rep_tab %>% 
  flextable %>% 
  set_header_labels(values=list(
    "migration_metric"="Migration Metric",
    "swan-years"="Number of Migratory Tracks",
    "individuals"="Number of Swans",
    "within-individual_variance (average difference in fall timing within an individual)"=
      "Within-individual Variance",
    "within-individual_std (std of individual difference in fall timing)"=
      "Within-individual Standard Deviation",
    "among-individual_variance"=
      "Among-individual Variance",
    "among-individual_std"=
      "Among-individual Standard Deviation",
    "repeatability_estimate"=
      "Repeatability",
    "repeatability_SE"=
      "Repeatability Standard Error",
    "repeatability_lcl"=
      "Repeatability Lower Confidence Limit",
    "repeatability_ucl"=
      "Repeatability Upper Confidence Limit"
  )) %>% 
  set_table_properties(layout="autofit") %>% 
  vline(part="header") %>% 
  align(align ='center', part = 'all') %>% 
  bg(part="header", bg="#dbdbdb") %>% 
  bold(part="header")


#######
# Plot model coefficients

library(sjPlot)


m1<-plot_model(spring_lmer)
m2<-plot_model(fall_lm)
m3<-plot_model(duration_lm)

plot_grid(m1, m2, m3)

