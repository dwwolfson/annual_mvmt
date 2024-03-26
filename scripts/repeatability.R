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
fall_rep<-rpt(fall_mig_onset~sex+breeding_status+breeding_lat+
                (1|swan_ID),
              data=param_df,
              grname="swan_ID",
              datatype="Gaussian",
              nboot=1000,
              ratio=T,
              ncores=7)
summary(fall_rep)


# Spring arrival
spring_lmer<-lmer(spring_arrival~sex+breeding_status+breeding_lat+
                    (1|swan_ID), 
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
duration_lmer<-lmer(mig_duration~sex+breeding_status+
                      (1|swan_ID),
                    data=param_df)

dm1<-rpt(mig_duration~sex+breeding_status+breeding_lat+
           (1|swan_ID),
         data=param_df,
         grname="swan_ID",
         datatype="Gaussian",
         nboot=1000,
         ratio=T,
         ncores=7)

##############################################################################################
# This is the chunk that I had in the Rmd document before we removed the repeatability section

# Table from all the previous estimations
rep_tab<-read_csv(here("output/repeatability/repeatability_lmm_or_lm_reduced_CI_version.csv"))

rep_tab %>% 
  flextable %>% 
  compose(i=1, j=7, value=as_paragraph("NA")) %>% # the en-dash changes column from numeric to character and then                                                    # NA doesn't display properly, so have to hard-code it 
  set_header_labels(values=list(
    "migration_metric"="Migration Metric",
    "swan-years"="Number of Migratory Tracks",
    "individuals"="Number of Swans",
    "within-individual_variance"=
      "Within-Individual Variance",
    "among-individual_variance"=
      "Among-Individual Variance",
    "repeatability_estimate"=
      "Repeatability",
    "repeatability_95%_CI"=
      "Repeatability 95% CI"
  )) %>%
  fontsize(size=9, part="all") %>% 
  vline(part="header") %>% 
  align(align ='center', part = 'all') %>% 
  bg(part="header", bg="#dbdbdb") %>% 
  bold(part="header") %>% 
  colformat_num(na_str="NA") %>% 
  set_caption("Estimates of variance parameters for derived migration metrics. Number of migratory tracks represents the number of 'swan-years' that satisfied the rulesets for each metric. Repeatability is the proportion of the total variance accounted for by differences among groups; estimated as a point value between 0 and 1. Estimates for autumn departure came from a linear model and estimates for spring arrival and migration duration came from a linear mixed model.") %>% 
  width(., width = dim(.)$widths * 6.5 / (flextable::flextable_dim(.)$widths))


