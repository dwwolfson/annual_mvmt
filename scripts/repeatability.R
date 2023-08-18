# Variation and repeatability

library(here)
library(tidyverse)
library(rptR)
library(lme4)
library(flextable)


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

# lmer models for checking random effects
# spring_lmer<-lmer(spring_arrival~sex+breeding_status+
#                   (1|swan_ID),
#                 data=param_df)
# random_spr<-REsim(spring_lmer, n.sims=500)
# plotREsim(random_spr)
# s1<-broom.mixed::tidy(spring_lmer)
# 
# d4_lmer<-lmer(mig_duration~(1|swan_ID),
#               data=param_df)
# rand1_dur<-REsim(d4_lmer, n.sims=500)
# plotREsim(rand1_dur)
# qqnorm(residuals(d4_lmer))




# fall duration
# all combinations of fixed and random effects came out as singular models

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

# dm1: mig_duration~sex+breeding_status+(1|swan_ID)
# dm2: mig_duration~sex+(1|swan_ID)
# dm3: mig_duration~breeding_status+(1|swan_ID)
# dm1 and dm3 were singular, dm2 wasn't, but dm4 had better diagnostics
dm4<-rpt(mig_duration~(1|swan_ID),
         data=param_df,
         grname="swan_ID",
         datatype="Gaussian",
         nboot=1000,
         ratio=T,
         ncores=7)

# To get variability info for fall departure, I can pull off a non-model-based option
# within-individual: difference in timing for an individual between consecutive years 
#   (report average difference, and standard deviations) 
# among-individual: time span between the first and last individual 
#   (report average time span and standard deviation)

# need to add year in
param_df<-param_df %>% 
  mutate(fall_yr=map_chr(strsplit(.$id_year, "-"), ~.x[2]),
         spring_yr=map_chr(strsplit(.$id_year, "-"), ~.x[3]))



# within
df<-param_df %>% 
  filter(!is.na(fall_mig_onset)) 
df<-df %>% 
  group_by(swan_ID) %>% 
  mutate(nrow=n(),
         mig_fall_diff=ifelse(nrow>1, 
                              max(fall_mig_onset)-min(fall_mig_onset),
                              NA))

# count multiple annual differences for swans with 3 years
diffs<-vector()
ids<-unique(df$swan_ID)
for(i in 1:length(ids)){
  tmp<-df[df$swan_ID==ids[i],]
  nrow<-nrow(tmp)
  if(nrow==2){
    diffs<-c(diffs, abs(tmp$fall_mig_onset[2]-tmp$fall_mig_onset[1]))
  }else if(nrow==3){
    diffs<-c(diffs, abs(tmp$fall_mig_onset[2]-tmp$fall_mig_onset[1]))
    diffs<-c(diffs, abs(tmp$fall_mig_onset[3]-tmp$fall_mig_onset[2]))
  }
}
length(diffs)

mean(diffs)
sd(diffs)


#among
fall_ranges<-df %>% 
  group_by(fall_yr) %>% 
  summarise(fall_range=max(fall_mig_onset)-min(fall_mig_onset))
mean(fall_ranges$fall_range)
sd(fall_ranges$fall_range)

# check sample sizes
length(unique(df$swan_ID))
length(unique(df$id_year))

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


#####
# Try to make dumbbell plots
# Fall departure
# bob<-param_df %>%  
#   filter(!is.na(fall_mig_onset)) %>% 
#   group_by(swan_ID) %>% 
#   mutate(avg_fall_dept=floor(mean(fall_mig_onset)),
#          min_fall_dept=floor(min(fall_mig_onset)),
#          max_fall_dept=floor(max(fall_mig_onset))) %>% 
#   ungroup()
# 
# bob<-bob %>% 
#   arrange(avg_fall_dept) %>% 
#   mutate(order=seq(1, length(avg_fall_dept),1))
#   
# ranges<-bob %>% 
#   select(swan_ID, min_fall_dept, max_fall_dept, order) %>% 
#   distinct(swan_ID,min_fall_dept, max_fall_dept)
# 
# ggplot()+
#   #geom_point(data=bob, aes(x=order, y=avg_fall_dept, colour=fall_yr), size=2)+
#   geom_segment(data=ranges, aes(x=order, 
#                                 xend=order, 
#                                 y=min_fall_dept,
#                                 yend=max_fall_dept),
#                colour="darkgrey")




