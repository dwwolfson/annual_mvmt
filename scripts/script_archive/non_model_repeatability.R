# Non-model-based repeatability

library(here)
library(tidyverse)
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