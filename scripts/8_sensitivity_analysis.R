# Sensitivity analysis for migration metric thresholds

# The objective of this script is to test a range of values used for some of the threshold cutoffs used
# when extracting migration information from the fitted mcp piecewise regression models. For each of these
# that I test (autumn migration onset and spring proximity threshold), I'll estimate the proportion of data
# that is screened for that specific metric (i.e. for autumn migration, how many total swans had an autumn date, 
# and therefore were considered long-distance migrants; and for spring return, how many swans that had an autumn
# date got close enough to the summer territory to get a spring arrival date)

# I'll run 3 sets of analyses:
# 1) Testing a range of distance thresholds for considering a swan a long-distance migrant (and therefore assigning an
# autumn departure date); then I'll divide the sample size by the total number of swans (that passed mcp fit check) to get the proportion retained
# 2) Testing a range of spring proximity thresholds (at a fixed 100km autumn distance threshold) with the other 
# constraints for being assigned a spring return date:
  # 1) the last changepoint is during the right time of year (after Dec 1), and 
  # 2) the last change in intercepts moved towards the origin
# 3) A combined sensitivity analysis for both the autumn onset distance threshold and the spring proximity
# threshold at the same time, then plot the proportion of sample retained as a heat map grid.

# Although the initial script had 10 migration metric parameters estimated, I"ll just strip it down and get the necessary ones.

# package names
packages <- c("tidyverse", "here", "ggpubr")

# load packages
invisible(lapply(packages, library, character.only = TRUE))

###################################################################################################

######################################################################
## Initial Thresholds ###

# rule 1: segments must be at least 2 km from each other in displacement      
# dist_threshold<-2  #kilometers
# 
# # rule 2: segments must be at least 2 days separate from each other in time   
# time_threshold<-2 #days
# 
# # fall migration onset threshold
# # In order for 'traditional' fall migration to start, 
# # the animal must move a certain distance between breeding/capture zone and the furthest eventual segment.
# # This is to exclude the movements of locally resident animals from those of migrants
# fall_onset_threshold<-100    # kilometers
# 
# # spring migration onset threshold
# # similar to fall, I want to exclude locally resident animals from registering spring arrivals
# # this is the minimum distance between the max segment and the segment considered spring return
# spring_distance_threshold<-100
# 
# # spring return threshold
# # In order to be considered a return to the breeding/capture site, the last segment ('presumably return to breeding site') must be 
# # within a certain distance of the first segment ('presumable breeding site during first summer')
# spring_proximity_threshold<-10   # kilometers
# 
# # latest date to be considered fall migration onset/ earliest date to be considered spring return
# fall_spring_threshold<-150  #translates to about December 1

######################################################################

# Analysis 1: Distance threshold for autumn departure (and long-distance migrant classification)



# Previous thresholds to retain:

# rule 1: segments must be at least 2 km from each other in displacement      
dist_threshold<-2  #kilometers
# 
# # rule 2: segments must be at least 2 days separate from each other in time   
time_threshold<-2 #days
# 
# latest date to be considered fall migration onset/ earliest date to be considered spring return
fall_spring_threshold<-150  #translates to about December 1 (remember that dates start at July 1, not Jan 1)


# parameter to vary
fall_onset_threshold<-seq(10, 500, 10)


########
# Estimate autumn departure

# This is all the output from the mcp best fit models
mod_df <- read_csv(here("output/best_mod_params.csv"))
ids <- unique(mod_df$id)
autumn_sensitivity_params<-list()
skipped_vec<-vector()

# these are the assessments of whether the mcp fit is adequate to extract model parameters
assess<-read_csv(here("mcp_assessment.csv"))

# loop by values of fall onset threshold
for (k in seq_along(fall_onset_threshold)){
  
  # loop by individual swans
  for (i in seq_along(ids)){
    
    # filter dataset for each swan
    tmp <- mod_df %>%
      filter(id == ids[[i]])
    
    years <- unique(tmp$year)
    
    # loop by swan-year dataset
    for (j in seq_along(years)) {
      
      # check to see that mcp fit reasonably well from visual check 
      if(assess[assess$year==years[[j]],"mcp_fit_well"]=="Y"){
        
        tmp_yr <- tmp %>%
          filter(year == years[[j]])
        
        out_params <- list()
        
        # Data requirement: individuals have to have a full year of data
        out_params["swan_ID"]<-ids[[i]]
        out_params["year"]<-years[[j]]
        out_params["autumn_onset_threshold"]<-fall_onset_threshold[[k]]
        
        # estimate autumn departure
        if (length(grep("int", tmp_yr$name))==2){  #if only 1 changepoint, (exclude residents with only one intercept)
          out_params["fall_mig_onset"]<-NA
          out_params["fall_mig_onset_comment"]<-"can't consider a migrant with only 1 changepoint"
        } else if (length(grep("int", tmp_yr$name))>2){ # at least 2 changepoints
          if((tmp_yr[tmp_yr$name == "int_2", "mean"] - tmp_yr[tmp_yr$name == "int_1", "mean"])>dist_threshold && # rule 1 (dist b/w segments) passes for 1st 2 segments
             tmp_yr[tmp_yr$name=="int_2", "mean"]>tmp_yr[tmp_yr$name=="int_1", "mean"] && # swan must be moving away from breeding/capture origin
             tmp_yr[tmp_yr$name == "cp_1", "mean"]<fall_spring_threshold &&    # must be between July and December
             # get the first and max intercepts and determining if the distance between exceeds the fall_onset_threshold 
             abs(max(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean)-tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean[[1]])>fall_onset_threshold[[k]]
          ){out_params["fall_mig_onset"] <- tmp_yr[tmp_yr$name == "cp_1", "mean"] # rules are satisfied; estimate an autumn migration onset
          } else if ((tmp_yr[tmp_yr$name == "int_3", "mean"] - tmp_yr[tmp_yr$name == "int_1", "mean"])>dist_threshold && # rule 1 passes for 1st and 3rd segment
                     tmp_yr[tmp_yr$name=="int_3", "mean"]>tmp_yr[tmp_yr$name=="int_1", "mean"] && # swan must be moving away from breeding/capture origin
                     tmp_yr[tmp_yr$name == "cp_1", "mean"]<fall_spring_threshold  &&   # must be between July and December
                     # get the first and max intercepts and determining if the distance between exceeds the fall_onset_threshold
                     abs(max(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean)-tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean[[1]])>fall_onset_threshold[[k]]
          ){# rules are satisfied; estimate an autumn migration onset
            out_params["fall_mig_onset"] <- tmp_yr[tmp_yr$name == "cp_2", "mean"]   #skip 1st segment
          } else {
            out_params["fall_mig_onset"]<-NA
            out_params["fall_mig_onset_comment"]<-"no onset because swan didn't fly far enough away, wasn't moving away from origin, or movement wasn't during fall"
            #I'm going to assume that there aren't legit fall departures to grab if both the 1st and 2nd segments break rules.....
          }}else if (length(grep("int", tmp_yr$name))==1){
            out_params["fall_mig_onset"] <-NA
            out_params["fall_mig_onset_comment"] <-"no onset because 1 intercept resident"
          }
        
        # Append 
        out_df<-bind_rows(out_params)
        autumn_sensitivity_params<-append(autumn_sensitivity_params, list(out_df))
        
      }else{
        skipped<-paste0(years[[j]], " skipped because mcp didn't fit well")
        skipped_vec<-c(skipped_vec, skipped) # this is 50 times bigger than actual because it repeats with every iteration
      }
    
} # end loop for years
} # end loop for individuals
} # end loop for varying parameter values

autumn_df<-autumn_sensitivity_params %>% bind_rows()

# Now determine the proportion of swan-year datasets that were filtered/retained at each value of the autumn departure threshold

autumn_gg<-autumn_df %>% 
  group_by(autumn_onset_threshold) %>% 
  summarise(autumn_onset_threshold=autumn_onset_threshold,
            total_swan_years=n(),
            unique_swans=length(unique(swan_ID)),
            prop_years_filtered=sum(is.na(fall_mig_onset))/total_swan_years) %>% 
  distinct() %>% 
  ggplot(., aes(autumn_onset_threshold, prop_years_filtered))+
  geom_point()+
  theme_bw()+
  labs(x="\n Distance threshold used to determine 'long-distance' migration",
       y="Proportion of swan-year datasets filtered out\n")+
  ggtitle("Sensitivity for distance threshold to consider 'long-distance' migration and estimate an autumn departure date")+
  theme(plot.title = element_text(hjust=0.5, size=20),
        text=element_text(size=20))+
  geom_vline(xintercept=100, color="red")+
  geom_text(aes(x=100, label="\nInitial threshold used", y=0.5),
            color="red", angle=90, size=6)

# save out csv and figure
write_csv(autumn_df, here("output/sensitivity_analysis/autumn_departure_threshold.csv"))
ggsave(plot=autumn_gg, filename=here("output/sensitivity_analysis/autumn_departure.tiff"),
       dpi=300, compression='lzw')


#############################################################################

# Analysis 2: Spring proximity threshold for spring arrival


###################################################################
# Previous thresholds to retain:

# rule 1: segments must be at least 2 km from each other in displacement      
dist_threshold<-2  #kilometers
# 
# # rule 2: segments must be at least 2 days separate from each other in time   
time_threshold<-2 #days
# 
# latest date to be considered fall migration onset/ earliest date to be considered spring return
fall_spring_threshold<-150  #translates to about December 1 (remember that dates start at July 1, not Jan 1)

# reset fall threshold to original 100 km
fall_onset_threshold<-100 

# this is equivalent to the fall threshold; used to make sure when swans return to their previous summer 
# territory, that they had traveled >100km during the nonbreeding period
spring_distance_threshold<-100
###################################################################


# set range of spring migration thresholds
# In order to be considered a return to the breeding/capture site, the last segment ('presumably return to breeding site') must be 
# within a certain distance of the first segment ('presumable breeding site during first summer')
spring_proximity_vec<-seq(2,100, 2)

spring_sensitivity_params<-list()

# loop by values of spring proximity threshold
for (k in seq_along(spring_proximity_vec)){
  
  # loop by individual swans
  for (i in seq_along(ids)){
    
    # filter dataset for each swan
    tmp <- mod_df %>%
      filter(id == ids[[i]])
    
    years <- unique(tmp$year)
    
    # loop by swan-year dataset
    for (j in seq_along(years)) {
      
      # check to see that mcp fit reasonably well from visual check 
      if(assess[assess$year==years[[j]],"mcp_fit_well"]=="Y"){
        
        tmp_yr <- tmp %>%
          filter(year == years[[j]])
        
        out_params <- list()
        
        # Data requirement: individuals have to have a full year of data
        out_params["swan_ID"]<-ids[[i]]
        out_params["year"]<-years[[j]]
        out_params["spring_proximity_value"]<-spring_proximity_vec[[k]]

# estimate autumn departure
if (length(grep("int", tmp_yr$name))==2){  #if only 1 changepoint, (exclude residents with only one intercept)
  out_params["fall_mig_onset"]<-NA
  out_params["fall_mig_onset_comment"]<-"can't consider a migrant with only 1 changepoint"
} else if (length(grep("int", tmp_yr$name))>2){ # at least 2 changepoints
  if((tmp_yr[tmp_yr$name == "int_2", "mean"] - tmp_yr[tmp_yr$name == "int_1", "mean"])>dist_threshold && # rule 1 (dist b/w segments) passes for 1st 2 segments
     tmp_yr[tmp_yr$name=="int_2", "mean"]>tmp_yr[tmp_yr$name=="int_1", "mean"] && # swan must be moving away from breeding/capture origin
     tmp_yr[tmp_yr$name == "cp_1", "mean"]<fall_spring_threshold &&    # must be between July and December
     # get the first and max intercepts and determining if the distance between exceeds the fall_onset_threshold 
     abs(max(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean)-tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean[[1]])>fall_onset_threshold
  ){out_params["fall_mig_onset"] <- tmp_yr[tmp_yr$name == "cp_1", "mean"] # rules are satisfied; estimate an autumn migration onset
  } else if ((tmp_yr[tmp_yr$name == "int_3", "mean"] - tmp_yr[tmp_yr$name == "int_1", "mean"])>dist_threshold && # rule 1 passes for 1st and 3rd segment
             tmp_yr[tmp_yr$name=="int_3", "mean"]>tmp_yr[tmp_yr$name=="int_1", "mean"] && # swan must be moving away from breeding/capture origin
             tmp_yr[tmp_yr$name == "cp_1", "mean"]<fall_spring_threshold  &&   # must be between July and December
             # get the first and max intercepts and determining if the distance between exceeds the fall_onset_threshold
             abs(max(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean)-tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean[[1]])>fall_onset_threshold
  ){# rules are satisfied; estimate an autumn migration onset
    out_params["fall_mig_onset"] <- tmp_yr[tmp_yr$name == "cp_2", "mean"]   #skip 1st segment
  } else {
    out_params["fall_mig_onset"]<-NA
    out_params["fall_mig_onset_comment"]<-"no onset because swan didn't fly far enough away, wasn't moving away from origin, or movement wasn't during fall"
    #I'm going to assume that there aren't legit fall departures to grab if both the 1st and 2nd segments break rules.....
  }}else if (length(grep("int", tmp_yr$name))==1){
    out_params["fall_mig_onset"] <-NA
    out_params["fall_mig_onset_comment"] <-"no onset because 1 intercept resident"
  }

# estimate spring arrival
if (length(grep("int", tmp_yr$name))>2){       # have at least 3 intercepts
num_ints<-length(grep("int", tmp_yr$name))
first_int<-tmp_yr[tmp_yr$name=="int_1", "mean"]
last_int<-tmp_yr[tmp_yr$name==paste0("int_", num_ints), "mean"]
if(abs(first_int-last_int)<spring_proximity_threshold[[k]]){    # the spring proximity threshold is satisfied
  num_cp<-length(grep("cp", tmp_yr$name))
  if(tmp_yr[tmp_yr$name==paste0("cp_", num_cp), "mean"]>fall_spring_threshold  && # the last changepoint is during the winter/spring/summer
     last_int<tmp_yr[tmp_yr$name==paste0("int_", num_ints-1), "mean"] && # last change in intercepts moved towards the origin
     abs(max(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean)-tail(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean, n=1))>spring_distance_threshold                                                                    # overall migration extent was enough to not be considered resident
  ){    
    out_params["spring_arrival"]<-tmp_yr[tmp_yr$name==paste0("cp_", num_cp), "mean"] # spring arrival is the last changepoint
  }else{
    out_params["spring_arrival"]<-NA
    out_params["spring_arrival_comment"]<-glue::glue("last changepoint not during winter/spring/summer (Dec-July),
                                                   or last movement away from origin, or 
                                                   difference between max and last int not above {spring_distance_threshold} km")
  }
} else if(abs(first_int-last_int)>spring_proximity_threshold[[k]]){
  out_params["spring_arrival"]<-NA
  out_params["spring_arrival_comment"]<-glue::glue("first and last intercepts not within {spring_proximity_threshold} km")
}} else if (length(grep("int", tmp_yr$name))<3){
  out_params["spring_arrival"]<-NA
  out_params["spring_arrival_comment"]<-"no spring arrival date because less than 3 intercepts"
}
        
        
        # Append 
        out_df<-bind_rows(out_params)
        spring_sensitivity_params<-append(spring_sensitivity_params, list(out_df))
        
      }else{
        skipped<-paste0(years[[j]], " skipped because mcp didn't fit well")
        skipped_vec<-c(skipped_vec, skipped) # this is 50 times bigger than actual because it repeats with every iteration
      }
      
    } # end loop for years
  } # end loop for individuals
} # end loop for varying parameter values
        
        
spring_df<-spring_sensitivity_params %>% bind_rows()

# Now determine the proportion of swan-year datasets that were filtered/retained at each value of the autumn departure threshold

spring_gg<-spring_df %>% 
  group_by(spring_proximity_value) %>% 
  summarise(spring_proximity_value=spring_proximity_value,
            total_swan_years=n(),
            unique_swans=length(unique(swan_ID)),
            num_swan_years_retained=sum(!is.na(spring_arrival)),
            prop_years_filtered=sum(is.na(spring_arrival))/total_swan_years) %>% 
  distinct() %>% 
  ggplot(., aes(spring_proximity_value, num_swan_years_retained))+
  geom_point()+
  theme_bw()+
  labs(x="\n Spring proximity threshold used to determine spring arrival",
       y="Proportion of swan-year datasets filtered out\n")+
  theme(plot.title = element_text(hjust=0.5, size=20),
        text=element_text(size=20))+
  geom_vline(xintercept=10, color="red")+
  geom_text(aes(x=10, label="\nInitial threshold used", y=40),
            color="red", angle=90, size=5)

# add in breeding status
ids<-read_csv(here("ids.csv"))
spring_df<-spring_df %>% 
  left_join(., ids,
            by=c("swan_ID" = "id")) %>% 
  select(-mate_present, -'mass (kg)', -'skull (mm)',-'tarsus (mm)', -comments) %>% 
  rename(breeding_status="breeding_status(if cygnets=breeder; if mate=paired;else non-breeder or cygnet)",
         id_year=year)

spring_df<-spring_df%>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired"))

bob<-spring_df %>% 
  group_by(spring_proximity_value, breeding_status) %>% 
  summarise(spring_proximity_value=spring_proximity_value,
            total_swan_years=n(),
            unique_swans=length(unique(swan_ID)),
            prop_long_distance_retained=sum(!is.na(spring_arrival))/sum(!is.na(fall_mig_onset))) %>% 
  distinct()


  bob %>% ggplot(., aes(spring_proximity_value, prop_long_distance_retained, color=breeding_status))+
  geom_line()+
  theme_pubclean()+
    labs(x="Proximity threshold for spring arrival",
         y="Number of swan-year datasets retained",
         color="Breeding status")+
    #geom_vline(xintercept=10, color="red")+
    #geom_text(aes(x=10, label="\n10 km", y=15),
     #         color="red", angle=90, size=4)+
    theme(text=element_text(size=14))


  spring_df %>% 
    group_by(spring_proximity_value, breeding_status) %>% 
    summarise(spring_proximity_value=spring_proximity_value,
              total_swan_years=n(),
              unique_swans=length(unique(swan_ID)),
              num_swan_years_retained=sum(!is.na(spring_arrival)),
              prop_years_filtered=sum(is.na(spring_arrival))/total_swan_years,
              p) %>% 
    distinct()%>% ggplot(., aes(spring_proximity_value, num_swan_years_retained, color=breeding_status))+
    geom_line()+
    theme_pubclean()

  
  mutate(fall_NA=as.integer(is.na(fall_mig_onset)),
         spring_NA=as.integer(is.na(spring_arrival)),
         either_NA=if_else(fall_NA==0&spring_NA==0,0,1))



# save out csv and figure
write_csv(spring_df, here("output/sensitivity_analysis/spring_arrival_threshold.csv"))
ggsave(plot=spring_gg, filename=here("output/sensitivity_analysis/spring_arrival.tiff"),
       dpi=300, compression='lzw')

################################################################################################################################################## 

# Analysis 3
# Vary both fall and spring parameters

###################################################################
# Previous thresholds to retain:

# rule 1: segments must be at least 2 km from each other in displacement      
dist_threshold<-2  #kilometers
# 
# # rule 2: segments must be at least 2 days separate from each other in time   
time_threshold<-2 #days
# 
# latest date to be considered fall migration onset/ earliest date to be considered spring return
fall_spring_threshold<-150  #translates to about December 1 (remember that dates start at July 1, not Jan 1)

# Instead of using a separate parameter for sping distance threshold, I'll just plug in what the fall departure value is
###################################################################


# Parameters to vary

# fall 
fall_onset_threshold<-seq(10, 500, 10)

# spring arrival proximity threshold
# In order to be considered a return to the breeding/capture site, the last segment ('presumably return to breeding site') must be 
# within a certain distance of the first segment ('presumable breeding site during first summer')
spring_proximity_threshold<-seq(2,100, 2)


combined_sensitivity_params<-list()


# loop by values of fall departure threshold
for(m in seq_along(fall_onset_threshold)){

  # loop by values of spring proximity threshold
  for (k in seq_along(spring_proximity_threshold)){
  
    # loop by individual swans
    for (i in seq_along(ids)){
    
      # filter dataset for each swan
      tmp <- mod_df %>%
        filter(id == ids[[i]])
    
      years <- unique(tmp$year)
    
      # loop by swan-year dataset
      for (j in seq_along(years)) {
        
          # check to see that mcp fit reasonably well from visual check 
          if(assess[assess$year==years[[j]],"mcp_fit_well"]=="Y"){
          
            tmp_yr <- tmp %>%
              filter(year == years[[j]])
          
            out_params <- list()
          
          # Data requirement: individuals have to have a full year of data
          out_params["swan_ID"]<-ids[[i]]
          out_params["year"]<-years[[j]]
          out_params["spring_proximity_value"]<-spring_proximity_threshold[[k]]
          out_params["fall_departure_value"]<-fall_onset_threshold[[m]]
          
          # estimate autumn departure
          if (length(grep("int", tmp_yr$name))==2){  #if only 1 changepoint, (exclude residents with only one intercept)
            out_params["fall_mig_onset"]<-NA
            out_params["fall_mig_onset_comment"]<-"can't consider a migrant with only 1 changepoint"
          } else if (length(grep("int", tmp_yr$name))>2){ # at least 2 changepoints
            if((tmp_yr[tmp_yr$name == "int_2", "mean"] - tmp_yr[tmp_yr$name == "int_1", "mean"])>dist_threshold && # rule 1 (dist b/w segments) passes for 1st 2 segments
               tmp_yr[tmp_yr$name=="int_2", "mean"]>tmp_yr[tmp_yr$name=="int_1", "mean"] && # swan must be moving away from breeding/capture origin
               tmp_yr[tmp_yr$name == "cp_1", "mean"]<fall_spring_threshold &&    # must be between July and December
               # get the first and max intercepts and determining if the distance between exceeds the fall_onset_threshold 
               abs(max(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean)-tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean[[1]])>fall_onset_threshold[[m]]
            ){out_params["fall_mig_onset"] <- tmp_yr[tmp_yr$name == "cp_1", "mean"] # rules are satisfied; estimate an autumn migration onset
            } else if ((tmp_yr[tmp_yr$name == "int_3", "mean"] - tmp_yr[tmp_yr$name == "int_1", "mean"])>dist_threshold && # rule 1 passes for 1st and 3rd segment
                       tmp_yr[tmp_yr$name=="int_3", "mean"]>tmp_yr[tmp_yr$name=="int_1", "mean"] && # swan must be moving away from breeding/capture origin
                       tmp_yr[tmp_yr$name == "cp_1", "mean"]<fall_spring_threshold  &&   # must be between July and December
                       # get the first and max intercepts and determining if the distance between exceeds the fall_onset_threshold
                       abs(max(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean)-tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean[[1]])>fall_onset_threshold[[m]]
            ){# rules are satisfied; estimate an autumn migration onset
              out_params["fall_mig_onset"] <- tmp_yr[tmp_yr$name == "cp_2", "mean"]   #skip 1st segment
            } else {
              out_params["fall_mig_onset"]<-NA
              out_params["fall_mig_onset_comment"]<-"no onset because swan didn't fly far enough away, wasn't moving away from origin, or movement wasn't during fall"
              #I'm going to assume that there aren't legit fall departures to grab if both the 1st and 2nd segments break rules.....
            }}else if (length(grep("int", tmp_yr$name))==1){
              out_params["fall_mig_onset"] <-NA
              out_params["fall_mig_onset_comment"] <-"no onset because 1 intercept resident"
            }
          
          # estimate spring arrival
          if (length(grep("int", tmp_yr$name))>2){       # have at least 3 intercepts
            num_ints<-length(grep("int", tmp_yr$name))
            first_int<-tmp_yr[tmp_yr$name=="int_1", "mean"]
            last_int<-tmp_yr[tmp_yr$name==paste0("int_", num_ints), "mean"]
            if(abs(first_int-last_int)<spring_proximity_threshold[[k]]){    # the spring proximity threshold is satisfied
              num_cp<-length(grep("cp", tmp_yr$name))
              if(tmp_yr[tmp_yr$name==paste0("cp_", num_cp), "mean"]>fall_spring_threshold  && # the last changepoint is during the winter/spring/summer
                 last_int<tmp_yr[tmp_yr$name==paste0("int_", num_ints-1), "mean"] && # last change in intercepts moved towards the origin
                 abs(max(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean)-tail(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean, n=1))>fall_onset_threshold[[m]] # make sure it moved far enough to have a fall departure                                                                   # overall migration extent was enough to not be considered resident
              ){    
                out_params["spring_arrival"]<-tmp_yr[tmp_yr$name==paste0("cp_", num_cp), "mean"] # spring arrival is the last changepoint
              }else{
                out_params["spring_arrival"]<-NA
                out_params["spring_arrival_comment"]<-glue::glue("last changepoint not during winter/spring/summer (Dec-July),
                                                     or last movement away from origin, or 
                                                     extent not within long-distance threshold")
              }
            } else if(abs(first_int-last_int)>spring_proximity_threshold[[k]]){
              out_params["spring_arrival"]<-NA
              out_params["spring_arrival_comment"]<-glue::glue("first and last intercepts not within {spring_proximity_threshold} km")
            }} else if (length(grep("int", tmp_yr$name))<3){
              out_params["spring_arrival"]<-NA
              out_params["spring_arrival_comment"]<-"no spring arrival date because less than 3 intercepts"
            }
          
          
          # Append 
          out_df<-bind_rows(out_params)
          combined_sensitivity_params<-append(combined_sensitivity_params, list(out_df))
        
        }else{
          skipped<-paste0(years[[j]], " skipped because mcp didn't fit well")
          skipped_vec<-c(skipped_vec, skipped) # this is 50*50 times bigger than actual because it repeats with every iteration
        }
      
      } # end loop for years
    } # end loop for individuals
    cat("Working on spring threshold value ", k, "\n")
  } # end loop for spring threshold values
  cat("Working on fall threshold value ", m, "\n" )
} # end loop for fall threshold values  

# started at 3:30pm
#finished ~7:30
combined_df<-combined_sensitivity_params %>% bind_rows()
write_csv(combined_df, here("output/sensitivity_analysis/combined_dataframe.csv"))

combined_df<-combined_df %>% 
  group_by(spring_proximity_value, fall_departure_value) %>% 
  mutate(fall_NA=as.integer(is.na(fall_mig_onset)),
         spring_NA=as.integer(is.na(spring_arrival)),
         either_NA=if_else(fall_NA==0&spring_NA==0,0,1))

combined_gg<-combined_df %>% 
  group_by(spring_proximity_value, fall_departure_value) %>% 
  summarise(spring_proximity_value=spring_proximity_value,
            fall_departure_value=fall_departure_value,
            total_swan_years=n(),
            prop_years_filtered=sum(either_NA)/total_swan_years) %>% 
  distinct() %>% 
  ggplot(., aes(fall_departure_value, spring_proximity_value, fill=prop_years_filtered))+
  #geom_tile()+
  #scale_fill_viridis_b()
  geom_tile() +
  # scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"))+
  scale_fill_distiller(palette = 'Spectral')+
  geom_segment(aes(x=100, xend=100, y=0, yend=10), colour="blue", linetype="dashed")+
  geom_segment(aes(x=0, xend=100, y=10, yend=10), color="blue", linetype="dashed")+
  labs(x="\nDistance threshold used to determine 'long-distance' migration",
       y="Spring proximity threshold used to determine spring arrival\n",
       fill="Proportion of swan-year datasets without autumn and spring dates")+
  theme_pubclean()+
  theme(axis.title=element_text(size=18),
        text=element_text(size=16))

ggsave(plot=combined_gg, filename=here("output/sensitivity_analysis/combined.tiff"),
       dpi=300, compression='lzw')  
