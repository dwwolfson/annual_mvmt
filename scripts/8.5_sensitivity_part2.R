# Sensitivity round 2
# 3/14/2024

# package names
packages <- c("tidyverse", "here", "ggpubr")

# load packages
invisible(lapply(packages, library, character.only = TRUE))

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
fall_spring_threshold<-184  #translates to Jan 1 as the cutoff (remember that julian dates for each swan-year start at July 1, not Jan 1)


# parameter to vary
fall_onset_threshold<-seq(0, 500, 1)


########
# Estimate autumn departure

# This is all the output from the mcp best fit models
mod_df <- read_csv(here("output/best_mod_params.csv"))
ids <- unique(mod_df$id)
autumn_sensitivity_params<-list()
skipped_vec<-vector()

mod_df<-mod_df %>% 
  filter(!id%in%c("7L", "8L", "9L")) # Arkansas captures don't fit pipeline

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
          
            }else if (length(grep("int", tmp_yr$name))>3){
              if((tmp_yr[tmp_yr$name == "int_4", "mean"] - tmp_yr[tmp_yr$name == "int_1", "mean"])>dist_threshold && # rule 1 passes for 1st and 4rd segment
                      tmp_yr[tmp_yr$name=="int_4", "mean"]>tmp_yr[tmp_yr$name=="int_1", "mean"] && # swan must be moving away from breeding/capture origin
                      tmp_yr[tmp_yr$name == "cp_1", "mean"]<fall_spring_threshold  &&   # must be between July and December
                      # get the first and max intercepts and determining if the distance between exceeds the fall_onset_threshold
                      abs(max(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean)-tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean[[1]])>fall_onset_threshold[[k]]
            ){# rules are satisfied; estimate an autumn migration onset
              out_params["fall_mig_onset"] <- tmp_yr[tmp_yr$name == "cp_3", "mean"]   #skip 1st segment
              
            } else {
            out_params["fall_mig_onset"]<-NA
            out_params["fall_mig_onset_comment"]<-"no onset because swan didn't fly far enough away, wasn't moving away from origin, or movement wasn't during fall"
            }
              }else if (length(grep("int", tmp_yr$name))==1){
            out_params["fall_mig_onset"] <-NA
            out_params["fall_mig_onset_comment"] <-"no onset because 1 intercept resident"
              }else {
                out_params["fall_mig_onset"]<-NA
                out_params["fall_mig_onset_comment"]<-"no onset because swan didn't fly far enough away, wasn't moving away from origin, or movement wasn't during fall"
              }
        
        # Append 
        out_df<-bind_rows(out_params)
        autumn_sensitivity_params<-append(autumn_sensitivity_params, list(out_df))
        
      }}else{
        skipped<-paste0(years[[j]], " skipped because mcp didn't fit well")
        skipped_vec<-c(skipped_vec, skipped) # 
      }
      
    } # end loop for years
  } # end loop for individuals
  cat("Working on fall threshold value ", k, "\n" )
} # end loop for varying parameter values

autumn_df<-autumn_sensitivity_params %>% bind_rows()
b6<-autumn_sensitivity_params %>% bind_rows()


# Now determine the proportion of swan-year datasets that were filtered/retained at each value of the autumn departure threshold

autumn_gg1<-autumn_df %>% 
  group_by(autumn_onset_threshold) %>% 
  summarise(autumn_onset_threshold=autumn_onset_threshold,
            total_swan_years=n(),
            unique_swans=length(unique(swan_ID)),
            prop_years_filtered=sum(is.na(fall_mig_onset))/total_swan_years,
            prop_swan_years_retained=sum(!is.na(fall_mig_onset))/total_swan_years) %>% 
  distinct() %>% 
  ggplot(., aes(autumn_onset_threshold, prop_swan_years_retained))+
  geom_line()+
  theme_bw()+
  labs(x="\n Distance threshold used to determine 'long-distance' migration",
       y="Proportion of swan-year datasets retained\n")+
  theme(text=element_text(size=20))+
  geom_vline(xintercept=100, color="red")

# save out csv and figure
write_csv(autumn_df, here("output/sensitivity_analysis/autumn2_v2_500pts.csv"))
ggsave(plot=autumn_gg1, filename=here("output/sensitivity_analysis/autumn_departure_500pts_v2.tiff"),
       dpi=300, compression='lzw')


# add in breeding status
ids<-read_csv(here("ids.csv"))
autumn_df <-autumn_df  %>% 
  left_join(., ids,
            by=c("swan_ID" = "id")) %>% 
  select(-mate_present, -'mass (kg)', -'skull (mm)',-'tarsus (mm)', -comments) %>% 
  rename(breeding_status="breeding_status(if cygnets=breeder; if mate=paired;else non-breeder or cygnet)",
         id_year=year)

autumn_df <-autumn_df %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired"))

by_breeding<-autumn_df %>% 
  group_by(autumn_onset_threshold, breeding_status) %>% 
  summarise(autumn_onset_threshold=autumn_onset_threshold,
            total_swan_years=n(),
            unique_swans=length(unique(swan_ID)),
            prop_years_filtered=sum(is.na(fall_mig_onset))/total_swan_years,
            prop_swan_years_retained=sum(!is.na(fall_mig_onset))/total_swan_years) %>% 
  distinct() %>% 
  ggplot(., aes(autumn_onset_threshold, prop_swan_years_retained, color=breeding_status))+
  geom_line()+
  theme_bw()+
  labs(x="\n Distance threshold used to determine 'long-distance' migration",
       y="Proportion of swan-year datasets retained\n")+
  theme(text=element_text(size=20))+
  geom_vline(xintercept=100, color="red")

ggsave(plot=by_breeding, filename=here("output/sensitivity_analysis/autumn_departure_500pts_v2_breeding.tiff"),
       dpi=300, compression='lzw')

# recovered a bunch of swans that were filtered out but shouldn't have been
# One issue was when segments 1 and 3 or segments 1 and 4 were >2km and should have been used for the 'mig extent'
# Another is that a lot of swans were leaving a lot later than I realized; well into December. 
# I pushed the data threshold back all the way till late Dec
# I went from a filter/retention rate of 32%/68% at fall distance threshold==0, to 10%/90% after changes
# Part of this was removing the swan-years from the swans capture in Arkansas because we can estimate max nsd for those but not dates
# I should probably do those by hand for the phenology analysis in the manuscript

# In terms of swans not filtered out, the original parameters estimated 157 swan-years 
# with a fall migration onset date if the fall_onset_threshold was 0
# After the tweaks applied, we estimated fall dates for an additional 32 swan-years (n=189)


# To instead plot this as an empirical cumulative distribution function as John recommended, I'll need to directly pull off the 
# values that are being used to determine the proportion of swan-years that are making the cutoff and then plug them into the cdf.
# The information being given by that is slightly different, but the plot likely will look similar.
# I'll maybe come back to this in the future.

#################################################################################################################
#################################################################################################################

# Spring sensitivity

# Anyway, need to conduct sensitivity on spring now, and as opposed to the previous 8_sensitivity_analysis.R script, 
# I'll make the spring proportion conditional on receiving a fall migration date (i.e. long-distance) and switch to 
# proportion retained instead of proportion filtered



# Previous thresholds to retain:

# rule 1: segments must be at least 2 km from each other in displacement      
dist_threshold<-2  #kilometers
# 
# # rule 2: segments must be at least 2 days separate from each other in time   
time_threshold<-2 #days
# 
# latest date to be considered fall migration onset/ earliest date to be considered spring return
fall_spring_threshold<-184  #translates to Jan 1 as the cutoff (remember that julian dates for each swan-year start at July 1, not Jan 1)

# fall migration onset threshold
# In order for 'traditional' fall migration to start, 
# the animal must move a certain distance between breeding/capture zone and the furthest eventual segment.
# This is to exclude the movements of locally resident animals from those of migrants
fall_onset_threshold<-100 

# this is equivalent to the fall threshold; used to make sure when swans return to their previous summer 
# territory, that they had traveled >100km during the nonbreeding period
spring_distance_threshold<-100

# set range of spring migration thresholds
# In order to be considered a return to the breeding/capture site, the last segment ('presumably return to breeding site') must be 
# within a certain distance of the first segment ('presumable breeding site during first summer')
spring_proximity_threshold<-seq(0,100, 1)

spring_sensitivity_params<-list()


# This is all the output from the mcp best fit models
mod_df <- read_csv(here("output/best_mod_params.csv"))
ids <- unique(mod_df$id)
skipped_vec<-vector()

mod_df<-mod_df %>% 
  filter(!id%in%c("7L", "8L", "9L")) # Arkansas captures don't fit pipeline

# these are the assessments of whether the mcp fit is adequate to extract model parameters
assess<-read_csv(here("mcp_assessment.csv"))

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
            
          }else if (length(grep("int", tmp_yr$name))>3){
            if((tmp_yr[tmp_yr$name == "int_4", "mean"] - tmp_yr[tmp_yr$name == "int_1", "mean"])>dist_threshold && # rule 1 passes for 1st and 4rd segment
               tmp_yr[tmp_yr$name=="int_4", "mean"]>tmp_yr[tmp_yr$name=="int_1", "mean"] && # swan must be moving away from breeding/capture origin
               tmp_yr[tmp_yr$name == "cp_1", "mean"]<fall_spring_threshold  &&   # must be between July and December
               # get the first and max intercepts and determining if the distance between exceeds the fall_onset_threshold
               abs(max(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean)-tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean[[1]])>fall_onset_threshold
            ){# rules are satisfied; estimate an autumn migration onset
              out_params["fall_mig_onset"] <- tmp_yr[tmp_yr$name == "cp_3", "mean"]   #skip 1st segment
              
            } else {
              out_params["fall_mig_onset"]<-NA
              out_params["fall_mig_onset_comment"]<-"no onset because swan didn't fly far enough away, wasn't moving away from origin, or movement wasn't during fall"
            }
          }else if (length(grep("int", tmp_yr$name))==1){
            out_params["fall_mig_onset"] <-NA
            out_params["fall_mig_onset_comment"] <-"no onset because 1 intercept resident"
          }else {
            out_params["fall_mig_onset"]<-NA
            out_params["fall_mig_onset_comment"]<-"no onset because swan didn't fly far enough away, wasn't moving away from origin, or movement wasn't during fall"
          }
          }# end of autumn departure section
        
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
          } #end of spring estimation; but append params to list before closing loop for 'did mcp model fit'
        
        
        # Append 
        out_df<-bind_rows(out_params)
        spring_sensitivity_params<-append(spring_sensitivity_params, list(out_df))
        
        } # end of 'did mcp model fit well' test
      else{skipped<-paste0(years[[j]], " skipped because mcp didn't fit well")
          skipped_vec<-c(skipped_vec, skipped)}
      
    } # end loop for years
  } # end loop for individuals
  cat("Working on spring threshold value ", k, "\n" )
} # end loop for varying parameter values


spring_df<-spring_sensitivity_params %>% bind_rows()

# Some swans that migrated really late (e.g. early Jan) didn't get a fall migration date

# save out csv and figure
write_csv(spring_df, here("output/sensitivity_analysis/spring_v2.csv"))


spring_v2_gg<-spring_df %>% 
  group_by(spring_proximity_value) %>% 
  summarise(spring_proximity_value=spring_proximity_value,
            total_swan_years=n(),
            unique_swans=length(unique(swan_ID)),
            num_swan_years_retained=sum(!is.na(spring_arrival)),
            prop_years_retained=sum(!is.na(spring_arrival))/sum(!is.na(fall_mig_onset))) %>% 
  distinct() %>% 
  ggplot(., aes(spring_proximity_value, prop_years_retained))+
  geom_line()+
  theme_bw()+
  labs(x="\n Spring proximity threshold used to determine spring arrival",
       y="Proportion of long-distance migrants retained\n")+
  theme(plot.title = element_text(hjust=0.5, size=20),
        text=element_text(size=20))+
  geom_vline(xintercept=10, color="red")+
  geom_text(aes(x=10, label="\nOriginal threshold", y=0.2),
            color="red", angle=90, size=5)+
  geom_vline(xintercept=30, color="blue")+
  geom_text(aes(x=30, label="\nNew threshold proposed", y=0.2),
            color="blue", angle=90, size=5)

# save figure
ggsave(here("output/sensitivity_analysis/spring_arrival_v2.tiff"),
       dpi=300, compression="lzw")


# add in breeding status
# ids<-read_csv(here("ids.csv"))
# spring_df<-spring_df %>% 
#   left_join(., ids,
#             by=c("swan_ID" = "id")) %>% 
#   select(-mate_present, -'mass (kg)', -'skull (mm)',-'tarsus (mm)', -comments) %>% 
#   rename(breeding_status="breeding_status(if cygnets=breeder; if mate=paired;else non-breeder or cygnet)",
#          id_year=year)
# 
# spring_df<-spring_df%>% 
#   filter(breeding_status%in%c("breeder", "non_breeder", "paired"))
# 
# spring_1k<-spring_df %>% 
#   group_by(spring_proximity_value, breeding_status) %>% 
#   summarise(spring_proximity_value=spring_proximity_value,
#             total_swan_years=n(),
#             unique_swans=length(unique(swan_ID)),
#             num_swan_years_retained=sum(!is.na(spring_arrival)),
#             prop_years_retained=sum(!is.na(spring_arrival))/sum(!is.na(fall_mig_onset))) %>% 
#   distinct() %>% 
#   ggplot(., aes(spring_proximity_value, prop_years_retained, color=breeding_status))+
#   geom_line()+
#   theme_pubclean()+
#   labs(x="\n Spring proximity threshold used to determine spring arrival",
#        y="Proportion of long-distance migrants retained\n")+
#   theme(plot.title = element_text(hjust=0.5, size=20),
#         text=element_text(size=20))


