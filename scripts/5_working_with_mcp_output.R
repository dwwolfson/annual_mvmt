# working with mcp model output

# package names
packages <- c("tidyverse", "here")

# install any packages not previously installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# load packages
invisible(lapply(packages, library, character.only = TRUE))
source(here("scripts/ggplot_custom_function.R"))

###################################################################################################
# Exclude years with incomplete info (e.g. only summer/fall migration data)
# This is the entire dataset (n=125) condensed to one record for day with averaged NSD
nsd_df<-read_csv(here("data/full_dataset_6_28_2022/full_daily_nsd.csv"))

# split years up each summer
nsd_df <- nsd_df %>%
  group_by(id) %>%
  mutate(swan_yr = ifelse(yday < 182, paste(id, year - 1, year, sep = "-"),
                          paste(id, year, year + 1, sep = "-")
  )) # 182 is julian day for july 1

# check each swan-year to see if it's a complete time-series of data
# this doesn't account for data gaps
nsd_df %>% 
  group_by(swan_yr) %>% 
  mutate(ndays=n()) %>% 
  ggplot(aes(ndays))+geom_histogram()

# Hard to pick a clear cutoff threshold for considering it a 'complete-enough' year
# Some swans are still on wintering area when year ends at the same time that others
# have already made it back to the breeding area

#  *** The best way might be to divide everything into 2 datasets: one for 'complete-enough',
#  *** and the other into a partial dataset. There seems to be a bunch of years where at least the 
#  *** summer->winter data is there, just not the entire return trip back to the breeding area.




###################################################################################################
# Now pick apart all the migration metrics from mcp output

# This is all the output from the mcp best fit models
mod_df <- read_csv(here("output/best_mod_params.csv"))
ids <- unique(mod_df$id)
master_params<-list()
skipped_vec<-vector()

# these are the assessments of whether the mcp fit is adequate to extract model parameters
assess<-read_csv(here("mcp_assessment.csv"))

for (i in seq_along(ids)) {
  
    # filter dataset for each swan
  tmp <- mod_df %>%
    filter(id == ids[[i]])

  years <- unique(tmp$year)
  for (j in seq_along(years)) {
    # check to see that mcp fit reasonably well from visual check 
    if(assess[assess$year==years[[j]],"mcp_fit_well"]=="Y"){
    
    tmp_yr <- tmp %>%
      filter(year == years[[j]])

    out_params <- list()
    
    # Data requirement: individuals have to have a full year of data
    out_params["swan_ID"]<-ids[[i]]
    out_params["year"]<-years[[j]]
    
    
    # rule 1: segments must be at least 2 km from each other in displacement      
    dist_threshold<-2  #kilometers
    # rule 2: segments must be at least 2 days separate from each other in time   
    time_threshold<-2 #days
    # spring return to breeding/capture area threshold
    spring_return_threshold<-10
    # latest date to be considered fall migration onset/ earliest date to be considered spring return
    fall_spring_threshold<-150  #translates to about December 1

    # parameter 1: when do individuals leave the initial breeding area intercept
    # I don't think I need rule 2 for this parameter
    if (length(grep("int", tmp_yr$name))==2){  #if only 1 changepoint, (exclude residents with only one intercept)
      if((tmp_yr[tmp_yr$name == "int_2", "mean"] - tmp_yr[tmp_yr$name == "int_1", "mean"])>dist_threshold &&   # rule 1 passes
         tmp_yr[tmp_yr$name=="int_2", "mean"]>tmp_yr[tmp_yr$name=="int_1", "mean"]    &&    # swan must be moving away from breeding/capture origin
         tmp_yr[tmp_yr$name == "cp_1", "mean"]<fall_spring_threshold    # must be between July and December
         ){ out_params["fall_mig_onset"] <- tmp_yr[tmp_yr$name == "cp_1", "mean"]
      } else{
        out_params["fall_mig_onset"]<-NA
        out_params["fall_mig_onset_comment"]<-"no onset because swan didn't fly far enough away or away from origin, or wasn't during fall"
      }}else if (length(grep("int", tmp_yr$name))>2){ # at least 2 changepoints
        if((tmp_yr[tmp_yr$name == "int_2", "mean"] - tmp_yr[tmp_yr$name == "int_1", "mean"])>dist_threshold && # rule 1 passes for 1st 2 segments
           tmp_yr[tmp_yr$name=="int_2", "mean"]>tmp_yr[tmp_yr$name=="int_1", "mean"] && # swan must be moving away from breeding/capture origin
           tmp_yr[tmp_yr$name == "cp_1", "mean"]<fall_spring_threshold     # must be between July and December
           ){      
          out_params["fall_mig_onset"] <- tmp_yr[tmp_yr$name == "cp_1", "mean"]
        } else if ((tmp_yr[tmp_yr$name == "int_3", "mean"] - tmp_yr[tmp_yr$name == "int_1", "mean"])>dist_threshold && # rule 1 passes for 1st and 3rd seg
                   tmp_yr[tmp_yr$name=="int_3", "mean"]>tmp_yr[tmp_yr$name=="int_1", "mean"] && # swan must be moving away from breeding/capture origin
                   tmp_yr[tmp_yr$name == "cp_1", "mean"]<fall_spring_threshold     # must be between July and December
                   ){  out_params["fall_mig_onset"] <- tmp_yr[tmp_yr$name == "cp_2", "mean"]   #skip 1st segment
      } else {
        out_params["fall_mig_onset"]<-NA
        out_params["fall_mig_onset_comment"]<-"no onset because swan didn't fly far enough away or away from origin, or wasn't during fall"
        #I'm going to assume that there aren't legit fall departures to grab if both the 1st and 2nd segments break rules.....
        }}else if (length(grep("int", tmp_yr$name))==1){
      out_params["fall_mig_onset"] <-NA
      out_params["fall_mig_onset_comment"] <-"no onset because 1 intercept resident"
    }
    
    
    #################################################################################################################
    
    # extra param: number of intercepts in loo-cv chosen model
    out_params["num_intercepts"]<-length(grep("int", tmp_yr$name))

    #################################################################################################################
    
    # parameter 2: number of stops (i.e. segments additional to first and last, therefore wintering range counts as a stop)
    # needs to pass both rules
    if (length(grep("int", tmp_yr$name)) == 1) {
      out_params["num_stops"] <- 0 # if only 1 intercept, no changepoints, no stops, rules don't matter
    } else if (length(grep("int", tmp_yr$name)) == 2) {
      out_params["num_stops"] <- 0 # need at least 3 intercepts to have stops b/w plateaus
      # if 2 intercepts, 1 changepoints, disperser??
    } else if (length(grep("int", tmp_yr$name)) == 3) {
      if (min(abs(diff(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean))) > dist_threshold && # rule 1 passes
        min(abs(diff(tmp_yr[grepl("cp", tmp_yr$name), "mean"]$mean))) > time_threshold) { # rule 2 passes
        out_params["num_stops"] <- 1 # assuming 2 of the plateaus are breeding range, then 1 additional stop
      } else { # don't pass both rules
        out_params["num_stops"] <- 0 # 1 stop but breaks one of the rules; only in cases of residents?
      }
    } else if (length(grep("int", tmp_yr$name)) == 4) {
      if (min(abs(diff(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean))) > dist_threshold && # rule 1 passes
        min(abs(diff(tmp_yr[grepl("cp", tmp_yr$name), "mean"]$mean))) > time_threshold) { # rule 2 passes
        out_params["num_stops"] <- 2 # if 4 intercepts, wintering and one additional staging area stop
      } else { # don't pass both rules
        dists <- abs(diff(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean))
        time_diffs <- abs(diff(tmp_yr[grepl("cp", tmp_yr$name), "mean"]$mean))
        ints_ineligible <- length(dists[dists < dist_threshold])
        cp_ineligible <- length(time_diffs[time_diffs < time_threshold])
        if (sum(ints_ineligible, cp_ineligible) == 1) {
          out_params["num_stops"] <- 1
        } else if (sum(ints_ineligible, cp_ineligible) > 1) {
          ###  HOW TO DETERMINE THE NUMBER OF STOPS?
          ###  NEED TO KNOW IF RULE BREAKS ARE FOR THE SAME TRANSITION OR NOT
          out_params["num_stops"] <- NA
          out_params["num_stops_comment"] <- "multiple rule breaks, determine by hand"
        }
      }
    } else if (length(grep("int", tmp_yr$name)) == 5) {
      if (min(abs(diff(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean))) > dist_threshold && # rule 1 passes
        min(abs(diff(tmp_yr[grepl("cp", tmp_yr$name), "mean"]$mean))) > time_threshold) { # rule 2 passes
        out_params["num_stops"] <- 3 # if 5 intercepts, wintering and 2 additional staging area stops
      } else { # don't pass both rules
        dists <- abs(diff(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean))
        time_diffs <- abs(diff(tmp_yr[grepl("cp", tmp_yr$name), "mean"]$mean))
        ints_ineligible <- length(dists[dists < dist_threshold])
        cp_ineligible <- length(time_diffs[time_diffs < time_threshold])
        if (sum(ints_ineligible, cp_ineligible) == 1) {
          out_params["num_stops"] <- 2 # if only 1 intercept/changepoint is ineligible, then 1 less stop
        } else if (sum(ints_ineligible, cp_ineligible) > 1) {
          ###  HOW TO DETERMINE THE NUMBER OF STOPS?
          ###  NEED TO KNOW IF RULE BREAKS ARE FOR THE SAME TRANSITION OR NOT
          ###  It's possible to have 0, 1, or 2 stops in this case
          out_params["num_stops"] <- NA
          out_params["num_stops_comment"] <- "multiple rule breaks, determine by hand"
        }
      }
    } else if (length(grep("int", tmp_yr$name)) == 6) {
      if (min(abs(diff(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean))) > dist_threshold && # rule 1 passes
          min(abs(diff(tmp_yr[grepl("cp", tmp_yr$name), "mean"]$mean))) > time_threshold) { # rule 2 passes
        out_params["num_stops"] <- 4 # if 6 intercepts, wintering and 3 additional staging area stops
      } else { # don't pass both rules
        dists <- abs(diff(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean))
        time_diffs <- abs(diff(tmp_yr[grepl("cp", tmp_yr$name), "mean"]$mean))
        ints_ineligible <- length(dists[dists < dist_threshold])
        cp_ineligible <- length(time_diffs[time_diffs < time_threshold])
        if (sum(ints_ineligible, cp_ineligible) == 1) {
          out_params["num_stops"] <- 3 # if only 1 intercept/changepoint is ineligible, then 1 less stop
        } else if (sum(ints_ineligible, cp_ineligible) > 1) {
          ###  HOW TO DETERMINE THE NUMBER OF STOPS?
          ###  NEED TO KNOW IF RULE BREAKS ARE FOR THE SAME TRANSITION OR NOT
          ###  It's possible to have 0, 1, or 2 stops in this case
          out_params["num_stops"] <- NA
          out_params["num_stops_comment"] <- "multiple rule breaks, determine by hand"
        }
      }
    } else if (length(grep("int", tmp_yr$name)) == 7) {
      if (min(abs(diff(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean))) > dist_threshold && # rule 1 passes
          min(abs(diff(tmp_yr[grepl("cp", tmp_yr$name), "mean"]$mean))) > time_threshold) { # rule 2 passes
        out_params["num_stops"] <- 5 # if 7 intercepts, wintering and 4 additional staging area stops
      } else { # don't pass both rules
        dists <- abs(diff(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean))
        time_diffs <- abs(diff(tmp_yr[grepl("cp", tmp_yr$name), "mean"]$mean))
        ints_ineligible <- length(dists[dists < dist_threshold])
        cp_ineligible <- length(time_diffs[time_diffs < time_threshold])
        if (sum(ints_ineligible, cp_ineligible) == 1) {
          out_params["num_stops"] <- 4 # if only 1 intercept/changepoint is ineligible, then 1 less stop
        } else if (sum(ints_ineligible, cp_ineligible) > 1) {
          ###  HOW TO DETERMINE THE NUMBER OF STOPS?
          ###  NEED TO KNOW IF RULE BREAKS ARE FOR THE SAME TRANSITION OR NOT
          ###  It's possible to have 0, 1, or 2 stops in this case
          out_params["num_stops"] <- NA
          out_params["num_stops_comment"] <- "multiple rule breaks, determine by hand"
        }
      }
    } else {
      out_params["num_stops"] <- NA
      out_params["num_stops_comment"] <- "flag_didn't_fit_conditions" # go back and figure out what's wrong
    }
    #################################################################################################################
    # parameter 3: how long was each stop
    if (length(grep("int", tmp_yr$name)) == 3 &&
      min(abs(diff(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean))) > dist_threshold &&
      min(abs(diff(tmp_yr[grepl("cp", tmp_yr$name), "mean"]$mean))) > time_threshold) {
      out_params["stop1_duration"] <- tmp_yr[tmp_yr$name == "cp_2", "mean"] - tmp_yr[tmp_yr$name == "cp_1", "mean"]
    } else if (length(grep("int", tmp_yr$name)) == 4 &&
      min(abs(diff(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean))) > dist_threshold &&
      min(abs(diff(tmp_yr[grepl("cp", tmp_yr$name), "mean"]$mean))) > time_threshold) {
      out_params["stop1_duration"] <- tmp_yr[tmp_yr$name == "cp_2", "mean"] - tmp_yr[tmp_yr$name == "cp_1", "mean"]
      out_params["stop2_duration"] <- tmp_yr[tmp_yr$name == "cp_3", "mean"] - tmp_yr[tmp_yr$name == "cp_2", "mean"]
    } else if (length(grep("int", tmp_yr$name)) == 5 &&
      min(abs(diff(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean))) > dist_threshold &&
      min(abs(diff(tmp_yr[grepl("cp", tmp_yr$name), "mean"]$mean))) > time_threshold) {
      out_params["stop1_duration"] <- tmp_yr[tmp_yr$name == "cp_2", "mean"] - tmp_yr[tmp_yr$name == "cp_1", "mean"]
      out_params["stop2_duration"] <- tmp_yr[tmp_yr$name == "cp_3", "mean"] - tmp_yr[tmp_yr$name == "cp_2", "mean"]
      out_params["stop3_duration"] <- tmp_yr[tmp_yr$name == "cp_4", "mean"] - tmp_yr[tmp_yr$name == "cp_3", "mean"]
    } else if (length(grep("int", tmp_yr$name)) == 6 &&
               min(abs(diff(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean))) > dist_threshold &&
               min(abs(diff(tmp_yr[grepl("cp", tmp_yr$name), "mean"]$mean))) > time_threshold) {
      out_params["stop1_duration"] <- tmp_yr[tmp_yr$name == "cp_2", "mean"] - tmp_yr[tmp_yr$name == "cp_1", "mean"]
      out_params["stop2_duration"] <- tmp_yr[tmp_yr$name == "cp_3", "mean"] - tmp_yr[tmp_yr$name == "cp_2", "mean"]
      out_params["stop3_duration"] <- tmp_yr[tmp_yr$name == "cp_4", "mean"] - tmp_yr[tmp_yr$name == "cp_3", "mean"]
      out_params["stop4_duration"] <- tmp_yr[tmp_yr$name == "cp_5", "mean"] - tmp_yr[tmp_yr$name == "cp_4", "mean"]
    } else if (length(grep("int", tmp_yr$name)) == 7 &&
              min(abs(diff(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean))) > dist_threshold &&
              min(abs(diff(tmp_yr[grepl("cp", tmp_yr$name), "mean"]$mean))) > time_threshold) {
      out_params["stop1_duration"] <- tmp_yr[tmp_yr$name == "cp_2", "mean"] - tmp_yr[tmp_yr$name == "cp_1", "mean"]
      out_params["stop2_duration"] <- tmp_yr[tmp_yr$name == "cp_3", "mean"] - tmp_yr[tmp_yr$name == "cp_2", "mean"]
      out_params["stop3_duration"] <- tmp_yr[tmp_yr$name == "cp_4", "mean"] - tmp_yr[tmp_yr$name == "cp_3", "mean"]
      out_params["stop4_duration"] <- tmp_yr[tmp_yr$name == "cp_5", "mean"] - tmp_yr[tmp_yr$name == "cp_4", "mean"]
      out_params["stop5_duration"] <- tmp_yr[tmp_yr$name == "cp_6", "mean"] - tmp_yr[tmp_yr$name == "cp_5", "mean"]
    }

    # (original param 4 excluded): duration of fall migration
    # I decided to axe this one because it only would sum the length of time in staging plateaus, which is not
    # as accurate as the actual duration of fall migration. Also, for any swan that doesn't have a "proper"
    # staging area fit with an intercept, the duration would come out as 0, which is clearly wrong.
   #################################################################################################################    
    # parameter 4: Overall extent of migration
    # still needs to clear the distance rule
    if (length(grep("int", tmp_yr$name))>1){  #exclude residents with only one intercept
      min_int<-min(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean)
      max_int<-max(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean)
      if(max_int-min_int>dist_threshold){                              # traveled beyond segment distance threshold
      out_params["mig_extent"]<-max_int-min_int
      }else{
        out_params["mig_extent"]<-NA
        out_params["mig_extent_comment"]<-glue::glue("didn't travel beyond {dist_threshold} kilometers")
      }
      }
  ##############################################################################################################     
    # parameter 5: date of arrival on 'winter range'/(furthest displacement from origin)
    if (length(grep("int", tmp_yr$name))>1){  #exclude residents with only one intercept
      min_int<-min(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean)
      max_int<-max(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean)
      if(max_int-min_int>dist_threshold){                          # traveled beyond segment distance threshold
      max_int_name<-tmp_yr[tmp_yr$mean==max_int,"name"]            # name of max intercept
      order_int<-str_sub(max_int_name, start=-1)                   # which intercept is max?
      if(order_int==2){
        out_params["furthest_seg_arrival"]<-tmp_yr[tmp_yr$name=="cp_1", "mean"] #date of arrival
      } else if(order_int==3){
        out_params["furthest_seg_arrival"]<-tmp_yr[tmp_yr$name=="cp_2", "mean"] #date of arrival
      } else if(order_int==4){
        out_params["furthest_seg_arrival"]<-tmp_yr[tmp_yr$name=="cp_3", "mean"] #date of arrival
      } else if(order_int==5){
        out_params["furthest_seg_arrival"]<-tmp_yr[tmp_yr$name=="cp_4", "mean"] #date of arrival
      }else if(order_int==6){
        out_params["furthest_seg_arrival"]<-tmp_yr[tmp_yr$name=="cp_5", "mean"] #date of arrival
      }else if(order_int==7){
        out_params["furthest_seg_arrival"]<-tmp_yr[tmp_yr$name=="cp_6", "mean"] #date of arrival
      }}
      else{
        out_params["furthest_seg_arrival"]<-NA
      }}
    #################################################################################################################  
    # parameter 6: how long did they stay on furthest displacement segment
    if (length(grep("int", tmp_yr$name))>1){  #exclude residents with only one intercept
      min_int<-min(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean)
      max_int<-max(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean)
      if(max_int-min_int>dist_threshold){                          # traveled beyond segment distance threshold
      max_int_name<-tmp_yr[tmp_yr$mean==max_int,"name"]            # name of max intercept
      order_int<-str_sub(max_int_name, start=-1)                   # which intercept is max?
      if(order_int==2){
        out_params["furthest_seg_duration"]<-NA
        # If there are only 2 intercepts, there is not a changepoint at the end of the
        # max segment (assuming origin near 0), and so there isn't sufficient info in the 
        # mcp output to know how long the second segment was. There isn't even with the raw data,
        # because presumably the second plateau would extend past the duration of the time-series.
      } else if(order_int==3&&                                    # 3rd intercept is highest
                length(grep("int", tmp_yr$name))!=3){             # and 3rd intercept isn't the last 
        cp3<-tmp_yr[tmp_yr$name=="cp_3", "mean"]
        cp2<-tmp_yr[tmp_yr$name=="cp_2", "mean"]
        out_params["furthest_seg_duration"]<-cp3-cp2
      } else if(order_int==4&&                                  # 4th intercept is highest
                length(grep("int", tmp_yr$name))!=4){           # and 4th intercept isn't the last
        cp4<-tmp_yr[tmp_yr$name=="cp_4", "mean"]
        cp3<-tmp_yr[tmp_yr$name=="cp_3", "mean"]
        out_params["furthest_seg_duration"]<-cp4-cp3
      } else if(order_int==5&&                                  # 5th intercept is highest
                length(grep("int", tmp_yr$name))!=5){           # and 5th intercept isn't the last
        cp5<-tmp_yr[tmp_yr$name=="cp_5", "mean"]
        cp4<-tmp_yr[tmp_yr$name=="cp_4", "mean"]
        out_params["furthest_seg_duration"]<-cp5-cp4
      }else if(order_int==6&&                                  # 6th intercept is highest
               length(grep("int", tmp_yr$name))!=6){           # and 6th intercept isn't the last
        cp6<-tmp_yr[tmp_yr$name=="cp_6", "mean"]
        cp5<-tmp_yr[tmp_yr$name=="cp_5", "mean"]
        out_params["furthest_seg_duration"]<-cp6-cp5
      }else if(order_int==7){
        out_params["furthest_seg_duration"]<-NA
        # If the furthest segment is also the last, then we can't get the duration 
        # of that segment because we don't know the endpoint
      }}}
    
    #################################################################################################################
    
    # parameter 7: when did they leave max segment
    # 7) When did they leave wintering ground and start spring migration? -> changepoint at end of max intercept segment
    # How to distinguish between start of spring migration versus other movement?
    #   param name: furthest_seg_departure
    
    if (length(grep("int", tmp_yr$name))>2){                       # need at least 3 segments to estimate departure from max segment
      max_int<-max(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean) # value of max intercept
      max_int_name<-tmp_yr[tmp_yr$mean==max_int,"name"]            # name of max intercept
      order_int<-str_sub(max_int_name, start=-1)                   # which intercept is max?
      if(order_int==2){
        out_params["furthest_seg_departure"]<-NA
        # If there are only 2 intercepts, there is not a changepoint at the end of the
        # max segment (assuming origin near 0), and so there isn't sufficient info in the 
        # mcp output to know when the individual left. There isn't even with the raw data,
        # because presumably the second plateau would extend past the duration of the time-series.
        
      } else if(order_int==3&&                                     # 3rd intercept is highest
                length(grep("int", tmp_yr$name))!=3){              # and 3rd intercept isn't the last 
        out_params["furthest_seg_departure"]<-tmp_yr[tmp_yr$name=="cp_3", "mean"]
      } else if(order_int==4&&                                     # 4th intercept is highest
                length(grep("int", tmp_yr$name))!=4){              # and 4th intercept isn't the last
        out_params["furthest_seg_departure"]<-tmp_yr[tmp_yr$name=="cp_4", "mean"]
      } else if(order_int==5&&                                     # 5th intercept is highest
                length(grep("int", tmp_yr$name))!=5){              # and 5th intercept isn't the last
        out_params["furthest_seg_departure"]<-tmp_yr[tmp_yr$name=="cp_5", "mean"]
      }else if(order_int==6&&                                     # 6th intercept is highest
               length(grep("int", tmp_yr$name))!=6){              # and 6th intercept isn't the last
        out_params["furthest_seg_departure"]<-tmp_yr[tmp_yr$name=="cp_6", "mean"]
      }else if(order_int==7){
        out_params["furthest_seg_departure"]<-NA
        # If the furthest segment is also the last, then we can't get the departure
        # of that segment because we don't know the endpoint
      } else{
        out_params["furthest_seg_departure"]<-NA 
        # the only other situations should be where the max segment was 3rd, 4th, 5th, or 6th, and also the last segment
      }}
    
    #################################################################################################################
    # parameter 8: When did they return to breeding grounds? 
    #  last plateau has to be within the 'spring_return_threshold' to the first plateau
    #    param name: spring_arrival
    if (length(grep("int", tmp_yr$name))>2){       # have at least 3 intercepts
      num_ints<-length(grep("int", tmp_yr$name))
      first_int<-tmp_yr[tmp_yr$name=="int_1", "mean"]
      last_int<-tmp_yr[tmp_yr$name==paste0("int_", num_ints), "mean"]
        if(abs(first_int-last_int)<spring_return_threshold){    # the spring distance threshold is satisfied
          num_cp<-length(grep("cp", tmp_yr$name))
          if(tmp_yr[tmp_yr$name==paste0("cp_", num_cp), "mean"]>fall_spring_threshold  && # the last changepoint is during the winter/spring/summer
             last_int<tmp_yr[tmp_yr$name==paste0("int_", num_ints-1), "mean"] #last intercept was moving towards the origin
             ){    
          out_params["spring_arrival"]<-tmp_yr[tmp_yr$name==paste0("cp_", num_cp), "mean"]
          }else{
            out_params["spring_arrival"]<-NA
            out_params["spring_arrival_comment"]<-"last changepoint not during winter/spring/summer (Dec-July), or last movement away from origin"
        }
          } else if(abs(first_int-last_int)>spring_return_threshold){
          out_params["spring_arrival"]<-NA
          out_params["spring_arrival_comment"]<-glue::glue("first and last intercepts not within {spring_return_threshold} km")
        }} else if (length(grep("int", tmp_yr$name))<3){
          out_params["spring_arrival"]<-NA
          out_params["spring_arrival_comment"]<-"no spring arrival date because less than 3 intercepts"
        }
    #################################################################################################################
    # parameter 9: How long was the duration of time between fall onset and spring arrival
    #  param name: mig_duration
    if (length(grep("int", tmp_yr$name))>2){          # have at least 3 intercepts
        if (!is.na(out_params[["fall_mig_onset"]]) &&     # have a legit fall departure
          !is.na(out_params[["spring_arrival"]])){        # have a legit spring arrival
          out_params["mig_duration"]<-out_params[["spring_arrival"]]-out_params[["fall_mig_onset"]]
    } else {
      out_params["mig_duration"]<-NA
      out_params["mig_duration_comment"]<-"lack of fall departure and/or spring arrival"
    }} else{
      out_params["mig_duration"]<-NA
      out_params["mig_duration_comment"]<-"need at least 3 intercepts to get migration duration"
    }
    
    #################################################################################################################
    # Append 
    out_df<-bind_rows(out_params)
    master_params<-append(master_params, list(out_df))
    
    #################################################################################################################
    
    #################################################################################################################
    }else{
      skipped<-paste0(years[[j]], " skipped because mcp didn't fit well")
     skipped_vec<-c(skipped_vec, skipped) 
    }
  }
}

param_df<-master_params %>% bind_rows()

write_csv(param_df, here("output/migration_metrics.csv"))
# Translate back to dates from julian day

