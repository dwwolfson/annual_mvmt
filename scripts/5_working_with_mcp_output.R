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
# options(scipen = 999)

df <- read_csv(here("output/best_mod_params.csv"))
ids <- unique(df$id)


# out <- data.frame(id = NA, id_year = NA, param = NA, value = NA)

# 1) When did they leave? -> first changepoint
#  param name: fall_mig_onset

# 2) How many segments including overwintering (over min time)? -> # of segments b/w first and last intercept with min duration (>3 days?)
#   param name: num_stops

# 3) How long was each stop? -> duration of each segment between first and last (i.e. 2nd changepoint minus 1st, 3rd minus 2nd, etc)
#  param names: stop1_duration, stop2_duration, stop3_duration

# 4) How far was the overall extent of migration? -> (max value intercept - first intercept)
#    param name: mig_extent

# 5) When did they arrive on wintering ground/furthest segment? -> changepoint at beginning of max intercept segment
#    param name: furthest_seg_arrival

# 6) How long on wintering ground? -> duration of max intercept segment
#    param name: furthest_seg_duration

# 7) When did they leave wintering ground and start spring migration? -> changepoint at end of max intercept segment
# How to distinguish between start of spring migration versus other movement?
#   param name: spring_mig_onset

# 8) Time between spring onset and breeding ground arrival? -> diff in time b/w changepoint at max intercept and last changepoint
#   param name: spring_mig_duration

# 9) When did they return to breeding grounds? last changepoint (assuming last intercept is near first intercept (how near???), otherwise consider categorizing as a dispersal)
#    param name: spring_arrival

# 10) Breeding site fidelity -> (last - first intercept; if 0 or low then high site fidelity)
#    param name: breeding_site_fidelity

for (i in seq_along(ids)) {
  # filter dataset for each swan
  tmp <- df %>%
    filter(id == ids[[i]])

  years <- unique(tmp$year)
  for (j in seq_along(years)) {
    tmp_yr <- tmp %>%
      filter(year == years[[j]])

    out_params <- list()

    # parameter 1: when do individuals leave the initial breeding area intercept
    if (length(grep("int", tmp_yr$name))>1){  #exclude residents with only one intercept
    out_params["fall_mig_onset"] <- tmp_yr[tmp_yr$name == "cp_1", "mean"]
    } else if (length(grep("int", tmp_yr$name))==1){
      out_params["fall_mig_onset"] <-"no onset because 1 intercept resident"
    }
    
    # extra param: number of intercepts in loo-cv chosen model
    out_params["num_intercepts"]<-length(grep("int", tmp_yr$name))

    # parameter 2: number of stops (i.e. segments additional to first and last, thefore wintering range counts as a stop)
    # rule 1: segments must be at least 2 km from each other in displacement
    # rule 2: segments must be at least 2 days separate from each other in time
    if (length(grep("int", tmp_yr$name)) == 1) {
      out_params["num_stops"] <- 0 # if only 1 intercept, no changepoints, no stops, rules don't matter
    } else if (length(grep("int", tmp_yr$name)) == 2) {
      out_params["num_stops"] <- 0 # need at least 3 intercepts to have stops b/w plateaus
      # if 2 intercepts, 1 changepoints, disperser??
    } else if (length(grep("int", tmp_yr$name)) == 3) {
      if (min(abs(diff(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean))) > 2 && # rule 1 passes
        min(abs(diff(tmp_yr[grepl("cp", tmp_yr$name), "mean"]$mean))) > 2) { # rule 2 passes
        out_params["num_stops"] <- 1 # assuming 2 of the plateaus are breeding range, then 1 additional stop
      } else { # don't pass both rules
        out_params["num_stops"] <- 0 # 1 stop but breaks one of the rules; only in cases of residents?
      }
    } else if (length(grep("int", tmp_yr$name)) == 4) {
      if (min(abs(diff(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean))) > 2 && # rule 1 passes
        min(abs(diff(tmp_yr[grepl("cp", tmp_yr$name), "mean"]$mean))) > 2) { # rule 2 passes
        out_params["num_stops"] <- 2 # if 4 intercepts, wintering and one additional staging area stop
      } else { # don't pass both rules
        dists <- abs(diff(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean))
        time_diffs <- abs(diff(tmp_yr[grepl("cp", tmp_yr$name), "mean"]$mean))
        ints_ineligible <- length(dists[dists < 2])
        cp_ineligible <- length(time_diffs[time_diffs < 2])
        if (sum(ints_ineligible, cp_ineligible) == 1) {
          out_params["num_stops"] <- 1
        } else if (sum(ints_ineligible, cp_ineligible) > 1) {
          ###  HOW TO DETERMINE THE NUMBER OF STOPS?
          ###  NEED TO KNOW IF RULE BREAKS ARE FOR THE SAME TRANSITION OR NOT
          out_params["num_stops"] <- "multiple rule breaks, determine by hand"
        }
      }
    } else if (length(grep("int", tmp_yr$name)) == 5) {
      if (min(abs(diff(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean))) > 2 && # rule 1 passes
        min(abs(diff(tmp_yr[grepl("cp", tmp_yr$name), "mean"]$mean))) > 2) { # rule 2 passes
        out_params["num_stops"] <- 3 # if 5 intercepts, wintering and two additional staging area stops
      } else { # don't pass both rules
        dists <- abs(diff(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean))
        time_diffs <- abs(diff(tmp_yr[grepl("cp", tmp_yr$name), "mean"]$mean))
        ints_ineligible <- length(dists[dists < 2])
        cp_ineligible <- length(time_diffs[time_diffs < 2])
        if (sum(ints_ineligible, cp_ineligible) == 1) {
          out_params["num_stops"] <- 2 # if only 1 intercept/changepoint is ineligible, then 1 less stop
        } else if (sum(ints_ineligible, cp_ineligible) > 1) {
          ###  HOW TO DETERMINE THE NUMBER OF STOPS?
          ###  NEED TO KNOW IF RULE BREAKS ARE FOR THE SAME TRANSITION OR NOT
          ###  It's possible to have 0, 1, or 2 stops in this case
          out_params["num_stops"] <- "multiple rule breaks, determine by hand"
        }
      }
    } else {
      out_params["num_stops"] <- "flag" # go back and figure out what's wrong
    }

    # parameter 3: how long was each stop
    if (length(grep("int", tmp_yr$name)) == 3 &&
      min(abs(diff(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean))) > 2 &&
      min(abs(diff(tmp_yr[grepl("cp", tmp_yr$name), "mean"]$mean))) > 2) {
      out_params["stop1_duration"] <- tmp_yr[tmp_yr$name == "cp_2", "mean"] - tmp_yr[tmp_yr$name == "cp_1", "mean"]
    } else if (length(grep("int", tmp_yr$name)) == 4 &&
      min(abs(diff(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean))) > 2 &&
      min(abs(diff(tmp_yr[grepl("cp", tmp_yr$name), "mean"]$mean))) > 2) {
      out_params["stop1_duration"] <- tmp_yr[tmp_yr$name == "cp_2", "mean"] - tmp_yr[tmp_yr$name == "cp_1", "mean"]
      out_params["stop2_duration"] <- tmp_yr[tmp_yr$name == "cp_3", "mean"] - tmp_yr[tmp_yr$name == "cp_2", "mean"]
    } else if (length(grep("int", tmp_yr$name)) == 5 &&
      min(abs(diff(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean))) > 2 &&
      min(abs(diff(tmp_yr[grepl("cp", tmp_yr$name), "mean"]$mean))) > 2) {
      out_params["stop1_duration"] <- tmp_yr[tmp_yr$name == "cp_2", "mean"] - tmp_yr[tmp_yr$name == "cp_1", "mean"]
      out_params["stop2_duration"] <- tmp_yr[tmp_yr$name == "cp_3", "mean"] - tmp_yr[tmp_yr$name == "cp_2", "mean"]
      out_params["stop3_duration"] <- tmp_yr[tmp_yr$name == "cp_4", "mean"] - tmp_yr[tmp_yr$name == "cp_3", "mean"]
    }

    # (original param 4 excluded): duration of fall migration
    # I decided to ax this one because it only would sum the length of time in staging plateaus, which is not
    # as accurate as the actual duration of fall migration. Also, for any swan that doesn't have a "proper"
    # staging area fit with an intercept, the duration would come out as 0, which is clearly wrong.
    
    # parameter 4: Overall extent of migration
    if (length(grep("int", tmp_yr$name))>1){  #exclude residents with only one intercept
      min_int<-min(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean)
      max_int<-max(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean)
      out_params["mig_extent"]<-max_int-min_int
    }
    
    # parameter 5: date of arrival on 'winter range'/(furthest displacement from origin)
    if (length(grep("int", tmp_yr$name))>1){  #exclude residents with only one intercept
      max_int<-max(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean) # value of max intercept
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
      }}
    
    # parameter 6: how long did they stay on furthest displacement segment
    if (length(grep("int", tmp_yr$name))>1){  #exclude residents with only one intercept
      max_int<-max(tmp_yr[grepl("int", tmp_yr$name), "mean"]$mean) # value of max intercept
      max_int_name<-tmp_yr[tmp_yr$mean==max_int,"name"]            # name of max intercept
      order_int<-str_sub(max_int_name, start=-1)                   # which intercept is max?
      if(order_int==2){
        out_params["furthest_seg_arrival"]<-NA
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
      } else if(order_int==5){
        out_params["furthest_seg_duration"]<-NA
        # Similar to before, if the furthest segment is also the last, then we can't get the duration 
        # of that segment becuase we don't know the endpoint
      }}
    
    
    
  }
}
