# 1) When did they leave? -> first changepoint
#  param name: fall_mig_onset

# 2) How many segments including overwintering (over min time)? -> # of segments b/w first and last intercept with min duration (>3 days?)
#   param name: num_stops

# 3) How long was each stop? -> duration of each segment between first and last (i.e. 2nd changepoint minus 1st, 3rd minus 2nd, etc)
#  param names: stop1_duration, stop2_duration, stop3_duration

# 4) How far was the overall extent of migration? -> (max value intercept - first intercept)
#    param name: mig_extent

# 5) When did they arrive on wintering ground? -> changepoint at beginning of max intercept segment
#    param name: furthest_seg_arrival

# 6) How long on wintering ground? -> duration of max intercept segment
#    param name: furthest_seg_duration

# 7) When did they leave wintering ground and start spring migration? -> changepoint at end of max intercept segment
# How to distinguish between start of spring migration versus other movement?
#   param name: spring_mig_onset

# 8) When did they return to breeding grounds? last changepoint (assuming last intercept is near first intercept (how near???), otherwise consider categorizing as a dispersal)
#    param name: spring_arrival

# 9) Breeding site fidelity -> (last - first intercept; if 0 or low then high site fidelity)
#    param name: breeding_site_fidelity

# Extra (Non-Biological param): number of intercepts in chosen model (just for housekeeping)
#    param name: num_intercepts


# Keep this as a text file instead?