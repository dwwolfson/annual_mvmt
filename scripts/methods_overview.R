# To Do
# 1) download and save data in a data folder for testing
# 2) thin down and make NSD plots for every individual


# - create dummy var for 'swan-year' (July 1 to July 1?)
# - calculate NSD, then also a daily average (or even different numbers of days)
# - create subset of data with distinct set of 1) swan-year (e.g. 2020-2021), 2) yr, 3) julian, 4) avg NSD

# -calculate HR for each swan (and each swan-year) in the summer (July1-Aug1?)
# - for each swan, take that HR estimate and consider anything under 5x  (?arbitrary?) the HR radius as "local movements"
#  - consider a category above 'local movements' and below 'long-distance movements"


# - look at the max NSD during that period and use a threshold to designate resident swans
# - remove those from the dataset going forward 



# mcp part
# -might need to thin data even more depending on how slow everything is
# - set up for loop (and then turn into function with parameters)
  # for each ID
    # for each swan-year (spanning from summer to summer)
      

    # Function to be able to run in parallel: (w/ progress bar, 8 cores)
       # filter to a subset of the data based on ID and swan-year
       # set model syntax ( 3-intercept, int_1=int_3 or w/in range of similarity)
       # run mcp on both a single intercept model (for residents) and a 3-intercept model for migrants (consider best model fitting options for convergence vs speed)
       # include progress bar; print info when done fitting; store time from tiktok?
       # calculate loo and compare the two models for each swan-year combination

       # Store: 
          # - model params, r-hat, eff_n for each model and write to table
          # - store elpd values and diff, also which model was better fit
          # - save fitted model object for better fit (only if well converged?)
          # - save a plot of model fit with q-fit=T?


# could have a minimum length of time for the second intercept (overwintering)
# could have a minimum change in intercept (local/short/long distance movement)  (just for descriptive purposes)
# might not need a comparison between 1 and 3 intercept mcp models?
# what to do about winters with multiple plateaus?

# ******
# look at 4K as an example of short-distance (regional) movements; moved about 125km each winter for 3 years in a row
# ******

# ****
# consider the Ohio birds as examples of residents
# visually checking out their NSD plots, most bounce around below 10km displacement