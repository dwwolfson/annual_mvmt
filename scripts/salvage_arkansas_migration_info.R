# Try to pull useful information from the Arkansas swans


# package names
packages <- c("tidyverse", "here")

# load packages
invisible(lapply(packages, library, character.only = TRUE))


# Now pick apart all the migration metrics from mcp output

# This is all the output from the mcp best fit models
mod_df <- read_csv(here("output/best_mod_params.csv"))
ids <- unique(mod_df$id)
master_params<-list()
skipped_vec<-vector()

# these are the assessments of whether the mcp fit is adequate to extract model parameters
assess<-read_csv(here("mcp_assessment.csv"))



# - were caught in Arkansas  n=4   # we could consider the lat from the eventual summer spot?
ark<-c("0H_2nd-2021-2022", 
       "7L-2021-2022", "7L-2022-2023", 
       "8L-2021-2022", "8L-2022-2023", 
       "9L-2021-2022", "9L-2022-2023")


df<-mod_df %>%
  filter(year%in%ark)


  
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
      
      
      
      