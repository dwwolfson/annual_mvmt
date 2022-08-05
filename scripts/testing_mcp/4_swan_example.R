# Test out mcp for migration delineation

# package names
packages<-c("tidyverse", "here", "mcp", "lubridate", "knitr", "ezknitr", "loo", "flextable")

# install any packages not previously installed
installed_packages<-packages %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])
}

# load packages
invisible(lapply(packages, library, character.only = TRUE))

# Import data
# This is the entire dataset (n=125) condensed to one record for day with averaged NSD
df<-read_csv(here("data/daily_NSD.csv"))

# Pull off a few swans to try with mcp
# I'll take a couple from a few different categories (easy long, short, resident, etc)

#1 resident with a few spikes in the winter
# 0C_2nd  (csv== resident, no clear pattern)

# 2 'long-distance' migrant with pretty clear pattern but not perfect
# 3C (csv==migrant, clear pattern)

# 3 resident with one short-ish spike
# 4M (csv==resident, no clear pattern)

# 4 long-distance migrant with multiple extended winter/spring plateaus
# 9H_2nd (csv==migrant, clear pattern)

ids<-c("0C_2nd", "3C", "4M", "9H_2nd")
df<-df %>% 
  filter(id%in%ids)

# split years up each summer
df$yr<-year(df$jdate)
df$julian<-yday(df$jdate)  
df <- df %>%
  group_by(id) %>%
  mutate(swan_yr = ifelse(julian < 182, paste(id, yr - 1, yr, sep = "-"),
                          paste(id, yr, yr + 1, sep = "-")
  )) # 182 is julian day for july 1

df$sqrt<-sqrt(df$daily_nsd)
df$rescale<-df$sqrt/1000


# model syntax
one_int<-list(rescale~1)
two_int<-list(rescale~1,
              ~1)
three_int<-list(rescale~1,
                ~1,
                ~1)
four_int<-list(rescale~1,
               ~1,
               ~1,
               ~1)
five_int<-list(rescale~1,
               ~1,
               ~1,
               ~1,
               ~1)
int_mods<-list(one_int, two_int, three_int, four_int, five_int)

# need to give priors so that appropriate scales are used for migration segments
prior1<-list(int_1="dunif(0, 30") # resident
prior2
prior3
prior4
prior5



# # thin dataset by discarding every other observation
# df<-df %>% 
#   filter(row_number()%%2==1)

# fit models
for(i in seq_along(ids)){
  # filter dataset for each swan
  tmp<-df %>% 
    filter(id==ids[[i]])
  
  # filter each "swan-year"
  years<-unique(tmp$swan_yr)
  
  for(j in seq_along(years)){
  tmp_yr<-tmp %>% 
    filter(swan_yr==years[[j]])
  
  # fit mcp models
  out_mods<-list()

  # worth keeping track of names?
  # mod_names<-NA
  # for(k in 1:5){   #hardwired for the number of models I'm currently trying
  #   mod_names[[k]]<-paste("fit", years[[j]], k, "int", sep="_")
  # }

  for(mm in 1:length(int_mods)){
    out_mods[[mm]]<-mcp(model = int_mods[[mm]], 
                        data = tmp_yr[,c("rescale", "julian")],
                       par_x = "julian")
  }
  
   # names(out_mods)<-mod_names

  
  
  for(nn in 1:length(out_mods)){
    out_mods[[nn]]$loo<-loo(out_mods[[nn]])
  }
  
  loo_compare(out_mods[[1]]$loo,
              out_mods[[2]]$loo,
              out_mods[[3]]$loo,
              out_mods[[4]]$loo,
              out_mods[[5]]$loo)
  
  # plot best model? all models?
  # save out all model output? or just the best model?
  
  }
}


