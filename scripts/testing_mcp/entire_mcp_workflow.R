# Test out mcp for migration delineation

# package names
packages<-c("tidyverse", "here", "mcp", "lubridate", "loo", "doFuture")

# install any packages not previously installed
installed_packages<-packages %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])
}

# load packages
invisible(lapply(packages, library, character.only = TRUE))
source(here("scripts/ggplot_custom_function.R"))

registerDoFuture()
plan(multisession)

# Import data
# This is the entire dataset (n=125) condensed to one record for day with averaged NSD
df<-read_csv(here("data/full_dataset_6_28_2022/full_daily_nsd.csv"))
ids<-unique(df$id)

# Create objects to store the results
res<-data.frame(id=NA, year=NA, name=NA, mean=NA, lower=NA, upper=NA, Rhat=NA, n.eff=NA)
write_csv(res, here("output/best_mod_params.csv"))

model_comparison<-data.frame(id=NA, year=NA, 
                             one_int_loo=NA, 
                             two_int_loo=NA,
                             three_int_loo=NA,
                             four_int_loo=NA,
                             five_int_loo=NA)
write_csv(model_comparison, here("output/model_comparisons.csv"))

# split years up each summer
df <- df %>%
  group_by(id) %>%
  mutate(swan_yr = ifelse(yday < 182, paste(id, year - 1, year, sep = "-"),
                          paste(id, year, year + 1, sep = "-")
  )) # 182 is julian day for july 1

df$sqrt<-sqrt(df$nsd_daily_mean)
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
  
  # create a numeric index so that dates track chronologically
  tmp_yr$index<-1:nrow(tmp_yr)
  
  # fit mcp models
  out_mods<-list()
  
  # fit mcp models
  out_mods<-foreach(mm=1:length(int_mods))%dopar%{
    out_mods<-.GlobalEnv$out_mods
    out_mods[[mm]]<-mcp(model = int_mods[[mm]], 
                        data = tmp_yr[,c("rescale", "index")],
                        par_x = "index")
  }
  
  # determine if models fit well or if they are junk based on Rhat values
  for(k in 1:length(out_mods)){
    mod<-as.data.frame(summary(out_mods[[k]]))
    out_mods[[k]]$rhat_check<-any(mod$Rhat<1.1)
  }
  
  loo_list<-list()
  for(nn in 1:length(out_mods)){
    if(out_mods[[nn]]$rhat_check==T){
    out_mods[[nn]]$loo<-loo(out_mods[[nn]])
    loo_list[[nn]]<-out_mods[[nn]]$loo$estimates[[1]]
    }else{
      loo_list[[nn]]<-(-9999) # this is reflect that it didn't pass rhat check but stay in numeric for which.max
      }
  }
  
   # Save the relative fit of each model
   loo_vec<-unlist(loo_list)
   # cbind together with id as 1st col and year as 2nd
   mods<-c(ids[[i]], years[[j]],loo_vec)
   # write out to file
   write_csv(as.data.frame(t(mods)), here("output/model_comparisons.csv"), append = T)

  # pick the best model based on loo
  best_mod<-out_mods[[which.max(loo_list)]]
  params<-as.data.frame(summary(best_mod))
  
  # cbind together with id as 1st col and year as 2nd
  params<-cbind.data.frame(id=ids[[i]], year=years[[j]], params)
  # write out to file
  write_csv(params, here("output/best_mod_params.csv"), append = T)

  

  p<-plot(best_mod, q_fit=T)+
      ggtitle(glue::glue("The best model for {ids[[i]]} in year {years[[j]]} has {length(best_mod$model)} intercepts"))
  p<-p+labs(y = "displacement in km", x = "Date", title = paste(years[[j]], sep = "-"))
  
  ggsave(plot = p, filename = here(glue::glue("output/best_mod_plots/{years[[j]]}.pdf")))
  
  
  }
    cat("Working on swan", i, "out of", length(ids), "\n")
  }

































