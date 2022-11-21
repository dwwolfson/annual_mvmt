# Latent Mixture Model with migration duration and breeding lat

# Summary and visualization of migration metrics


# package names
packages <- c("tidyverse", "here", "lubridate", "R2jags", "rjags", "mcmcplots", "loo", "MCMCvis")

# install any packages not previously installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# load packages
invisible(lapply(packages, library, character.only = TRUE))
# source(here("scripts/ggplot_custom_function.R"))


param_df<-read_csv(here("output/migration_metrics.csv"))

# Merge additional info onto dataframe
ids<-read_csv(here("ids.csv"))
param_df<-param_df %>% 
  left_join(., ids,
            by=c("swan_ID" = "id")) %>% 
  select(-mate_present, -'mass (kg)', -'skull (mm)',-'tarsus (mm)', -comments) %>% 
  rename(breeding_status="breeding_status(if cygnets=breeder; if mate=paired;else non-breeder or cygnet)",
         id_year=year)


# Translate back to dates from julian day
p_dates<-param_df %>% 
  mutate(across(c(fall_mig_onset, 
                  first_departure,
                  furthest_seg_arrival, 
                  furthest_seg_departure, 
                  spring_arrival),
                ~ifelse(.<186, .+181, .-185)))


p_dates<-p_dates %>% 
  mutate(across(c(fall_mig_onset, 
                  first_departure,
                  furthest_seg_arrival, 
                  furthest_seg_departure, 
                  spring_arrival),
                ~as.Date(., origin="2019-12-31")))

# Add specific years for the fall and spring events (fall_onset and spring_arrival) to track yearly variation
p_dates<-p_dates %>% 
  mutate(fall_yr=map_chr(strsplit(.$id_year, "-"), ~.x[2]),
         spring_yr=map_chr(strsplit(.$id_year, "-"), ~.x[3]))

# add column for entire year cycle
p_dates<-p_dates %>% 
  mutate(entire_yr=paste(map_chr(strsplit(.$id_year, "-"), ~.x[2]),
                         map_chr(strsplit(.$id_year, "-"), ~.x[3]), sep="-"))
###############################################################################
# More data filtering is needed:







# Fit straight line or quadratic polynomial to each group

p_dates<-p_dates %>% drop_na(mig_extent)

migs<-p_dates$mig_extent
lats<-p_dates$breeding_lat
n_obs<-nrow(p_dates)

latent_model<-function(){
  
  # hyperprior for latent state
  pi ~ dunif(0,1)    
  
  # Priors
  alpha ~ dnorm(0,0.01) # intercept for linear model
  beta1 ~ dnorm(0,0.01) # slope for linear model
  gamma0 ~ dunif(0,200)
  gamma1 ~ dunif(0,100)
  gamma2 ~ dunif(0,100)
  sigma ~ dunif(0,500)
  tau <- 1 / (sigma*sigma)
  
  
  # likelihood
  for (i in 1:n.obs){        
    Y[i] ~ dnorm(mu[i], tau) 
    z[i] ~ dbern(pi)                    # latent parameter
    
   # take either functional form based on value of z[i]
      mu[i] <- z[i]*(alpha+beta1*x[i])+
              (1-z[i])*(gamma0+gamma1*x[i]+gamma2*x[i]^2)
      
      # collect a log-likelihood for both?
      log_lik[i]<-log(dnorm(Y[i], mu[i], tau))
    
  }
  
}
  
# MCMC settings
nc <- 3 # number of chains
ni <- 20000 #number of total iterations
nb <- 5000 #burn in 
nt <- 10 # thinning

# JAGS data object
jags.dat<-(list(x = lats, Y = migs, n_obs=n_obs))

# Parameters and computed values to track
params <- c("mu","alpha","beta1","gamma0", "gamma1", "gamma2","sigma", "z", "log_lik")

# Run jags
jagsfit <- jags(data=jags.dat, parameters.to.save=params,
                model.file=latent_model, n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni) 

# # Convergence diagnostic plots
# mcmcplot(jagsfit, parms=c("deviance", "mu","alpha","beta1","gamma0", 
#                           "gamma1", "gamma2","sigma", "z", "log_lik")) 

# Brooks-Gelman-Rubin statistic
BGR=jagsfit$BUGSoutput$summary[,"Rhat"] 
summary(BGR)

df<-data.frame(MCMCsummary(jagsfit, 
                           params=c("mu","alpha","beta1","gamma0", "gamma1", 
                                    "gamma2","sigma", "z", "log_lik")))

yhats<-apply(jagsfit$BUGSoutput$sims.list$mu, 2, quantile, probs=0.5)
ylow<-apply(jagsfit$BUGSoutput$sims.list$mu, 2, quantile, probs=0.025)
yhats<-apply(jagsfit$BUGSoutput$sims.list$mu, 2, quantile, probs=0.975)

 mcmcplot(jagsfit, parms="z") 

p<-p_dates %>% 
  ggplot(aes(breeding_lat, mig_extent))+
  geom_point()+
  geom_line(aes(breeding_lat, yhats))
fig<-plot_ly(p_dates, x=~breeding_lat, y=~mig_extent, color=~swan_ID)
fig
# # Model selection criteria
# llk<-jagsfit$BUGSoutput$sims.matrix[,grep("log.lk[",colnames(jagsfit$BUGSoutput$sims.matrix),fixed="true")]
# loo=loo(llk) #Leave one out cross validation
# waic=waic(llk) #widely applicable information criterion
# DIC=jagsfit$BUGSoutput$DIC
