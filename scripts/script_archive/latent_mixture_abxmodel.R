# Pull out max displacement by hand for entire swan dataset

# package names
packages<-c("tidyverse", "here", "lubridate", "R2jags", "rjags", 
            "mcmcplots", "loo", "MCMCvis", "viridis")

# install any packages not previously installed
installed_packages<-packages %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])
}

# load packages
invisible(lapply(packages, library, character.only = TRUE))

# This is the entire dataset (n=125) condensed to one record for day with averaged NSD
df<-read_csv(here("data/full_dataset_6_28_2022/full_daily_nsd.csv"))
ids<-unique(df$id)

# split years up each summer
df <- df %>%
  group_by(id) %>%
  mutate(swan_yr = ifelse(yday < 182, paste(id, year - 1, year, sep = "-"),
                          paste(id, year, year + 1, sep = "-")
  )) # 182 is julian day for july 1

# 125 separate collar deployments and 204 swan-year combinations


# Convert NSD to simple displacement in kilometers
df$sqrt<-sqrt(df$nsd_daily_mean)
df$rescale<-df$sqrt/1000

#########################################################################

# Pull out the maximum displacement value for each swan-year
df<-df %>% 
  group_by(swan_yr) %>% 
  mutate(max_nsd=max(rescale)) %>% 
  select(id, capture_state, state_ID, sex, swan_yr, max_nsd) %>% 
  distinct()

# add in breeding lat and other info
ids<-read_csv(here("ids.csv"))
df<-df %>% 
  left_join(., ids) %>% 
  select(-mate_present, -comments) %>% 
  rename(breeding_status="breeding_status(if cygnets=breeder; if mate=paired;else non-breeder or cygnet)",
         mass='mass (kg)', skull='skull (mm)')

# Filter out swans that:
# - had incomplete years  (6/7 of 1st yr MN collars)
# I'll consider a swan-year incomplete if it doesn't make it through Jan (unless there is already a settled winter plateau)
incomplete<-c("0H-2021-2022" ,"1P-2020-2021", "3N-2021-2022",
              "4H-2021-2022", "5E-2021-2022",
              "9J (first deployment)-2019-2020",
              "9J (swan originally collared as 5J)-2021-2022")

# - made big summer dispersal movements  n=5? (5L, 6M, 9N, 4P, 8P, 7M)
# 4P did make big movements during the summer, but they were equivalent in distance to it's winter movements, so retain
# Dropped 7M because it made a big movement NE into Pennsylvania (and then back to territory)
dispersers<-c("6M-2021-2022","5L-2020-2021" ,"5L-2021-2022",
              "7M-2021-2022", "8P-2021-2022", "9N-2021-2022")
# - were caught in Arkansas  n=4   # we could consider the lat from the 
ark<-c("0H_2nd-2021-2022", "7L-2021-2022", "8L-2021-2022",  "9L-2021-2022")
filt_vec<-c(incomplete, dispersers, ark)


df<-df %>% 
  filter(!swan_yr%in%filt_vec)
####################################################
# Now fit latent mixture model

# Model 1: Just latitude and max_nsd 
migs<-df$max_nsd
lats<-df$breeding_lat-min(df$breeding_lat)
df$bl2<-df$breeding_lat-min(df$breeding_lat)
n_obs<-nrow(df)

latent_model<-function(){
  # hyperprior for latent state
  pi ~ dunif(0,1)    
  # Priors
  alpha ~ dnorm(400, 0.001) # intercept for linear model
  beta1 ~ dnorm(150, 0.001) # slope for linear model
  delta ~ dunif(2,5)
  #gamma0 ~ dunif(0,400)
  #gamma1 ~ dunif(0,300)
  #gamma2 ~ dunif(0,300)
  a ~ dnorm(30, 0.001)
  b ~ dunif(0, 30)
  c ~ dunif(0,100)
  #gamma0 ~ dnorm(10, 0.001)
  #gamma1 ~ dnorm(0, 0.001)
  #gamma2 ~ dunif(0, 500)
  
  sigma1 ~ dunif(1,200)
  tau1 <- 1 / (sigma1*sigma1)
  sigma2 ~ dunif(1,200)
  tau2 <- 1 / (sigma2*sigma2)
  
  # likelihood
  for (i in 1:n_obs){        
    Y[i] ~ dnorm(mu[i], z[i]*tau1+(1-z[i])*tau2) 
    z[i] ~ dbern(pi)                    # latent parameter
    
    # take either functional form based on value of z[i]
    mu[i] <- z[i]*(alpha+beta1*x[i])+
      (1-z[i])*(c+a*x[i]^b)
  }}

# MCMC settings
# nc <- 3 # number of chains
# ni <- 100000 #number of total iterations
# nb <- 20000 #burn in 
# nt <- 10 # thinning

# JAGS data object
jags.dat<-(list(x = lats, Y = migs, n_obs=n_obs))

# Parameters and computed values to track
params <- c("mu","alpha","beta1", "a","b", "c", "sigma1", "sigma2", "z", "delta")

# Run jags
jagsfit <- jags.parallel(data=jags.dat, parameters.to.save=params,
                model.file=latent_model,
                n.thin=10, n.chains=3, n.burnin=20000, n.iter=200000) 

MCMCsummary(jagsfit, params = c("alpha", "beta1", "a","b", "c", "sigma1", "sigma2"))

# out<-data.frame(MCMCsummary(jagsfit))
# out<-rownames_to_column(out, "param")

betas<-MCMCpstr(jagsfit, params=c("alpha", "beta1", "a","b", "c"), type="chains")
save(betas, file="output/latent_state_model/fit_jags.Rda")

lats_pred<-seq(from=min(df$bl2),
               to=max(df$bl2),
               length=100)

# Credible Interval plotting

# number of mcmc samples
nmcmc<-dim(betas$alpha)[2]

# number of values to predict
nlats<-length(lats_pred)

# matrix to hold 95% CI for each value of lats_pred
conf.int1<-matrix(NA, nlats, 2)
conf.int2<-matrix(NA, nlats, 2)

# loop over values for lats_pred
for(i in 1:nlats){
  # Estimate the migration extent for each breeding lat and for 
  #   each MCMC sample of beta0 and beta1
  mig1_hats <- betas$alpha + rep(lats_pred[i], nmcmc)*betas$beta1
  mig2_hats <- betas$c+betas$a*rep(lats_pred[i],nmcmc)^betas$b
   

  conf.int1[i,] <- quantile(mig1_hats, prob = c(0.025, 0.975))
  conf.int2[i,] <- quantile(mig2_hats, prob = c(0.025, 0.975))
  
}

betas_hat<-MCMCpstr(jagsfit, params = c("alpha", "beta1", "a", "b", "c", "z"), func=median)
save(betas_hat, file="output/latent_state_model/posterior_chains.Rda")

df$groupID<-jagsfit$BUGSoutput$mean$z

mu_hats1<-data.frame(est=rep(betas_hat$alpha, nlats)+
                       rep(betas_hat$beta1, nlats)*lats_pred,
                     LCL=conf.int1[,1],
                     UCL=conf.int1[,2],
                     latitudes=lats_pred)

mu_hats2<-data.frame(est=betas_hat$c+betas_hat$a*lats_pred^betas_hat$b,
                     LCL=conf.int2[,1],
                     UCL=conf.int2[,2],
                     latitudes=lats_pred)

write_csv(mu_hats1, file="output/latent_state_model/mu_hats1.csv")
write_csv(mu_hats2, file="output/latent_state_model/mu_hats2.csv")

source(here("scripts/ggplot_custom_function.R"))


ggplot(mu_hats1, aes(latitudes, est))+
  geom_ribbon(aes(ymin=LCL, ymax=UCL), fill="grey70", alpha=2)+
  geom_line()+
  geom_ribbon(data=mu_hats2, aes(ymin=LCL, ymax=UCL), fill="grey70", alpha=2)+
  #geom_line(data=mu_hats2, aes(latitudes, est))+
  geom_point(data=df, aes(bl2, max_nsd, color=groupID), size=2)+
  geom_line(aes(x=lats_pred, y=betas_hat$c+betas_hat$a*lats_pred^betas_hat$b))+
  scale_color_viridis()+
  labs(x="Breeding/Capture Latitude", 
       y="Extent of Migration (in km)", 
       color="Pr (Group 1)")


# try to use raw latitude values
mu_hats1$raw_lats<-mu_hats1$latitudes+min(df$breeding_lat)
mu_hats2$raw_lats<-mu_hats2$latitudes+min(df$breeding_lat)

fig<-ggplot(mu_hats1, aes(raw_lats, est))+
  geom_ribbon(aes(ymin=LCL, ymax=UCL), fill="grey70", alpha=2)+
  geom_line()+
  geom_ribbon(data=mu_hats2, aes(ymin=LCL, ymax=UCL), fill="grey70", alpha=2)+
  #geom_line(data=mu_hats2, aes(latitudes, est))+
  geom_point(data=df, aes(breeding_lat, max_nsd, color=groupID), size=2)+
  geom_line(aes(x=lats_pred+min(df$breeding_lat), y=betas_hat$c+betas_hat$a*lats_pred^betas_hat$b))+
  scale_color_viridis()+
  labs(x="Breeding/Capture Latitude", 
       y="Extent of Migration (in km)", 
       color="Pr (Group 1)")
ggsave(here("figures/latent_state.png"))

# use scale_color_continuous(low=red hex, mid=yellow, high=blue)
