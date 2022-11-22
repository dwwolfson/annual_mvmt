# Pull out max displacement by hand for entire swan dataset

# package names
packages<-c("tidyverse", "here", "lubridate", "R2jags", "rjags", "mcmcplots", "loo", "MCMCvis")

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
  #gamma0 ~ dnorm(10, 0.001)
  #gamma1 ~ dnorm(0, 0.001)
  #gamma2 ~ dunif(0, 500)
  
  sigma1 ~ dunif(1,200)
  tau1 <- 1 / (sigma1*sigma1)
  sigma2 ~ dunif(1,200)
  tau2 <- 1 / (sigma2*sigma2)
  # sigma1 ~ dunif(1,200)
  # sigma2 ~ dunif(1,200)
  # tau1 <- 1 / (sigma1*sigma1)
  # tau2 <- 1 / (sigma2*sigma2)
  # the extra error term didn't seem to help anything
  
  # likelihood
  for (i in 1:n_obs){        
    Y[i] ~ dnorm(mu[i], z[i]*tau1+(1-z[i])*tau2) 
    # Y[i] ~ dnorm(mu[i], z[i]*tau1+(1-z[i])*tau2) 
    z[i] ~ dbern(pi)                    # latent parameter
    
    # take either functional form based on value of z[i]
    mu[i] <- z[i]*(alpha+beta1*x[i])+
      (1-z[i])*(a*x[i]^b)
    #log_lik[i]<-log(dnorm(Y[i], mu[i], tau))
    # not needed
  }}

# MCMC settings
nc <- 3 # number of chains
ni <- 100000 #number of total iterations
nb <- 20000 #burn in 
nt <- 10 # thinning

# JAGS data object
jags.dat<-(list(x = lats, Y = migs, n_obs=n_obs))

# Parameters and computed values to track
params <- c("mu","alpha","beta1", "a","b", "sigma1", "sigma2", "z")

# Run jags
jagsfit <- jags.parallel(data=jags.dat, parameters.to.save=params,
                model.file=latent_model,
                n.thin=10, n.chains=3, n.burnin=20000, n.iter=100000) 

MCMCsummary(jagsfit, params = c("alpha", "beta1", "a","b", "sigma1", "sigma2"))

# out<-data.frame(MCMCsummary(jagsfit))
# out<-rownames_to_column(out, "param")
# mus<-out %>% 
#   filter(grepl("mu", param))
# zs<-out %>% 
#   filter(grepl("z", param))


betas<-MCMCpstr(jagsfit, params=c("alpha", "beta1", "a","b"), type="chains")

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
  mig2_hats <- betas$a*rep(lats_pred[i],nmcmc)^betas$b
   

  conf.int1[i,] <- quantile(mig1_hats, prob = c(0.025, 0.975))
  conf.int2[i,] <- quantile(mig2_hats, prob = c(0.025, 0.975))
  
}

betas_hat<-MCMCpstr(jagsfit, params = c("alpha", "beta1", "a", "b", "z"), func=median)

df$groupID<-jagsfit$BUGSoutput$mean$z

mu_hats1<-data.frame(est=rep(betas_hat$alpha, nlats)+
                       rep(betas_hat$beta1, nlats)*lats_pred,
                     LCL=conf.int1[,1],
                     UCL=conf.int1[,2],
                     latitudes=lats_pred)

mu_hats2<-data.frame(est=betas_hat$a*lats_pred^betas_hat$b,
                     LCL=conf.int2[,1],
                     UCL=conf.int2[,2],
                     latitudes=lats_pred)

ggplot(mu_hats1, aes(latitudes, est))+
  geom_ribbon(aes(ymin=LCL, ymax=UCL), fill="grey70", alpha=2)+
  geom_line()+
  geom_point(data=df, aes(breeding_lat, max_nsd))

ggplot(mu_hats2, aes(latitudes, est))+
  geom_ribbon(aes(ymin=LCL, ymax=UCL), fill="grey70", alpha=2)+
  geom_line()+
  geom_point(data=df, aes(breeding_lat, max_nsd))

ggplot(mu_hats1, aes(latitudes, est))+
  geom_ribbon(aes(ymin=LCL, ymax=UCL), fill="grey70", alpha=2)+
  geom_line()+
  geom_ribbon(data=mu_hats2, aes(ymin=LCL, ymax=UCL), fill="grey70", alpha=2)+
  geom_line(data=mu_hats2, aes(latitudes, est))+
  geom_point(data=df, aes(bl2, max_nsd, color=groupID))+
  geom_line(aes(x=lats_pred, y= betas_hat$a*lats_pred^betas_hat$b))




#############################################
# Find a better functional form for the nonlinear model

# first filter to just get those data points
# 1) migrations under 275 and lat under 46.5
# 2) migration under 700 and lat over 46.5

dat<-df %>% 
  filter((breeding_lat<46.5&max_nsd<275)|
          breeding_lat>46.5&max_nsd<700)

dat %>% ggplot(aes(breeding_lat, max_nsd))+geom_point()
dat %>% ggplot(aes(breeding_lat, max_nsd))+geom_point()+geom_smooth()

# polynomial
m1.poly<-lm(max_nsd~poly(breeding_lat, 2, raw = T), data=dat)

# linear spline
dat$lat_sp1<-ifelse(dat$breeding_lat<43.5, 0, dat$breeding_lat-43.5)
dat$lat_sp2<-ifelse(dat$breeding_lat<46.5, 0, dat$breeding_lat-46.5)

m2.sp<-lm(max_nsd~breeding_lat+lat_sp1+lat_sp2, data=dat)

dat %>% ggplot(aes(breeding_lat, max_nsd))+
  geom_point()+
  geom_smooth(method="lm", formula=y ~ ns(x, 3), se=T)

# natural cubic regression splines
m3.ns<-lm(max_nsd~ns(breeding_lat, 3), data=dat)
attr(ns(dat$breeding_lat, 3), "knots")

AIC(m1.poly, m2.sp, m3.ns)
# cubic spline does the best


