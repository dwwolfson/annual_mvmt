# logit latent state model

# package names
packages<-c("tidyverse", "here", "lubridate", "R2jags", 
            "mcmcplots", "loo", "MCMCvis", "viridis")
options(mc.cores=7)

# install any packages not previously installed
installed_packages<-packages %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])
}

# load packages
invisible(lapply(packages, library, character.only = TRUE))

# most updated version of dataset
df<-read_csv(here("data/full_dataset_4_28_2023/daily_nsd.csv"))
# 126 separate collar deployments

# split years up each summer
df <- df %>%
  group_by(id) %>%
  mutate(swan_yr = ifelse(yday < 182, paste(id, year - 1, year, sep = "-"),
                          paste(id, year, year + 1, sep = "-")
  )) # 182 is julian day for july 1

df<-df %>% 
  group_by(swan_yr) %>% 
  mutate(num_days=n())

# filter out swans that had years with less than 90 days
df<-df %>% 
  filter(num_days>90)

# filtered out 21 swan-years with less than 90 days; 231 swan-year combinations

# Convert NSD to simple displacement in kilometers
df$sqrt<-sqrt(df$nsd_daily_mean)
df$rescale<-df$sqrt/1000

#########################################################################

# Pull out the maximum displacement value for each swan-year
df<-df %>% 
  group_by(swan_yr) %>% 
  mutate(max_nsd=max(rescale)) %>% 
  select(id, capture_state, state_ID, sex, swan_yr, max_nsd, num_days) %>% 
  distinct()


# arkansas data separate from the overall mcp fit
# ark<-read_csv(here("output/arkansas_migration_metrics_salvaged.csv"))
# The numbers that I pulled off from AR swans actually agrees pretty closely with 
# what mcp got from the batch run, so I'll leave it for now (8/18/2023).

# others to exclude
exclude<-c(
  "1P-2020-2021", # taken into custody, year all screwy
  "9J (swan originally collared as 5J)-2021-2022", # collar died before winter
  "5L-2020-2021" ,"5L-2021-2022", # the cygnet that went up to Hudson Bay
  "6M-2021-2022", "6M-2022-2023", # Ohio disperser
  "7M-2021-2022", # Dropped 7M because it made a big movement NE into Pennsylvania (and then back to territory)
  "8P-2021-2022", # big summer dispersal N and then collar died
  "9N-2021-2022", # big summer dispersal
  "9N-2022-2023"  # big summer dispersal
)

# add in breeding lat and other info
ids<-read_csv(here("ids.csv"))
df<-df %>% 
  left_join(., ids) %>% 
  select(-comments) %>% 
  rename(breeding_status="breeding_status(if cygnets=breeder; if mate=paired;else non-breeder or cygnet)",
         mass='mass (kg)', skull='skull (mm)')

df<-df %>% 
  filter(!swan_yr%in%exclude)
# excluded 10 more swan-year datasets, from 231 to 221

# How many were over/under 100km threshold?
df %>% 
  filter(max_nsd>100) %>% 
  nrow()
# 90 under, 131 over

####################################################
# Now fit latent mixture model

# get index variable for individual
ind_index<-consecutive_id(df$id)

# Model 1: Just latitude and max_nsd 
migs<-df$max_nsd
lats<-df$breeding_lat-min(df$breeding_lat)
df$bl2<-df$breeding_lat-min(df$breeding_lat)
n_obs<-nrow(df)
n_inds<-length(unique(df$id))

latent_model<-function(){
  #Random effects
  for(j in 1:n_inds){
    logit(pi[j])<-beta0+b[j]
    b[j] ~ dnorm(0, tau_b) # prior/distribution for the random effects
    z[j] ~ dbern(pi[j]) # latent variables, held constant across years for each individual
  }
  
  beta0 ~ dnorm(0,1/3)
  tau_b <- 1/(sigma_b*sigma_b)
  sigma_b ~ dunif(0, 200)  # prior for random intercepts
  
  # Other Priors
  alpha ~ dnorm(400, 0.001) # intercept for linear model
  beta1 ~ dnorm(150, 0.001) # slope for linear model

  
  a ~ dnorm(30, 0.001)
  exp ~ dunif(0, 30)
  c ~ dunif(0,100)
  
  sigma1 ~ dunif(1,200)
  tau1 <- 1 / (sigma1*sigma1)
  sigma2 ~ dunif(1,200)
  tau2 <- 1 / (sigma2*sigma2)
  
  # Likelihood
  for (i in 1:n_obs){        
    Y[i] ~ dnorm(mu[i], z[ind_index[i]]*tau1+(1-z[ind_index[i]])*tau2) 
    
    # take either functional form based on value of z[i]
    mu[i] <- z[ind_index[i]]*(alpha+beta1*x[i])+
      (1-z[ind_index[i]])*(c+a*x[i]^exp)
  }}

# JAGS data object
jags.dat<-(list(x = lats, Y = migs, n_obs=n_obs, n_inds=n_inds, ind_index=ind_index))

# Parameters and computed values to track
params <- c("mu","beta0","alpha", "beta1", "a","b", "c", "sigma1", "sigma2", "z", "exp", "sigma_b")

# Run jags
jagsfit <- jags.parallel(data=jags.dat, parameters.to.save=params,
                         model.file=latent_model,
                         n.thin=10, n.chains=3, n.burnin=10000, n.iter=20000) 

MCMCsummary(jagsfit, params = c("alpha","beta0", "beta1", "a", "c", "sigma1", "sigma2", "exp"))
# out<-data.frame(MCMCsummary(jagsfit))
# out<-rownames_to_column(out, "param")

betas<-MCMCpstr(jagsfit, params=c("alpha", "beta1", "a","exp", "c"), type="chains")
# save(betas, file="output/updated_latent_state_aug2023/fit_jags.Rda")

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
  mig2_hats <- betas$c+betas$a*rep(lats_pred[i],nmcmc)^betas$exp
  
  
  conf.int1[i,] <- quantile(mig1_hats, prob = c(0.025, 0.975))
  conf.int2[i,] <- quantile(mig2_hats, prob = c(0.025, 0.975))
  
}

betas_hat<-MCMCpstr(jagsfit, params = c("alpha", "beta1", "a", "b", "c", "z", "exp"), func=median)
# save(betas_hat, file="output/updated_latent_state_aug2023/posterior_chains.Rda")

#  NEED TO GIVE EACH SWAN A Z INSTEAD OF EACH SWAN-YEAR
# old version: df$groupID<-jagsfit$BUGSoutput$mean$z
zdat<-data.frame(id=unique(df$id), groupID=jagsfit$BUGSoutput$mean$z)
df<-left_join(df, zdat)


mu_hats1<-data.frame(est=rep(betas_hat$alpha, nlats)+
                       rep(betas_hat$beta1, nlats)*lats_pred,
                     LCL=conf.int1[,1],
                     UCL=conf.int1[,2],
                     latitudes=lats_pred)

mu_hats2<-data.frame(est=betas_hat$c+betas_hat$a*lats_pred^betas_hat$exp,
                     LCL=conf.int2[,1],
                     UCL=conf.int2[,2],
                     latitudes=lats_pred)

#write_csv(mu_hats1, file="output/updated_latent_state_aug2023/mu_hats1.csv")
#write_csv(mu_hats2, file="output/updated_latent_state_aug2023/mu_hats2.csv")


ggplot(mu_hats1, aes(latitudes, est))+
  geom_ribbon(aes(ymin=LCL, ymax=UCL), fill="grey70", alpha=2)+
  geom_line()+
  geom_ribbon(data=mu_hats2, aes(ymin=LCL, ymax=UCL), fill="grey70", alpha=2)+
  #geom_line(data=mu_hats2, aes(latitudes, est))+
  geom_line(aes(x=lats_pred, y=betas_hat$c+betas_hat$a*lats_pred^betas_hat$exp))+
  geom_point(data=df, aes(bl2, max_nsd, color=groupID), size=2)+
  scale_color_continuous(low='red', high='blue')+
  labs(x="Breeding/Capture Latitude", 
       y="Extent of Migration (in km)", 
       color="Pr (Group 1)")


# try to use raw latitude values
mu_hats1$raw_lats<-mu_hats1$latitudes+min(df$breeding_lat)
mu_hats2$raw_lats<-mu_hats2$latitudes+min(df$breeding_lat)

fig<-ggplot(mu_hats1, aes(raw_lats, est))+
  geom_ribbon(aes(ymin=LCL, ymax=UCL), fill="grey70", alpha=0.5)+
  geom_line()+
  geom_ribbon(data=mu_hats2, aes(ymin=LCL, ymax=UCL), fill="grey70", alpha=0.5)+
  #geom_line(data=mu_hats2, aes(latitudes, est))+
  geom_point(data=df, aes(breeding_lat, max_nsd, color=groupID), size=2)+
  geom_line(aes(x=lats_pred+min(df$breeding_lat), y=betas_hat$c+betas_hat$a*lats_pred^betas_hat$exp))+
  scale_color_continuous(low='red', high='blue')+
  labs(x="Breeding/Capture Latitude", 
       y="Extent of Migration (in km)", 
       color="Pr (Group 1)")

library(ggpubr)

# ggsave(here("output/updated_latent_state_aug2023/logistic_pubr.tiff"),
#        compression="lzw")


alt_fig<-ggplot(mu_hats1, aes(raw_lats, est))+
  geom_ribbon(aes(ymin=LCL, ymax=UCL), fill="grey70", alpha=0.5)+
  geom_line()+
  geom_ribbon(data=mu_hats2, aes(ymin=LCL, ymax=UCL), fill="grey70", alpha=0.5)+
  #geom_line(data=mu_hats2, aes(latitudes, est))+
  geom_point(data=df, aes(breeding_lat, max_nsd, color=groupID), size=2)+
  geom_line(aes(x=lats_pred+min(df$breeding_lat), y=betas_hat$c+betas_hat$a*lats_pred^betas_hat$exp))+
  scale_color_continuous(low='red', high='blue', breaks=c(0,1))+
  labs(x="Breeding/Capture Latitude", 
       y="Extent of Migration (in km)", 
       color="Probability of Group 1")+
  theme_pubclean()+
  theme(text=element_text(size=18))

# ggsave(here("output/updated_latent_state_aug2023/logistic_pubclean.tiff"),
#        compression="lzw")

zdat$assignment<-round(zdat$groupID)
zdat<-zdat %>% 
  mutate(mig_status=ifelse(assignment==0, "non-migratory", "migratory"))
write_csv(zdat, here("output/latent_state_assignments.csv"))








ggplot(mu_hats1, aes(raw_lats, est))+
  geom_ribbon(aes(ymin=LCL, ymax=UCL), fill="grey70", alpha=0.5)+
  geom_line()+
  geom_ribbon(data=mu_hats2, aes(ymin=LCL, ymax=UCL), fill="grey70", alpha=0.5)+
  #geom_line(data=mu_hats2, aes(latitudes, est))+
  geom_point(data=df, aes(breeding_lat, max_nsd, color=groupID), size=2)+
  geom_line(aes(x=lats_pred+min(df$breeding_lat), y=betas_hat$c+betas_hat$a*lats_pred^betas_hat$exp))+
  scale_color_continuous(low='red', high='blue', breaks=c(0,1))+
  labs(x="Breeding/Capture Latitude", 
       y="Extent of Migration (in km)", 
       color="Probability of Group 1")+
  theme_pubclean()+
  theme(text=element_text(size=18))+
  theme(legend.key.size = unit(1.5, "line"))
