# Investigate differences between breeders and non-breeders for Swan Symposium abstract

library(leaflet)
library(tidyverse)
library(lubridate)
library(viridis)
library(sf)
library(mapview)
library(stringr)
library(here)
library(patchwork)

# pull in dataset that was full and saved on 2/11/2022
# 
# df<-read_csv(here("data/full_with_mi.csv"))
# head(df)
# # CTT errors still there
# df<-df %>% filter(lat>25)
# # removed 56,559 points at 0,0
# 
# df$yday<-yday(df$timestamp)
# 
# 
# df$jdate<-as.Date(paste(as.character(df$year), as.character(df$yday), sep="-"), "%Y-%j")
# 
# 
# df$capture_state<-as.factor(df$capture_state)
# df<-df %>% 
#   filter(year<2023)
# # removed one more outlier


# I"ll try to directly load the full dataset will nsd calculated already
df<-readRDS(here("data/scratch/full_with_nsd.rds"))
df<-as.data.frame(df[names(df)])

ids<-read_csv(here("ids.csv"))
dat<-left_join(df, ids) # this is kinda slow, big dataset

# can drop the geometry to make it easier to manipulate
dat<-dat %>% select(-geometry)
table(ids$`breeding_status(if cygnets=breeder; if mate=paired;else non-breeder or cygnet)`)

# 58% are confirmed breeders
# 5% are cygnets
# 18% are paired but no cygnets (possible breeder/non_breeder)
# 19% are non breeders

# 62% female
# 38% male

dat<-dat %>% 
  rename(breeding=`breeding_status(if cygnets=breeder; if mate=paired;else non-breeder or cygnet)`)
dat<-dat %>% 
  rename(mass=`mass (kg)`)
# calculate average daily nsd value
nsd_dat<-dat %>%
  group_by(id,jdate) %>%
  mutate(daily_nsd=mean(nsd))
  

# Reduce down to a single point a day
nsd_sub<-nsd_dat%>% 
  distinct(id, jdate, daily_nsd)

# from 4.3M points down to 49k

# join info back on 
nsd_sub<-left_join(nsd_sub, ids)
nsd_sub<-nsd_sub %>% 
  rename(breeding=`breeding_status(if cygnets=breeder; if mate=paired;else non-breeder or cygnet)`)

# save dataset
write_csv(nsd_sub, here("data/daily_NSD.csv"))



bob<-nsd_sub %>% 
  filter(breeding=="breeder"|breeding=="paired"|breeding=="non_breeder")

bob$year<-year(bob$jdate)
ggplot(subset(bob, jdate>'2020-07-01'), aes(jdate, daily_nsd, color=breeding))+
  geom_smooth()


ggplot(bob, aes(jdate, daily_nsd, color=sex))+
  geom_smooth()


#####################
# Take the max nsd and model as a dependent variable 
#against breeding status and latitude
filt<-nsd_sub %>% 
  group_by(id) %>% 
  filter(daily_nsd==max(daily_nsd))

filt<-left_join(filt, ids)

#take out arkansas swans
filt<-filt %>% 
  filter(id!="7L"&id!="8L"&id!="9L"&id!="0H_2nd")

filt %>% ggplot(aes(id, sqrt(daily_nsd)/1000, color=as.factor(cygnets)))+
  geom_point(size=3)+
  coord_flip()+
  facet_wrap(~breeding)

# take out cygnets
filt<-filt %>% 
  filter(breeding!="cygnet")





# now only the farthest point is retained for each swan
m1<-glm(daily_nsd~breeding+breeding_lat+breeding:breeding_lat,
          data=filt)

# center and scale breeding latitude
filt$breed_lat_scale<-scale(filt$breeding_lat)

m2<-glm(daily_nsd~breeding+breed_lat_scale+breeding:breed_lat_scale,
        data=filt)

# try gamma distribution
m3<-glm(daily_nsd~breeding+breed_lat_scale+breeding:breed_lat_scale,
        data=filt, family=Gamma)

m4<-glm(sqrt(daily_nsd)~breeding+breed_lat_scale+breeding:breed_lat_scale,
        data=filt, family=Gamma)

m5<-glm(sqrt(daily_nsd)~breeding+breed_lat_scale+breeding:breed_lat_scale,
        data=filt, family=Gamma(link="log"))

m6<-glm(daily_nsd~breeding+breed_lat_scale+breeding:breed_lat_scale,
        data=filt, family=Gamma(link="log"))
#########
mod1<-glm(log(daily_nsd)~breeding+breed_lat_scale,
          data=filt)

mod2<-glm(log(daily_nsd)~breeding+breeding_lat,
          data=filt)

mod3<-glm(log(daily_nsd)~breeding+breed_lat_scale+breeding:breed_lat_scale,
          data=filt)


filt$log_nsd<-log(filt$daily_nsd)
filt$breeding<-as.factor(filt$breeding)
model2<-glm(log_nsd~breeding+breeding_lat,
            data=filt, family=gaussian)

library(car)
residualPlots(mod2)
ggPredict(model2)

fake<-tidyr::expand_grid(breeding_lat=seq(min(filt$breeding_lat), 
                          max(filt$breeding_lat), length.out=100), 
                          breeding=filt$breeding)
bob<-predict(mod2, fake)              

filt<-filt %>% 
  rename(mass=`mass (kg)`)

filt<-filt %>% 
  rename(skull=`skull (mm)`,
         tarsus=`tarsus (mm)`)

bob<-glm(log(daily_nsd)~breeding+breeding_lat+cygnets+sex+mass+skull+tarsus+lead,
         data=filt)

