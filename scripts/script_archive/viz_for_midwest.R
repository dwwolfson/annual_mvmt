# Visualizations for the flyway update on Aug 2022

library(leaflet)
library(tidyverse)
library(lubridate)
library(viridis)
library(sf)
library(mapview)
library(move)
library(stringr)
library(here)
library(amt)
library(patchwork)

# pull in dataset that was full and saved on 2/11/2022

df<-read_csv(here("data/full_with_mi.csv"))
head(df)
# CTT errors still there
df<-df %>% filter(lat>25)
# removed 56,559 points at 0,0

df$yday<-yday(df$timestamp)


df$jdate<-as.Date(paste(as.character(df$year), as.character(df$yday), sep="-"), "%Y-%j")


df$capture_state<-as.factor(df$capture_state)
df<-df %>% 
  filter(year<2023)
# removed one more outlier


sub<-df %>% distinct(jdate, id, capture_state)
sub<-sub %>% 
  group_by(id) %>% 
  mutate(days=n())



sub$id<-factor(sub$id, levels=sort(unique(sub$id[order(sub$days)])))

p1<-ggplot(sub, aes(jdate, fct_reorder(id, days, .desc=F), color=capture_state))+
  geom_point(size=1.5)+
  theme(axis.text=element_blank())+
  facet_grid(capture_state~., scales="free")

# now overlay the dates of fieldwork to orient people of the multiple years
rect1<-data.frame(xstart=as.Date("2019-07-25"), xend=as.Date("2019-08-29"), col="brown")
rect2<-data.frame(xstart=as.Date("2020-06-23"), xend=as.Date("2020-09-08"), col="red")
rect3<-data.frame(xstart=as.Date("2021-07-06"), xend=as.Date("2021-08-22"), col="green")
rect4<-data.frame(xstart=as.Date("2021-12-15"), xend=as.Date("2021-12-30"), col="blue")

p2<-p1+geom_rect(data=rect1, aes(xmin=xstart, xmax=xend, ymin=-Inf, ymax=Inf, 
                             fill="2019"), alpha=0.3, inherit.aes=F)+
  geom_rect(data=rect2, aes(xmin=xstart, xmax=xend, ymin=-Inf, ymax=Inf, 
                            fill="2020"), alpha=0.3, inherit.aes=F)+
  geom_rect(data=rect3, aes(xmin=xstart, xmax=xend, ymin=-Inf, ymax=Inf, 
                            fill="2021"), alpha=0.3, inherit.aes=F)+
  geom_rect(data=rect4, aes(xmin=xstart, xmax=xend, ymin=-Inf, ymax=Inf, 
                            fill="Arkansas"), alpha=0.3, inherit.aes=F)+
  xlab("Date")+
  theme(axis.title.y=element_blank())+
  theme(strip.text.y = element_text(size=14, face="bold"))+
  theme(strip.background = element_rect(color="black", fill="white"))+
  guides(colour=FALSE, fill=guide_legend("Field season"))



# sorted by swans with the most data  
# redo text for axes
# show mortalities
# show redeployments?

######################################################################################
######################################################################################

# Now make some NSD plots

df_sp<-st_as_sf(df, coords = c('long', 'lat'), crs=4326)

# now transform from lat/long to UTM's
df_sp<-st_transform(df_sp, crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")


# get UTM coordinates and add directly to dataframe
df_sp$utm_E<-st_coordinates(df_sp)[,1]
df_sp$utm_N<-st_coordinates(df_sp)[,2]

# switch back to lat/long
df_sp<-st_transform(df_sp, crs = 4326)

# add lat and long to dataframe as well
df_sp$lon<-st_coordinates(df_sp)[,1]
df_sp$lat<-st_coordinates(df_sp)[,2]


trk<-mk_track(df_sp, .x = utm_E, .y = utm_N, .t = timestamp, id = id, all_cols = T)
saveRDS(trk, file=here("data/scratch/full_no_nsd.rds"))


# tried to resample the track and for some reason the computer totally hung
# besides, NSD doesn't need regularly sampled data anyway as long as there aren't outliers
# I should be able to get nsd without it using amt, otherwise I can calculate it directly
trk2<-trk %>% nest(-id) %>%
  mutate(nsd=map(data, nsd))%>%unnest()

# that ran but it took a while
# I'm going to save out both objects to file
saveRDS(trk2, file=here("data/scratch/full_with_nsd.rds"))

#####
# Create NSD visualizations for presentation

trk2<-readRDS(here("data/scratch/full_with_nsd.rds"))

trk2<-as.data.frame(trk2[c("id", "capture_state", "jdate", "nsd")])

# pick out some examples of different 'categories'

#some long distance ones from Manitoba
mb<-trk2 %>% filter(id=="7H")
mb_nsd<-ggplot(mb, aes(jdate, nsd))+geom_point()

oh<-trk2 %>% filter(id=="1N")
oh_nsd<-ggplot(oh,aes(jdate, nsd))+geom_point()

mn<-trk2 %>% filter(id=="8T")
mn_nsd<-ggplot(mn,aes(jdate, nsd))+geom_point()

mb_nsd|oh_nsd

mig_cats<-rbind(mb, oh, mn)
ggplot(mig_cats, aes(jdate, nsd, color=id))+geom_point()


# how about big groups at a time
mbs<-as.data.frame(trk2 %>% filter(capture_state=="MB"))
ohs<-as.data.frame(trk2 %>% filter(capture_state=="OH"))
ias<-as.data.frame(trk2 %>% filter(capture_state=="IA"))
wis<-as.data.frame(trk2 %>% filter(capture_state=="WI"))
mis<-as.data.frame(trk2 %>% filter(capture_state=="MI"))
mns<-as.data.frame(trk2 %>% filter(capture_state=="MN"))

ggplot(mbs, aes(jdate, nsd, color=id))+geom_point()+ggtitle("Manitoba")
ggplot(ohs, aes(jdate, nsd, color=id))+geom_point()+ggtitle("Ohio")
ggplot(mis, aes(jdate, nsd, color=id))+geom_point()+ggtitle("Michigan")



ggplot(as.data.frame(trk2), aes(jdate, nsd, color=capture_state))+
  geom_point(size=0.1)+facet_wrap(~capture_state)

