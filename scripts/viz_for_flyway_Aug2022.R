# Visualizations for the flyway update on Aug 2022
# package names
packages<-c("tidyverse", "here", "lubridate")

# install any packages not previously installed
installed_packages<-packages %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])
}

# load packages
invisible(lapply(packages, library, character.only = TRUE))
# source(here("scripts/ggplot_custom_function.R"))
options(scipen = 999)

# pull in dataset that was downloaded on June 28 2022

df<-read_csv(here("data/full_dataset_6_28_2022/full_w_nsd.csv"))
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
  guides(colour="none", fill=guide_legend("Deployments"))

ggsave(plot=p2, filename="active_collar_fig_flyway_meeting_Aug2022.png")


# Figure out what percent of collars were still active at the end of June for each state?
# but this doesn't account for mortalities and redeployments (so number of collars is artificially high...)
states<-unique(df$capture_state)
for(i in seq_along(states)){
  tmp<-df %>% filter(capture_state==states[[i]])
  tmp1<-tmp %>% 
    filter(year==2022)
  tmp1$month<-month(tmp1$timestamp)
  tmp1<-tmp1 %>% 
    filter(month==6)
  active<-length(unique(tmp1$id))
  prop<-active/length(unique(tmp$id))
  print(paste(states[[i]], active, "/", length(unique(tmp$id)), prop, sep=" "))
}




# sorted by swans with the most data  
# redo text for axes
# show mortalities
# show redeployments?


