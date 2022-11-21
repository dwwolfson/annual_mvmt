# Data request for Laura of Ohio swan movements

# package names
packages <- c("tidyverse", "here", "lubridate")

# install any packages not previously installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# load packages
invisible(lapply(packages, library, character.only = TRUE))
# source(here("scripts/ggplot_custom_function.R"))

df<-read_csv(here("data/full_dataset_6_28_2022/full_daily_nsd.csv"))
df<-df %>% 
  filter(capture_state=="OH")

df<-df %>% 
  mutate(disp_km=sqrt(nsd_daily_mean)/1000)

max_displacement<-df %>% 
  group_by(id) %>% 
  summarize(max_displacement=max(disp_km))

df$month<-month(df$timestamp)

df<-df %>% 
  mutate(season=case_when(
    month==8|month==9|month==10|month==11 ~ "fall",
    month==12|month==1|month==2 ~ "winter",
    month==3|month==4|month==5|month==6|month==7 ~ "breeding"
  ))

df %>% 
  group_by(season) %>% 
  summarise(avg_max_displacement=mean(max(disp_km)))

df %>% 
  filter(id!="6M" & id!="9N" & id!="7M") %>% 
  group_by(season) %>% 
  summarise(avg_max_displacement=mean(max(disp_km)))

df %>% 
  group_by(month) %>% 
  summarise(avg_max_displacement=mean(max(disp_km)))

df %>% 
  filter(id!="6M" & id!="9N") %>% 
  group_by(month) %>% 
  summarise(avg_max_displacement=mean(max(disp_km)))

df %>% 
  filter(id!="6M" & id!="9N" & id!="7M") %>% 
  group_by(month) %>% 
  summarise(avg_max_displacement=mean(max(disp_km)))
