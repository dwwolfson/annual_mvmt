# Maps for Laura OH manuscript

# package names
packages <- c("tidyverse", "here", "sf")

# install any packages not previously installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# load packages
invisible(lapply(packages, library, character.only = TRUE))
# source(here("scripts/ggplot_custom_function.R"))

df<-read_csv(here("data/full_dataset_12_30_2022/full_daily_nsd.csv"))
df<-df %>% 
  filter(capture_state=="OH")