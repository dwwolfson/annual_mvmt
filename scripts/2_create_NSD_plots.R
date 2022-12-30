# Filter down to 1 average NSD value per day and create individual plots

dependencies <- c("lubridate", "dplyr", "here", "readr", "ggplot2")
for (i in 1:length(dependencies)) {
  if (dependencies[i] %in% installed.packages() == FALSE) {
    install.packages(dependencies[i])
    require(dependencies[i], character.only = TRUE)
  } else {
    require(dependencies[i], character.only = TRUE)
  }
}

source(here("scripts/ggplot_custom_function.R"))

# read in full dataset
df <- read_csv(here("data/full_dataset_12_30_2022/full_w_nsd.csv"))

# create variable for 'swan-year' using summer as endpoints
df$yr <- year(df$timestamp)

df <- df %>%
  filter(yr < 2023)
# removed one more outlier


df$julian <- yday(df$timestamp)

df$jdate <- as.Date(paste(as.character(df$yr), as.character(df$yday), sep = "-"), "%Y-%j")

df <- df %>%
  group_by(id) %>%
  mutate(swan_yr = ifelse(julian < 182, paste(id, yr - 1, yr, sep = "-"),
    paste(id, yr, yr + 1, sep = "-")
  )) # 182 is julian day for july 1

length(unique(df$swan_yr)) # number of swan-years of data (although some of these are incomplete)

# calculate average daily nsd value
df <- df %>%
  group_by(id, jdate) %>%
  mutate(daily_nsd = mean(nsd))

# Reduce down to a single point a day
nsd_sub <- df %>%
  distinct(id, jdate, daily_nsd)
nsd_sub$yr <- year(nsd_sub$jdate)

# Create NSD plots
ids <- unique(nsd_sub$id)

for (i in seq_along(ids)) {
  yrs <- unique(nsd_sub[nsd_sub$id == ids[[i]], "yr"])
  ggplot(nsd_sub[nsd_sub$id == ids[[i]], ], aes(jdate, sqrt(daily_nsd) / 1000)) +
    geom_line() +
    # geom_hline(yintercept=100, color="red")+
    # geom_hline(yintercept=75, color="blue")+
    # geom_hline(yintercept=50, color="green")+
    geom_vline(xintercept = as.Date(c("2019-07-01", "2020-07-01", "2021-07-01", "2022-07-01")), color = "orange") +
    labs(y = "displacement in km", x = "Date", title = paste(ids[[i]], yrs, sep = "-")) +
    theme(plot.title = element_text(size = 22))


  ggsave(here(glue::glue("figures/updated_Dec_2022/nsd_plots_by_swan/{ids[[i]]}.pdf")))
}





