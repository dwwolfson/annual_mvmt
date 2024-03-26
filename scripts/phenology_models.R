# Phenology models

library(here)
library(tidyverse)
library(lme4)
library(sjPlot)
library(patchwork)
library(ggpubr)
library(emmeans)
library(performance)


# third round (post apr/may 2023)
# param_df<-read_csv(here("output/migration_metrics_3rd.csv"))

param_df<-read_csv(here("output/post_march_2024/migration_metrics_v4.csv"))

# Merge additional info onto dataframe
ids<-read_csv(here("ids.csv"))
param_df<-param_df %>% 
  left_join(., ids,
            by=c("swan_ID" = "id")) %>% 
  select(-mate_present, -'mass (kg)', -'skull (mm)',-'tarsus (mm)', -comments) %>% 
  rename(breeding_status="breeding_status(if cygnets=breeder; if mate=paired;else non-breeder or cygnet)",
         id_year=year)

# drop cygnets
param_df<-param_df %>% 
  filter(breeding_status%in%c("breeder", "non_breeder", "paired"))


# Methodology is to fit a LMM with sex and breeding status and if the variance for the random intercept is 0,
# then drop the random effect and fit a LM instead.

# Autumn departure
fall_lmer<-lmer(fall_mig_onset~sex+breeding_status+breeding_lat+
                  (1|swan_ID),
                data=param_df)


####################################################
# Spring arrival
spring_lmer<-lmer(spring_arrival~sex+breeding_status+breeding_lat+
                    (1|swan_ID), 
                  data=param_df)


####################################################
# Migration duration
duration_lmer<-lmer(mig_duration~sex+breeding_status+breeding_lat+
                      (1|swan_ID),
                    data=param_df)


# 
# param_df$'Migration Duration'<-param_df$mig_duration
# duration_alt<-lm('Migration Duration'~sex+breeding_status+breeding_lat,
#                  data=param_df)
###########################################################################
# Plot model coefficients

# phenology_models<-plot_models(spring_lmer, fall_lm, duration_lm,
#                               axis.labels=c('Breeding\\Capture Latitude', 'Paired', 'Non-Breeder', 'Sex'),
#                               vline.color="black",
#                               colors=c("#1b9e77", "#d95f02", "#7570b3"), # green, orange, purple, color-blind friendly paletter from colorbrewer
#                               line.size=1.5, 
#                               dot.size=4)+
#   theme_pubr()+
#   theme(text=element_text(size=20, colour="black"),
#         panel.grid.major = element_line(colour="lightgrey"),
#         panel.border = element_blank(),
#         axis.line=element_line(colour="black"),
#         legend.pos="top")+
#   labs(y="\nCoefficient Estimates", color="Migration Metrics")+
#   scale_color_discrete(labels=c("Migration Duration","Autumn Departure","Spring Arrival"))
# 
# ggsave(phenology_models, file=here("figures/figs_for_manuscript/phenology_models.tiff"),
#               dpi=300, compression="lzw")  

#####
# Alternate plotting format (group by migration metric instead of predictors)
p1<-plot_model(fall_lmer,
               axis.labels=c('Breeding\\Capture Latitude', 'Paired', 'Non-Breeder', 'Sex'),
               vline.color="black",
               line.size=1.5, 
               dot.size=4,
               order.terms = c(1,2,3,4),
               group.terms=c(1,2,3,4),
               colors = c("red", "blue", "red", "blue"))+
  theme_pubclean()+
  labs(y="")+
  ggtitle("Autumn Departure")+
  theme(plot.title = element_text(hjust=0.5, size=20),
        text=element_text(size=20, colour="black"),
        panel.grid.major = element_line(colour="lightgrey"),
        panel.border = element_blank(),
        axis.line=element_line(colour="black"))

p2<-plot_model(spring_lmer,
               axis.labels=c('Breeding\\Capture Latitude', 'Paired', 'Non-Breeder', 'Sex'),
               vline.color="black",
               line.size=1.5, 
               dot.size=4,
               order.terms = c(1,2,3,4),
               group.terms=c(1,2,3,4),
               colors = c("red", "red", "red", "blue"))+
  theme_pubclean()+
  labs(y="")+
  ggtitle("Spring Arrival")+
  theme(plot.title = element_text(hjust=0.5, size=20),
        text=element_text(size=20, colour="black"),
        panel.grid.major = element_line(colour="lightgrey"),
        panel.border = element_blank(),
        axis.line=element_line(colour="black"))

p3<-plot_model(duration_lmer,
               axis.labels=c('Breeding\\Capture Latitude', 'Paired', 'Non-Breeder', 'Sex'),
               vline.color="black",
               line.size=1.5, 
               dot.size=4,
               order.terms = c(1,2,3,4),
               group.terms=c(1,2,3,4),
               colors = c("red", "red", "red", "blue"))+
  theme_pubclean()+
  labs(y="\nCoefficient Estimates")+
  ggtitle("Duration of Non-Breeding Season")+
  theme(plot.title = element_text(hjust=0.5, size=20),
        text=element_text(size=20, colour="black"),
        panel.grid.major = element_line(colour="lightgrey"),
        panel.border = element_blank(),
        axis.line=element_line(colour="black"))

new_phenology<-p1/p2/p3

ggsave(new_phenology, file=here("figures/figs_for_manuscript/post_march_2024/updated_phenology_models.tiff"),
       dpi=300, compression="lzw")


# Multiple comparisons
# autumn departure
autumn_contrasts<-emmeans(fall_lmer, "breeding_status")
autumn_contrasts
pairs(autumn_contrasts, infer=c(T,T))

# spring arrival
spring_contrasts<-emmeans(spring_lmer, "breeding_status")
spring_contrasts
pairs(spring_contrasts, infer=c(T,T))

# migration duration
duration_contrasts<-emmeans(duration_lmer, "breeding_status")
duration_contrasts
pairs(duration_contrasts, infer=c(T,T))



####################################################################################

# Marginal effects plot for latitude (by each response)
fall_marg<-plot_model(fall_lmer, type="pred", terms = "breeding_lat", show.data = T)
spring_marg<-plot_model(spring_lmer, type="pred", terms = "breeding_lat",show.data = T)
duration_marg<-plot_model(duration_lmer, type="pred", terms="breeding_lat",show.data = T)

library(ggeffects)
f1<-plot_model(fall_lmer, type="pred", terms = "breeding_status")
phat<-ggeffect(fall_lmer, terms="breeding_status")
f_gg<-plot(phat, show_data = T, jitter=0.3)

s1<-plot_model(spring_lmer, type="pred", terms = "breeding_status")
shat<-ggeffect(spring_lmer, terms="breeding_status")
s_gg<-plot(shat, show_data = T, jitter=0.3)

d1<-plot_model(duration_lmer, type="pred", terms = "breeding_status")
dhat<-ggeffect(duration_lmer, terms="breeding_status")
d_gg<-plot(dhat, show_data = T, jitter=0.3)

f_gg+s_gg+d_gg


# Julian date positions for Sep-Dec 1 dates on y axis
fall_dates<-c(64,94,125, 155)
fall_dates_text<-c("Sep 1", "Oct 1", "Nov 1", "Dec 1")

fall_plot<-fall_marg+
  scale_y_continuous(breaks=fall_dates, labels=fall_dates_text)+
  theme_pubr()+
  labs(x="\nBreeding/Capture Latitude", y="Date of Autumn Departure\n")+
  theme(text=element_text(size=20),
        panel.grid.major = element_line(colour="lightgrey"))+
  ggtitle("A)")



# spring dates for plotting
spring_dates<-c(217, 246, 277)
spring_dates_text<-c("Feb 1", "Mar 1", "Apr 1")

spring_plot<-spring_marg+
  scale_y_continuous(breaks=spring_dates, labels=spring_dates_text)+
  theme_pubr()+
  labs(x="\nBreeding/Capture Latitude", y="Date of Spring Arrival\n")+
  theme(text=element_text(size=20),
        panel.grid.major = element_line(colour="lightgrey"))+
  ggtitle("B)")

duration_plot<-duration_marg+
  theme_pubr()+
  labs(x="\nBreeding/Capture Latitude", y="Duration of non-breeding season\n")+
  theme(text=element_text(size=20),
        panel.grid.major = element_line(colour="lightgrey"))+
  ggtitle("C)")


marginals<-fall_plot+spring_plot+duration_plot

ggsave(marginals, file=here("figures/figs_for_manuscript/post_march_2024/marginal_effects_plots.tiff"),
       dpi=300, compression="lzw")




