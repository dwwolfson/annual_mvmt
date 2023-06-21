# Code used in analyses from Abrahms et al. "Ontogenetic shifts from social to experiential learning drive avian migration speed"
# Associated movement data is deposited in Movebank Data Repository, https://doi.org/10.5441/001/1.t23vm852.
# 0. Setup
# 1. Spring model selection
# 2. Autumn model selection, subadult
# 3. Autumn model selection, all ages
# 4. Cumulative snow exposure

#################
# 0. Setup ######
#################

#Load libraries
library(tidyverse)
library(lme4) # for generalized linear mixed models
library(effects) #for plotting partial effects

#Set aesthetics
mycols <- c("#FF7F0EFF", "#D62728FF", "#2CA02CFF", "#1F77B4FF", "#9467BDFF", "#8C564BFF")

#Load data
data <- read.csv('crane_data.csv')

#Calculate latitudinal speed
data <- data %>% 
  group_by(ID, year, season) %>% 
  mutate(lat.speed=(lead(latitude)-latitude)/as.numeric(difftime(lead(date),date, units="secs")))


#################################
# 1. Spring model selection #####
#################################

#Formulas
s1formula <- "SNWZ_scale + NDVI_scale + earlyexp + age + age_oldest_spring_LTM + yday + I(yday^2) + (1|id) + (1|group) + (1|year)"
s2formula <- "NDVI_scale + earlyexp + age + age_oldest_spring_LTM + yday + I(yday^2) + (1|id) + (1|group) + (1|year)"
s3formula <- "SNWZ_scale + earlyexp + age + age_oldest_spring_LTM + yday + I(yday^2) + (1|id) + (1|group) + (1|year)"
s4formula <- "SNWZ_scale + NDVI_scale + age + age_oldest_spring_LTM + yday + I(yday^2) + (1|id) + (1|group) + (1|year)"
s5formula <- "SNWZ_scale + NDVI_scale + earlyexp + age_oldest_spring_LTM + yday + I(yday^2) + (1|id) + (1|group) + (1|year)"
s6formula <- "SNWZ_scale + NDVI_scale + earlyexp + age + yday + I(yday^2) + (1|id) + (1|group) + (1|year)"
s7formula <- "SNWZ_scale + NDVI_scale + earlyexp + age + age_oldest_spring_LTM + (1|id) + (1|group) + (1|year)"
s8formula <- "SNWZ_scale + NDVI_scale + earlyexp + age + age_oldest_spring_LTM + yday + (1|id) + (1|group) + (1|year)"
s9formula <- "SNWZ_scale + NDVI_scale + earlyexp + age + age_oldest_spring_LTM + I(yday^2) + (1|id) + (1|group) + (1|year)"
s10formula <- "SNWZ_scale* earlyexp + NDVI_scale* earlyexp + age + age_oldest_spring_LTM + yday + (1|id) + (1|group) + (1|year)"
s11formula <- "SNWZ_scale* age + NDVI_scale* age + earlyexp + age_oldest_spring_LTM + yday + (1|id) + (1|group) + (1|year)"
s12formula <- "SNWZ_scale* age_oldest_spring_LTM + NDVI_scale* age_oldest_spring_LTM + earlyexp + age + yday + (1|id) + (1|group) + (1|year)"
s13formula <- "SNWZ_scale* earlyexp* age_oldest_spring_LTM + NDVI_scale* earlyexp* age_oldest_spring_LTM + age + yday + (1|id) + (1|group) + (1|year)"
s14formula <- "SNWZ_scale* earlyexp* age + NDVI_scale* earlyexp* age + age_oldest_spring_LTM + yday + (1|id) + (1|group) + (1|year)"
s15formula <- "SNWZ_scale* earlyexp* age + NDVI_scale* earlyexp* age + yday + (1|id) + (1|group) + (1|year)"
s16formula <- "SNWZ_scale* earlyexp* age_oldest_spring_LTM + NDVI_scale* earlyexp* age_oldest_spring_LTM + yday + (1|id) + (1|group) + (1|year)"
s17formula <- "SNWZ_scale* earlyexp + SNWZ_scale* age + NDVI_scale* earlyexp + NDVI_scale* age + yday + (1|id) + (1|group) + (1|year)"
s18formula <- "yday + (1|id) + (1|group) + (1|year)"
s19formula <- "SNWZ_scale* earlyexp* age + yday + (1|id) + (1|group) + (1|year)"
s20formula <- "NDVI_scale* earlyexp* age + yday + (1|id) + (1|group) + (1|year)"
s21formula <- "SNWZ_scale* age + yday + (1|id) + (1|group) + (1|year)"
s22formula <- "NDVI_scale* age + yday + (1|id) + (1|group) + (1|year)"
s23formula <- "SNWZ_scale* earlyexp + yday + (1|id) + (1|group) + (1|year)"
s24formula <- "NDVI_scale* earlyexp + yday + (1|id) + (1|group) + (1|year)"
s25formula <- "SNWZ_scale + yday + (1|id) + (1|group) + (1|year)"
s26formula <- "NDVI_scale + yday + (1|id) + (1|group) + (1|year)"
s27formula <- "SNWZ_scale + NDVI_scale + yday + (1|id) + (1|group) + (1|year)"

#Models
s1 <- glmer(lat.speed ~ SNWZ_scale + NDVI_scale + earlyexp + age + age_oldest_spring_LTM + yday + I(yday^2) + (1|id) + (1|year) + (1|tracking_group_id_spring), data=subset(data, season=="spring migration"))
s2 <- glmer(lat.speed ~ NDVI_scale + earlyexp + age + age_oldest_spring_LTM + yday + I(yday^2) + (1|id) + (1|year)+ (1|tracking_group_id_spring), data=subset(data, season=="spring migration"))
s3 <- glmer(lat.speed ~ SNWZ_scale + earlyexp + age + age_oldest_spring_LTM + yday + I(yday^2) + (1|id) + (1|year)+ (1|tracking_group_id_spring), data=subset(data, season=="spring migration"))
s4 <- glmer(lat.speed ~ SNWZ_scale + NDVI_scale + age + age_oldest_spring_LTM + yday + I(yday^2) + (1|id) + (1|year)+ (1|tracking_group_id_spring), data=subset(data, season=="spring migration"))
s5 <- glmer(lat.speed ~ SNWZ_scale + NDVI_scale + earlyexp + age_oldest_spring_LTM + yday + I(yday^2) + (1|id) + (1|year)+ (1|tracking_group_id_spring), data=subset(data, season=="spring migration"))
s6 <- glmer(lat.speed ~ SNWZ_scale + NDVI_scale + earlyexp + age + yday + I(yday^2) + (1|id) + (1|year)+ (1|tracking_group_id_spring), data=subset(data, season=="spring migration"))
s7 <- glmer(lat.speed ~ SNWZ_scale + NDVI_scale + earlyexp + age + age_oldest_spring_LTM + (1|id) + (1|year)+ (1|tracking_group_id_spring), data=subset(data, season=="spring migration"))
s8 <- glmer(lat.speed ~ SNWZ_scale + NDVI_scale + earlyexp + age + age_oldest_spring_LTM + yday + (1|id) + (1|year)+ (1|tracking_group_id_spring), data=subset(data, season=="spring migration"))
s9 <- glmer(lat.speed ~ SNWZ_scale + NDVI_scale + earlyexp + age + age_oldest_spring_LTM + I(yday^2) + (1|id) + (1|year)+ (1|tracking_group_id_spring), data=subset(data, season=="spring migration"))
s10 <- glmer(lat.speed ~ SNWZ_scale*earlyexp + NDVI_scale*earlyexp + age + age_oldest_spring_LTM + yday + (1|id) + (1|year)+ (1|tracking_group_id_spring), data=subset(data, season=="spring migration"))
s11 <- glmer(lat.speed ~ SNWZ_scale*age + NDVI_scale*age + earlyexp + age_oldest_spring_LTM + yday + (1|id) + (1|year)+ (1|tracking_group_id_spring), data=subset(data, season=="spring migration"))
s12 <- glmer(lat.speed ~ SNWZ_scale*age_oldest_spring_LTM + NDVI_scale*age_oldest_spring_LTM + earlyexp + age + yday + (1|id) + (1|year)+ (1|tracking_group_id_spring), data=subset(data, season=="spring migration"))
s13 <- glmer(lat.speed ~ SNWZ_scale*earlyexp*age_oldest_spring_LTM + NDVI_scale*earlyexp*age_oldest_spring_LTM + age + yday + (1|id) + (1|year)+ (1|tracking_group_id_spring), data=subset(data, season=="spring migration"))
s14 <- glmer(lat.speed ~ SNWZ_scale*earlyexp*age + NDVI_scale*earlyexp*age + age_oldest_spring_LTM + yday + (1|id) + (1|year)+ (1|tracking_group_id_spring), data=subset(data, season=="spring migration"))
s15 <- glmer(lat.speed ~ SNWZ_scale*earlyexp*age + NDVI_scale*earlyexp*age + yday + (1|id) + (1|year)+ (1|tracking_group_id_spring), data=subset(data, season=="spring migration"))
s16 <- glmer(lat.speed ~ SNWZ_scale*earlyexp*age_oldest_spring_LTM + NDVI_scale*earlyexp*age_oldest_spring_LTM + yday + (1|id) + (1|year)+ (1|tracking_group_id_spring), data=subset(data, season=="spring migration"))
s17 <- glmer(lat.speed ~ SNWZ_scale*earlyexp + SNWZ_scale*age + NDVI_scale*earlyexp + NDVI_scale*age + yday + (1|id) + (1|year)+ (1|tracking_group_id_spring), data=subset(data, season=="spring migration"))
s18 <- glmer(lat.speed ~ yday + (1|id) + (1|year)+ (1|tracking_group_id_spring), data=subset(data, season=="spring migration"))
s19 <- glmer(lat.speed ~ SNWZ_scale*earlyexp*age + yday + (1|id) + (1|year)+ (1|tracking_group_id_spring), data=subset(data, season=="spring migration"))
s20 <- glmer(lat.speed ~ NDVI_scale*earlyexp*age + yday + (1|id) + (1|year)+ (1|tracking_group_id_spring), data=subset(data, season=="spring migration"))
s21 <- glmer(lat.speed ~ SNWZ_scale*age + yday + (1|id) + (1|year)+ (1|tracking_group_id_spring), data=subset(data, season=="spring migration"))
s22 <- glmer(lat.speed ~ NDVI_scale*age + yday + (1|id) + (1|year)+ (1|tracking_group_id_spring), data=subset(data, season=="spring migration"))
s23 <- glmer(lat.speed ~ SNWZ_scale*earlyexp + yday + (1|id) + (1|year)+ (1|tracking_group_id_spring), data=subset(data, season=="spring migration"))
s24 <- glmer(lat.speed ~ NDVI_scale*earlyexp + yday + (1|id) + (1|year)+ (1|tracking_group_id_spring), data=subset(data, season=="spring migration"))
s25 <- glmer(lat.speed ~ SNWZ_scale + yday + (1|id) + (1|year)+ (1|tracking_group_id_spring), data=subset(data, season=="spring migration"))
s26 <- glmer(lat.speed ~ NDVI_scale + yday + (1|id) + (1|year)+ (1|tracking_group_id_spring), data=subset(data, season=="spring migration"))
s27 <- glmer(lat.speed ~ SNWZ_scale + NDVI_scale + yday + (1|id) + (1|year)+ (1|tracking_group_id_spring), data=subset(data, season=="spring migration"))

#AIC table
table <- data.frame(
  formula=c(s1formula, s2formula, s3formula, s4formula, s5formula, s6formula, s7formula, s8formula, s9formula, s10formula, s11formula, s12formula, s13formula, s14formula, s15formula,s16formula, s17formula, s18formula, s19formula, s20formula, s21formula, s22formula, s23formula, s24formula, s25formula, s26formula, s27formula, s28formula, s29formula), 
  AIC=c(AIC(s1), AIC(s2), AIC(s3), AIC(s4), AIC(s5), AIC(s6), AIC(s7), AIC(s8), AIC(s9), AIC(s10), AIC(s11), AIC(s12), AIC(s13), AIC(s14), AIC(s15), AIC(s16), AIC(s17), AIC(s18), AIC(s19), AIC(s20), AIC(s21), AIC(s22), AIC(s23), AIC(s24), AIC(s25), AIC(s26), AIC(s27)))
table <- table %>% arrange(AIC)

#Response coefficients and confidence intervals
coefs <- data.frame(coef(summary(s15))); coefs$p <- 2 * (1 - pnorm(abs(coefs$t.value))); confs <- confint(s15); coefs <- cbind(coefs, confs[5:7,]); coefs

#Plot response lines for snow
plot(effects::predictorEffects(s15, c("SNWZ_scale"), xlevels=list(age=c(1:5))),
     main=effect, 
     lines=list(multiline=TRUE, z.var="age", col=mycols, lwd=4),
     confint=list(style="auto"), 
     axes=list(grid=FALSE, y=list(type="response")),
     ylab="northward migration speed", 
     xlab="snow depth")

##############################################
# 2. Autumn model selection, subadults #####
##############################################
#Subset data to age 1 birds
data1 = subset(data, age==1)

#Formulas
f1formula <- "SNWZ_scale + NDVI_scale + earlyexp + age_oldest_fall_LTM + yday + I(yday^2) + (1|id) + (1|group) + (1|year)"
f2formula <- "NDVI_scale + earlyexp + age_oldest_fall_LTM + yday + I(yday^2) + (1|id) + (1|group) + (1|year)"
f3formula <- "SNWZ_scale + earlyexp + age_oldest_fall_LTM + yday + I(yday^2) + (1|id) + (1|group) + (1|year)"
f4formula <- "SNWZ_scale + NDVI_scale + age_oldest_fall_LTM + yday + I(yday^2) + (1|id) + (1|group) + (1|year)"
f5formula <- "SNWZ_scale + NDVI_scale + earlyexp + yday + I(yday^2) + (1|id) + (1|group) + (1|year)"
f6formula <- "SNWZ_scale + NDVI_scale + earlyexp + age_oldest_fall_LTM + (1|id) + (1|group) + (1|year)"
f7formula <- "SNWZ_scale + NDVI_scale + earlyexp + age_oldest_fall_LTM + yday + (1|id) + (1|group) + (1|year)"
f8formula <- "SNWZ_scale + NDVI_scale + earlyexp +  age_oldest_fall_LTM + I(yday^2) + (1|id) + (1|group) + (1|year)"
f9formula <- "SNWZ_scale* earlyexp + NDVI_scale* earlyexp + age_oldest_fall_LTM + yday + (1|id) + (1|group) + (1|year)"
f10formula <- "SNWZ_scale* age_oldest_fall_LTM + NDVI_scale* age_oldest_fall_LTM + earlyexp + yday + (1|id) + (1|group) + (1|year)"
f11formula <- "SNWZ_scale* earlyexp* age_oldest_fall_LTM + NDVI_scale* earlyexp* age_oldest_fall_LTM + yday + (1|id) + (1|group) + (1|year)"
f12formula <- "SNWZ_scale* earlyexp + NDVI_scale* earlyexp + yday + (1|id) + (1|group) + (1|year)"
f13formula <- "yday + (1|id) + (1|group) + (1|year)"
f14formula <- "SNWZ_scale* earlyexp + yday + (1|id) + (1|group) + (1|year)"
f15formula <- "NDVI_scale* earlyexp + yday + (1|id) + (1|group) + (1|year)"
f16formula <- "SNWZ_scale* age_oldest_fall_LTM + yday + (1|id) + (1|group) + (1|year)"
f17formula <- "NDVI_scale* age_oldest_fall_LTM + yday + (1|id) + (1|group) + (1|year)"
f18formula <- "SNWZ_scale + yday + (1|id) + (1|group) + (1|year)"
f19formula <- "NDVI_scale + yday + (1|id) + (1|group) + (1|year)"
f20formula <- "SNWZ_scale + NDVI_scale + yday + (1|id) + (1|group) + (1|year)"

#Models
f1 <- glmer(lat.speed ~ SNWZ_scale + NDVI_scale + earlyexp + age_oldest_fall_LTM + yday + I(yday^2) + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data1, season=="fall migration"))
f2 <- glmer(lat.speed ~ NDVI_scale + earlyexp + age_oldest_fall_LTM + yday + I(yday^2) + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data1, season=="fall migration"))
f3 <- glmer(lat.speed ~ SNWZ_scale + earlyexp + age_oldest_fall_LTM + yday + I(yday^2) + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data1, season=="fall migration"))
f4 <- glmer(lat.speed ~ SNWZ_scale + NDVI_scale + age_oldest_fall_LTM + yday + I(yday^2) + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data1, season=="fall migration"))
f5 <- glmer(lat.speed ~ SNWZ_scale + NDVI_scale + earlyexp + yday + I(yday^2) + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data1, season=="fall migration"))
f6 <- glmer(lat.speed ~ SNWZ_scale + NDVI_scale + earlyexp + age_oldest_fall_LTM + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data1, season=="fall migration"))
f7 <- glmer(lat.speed ~ SNWZ_scale + NDVI_scale + earlyexp + age_oldest_fall_LTM + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data1, season=="fall migration"))
f8 <- glmer(lat.speed ~ SNWZ_scale + NDVI_scale + earlyexp + age_oldest_fall_LTM + I(yday^2) + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data1, season=="fall migration"))
f9 <- glmer(lat.speed ~ SNWZ_scale*earlyexp + NDVI_scale*earlyexp + age_oldest_fall_LTM + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data1, season=="fall migration"))
f10 <- glmer(lat.speed ~ SNWZ_scale*age_oldest_fall_LTM + NDVI_scale*age_oldest_fall_LTM + earlyexp + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data1, season=="fall migration"))
f11 <- glmer(lat.speed ~ SNWZ_scale*earlyexp*age_oldest_fall_LTM + NDVI_scale*earlyexp*age_oldest_fall_LTM + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data1, season=="fall migration"))
f12 <- glmer(lat.speed ~ SNWZ_scale*earlyexp + NDVI_scale*earlyexp + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data1, season=="fall migration"))
f13 <- glmer(lat.speed ~ yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data1, season=="fall migration"))
f14 <- glmer(lat.speed ~ SNWZ_scale*earlyexp + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data1, season=="fall migration"))
f15 <- glmer(lat.speed ~ NDVI_scale*earlyexp + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data1, season=="fall migration"))
f16 <- glmer(lat.speed ~ SNWZ_scale*age_oldest_fall_LTM + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data1, season=="fall migration"))
f17 <- glmer(lat.speed ~ NDVI_scale*age_oldest_fall_LTM + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data1, season=="fall migration"))
f18 <- glmer(lat.speed ~ SNWZ_scale + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data1, season=="fall migration"))
f19 <- glmer(lat.speed ~ NDVI_scale + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data1, season=="fall migration"))
f20 <- glmer(lat.speed ~ SNWZ_scale + NDVI_scale + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data1, season=="fall migration"))

#AIC table
table <- data.frame(
  formula=c(f1formula, f2formula, f3formula, f4formula, f5formula, f6formula, f7formula, f8formula, f9formula, f10formula, f11formula,
            f12formula, f13formula, f14formula, f15formula,f16formula,f17formula,f18formula,f19formula, f20formula, f21formula, f22formula),
  AIC=c(AIC(f1), AIC(f2), AIC(f3), AIC(f4), AIC(f5), AIC(f6), AIC(f7), AIC(f8), AIC(f9), AIC(f10), AIC(f11), AIC(f12), AIC(f13), AIC(f14),
        AIC(f15), AIC(f16), AIC(f17), AIC(f18), AIC(f19), AIC(f20)))
table <- table %>% arrange(AIC)


#Response coefficients and confidence intervals
coefs <- data.frame(coef(summary(f11))); coefs$p <- 2 * (1 - pnorm(abs(coefs$t.value))); confs <- confint(f11); coefs <- cbind(coefs, confs[5:7,]); coefs

#Plot response lines for snow
plot(effects::predictorEffects(f11, c("SNWZ_scale")),
     main=effect, 
     lines=list(multiline=TRUE, z.var="age_oldest_fall_LTM", col=mycols, lwd=4),
     confint=list(style="auto"), 
     axes=list(grid=FALSE, y=list(type="response")),
     ylab="southward migration speed", 
     xlab="snow depth", 
     lattice=list(layout=c(3, 1)))


###########################################
# 3. Autumn model selection, all ages #####
###########################################

#Formulas
f1formula <- "SNWZ_scale + NDVI_scale + earlyexp + age + age_oldest_fall_LTM + yday + I(yday^2) + (1|id) + (1|group) + (1|year)"
f2formula <- "NDVI_scale + earlyexp + age + age_oldest_fall_LTM + yday + I(yday^2) + (1|id) + (1|group) + (1|year)"
f3formula <- "SNWZ_scale + earlyexp + age + age_oldest_fall_LTM + yday + I(yday^2) + (1|id) + (1|group) + (1|year)"
f4formula <- "SNWZ_scale + NDVI_scale + age + age_oldest_fall_LTM + yday + I(yday^2) + (1|id) + (1|group) + (1|year)"
f5formula <- "SNWZ_scale + NDVI_scale + earlyexp + age_oldest_fall_LTM + yday + I(yday^2) + (1|id) + (1|group) + (1|year)"
f6formula <- "SNWZ_scale + NDVI_scale + earlyexp + age + yday + I(yday^2) + (1|id) + (1|group) + (1|year)"
f7formula <- "SNWZ_scale + NDVI_scale + earlyexp + age + age_oldest_fall_LTM + (1|id) + (1|group) + (1|year)"
f8formula <- "SNWZ_scale + NDVI_scale + earlyexp + age + age_oldest_fall_LTM + yday + (1|id) + (1|group) + (1|year)"
f9formula <- "SNWZ_scale + NDVI_scale + earlyexp + age + age_oldest_fall_LTM + I(yday^2) + (1|id) + (1|group) + (1|year)"
f10formula <- "SNWZ_scale* earlyexp + NDVI_scale* earlyexp + age + age_oldest_fall_LTM + yday + (1|id) + (1|group) + (1|year)"
f11formula <- "SNWZ_scale* age + NDVI_scale* age + earlyexp + age_oldest_fall_LTM + yday + (1|id) + (1|group) + (1|year)"
f12formula <- "SNWZ_scale* age_oldest_fall_LTM + NDVI_scale* age_oldest_fall_LTM + earlyexp + age + yday + (1|id) + (1|group) + (1|year)"
f13formula <- "SNWZ_scale* earlyexp* age_oldest_fall_LTM + NDVI_scale* earlyexp* age_oldest_fall_LTM + age + yday + (1|id) + (1|group) + (1|year)"
f14formula <- "SNWZ_scale* earlyexp* age + NDVI_scale* earlyexp* age + age_oldest_fall_LTM + yday + (1|id) + (1|group) + (1|year)"
f15formula <- "SNWZ_scale* earlyexp* age + NDVI_scale* earlyexp* age + yday + (1|id) + (1|group) + (1|year)"
f16formula <- "SNWZ_scale* earlyexp* age_oldest_fall_LTM + NDVI_scale* earlyexp* age_oldest_fall_LTM + yday + (1|id) + (1|group) + (1|year)"
f17formula <- "SNWZ_scale* earlyexp + SNWZ_scale* age + NDVI_scale* earlyexp + NDVI_scale* age + yday + (1|id) + (1|group) + (1|year)"
f18formula <- "yday + (1|id) + (1|group) + (1|year)"
f19formula <- "SNWZ_scale* earlyexp* age + yday + (1|id) + (1|group) + (1|year)"
f20formula <- "NDVI_scale* earlyexp* age + yday + (1|id) + (1|group) + (1|year)"
f21formula <- "SNWZ_scale* age + yday + (1|id) + (1|group) + (1|year)"
f22formula <- "NDVI_scale* age + yday + (1|id) + (1|group) + (1|year)"
f23formula <- "SNWZ_scale* earlyexp + yday + (1|id) + (1|group) + (1|year)"
f24formula <- "NDVI_scale* earlyexp + yday + (1|id) + (1|group) + (1|year)"
f25formula <- "SNWZ_scale + yday + (1|id) + (1|group) + (1|year)"
f26formula <- "NDVI_scale + yday + (1|id) + (1|group) + (1|year)"
f27formula <- "SNWZ_scale + NDVI_scale + yday + (1|id) + (1|group) + (1|year)"

#Models
f1 <- glmer(lat.speed ~ SNWZ_scale + NDVI_scale + earlyexp + age + age_oldest_fall_LTM + yday + I(yday^2) + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data, season=="fall migration"))
f2 <- glmer(lat.speed ~ NDVI_scale + earlyexp + age + age_oldest_fall_LTM + yday + I(yday^2) + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data, season=="fall migration"))
f3 <- glmer(lat.speed ~ SNWZ_scale + earlyexp + age + age_oldest_fall_LTM + yday + I(yday^2) + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data, season=="fall migration"))
f4 <- glmer(lat.speed ~ SNWZ_scale + NDVI_scale + age + age_oldest_fall_LTM + yday + I(yday^2) + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data, season=="fall migration"))
f5 <- glmer(lat.speed ~ SNWZ_scale + NDVI_scale + earlyexp + age_oldest_fall_LTM + yday + I(yday^2) + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data, season=="fall migration"))
f6 <- glmer(lat.speed ~ SNWZ_scale + NDVI_scale + earlyexp + age + yday + I(yday^2) + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data, season=="fall migration"))
f7 <- glmer(lat.speed ~ SNWZ_scale + NDVI_scale + earlyexp + age + age_oldest_fall_LTM + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data, season=="fall migration"))
f8 <- glmer(lat.speed ~ SNWZ_scale + NDVI_scale + earlyexp + age + age_oldest_fall_LTM + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data, season=="fall migration"))
f9 <- glmer(lat.speed ~ SNWZ_scale + NDVI_scale + earlyexp + age + age_oldest_fall_LTM + I(yday^2) + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data, season=="fall migration"))
f10 <- glmer(lat.speed ~ SNWZ_scale*earlyexp + NDVI_scale*earlyexp + age + age_oldest_fall_LTM + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data, season=="fall migration"))
f11 <- glmer(lat.speed ~ SNWZ_scale*age + NDVI_scale*age + earlyexp + age_oldest_fall_LTM + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data, season=="fall migration"))
f12 <- glmer(lat.speed ~ SNWZ_scale*age_oldest_fall_LTM + NDVI_scale*age_oldest_fall_LTM + earlyexp + age + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data, season=="fall migration"))
f13 <- glmer(lat.speed ~ SNWZ_scale*earlyexp*age_oldest_fall_LTM + NDVI_scale*earlyexp*age_oldest_fall_LTM + age + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data, season=="fall migration"))
f14 <- glmer(lat.speed ~ SNWZ_scale*earlyexp*age + NDVI_scale*earlyexp*age + age_oldest_fall_LTM + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data, season=="fall migration"))
f15 <- glmer(lat.speed ~ SNWZ_scale*earlyexp*age + NDVI_scale*earlyexp*age + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data, season=="fall migration"))
f16 <- glmer(lat.speed ~ SNWZ_scale*earlyexp*age_oldest_fall_LTM + NDVI_scale*earlyexp*age_oldest_fall_LTM + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data, season=="fall migration"))
f17 <- glmer(lat.speed ~ SNWZ_scale*earlyexp + SNWZ_scale*age + NDVI_scale*earlyexp + NDVI_scale*age + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data, season=="fall migration"))
f18 <- glmer(lat.speed ~ yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data, season=="fall migration"))
f19 <- glmer(lat.speed ~ SNWZ_scale*earlyexp*age + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data, season=="fall migration"))
f20 <- glmer(lat.speed ~ NDVI_scale*earlyexp*age + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data, season=="fall migration"))
f21 <- glmer(lat.speed ~ SNWZ_scale*age + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data, season=="fall migration"))
f22 <- glmer(lat.speed ~ NDVI_scale*age + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data, season=="fall migration"))
f23 <- glmer(lat.speed ~ SNWZ_scale*age + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data, season=="fall migration"))
f24 <- glmer(lat.speed ~ NDVI_scale*age + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data, season=="fall migration"))
f25 <- glmer(lat.speed ~ SNWZ_scale + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data, season=="fall migration"))
f26 <- glmer(lat.speed ~ NDVI_scale + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data, season=="fall migration"))
f27 <- glmer(lat.speed ~ SNWZ_scale + NDVI_scale + yday + (1|id) + (1|year)+ (1|tracking_group_id_fall), data=subset(data, season=="fall migration"))

#AIC table
table <- data.frame(
  formula=c(f1formula, f2formula, f3formula, f4formula, f5formula, f6formula, f7formula, f8formula, f9formula, f10formula, f11formula,
            f12formula, f13formula, f14formula, f15formula,f16formula,f17formula,f18formula,f19formula, f20formula, f21formula, f22formula, f23formula, f24formula, f25formula, f26formula, f27formula, f28formula, f29formula, f30formula), 
  AIC=c(AIC(f1), AIC(f2), AIC(f3), AIC(f4), AIC(f5), AIC(f6), AIC(f7), AIC(f8), AIC(f9), AIC(f10), AIC(f11), AIC(f12), AIC(f13), AIC(f14),
        AIC(f15), AIC(f16), AIC(f17), AIC(f18), AIC(f19), AIC(f20), AIC(f21), AIC(f22), AIC(f23), AIC(f24), AIC(f25), AIC(f26), AIC(f27)))
table <- table %>% arrange(AIC)

#Response coefficients and confidence intervals
coefs <- data.frame(coef(summary(f15))); coefs$p <- 2 * (1 - pnorm(abs(coefs$t.value))); confs <- confint(f15); coefs <- cbind(coefs, confs[5:7,]); coefs

#Plot response lines for snow
plot(effects::predictorEffects(f15, c("SNWZ_scale"), xlevels=list(age=c(1:5))),
     main=effect, 
     lines=list(multiline=TRUE, z.var="age", col=mycols, lwd=4),
     confint=list(style="auto"), 
     axes=list(grid=FALSE, y=list(type="response")),
     ylab="southward migration speed", 
     xlab="snow depth", 
     lattice=list(layout=c(3, 1)))


###################################
# 4. Cumulative snow exposure #####
###################################

snow_data <- data %>% 
  filter(age >0, earlyexp != "ultralight") %>%
  group_by(ID, year, season) %>%
  mutate(cum_snow = sum(snow*row_number()/n())) %>%
  filter(row_number()==n())

cum_snow_model <- lmer(cum_snow ~ age + AWSSI + (1|id), data=snow_data)

coefs <- data.frame(coef(summary(cum_snow_model))); coefs$p <- 2 * (1 - pnorm(abs(coefs$t.value))); confs <- confint(cum_snow_model); coefs <- cbind(coefs, confs[5:7,]); coefs

