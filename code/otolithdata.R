#otolith data from Levi

library(tidyverse)
library(readxl)

library(lme4)
library(lmerTest)


library(MuMIn)
library(car)
library(effects)

#Note: in Lewis et 2021, growth rate (msgr) was log10 transformed and age corrected before use in models

#msgr: reconstructed mean somatic growth rate of the final 14 days prior to capture
#Recent growth rate (G) of a given fish was calculated as the log10-transformed mean somatic growth rate: 
# G = log10(1/n sum(yi))
#where yi is the somatic growth rate (mm d−1) for day i, and n is the number of days included in the 
#growth interval (n = 14). The log10 transformation was used to homogenize the variance across age groups. 
#Adjusted growth rates (Ga) were estimated using the residuals of the intrinsic model (see Section 2.6).

#Growth rates were first modeled as individual and combined functions of 2 
#intrinsic factors, age-at-capture (herein ‘age’) and prior (larval) growth rate (0− 30 dph), 
#that often explain significant variation in the growth rates of young fishes (Hinrichsen et al. 2010, 
#Schismenou et al. 2014, 2016). For example, intrinsic models account for the inherent dependence 
#structure of otolith increments with age (ontogenetic ef fect) and previous growth rates 
#(e.g. individual variation or increment autocorrelation) (Morrongiello & Thresher 2015, Barrow et al. 2018). 
#Adjusted growth rate (Ga) for each fish was calculated as the residual variation in growth after accounting 
#for the selected intrinsic model (Shima & Swearer 2019).

#Prior growth (R2 = 0.18) and its additive (R2 = 0.63) and interactive (R2 = 0.64) effects with age contributed 
#little to the fit of intrinsic growth models (Fig. 3B), indicating that marginal growth rates were largely 
#independent of early growth history. 

#import data
otoliths = read_excel("data/otolith data_for_export.xlsx", sheet = "data") %>%
  mutate(Year = year(date), Month = month(date))

ggplot(otoliths, aes(x = as.factor(Year), y = log(msgr, 10))) + geom_boxplot()


#just the summer and fall data
falloto = filter(otoliths, Month %in% c(6,7,8,9, 10))

ggplot(falloto, aes(x = as.factor(Year), y = msgr)) + geom_boxplot()

ggplot(falloto, aes(x = as.factor(Year), y = msgr)) + geom_boxplot()+
  facet_wrap(~edsmstratum)

#add number of samples
otonums = group_by(falloto, Year, edsmstratum) %>%
  summarize(N = n())

ggplot(falloto, aes(x = as.factor(Year), y = msgr)) + geom_boxplot()+
  facet_wrap(~edsmstratum)+ geom_label(data = otonums, aes(y = 0, label = N))

#some relationship with salinity, similar to Levi's previous work
ggplot(falloto, aes(x = log(sal), y = msgr)) + geom_point(aes(color = as.factor(Year)))+
  geom_smooth(method = "lm")

#2011 is just WAY higher than the other years
ggplot(falloto, aes(x = temp, y = msgr)) + geom_point(aes(color = as.factor(Year)))+
  geom_smooth(method = "lm")


#ok so to do the age correction, first I model based on age anc calculate residuals

ggplot(falloto,  aes(x = age, y = log(msgr,10))) + geom_point(aes(color = as.factor(Year)))+
  geom_smooth(method = "lm")

falloto = mutate(falloto, msgrlog10 = log(msgr, 10))

agerate = lm( msgrlog10 ~ age, data = falloto)
summary(agerate)
plot(agerate)
falloto$resid = residuals(agerate)

#plot the age-corrected growth rate
#negative effect of temperature, also found by Lewis et al 2021
ggplot(falloto, aes(x = temp, y = resid)) + geom_point(aes(color = as.factor(Year)))+
  geom_smooth(method = "lm")+
  ylab("log-transformed, age-corrected, 14-day growth rate")+
  xlab("water temperature")

#there is a fair amount of data in teh lower sacramento region, not much from other areas
ggplot(filter(falloto, edsmstratum == "Lower Sacramento"), aes(x = temp, y = resid)) + geom_point(aes(color = as.factor(Year)))+
  geom_smooth(method = "lm")+
  ylab("log-transformed, age-corrected, 14-day growth rate")+
  xlab("water temperature")

#does a non-linear model fit better?
ggplot(filter(falloto, edsmstratum == "Lower Sacramento"), aes(x = log(sal), y = resid)) + geom_point(aes(color = as.factor(Year)))+
  geom_smooth()+
  ylab("log-transformed, age-corrected, 14-day growth rate")+
  xlab("log-transformed salinity")

ggplot(falloto, aes(x = log(sal), y = resid)) + geom_point(aes(color = as.factor(Year)))+
  geom_smooth(method = "lm")

#look by year - no clear trend by water year type.
ggplot(falloto, aes(x = as.factor(Year), y = resid)) + geom_boxplot()+ 
  ylab("log-transformed, age-corrected, 14-day growth rate")

#plot by region - Cache Slough is a bit lower, but other regions all overlap
ggplot(falloto, aes(x = edsmstratum, y = resid)) + geom_boxplot()+ 
  ylab("log-transformed, age-corrected, 14-day growth rate")

#data are too sparse to really see trends
ggplot(falloto, aes(x = as.factor(Year), y = resid)) + geom_boxplot()+
  facet_wrap(~edsmstratum)+ ylab("log-transformed, age-corrected, 14-day growth rate")

ggplot(falloto, aes(x = log(sal), y = resid)) + geom_point(aes(color = as.factor(Year)))+
  geom_smooth(method = "lm")+
 # facet_wrap(~edsmstratum)+
  ylab("log-transformed, age-corrected, 14-day growth rate")+
  xlab("Log-transformed salinity")

ggplot(falloto, aes(x = log(sal), y = resid)) + geom_point(aes(color = edsmstratum))+
  geom_smooth(method = "lm")+
  facet_wrap(~as.factor(Year))

############
#add X2

load("Data/Dayflow_allw2023.RData")
yrs = read_csv("data/wtryrtype.csv")
DF = filter(Dayflow, Year > 2010) %>%
  mutate(Mo = month(Date),WaterYear = case_when(Mo %in% c(10,11,12) ~Year+1, TRUE ~Year),
         Year2 = case_when(Mo %in% c(12) ~Year+1,TRUE ~Year))

#while the water year ends Sep 30, we want october and november to be with the previous water year

DF = mutate(DF, Season = case_when(Mo %in% c(3,4,5) ~ "Spring",
                                   Mo %in% c(6,7,8) ~ "Summer",
                                   Mo %in% c(9,10,11) ~ "Fall",
                                   TRUE ~ "Winter"))



falloto = left_join(falloto, DF, by = c("date" = "Date", "Year" = "Year2")) %>%
  mutate(DOY = yday(date), Yearf = as.factor(Year))

#very weak trend with X2, probably not significant
ggplot(falloto, aes(x = X2, y = resid)) + geom_point(aes(color = as.factor(Year)))+
  geom_smooth(method = "lm")+
  #facet_wrap(~as.factor(edsmstratum))+
  ylab("log-transformed, age-corrected, 14-day growth rate")+
  xlab("X2")

#if i limit it to the sacramento river region (where I have the most data) there is reallynothing there at all.
ggplot(filter(falloto, edsmstratum == "Lower Sacramento"), aes(x = X2, y = resid)) + geom_point(aes(color = as.factor(Year)))+
  geom_smooth(method = "lm")+
  #facet_wrap(~as.factor(edsmstratum))+
  ylab("log-transformed, age-corrected, 14-day growth rate")+
  xlab("X2")


ggplot(falloto, aes(x = DOY, y = resid)) + geom_point(aes(color = as.factor(Year)))+
  geom_smooth(method = "lm")+
  #facet_wrap(~as.factor(edsmstratum))+
  ylab("log-transformed, age-corrected, 14-day growth rate")+
  xlab("Day of year")


#Let's just dredge through all possible models. That's always useful.


#scale and center everythign
falloto = mutate(falloto, X2s = scale(X2), temps = scale(temp), ages = scale(age))

otoglobal = lm(resid ~ X2s+temps+edsmstratum+Yearf+DOY+ages, data = falloto, na.action = "na.fail") 
summary(otoglobal)
vif(otoglobal)
#right, DOY and age are super auto=coorelated. Duh and X2.
otoglobal2 = lm(resid ~ X2s+temps+edsmstratum+Yearf+ages, data = falloto, na.action = "na.fail") 
summary(otoglobal2)
vif(otoglobal2)


#X2 is super correlated with Year
otoglobal3 = lm(resid ~ X2s*edsmstratum+temps+ages, data = falloto, na.action = "na.fail") 
summary(otoglobal3)
vif(otoglobal3)


dredge(otoglobal3)

#Interaction isn't inthe top model 

otos = lm(resid ~ X2s+edsmstratum+temps+ages, data = falloto, na.action = "na.fail") 
summary(otos)
vif(otos)
#why is age  important if it's supposidly the age-corrected growth rate? something to look into. 

#diagnostic plots
plot(otos)

#effects plots
plot(allEffects(otos))

#The fact that the interaction doesn't come out as important makes me think it's not really picking
#up an X2 effect. The sampling is really just too unbalanced. And X2 is so correlated with year....

#maybe I can add year as a random effect?

otos2 = lmer(resid ~ X2s+edsmstratum+temps+ages + (1|Yearf), data = falloto, na.action = "na.fail") 
summary(otos2)
#yeah, with year as a random effect the impact of X2 goes away.
