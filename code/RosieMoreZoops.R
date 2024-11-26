#Linear models of pesudodiaptomus versus salinity at belden's and/or fall X2

library(tidyverse)
library(lme4)
library(lmerTest)
library(effects)
library(zooper)
library(glmmTMB)
library(DHARMa)
library(conflicted)
library(sf)

conflicts_prefer(lmerTest::lmer)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
#question - should i seperate adults and copepedites? or combine theM?

#other question - what about P. marinus? Do I just not wory about the fact that we don't have juveniels to species?

#### Load Data ####################
yrs = read_csv("data/wtryrtype.csv") %>%
  rename(YrType = `Yr-type`, Year = WY)

load("data/SMSCGRegions.RData")
load("data/Dayflow_allw2023.RData")
DF = filter(Dayflow, Year >2009) %>%
  mutate(Month = month(Date)) %>%
  select(Date, Month, Year, OUT, X2)

pseudos = Zoopsynther(Data_type = "Community", Years = c(2011:2023), 
                      Sources = c("EMP", "FMWT", "STN", "20mm", "DOP"),
                      Size_class = "Meso",
                      Month = c(6:10)) %>%
  filter(Genus == "Pseudodiaptomus", Lifestage != "Larva")

pseudo = group_by(pseudos, SampleID, Latitude, Station, Longitude, SalSurf, TowType, Source, Date, Year) %>%
  summarize(CPUE = sum(CPUE)) %>%
  left_join(yrs) %>%
  left_join(DF) %>%
  mutate(logCPUE = log(CPUE+1))

pseudoreg = pseudo %>%
  filter(!is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(Regions))%>%
  st_join(Regions) %>%
  st_drop_geometry() %>%
  filter(!is.na(Region))

hist(pseudoreg$CPUE)
hist(log(pseudoreg$CPUE))
#actually not too bad, but I should test whether we need a zero-inflation term anyway

ggplot(pseudoreg, aes(x = X2, y = logCPUE))+
  geom_point(aes(color = Month))+
  facet_wrap(~Region)+
  geom_smooth(method = "lm")

######## start with X2 ############# 
lm1 = lmer(logCPUE ~ X2*Region + (1|Month) + (1|Year), data = pseudoreg)
summary(lm1)
plot(lm1)
plot(simulateResiduals(lm1))
plot(allEffects(lm1))
testZeroInflation(lm1)
#outlier test significant, KS test significant. But it doesn't look terrible.
#also zero inflated, not surprsiing

#what about a zero inflation model?

zm1 = glmmTMB(logCPUE ~ X2*Region + (1|Month) + (1|Year), 
              ziformula =~ SalSurf,data = pseudoreg) 

summary(zm1)
plot(simulateResiduals(zm1))
testZeroInflation(zm1)
plot(allEffects(zm1))
#still zero inflated, not sur what to do about that
#but otherwise an OK model

#try a zero inflated negative binomial model on CPUE isntead
zm2 = glmmTMB(CPUE ~ X2*Region + (1|Month) + (1|Year), 
              ziformula =~ SalSurf, family=nbinom2, data = pseudoreg) 

summary(zm2)
plot(simulateResiduals(zm2))
testZeroInflation(zm2)
plot(allEffects(zm2))
#OK! no longer zero inflated. I think we are OK!

#sometime the negative binomial is enough without the zip term

zm3 = glmmTMB(CPUE ~ X2*Region + (1|Month) + (1|Year),  family=nbinom2, data = pseudoreg) 
summary(zm3)
plot(simulateResiduals(zm3))
testZeroInflation(zm3)
plot(allEffects(zm3))
#yeah, just leave out the zero inflation so long as you have the negative binomial. 
#all of these results match our expectations pretty darn well anyway.

##########now try salinity at belden's #################
BDL = cdec_query("BDL", 
                sensors = 100,
                start.date = as.Date("2011-06-01"), end.date =  as.Date("2024-11-01"))


BDLx = mutate(BDL, Salinity = ec2pss(Value/1000, 25), Date = date(DateTime), Year = year(Date)) %>% 
  group_by(Date, Year) %>%
  summarize(Salinity = mean(Salinity, na.rm =T))

pseudo2 = left_join(pseudoreg, BDLx)

bdlm1 = glmmTMB(CPUE ~ Salinity*Region + (1|Month) + (1|Year),  family=nbinom2, data = pseudo2) 
summary(bdlm1)
plot(simulateResiduals(bdlm1))
testZeroInflation(bdlm1)
plot(allEffects(bdlm1))
#huh, very similar to the X2 results. Probably because they are correlated. 

#Does lower salinity at Beldens' give you things that X2 doesn't?

bdlm2 = glmmTMB(CPUE ~ X2*Region +Salinity*Region + (1|Month) + (1|Year),  family=nbinom2, data = pseudo2) 
summary(bdlm2)
plot(simulateResiduals(bdlm2))
testZeroInflation(bdlm2)
plot(allEffects(bdlm2))
#ok, so lower salintiy in the marsh gives you more of a change in zoops in the marsh than in the other regions,
#but even suisun bay has some impcat above the X2 impact. I think. 