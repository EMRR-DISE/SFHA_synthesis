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
library(emmeans)

emtrends(bdlm2, pairwise ~ Region, var = "X2")
emtrends(bdlm2, pairwise ~ Region, var = "Salinity")

#but is this too colinear to even work?
library(performance)

#ok, this istn' working
check_collinearity(bdlm2)
# Model has interaction terms. VIFs might be inflated.
# You may check multicollinearity among predictors of a model without interaction terms.

bdlm2z = glmmTMB(CPUE ~ X2+Region +Salinity+ (1|Month) + (1|Year),  family=nbinom2, data = pseudo2) 
summary(bdlm2z)
check_collinearity(bdlm2z)
plot(allEffects(bdlm2z))
#"low correlation" interesting. I would have thought X2 was pretty darn correlated with salinity, 
#but I guess not too much

###########BDL versus X2#############

BDLX2 = left_join(BDLx, DF) %>%
  left_join(yrs)
ggplot(BDLX2, aes(x = X2, y = Salinity))+ geom_point()+
  geom_smooth(method = "lm")

ggplot(BDLX2, aes(x = X2, y = Salinity))+ geom_point(aes(color = as.factor(Year)))+
  geom_smooth(method = "lm")

ggplot(BDLX2, aes(x = X2, y = Salinity, color = YrType))+ geom_point()+
  geom_smooth()

ggplot(BDLX2, aes(x = log(X2), y = Salinity, color = YrType))+ geom_point()+
  geom_smooth()


#Ok, so below a certain level of X2, salnity at BDL is basically zero, 
#but you would expect pseudos to keep increasing, right? Maybe?
ggplot(pseudo2, aes(x = X2, y = logCPUE))+
  facet_wrap(~Region)+
  geom_point()+
  geom_smooth(method = "lm")


ggplot(filter(pseudo2, X2 <70), aes(x = X2, y = logCPUE))+
  facet_wrap(~Region)+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(filter(pseudo2, X2 >70), aes(x = X2, y = logCPUE))+
  facet_wrap(~Region)+
  geom_point()+
  geom_smooth(method = "lm")
#yeah, but it's messy

bdlm70 = glmmTMB(CPUE ~ X2*Region + (1|Month),  family=nbinom2, data = filter(pseudo2, X2<70)) 
summary(bdlm70)
plot(allEffects(bdlm70))
#soooo, I think this is saying that really low X2 causes decreases in pseudos in the river, 
#doesn't change the suidun areas at alll. Weird.


bdlm70p = glmmTMB(CPUE ~ X2*Region + (1|Month)+ (1|Year),  family=nbinom2, data = filter(pseudo2, X2>=70)) 
summary(bdlm70p)
plot(allEffects(bdlm70p))

# I could limit this to just higher X2 values, or i could to to a GAM. Or something else. 
#But I think the origional plot of pseudos versus X2 shows a pretty good linear relationship over
#the whold thing, so maybe just go with that.

bdlm2z = glmmTMB(CPUE ~ X2+Region +Salinity+ (1|Month) + (1|Year),  family=nbinom2, data = pseudo2) 
summary(bdlm2z)
check_collinearity(bdlm2z)
plot(allEffects(bdlm2z))

###### Salinity Bins instead of Region ###########################

pseudo2 = mutate(pseudo2, SalBin = case_when(SalSurf < 0.5 ~ "1-Fresh",
                                             SalSurf >=0.5 & SalSurf < 6 ~ "2 LSZ",
                                             SalSurf >=6  ~ "3 high"),
                 SalBin2 = case_when(SalSurf < 0.1 ~ "1-Fresher",
                                     SalSurf >= 0.1 &SalSurf < 0.5 ~ "1.5-Fresh",
                                    SalSurf >=0.5 & SalSurf < 6 ~ "2 LSZ",
                                    SalSurf >=6  ~ "3 high"))

sallm = glmmTMB(CPUE ~ X2*SalBin2 + (1|Month) + (1|Year),  family=nbinom2, data = pseudo2) 
summary(sallm)
plot(allEffects(sallm))
emtrends(sallm,  pairwise ~ SalBin2, var = "X2")
#huh, this has pseudos increasing with increasing X2 even in the freshwater zone. 
#overall, we definitely get more pseudos in higher flows regardless of slinity zone