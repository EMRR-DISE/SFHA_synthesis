#Linear models of pesudodiaptomus versus salinity at belden's and/or fall X2

library(tidyverse)
library(lme4)
library(lmerTest)
library(effects)
library(zooper)
library(glmmTMB)
library(DHARMa)
library(conflicted)
library(cder)
library(sf)
library(wql)

# conflicts_prefer(lmerTest::lmer)
# conflicts_prefer(dplyr::filter)
# conflicts_prefer(dplyr::select)
# #question - should i seperate adults and copepedites? or combine theM?
# 
# #other question - what about P. marinus? Do I just not wory about the fact that we don't have juveniels to species?
# 
# #### Load Data ####################
yrs = read_csv("data/wtryrtype.csv") %>%
  rename(Year = WY)

load("data/SMSCGRegions.RData")
load("data/dayflow_w2024.RData")
DF = filter(Dayflow, Year >2009) %>%
  mutate(Month = month(Date)) %>%
  select(Date, Month, Year, OUT, X2)
# 
# pseudos = Zoopsynther(Data_type = "Community", Years = c(2011:2022), 
#                       Sources = c("EMP", "FMWT", "STN", "20mm", "DOP"),
#                       Size_class = "Meso",
#                       Month = c(6:10)) %>%
#   filter(Genus == "Pseudodiaptomus", Lifestage != "Larva") %>%
#   filter(!is.na(Longitude)) %>%
#   st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
#   st_transform(crs = st_crs(Regions))%>%
#   st_join(Regions) %>%
#   st_drop_geometry() %>%
#   filter(!is.na(Region))
# 
# 
# #how much of this is marinsu versus forbesi?
# 
# pseudostest = group_by(pseudos, Species, Lifestage) %>%
#   summarize(CPUE = sum(CPUE)) %>%
#   filter(!is.na(Species))
# pseudos$CPUE[1]/sum(pseudos$CPUE)
# 
# #most of the data from 2024 isn't public yet, data from Spenser
# smscgto2024 <- read_csv("C:/Users/rhartman/OneDrive - California Department of Water Resources/salinity control gates/SMSCG/Data/smscgto2024_zooplankton_long.csv")
# 
# smscgto2024a = mutate(smscgto2024, DOY = yday(Date)) %>%
#   filter(Year %in% c(2023, 2024))%>%
#   mutate(Taxlifestage=str_remove(Taxlifestage, fixed("_UnID")))%>%
#   filter(Taxlifestage %in% c("Pseudodiaptomus forbesi Adult", "Pseudodiaptomus Adult",
#                              "Pseudodiaptomus Juvenile")) %>%
#   mutate(Taxlifestage = recode(Taxlifestage,#
#                                `Pseudodiaptomus Adult`="Pseudodiaptomus forbesi Adult"))
# 
# #ok, good, same format, regions are already on there.
# #maybe join this after the other stuff has the regions added
# #need salinity data
# fmwtmeta = read_csv("C:/Users/rhartman/OneDrive - California Department of Water Resources/salinity control gates/SMSCG/Data/SMSCG_CBNet_2018to2024CPUE_03Feb2025.csv")
# 
# fmwtmet = fmwtmeta %>%
#   select(Project, Date, Station, CondSurf, PDIAPFOR, PDIAPJUV) %>%
#   distinct() %>%
#   mutate(Source = Project, Station = as.character(Station), 
#          SalSurf = ec2pss(CondSurf/1000, 25), Date = mdy(Date),
#          CPUE =(PDIAPFOR+PDIAPJUV)) #I need to check and make sure this is right.
# 
# smscgto2024b = left_join(smscgto2024a, fmwtmet)
# 
# psudoall = bind_rows(pseudos, smscgto2024b)
# 
# save(psudoall, file = "data/psudoall.RData")

load("data/psudoall.RData")

pseudo = group_by(psudoall, SampleID, Station, SalSurf,  Source, Date, Year, Region) %>%
  summarize(CPUE = sum(CPUE)) %>%
  left_join(yrs) %>%
  left_join(DF) %>%
  mutate(logCPUE = log(CPUE+1))

#make sure I got everything right

ggplot(pseudo, aes(x = Year, y = CPUE, fill = Source))+ geom_col()

#yu
pseudoreg = pseudo

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
#The test statistic is the ratio of observed to simulated zeros. A value < 1 means that the observed data have fewer 
#zeros than expected, a value > 1 means that they have more zeros than expected (aka zero inflation).

##########################THIS IS MODEL #!
#sometime the negative binomial is enough without the zip term

zm3 = glmmTMB(CPUE ~ X2*Region + (1|Month) + (1|Year),  family=nbinom2, data = pseudoreg) 
foo = summary(zm3)
plot(simulateResiduals(zm3))
testZeroInflation(zm3)
plot(allEffects(zm3))
#Huh, so now there are fewer zeros than expeted
#all of these results match our expectations pretty darn well anyway.

write.csv(summary(zm3)$coefficients$cond, "outputs/ZoopsX2model.csv")

#pairwise scomparisons
emmeans(zm3, pairwise  ~ Region)
zm3trends = emtrends(zm3,pairwise ~ Region, var = "X2")
plot(emtrends(zm3,pairwise ~ Region, var = "X2"))

write.csv(zm3trends, file = "outputs/ZoopX2trends.csv")
plot(allEffects(zm3, residuals = T))
zm3eff = allEffects(zm3, residuals = T)[[1]]

zm3effects = data.frame(X2 = zm3eff$x$X2, Region= zm3eff$x$Region,
                        fit =  zm3eff$fit, Lower = zm3eff$lower, Upper = zm3eff$upper)

zm3resid = data.frame(X2 = zm3eff$x.all$X2, Region= zm3eff$x.all$Region, fita = predict(zm3),
                      resid =  zm3eff$residual) %>%
  mutate(partial = resid+fita)
###############################################

#for ITP analysis - is there a relationship in fall?


zm3fall = glmmTMB(CPUE ~ X2*Region + (1|Month) + (1|Year),  family=nbinom2, data = filter(pseudoreg, Month %in% c(9,10))) 
foo = summary(zm3fall)

plot(allEffects(zm3fall))


# #############
# ggplot(zm3effects, aes(x = X2, y = fit)) + 
#  # geom_point(data = zm3resid, aes(y = partial), alpha = 0.3, size =1)+
#   geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.5)+
#   geom_smooth(method = "lm")+
#   facet_wrap(~Region)+
#   ylab("CPUE - partial residuals")+
#   xlab("X2 (km)")+
#   theme_bw()
# 
# ggsave("plots/ZoopX2modeloutcome.tiff", device = "tiff", width =5, height =5, dpi = 400)

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

emtrends(bdlm2, pairwise ~ Region, var = "X2")
emtrends(bdlm2, pairwise ~ Region, var = "Salinity")

write.csv(summary(bdlm2)$coefficients$cond, "outputs/SalinityAndX2Zoopmod.csv")


# 
# #um, maybe visreg?
# library(visreg)
# 
# bdlm2vis = visreg(bdlm2, xvar = "X2", by = "Region")
# bdlm2visS = visreg(bdlm2, xvar = "Salinity", by = "Region")
# 
# 
# pseudo3 = filter(pseudo2, !is.na(Salinity), !is.na(X2)) %>%
#   bind_cols(PredictionX2 = predict(bdlm2, newdata = bdlm2vis[[2]])) %>%
#   bind_cols(PredictionSal = predict(bdlm2, newdata = bdlm2visS[[2]])) %>%
#   mutate(X2resid = CPUE -PredictionX2, Salresid = CPUE-PredictionSal)
# 
# p1 = ggplot(bdlm2vis[[1]])+
#   geom_ribbon(aes(x = X2, ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
#   geom_line(aes(x = X2, y = visregFit), size =1, color = "blue")+
#   #geom_point(data = bdlm2vis[[2]], aes(x = X2, y = visregRes), alpha = 0.2)+
#   #geom_point(data = pseudo3, aes(x = X2, y = X2resid), alpha = 0.2)+
#   facet_wrap(~Region)+
#   theme_bw()+
#   ylab("CPUE - partial residuals")
# 
# p2 = ggplot(bdlm2visS[[1]])+
#   geom_ribbon(aes(x = Salinity, ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
#   geom_line(aes(x = Salinity, y = visregFit), size =1, color = "blue")+
#   #geom_point(data = bdlm2visS[[2]], aes(x = Salinity, y = visregRes), alpha = 0.2)+
#   #geom_point(data = pseudo3, aes(x = Salinity, y = Salresid), alpha = 0.2)+
#   facet_wrap(~Region)+
#   theme_bw()+
#   ylab("CPUE - partial residuals")
# 
# 
# library(patchwork)
# p1+p2
# 
# ggsave("plots/ZoopX2Salmodeloutcome.tiff", device = "tiff", width =5, height =5, dpi = 400)
# #i'm temptd to leave the partial residuals off...
# 

###########################################
#plots of raw data for report - versus X2 and versus salinity

p1 = ggplot(pseudo2, aes(x = X2, y = logCPUE))+
  geom_point(aes(color = Salinity))+
  scale_color_viridis_c(name = "Salinity \nat BDL")+
  facet_wrap(~Region)+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("ln(CPUE+1) of Pseudodiaptomus")+
  theme(legend.position = "bottom")

p2 = ggplot(pseudo2, aes(x = Salinity, y = logCPUE))+
  geom_point(aes(color = X2))+
  scale_color_viridis_c(name = "X2 (km)", option = "B")+
  facet_wrap(~Region)+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("ln(CPUE+1) of Pseudodiaptomus")+
  xlab("Salinity at BDL")+
  theme(legend.position = "bottom")

p1+p2
ggsave("plots/ZoopREgressions.tiff", device = "tiff", width = 10, height =6)

###############################################
#but is this too colinear to even work?
library(performance)

check_collinearity(mod2)

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

ggplot(pseudo2, aes(x = X2, y = Salinity)) + geom_point()+geom_smooth()+
  facet_wrap(~Region)

acf(residuals(bdlm2z))
###############predictions ####################
#how low does X2 need to be in order so see similar Pseudodiaptomus as the river?
#median river Psudo abundance
RiverPseudo =  filter(pseudo2, Region == "River") %>%
  group_by(Month, Year) %>%
  summarize(RiverCPUE = median(CPUE))
#I may want to break it out by season and year, but start with this

#
X2s = data.frame(X2 = c(60:90))
Years = data.frame(Year = c(2011:2023))
newdat = data.frame(Region = c(rep("Suisun Marsh", 5),rep("Suisun Bay", 5), rep("Grizzly Bay", 5)),
                    Month = rep(c(6:10),3)) %>%
  merge(X2s) %>%
  merge(Years) %>%
  merge(RiverPseudo)

Model_output = newdat %>%
  bind_cols(Predicted_CPUE = predict(zm3, type = "response", newdata = newdat))

ggplot(Model_output, aes(x = X2, y = log(Predicted_CPUE), color = as.factor(Year))) +
  facet_wrap(~Month)+
  geom_point()+
  geom_hline(aes(yintercept = log(RiverCPUE)))

#OK, now calculate the X2-pseudo relationship for each motnh and year. Same slope, jsut differnt intercepts

X2mods = group_by(Model_output, Month, Year, Region, RiverCPUE) %>%
  summarize(slope = summary(lm(log(Predicted_CPUE) ~ X2))$coefficients[2,1],
            intercept = summary(lm(log(Predicted_CPUE) ~ X2))$coefficients[1,1]) %>%
  mutate(X2needed = (log(RiverCPUE)-intercept)/slope)


ggplot(X2mods, aes(x = Month, y = Year, fill = X2needed))+
  geom_tile()+
  geom_text(aes(label = round(X2needed)))+
  facet_wrap(~Region)+
  scale_fill_viridis_c()+
  theme_bw()

ggsave("plots/ZoopPredictedX2.tiff", device = "tiff", width =7, height =5)

#Now what is the difference between X2 needed and X2 actual?
X2actual = select(Dayflow, Date, Year, X2) %>%
  mutate(Month = month(Date)) %>%
  group_by(Month, Year) %>%
  summarize(X2 = mean(X2, na.rm =T))

X2mods = left_join(X2mods, X2actual) %>%
  mutate(X2diff = X2needed-X2,
         numscale = case_when(X2diff <= -5 ~ 1,
                              TRUE ~ 2))

ggplot(X2mods, aes(x = Month, y = Year, fill = X2diff))+
  geom_tile()+
  geom_text(aes(label = round(X2diff), color = numscale))+
  facet_wrap(~Region)+
  scale_fill_viridis_c(option = "B")+
  scale_color_manual(values = c("black", "white"), guide = NULL)+
  theme_bw()

ggsave("plots/ZoopX2diff.tiff", device = "tiff", width =7, height =5)


###########BDL versus X2########################BDL versus X2#############Years

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

#just summer-fall

ggplot(filter(BDLX2, month(Date) %in% c(6:10)), aes(x = log(X2), y = log(Salinity), color = YrType))+ geom_point()+
  geom_smooth(method = "lm")


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

pseudo2 = mutate(pseudo2, SalBin = case_when(SalSurf < 0.5 ~ "Fresh",
                                             SalSurf >=0.5 & SalSurf < 6 ~ "LSZ",
                                             SalSurf >=6  ~ "3 high"),
                 SalBin2 = case_when(SalSurf < 0.1 ~ "Very Fresh <0.1",
                                     SalSurf >= 0.1 &SalSurf < 0.5 ~ "Fresh 0.1-0.5",
                                    SalSurf >=0.5 & SalSurf < 6 ~ "LSZ 0.5-6",
                                    SalSurf >=6  ~ "Salty >6"),
                 SalBin2 = factor(SalBin2, levels = c("Very Fresh <0.1", "Fresh 0.1-0.5", "LSZ 0.5-6", "Salty >6"))) 

sallm = glmmTMB(CPUE ~ X2*SalBin2 + (1|Month) + (1|Year),  family=nbinom2, data = pseudo2) 
summary(sallm)
plot(allEffects(sallm))
emtrends(sallm,  pairwise ~ SalBin2, var = "X2")
#huh, this has pseudos increasing with increasing X2 even in the freshwater zone. 
#overall, we definitely get more pseudos in higher flows regardless of slinity zone

ggplot(filter(pseudo2, !is.na(SalBin2)), aes(x = X2, y = logCPUE)) +
  facet_wrap(~SalBin2)+
  geom_point(aes(color = Salinity))+
  scale_color_viridis_c(name = "Salinity \nat BDL")+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("ln(CPUE+1) of Pseudodiaptomus")+
  theme(legend.position = "bottom")

ggsave("plots/zoopsbysalbin.tiff", device = "tiff", width =6, height =6)



#what about fall only?

sallmfall = glmmTMB(CPUE ~ X2*SalBin2 + (1|Month) + (1|Year),  family=nbinom2, 
                    data = filter(pseudo2, Month %in% c(9,10))) 
summary(sallmfall)

sallmfall3 = glmmTMB(CPUE ~ log(OUT)*SalBin2 + (1|Month) + (1|Year),  family=nbinom2, 
                    data = filter(pseudo2, Month %in% c(9,10))) 
summary(sallmfall3)
plot(allEffects(sallmfall3))

ggplot(filter(pseudo2, Month %in% c(9,10)), aes(x = log(OUT), y = log(CPUE), color = as.factor(Year)))+ geom_point()
#################################################
#britt wants outflow instead of X2

###############predictions ####################
#how low does outflow need to be in order so see similar Pseudodiaptomus as the river?


#try a zero inflated negative binomial model on CPUE isntead
pseudoreg = mutate(pseudoreg, logout = log(OUT))

zm2out = glmmTMB(CPUE ~ logout*Region + (1|Month) + (1|Year), family=nbinom2, data = pseudoreg) 

summary(zm2out)
plot(allEffects(zm2out))
#There is actually a decrease in the river when outflow is the response, whereas with X2 it's just flat.

ggplot(pseudoreg, aes(x = log(OUT), y = log(CPUE+1)))+ geom_point()+ geom_smooth(method = "lm")+
  facet_wrap(~Region)

#
OUTs = data.frame(OUT = seq(2000, 90000, by = 50)) %>%
  mutate(logout = log(OUT))
Years = data.frame(Year = c(2011:2023))
newdatout = data.frame(Region = c(rep("Suisun Marsh", 5),rep("Suisun Bay", 5), rep("Grizzly Bay", 5)),
                    Month = rep(c(6:10),3)) %>%
  merge(OUTs) %>%
  merge(Years) %>%
  merge(RiverPseudo)

Model_outputout = newdatout %>%
  bind_cols(Predicted_CPUE = predict(zm2out, type = "response", newdata = newdatout))

ggplot(Model_outputout, aes(x = OUT, y = Predicted_CPUE, color = as.factor(Year))) +
  facet_grid(Region~Month)+
  geom_line()+
  geom_point(data = filter(pseudoreg, Region != "River", !is.na(Month)), aes(y = CPUE))+
  coord_cartesian(ylim = c(0,60000))

ggplot(Model_outputout, aes(x = OUT, y = Predicted_CPUE, color = as.factor(Year))) +
  facet_grid(Region~Month)+
  geom_line()+
  geom_point(data = filter(pseudoreg, Region != "River", !is.na(Month)), aes(y = CPUE))+
  coord_cartesian(ylim = c(0,5000), xlim = c(0,10000))

#OK, now calculate the outflow-pseudo relationship for each motnh and year. Same slope, jsut differnt intercepts

OUTmods = group_by(Model_outputout, Month, Year, Region, RiverCPUE) %>%
  summarize(slope = summary(lm(log(Predicted_CPUE) ~ logout))$coefficients[2,1],
            intercept = summary(lm(log(Predicted_CPUE) ~ logout))$coefficients[1,1]) %>%
  mutate(OUTneeded = (log(RiverCPUE)-intercept)/slope)


ggplot(OUTmods, aes(x = Month, y = Year, fill = OUTneeded))+
  geom_tile()+
  geom_text(aes(label = round(exp(OUTneeded)/1000)))+
  facet_wrap(~Region)+
  scale_fill_viridis_c(name = "Outflow Needed \n Thousand CFS")

#Now what is the difference between X2 needed and X2 actual?
OUTactual = select(Dayflow, Date, Year, OUT) %>%
  mutate(Month = month(Date)) %>%
  group_by(Month, Year) %>%
  summarize(OUT = mean(OUT, na.rm =T))

OUTmods = left_join(OUTmods, OUTactual) %>%
  mutate(OUTdiff = exp(OUTneeded)-OUT)

ggplot(OUTmods, aes(x = Month, y = Year, fill = OUTdiff))+
  geom_tile()+
  geom_text(aes(label = round(OUTdiff/1000), color = OUTdiff/1000))+
  facet_wrap(~Region)+
  scale_fill_viridis_c(option = "B")+
  scale_color_viridis_c(option = "B", begin =1, end =0, guide = NULL)

ggplot(OUTmods, aes(x = Month, y = Year, fill = OUT))+
  geom_tile()+
  geom_text(aes(label = round(OUT/1000), color = OUT))+
  facet_wrap(~Region)+
  scale_fill_viridis_c(option = "B")+
  scale_color_viridis_c(option = "B", begin =1, end =0, guide = NULL)

#########################################################################
#OK, now let's run them again with the exact same data to compare BIC

pseudo3 = filter(pseudo2, !is.na(Salinity), !is.na(X2), !is.na(SalBin2))

mod1 = glmmTMB(CPUE ~ X2*Region +(1|Month) + (1|Year),  family=nbinom2, data = pseudo3) 
mod2 = glmmTMB(CPUE ~ X2*Region +Salinity*Region + (1|Month) + (1|Year),  family=nbinom2, data = pseudo3) 
mod3 = glmmTMB(CPUE ~ X2*SalBin +(1|Month) + (1|Year),  family=nbinom2, data = pseudo3) 
mod3.1 = glmmTMB(CPUE ~ X2*SalBin2 +(1|Month) + (1|Year),  family=nbinom2, data = pseudo3) 


BIC(mod1)
BIC(mod2)
BIC(mod3)
#OK, so BIC for model 2 is an improvement, 3 is even better

summary(mod3)
emtrends(mod3,pairwise ~ SalBin, var = "X2")
plot(emtrends(mod3,pairwise ~ Region, var = "X2"))


summary(mod3.1)
emtrends(mod3.1,pairwise ~ SalBin2, var = "X2")
plot(emtrends(mod3.1,pairwise ~ SalBin2, var = "X2"), comparison =T)

plot(allEffects(mod1, residuals = T))
plot(allEffects(mod2, residuals = T))
