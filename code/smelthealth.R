#smelt health and condition - exploritory analysis

library(tidyverse)
library(readxl)
library(RColorBrewer)
library(lme4)
library(lmerTest)
library(effects)

mypal = c(brewer.pal(8, "Dark2"), brewer.pal(8, "Set2"), "black", "purple", "skyblue")

health = read_excel("Data/Master_ordered_srdwsc_zoop.xlsx", sheet = "Master_ordered")
excel_sheets("Data/Master_ordered_srdwsc_zoop.xlsx")
metadata = read_excel("Data/Master_ordered_srdwsc_zoop.xlsx")

#Condition factor
ggplot(health, aes(x = as.factor(cohort...7), y = cf)) +
  facet_wrap(~region1)+
  geom_boxplot()+
  ylab("Condition Factor")

#Summer and fall only
ggplot(filter(health, season %in% c("Summer", "Fall"), 
              fork_length <70), aes(x = as.factor(cohort...7), y = cf)) +
  facet_wrap(~region1)+
  geom_boxplot()+
  ylab("Condition Factor")

ggplot(filter(health, season %in% c("Summer", "Fall"), fork_length <70, !is.na(region)), aes(x = region1, y = cf, fill = season)) +
 # facet_wrap(~region1)+
  geom_boxplot()+
  ylab("Condition Factor")

ggplot(filter(health, season %in% c("Summer", "Fall"), fork_length <70, !is.na(region)), aes(x = hsi, y = cf, color = region)) +
  #facet_wrap(~region1)+
  geom_point()+
  ylab("Condition Factor")

ggplot(filter(health, season %in% c("Summer", "Fall"), fork_length <70), aes(x = hsi, y = cf, color = as.factor(cohort...7))) +
  facet_wrap(~season)+
  geom_point()+
  stat_ellipse()+
  ylab("Condition Factor")

#Now look at X2 versus condition factor
ggplot(filter(health, season %in% c("Summer", "Fall")), aes(x = x2_jun_dec, y = cf)) +
  facet_wrap(~region1)+
  geom_point(aes(color = as.factor(cohort...7)))+
  geom_smooth(method = "lm")+
  ylab("Condition Factor")

#What happens if we throw a linear model at it?
hist(health$cf)

cfmod1 = lm(cf ~ x2_actual + day + region1, data = filter(health, season %in% c("Summer", "Fall")))
summary(cfmod1)
plot(cfmod1)
plot(allEffects(cfmod1))


################ Growth and stuff from Otoliths #####################
#we only have otolith data from 2011, but it's something. 
nsmelt = filter(health, season %in% c("Summer", "Fall"), fork_length <70,
                !is.na(gr7)) %>%
  group_by(season, region1) %>%
  summarize(n = n())
ggplot(filter(health, season %in% c("Summer", "Fall"), fork_length <70), 
       aes(x = region1, y = gr7, fill = season)) +
  # facet_wrap(~region1)+
  geom_boxplot()+
  geom_label(data = nsmelt, aes(x = region1, y =0, group = season, label = n), inherit.aes = F,
             position = position_dodge(width = .5))+
  ylab("growth rate from 7 days prior")

ggplot(filter(health, season %in% c("Summer", "Fall"), fork_length <70), 
       aes(x = region1, y = gr30, fill = season)) +
  # facet_wrap(~region1)+
  geom_boxplot()+
  ylab("growth rate from 30 days prior")

### Diet data #########################################################################################

#How many fish diets do we have in summer and fall?
ndietfish = group_by(health, season, region1, cohort...7, year(collection_date)) %>%
  filter(!is.na(percent_weight_gc), season %in% c("Summer", "Fall"), fork_length <70,
         percent_weight_gc <5) %>%
  summarise(n = n())

ggplot(filter(ndietfish, season %in% c("Summer", "Fall"), region1 %in% c("Conf.", "S. Bay", "S. Marsh")), 
       aes(x = `year(collection_date)`, y = n, fill = region1))+
         geom_col(position = position_dodge(preserve = "single") )+
  ylab("Number of smelt diets (June-Nov)")


ggplot(filter(health, season %in% c("Summer", "Fall"), fork_length <70,
              percent_weight_gc <5), 
       aes(x = region1, y = fi, fill = season)) +
  # facet_wrap(~region1)+
  geom_boxplot()+
  
  ylab("gut fullness")


ggplot(filter(health, season %in% c("Summer", "Fall"), fork_length <70,
              percent_weight_gc <5), 
       aes(x = as.factor(cohort...7), y = percent_weight_gc, fill = season)) +
   facet_wrap(~region1)+
  geom_boxplot()+
  ylab("gut fullness")+
  xlab("Cohort")+
  geom_label(data = ndietfish, aes(x = as.factor(cohort...7), y = -0.5, label = n, group = season),
             inherit.aes = F, position = position_dodge(width = 0.5))

ggplot(filter(health, season %in% c("Summer", "Fall"), fork_length <70,
              percent_weight_gc <5), 
       aes(x = x2_actual, y = percent_weight_gc, color = season)) +
  facet_wrap(~region1)+
  geom_point()+
  ylab("gut fullness")+
  xlab("X2")

### What are they eating ####################################

dietfish = filter(health, season %in% c("Summer", "Fall"), fork_length <70, 
                  region1 %in% c("Conf.", "S. Bay", "S. Marsh")) %>%
  select(fish_id, cohort...7, collection_date, season, percent_weight_gc, cf, hsi, gsi,
         fork_length, region1, pf_spp_gc, 	
         as_gc, sd_gc, 	
         tortanus_spp_gc,  `ea_ spp_gc`, other_calanoids_gc, 	
         other_cyclopoids_gc, av_gc, limnoithona_spp_gc, harpacticoids_gc,
         copepod_nauplii_gc, cladocerans_gc, mysids_gc, `corophium_ spp_gc`,
         gammarus_spp_gc, unid_amphipods_gc, cumaceans_gc, fish_gc,  other_gc) %>%
  pivot_longer(cols = c(pf_spp_gc:other_gc), names_to = "Taxa", values_to = "biomass") %>%
  mutate(Year = year(collection_date))

ndietfish2 = health %>%
  mutate(Year = year(collection_date)) %>%
  filter(season %in% c("Summer", "Fall"), fork_length <70, 
         region1 %in% c("Conf.", "S. Bay", "S. Marsh"),
         !is.na(pf_spp_gc)) %>%
  group_by(region1, Year) %>%
  summarise(n = n())

ggplot(dietfish, aes(x = Year, y = biomass, fill = Taxa))+
  geom_col(position = "fill")+
  facet_wrap(~region1)+
  scale_fill_manual(values = mypal)+
  geom_label(data = ndietfish2, aes(x = Year, y = -.05, label = n), inherit.aes = F)

## Other Health Metrics #################################################

#Hepatosomatic index
ggplot(filter(health, season %in% c("Summer", "Fall"), hsi <3), 
       aes(x = as.factor(cohort...7), y = hsi, fill = season)) +
  facet_wrap(~region1)+
  geom_boxplot()+
  ylab("Hepatosomatic index")




ggplot(filter(health, season %in% c("Summer", "Fall"), hsi <3), 
       aes(x = as.factor(cohort...7), y = hsi, fill = region1)) +
  facet_wrap(.~season)+
  geom_boxplot()+
  ylab("Hepatosomatic index")+
  xlab("Year Class")


#liver glycogen depletion. This is the hisology version which isn't very accurate

ggplot(health, aes(x = as.factor(cohort...7), y = liver_gd)) +
  facet_wrap(~region1)+
  geom_boxplot()+
  ylab("liver glycogen depletion")


ggplot(health, aes(x = as.factor(cohort...7), y = tag)) +
  facet_wrap(~region1)+
  geom_boxplot()+
  ylab("I don't know what tag stands for")

##############################################################




