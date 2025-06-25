#smelt health and condition - exploritory analysis

library(tidyverse)
library(readxl)
library(RColorBrewer)
library(lme4)
library(lmerTest)
library(effects)

mypal = c(brewer.pal(8, "Dark2"), brewer.pal(8, "Set2"), "black", "purple", "skyblue")

health = read_excel("Data/Master_ordered_srdwsc_zoop.xlsx", sheet = "Master_ordered", guess_max = 10000)
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


#get the station locations
EDSM20mm = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.415.11&entityid=d468c513fa69c4fc6ddc02e443785f28")

EDSMSKT = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.415.11&entityid=4d7de6f0a38eff744a009a92083d37ae")

EDSMstas = bind_rows(EDSM20mm, EDSMSKT) %>%
select(RegionCode, Subregion, Stratum, Phase, StationCode, LatitudeStart, LongitudeStart, SampleDate) %>%
  distinct() %>% #multiple tows per station, I'll just take the start of the first tow
  group_by(Phase, StationCode, SampleDate) %>%
  summarise(Latitude = first(LatitudeStart), Longitude = first(LongitudeStart)) %>%
  mutate(survey_char = "edsm")

#why do we have some duplicates?
foo = EDSMstas %>%
  group_by(StationCode, SampleDate) %>%
  summarise(n = n()) %>%
  filter(n>1) %>%
  left_join(EDSMstas)

library(deltamapr)
stas2 = deltamapr::P_Stations %>%
  st_drop_geometry() %>%
  filter(Source %in% c("FMWT", "STN", "SKT")) %>%
  mutate(survey_char = case_when(Source == "FMWT" ~ "fmwt",
                                 Source == "STN" ~ "stn", 
                                 Source == "SKT" ~ "skt")) %>%
  rename(StationCode = Station) %>%
  select(Latitude, Longitude, survey_char, StationCode) %>%
  group_by(survey_char, StationCode) %>%
  summarize(Latitude = first(Latitude), Longitude = first(Longitude))

allstas = bind_rows(EDSMstas, stas2) %>%
  distinct()

healthstas = select(health, fish_id, casette_number, station, collection_date, survey_char) %>%
  left_join(allstas, by = c("station" = "StationCode", "survey_char"))

#Grab the fish ID from EDSM
extract_after_slash <- function(input_string) {
  pattern <- "(?<=/)\\w+"
  ID = case_when(str_detect(input_string, "/") ~ stringr::str_extract_all(input_string, pattern, simplify = T),
                 TRUE ~ input_string)
  return(ID)
}

EDSMsmelt = bind_rows(EDSM20mm, EDSMSKT) %>%
  select(RegionCode, Subregion, Stratum, Phase, StationCode, LatitudeStart, LongitudeStart, SampleDate, SpecialStudyID,
         NetSize, TowNumber, TowSchedule, TowDuration, Volume, Secchi, MarkCode, ForkLength) %>%
  filter(!is.na(SpecialStudyID)) %>%
  mutate(DOPID = extract_after_slash(SpecialStudyID))


EDSMsmeltall =  filter(health, survey_char == "edsm") %>%
  select(fish_id, casette_number, collection_date) %>%
  left_join(EDSMsmelt, by = c("fish_id" = "DOPID"))


##########liver lesions #####################

names(health)
livers = filter(health, season %in% c("Summer", "Fall"), fork_length <70, !is.na(region), !is.na(liver_lesions))

ggplot(livers, aes(x = region1, y = liver_lesions, fill = season)) +
  # facet_wrap(~region1)+
  geom_boxplot()+
  ylab("liver_lesions")

ggplot(livers, aes(x = cohort...7, y = liver_lesions, fill = season)) +
   facet_wrap(~region1)+
  geom_boxplot()+
  ylab("liver_lesions")
#ugh, not a lot of data.

ggplot(livers, aes(x = cohort...7, y = liver_lesions, fill = season)) +
  facet_wrap(~region1)+
  geom_boxplot()+
  ylab("liver_lesions")
