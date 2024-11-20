#rosie zoop playing

library(tidyverse)
library(lme4)
library(effects)
library(zooper)

source("SMSCG Zoops for Report.R")


zoopgroupsX= mutate(all_data_wide, 
                   Acartiella = `Acartiella sinensis Adult` + `Acartiella sinensis Juvenile`,
                   Tortanus = `Tortanus_UnID Adult` + `Tortanus_UnID Juvenile`,
                   Pseudodiaptomus = `Pseudodiaptomus forbesi Juvenile` + `Pseudodiaptomus forbesi Adult` + 
                      `Pseudodiaptomus_UnID Adult` + `Pseudodiaptomus_UnID Juvenile`,    
                   Eurytemora = `Eurytemora affinis Larva` + `Eurytemora affinis Adult` + 
                     `Eurytemora affinis Juvenile`,         
                   `Other Calanoids` = `Sinocalanus doerrii Juvenile` + `Sinocalanus doerrii Adult` + 
                      + `Sinocalanus_UnID Juvenile` + `Acartia_UnID Adult` + `Acartia_UnID Juvenile` 
                   +   `Diaptomidae_UnID Adult` + `Diaptomidae_UnID Juvenile` + `Pseudodiaptomus marinus Adult` + 
                     `Calanoida_UnID Adult` + `Calanoida_UnID Juvenile`,
                   Nauplii = `Pseudodiaptomus_UnID Larva` + `Eurytemora_UnID Larva`,`Sinocalanus doerrii Larva`
                   ) %>% 
  select(Source:Month, Acartiella, Tortanus, Pseudodiaptomus, Eurytemora, `Other Calanoids`, Nauplii) %>%  #remove all the other taxa files
  mutate(SampleID = paste (Source, Region, Year, Month, Station, sep = ' ')) %>%   #create a sample ID b/c things get funky with the total graph later
  pivot_longer(., cols = (c(Acartiella: Eurytemora, Nauplii)), names_to = "Taxa_Group", values_to = "BPUE") %>%
  filter(!is.na(Region)) #move back to long format for combining

ggplot(zoopgroupsX, aes(x = Year, y = BPUE, fill = Taxa_Group))+ geom_col()

#quick model on pseudo abundance by year

pseudo = filter(zoopgroupsX, Taxa_Group == "Pseudodiaptomus") %>%
  mutate(Yearf = as.factor(Year), logBPUE = log(BPUE+1), Month2 = as.numeric(Month))

hist(log(pseudo$BPUE+1))

lm1 = lmer(logBPUE ~ Yearf*Region+ Month2+(1|Station), data = pseudo)
plot(lm1)
summary(lm1)
plot(allEffects(lm1))
library(emmeans)
plot(emmeans(lm1, ~ Yearf|Region), comparison = T)

pairs(emmeans(lm1,  ~ Yearf|Region))

#now let's break out pseudo nauplii and check out acanthocyclops

nauplii = filter(all_data, str_detect(Taxlifestage, "Larva"), !Taxlifestage %in% c("Cirripedia_UnID Larva", "Decapoda_UnID Larva"))
ggplot(nauplii, aes(x = Year, y = BPUE, fill = Taxlifestage))+ geom_col()

acantho = filter(all_data, str_detect(Taxlifestage, "Acanthocyclops")) %>%
  group_by(Month, Region, Year) %>%
  summarize(BPUE = mean(BPUE))
ggplot(acantho, aes(x = Year, y = BPUE), fill = Taxlifestage)+ geom_col()+
  facet_wrap(~Region)
