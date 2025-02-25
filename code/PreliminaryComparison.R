#start thinking through the synthesis work

library(tidyverse)
library(readxl)
library(cder)
library(wql)
library(sf)
#first we need a dataset of mean fall X2, mean summer X2, water year type, NDOI, etc.
#plus number of days of SMSCG operations

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
DFsum = group_by(DF, Season, Year2) %>%
  summarize(OUT = mean(OUT), X2 = mean(X2), CVP = mean(CVP), SWP = mean(SWP))


#add water year type and filter to just 2010-2024
yrsAction = left_join(DFsum, yrs, by = c("Year2" = "WY")) %>%
  filter(Year2 >2010) %>%
  select(Season, Year2, OUT, X2, CVP, SWP, Index, `Yr-type`, Action)

write.csv(yrsAction, "YearTypes_actions.csv", row.names = FALSE)

#OK, now number of days of gate operations
#https://data.cnra.ca.gov/dataset/suisun-marsh-salinity-control-gates-log
gates = read_excel("Data/smscg-log.xlsx")
gateop = mutate(gates, Endtime = lead(DATETIME), ndays = as.numeric(Endtime-DATETIME)/1440,
                ndaysR = round(ndays))

ops = filter(gateop, ACTION == "Tidal Operations") %>%
  mutate(Date = date(DATETIME))

foo3 = data.frame(Date = NA, ACTION = NA)

for(i in 1:nrow(ops)) {
  foo = alldays %>%
    filter(Date >= ops$Date[i] & Date <= ops$Endtime[i]) %>%
    mutate(ACTION = "Tidal")
  foo3 = bind_rows(foo3, foo)
}

foo3 = foo3%>%
  mutate(Mo = month(Date), Year = year(Date),             
         Season = case_when(Mo %in% c(3,4,5) ~ "Spring",
                            Mo %in% c(6,7,8) ~ "Summer",                              
                            Mo %in% c(9,10) ~ "Fall",                               
                            TRUE ~ "Winter"),
         Year2 = case_when(Mo %in% c(11,12) ~Year+1,
                           TRUE ~Year)) %>%
  filter(Year2 >=2018)

histops = read_csv("Data/gatelog.csv") %>% 
                  mutate(Endtime = lead(Date), ndays = as.numeric(Endtime-Date),
                   ndaysR = round(ndays),
                   ACTION = case_when(str_detect(`Gate 1`,"OP")|str_detect(`Gate 2`,"OP")| str_detect(`Gate 3`,"OP") ~ "Tidal Operations"))

hops = filter(histops, ACTION == "Tidal Operations") %>%
  mutate(Mo = month(Date), Year = year(Date),             
         Season = case_when(Mo %in% c(3,4,5) ~ "Spring",
                            Mo %in% c(6,7,8) ~ "Summer",                              
                            Mo %in% c(9,10) ~ "Fall",                               
                            TRUE ~ "Winter"),
         Year2 = case_when(Mo %in% c(11,12) ~Year+1,
                           TRUE ~Year))

alldays = data.frame(Date = seq(from = ymd("2010-01-01"), to = ymd("2024-11-01"), by = "day"))
foo2 = data.frame(Date = NA, ACTION = NA)

for(i in 1:nrow(hops)) {
   foo = alldays %>%
     filter(Date >= hops$Date[i] & Date <= hops$Endtime[i]) %>%
     mutate(ACTION = "Tidal")
   foo2 = bind_rows(foo2, foo)
}

foo2 = foo2%>%
  mutate(Mo = month(Date), Year = year(Date),             
         Season = case_when(Mo %in% c(3,4,5) ~ "Spring",
                            Mo %in% c(6,7,8) ~ "Summer",                              
                            Mo %in% c(9,10) ~ "Fall",                               
                            TRUE ~ "Winter"),
         Year2 = case_when(Mo %in% c(11,12) ~Year+1,
                           TRUE ~Year)) %>%
  filter(Year2 <2018, Year2 >2009)


allops = left_join(alldays,
  bind_rows(foo2, foo3) )%>%
  mutate(Mo = month(Date), Year = year(Date),             
         Season = case_when(Mo %in% c(3,4,5) ~ "Spring",
                            Mo %in% c(6,7,8) ~ "Summer",                              
                            Mo %in% c(9,10) ~ "Fall",                               
                            TRUE ~ "Winter"),
         Year2 = case_when(Mo %in% c(11,12) ~Year+1,
                           TRUE ~Year))


opsbyyear = group_by(allops, Season,Year2) %>%
  summarize(ndays = length(ACTION[which(ACTION == "Tidal")]))

DFsum2 = left_join(DFsum, opsbyyear) %>%
  left_join(yrs, by = c("Year2"="WY"))

##################################################
#how does this relate to salinity at Beldens'?

BDL = cdec_query("BDL", "100", durations = "E", start.date = ymd("2010-01-01"), end.date = ymd("2024-11-01"))

BDL2 = mutate(BDL, Salinity = ec2pss(Value/1000, 25), Year = year(ObsDate), Mo = month(ObsDate),
              Year2 = case_when(Mo %in% c(11,12) ~ Year+1,
                                TRUE ~ Year),
              Season  = case_when(Mo %in% c(3,4,5) ~ "Spring",
                                  Mo %in% c(6,7,8) ~ "Summer",                              
                                  Mo %in% c(9,10) ~ "Fall",                               
                                  TRUE ~ "Winter")) %>%
  group_by(Year2, Season) %>%
  summarize(BDL = mean(Salinity, na.rm =T))

DFsum2_wsal = left_join(DFsum2, BDL2)

ggplot(DFsum2_wsal, aes(x = X2, y = BDL))+
  geom_point(aes(color = `YrType`))+
  geom_smooth(method = "lm")+
  facet_wrap(~Season)

ggplot(DFsum2_wsal, aes(x = ndays, y = BDL))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Season)

#Huh, number of days of gate operation doesn't seem to impact salinity that much.
#probably because when it's fresh you don't operate the gates mcuh

#try modeling it

lm1 = lm(BDL ~ X2 + ndays, data = DFsum2_wsal)
summary(lm1)
library(effects)
plot(allEffects(lm1))
#OK, that makes more sense

#Smelt caught in summer and fall versus X2 and gate operations
#pull all sme\lt catch from deltafish?
# library(deltafish)
# create_fish_db()
# 
# con = open_database()
# 
# surv <- open_survey(con)
# fish <- open_fish(con)
# 
# # filter for sources and taxa of interest
# # Also filter for dates of interest. Although dates and datetimes are stored as text in the dataset,
# # you can still filter on them using text strings in the "YYYY-MM-DD" or "YYYY-MM-DD HH:MM:SS" format.
# 
# surv_FMWT <- surv %>% 
#   filter(Source %in% c("FMWT", "EDSM", "DJFMP", "STN", "SKT", "20mm", "Suisun") & Date > "2010-01-01")
# 
# fish_smelt <- fish %>% 
#   filter(Taxa %in% c("Hypomesus transpacificus"))
# 
# 
# # do a join and collect_data the resulting data frame
# # collect_data executes the sql query, converts Date and Datetime columns to the correct format and timezone, and gives you a table
# df <- left_join(surv_FMWT, fish_smelt) %>% 
#   collect_data() 
# # close connection to database
# close_database(con)
# 
# #OK, what is the total catch of smelt in suisun and CPUE of smelt in suisun by year?
# 
# smelt = df %>%
#   filter(!is.na(Longitude)) %>%
#   st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
#   st_transform(crs = st_crs(Regions)) %>%
#   st_join(Regions) %>%
#   st_drop_geometry() 
# 
# save(smelt, file = "data/smelt.RData")

load("data/smelt.RData")

smeltCPUE = filter(smelt, !is.na(Tow_volume)) %>%
  mutate(CPUE = Count/Tow_volume, Year = year(Date), Month = month(Date)) %>%
  group_by(Source, Region, Year, Month, SampleID) %>%
  summarize(Count = sum(Count, na.rm =T), CPUE = sum(CPUE)) %>%
  group_by(Source, Region, Year, Month) %>%
  summarize(CPUE = mean(CPUE, na.rm =T), Count = sum(Count)) %>%
  mutate(Season = case_when(Month %in% c(1,2,3,4,5) ~ "Spring",
                            Month %in% c(2,3,4,5) ~ "Spring",
                            Month %in% c(6,7,8)~ "Summer",
                            Month %in% c(11,12,1)~ "Winter",
                            Month %in% c(9,10) ~ "Fall"))

ggplot(filter(smeltCPUE, !is.na(Region)), aes(x = Year, y = CPUE, fill = Source))+
  facet_grid(Region~Season)+
  geom_col()

ggplot(filter(smeltCPUE, !is.na(Region)), aes(x = Year, y = log(Count+1), fill = Source))+
  facet_grid(Region~Season)+
  geom_col()

ggplot(filter(smeltCPUE, !is.na(Region), Season %in% c("Fall", "Summer")), aes(x = Year, y = log(CPUE+1), fill = Source))+
  facet_grid(Region~Season)+
  geom_col()


ggplot(filter(smeltCPUE, !is.na(Region), Season %in% c("Fall", "Summer")), 
       aes(x = Year, y = Count, fill = Source))+
  facet_grid(Region~Season)+
  geom_col()

test = filter(smeltCPUE, !is.na(Region), Season %in% c("Fall", "Summer"))

ggplot(filter(smeltCPUE, !is.na(Region), Source != "DJFMP", Season %in% c("Fall", "Summer")), aes(x = Year, y = log(CPUE+1), fill = Source))+
  facet_grid(Season~Region, scales = "free_y")+
  geom_col()


ggplot(filter(smeltCPUE, !is.na(Region), Source != "DJFMP", Season == "Fall"), aes(x = Year, y = log(Count+1), fill = Source))+
  facet_grid(Region~Season)+
  geom_col()

#Spawning adults by year
ggplot(filter(smeltCPUE, Source %in% c("SKT", "DJFMP", "EDSM"), Month %in% c(1,2,3,4)), 
       aes(x = Year, y = CPUE, fill = Source))+
  geom_col()+
  ylab("CPUE spawning adults, Jan=April")

ggplot(filter(smeltCPUE, Source %in% c("SKT", "DJFMP", "EDSM"), Month %in% c(1,2,3,4)), 
       aes(x = Year, y = Count, fill = Source))+
  geom_col()+
  ylab("Total spawning adults caught, Jan=April")


#set some bayesian priors for prevelance of smelt. 
#start with just two groups, flow versus not-flow

#### smelt versus flow ###############################

names(smeltCPUE)


DF = Dayflow %>%
  mutate(Month = month(Date)) %>%
  group_by( Year, Month) %>%
  filter(OUT >1) %>%
  summarize(OUT = mean(OUT, na.rm =T)) 
smeltCPUE2 = left_join(smeltCPUE, DF) %>%
  group_by(Month, Year, Region, Season) %>%
  summarize(CPUE = mean(CPUE), OUT = mean(OUT))


ggplot(smeltCPUE2, aes(x = log(OUT), y = log(CPUE+1)))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Season, scales = "free_y")

#####Pseudodiaptomus #############################################################################
#Pseudodiaptomus in Suisun Bay, SuisuN Marsh versus X2 and gate operations
library(zooper)

pseudos = Zoopsynther(Data_type = "Community", Years = c(2010:2023),
                      Size_class = "Meso") %>%
  filter(Genus == "Pseudodiaptomus")

load("data/SMSCGRegions.RData")
pseudosum = pseudos %>%
  mutate(Mo = month(Date), 
         Year2 = case_when(Mo %in% c(11,12) ~ Year+1,
                           TRUE ~ Year),
         Season  = case_when(Mo %in% c(3,4,5) ~ "Spring",
                             Mo %in% c(6,7,8) ~ "Summer",                              
                             Mo %in% c(9,10) ~ "Fall",                               
                             TRUE ~ "Winter")) %>%
  filter(!is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(Regions)) %>%
  st_join(Regions) %>%
  st_drop_geometry()

pseudo2 = group_by(pseudosum, Region, Year2, Season, SampleID) %>%
  filter(!Undersampled) %>%
  summarize(CPUE = sum(CPUE)) %>%
  group_by(Region, Year2, Season) %>%
  summarize(CPUE = mean(CPUE)) %>%
  filter(!is.na(Region))

pseudowflow = left_join(DFsum2_wsal, pseudo2) %>%
  filter(!is.na(Region))

ggplot(pseudowflow, aes(x = X2, y = log(CPUE+1))) + geom_point()+
  geom_point(aes(color = `Yr-type`))+ geom_smooth(method = "lm")+
  facet_grid(Season~Region)

ggplot(pseudowflow, aes(x = ndays, y = log(CPUE+1))) + geom_point()+
  geom_point(aes(color = `Yr-type`))+ geom_smooth(method = "lm")+
  facet_grid(Season~Region)

ggplot(pseudowflow, aes(x = BDL, y = log(CPUE+1))) + geom_point()+
  geom_point(aes(color = `Yr-type`))+ geom_smooth(method = "lm")+
  facet_grid(Season~Region)


plm1 = lm(log(CPUE+1) ~ X2 + ndays*Region + Season, data = pseudowflow)
summary(plm1)
plot(allEffects(plm1))

#try just fall in SUisun marsh
plm2= lm(log(CPUE+1) ~ X2 + ndays, 
         data = filter(pseudowflow, Region == "Suisun Marsh", Season == "Fall"))
summary(plm2)
plot(allEffects(plm2))
#huh, no impact at all
plm3= lm(log(CPUE+1) ~ X2 + ndays+ Season, 
         data = filter(pseudowflow, Region == "Suisun Marsh", Season %in% c("Summer", "Fall")))
summary(plm3)
plot(allEffects(plm3))

plm4= lm(log(CPUE+1) ~ BDL + ndays+ Season, 
         data = filter(pseudowflow, Region == "Suisun Marsh", Season %in% c("Summer", "Fall")))
summary(plm4)
plot(allEffects(plm4))


plm4= lm(log(CPUE+1) ~ ndays+ Season, 
         data = filter(pseudowflow, Region == "Suisun Marsh", Season %in% c("Summer", "Fall")))
summary(plm4)
plot(allEffects(plm4))

psum = mutate(pseudosum, DOY = yday(Date))
ggplot(psum, aes(x = DOY, y = log(CPUE+1),
                      color = as.factor(Year2)))+ 
  geom_point(alpha = 0.5)+ geom_smooth()

#I guess this is telling me that salinity is important, it doesn't matter how you get there.


#Ok, now let's just make a basic graph of sample availabilty by year
zoopsamps = Zoopsynther(Data_type = "Community", Years = c(2010:2023),
                      Size_class = c("Meso", "Macro")) 

zoopsampsX = zoopsamps %>%
  mutate(Mo = month(Date), 
         Year2 = case_when(Mo %in% c(11,12) ~ Year+1,
                           TRUE ~ Year),
         Season  = case_when(Mo %in% c(3,4,5) ~ "Spring",
                             Mo %in% c(6,7,8) ~ "Summer",                              
                             Mo %in% c(9,10) ~ "Fall",                               
                             TRUE ~ "Winter")) %>%
  filter(!is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(Regions)) %>%
  st_join(Regions) %>%
  st_drop_geometry()

zoopsampsXY = select(zoopsampsX, Region, Mo, Season, SampleID, SizeClass, Year)  %>%
  distinct() %>%
  filter(!is.na(Region)) %>%
  ungroup()

zoopsampssum = group_by(zoopsampsXY, Region, Mo, Season, SizeClass, Year)  %>%
  summarize(N = n())

ggplot(zoopsampssum, aes(x = Year, y = N, fill = SizeClass)) + geom_col(position = "dodge")+
  facet_wrap(~Region)+
  ylab("Number of Samples (June-October)")
