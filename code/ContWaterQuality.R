#continuous water quality
library(tidyverse)
library(discretewq)
library(wql)

WQ = read_csv("data/SMSCG_wq_data_2017-2024.csv")

#calculate percentage of day above or below the threshold

WQsum = WQ %>%
  select(-fluorescence, -ph, -spc, -spc_milli, -dissolvedoxygen) %>%
  pivot_longer(cols = c(turbidity, watertemperature, salinity), names_to = "Analyte", values_to = "Value") %>%
  mutate(Date = date(date_time_pst), Year = year(Date), Month = month(Date), DOY = yday(Date),
         region = case_match(station, c("GZL", "GZB", "GZM") ~ "Grizzly Bay",
                             .default = region))

WQsumdaily = WQsum %>%
  group_by(Date, Year, Month, DOY, region, Analyte) %>%
  summarize(Value = mean(Value, na.rm =T))

###########################################
#quick look at achlorohpyll


chl = WQ %>%
  select(station, year, date_time_pst, fluorescence, region) %>%
  filter(!is.na(fluorescence)) %>%
  mutate(Date = date(date_time_pst), Year = year(Date), Month = month(Date), DOY = yday(Date),
         region = case_match(station, c("GZL", "GZB", "GZM") ~ "Grizzly Bay",
                             .default = region))

chldaily = chl %>%
  group_by(Date, Year, Month, DOY, region) %>%
  summarize(Fluorescence = mean(fluorescence, na.rm =T))


ggplot(chldaily, aes(x = region, y = Fluorescence)) + geom_boxplot()+
  facet_wrap(~Year)


ggplot(chl, aes(x = region, y = fluorescence)) + geom_boxplot()+
  facet_wrap(~Year)

chldisc = wq(Sources = c("EMP", "USGS_CAWSC", "USGS_SFBS"), Start_year = 2010, End_year = 2024)

ggplot(chldaily, aes(x = DOY, y = Fluorescence, color = as.factor(Year)))+ geom_line()+
  facet_wrap(~region)

ggplot(chldisc, aes(x = Source, y = Chlorophyll))+ geom_boxplot()

##############################################
#grab a bit more data from north delta and south delta for ITP ammendment

extradata = cdec_query(c("LIB", "DWS", "RYI", "SJJ", "OSJ"), sensors = c(
                                                                       25, #temperature in degrees F
                                                                       27, #Turbidity NTU
                                                                       221), #Turbidity FNU),
                                   start.date = ymd("2019-01-01"), end.date = ymd("2024-12-31"))

extradat = extradata %>%
  filter(Duration == "E") %>%
  rename(station = StationID) %>%
  filter(!is.na(Value),Value <900000, !(SensorUnits == "DEG F" & Value >82),
!(SensorUnits == "DEG F" & Value < 35),
!(SensorType == "EL COND" & Value > 55000),
!(SensorUnits == "NTU" & Value >500)) %>%
  mutate(Value2 = case_when(SensorUnits == "DEG F" ~ (Value-32)*5/9,
                            SensorUnits == "uS/cm" ~ ec2pss(Value/1000, 25),
                            TRUE ~ Value),
         Analyte = recode(SensorType, "EL COND" = "salinity", 
                            "TEMP W" = "watertemperature", "TURB W" = "turbidity", "TURB WF" = "turbidity"),
         Date = date(ObsDate), DOY = yday(Date), Month = month(Date), Year = year(Date),
         region = case_match(station, c("LIB", "DWS", "RYI") ~ "North Delta",
                             c("SJJ", "OSJ") ~ "San Joaquin")) %>%
  select(station, ObsDate, Date, Analyte, Value2, region, Year, Month, DOY) %>%
  rename(Value = Value2) %>%
  group_by( Date, Analyte, region, Year, Month, DOY) %>%
  summarise(Value = mean(Value))


WQsumdaily2 = bind_rows(WQsumdaily, extradat) %>%
  mutate(region = recode(region, "River" = "Lower Sacramento",
                         "Marsh" = "Suisun Marsh", "Bay" = "Suisun Bay"))

save(WQsumdaily2, file = "data/WQsumdaily2.RData")

#####plots for ITP ammendment####

ggplot(filter(WQsumdaily2, Analyte == "watertemperature", Year >2018, DOY %in% c(153:305)), 
       aes(x = DOY, y = Value, color = as.factor(Year))) + geom_smooth()+ geom_line(alpha = 0.5)+
  facet_wrap(~region) + geom_hline(yintercept = 22)+ ylab("Temperature (C)")+
  xlab("Day of Year")+
  scale_color_discrete(name = "Year")+
  theme_bw()

ggsave("plots/TemperatureByYear.png", width =10, height =8, device = "png")


ggplot(filter(WQsumdaily2, Analyte == "watertemperature", Year >2018, DOY %in% c(153:304)), 
       aes(x = as.factor(Month), y = Value, fill = region)) + geom_boxplot()+
  geom_hline(yintercept = 22)+ ylab("Temperature (C)")+
  xlab("Month")+
  scale_color_discrete(name = "Year")+
  theme_bw()



ggplot(filter(WQsumdaily2, Analyte == "turbidity", Year >2018, DOY %in% c(153:305)), 
       aes(x = DOY, y = Value, color = as.factor(Year))) + geom_line(alpha = 0.5)+geom_smooth()+ 
  facet_wrap(~region) + geom_hline(yintercept = 12)+ ylab("Turbidity (NTU)")+
  xlab("Day of Year")+
  scale_color_discrete(name = "Year")+
  theme_bw()+ coord_cartesian(ylim = c(0, 90))

ggsave("plots/TurbidityByYear.png", width =10, height =8, device = "png")

ggplot(filter(WQsumdaily2, Analyte == "turbidity", Year >2018, DOY %in% c(153:304)), 
       aes(x = as.factor(Month), y = Value, fill = region)) + geom_boxplot()+
  geom_hline(yintercept = 12)+ ylab("Turbidity (NTU)")+
  xlab("Month")+
  scale_color_discrete(name = "Year")+
  theme_bw()



#####################################
#we don't have turbidity data for the marsh in earlier years. Need to figure out what to do there. 
#Oh! I know! use the data I developed for the bioenergetics work. 
#i imputed turbidity with linear interpolation between discrete measurements
#ugh, though i only did it daily, so it doens't work with my percent-of-day thing. I think that's ok.

load("C:/Users/rhartman/OneDrive - California Department of Water Resources/salinity control gates/DSBEM_HSI/data/WaterQuality20102024.RData")

WQsumave = AllWQmean2  %>%
  rename(Analyte = Parameter) %>%
  mutate(Month = month(Date), Season = case_when(Month %in% c(6,7,8) ~ "Summer", 
                                                                   Month %in% c(9,10) ~ "Fall")) %>%
  filter(Month %in% c(6:10)) %>%
  mutate(Suitable = case_when(Analyte == "watertemperature" & ValueImputed >22 ~ 0,
                           Analyte == "watertemperature" & ValueImputed <= 22 ~ 1,
                           Analyte == "turbidity" & ValueImputed >12 ~ 1,
                           Analyte == "turbidity" & ValueImputed <= 12 ~ 0,
                           Analyte == "salinity" & ValueImputed > 6 ~ 0,
                           Analyte == "salinity" & ValueImputed <= 6 ~ 1)) 


WQsumsuit = WQsumave%>%
  group_by(Date, DOY, Year, Month, Season, Analyte, Region) %>%
  summarize(PercentSuitable = sum(Suitable)/n(),
            Suitable = sum(Suitable, na.rm  =T), ValueX = mean(Value, na.rm =T), Value = mean(ValueImputed, na.rm =T))%>%
  mutate(Region = case_match(Region, c("NE Suisun", "SE Suisun", "SW Suisun") ~ "Suisun Bay",
                             "NW Suisun" ~ "Grizzly Bay",
                             c("Confluence", "Lower Sacramento River") ~ "River",
                             "Suisun Marsh" ~ "Suisun Marsh"),
         Imputed = case_when(is.na(ValueX) ~ TRUE,
                             TRUE ~ FALSE))


#plot of trends - does this look right?
ggplot(WQsumsuit, aes(x = Date, y = Value))+
  facet_grid(Analyte~Region, scales = "free")+ geom_line()

#better plot with regional means
WQsumave2 = group_by(WQsumsuit, Date, DOY, Year, Month, Analyte, Region) %>%
  summarize(Value = mean(Value, na.rm =T), ValueX = mean(ValueX, na.rm =T)) %>%
  mutate(Region = factor(Region, levels = c( "Grizzly Bay", "Suisun Bay", "Suisun Marsh", "River"),
                labels = c( "Grizzly Bay", "Suisun Bay", "Suisun Marsh", "Lower Sacramento\nRiver")),
         Imputed = case_when(is.na(ValueX) ~ TRUE,
                             TRUE ~ FALSE))

ggplot(WQsumave2, aes(x = Date, y = Value))+
  facet_grid(Analyte~Region, scales = "free")+ geom_line()+
  geom_point(aes(color = Imputed), size = 1)+
  scale_color_manual(values = c("black", "coral"))+
  theme_bw()+ theme(legend.position = "bottom")


ggsave("plots/WQtrends.png", device = "png", width = 8, height =6)

#now get back to the number of suitable days. 
SuitableAll = WQsumave %>%
  group_by(Date, DOY, Year, Month,Season, Region) %>%
  summarize(GoodSmeltHabitat = sum(Suitable)) %>%
  mutate(GoodSmeltHabitat = case_when(GoodSmeltHabitat ==3 ~ 1,
                                      TRUE ~ 0)) %>%
  mutate(Region = case_match(Region, c("NE Suisun", "SE Suisun", "SW Suisun") ~ "Suisun Bay",
                            "NW Suisun" ~ "Grizzly Bay",
                            c("Confluence", "Lower Sacramento River") ~ "River",
                            "Suisun Marsh" ~ "Suisun Marsh"))

#make bigger regions


Suitallannual = SuitableAll %>%
  group_by(Year, DOY, Date, Season, Region) %>%
  summarize(Suitable = mean(GoodSmeltHabitat, na.rm =T)) %>%
  group_by(Year, Season, Region) %>%
  summarize(Suitable = sum(Suitable, na.rm =T),
            PercentSuitable = sum(Suitable, na.rm =T)/n())%>%
  mutate(Analyte = "GoodSmeltHabitat")


WQsumannual = WQsumsuit %>%
  group_by(Year, Date, DOY, Season, Region, Analyte) %>%
  summarise(Suitable = mean(Suitable, na.rm =T)) %>%
  group_by(Year, Season, Region, Analyte) %>%
  summarize(Suitable = sum(Suitable, na.rm =T),
            PercentSuitable = sum(Suitable, na.rm =T)/n()) %>%
  bind_rows(Suitallannual) %>%
  filter(!is.na(Season))


ggplot(WQsumannual, aes(x = Year, y = Suitable, fill = Region))+
  geom_col(position = "dodge")+
  facet_grid(Season~Analyte)



#water year index
yrs = read_csv("data/wtryrtype.csv") %>%
  rename(Year = WY)

WQsuman = left_join(WQsumannual, yrs) %>%
  mutate(YrType = factor(YrType, levels = c("C", "D", "BN", "AN", "W")),
         Action = case_match(Action, "FallX2" ~ "X2",
                             "FallX2+Gates" ~ "X2+SMSCG",
                             "Gates" ~ "SMSCG",
                             .default = Action),
         Region = factor(Region, levels = c( "Grizzly Bay", "Suisun Bay", "Suisun Marsh", "River"),
                         labels = c( "Grizzly Bay", "Suisun Bay", "Suisun Marsh", "Lower Sacramento\nRiver")),
         Season = factor(Season, levels = c("Summer", "Fall")))

ggplot(WQsuman, aes(x = Index, y = Suitable))+
  geom_point()+ geom_smooth(method = "lm")+
  facet_wrap(Analyte~Region, scales = "free_y")+
  ylab("Days of Suitable Habitat")+
  xlab("Water Year Index")

ggplot(WQsuman, aes(x = Year, y = Suitable, fill = YrType)) +
  geom_col()+ facet_grid(Analyte~Region)+
  geom_text(aes(y=10, x = Year, label = Action), angle = 90, hjust =0)+
  theme_bw()+
  scale_fill_manual(values =c("red", "orange", "yellow", "limegreen", "skyblue"),
                    labels = c("critical", "dry", "below normal", "above normal", "wet"))+
  ylab("Days of Suitable Habitat")+
  theme(legend.position = "bottom")

ggsave("plots/SuitablHabbyYear.png", device = "png", width =8, height =8)

ggplot(filter(WQsuman, Region == "Suisun Bay", Analyte == "GoodSmeltHabitat"), aes(x = Year, y = Suitable, fill = YrType)) +
  geom_col()+ facet_grid(Analyte~Region)+
  geom_text(aes(y=10, x = Year, label = Action), angle = 90, hjust =0)+
  theme_bw()+
  scale_fill_manual(values =c("red", "orange", "yellow", "limegreen", "skyblue"),
                    labels = c("critical", "dry", "below normal", "above normal", "wet"))+
  ylab("Days of Suitable Habitat")




ggplot(filter(WQsuman, Region == "Suisun Marsh", Analyte == "GoodSmeltHabitat"), aes(x = Year, y = Suitable, fill = YrType)) +
  geom_col()+ facet_grid(Analyte~Region)+
  geom_text(aes(y=10, x = Year, label = Action), angle = 90, hjust =0)+
  theme_bw()+
  scale_fill_manual(values =c("red", "orange", "yellow", "limegreen", "skyblue"),
                    labels = c("critical", "dry", "below normal", "above normal", "wet"))+
  ylab("Days of Suitable Habitat")


#Average X2
load("data/dayflow_w2024.RData")
X2 = Dayflow %>%
  mutate(Year = year(Date), DOY = yday(Date), Month = month(Date),
         Season = case_when(Month %in% c(6,7,8) ~ "Summer", Month %in% c(9,10) ~ "Fall")) %>%
  filter(Year %in% c(2010:2024), Month %in% c(6:10)) %>%
  group_by(Year, Season) %>%
  summarize(X2 = mean(X2, na.rm =T))


WQsuman2 = left_join(WQsuman, X2, by = c("Year", "Season")) %>%
  mutate(PercentTime = case_when(Season == "Summer" ~ Suitable/92,
                                 Season == "Fall" ~ Suitable/61))

#Let's break it out by summer vsersus fall
ggplot(WQsuman2, aes(x = X2, y = PercentTime, color = Season, fill = Season))+
  geom_point()+ geom_smooth(method = "lm")+
  facet_grid(Analyte~Region, scales = "free_y")+
  ylab("Percent time habitat is suitable")+
  xlab("average seasonal X2")+
  theme_bw()+ theme(legend.position = "bottom")+
  scale_color_manual(values = c("skyblue3", "firebrick"))+
  scale_fill_manual(values = c("skyblue2", "firebrick"))+
  coord_cartesian(ylim = c(0,1.1))+
  scale_y_continuous(labels = scales::percent)

ggsave("plots/suitablehabitatvX2.png", device = "png", width =8, height =8)


#Statistics.

habmod = lm(PercentTime ~ Region*X2 + Season, data = filter(WQsuman2, Analyte == "GoodSmeltHabitat"))
summary(habmod)
plot(allEffects(habmod))
anova(habmod)
emtrends(habmod, pairwise ~Region, var = "X2")
#I fieel like maybe i should have a three-way itneraction with season, but I relly hate those. 

#now do it by whether there was an action rather than X2
#Huh. I think I need to just do total days, not be season here.

WQsuman3 = group_by(WQsuman2, Year, Region, Action, Analyte) %>%
  summarize(X2 = mean(X2), Suitable = sum(Suitable), PercentSuitable = Suitable/153) %>%
  mutate(X2YN = case_when(str_detect(Action, "X2") ~ "Yes",
                          TRUE ~ "NO"),
         SMSCGYN = case_when(str_detect(Action, "SMSCG") ~ "Yes",
                             TRUE ~ "NO"))


habmod2 = lm(PercentSuitable ~ Region*Action, data = filter(WQsuman3, Analyte == "GoodSmeltHabitat"))
summary(habmod2)
plot(allEffects(habmod2))

#THIS IS THE MODEL I"M USING.
habmod3 = lm(PercentSuitable ~ Region*X2YN + Region*SMSCGYN, data = filter(WQsuman3, Analyte == "GoodSmeltHabitat"))
summary(habmod3)
plot(allEffects(habmod3))
plot(habmod3)
library(emmeans)
emmeans(habmod3, pairwise ~ "Region", by = "X2YN")
emmeans(habmod3, pairwise ~ "Region", by = "SMSCGYN")
anova(habmod3)
