#continuous water quality
library(tidyverse)
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
WQsumave = WQsum  %>%
  mutate(DOY = yday(Date), Month = month(Date), Season = case_when(Month %in% c(6,7,8) ~ "Summer", 
                                                                   Month %in% c(9,10) ~ "Fall")) %>%
  filter(DOY > 135 & DOY < 306) %>%
  mutate(Suitable = case_when(Analyte == "watertemperature" & Value >22 ~ 0,
                           Analyte == "watertemperature" & Value <= 22 ~ 1,
                           Analyte == "turbidity" & Value >12 ~ 1,
                           Analyte == "turbidity" & Value <= 12 ~ 0,
                           Analyte == "salinity" & Value > 6 ~ 0,
                           Analyte == "salinity" & Value <= 6 ~ 1)) 

WQsumsuit = WQsumave%>%
  group_by(Date, DOY, year, Month, Season, station, region, water_year_type, Analyte) %>%
  summarize(PercentSuitable = sum(Suitable)/n())

SuitableAll = WQsumave %>%
  group_by(date_time_pst, Date, DOY, year, Month,Season, station, region, water_year_type) %>%
  summarize(GoodSmeltHabitat = sum(Suitable)) %>%
  mutate(GoodSmeltHabitat = case_when(GoodSmeltHabitat ==3 ~ 1,
                                      TRUE ~ 0))

Suitallannual = SuitableAll %>%
  group_by(Date, DOY, year, Month, Season, station, region, water_year_type) %>%
  summarize(PercentSuitable = sum(GoodSmeltHabitat)/n()) %>%
  group_by(year, station, region, Season, water_year_type) %>%
  summarize(Suitable = sum(PercentSuitable, na.rm =T))%>%
  group_by(year, region, Season, water_year_type) %>%
  summarize(Suitable = mean(Suitable, na.rm =T)) %>%
  mutate(Analyte = "GoodSmeltHabitat")


WQsumannual = WQsumsuit %>%
  group_by(year, Analyte, station, Season, region, water_year_type) %>%
  summarize(Suitable = sum(PercentSuitable, na.rm =T))%>%
  group_by(year, Analyte, region, Season, water_year_type) %>%
  summarize(Suitable = mean(Suitable, na.rm =T)) %>%
  bind_rows(Suitallannual) %>%
  filter(!is.na(Season))


ggplot(WQsumannual, aes(x = year, y = Suitable, fill = region))+
  geom_col(position = "dodge")+
  facet_grid(Season~Analyte)



#water year index
yrs = read_csv("data/wtryrtype.csv") %>%
  rename(year = WY)

WQsuman = left_join(WQsumannual, yrs)

ggplot(WQsuman, aes(x = Index, y = Suitable))+
  geom_point()+ geom_smooth(method = "lm")+
  facet_wrap(Analyte~region, scales = "free_y")+
  ylab("Days of Suitable Habitat")+
  xlab("Water Year Index")

#Average X2
load("data/Dayflow_allw2023.RData")
X2 = Dayflow %>%
  mutate(year = year(Date), DOY = yday(Date), Month = month(Date),
         Season = case_when(Month %in% c(6,7,8) ~ "Summer", Month %in% c(9,10) ~ "Fall")) %>%
  filter(year %in% c(2017:2024), Month %in% c(6:10)) %>%
  group_by(year, Season) %>%
  summarize(X2 = mean(X2, na.rm =T))


WQsuman2 = left_join(WQsuman, X2, by = c("year", "Season")) %>%
  mutate(PercentTime = case_when(Season == "Summer" ~ Suitable/92,
                                 Season == "Fall" ~ Suitable/61))


ggplot(WQsuman2, aes(x = X2, y = PercentTime, color = Season))+
  geom_point()+ geom_smooth(method = "lm")+
  facet_grid(Analyte~region, scales = "free_y")+
  ylab("Percent time habitat is suitable")+
  xlab("average seasonal X2")

#Let's break it out by summer vsersus fall

