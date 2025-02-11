#How often do we hit the Polanksy et al summer flow threshold?

library(tidyverse)

# The approximate values at which P λt > 50% are 15.8C
# for spring temperature, 18 biomass per unit volume (BPUV) of spring adult copepod and mysid (ACM), 
# 25 million (MM) m3/day for summer Delta outflow, 68cm for fall Secchi depth, 190m3/sec for winter OMR flows, 
# and 60cm for winter south Delta Secchi depth (Figure 3).

#June-August delta outflow of 25 million cubic meters per day
#translatest: 25,000,000 m3/day * 1day/86400sec * 35.3 ft3/m3 = 10,214 cfs

load("data/Dayflow_allw2023.RData")
yrs = read_csv("data/wtryrtype.csv") %>%
  rename(YrType = `Yr-type`, Year = WY) %>%
  mutate(YrType = factor(YrType, levels = c("C", "D", "BN", "AN", "W")))

#What's the average summer outflow?

Dayf = mutate(Dayflow, Month = month(Date)) %>%
  filter(Year >1999, Month %in% c(6,7,8), OUT >0) %>%
  group_by(Year) %>%
  summarize(OUT = mean(OUT, na.rm =T), X2m = mean(X2, na.rm =T), maxX2 = max(X2, na.rm =T),
            minX2 = min(X2, na.rm =T)) %>%
  left_join(yrs)

ggplot(Dayf, aes(x = Year, y = OUT, fill = YrType))+
  geom_col(color = "black")+
  geom_hline(yintercept = 10214)+
  geom_hline(yintercept = 15000, linetype =2)+
  scale_fill_manual(values = c("firebrick3", "darkorange", "yellow2", "palegreen2", "skyblue"))+
  ylab("Average summer outflow, cfs")

Vol10000cfs = 10214*86400*92/43560
Vol15000cfs = 15214*86400*92/43560

Dayf = mutate(Dayf, SummerVolume = OUT*86400*92/43560, VolumeDiff = Vol10000cfs-SummerVolume,
              VolumeDiff = case_when(VolumeDiff <0 ~ 0,
                                     TRUE ~ VolumeDiff),
              VolumeDiff5 = Vol15000cfs-SummerVolume,
              VolumeDiff5 = case_when(VolumeDiff5 <0 ~ 0,
                                     TRUE ~ VolumeDiff5)) 

ggplot(Dayf, aes(x = Year, y = VolumeDiff/1000, fill = YrType))+
  geom_col(color = "black")+
  scale_fill_manual(values = c("firebrick3", "darkorange", "yellow2", "palegreen2", "skyblue"))+
  ylab("Additional Volume needed in TAF")+
  coord_cartesian(ylim = c(0,2100))


ggplot(Dayf, aes(x = Year, y = VolumeDiff5/1000, fill = YrType))+
  geom_col(color = "black")+
  scale_fill_manual(values = c("firebrick3", "darkorange", "yellow2", "palegreen2", "skyblue"))+
  ylab("Additional Volume needed in TAF")+
  coord_cartesian(ylim = c(0,2100))


ggplot(Dayf, aes(x = Year, y = X2m, color = YrType))+
  geom_point()+
  geom_errorbar(aes(ymin = minX2, ymax = maxX2))+
  scale_color_manual(values = c("firebrick3", "darkorange", "yellow3", "palegreen2", "skyblue"))+
  theme_bw()


ggplot(Dayf, aes(x = X2m, y = OUT)) + geom_point()+ geom_smooth()


######################################################
#What is the salinity field like in the summer of wet and dry years?

sal = cdec_query(c("BDL", "CSE", "MRZ", "PTO", "RVB"), 100, "E", start.date = ymd("2000-01-01"), end.date = today())
salx = cdec_query(c("RYC", "GZL", "MAL"), 100, "E", start.date = ymd("2000-01-01"), end.date = today())

sal = bind_rows(sal, salx)

saldaily = mutate(sal, Date = date(ObsDate), Year = year(Date), DOY = yday(Date)) %>%
  filter(Value>0) %>%
  group_by(Date, Year, DOY, StationID) %>%
  summarize(Sal = mean(Value, na.rm =T)) %>%
  mutate(Salinity = ec2pss(Sal/1000, 25)) %>%
  left_join(yrs) %>%
  filter(Year != 2025) %>%
  mutate(Station = factor(StationID, levels = c("MRZ", "GZL", "RYC", "BDL", "MAL", "CSE", "RVB"),
                          labels = c("Martinez", "Grizzly Bay", "Ryer Island", "Belden's Landing",
                                     "Mallard Island", "Collinsville", "Rio Vista")))

ggplot(saldaily, aes(x = DOY, y = Salinity, group = Year, color = YrType)) + geom_line()+
  facet_wrap(~Station, nrow =4)+
  coord_cartesian(xlim = c(150, 270))+
  scale_x_continuous(breaks = c(153,183, 214, 245, 275), labels = c("Jun", "Jul", "Aug", "Sep", "Oct"))+
  geom_hline(yintercept = 6, linetype =2, color = "black", size =1)+
  geom_vline(xintercept = c(153, 245))+
  scale_color_manual(values = c("firebrick3", "darkorange", "yellow3", "green3", "skyblue2"))+
  theme_bw() + ylab("Daily average Salinity (PSU)") + xlab("Day of Year")
