#OK, how is X2 in Dayflow versus teh CDEC X2?

library(cder)
library(tidyverse)
library(zoo)
library(wql)

load("data/Dayflow_allw2023.RData")

X2cdec = cdec_query("CX2", 145, start.date = ymd("2000-01-01"), end.date = ymd("2023-09-01"))

cdX2 = X2cdec %>%
  rename(CDEC = Value) %>%
  mutate(Date = date(DateTime))

dfx2 = select(Dayflow,Year, Date, X2) %>%
  filter(Year >1999)

bothX2 = left_join(cdX2, dfx2) %>%
  mutate(Month = month(Date), DOY = yday(Date), Week = week(Date),
         X2weekly = rollmean(CDEC, k =7, na.rm =T, na.pad = T))


X2weekly2 = group_by(bothX2, Week, Year) %>%
  summarize(CDEC = mean(CDEC, na.rm =T), X2 = mean(X2, na.rm =T))

ggplot(bothX2, aes(x = X2, y = CDEC, color = Month)) + geom_point()+
  facet_wrap(~Year) +
  geom_abline(slope =1)+
  ylab("X2 from CDEC station CX2")+
  xlab("X2 from Dayflow")


ggplot(filter(bothX2, Month %in% c(6:10)), aes(x = X2, y = CDEC, color = Month)) + geom_point()+
  facet_wrap(~Year) +
  geom_abline(slope =1)+
  ylab("X2 from CDEC station CX2")+
  xlab("X2 from Dayflow")


ggplot(bothX2)+
 geom_line(aes(x = DOY, y = CDEC), color = "red") + 
  geom_line(aes(x = DOY, y = X2), color = "black") +
  facet_wrap(~Year) +
  ylab("X2")+
  xlab("Day of year")

ggplot(bothX2)+
  geom_line(aes(x = DOY, y = X2weekly), color = "red") + 
  geom_line(aes(x = DOY, y = X2), color = "black") +
  facet_wrap(~Year) +
  ylab("X2")+
  xlab("Day of year")

ggplot(X2weekly2)+
  geom_line(aes(x = Week, y = CDEC), color = "red") + 
  geom_line(aes(x = Week, y = X2), color = "black") +
  facet_wrap(~Year) +
  ylab("X2")+
  xlab("Day of year")

######## Check some of the stations that can give us an estimate ####
# The X2 position is estimated by the Projects in the lower Sacramento River 
# when the daily average EC is below 2.64 mS/cm at Collinsville and above 2.64 mS/cm at Martinez.  
# The three stations identified in the State Water Resources Control Board’s 2000 Revised Water 
# Rights Decision 1641 (Collinsville, Chipps Island, and Port Chicago) as well as the station at 
# Martinez are used to estimate the X2 location.  Those stations are located at 81 km, 74 km, 64 km, 
# and 56 km east of the Golden Gate Bridge, respectively.  To calculate the X2 location, the Projects 
# interpolate between the two stations where EC at the downstream location is above 2.64 mS/cm and 
# EC at the upstream location is below 2.64 mS/cm.  To calculate the monthly average X2 location, 
# the Projects conduct the same interpolation on the monthly average EC for the stations that bound 2.64 mS/cm.

#OK, so start with Martinez (MRZ 56), Port Chigaco (PCT 64), Chipps (PTS 74) and Collinsville (CSE 81) 

#Other stations to try are C10, C05, EMM (95 Decker), RVB (100 RIo Vista)

X2stats = cdec_query(c("MRZ", "PCT", "PTS", "CSE", "EMM", "RVB"), 100, 
                     start.date = ymd("2010-01-01"), end.date = today())

stations = data.frame(StationID = c("MRZ", "PCT", "PTS", "CSE", "EMM", "RVB"),
             RKI = c(56, 64, 74, 81, 94, 100))

#So 3.8 mS/cm is about 2 PSU
ec2pss(3.8, 25)

str(X2stats)

#Daily averages
X2stadaily = X2stats %>%
  filter(Duration == "E") %>%
  mutate(Date = date(DateTime)) %>%
  group_by(StationID, Date) %>%
  summarize(EC = mean(Value, na.rm =T)) %>%
  left_join(stations)

test = filter(X2stadaily, Date == ymd("2020-06-01"))
ggplot(test, aes(x = RKI, y = EC))+ geom_point()+ geom_smooth()


#function to interpolate X2
X2interp = function(X2daily) {
  x2lm = loess(EC ~ RKI, data = X2daily)
  RKIs = data.frame(RKI = c(50:100))
  Xs = mutate(RKIs, ppred = predict(x2lm, RKIs)/1000) %>%
    mutate(ppred2 = ppred-2.64) 
  X2 = filter(Xs, ppred2 == min(abs(ppred2), na.rm =T))$RKI
  return(X2)
}

#OK, now daily X2, not sure if this is quite the way to do it.

test = filter(X2stadaily, year(Date) == 2023, month(Date) == 9, !is.nan(EC))

X2s = test %>%
  group_by(Date) %>%
  do(X2 = X2interp(.))
#meh, i don't know why this isn't working.

###outflow################

#color code good years
yrs = read_csv("data/wtryrtype.csv") 
yearsYes = c(2005, 2006, 2011, 2017, 2019, 2023)
yearsYes15 = c(2006, 2011, 2017, 2019, 2023)
  

Dayflowrecent = filter(Dayflow, year(Date) >2000, OUT>0)  %>%
  mutate(DOY = yday(Date), Good = case_when(Year %in% yearsYes ~ TRUE,
                                            TRUE ~ FALSE),
         Good15 = case_when(Year %in% yearsYes15 ~ TRUE,
                            TRUE ~ FALSE)) %>%
  left_join(yrs, by =c("Year" = "WY")) %>%
  mutate(YrType = factor(`Yr-type`, levels = c("C", "D", "BN", "AN", "W")))

ggplot(Dayflowrecent, aes(x = DOY, y = OUT, group = as.factor(Year), color = YrType, linetype =  Good))+ 
  geom_line(size = 1)+
  coord_cartesian(xlim = c(150, 300), ylim = c(0, 50000))+
  scale_x_continuous(breaks = c(153,183, 214, 245, 275), labels = c("Jun", "Jul", "Aug", "Sep", "Oct"))+
  geom_hline(yintercept = 10000, color = "black", linetype =4, size =1)+
  scale_color_manual(values = c("firebrick4", "firebrick2", "orange", "springgreen3", "dodgerblue"), name = "Year Type")+
  xlab("Sacramento Valley Water Year Index")+
  scale_linetype_manual(values = c(1,2), labels = c("No", "Yes"), name = c("Do we hit \nthe 10K CFS \nThreshold?"))+
  ylab("Daily average Outflow (CFS)")+
  theme_bw()




ggplot(Dayflowrecent, aes(x = DOY, y = OUT, group = as.factor(Year), color = YrType, linetype = Good15))+ 
  geom_line(size = 1)+
  coord_cartesian(xlim = c(150, 300), ylim = c(0, 50000))+
  scale_x_continuous(breaks = c(153,183, 214, 245, 275), labels = c("Jun", "Jul", "Aug", "Sep", "Oct"))+
  geom_hline(yintercept = 15000, color = "black", linetype =2)+
  scale_color_manual(values = c("firebrick4", "firebrick2", "orange", "springgreen3", "dodgerblue"), name = "Year Type")+
  xlab("Sacramento Valley Water Year Index")+
  scale_linetype_manual(values = c(1,2), labels = c("No", "Yes"), name = c("Do we hit \nthe 15K CFS \nThreshold?"))+
  ylab("Daily average Outflow (CFS)")+
  theme_bw()



ggplot(Dayflowrecent, aes(x = DOY, y = X2, group = as.factor(Year), color = Good))+ geom_line()+
  coord_cartesian(xlim = c(150, 300), ylim = c(50, 100))+
  scale_x_continuous(breaks = c(153,183, 214, 245, 275), labels = c("Jun", "Jul", "Aug", "Sep", "Oct"))+
  geom_hline(yintercept = 10000, color = "black", linetype =2)

#####################################
#break it out by month
Dayflowmonthly = mutate(Dayflowrecent, Month = month(Date)) %>%
  filter(Month %in% c(6:10)) %>%
  group_by(Month, Year, Good, Good15, YrType, Index) %>%
  summarize(OUT = mean(OUT, an.rm =T))


ggplot(Dayflowmonthly, aes(x = as.factor(Month), y = OUT, fill = YrType))+
  geom_boxplot()


ggplot(Dayflowmonthly, aes(x = as.factor(Month), y = OUT, fill = Good))+
  geom_boxplot()

ggplot(Dayflowmonthly, aes(x = as.factor(Month), y = OUT, fill = Good15))+
  geom_boxplot()

#OK, what are our monthly averages over time?
ggplot(Dayflowmonthly, aes(x = as.factor(Month), y = OUT, fill = Good))+
  geom_col()+
  facet_wrap(~Year)+
  geom_text(aes(y = 10000, label = round(OUT/1000)))

ggplot(Dayflowmonthly, aes(x = as.factor(Month), y = OUT, fill = YrType))+
  geom_col()+
  facet_wrap(~Year)+
  geom_text(aes(y = 10000, label = round(OUT/1000)))

YRs2 = data.frame(Year = c(2001:2023), YrTypeMay1 = c("D", "D", "AN", "BN", "BN", "W", "D", "C",
                                                      "D", "BN", "W", "BN", "D", "C", "C", "BN", "W", "BN", "W",
                                                      "D", "C", "C", "W"))


#Outflow from 2024
Out2024 = cdec_query("DTO", 23, start.date = ymd("2023-10-01"), end.date = ymd("2024-10-31"))

Out2024monthly = Out2024 %>%
  mutate(Year = year(DateTime), Month = month(DateTime)) %>%
  group_by(Month, Year) %>%
  summarize(OUT = mean(Value)) %>%
  mutate(YrTypeMay1 = case_when(Year == 2023 ~ "W",
                                Year == 2024 ~ "AN")) %>%
  filter(Month %in% c(6:10))

DayflowmonthlyX = left_join(Dayflowmonthly, YRs2) %>%
  bind_rows(Out2024monthly)

ggplot(DayflowmonthlyX, aes(x = as.factor(Month), y = OUT/1000, fill = YrTypeMay1))+
  geom_col()+
  facet_wrap(~Year)+
  geom_text(aes(y = 10, label = round(OUT/1000)))+
  xlab("Month")+
  ylab("Mean Monthly Outflow (thousand cfs)")


######unimpared flow#############

Unimpaired = cdec_query("8SI", 2, "D", start.date = ymd("2000-01-01"), end.date = today())


##other factors #######################

#so, the benifits of summer flow are also dependent on Winter OMR,
#Spring zooplankton, fall secchi depth, winter secchi depth, 
#spring temperature

#USe the data that wre inputs to the life cycle model in Polansky et al: https://portal.edirepository.org/nis/mapbrowse?packageid=edi.1433.2

LSM = read_csv("data/DSLCMinputs_2024.csv")
names(LSM)

LSM = LSM %>%
  left_join(yrs, by = c("Cohort_Year" = "Year")) %>%
  mutate(YrType = factor(YrType, levels = c("C", "D", "BN", "AN", "W")))

ggplot(LSM, aes(x = Cohort_Year, y = Temperature_meanofmeans_Mar0May0, fill = YrType))+
  geom_col()


ggplot(LSM, aes(x = Cohort_Year, y = NJ_BPUV_Mar0May0, fill = YrType))+
  geom_col()

ggplot(LSM, aes(x = Cohort_Year, y = Secchi_meanofmeans_Sep0Nov0, fill = YrType))+
  geom_col()

LSMcrit = select(LSM, Year = "Cohort_Year", SpringTemp = "Temperature_meanofmeans_Mar0May0",
                 FallSecchi = "Secchi_meanofmeans_Sep0Nov0", WinterSecchi = "SouthSecchi_meanofmeans_Dec0Mar1",
                 SummerFlow = "Outflow_Jun0Aug0_daily", SpringZoops = "NJ_BPUV_Mar0May0", 
                 WinterOMR = "OMR_Dec0Mar1_metric",YrType, Index) %>%
  pivot_longer(cols = c(SpringTemp,
                        FallSecchi, WinterOMR,
                        WinterSecchi,
                        SpringZoops, SummerFlow),
               names_to = "Metric", values_to = "Value") %>%
  mutate(YrType = factor(YrType, levels = c("C", "D", "BN", "AN", "W")))

ggplot(LSMcrit, aes(x = YrType, y = Value))+
  geom_boxplot()+
  facet_wrap(~Metric, scales = "free_y")

ggplot(LSMcrit, aes(x = Index, y = Value))+
  geom_point(aes(color= YrType))+ geom_smooth(method = "lm")+
  facet_wrap(~Metric, scales = "free_y")

BetterWorse = data.frame(Metric = c("SpringTemp",
                                    "FallSecchi", "WinterOMR",
                                    "WinterSecchi",
                                    "SpringZoops", "SummerFlow"),
                         Better = c("Lower is better",
                                    "Lower is better",
                                    "Higher is better",
                                    "Higher is better",
                                    "Higher is better",
                                    "Higher is better"),
                         yval = c(17.5, 92, 290, 142, 2200, 100))

ggplot(LSMcrit, aes(x = Index, y = Value))+
  geom_point(aes(color= YrType))+ geom_smooth(method = "lm")+
  facet_wrap(~Metric, scales = "free_y")+
  geom_text(data =BetterWorse, aes(x = 9, y = yval, label = Better))+
  theme_bw()+
  scale_color_manual(values = c("firebrick4", "firebrick2", "orange", "springgreen3", "dodgerblue"), name = "Year Type")+
  xlab("Sacramento Valley Water Year Index")


