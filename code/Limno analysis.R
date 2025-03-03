#Limnoithona analysis


library(conflicted)
library(MASS)
library(dplyr)
library(zooper)
library(lubridate)
library(readr)
library(tidyr)
library(ggplot2)
library(sf)
library(readxl)
library(stringr)
library(mgcv)
library(purrr)
library(deltamapr)
library(scales)
library(here)
library(DHARMa)
library(effects)
library(lme4)
library(lmerTest)
conflict_prefer("filter", "dplyr")
conflicts_prefer(lmerTest::lmer)
conflict_prefer("select", "dplyr")

load("Data/SMSCGRegions.RData")

#load zoop data from 1972-2010
zoop_data<-Zoopsynther(Data_type="Community", Sources=c("EMP", "STN", "20mm", "FMWT"), 
                       Time_consistency = FALSE, Years = c(1972:2010), Size_class = "Meso")


load("data/Dayflow_allw2023.RData")

DF = Dayflow %>%
  mutate(Month = month(Date)) %>%
  group_by( Year, Month) %>%
  filter(OUT >1) %>%
  summarize(OUT = mean(OUT, na.rm =T)) 



#Mass conversions
zoop_mass_conversions<-read_excel(here("Data/Biomass conversions.xlsx"), sheet="Micro and Meso-zooplankton")%>%
  mutate(Taxname=case_when(Taxname=="Sinocalanus"~"Sinocalanus doerrii", # Change to help this match to zoop data
                           TRUE ~ Taxname),
         Taxlifestage=paste(Taxname, Lifestage))%>%
  select(Taxlifestage, CarbonWeight_ug)

#Read in zoop groupings
zoop_groups<-read_csv(here("Data/zoopcrosswalk2.csv"), col_types=cols_only(Taxlifestage="c", IBMR="c"))%>%
  distinct()
#Process the zoop data

limno_data_mass<-zoop_data%>%
  mutate(Taxlifestage=str_remove(Taxlifestage, fixed("_UnID")))%>%
  filter(Taxlifestage %in% c("Limnoithona tetraspina Adult", "Limnoithona_UnID Adult")) %>%
  left_join(zoop_mass_conversions, by="Taxlifestage")%>% # Add biomass conversions
  
  mutate(BPUE= CPUE*CarbonWeight_ug)%>% # Create 1 BPUE variable
  filter(!is.na(BPUE) & !is.na(Latitude) & !is.na(Longitude) & !is.na(SalSurf))%>% # Removes any data without BPUE, which is currently restricted to Rotifera Adult, Copepoda Larva, and H. longirostris from STN. Also removes 20mm and EMP EZ stations without coordinates
  select(SampleID, Station, Latitude, Longitude, SalSurf, Date, Year, BPUE)%>%
  group_by(across(-BPUE))%>%
  summarise(BPUE=sum(BPUE), .groups="drop")%>% # Sum each IBMR categories
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326)%>%
  st_transform(crs=st_crs(Regions)) %>% 
  st_join(select(Regions, Region)) %>%
  st_drop_geometry() %>% 
  filter(!is.na(Region))%>%
  mutate(doy=yday(Date), #Day of year
         Month=month(Date), # Month
         Year_fac=factor(Year), # Factor year for model random effect
         Station_fac=factor(Station), # Factor station for model random effect
         across(c(SalSurf, doy), list(s=~(.x-mean(.x))/sd(.x))), # Center and standardize predictors
         BPUE_log1p=log(BPUE+1)) # log1p transform BPUE for model


ggplot(limno_data_mass, aes(x = doy, y = BPUE_log1p)) + geom_smooth()+
  scale_x_continuous(breaks = c(153, 183, 214), labels = c("Jun", "Jul", "Aug"))

####### Prediction model ###############################

#Set up prediction data for model

# Min year to start models
year_min<-1995
#do I want to start it ealier? Not sure. Leave it at this for now


newdata_function<-function(region, data=limno_data_mass, quant=0.99){
  
  lower<-(1-quant)/(2)
  upper<-1-lower
  
  data_filt<-data%>%
    filter(Region%in%region & Year >= year_min)
  
  # Calculate monthly quantiles of salinity
  month_sal<-data_filt%>%
    group_by(Month)%>%
    summarise(l=quantile(SalSurf, lower),
              u=quantile(SalSurf, upper), .groups="drop")
  
  newdata<-expand_grid(date=mdy(paste(1:12, 15, 2001, sep="/")), # The 15th of each month on a non-leap year
                       SalSurf=seq(round(min(data_filt$SalSurf), 1), 
                                   round(max(data_filt$SalSurf), 1), by=0.1))%>% # Salinity sequence nicely rounded to 1 decimal
    mutate(Month=month(date),
           doy=yday(date), # Day of year
           SalSurf_s=(SalSurf-mean(data$SalSurf))/sd(data$SalSurf), # center and standardize salinity to match data
           doy_s=(doy-mean(data$doy))/sd(data$doy))%>% # center and standardize doy to match data
    left_join(month_sal, by="Month")%>%
    filter(SalSurf >= l & SalSurf <= u)%>% # Remove any salinity values outside the quantiles for each month
    select(Month, doy, doy_s, SalSurf, SalSurf_s)
  
}

newdatalimno<-map(set_names(unique(limno_data_mass$Region)), newdata_function)

####################
## Posterior prediction function #######################################
# I am just copying htis from Sam's code, i don't know what it does

lt_model<-function(region,new_data=newdatalimno){
  
  cat("<<<<<<<<<<<<<<<<<<<<<<< modeling", region, ">>>>>>>>>>>>>>>>>>>>>>>>>\n\n")
  
  new_data<-new_data[[region]]
  
  data<-filter(limno_data_mass,Region==region & Year>=year_min)
  
  par(mfrow=c(2,2))
  
  if(length(unique(data$Station_fac))>1){
    model<-gam(BPUE_log1p ~ te(SalSurf_s, doy_s, k=c(5,5), bs=c("cs", "cc")) + 
                 s(Year_fac, bs="re") + s(Station_fac, bs="re"),
               data=data, 
               method="REML")
    
    random_effects<-c("s(Year_fac)", "s(Station_fac)")
    
  }else{
    
    model<-gam(BPUE_log1p ~ te(SalSurf_s, doy_s, k=c(5,5), bs=c("cs", "cc")) + 
                 s(Year_fac, bs="re"),
               data=data, 
               method="REML")
    
    random_effects<-c("s(Year_fac)")
  }
  
  cat("-------------gam check-------------\n")
  gam.check(model)
  
  cat("\n\n-------------summary-------------\n")
  print(summary(model))
  
  sal<-predict_posterior(model, new_data, random_effects)%>%
    bind_cols(new_data%>% # Add covariate columns before these columns
                select(-doy_s, -SalSurf_s), 
              .)
  return(sal)
}


#Apply model to all regions
#sam has  a slick way of putting all this together, but I can't figure it out.

lt_grizz = lt_model("Grizzly Bay") %>%
  mutate(Region = "Grizzly Bay")
lt_SM = lt_model("Suisun Marsh")%>%
  mutate(Region = "Suisun Marsh")
lt_SB = lt_model("Suisun Bay")%>%
  mutate(Region = "Suisun Bay")
lt_Riv = lt_model("River")%>%
  mutate(Region = "River")

lt_conversions = bind_rows(lt_grizz, lt_SM, lt_SB, lt_Riv)

#calculate the mean and quartiles for each salinity bin, month, and region
lt_conversions_sum<-apply(select(lt_conversions, starts_with("draw_")), 1, 
                          function(x) quantile(x, c(0.025, 0.5, 0.975)))

#set it up as a dataframe for plotting
lt_conversions_plot<-lt_conversions%>%
  select(-starts_with("draw_"))%>%
  bind_cols(tibble(l95=lt_conversions_sum["2.5%",], 
                   median=lt_conversions_sum["50%",], 
                   u95=lt_conversions_sum["97.5%",]))

#Plot it! - this is based on 1995-2010
ggplot(lt_conversions_plot, aes(x=SalSurf, y=median, ymin=l95, ymax=u95))+
  geom_ribbon(alpha=0.4)+
  geom_line()+
  ylab("Limnoithona biomass (log scale)")+
  facet_grid(Region~month(Month, label=T))+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))

#summer fall only
ggplot(filter(lt_conversions_plot, Month %in% c(6:10)), aes(x=SalSurf, y=median, ymin=l95, ymax=u95))+
  geom_ribbon(alpha=0.4)+
  geom_line()+
  ylab("Limnoithona biomass (log scale)")+
  facet_grid(Region~month(Month, label=T))+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))



######now just the linear model

#### Load Data ####################
yrs = read_csv("data/wtryrtype.csv") %>%
  rename(YrType = `Yr-type`, Year = WY)

load("data/SMSCGRegions.RData")
load("data/Dayflow_allw2023.RData")
DF = filter(Dayflow, Year >2009) %>%
  mutate(Month = month(Date)) %>%
  select(Date, Month, Year, OUT, X2)

limnos = Zoopsynther(Data_type = "Community", Years = c(2011:2023), 
                      Sources = c("EMP", "FMWT", "STN", "20mm", "DOP"),
                      Size_class = c("Meso"),
                      Month = c(6:10)) %>%
  filter(Genus == "Limnoithona", Lifestage == "Adult")

limnox = group_by(limnos, SampleID, Latitude, Station, Longitude, SalSurf, TowType, Source, Date, Year) %>%
  summarize(CPUE = sum(CPUE)) %>%
  left_join(yrs) %>%
  left_join(DF) %>%
  mutate(logCPUE = log(CPUE+1))

limnoreg = limnox %>%
  filter(!is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(Regions))%>%
  st_join(Regions) %>%
  st_drop_geometry() %>%
  filter(!is.na(Region))

hist(limnoreg$CPUE)
hist(log(limnoreg$CPUE))

ggplot(limnoreg, aes(x = X2, y = logCPUE))+
  geom_point(aes(color = Month))+
  facet_wrap(~Region)+
  geom_smooth(method = "lm")

######## start with X2 ############# 
lm1limno = lmer(logCPUE ~ X2*Region + (1|Month) + (1|Year), data = limnoreg)
summary(lm1limno)
plot(lm1limno)
plot(simulateResiduals(lm1limno))
plot(allEffects(lm1limno))
testZeroInflation(lm1limno)
#outlier test significant, KS test significant. But it doesn't look terrible.
#also zero inflated, not surprsiing

#what about a zero inflation model?
library(glmmTMB)
zm1 = glmmTMB(logCPUE ~ X2*Region + (1|Month) + (1|Year), 
              ziformula =~ SalSurf,data = limnoreg) 

summary(zm1)
plot(simulateResiduals(zm1))
testZeroInflation(zm1)
plot(allEffects(zm1))
#still zero inflated, not sur what to do about that
#but otherwise an OK model



zm3 = glmmTMB(CPUE ~ X2*Region + (1|Month) + (1|Year),  family=nbinom2, data = limnoreg) 
summary(zm3)
plot(simulateResiduals(zm3))
testZeroInflation(zm3)
plot(allEffects(zm3))
#yeah, just leave out the zero inflation so long as you have the negative binomial. 
#all of these results match our expectations pretty darn well anyway.
