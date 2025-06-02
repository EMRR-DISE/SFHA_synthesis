#New zoop-salinity GAMs

#Start with just Pseudodiaptomus

#Fit a model with 1995-2010, then see how actual values from 2011-2024 work.

#How do action years and non-action years compare in terms of relationship to the model?

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
library(patchwork)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

load("Data/SMSCGRegions.RData")
# 
# #load zoop data from 1972-2010
# zoop_data<-Zoopsynther(Data_type="Community", Sources=c("EMP", "STN", "20mm", "FMWT"), 
#                        Time_consistency = FALSE, Years = c(1972:2010), Size_class = "Meso")
# 
# 
load("data/dayflow_w2024.RData")
# 
# DF = Dayflow %>%
#   mutate(Month = month(Date)) %>%
#   group_by( Year, Month) %>%
#   filter(OUT >1) %>%
#   summarize(OUT = mean(OUT, na.rm =T)) 
# 
# 
# 
# #Mass conversions
# zoop_mass_conversions<-read_excel(here("Data/Biomass conversions.xlsx"), sheet="Micro and Meso-zooplankton")%>%
#   mutate(Taxname=case_when(Taxname=="Sinocalanus"~"Sinocalanus doerrii", # Change to help this match to zoop data
#                            TRUE ~ Taxname),
#          Taxlifestage=paste(Taxname, Lifestage))%>%
#   select(Taxlifestage, CarbonWeight_ug)
# 
# #Read in zoop groupings
# zoop_groups<-read_csv(here("Data/zoopcrosswalk2.csv"), col_types=cols_only(Taxlifestage="c", IBMR="c"))%>%
#   distinct()
# 
# #Process the zoop data
# 
# pseudo_data_mass<-zoop_data%>%
#   mutate(Taxlifestage=str_remove(Taxlifestage, fixed("_UnID")))%>%
#   filter(Taxlifestage %in% c("Pseudodiaptomus forbesi Adult", "Pseudodiaptomus Adult",
#                              "Pseudodiaptomus Juvenile")) %>%
#            
#            mutate(Taxlifestage = recode(Taxlifestage,#
#                              `Pseudodiaptomus Adult`="Pseudodiaptomus forbesi Adult",
#                              `Acanthocyclops vernalis Adult`="Acanthocyclops Adult"))%>%
#   left_join(zoop_mass_conversions, by="Taxlifestage")%>% # Add biomass conversions
#   
#   mutate(BPUE= CPUE*CarbonWeight_ug)%>% # Create 1 BPUE variable
#   filter(!is.na(BPUE) & !is.na(Latitude) & !is.na(Longitude) & !is.na(SalSurf))%>% # Removes any data without BPUE, which is currently restricted to Rotifera Adult, Copepoda Larva, and H. longirostris from STN. Also removes 20mm and EMP EZ stations without coordinates
#    select(SampleID, Station, Latitude, Longitude, SalSurf, Date, Year, BPUE)%>%
#   group_by(across(-BPUE))%>%
#   summarise(BPUE=sum(BPUE), .groups="drop")%>% # Sum each IBMR categories
#   st_as_sf(coords=c("Longitude", "Latitude"), crs=4326)%>%
#   st_transform(crs=st_crs(Regions)) %>% 
#   st_join(select(Regions, Region)) %>%
#   st_drop_geometry() %>% 
#   filter(!is.na(Region))%>%
#   mutate(doy=yday(Date), #Day of year
#          Month=month(Date), # Month
#          Year_fac=factor(Year), # Factor year for model random effect
#          Station_fac=factor(Station), # Factor station for model random effect
#          across(c(SalSurf, doy), list(s=~(.x-mean(.x))/sd(.x))), # Center and standardize predictors
#          BPUE_log1p=log(BPUE+1)) # log1p transform BPUE for model
# 
# save(pseudo_data_mass, file = "data/pseudo_data_mass.RData")
load("data/pseudo_data_mass.RData")

ggplot(pseudo_data_mass, aes(x = doy, y = BPUE_log1p)) + geom_smooth()+
  scale_x_continuous(breaks = c(153, 183, 214), labels = c("Jun", "Jul", "Aug"))+
  coord_cartesian(xlim = c(153, 230))

ggplot(pseudo_data_mass, aes(x = doy, y = BPUE)) + geom_smooth()+
  scale_x_continuous(breaks = c(153, 183, 214), labels = c("Jun", "Jul", "Aug"))+
  coord_cartesian(xlim = c(153, 230))

####### Prediction model ###############################

#Set up prediction data for model

# Min year to start models
year_min<-1995
#do I want to start it ealier? Not sure. Leave it at this for now


newdata_function<-function(region, data=pseudo_data_mass, quant=0.99){
  
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

newdata<-map(set_names(unique(pseudo_data_mass$Region)), newdata_function)

####################
## Posterior prediction function #######################################
# I am just copying htis from Sam's code, i don't know what it does

# Function to generate posterior predictions from a gam model
# From https://stats.stackexchange.com/questions/190348/can-i-use-bootstrapping-to-estimate-the-uncertainty-in-a-maximum-value-of-a-gam
predict_posterior<-function(model, newdata, exclude, n=1e3, seed=999){
  Xp <- predict(model, newdata=newdata, 
                #type="lpmatrix", 
                type = "response",
                exclude=exclude, newdata.guaranteed=TRUE) ## map coefs to fitted curves
  beta <- coef(model)
  Vb   <- vcov(model) ## posterior mean and cov of coefs
  set.seed(seed)
  mrand <- mvrnorm(n, beta, Vb) ## simulate n rep coef vectors from posterior
  pred<-matrix(nrow=nrow(newdata), ncol=n)
  ilink <- family(model)$linkinv
  for (i in seq_len(n)) { 
    pred[,i]   <- ilink(Xp %*% mrand[i, ])
  }
  colnames(pred)<-paste("draw", 1:n, sep="_")
  pred<-as_tibble(pred)
  return(pred)
}

ps_model<-function(region,new_data=newdata, datain = pseudo_data_mass){
  
  cat("<<<<<<<<<<<<<<<<<<<<<<< modeling", region, ">>>>>>>>>>>>>>>>>>>>>>>>>\n\n")
  
  new_data<-new_data[[region]]
  
  data<-filter(datain,Region==region & Year>=year_min)
  
  par(mfrow=c(2,2))
  
  if(length(unique(data$Station_fac))>1){
    model<-gam(BPUE_log1p ~ te(SalSurf_s, doy_s, k=c(5,5), bs=c("cs", "cs")) + 
                 s(Year_fac, bs="re") + s(Station_fac, bs="re"),
               data=data, 
               method="REML")
    
    random_effects<-c("s(Year_fac)", "s(Station_fac)")
    
  }else{
    
    model<-gam(BPUE_log1p ~ te(SalSurf_s, doy_s, k=c(5,5), bs=c("cs", "cs")) + 
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

ps_grizz = ps_model("Grizzly Bay", datain = filter(zoopsflow, Month %in% c(6:10))) %>%
  mutate(Region = "Grizzly Bay")
ps_SM = ps_model("Suisun Marsh", datain = filter(zoopsflow, Month %in% c(6:10)))%>%
  mutate(Region = "Suisun Marsh")
ps_SB = ps_model("Suisun Bay", datain = filter(zoopsflow, Month %in% c(6:10)))%>%
  mutate(Region = "Suisun Bay")
ps_Riv = ps_model("River", datain = filter(zoopsflow, Month %in% c(6:10)))%>%
  mutate(Region = "River")


ps_conversions = bind_rows(ps_grizz, ps_SM, ps_SB, ps_Riv)

#calculate the mean and quartiles for each salinity bin, month, and region
ps_conversions_sum<-apply(select(ps_conversions, starts_with("draw_")), 1, 
                           function(x) quantile(x, c(0.025, 0.5, 0.975)))

#set it up as a dataframe for plotting
ps_conversions_plot<-ps_conversions%>%
  select(-starts_with("draw_"))%>%
  bind_cols(tibble(l95=ps_conversions_sum["2.5%",], 
                   median=ps_conversions_sum["50%",], 
                   u95=ps_conversions_sum["97.5%",]))

#Plot it! - this is based on 1995-2010
ggplot(ps_conversions_plot, aes(x=SalSurf, y=median, ymin=l95, ymax=u95))+
  geom_ribbon(alpha=0.4)+
  geom_line()+
  ylab("Pseudodiaptomus biomass (log scale)")+
  facet_grid(Region~month(Month, label=T))+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))

#summer fall only
ggplot(filter(ps_conversions_plot, Month %in% c(6:10)), aes(x=SalSurf, y=median, ymin=l95, ymax=u95))+
  geom_ribbon(alpha=0.4, fill = "slateblue")+
  geom_line()+
  ylab("Pseudodiaptomus biomass (log scale)")+
  facet_grid(Region~month(Month, label=T))+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  xlab("Salinity (PSU)")

ggsave("plots/PseudoGAM.tiff", width = 7, height = 6, device = "tiff")

############ More recent data ############################


#load zoop data from 2011:2024
zoop_datarecent <-Zoopsynther(Data_type="Community", Sources=c("EMP", "STN", "20mm", "FMWT", "DOP"), 
                       Time_consistency = FALSE, Years = c(2010:2024), Size_class = "Meso")
#most of the data from 2024 isn't public yet, data from Spenser
smscgto2024 <- read_csv("C:/Users/rhartman/OneDrive - California Department of Water Resources/salinity control gates/SMSCG/Data/smscgto2024_zooplankton_long.csv")

smscgto2024a = mutate(smscgto2024, DOY = yday(Date)) %>%
  filter(Year %in% c(2023, 2024))%>%
  mutate(Taxlifestage=str_remove(Taxlifestage, fixed("_UnID")))%>%
  filter(Taxlifestage %in% c("Pseudodiaptomus forbesi Adult", "Pseudodiaptomus Adult",
                             "Pseudodiaptomus Juvenile")) %>%
  mutate(Taxlifestage = recode(Taxlifestage,#
                               `Pseudodiaptomus Adult`="Pseudodiaptomus forbesi Adult"))

#ok, good, same format, regions are already on there.
#maybe join this after the other stuff has the regions added
#need salinity data
fmwtmeta = read_csv("C:/Users/rhartman/OneDrive - California Department of Water Resources/salinity control gates/SMSCG/Data/SMSCG_CBNet_2018to2024CPUE_03Feb2025.csv")

fmwtmet = fmwtmeta %>%
  select(Project, Date, Station, CondSurf) %>%
  distinct() %>%
  mutate(Source = Project, Station = as.character(Station), 
         SalSurf = ec2pss(CondSurf/1000, 25), Date = mdy(Date))

smscgto2024b = left_join(smscgto2024a, fmwtmet)

################################################

#export data for bioenergetics
# 
# IBMRregions = deltamapr::R_DSIBM %>%
#   filter(SUBREGION %in% c("Lower Sacramento River",  "Confluence", "Suisun Marsh",
#                           "SW Suisun",  "SE Suisun", "NE Suisun", "NW Suisun")) %>%
#   rename(Region = SUBREGION) %>%
#   st_transform(4326)
# 
# 
# #FMWT station GPS coordinates
# FMWTcoords = deltamapr::P_Stations %>%
#   filter(Parameter == "Zooplankton") %>%
#   st_transform(crs = 4326)
# 
# extracoords = data.frame(Source = c("FMWT", "FMWT", "FMWT"),
#                          Station = c("609", "610", "611"),
#                          Latitude = c(	38.16719,38.118805, 38.14387),
#                          Longitude =  c(-121.938,	
#                                       -121.8891944,	-121.90889)) %>%
#   st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove =F) %>%
#   bind_rows(FMWTcoords)
# 
# stas = smscgto2024 %>%
#   filter(Year %in% c(2023, 2024)) %>%
#   select(Station, Source) %>%
#   distinct() %>%
#   left_join(extracoords)
# 
# 
# 
# smscgto2024a2 = mutate(smscgto2024, DOY = yday(Date)) %>%
#   filter(Year %in% c(2023, 2024))%>%
#   mutate(Taxlifestage=str_remove(Taxlifestage, fixed("_UnID"))) %>%
#   mutate(Taxlifestage = recode(Taxlifestage,#
#                                `Pseudodiaptomus Adult`="Pseudodiaptomus forbesi Adult")) %>%
#   left_join(zoop_groups)%>% # 
#   select(SampleID, Station, Source, Month, Date, Year, BPUE, IBMR)%>%
#   group_by(across(-BPUE))%>%
#   summarise(BPUE=sum(BPUE, na.rm =T), .groups="drop") %>% # Sum each IBMR categories
# left_join(stas) 
# 
# smscgto2024a3 = st_join(st_as_sf(smscgto2024a2), select(IBMRregions, Region)) %>%
#   st_drop_geometry() %>%
#   mutate(Region = case_when(Station == "707" ~ "Lower Sacramento River",
#                             TRUE ~ Region))
# 
# sfhazoops = zoop_datarecent%>%
#   mutate(Taxlifestage=str_remove(Taxlifestage, fixed("_UnID"))) %>%
#   mutate(Taxlifestage = recode(Taxlifestage,
#                                `Pseudodiaptomus Adult`="Pseudodiaptomus forbesi Adult",
#                                `Acanthocyclops vernalis Adult`="Acanthocyclops Adult"))%>%
#   left_join(zoop_mass_conversions, by="Taxlifestage")%>% # Add biomass conversions
#   left_join(zoop_groups) %>%
#   mutate(BPUE= CPUE*CarbonWeight_ug)%>% # Create 1 BPUE variable
#   filter(!is.na(BPUE) & !is.na(Latitude) & !is.na(Longitude) & !is.na(SalSurf))%>% # Removes any data without BPUE, which is currently restricted to Rotifera Adult, Copepoda Larva, and H. longirostris from STN. Also removes 20mm and EMP EZ stations without coordinates
#   select(SampleID, Station, Latitude, Longitude, SalSurf, Date, Year, BPUE, IBMR)%>%
#   group_by(across(-BPUE))%>%
#   summarise(BPUE=sum(BPUE, na.rm =T), .groups="drop")%>% # Sum each IBMR categories
#   st_as_sf(coords=c("Longitude", "Latitude"), crs=4326)%>%
#   st_transform(crs=st_crs(IBMRregions)) %>% 
#   st_join(select(IBMRregions, Region)) %>%
#   st_drop_geometry() %>% 
#   filter(!is.na(Region))%>%
#   bind_rows(smscgto2024a3) %>%
#   mutate(doy=yday(Date), #Day of year
#          Month=month(Date), # Month
#          Year_fac=factor(Year), # Factor year for model random effect
#          Station_fac=factor(Station), # Factor station for model random effect
#          across(c(SalSurf, doy), list(s=~(.x-mean(.x))/sd(.x))), # Center and standardize predictors
#          BPUE_log1p=log(BPUE+1)) %>%
#   filter(Month %in% c(6:10))
# 
# ggplot(sfhazoops, aes(x = Year, fill = IBMR)) + geom_bar()+
#   facet_grid(Month~Region)
# 
# table(sfhazoops$Region, sfhazoops$Year, sfhazoops$Month)

#save(sfhazoops, file = "Data/sfhazoops.RData")
load("Data/sfhazoops.RData")
#hmm.... missing data for SW suisun in 2024, but I guess that's what we're going to get.
###################################################


#Process the zoop data
# 
# pseudo_data_mass2<-zoop_datarecent%>%
#   mutate(Taxlifestage=str_remove(Taxlifestage, fixed("_UnID")))%>%
#   filter(Taxlifestage %in% c("Pseudodiaptomus forbesi Adult", "Pseudodiaptomus Adult",
#                              "Pseudodiaptomus Juvenile")) %>%
#   
#   mutate(Taxlifestage = recode(Taxlifestage,#
#                                `Pseudodiaptomus Adult`="Pseudodiaptomus forbesi Adult",
#                                `Acanthocyclops vernalis Adult`="Acanthocyclops Adult"))%>%
#   left_join(zoop_mass_conversions, by="Taxlifestage")%>% # Add biomass conversions
#   
#   mutate(BPUE= CPUE*CarbonWeight_ug)%>% # Create 1 BPUE variable
#   filter(!is.na(BPUE) & !is.na(Latitude) & !is.na(Longitude) & !is.na(SalSurf))%>% # Removes any data without BPUE, which is currently restricted to Rotifera Adult, Copepoda Larva, and H. longirostris from STN. Also removes 20mm and EMP EZ stations without coordinates
#   select(SampleID, Station, Latitude, Longitude, SalSurf, Date, Year, BPUE)%>%
#   group_by(across(-BPUE))%>%
#   summarise(BPUE=sum(BPUE), .groups="drop")%>% # Sum each IBMR categories
#   st_as_sf(coords=c("Longitude", "Latitude"), crs=4326)%>%
#   st_transform(crs=st_crs(Regions)) %>% 
#   st_join(select(Regions, Region)) %>%
#   st_drop_geometry() %>% 
#   filter(!is.na(Region))%>%
#   bind_rows(smscgto2024b) %>%
#   mutate(doy=yday(Date), #Day of year
#          Month=month(Date), # Month
#          Year_fac=factor(Year), # Factor year for model random effect
#          Station_fac=factor(Station), # Factor station for model random effect
#          across(c(SalSurf, doy), list(s=~(.x-mean(.x))/sd(.x))), # Center and standardize predictors
#          BPUE_log1p=log(BPUE+1)) # log1p transform BPUE for model
# 
# #OK, not sure the best way of doing this, but let's do monthly, regional means.
# pseudo_recent = pseudo_data_mass2 %>%
#   left_join(DF) %>%
#   group_by(Month, Region, Year) %>%
#   summarize(SalSurf = mean(SalSurf, na.rm =T), BPUE = mean(BPUE_log1p), OUT = mean(OUT, na.rm =T)) %>%
#   mutate(logout = log(OUT))
# 
# yrs = read_csv("data/wtryrtype.csv") %>%
#   rename(Year = WY) %>%
#   select(Year, WYsum, Index, YrType, Action)
# 
# pseudo_recent = left_join(pseudo_recent, yrs) %>%
#   mutate(YrType = factor(YrType, levels = c("C", "D", "BN", "AN", "W")))
#save(pseudo_data_mass,pseudo_recent,pseudo_data_mass2, file = "data/pseudo_data_mass.RData")
#now add it to the plot of the ealier data
load("data/pseudo_data_mass.RData")

#########plotforitp ammendment #############
ggplot(filter(pseudo_recent, Month%in% c(6:10)),  aes(x = as.factor(Month), y = BPUE, fill = YrType,))+ 
  geom_boxplot()+facet_wrap(~Region)+
  
  scale_fill_manual(values = c("darkred", "orange", "yellow", "springgreen4", "blue" ))+
  theme_bw()+ ylab("Biomass of Pseudodiaptomus")+ xlab("Month of Year")

ggsave("plots/PseudoBiomassbyYrType.png", width = 8, height = 6, device = "png")

##########################
ggplot(filter(ps_conversions_plot, Month %in% c(6:10)), aes(x=SalSurf, y=median, ymin=l95, ymax=u95))+
  geom_ribbon(alpha=0.4)+
  geom_line()+
  ylab("Pseudodiaptomus biomass (log scale)")+
  geom_point(data = filter(pseudo_recent, Month %in% c(6:10)), aes(color = YrType, x = SalSurf, y = BPUE), inherit.aes = F)+
  facet_grid(Region~month(Month, label=T))+
  scale_color_manual(values = c("darkred", "orange", "yellow", "springgreen4", "blue" ))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))



ggsave("plots/PseudoGAM_wresid_salonly.tiff", width = 8, height = 6, device = "tiff")

ggplot(ps_conversions_plot, aes(x=SalSurf, y=median, ymin=l95, ymax=u95))+
  geom_ribbon(alpha=0.4)+
  geom_line()+
  ylab("Pseudodiaptomus biomass (log scale)")+
  geom_point(data = pseudo_recent, aes(color = Action, x = SalSurf, y = BPUE), inherit.aes = F)+
  facet_grid(Region~month(Month, label=T))+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))

### Residuals ##########################################################

#now calculate the difference between model predicted biomass and atual biomass
#I"m not sure the best way, but I think this works
pseudo_recent_wpred = mutate(pseudo_recent, Sal = SalSurf, SalSurf = round(SalSurf, digits =1)) %>%
  left_join(ps_conversions_plot) %>%
  mutate(residual = median - BPUE,
         YrType = factor(YrType, levels = c("C", "D", "BN", "AN", "W")))

ggplot(pseudo_recent_wpred, aes(x = as.factor(Month), y = residual, fill = Action, color = Action))+
  geom_boxplot()+
  scale_color_manual(values = c("darkred", "darkgreen", "blue", "grey"))+
  facet_wrap(~Region)


ggplot(filter(pseudo_recent_wpred, Month %in% c(6:10)), 
       aes(x = as.factor(Month), y = residual, fill = YrType, color = YrType))+
  geom_boxplot(alpha = 0.5)+
  scale_color_manual(values = c("darkred", "orange3", "yellow4", "springgreen4", "blue"), name = "Water Year \nType")+
  scale_fill_manual(values = c("darkred", "orange", "yellow", "springgreen4", "blue" ), name = "Water Year \nType")+
  facet_wrap(~Region)+
  geom_hline(yintercept = 0, linetype =2)+
  theme_bw()+
  xlab("Month of Year")+
  ylab("Residual")
  

ggsave("plots/PseudoResidualBoxplot.tiff", width = 6, height = 4, device = "tiff")


ggplot(pseudo_recent_wpred, aes(x = as.factor(Year), y = BPUE, fill = YrType, color = YrType))+
  geom_boxplot(alpha = 0.5)+
  scale_color_manual(values = c("darkred", "orange3", "yellow4", "springgreen4", "blue"))+
  scale_fill_manual(values = c("darkred", "orange", "yellow", "springgreen4", "blue" ))+
  facet_wrap(~Region)+
  theme_bw()+ ylab("Pseudodiaptomus biomass") +  xlab("Year")+
  theme(axis.text.x = element_text(angle = 90))



ggplot(pseudo_recent_wpred, aes(x = Index, y = residual, fill = YrType, color = YrType))+
  geom_point()+
  scale_color_manual(values = c("darkred", "orange", "yellow", "springgreen4", "blue" ))+
  facet_wrap(~Region)+
  geom_hline(yintercept = 0, linetype =2)

#OK, now do it with flow instead of salinity

precent_wpred = left_join(pseudo_recent_wpred, DF)


ggplot(precent_wpred , aes(x = log(OUT), y = residual))+
  geom_point(aes(color = YrType))+
  geom_smooth(method = "lm")+
  
  scale_color_manual(values = c("darkred", "orange", "yellow2", "springgreen4", "blue" ), name = "Year Type")+
  facet_wrap(~Region)+
  geom_hline(yintercept = 0, linetype =2)+
  xlab("Log-transformed Delta Outflow")+
  theme_bw()

ggsave("plots/PseudovOutflow_residuals.tiff", device = "tiff", width =6, height =5)

#THEY GET WASHED DOWNSTREAM!!!
ggplot(precent_wpred , aes(x = log(OUT), y = residual))+
  geom_smooth(method = "lm")+
  geom_point(aes(color = Action))+
  scale_color_manual(values = c("darkred", "darkgreen", "blue", "purple", "pink"))+
  facet_wrap(~Region)+
  geom_hline(yintercept = 0, linetype =2)

ggplot(precent_wpred , aes(x = log(OUT), y = residual))+
  geom_smooth(method = "lm")+
  geom_point(aes(color = Region))+
  scale_color_manual(values = c("darkred", "darkgreen", "blue", "purple", "pink"))+
  facet_wrap(~Action)+
  geom_hline(yintercept = 0, linetype =2)


downlm = lm(residual ~ log(OUT)*Region, data = precent_wpred)
plot(downlm)
summary(downlm)
write.csv(summary(downlm)$coefficients, "outputs/zoopgamresid.csv" )

emtrends(downlm, pairwise ~ Region, var = "log(OUT)")
plot(allEffects(downlm))

############ what other taxa increase with flow? ##################


zoop_data_all<-Zoopsynther(Data_type="Community", Sources=c("EMP", "STN", "20mm", "FMWT"), 
                       Time_consistency = FALSE, Years = c(1995:2022))
zoop_groups<-read_csv(here("Data/zoopcrosswalk2.csv"), col_types=cols_only(Taxlifestage="c", IBMR="c"))%>%
  distinct()

zoopsall = zoop_data_all %>%
  mutate(Taxlifestage=str_remove(Taxlifestage, fixed("_UnID")))%>%
  filter(!is.na(Longitude), !is.na(SalSurf),
    !(SizeClass=="Meso" & #eliminating species which are counted in meso and micro and retained better in the micro net from the meso calcs
        
        Taxlifestage%in%c("Asplanchna Adult", "Copepoda Larva","Cyclopoida Juvenile", "Eurytemora Larva", "Harpacticoida Undifferentiated",
                          "Keratella Adult", "Limnoithona Adult", "Limnoithona Juvenile", "Limnoithona sinenesis Adult", "Limnoithona tetraspina
                                    Adult", "Oithona Adult", "Oithona Juvenile", "Oithona davisae Adult", "Polyarthra Adult","Pseudodiaptomus Larva", 
                          "Rotifera Adult", "Sinocalanus doerrii Larva", "Synchaeta Adult", "Synchaeta bicornis Adult", "Trichocerca Adult")) &
      
      !(SizeClass=="Micro" &Taxlifestage%in%c("Cirripedia Larva", "Cyclopoida Adult", "Oithona similis")) & #removing categories better retained in meso net from micro net matrix
      (is.na(Order) | Order!="Amphipoda") ) %>%#& # Remove amphipods
     # (is.na(Order) | Order!="Mysida" | Taxlifestage=="Hyperacanthomysis longirostris Adult"))%>% #Only retain Hyperacanthomysis longirostris
  mutate(Taxlifestage=recode(Taxlifestage, `Synchaeta bicornis Adult`="Synchaeta Adult", # Change some names to match to biomass conversion dataset
                             `Pseudodiaptomus Adult`="Pseudodiaptomus forbesi Adult",
                             `Acanthocyclops vernalis Adult`="Acanthocyclops Adult"))%>%
  left_join(zoop_groups, by="Taxlifestage")%>% # Add IBMR categories
   group_by(IBMR)%>%
  mutate(flag=if_else(all(c("Micro", "Meso")%in%SizeClass), "Remove", "Keep"))%>% # This and the next 2 lines are meant to ensure that all categories are consistent across the surveys. Since only EMP samples microzoops, only EMP data can be used for categories that include both micro and mesozoops.
  ungroup()%>%
  filter(!(flag=="Remove" & Source!="EMP"))%>%
  select(SampleID, Station, Latitude, Longitude, SalSurf, Date, Year, IBMR, CPUE)%>%
  group_by(across(-CPUE))%>%
  summarise(CPUE=sum(CPUE), .groups="drop")%>% # Sum each IBMR categories
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326)%>%
  st_transform(crs=st_crs(Regions)) %>% 
  st_join(Regions %>%
            select(Region)) %>%
  st_drop_geometry() %>% 
  filter(!is.na(Region))%>%
  mutate(doy=yday(Date), #Day of year
         Month=month(Date), # Month
         Year_fac=factor(Year), # Factor year for model random effect
         Station_fac=factor(Station), # Factor station for model random effect
         across(c(SalSurf, doy), list(s=~(.x-mean(.x))/sd(.x))), # Center and standardize predictors
         CPUE_log1p=log(CPUE+1)) # log1p transform BPUE for model
save(zoopsall, file = "data/zoopsall.RData")


ggplot(zoopsall, aes(x = SalSurf, y = CPUE, color = Region))+
  facet_grid(IBMR~Month, scales = "free_y")+
  geom_smooth(method = "lm")

ggplot(zoopsall, aes(x = SalSurf, y =  CPUE_log1p, color = Region))+
  facet_wrap(~IBMR, scales = "free_y")+
  geom_smooth(method = "lm")

ggplot(filter(zoopsall, Month %in% c(6:10), !is.na(IBMR)), 
       aes(x = SalSurf, y =  CPUE_log1p, color = Region))+
  facet_wrap(~IBMR, scales = "free_y")+
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 5))+
  coord_cartesian(xlim = c(0,5))
################################################
#need to do this with flow, not salinity
zoopsall_f = left_join(zoopsall, Dayflow)

ggplot(filter(zoopsall_f, Month %in% c(6:10), !is.na(IBMR)), 
       aes(x = OUT, y =  CPUE_log1p, color = Region))+
  facet_wrap(~IBMR, scales = "free_y")+
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 5))


ggplot(filter(zoopsall_f, Month %in% c(6:10), !is.na(IBMR), IBMR != "other"), 
       aes(x = log(OUT), y =  CPUE_log1p, color = Region))+
  facet_wrap(~IBMR, nrow =5, scales = "free_y")+
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 5))+
#geom_smooth(method = "lm")+
  theme_bw()

ggsave("plots/flow_abundance_zoops.tiff", width = 8, height = 9, device = "tiff")

#############################################
#why are mysids so weird?

#are they better if I take monthly averages to deal with stuff?

mysave = filter(zoopsall, IBMR == "mysid") %>%
  group_by(Year, Month, Region) %>%
  summarize(SalSurf = mean(SalSurf), CPUE = mean(CPUE), logCPUE = mean(CPUE_log1p))

ggplot(mysave, aes(x = SalSurf, y = logCPUE)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 4))+
  facet_wrap(~Region)
#OK, so they definitely have a peak in mid salinities. 

#do they increase with flow?
mysave = left_join(mysave, yrs)

ggplot(mysave, aes(x = Index, y = logCPUE)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 4))+
  geom_point()+
  facet_grid(Month~Region, scales = "free_y")

#that was confusing, let's drill down more

mysave = left_join(mysave, DF)

ggplot(filter(mysave, Month %in% c(6:10)), aes(x = log(OUT), y = logCPUE)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 4))+
  geom_point()+
  facet_grid(Month~Region, scales = "free_y")


ggplot(filter(mysave, Month %in% c(6:10)), aes(x = log(OUT), y = logCPUE, color = Month)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 4))+
  geom_point()+
  facet_wrap(~Region, scales = "free_y")
#Check drought paper
# seem to increase with outflow, but keep the peak at around 3 or 4


acave = filter(zoopsall, IBMR == "acartela") %>%
  group_by(Year, Month, Region) %>%
  summarize(SalSurf = mean(SalSurf), CPUE = mean(CPUE), logCPUE = mean(CPUE_log1p))

ggplot(acave, aes(x = SalSurf, y = logCPUE)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 4))+
  facet_wrap(~Region)
#OK, so they definitely have a peak in mid salinities. 

acave = left_join(acave, DF)

ggplot(acave, aes(x = log(OUT), y = logCPUE)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 4))+
  facet_wrap(~Region)
#So acartiella definitely decrease with outflow. 


#now limno
limave = filter(zoopsall, IBMR == "limno") %>%
  group_by(Year, Month, Region) %>%
  summarize(SalSurf = mean(SalSurf), CPUE = mean(CPUE), logCPUE = mean(CPUE_log1p))

ggplot(limave, aes(x = SalSurf, y = logCPUE)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 4))+
  facet_wrap(~Region)
#OK, so they definitely have a peak in mid salinities. 

limave = left_join(limave, DF)

ggplot(limave, aes(x = log(OUT), y = logCPUE)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 4))+
  facet_wrap(~Region)
#So limno also definitely decreases with outflow 

zoopsallflow = zoopsall %>%
  group_by(Year, Month, Region, IBMR) %>%
  summarize(CPUE = mean(CPUE), logCPUE = log(CPUE +1)) %>%
  left_join(DF)

ggplot(filter(zoopsallflow, Month %in% c(6:10), !is.na(IBMR)), 
       aes(x = log(OUT), y =  logCPUE, color = Region))+
  facet_wrap(~IBMR, scales = "free_y", nrow =4)+
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 5))


notincluded = filter(zoopsall, is.na(IBMR))

ggplot()+
  geom_sf(data = WW_Delta, fill = "lightblue", color = "grey40")+
geom_sf(data = Regions, aes(fill = Region), alpha = 0.5)+
    coord_sf(xlim = c(-122.1, -121.6), ylim = c(38.0, 38.25))

###########################################################################
###zoop-salnith AND flow gams ##################

zoopsflow = bind_rows(pseudo_data_mass2, pseudo_data_mass) %>%
  
  left_join(select(filter(Dayflow,OUT >0), Date, Year, Month, OUT)) %>%
  mutate(logout = log(OUT), logout_s = scale(logout)) %>%
  filter(!is.na(OUT), doy %in% c(150:310))

newdata_functionflow<-function(region, data=zoopsflow, quant=0.99){
  
  lower<-(1-quant)/(2)
  upper<-1-lower
  
  data_filt<-data%>%
    filter(Region%in%region & Year >= year_min)
  
  # Calculate monthly quantiles of salinity
  month_sal<-data_filt%>%
    group_by(Month)%>%
    summarise(l=quantile(SalSurf, lower),
              u=quantile(SalSurf, upper), .groups="drop")
  
  # Calculate monthly quantiles of outflow
  month_out<-data_filt%>%
    group_by(Month)%>%
    summarise(lo=quantile(logout, lower),
              uo=quantile(logout, upper), .groups="drop")
  
  #first do all the salinities with the mean of outflow, then all the outflows with mean of salinity
  newdata_sal<-expand_grid(date=mdy(paste(6:10, 15, 2001, sep="/")), # The 15th of each month on a non-leap year
                       SalSurf=seq(round(min(data_filt$SalSurf), 1), 
                                   round(max(data_filt$SalSurf), 1), by=0.1),
                       logout=mean(data_filt$logout))%>% # Salinity sequence nicely rounded to 1 decimal
    mutate(Month=month(date),
           Type = "Salinity",
           doy=yday(date), # Day of year
           SalSurf_s=(SalSurf-mean(data$SalSurf))/sd(data$SalSurf), # center and standardize salinity to match data
           logout_s=(logout-mean(data$logout))/sd(data$logout),
           doy_s=(doy-mean(data$doy))/sd(data$doy))%>% # center and standardize doy to match data
    left_join(month_sal, by="Month")%>%
    filter(SalSurf >= l & SalSurf <= u)%>% # Remove any salinity values outside the quantiles for each month
    select(Month, doy, doy_s, SalSurf, SalSurf_s, logout, logout_s, Type)
  
  newdata_out<-expand_grid(date=mdy(paste(6:10, 15, 2001, sep="/")), # The 15th of each month on a non-leap year
                       SalSurf=mean(data_filt$SalSurf),
                       logout=seq(round(min(data_filt$logout), 1), 
                                  round(max(data_filt$logout), 1), by=0.1))%>% # Salinity sequence nicely rounded to 1 decimal
    mutate(Month=month(date),
           Type = "out",
           doy=yday(date), # Day of year
           SalSurf_s=(SalSurf-mean(data$SalSurf))/sd(data$SalSurf), # center and standardize salinity to match data
           logout_s=(logout-mean(data$logout))/sd(data$logout),
           doy_s=(doy-mean(data$doy))/sd(data$doy))%>% # center and standardize doy to match data
    left_join(month_out, by="Month")%>%
    filter(logout >= lo & logout <= uo)%>% # Remove any salinity values outside the quantiles for each month
    
    select(Month, doy, doy_s, SalSurf, SalSurf_s, logout, logout_s, Type)
  
  newdata = bind_rows(newdata_sal, newdata_out)
  
}

newdata_flow = map(set_names(unique(zoopsflow$Region)), newdata_functionflow, data = zoopsflow)


ps_modelflow<-function(region,new_data=newdata_flow, type = "Both"){
  
  cat("<<<<<<<<<<<<<<<<<<<<<<< modeling", region, ">>>>>>>>>>>>>>>>>>>>>>>>>\n\n")
  
  new_data<-new_data[[region]]
  
  data<-filter(zoopsflow,Region==region & Year>=year_min)
  
  par(mfrow=c(2,2))
  
  if(type == "Both") {
  
  if(length(unique(data$Station_fac))>1){
    model<-gam(BPUE_log1p ~ te(SalSurf_s, doy_s,k =c(5,5), bs=c("cs", "cc")) + 
                 te(logout_s, doy_s, k=c(5,5), bs=c("cs", "cc")) + 
                 s(Year_fac, bs="re") + s(Station_fac, bs="re"),
               data=data, 
               method="REML")
    
    random_effects<-c("s(Year_fac)", "s(Station_fac)")
    
  }else{
    
    model<-gam(BPUE_log1p ~ te(SalSurf_s, doy_s, k=c(5,5), bs=c("cs", "cs")) + 
                 te(logout_s, doy_s, k=c(5,5), bs=c("cs", "cc")) +
                 s(Year_fac, bs="re"),
               data=data, 
               method="REML")
    
    random_effects<-c("s(Year_fac)")
  }}else{ 
    
    if(length(unique(data$Station_fac))>1){
      model<-gam(BPUE_log1p ~ te(logout_s, doy_s, k=c(5,5), bs=c("cs", "cs")) + 
                   s(Year_fac, bs="re") + s(Station_fac, bs="re"),
                 data=data, 
                 method="REML")
      
      random_effects<-c("s(Year_fac)", "s(Station_fac)")
      
    }else{
      
      model<-gam(BPUE_log1p ~ te(logout_s, doy_s, k=c(5,5), bs=c("cs", "cs")) +
                   s(Year_fac, bs="re"),
                 data=data, 
                 method="REML")
      
      random_effects<-c("s(Year_fac)")
    }
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

ps_grizzf = ps_modelflow("Grizzly Bay") %>%
  mutate(Region = "Grizzly Bay")
ps_SMf = ps_modelflow("Suisun Marsh")%>%
  mutate(Region = "Suisun Marsh")
ps_SBf = ps_modelflow("Suisun Bay")%>%
  mutate(Region = "Suisun Bay")
ps_Rivf = ps_modelflow("River")%>%
  mutate(Region = "River")


ps_conversionsf = bind_rows(ps_grizzf, ps_SMf, ps_SBf, ps_Rivf)

#calculate the mean and quartiles for each salinity bin, month, and region
ps_conversions_sumf<-apply(select(ps_conversionsf, starts_with("draw_")), 1, 
                          function(x) quantile(x, c(0.025, 0.5, 0.975)))

#set it up as a dataframe for plotting
ps_conversions_plotf<-ps_conversionsf%>%
  select(-starts_with("draw_"))%>%
  bind_cols(tibble(l95=ps_conversions_sumf["2.5%",], 
                   median=ps_conversions_sumf["50%",], 
                   u95=ps_conversions_sumf["97.5%",])) 

#Plot it! - this is based on 1995-2020
psal = ggplot(filter(ps_conversions_plotf, Type == "Salinity", Month %in% c(6:10)), 
       aes(x=SalSurf, y=median, ymin=l95, ymax=u95))+
  geom_ribbon(alpha=0.4, fill = "skyblue")+
  geom_line()+
  ylab("Pseudodiaptomus biomass (log scale)")+
  xlab("Surface Salinity")+
  facet_grid(Region~month(Month, label=T))+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_point(data = filter(pseudo_recent, Month %in% c(6:10)), 
             aes(x = SalSurf, y = BPUE, color = YrType), inherit.aes = F)+
  scale_color_manual(values = c("darkred", "orange", "yellow2", "springgreen4", "blue" ), name = "Year Type", guide = "none")

pout = ggplot(filter(ps_conversions_plotf, Type == "out", Month %in% c(6:10)), 
              aes(x=logout, y=median, ymin=l95, ymax=u95))+
  geom_ribbon(alpha=0.4, fill = "springgreen3")+
  geom_line()+
  ylab("Pseudodiaptomus biomass (log scale)")+
  xlab("Log-transformed Delta Outflow")+
  facet_grid(Region~month(Month, label=T))+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_point(data = filter(pseudo_recent, Month %in% c(6:10)), 
             aes(x = logout, y = BPUE, color = YrType), inherit.aes = F)+
  scale_color_manual(values = c("darkred", "orange", "yellow2", "springgreen4", "blue" ), name = "Year Type", guide = "none")




psal+pout

ggsave(filename = "plots/PseudoGamOutflowAndSal.tiff", device = "tiff", width = 14, height = 6)


#########################
#now with just outflow, i guess


ps_grizzfo = ps_modelflow("Grizzly Bay", type = "OUT") %>%
  mutate(Region = "Grizzly Bay")
ps_SMfo = ps_modelflow("Suisun Marsh",  type = "OUT")%>%
  mutate(Region = "Suisun Marsh")
ps_SBfo = ps_modelflow("Suisun Bay",  type = "OUT")%>%
  mutate(Region = "Suisun Bay")
ps_Rivfo = ps_modelflow("River",  type = "OUT")%>%
  mutate(Region = "River")


ps_conversionsfo = bind_rows(ps_grizzfo, ps_SMfo, ps_SBfo, ps_Rivfo)

#calculate the mean and quartiles for each salinity bin, month, and region
ps_conversions_sumfo<-apply(select(ps_conversionsfo, starts_with("draw_")), 1, 
                           function(x) quantile(x, c(0.025, 0.5, 0.975)))

#set it up as a dataframe for plotting
ps_conversions_plotfo<-ps_conversionsfo%>%
  select(-starts_with("draw_"))%>%
  bind_cols(tibble(l95=ps_conversions_sumfo["2.5%",], 
                   median=ps_conversions_sumfo["50%",], 
                   u95=ps_conversions_sumfo["97.5%",]))

#Plot it! - this is based on 1995-2020

pout2 = ggplot(filter(ps_conversions_plotfo, Type == "out", Month %in% c(6:10)), 
              aes(x=logout, y=median, ymin=l95, ymax=u95))+
  geom_ribbon(alpha=0.4, fill = "springgreen3")+
  geom_line()+
  ylab("Pseudodiaptomus biomass (log scale)")+
  xlab("Log-transformed Delta Outflow")+
  facet_grid(Region~month(Month, label=T))+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_point(data = filter(pseudo_recent, Month %in% c(6:10)), 
             aes(x = logout, y = BPUE, color = YrType), inherit.aes = F)+
  scale_color_manual(values = c("darkred", "orange", "yellow2", "springgreen4", "blue" ), name = "Year Type")


pout2
ggsave(filename = "plots/PseudoGamOutflow.tiff", device = "tiff", width = 8, height = 6)

#models individually, to grab summaries later.
modelgz<-gam(BPUE_log1p ~ te(SalSurf_s, doy_s, k=c(5,5), bs=c("cs", "cc")) + 
             te(logout_s, doy_s, k=c(5,5), bs=c("cs", "cc")) + 
             s(Year_fac, bs="re") + s(Station_fac, bs="re"),
           data=filter(zoopsflow, Region == "Grizzly Bay"), 
           method="REML")
plot(modelgz)

modelsm<-gam(BPUE_log1p ~ te(SalSurf_s, doy_s, k=c(5,5), bs=c("cs", "cc")) + 
               te(logout_s, doy_s, k=c(5,5), bs=c("cs", "cc")) + 
               s(Year_fac, bs="re") + s(Station_fac, bs="re"),
             data=filter(zoopsflow, Region == "Suisun Marsh"), 
             method="REML")
plot(modelsm)


modelsb<-gam(BPUE_log1p ~ te(SalSurf_s, doy_s, k=c(5,5), bs=c("cs", "cc")) + 
               te(logout_s, doy_s, k=c(5,5), bs=c("cs", "cc")) + 
               s(Year_fac, bs="re") + s(Station_fac, bs="re"),
             data=filter(zoopsflow, Region == "Suisun Bay"), 
             method="REML")
plot(modelsb)

modelsr<-gam(BPUE_log1p ~ te(SalSurf_s, doy_s, k=c(5,5), bs=c("cs", "cc")) + 
               te(logout_s, doy_s, k=c(5,5), bs=c("cs", "cc")) + 
               s(Year_fac, bs="re") + s(Station_fac, bs="re"),
             data=filter(zoopsflow, Region == "River"), 
             method="REML")
plot(modelsr)
summary(modelsr)
summary(modelsb)
summary(modelsm)
summary(modelgz)

#models individually, to grab summaries later.
modelgz1<-gam(BPUE_log1p ~ te(SalSurf_s, doy_s, k=c(5,5), bs=c("cs", "cc")) + 
               
               s(Year_fac, bs="re") + s(Station_fac, bs="re"),
             data=filter(zoopsflow, Region == "Grizzly Bay"), 
             method="REML")
plot(modelgz1)
library(mgcViz)
gz1p = getViz(modelgz1)
gam.check(modelgz1)

#all those zeros are an issue

o <- plot( sm(gz1p, 1) )
o + l_fitRaster() + 
  l_fitContour() + 
  l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic()

modelsm1<-gam(BPUE_log1p ~ te(SalSurf_s, doy_s, k=c(5,5), bs=c("cs", "cc")) + 
                
               s(Year_fac, bs="re") + s(Station_fac, bs="re"),
             data=filter(zoopsflow, Region == "Suisun Marsh"), 
             method="REML")



modelsb1<-gam(BPUE_log1p ~ te(SalSurf_s, doy_s, k=c(5,5), bs=c("cs", "cc")) + 
               
               s(Year_fac, bs="re") + s(Station_fac, bs="re"),
             data=filter(zoopsflow, Region == "Suisun Bay"), 
             method="REML")


modelsr1<-gam(BPUE_log1p ~ te(SalSurf_s, doy_s, k=c(5,5), bs=c("cs", "cc")) + 
                
               s(Year_fac, bs="re") + s(Station_fac, bs="re"),
             data=filter(zoopsflow, Region == "River"), 
             method="REML")

#now flow only

modelgz2<-gam(BPUE_log1p ~ te(logout_s, doy_s, k=c(5,5), bs=c("cs", "cc")) + 
                
                s(Year_fac, bs="re") + s(Station_fac, bs="re"),
              data=filter(zoopsflow, Region == "Grizzly Bay"), 
              method="REML")


modelsm2<-gam(BPUE_log1p ~ te(logout_s, doy_s, k=c(5,5), bs=c("cs", "cc")) + 
                
                s(Year_fac, bs="re") + s(Station_fac, bs="re"),
              data=filter(zoopsflow, Region == "Suisun Marsh"), 
              method="REML")



modelsb2<-gam(BPUE_log1p ~ te(logout_s, doy_s, k=c(5,5), bs=c("cs", "cc")) + 
                
                s(Year_fac, bs="re") + s(Station_fac, bs="re"),
              data=filter(zoopsflow, Region == "Suisun Bay"), 
              method="REML")


modelsr2<-gam(BPUE_log1p ~ te(logout_s, doy_s, k=c(5,5), bs=c("cs", "cc")) + 
                
                s(Year_fac, bs="re") + s(Station_fac, bs="re"),
              data=filter(zoopsflow, Region == "River"), 
              method="REML")

summary(modelgz) #91.4% - both
summary(modelgz1) #91% - salinity only
summary(modelgz2) #90.7% - flow only
#very similar ammount of variancec explained, probably because of the effect of DOY and random effects
summary(modelsm) #93.3 - both
summary(modelsm1) # 92.7% - salinity only
summary(modelsm2) #92.3%0 - flow only
#huh

summary(modelsb) #90.1%
summary(modelsb1) #89.8%
summary(modelsb2) #88.9% 
#also very simiarl


summary(modelsr) #93.1%
summary(modelsr1) #92.7%
summary(modelsr2) # 91.3%

gam.check(modelsr2)
BIC(modelsr) #both
BIC(modelsr1) #salinity only
BIC(modelsr2) # flow only
#model with both ranked quite a bit better

#how correlated are things?
ggplot(zoopsflow, aes(x = doy, y = SalSurf))+geom_smooth()+ facet_wrap(~Region)
ggplot(zoopsflow, aes(x = doy, y = logout))+geom_smooth()+ facet_wrap(~Region)
ggplot(zoopsflow, aes(x = SalSurf, y = logout))+geom_smooth()+ facet_wrap(~Region)
ggplot(zoopsflow, aes(x = SalSurf, y = logout))+geom_smooth()+ facet_grid(Month~Region)



#one more try at calculating predicted values and residuals

predictsm = data.frame(filter(zoopsflow, Region == "Suisun Marsh"), Predict = predict(modelsm))

ggplot(predictsm, aes(x = SalSurf, y = BPUE_log1p))+ geom_point()+ geom_point(aes(y = Predict), color = "blue")+
  facet_wrap(~ Month)

ggplot(predictsm, aes(x = logout, y = BPUE_log1p))+ geom_point()+ geom_point(aes(y = Predict), color = "blue")+
  facet_wrap(~ Month)

newSMflow = data.frame(select(filter(zoopsflow, Region == "Suisun Marsh"), -SalSurf_s), SalSurf_s = mean(filter(zoopsflow, Region == "Suisun Marsh")$SalSurf_s))
flowpartials = data.frame(filter(zoopsflow, Region == "Suisun Marsh"), 
                          Predict = predict(modelsm, newdata = newSMflow))

newSMsal = data.frame(select(filter(zoopsflow, Region == "Suisun Marsh"), -logout_s), logout_s = mean(filter(zoopsflow, Region == "Suisun Marsh")$logout_s))
salpartials = data.frame(filter(zoopsflow, Region == "Suisun Marsh"), 
                          Predict = predict(modelsm, newdata = newSMsal))


ggplot(flowpartials , aes(x = logout, y = BPUE_log1p))+ geom_point()+ geom_point(aes(y = Predict), color = "blue")+
  facet_wrap(~ Month)


ggplot(salpartials , aes(x = SalSurf, y = BPUE_log1p))+ geom_point()+ geom_point(aes(y = Predict), color = "blue")+
  facet_wrap(~ Month)

###########################################################
#model of zoops in SUisun marsh based on flow at NSL and salinity
#data from WDL station E33670

NSL = read_csv("data/E33670_StreamFlow_RAW.csv", skip =2)
NSL = mutate(NSL, DateTime = mdy_hms(Date))

NSL2 = NSL %>%
  mutate(Date = date(DateTime)) %>%
  filter(Qual %in% c(70, 1)) %>%
           group_by(Date) %>%
           summarize(Flow = mean(Point)) %>%
  filter(Flow <3000 & Flow > -1000)

ggplot(NSL2, aes(x = Date,y = Flow)) + geom_line()

#maybe 3-day average flows will be more meaningful?
library(zoo)
NSL2 = mutate(NSL2, Meanflow = rollmean(Flow, 3, na.pad = T))

zoopsflowNSL = filter(zoopsflow, Region == "Suisun Marsh", Month %in% c(6:10)) %>%
  left_join(NSL2) %>%
  mutate(Flow_s = scale(Flow), 
         Meanflow_s = scale(Meanflow),
         doy_s = scale(doy), 
         SalSurf_s = scale(SalSurf)) %>%
  filter(!is.na(Flow))

modelsm2NSL<-gam(BPUE_log1p ~ te(Meanflow_s, doy_s, k=c(5,5), bs=c("cs", "cs")) + 
                   te(SalSurf_s, doy_s, k=c(5,5), bs=c("cs", "cc")) +
                s(Year_fac, bs="re") + s(Station_fac, bs="re"),
              data=zoopsflowNSL, 
              method="REML")
summary(modelsm2NSL)
plot(modelsm2NSL)


  
  # Calculate monthly quantiles of salinity
  month_sal<-zoopsflowNSL %>%
    group_by(Month)%>%
    summarise(l=quantile(SalSurf, 0.05, na.rm =T),
              u=quantile(SalSurf, 0.95, na.rm =T), .groups="drop")
  
  # Calculate monthly quantiles of outflow
  month_out<-zoopsflowNSL %>%
    group_by(Month)%>%
    summarise(lo=quantile(Flow, 0.05, na.rm =T),
              uo=quantile(Flow, 0.95, na.rm =T), .groups="drop")
  
  #first do all the salinities with the mean of outflow, then all the outflows with mean of salinity
  newdata_sal<-expand_grid(date=mdy(paste(1:12, 15, 2001, sep="/")), # The 15th of each month on a non-leap year
                           SalSurf=seq(round(min(zoopsflowNSL$SalSurf), 1), 
                                       round(max(zoopsflowNSL$SalSurf), 1), by=0.1),
                           Flow=mean(zoopsflowNSL$Flow))%>% # Salinity sequence nicely rounded to 1 decimal
    mutate(Month=month(date),
           Meanflow=mean(zoopsflowNSL$Meanflow),
           Type = "Salinity",
           doy=yday(date), # Day of year
           SalSurf_s=(SalSurf-mean(zoopsflowNSL$SalSurf))/sd(zoopsflowNSL$SalSurf), # center and standardize salinity to match data
           Flow_s=(Flow-mean(zoopsflowNSL$Flow))/sd(zoopsflowNSL$Flow),
           Meanflow_s=(Meanflow-mean(zoopsflowNSL$Meanflow))/sd(zoopsflowNSL$Meanflow),
           
           doy_s=(doy-mean(zoopsflowNSL$doy))/sd(zoopsflowNSL$doy))%>% # center and standardize doy to match data
    left_join(month_sal, by="Month")%>%
    filter(SalSurf >= l & SalSurf <= u)%>% # Remove any salinity values outside the quantiles for each month
    select(Month, doy, doy_s, SalSurf, SalSurf_s, Flow, Flow_s, Type)
  
  newdata_out<-expand_grid(date=mdy(paste(1:12, 15, 2015, sep="/")), # The 15th of each month on a non-leap year
                           SalSurf=mean(zoopsflowNSL$SalSurf),
                           Flow=seq(round(min(zoopsflowNSL$Flow), 1), 
                                      round(max(zoopsflowNSL$Flow), 1), by=0.1))%>% # Salinity sequence nicely rounded to 1 decimal
    mutate(Month=month(date),
           Type = "out",
           Meanflow = rollmean(Flow, 3, na.pad = T),
           doy=yday(date), # Day of year
           SalSurf_s=(SalSurf-mean(zoopsflowNSL$SalSurf))/sd(zoopsflowNSL$SalSurf), # center and standardize salinity to match data
           Flow_s=(Flow-mean(zoopsflowNSL$Flow))/sd(zoopsflowNSL$Flow),
           Meanflow_s=(Meanflow-mean(zoopsflowNSL$Meanflow))/sd(zoopsflowNSL$Meanflow),
           
           doy_s=(doy-mean(zoopsflowNSL$doy))/sd(zoopsflowNSL$doy))%>% # center and standardize doy to match data
    left_join(month_out, by="Month")%>%
    filter(Flow >= lo & Flow <= uo)%>% # Remove any salinity values outside the quantiles for each month
    
    select(Month, doy, doy_s, SalSurf, SalSurf_s, Flow, Flow_s, Type)
  
  newdata = bind_rows(newdata_sal, newdata_out)
  



sal<-predict_posterior(modelsm2NSL, newdata, c("s(Year_fac)", "s(Station_fac)"))%>%
  bind_cols(newdata%>% # Add covariate columns before these columns
              select(-doy_s, -SalSurf_s), 
            .)


ps_conversions_sumNSL<-apply(select(sal, starts_with("draw_")), 1, 
                            function(x) quantile(x, c(0.025, 0.5, 0.975)))

#set it up as a dataframe for plotting
ps_conversions_plotNSL <-sal%>%
  select(-starts_with("draw_"))%>%
  bind_cols(tibble(l95=ps_conversions_sumNSL["2.5%",], 
                   median=ps_conversions_sumNSL["50%",], 
                   u95=ps_conversions_sumNSL["97.5%",]))

#Plot it! 

pout2NSL = ggplot(filter(ps_conversions_plotNSL, Type == "out", Month %in% c(6:10)), 
               aes(x=Flow, y=median, ymin=l95, ymax=u95))+
  geom_ribbon(alpha=0.4, fill = "springgreen3")+
  geom_line()+
  ylab("Pseudodiaptomus biomass (log scale)")+
  xlab("Flow at NSL")+
  facet_grid(.~month(Month, label=T))+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))#+
  #geom_point(data = filter(zoopsflowNSL, Month %in% c(6:10)), 
  #           aes(x = Flow, y = BPUE, color = YrType), inherit.aes = F)
pout2NSL

psal2NSL = ggplot(filter(ps_conversions_plotNSL, Type == "Salinity", Month %in% c(6:10)), 
                  aes(x=SalSurf, y=median, ymin=l95, ymax=u95))+
  geom_ribbon(alpha=0.4, fill = "skyblue")+
  geom_line()+
  ylab("Pseudodiaptomus biomass (log scale)")+
  xlab("Salinity")+
  facet_grid(.~month(Month, label=T))+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))

psal2NSL

ggplot(zoopsflowNSL, aes(x = SalSurf, y = BPUE_log1p))+ geom_point()+ geom_smooth()+
  facet_wrap(~Month)



ggplot(zoopsflowNSL, aes(x = Flow, y = BPUE_log1p))+ geom_point()+ geom_smooth()+
  facet_wrap(~Month)
