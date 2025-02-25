#New zoop-salinity GAMs

#Start with just Pseudodiaptomus

#Fit a model with 1972-2010, then see how actual values from 2011-2024 work.

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

conflict_prefer("filter", "dplyr")
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

pseudo_data_mass<-zoop_data%>%
  mutate(Taxlifestage=str_remove(Taxlifestage, fixed("_UnID")))%>%
  filter(Taxlifestage %in% c("Pseudodiaptomus forbesi Adult", "Pseudodiaptomus Adult",
                             "Pseudodiaptomus Juvenile")) %>%
           
           mutate(Taxlifestage = recode(Taxlifestage,#
                             `Pseudodiaptomus Adult`="Pseudodiaptomus forbesi Adult",
                             `Acanthocyclops vernalis Adult`="Acanthocyclops Adult"))%>%
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


ggplot(pseudo_data_mass, aes(x = doy, y = BPUE_log1p)) + geom_smooth()+
  scale_x_continuous(breaks = c(153, 183, 214), labels = c("Jun", "Jul", "Aug"))

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
  Xp <- predict(model, newdata=newdata, type="lpmatrix", exclude=exclude, newdata.guaranteed=TRUE) ## map coefs to fitted curves
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

ps_model<-function(region,new_data=newdata){
  
  cat("<<<<<<<<<<<<<<<<<<<<<<< modeling", region, ">>>>>>>>>>>>>>>>>>>>>>>>>\n\n")
  
  new_data<-new_data[[region]]
  
  data<-filter(pseudo_data_mass,Region==region & Year>=year_min)
  
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

ps_grizz = ps_model("Grizzly Bay") %>%
  mutate(Region = "Grizzly Bay")
ps_SM = ps_model("Suisun Marsh")%>%
  mutate(Region = "Suisun Marsh")
ps_SB = ps_model("Suisun Bay")%>%
  mutate(Region = "Suisun Bay")
ps_Riv = ps_model("River")%>%
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
  geom_ribbon(alpha=0.4)+
  geom_line()+
  ylab("Pseudodiaptomus biomass (log scale)")+
  facet_grid(Region~month(Month, label=T))+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))
############ More recent data ############################


#load zoop data from 2011:2024
zoop_datarecent <-Zoopsynther(Data_type="Community", Sources=c("EMP", "STN", "20mm", "FMWT"), 
                       Time_consistency = FALSE, Years = c(2011:2024), Size_class = "Meso")


#Process the zoop data

pseudo_data_mass2<-zoop_datarecent%>%
  mutate(Taxlifestage=str_remove(Taxlifestage, fixed("_UnID")))%>%
  filter(Taxlifestage %in% c("Pseudodiaptomus forbesi Adult", "Pseudodiaptomus Adult",
                             "Pseudodiaptomus Juvenile")) %>%
  
  mutate(Taxlifestage = recode(Taxlifestage,#
                               `Pseudodiaptomus Adult`="Pseudodiaptomus forbesi Adult",
                               `Acanthocyclops vernalis Adult`="Acanthocyclops Adult"))%>%
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

#OK, not sure the best way of doing this, but let's do monthly, regional means.
pseudo_recent = group_by(pseudo_data_mass2, Month, Region, Year) %>%
  summarize(SalSurf = mean(SalSurf, na.rm =T), BPUE = mean(BPUE_log1p))

yrs = read_csv("data/wtryrtype.csv") %>%
  rename(Year = WY, YrType = `Yr-type`) %>%
  select(Year, WYsum, Index, YrType, Action)

pseudo_recent = left_join(pseudo_recent, yrs)

#now add it to the plot of the ealier data

ggplot(ps_conversions_plot, aes(x=SalSurf, y=median, ymin=l95, ymax=u95))+
  geom_ribbon(alpha=0.4)+
  geom_line()+
  ylab("Pseudodiaptomus biomass (log scale)")+
  geom_point(data = pseudo_recent, aes(color = YrType, x = SalSurf, y = BPUE), inherit.aes = F)+
  facet_grid(Region~month(Month, label=T))+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))


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
  mutate(residual = median - BPUE)

ggplot(pseudo_recent_wpred, aes(x = as.factor(Month), y = residual, fill = Action, color = Action))+
  geom_boxplot()+
  scale_color_manual(values = c("darkred", "darkgreen", "blue", "grey"))+
  facet_wrap(~Region)


ggplot(pseudo_recent_wpred, aes(x = as.factor(Month), y = residual, fill = YrType, color = YrType))+
  geom_boxplot()+
  scale_color_manual(values = c("darkred", "darkgreen", "blue", "purple", "pink"))+
  facet_wrap(~Region)+
  geom_hline(yintercept = 0, linetype =2)


ggplot(pseudo_recent_wpred, aes(x = Index, y = residual, fill = YrType, color = YrType))+
  geom_point()+
  scale_color_manual(values = c("darkred", "darkgreen", "blue", "purple", "pink"))+
  facet_wrap(~Region)+
  geom_hline(yintercept = 0, linetype =2)

#OK, now do it with flow instead of salinity

precent_wpred = left_join(pseudo_recent_wpred, DF)


ggplot(precent_wpred , aes(x = log(OUT), y = residual))+
  geom_point(aes(color = YrType))+
  geom_smooth(method = "lm")+
  
  scale_color_manual(values = c("darkred", "darkgreen", "blue", "purple", "pink"), name = "Year Type")+
  facet_wrap(~Region)+
  geom_hline(yintercept = 0, linetype =2)+
  xlab("Log-transformed Delta Outflow")+
  theme_bw()

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

#Oh, this is interesting. During the years with fall x2 actions the model fits pretty good regardless of flow

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

ggplot(filter(zoopsall, Month %in% c(6:10)), aes(x = SalSurf, y =  CPUE_log1p, color = Region))+
  facet_wrap(~IBMR, scales = "free_y")+
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 5))+
  coord_cartesian(xlim = c(0,5))
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
