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


############ More recent data ############################


#load zoop data from 1972-2010
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

