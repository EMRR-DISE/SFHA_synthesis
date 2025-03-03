#smelt from Celeste

library(tidyverse)
library(lme4)
library(DHARMa)
library(mgcv)

Seasons = read_csv("data/SmeltSeasons.csv")

ggplot(Seasons, aes(x = Time, y = Prevalence, group = 1)) +
  geom_line(color = "blue", linewidth = 1.5) +  # Line for Prevalence
  geom_ribbon(aes(ymin = LowerBound, ymax = UpperBound), fill = "red", alpha = 0.3) +  # Shaded area for bounds
  geom_point(color = "blue", size = 3) +  # Points for Prevalence
  # New layer for Trend points with legend
  geom_point(data = subset(Seasons, Trend != 0),
             aes(x = Time, y = Prevalence, color = factor(Trend)),
             shape = 1,  # Hollow ring
             size = 10,
             stroke = 2) +
  scale_color_manual(name = "CI Compared to Previous",
                     values = c("-1" = "red", "1" = "darkgreen"),
                     labels = c("-1" = "Below (less frequent)", "1" = "Above (more frequent)")) +
  labs(title = "Seasonal Prevalence of Delta Smelt Observations in Tows with 95% Credible Interval (2010 - 2014)",
       x = "Time (Year-Season)",
       y = "Smelt Prevalence (Ratio of Smelt-Present Tows to Smelt-Absent Tows)") +
  theme(axis.text.x = element_text(angle = 45),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 60),
        plot.margin = margin(30, 40, 30, 60, "pt"),# top, right, bottom, left margins
        plot.title = element_text(size = 80),
        legend.title = element_text(size = 60),
        legend.text = element_text(size = 40),
        legend.key.size = unit(4, "cm"),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))#+


#OK, let's see what we can do with this prevalance data.

hist(Seasons$Prevalence)

#OK, definitely some sort of transormation or different distribution needed. proportional data are hard.

#trend over time, accounting for season
mod1 = lm(Prevalence ~ Season*Year2.x, data = Seasons)
plot(mod1)
#OK, residuals versus fitted looks pretty bad.

#Let's do binomial
mod2 = glm(cbind(Detected, Sampled) ~ Season*Year2.x, data = filter(Seasons, !is.na(Season)), family = "binomial")
summary(mod2)
plot(mod2)
#sigh, now it's a bunch of outliers
#probably over-dispersed.

#use a beta-binomial regression?
#some other modeling framework?

ggplot(filter(Seasons, !is.na(Season)), aes(x = Year2.x, y = Prevalence)) +
  geom_point(aes(shape = Yr.type))+
  geom_smooth()+
  geom_line(color = "red", linewidth = 0.5) +  # Line for Prevalence
  geom_ribbon(aes(ymin = LowerBound, ymax = UpperBound), fill = "red", alpha = 0.3) +
  facet_wrap(~Season)

#What it we ran a GAM through this and looked at the residuals from teh line. I's definitelynot liear
Seasons2 = filter(Seasons, !is.na(Season)) %>%
  mutate(Seasonf = as.factor(Season))
gam1 = gam(cbind(Detected, Sampled) ~ s(Year2.x, by = Seasonf), data = Seasons2, family = "binomial")
plot(gam1, pages=1,residuals=TRUE,all.terms=TRUE)
gam.check(gam1)

#maybe this is better? It looks a little odd

Seasons2 = mutate(Seasons2, resid = residuals(gam1))
ggplot(Seasons2, aes(x = X2, y = resid))+
  facet_wrap(~Season)+
  geom_point()+ geom_smooth(method = "lm")


