### Checking analyses 27/11/2021

library(tidyverse)
library(DHARMa)
library(ggplot2)

growthdata <- read_csv("Data/Growth_data.csv")
amygdata <- growthdata %>% filter(Species == "AMYG")
oblidata <- growthdata %>% filter(Species == "OBLI")
vimidata <- growthdata %>% filter(Species == "VIMI")

############ Height ########
#Plot it first!
ggplot(data = growthdata, aes(y = log(Height_wk7), x = Watering, fill= Grass_Treatment))+
  geom_boxplot()+
  geom_jitter(position = position_jitterdodge(), alpha = 0.3)+
  ylab("Final height")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))+
  facet_wrap(~Species)
#amyg #
#Checking distributions of data
hist(amygdata$Height_wk7)
hist(log(amygdata$Height_wk7))
#Better logged
amygheightmodel <- lm(log(Height_wk7) ~ Grass_Treatment + Watering + Grass_Treatment:Watering, data = amygdata)
summary(amygheightmodel)
#Checking model fit
amyg_resid<- simulateResiduals(amygheightmodel)
plot(amyg_resid) 
aov_amygheight <- aov(amygheightmodel)
summary(aov_amygheight)
TukeyHSD(aov_amygheight)

# obli #
hist(oblidata$Height_wk7)
hist(log(oblidata$Height_wk7))
#Better logged
obliheightmodel <- lm(log(Height_wk7) ~ Grass_Treatment + Watering + Grass_Treatment:Watering, data = oblidata)
summary(obliheightmodel)
#Checking model fit
obli_resid<- simulateResiduals(obliheightmodel)
plot(obli_resid) 
aov_obliheight <- aov(obliheightmodel)
summary(aov_obliheight)
TukeyHSD(aov_obliheight)

# vimi #
hist(vimidata$Height_wk7)
hist(log(vimidata$Height_wk7))
#Better logged
vimiheightmodel <- lm(log(Height_wk7) ~ Grass_Treatment + Watering + Grass_Treatment:Watering, data = vimidata)
summary(vimiheightmodel)
#Checking model fit
vimi_resid<- simulateResiduals(vimiheightmodel)
plot(vimi_resid) 
aov_vimiheight <- aov(vimiheightmodel)
summary(aov_vimiheight)
TukeyHSD(aov_vimiheight)
