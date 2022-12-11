install.packages("ggplot2")
library(ggplot2)

library(tidyverse)
growthdata <- read_csv("Data/Growth_data.csv")



amygheightmodel <- lm(Height_wk7 ~ Grass_Treatment + Watering + Grass_Treatment:Watering, data = amygsurv)
summary(amygheightmodel)
#Checking fit of the model
amyg_resid<- simulateResiduals(amygheightmodel)
plot(amyg_resid) 

obliheightmodel <- lm(Height_wk7 ~ Grass_Treatment + Watering + Grass_Treatment:Watering, data = oblisurv)
summary(obliheightmodel)

vimiheightmodel <- lm(Height_wk7 ~ Grass_Treatment + Watering + Grass_Treatment:Watering, data = vimisurv)
summary(vimiheightmodel)

##ANOVAS##

aov_amygheight <- aov(amygheightmodel)
summary(aov_amygheight)

aov_obliheight <- aov(obliheightmodel)
summary(aov_obliheight)

aov_vimiheight <- aov(vimiheightmodel)
summary(aov_vimiheight)

##PLOTTING##

ggplot(data = growthdata, aes(y = Height_wk7, x = Species, fill= Grass_Treatment))+
  geom_boxplot()+
  geom_jitter(position = position_jitterdodge(), alpha = 0.3)+
  ylab("Final height")+
  labs(fill="Treatment")+
  (scale_fill_discrete)(labels = c("Grass", "Control"))+
  theme_classic()+
  scale_fill_manual(values=c("Tomato","#999999"))





