## Question: How does the presence of grass influence germination for each species?
#One model per species
#Model of total germination
#using cbind so that it is scaled by the number of seeds that were sown
# Need a column for the number of seeds that didn't germinate.
#Were there 20 seeds sown?? Assuming so but update this later*
library(lmerTest)
allgermdata <- allgermdata %>% mutate(total_no_germ = Seeds_sown-total_germ)
# allgermdata <- allgermdata %>% mutate(total_no_germ = seeds_sown-total_germ)
#Filtering the datasets for each species
germamyg <- allgermdata %>% filter(Species == "AMYG")
germobli <- allgermdata %>% filter(Species == "OBLI")
germvimi <- allgermdata %>% filter(Species == "VIMI")
#Remember that mtg dataset is different

#Modelling using cbind with generalised linear model with binomial distribution
#What are the random effects? If any...
### AMYG
#### telling the model the sample sizes
amyggermmodel <- glm(cbind(total_germ, total_no_germ) ~ Grass_presence, family = binomial, data = germamyg)
summary(amyggermmodel)
#Q: When would you use a linear model over a t-test/anova?
#Q: When would you run model with all species vs. individual models for each species? And include them as random of fixed effects?
### OBLI
obligermmodel <- glm(cbind(total_germ, total_no_germ) ~ Grass_presence, family = binomial, data = germobli)
summary(obligermmodel)
### VIMI
vimigermmodel <- glm(cbind(total_germ, total_no_germ) ~ Grass_presence, family = binomial, data = germvimi)
summary(vimigermmodel)
### answering research Q1

### presence of grass did not have a statistically significant influence on germination fractions for any species 

## The presence of grass did not have a significant influence on the germination fraction for any species
#Species as fixed effects
#germmodel2 <- glm(cbind(total_germ, total_no_germ) ~ Grass_presence + Species, family = binomial, data = allgermdata)
#summary(germmodel2)
## Need to look at the fit of all of these models. Using dharma.

#Species as fixed effects is telling me that the germination of vimi is significantly different from other species?
#Testing whether the interaction of germination with grasses is dependent on species
#germmodel3 <- glm(cbind(total_germ, total_no_germ) ~ Grass_presence + Species + Grass_presence*Species, family = binomial, data = allgermdata)
#summary(germmodel3)
#Conclusion: no, the impact of grass presence on germination does not vary by species?

###Need to look at whether variables need to be transformed!! 
# No continuous variables
#Need to look at model fits

## Research questions:
######### 1. Does the presence of grass affect eucalypt seedling germination? #######

##### Probabiliy of germination:
#germmodel <- glmer(cbind(total_germ, total_no_germ) ~ Grass_presence + Species + Grass_presence:Species, family = binomial, data = allgermdata)
#summary(germmodel)
#DO THIS BY SPECIES!:
germmodel <- glmer(cbind(total_germ, total_no_germ) ~ Grass_presence + (1|Pot_number), family = binomial, data = allgermdata)
summary(germmodel)

library(lme4)
install.packages("DHARMa")
library(DHARMa)

#AMYG
amyg_germmodel <- glmer(cbind(total_germ, total_no_germ) ~ Grass_presence + (1|Pot_number), family = binomial, data = germamyg)
summary(amyg_germmodel)

#testing if the model is fitting well
amyg_germ_resid<- simulateResiduals(amyg_germmodel)
plot(amyg_germ_resid)

#points should be close to red line

#OBLI
obli_germmodel <- glmer(cbind(total_germ, total_no_germ) ~ Grass_presence + (1|Pot_number), family = binomial, data = germobli)
summary(obli_germmodel)

obli_germ_resid<- simulateResiduals(obli_germmodel)
plot(obli_germ_resid)

#VIMI
vimi_germmodel <- glmer(cbind(total_germ, total_no_germ) ~ Grass_presence + (1|Pot_number), family = binomial, data = germvimi)
summary(vimi_germmodel)

vimi_germ_resid<- simulateResiduals(vimi_germmodel)
plot(vimi_germ_resid)

#accounting for extra variance. Ran generalised linear mixed effects models with germination as a binomial response and pot number at random effect. 

#How strong the response is? - only if significant. Estimate = coefficient

##### Mean time to germination (have to describe why this is an important variable):
#Suspect I have to subset the data by what actually germinated*
#mtgmodel <- lm(mtg ~ Grass_presence + Species + Grass_presence:Species, data = mtgdata)
#summary(mtgmodel)
#Sanity check that obli response isn't different
# oblimtg <- mtgdata %>% filter(Species == "OBLI")
# oblimtgmodel <- lm(mtg ~ Grass_presence, oblimtg)
# summary(oblimtgmodel)

#### 2. How does water availability and the presence of grass influence survival and growth? ####
#focusing on thinned individuals
###### Survival: same as germ
#Need to create a column with binary survival or not ... 1s and 0s

library(tidyverse)
growthdata <- read_csv("Data/Growth_data.csv")

amygsurv <- growthdata %>% filter(Species == "AMYG")
oblisurv <- growthdata %>% filter(Species == "OBLI")
vimisurv <- growthdata %>% filter(Species == "VIMI")

library(lme4)
library(DHARMa)

head(amygsurv) #check data 
str(vimisurv) #check structure

amygsurvmodel <- glm(Survival ~ Grass_Treatment + Watering + Grass_Treatment:Watering, family = binomial, data = amygsurv)
summary(amygsurvmodel)

amygsurv_resid<- simulateResiduals(amygsurvmodel)
plot(amygsurv_resid) 

oblisurvmodel <- glm(Survival ~ Grass_Treatment + Watering + Grass_Treatment:Watering, family = binomial, data = oblisurv)
summary(oblisurvmodel)

oblisurv_resid<- simulateResiduals(oblisurvmodel)
plot(oblisurv_resid)

vimisurvmodel <- glm(Survival ~ Grass_Treatment + Watering + Grass_Treatment:Watering, family = binomial, data = vimisurv)
summary(vimisurvmodel)

vimisurv_resid<- simulateResiduals(vimisurvmodel)
plot(vimisurv_resid)

##RUNNING ANOVAS FOR SURVIVAL##

aov_amygsurv <- aov(amygsurvmodel)
summary(aov_amygsurv)
  
aov_oblisurv <- aov(oblisurvmodel)
summary(aov_oblisurv)

aov_vimsurv <- aov(vimisurvmodel)
summary(aov_vimsurv)

##RUNNING ANOVAS FOR ABOVE BIOMASS##

aov_amygbio <- aov(amygbiomodel1)
summary(aov_amygbio)

aov_oblibio <- aov(oblibiomodel1)
summary(aov_oblibio)

aov_vimibio <- aov(vimibiomodel1)
summary(aov_vimibio)


  
##Plotting survival- this does not work since survival is measured by 1s or 0s

ggplot(growthdata, aes(x = Species, y = Survival, fill = Grass_Treatment))+
  #geom_boxplot()+
  geom_jitter(position = position_jitterdodge(), alpha = 0.3)+
  ylab("Seedling Survival")+
  labs(fill="Grass Treatment")+
  theme_classic()+
  scale_fill_manual(values=c("#CC79A7","#E69F00"))

## plot probability, Kaplan-Meier Method

#need to work out how to plot survival
# need to test goodness of fit 
## talk about sample sizes
#survival is talking about absolute thresholds of performance

#biomass and height using lm - sep models for each species

##ABOVE GROUND BIOMASS##

#MODEL 1#

amygbiomodel1 <- lm(log(Above_biomass) ~ Grass_Treatment + Watering + Grass_Treatment:Watering, data = amygsurv)
summary(amygbiomodel1) #keep log #nothing significant

amygbio_resid<- simulateResiduals(amygbiomodel)
plot(amygbio_resid)

oblibiomodel1 <- lm(Above_biomass ~ Grass_Treatment + Watering + Grass_Treatment:Watering, data = oblisurv)
summary(oblibiomodel1) #no log 

#nothing sig
#can't compute interaction between grass and watering
# we looked at the response for each species etc. Couldn't looked at interaction for Obli because of numbers of mortality

oblibio_resid<- simulateResiduals(oblibiomodel)
plot(oblibio_resid)

vimibiomodel1 <- lm(log(Above_biomass) ~ Grass_Treatment + Watering + Grass_Treatment:Watering, data = vimisurv)
summary(vimibiomodel1) #keep log

#nothing sig

vimibio_resid<- simulateResiduals(vimibiomodel)
plot(vimibio_resid)

#MODEL 2#

##same model but without interaction between watering and grass

amygbiomodel2 <- lm(log(Above_biomass) ~ Grass_Treatment + Watering, data = amygsurv)
summary(amygbiomodel2)

AIC(amygbiomodel1, amygbiomodel2) 
#Akaike Information Criterion - evaluates models - the more simple model is considered best (principle of parsimony)
#value within 2 says they are equally good at describing the data
##model 2 better than 1

#MODEL 3#

amygbiomodel3 <- lm(log(Above_biomass) ~ Grass_Treatment, data = amygsurv)
summary(amygbiomodel3)

AIC(amygbiomodel1, amygbiomodel2, amygbiomodel3) #model 3 best fit

#plotting above-ground biomass

ggplot(growthdata, aes(x = Grass_Treatment, y = log(Above_biomass), fill = Watering))+
  geom_boxplot()+
  geom_jitter(position = position_jitterdodge(), alpha = 0.3)+
  ylab("Above-ground Biomass")+
  xlab("Grass Treatment")+
  labs(fill="Watering Treatment")+
  scale_x_discrete(labels=c("Grass", "Control", "Grass", "Control", "Grass", "Control"))+
  theme_classic()+
  scale_fill_manual(values=c("sienna2", "skyblue2"))+
  facet_wrap(vars(Species))

abovebiomassplot + scale_color_discrete(name="Watering Treatment", labels = c("Drought", "Control"))


ggplot(seeddatanounk, aes(x = Treatment, y = log1_viable_seeds, fill = Neighbours))+
  geom_boxplot()+
  geom_point(position = position_jitterdodge(), alpha = 0.3)+
  ylab("log(Fecundity+1)")+
  xlab("Watering treatment")+
  theme_classic()+
  scale_fill_manual(values=c("gold", "deepskyblue"))+
  my_theme+
  facet_wrap(vars(Species))


#Plot per species showing watering and grass treatment

hist(mtgdata$mtg)

hist(log(amygsurv$Above_biomass)) #log

hist(oblisurv$Above_biomass) #do not log

hist(log(vimisurv$Above_biomass)) #log

##BELOW GROUND BIOMASS##

amygbelowbiomodel <- lm(Below_biomass ~ Watering + Grass_Treatment:Watering, data = amygsurv)
summary(amygbelowbiomodel)

oblibelowbiomodel <- lm(Below_biomass ~ Grass_Treatment + Watering + Grass_Treatment:Watering, data = oblisurv)
summary(oblibelowbiomodel)

vimibiomodel <- lm(Below_biomass ~ Grass_Treatment + Watering + Grass_Treatment:Watering, data = vimisurv)
summary(vimibiomodel)


ggplot(growthdata, aes(x = Grass_Treatment, y = log(Below_biomass), fill = Watering))+
  geom_boxplot()+
  geom_jitter(position = position_jitterdodge(), alpha = 0.3)+
  ylab("Below-ground Biomass")+
  xlab("Grass Treatment")+
  labs(fill="Watering Treatment")+
  scale_x_discrete(labels=c("Grass", "Control", "Grass", "Control", "Grass", "Control"))+
  theme_classic()+
  scale_fill_manual(values=c("sienna2", "skyblue2"))+
  facet_wrap(vars(Species))


#plot ratio of below-ground to above-ground --> don't have the data to statistically analyse this
# assessment of investment by plants
## supplementary information in appendix

#need to log it and do histograms


#make a model per species then run anova

#aov(amygsurvivalmodel)


###### Growth: aboveground biomass #data should have normal dist = lm
# biomassmodel <- lm(AGBM ~ Grass_presence + Watering + Species, data = df)
#FOR GROWTH... I expect species to differ in their overall growth because of different growth rates.
#So that isn't interesting. What would be interesting is whether their response to grass/watering differs.
#But in order to model this do we need to justify why we expect species to differ in their responses?
#DO I NEED TO SCALE GROWTH ACROSS SPECIES THIS? As fraction of maximum (like I did with seeds for WA stuff)

hist(mtgdata$mtg)

hist(log(mtgdata$mtg)) #closer to normal distribution

#lm(log(biomass) ~ grass + water, data)


###### Growth: height, through time
#HOW TO MODEL INCREMENTS THROUGH TIME?* Especially when severity of watering increased through time... no idea.

#use emmeans to see if species differ in their average values of response
library(emmeans)
test <- emmeans(germmodel, "Species")
pairs(test)
#emmeans prob scale, backtransform and plot #comparing average values across multiple levels 

##### 3. Does the impact of grass change with water availability? #####
# e.g. does biomass change with grass presence, watering, or their interaction?

# Test by a positive interaction between grass presence and watering
#May need to be logged?

#### Not a core research Q - more of a methods Q: ####
#Did stomatal conductance change in presence or absence of drought?
#Proxy for: were plants closing stomata in response to drought?
# Visually match these to probe readings over time, just lines of stomatal conductance and lines of water availability
# probably two panels in one graph



# Import stomatal conductance data
stomatadata <- read_csv("Data/courtney_stomatalcond_data.csv")
#Need it to be in long format with Week as wk_1, wk_2 or wk_3
longstomatadata <- gather(data = stomatadata, key = Week, value = Stomatal_conductance, SC_wk1, SC_wk2, SC_wk3)

#Plotting it
ggplot(longstomatadata, aes(x = Week, y = Stomatal_conductance)) + 
  geom_point(colour="lightblue",size=1.6) + 
  geom_point(stat="summary", fun.y="mean",size=2) +
  geom_errorbar(stat="summary", fun.data="mean_se",width=0,size=.8) +
  theme_bw()
#This gives means overall, but not to separate individual plants

#By plant:
str(longstomatadata)
# Need pot to be a factor, not numeric
longstomatadata$Pot <- as.factor(longstomatadata$Pot)


#Colouring by pot
ggplot(longstomatadata, aes(x = Week, y = Stomatal_conductance, group = Pot, colour = Pot)) + 
  geom_point(stat="summary", fun ="mean",size=2.5, position=position_dodge(width=0.3)) +
  geom_smooth(stat="summary", position=position_dodge(width=0.3), size = 1, alpha = 0.2)+
  geom_errorbar(stat="summary", fun.data="mean_se",width=0,size=1, position=position_dodge(width=0.3)) +
  theme_classic()

#Colouring by drought or control
ggplot(longstomatadata, aes(x = Week, y = Stomatal_conductance, group = Pot, colour = Control_or_drought)) + 
  geom_point(stat="summary", fun ="mean",size=2.5, position=position_dodge(width=0.3)) +
  geom_smooth(stat="summary", position=position_dodge(width=0.3), size = 1, alpha = 0.2)+
  geom_errorbar(stat="summary", fun.data="mean_se",width=0,size=1, position=position_dodge(width=0.3)) +
  theme_classic()
#Grouping by drought or control
#This is probably the most clearly informative one!
# At the moment the raw data is a bit misleading though since it is plotting each leaf
# and should be plotting each individual plant - so this is where I would use your average values!
ggplot(longstomatadata, aes(x = Week, y = Stomatal_conductance, group = Control_or_drought, colour = Control_or_drought)) + 
  geom_point(alpha = 0.4, position=position_dodge(width=0.3))+
  geom_point(stat="summary", fun ="mean",size=2.5, position=position_dodge(width=0.3)) +
  geom_smooth(stat="summary", position=position_dodge(width=0.3), size = 1, alpha = 0.2)+
  geom_errorbar(stat="summary", fun.data="mean_se",width=0,size=1, position=position_dodge(width=0.3)) +
  ylab("Stomatal Conductance") +
  labs(colour = "Watering Treatment") +
  scale_x_discrete(labels=c(1,2,3))+
  scale_color_manual(values=c("darkblue", "red"))+
  theme_classic()
  

  
#Up to you how you want to show it! Play around with the different options :)
library(tidyverse)
soilmoisturedata <- read_csv("Data/Soil_moisture_copy.csv")

#ggplot(soilmoisturedata, aes(x = Date, y = Logger1_probe1))+
  #geom_line(alpha = 0.4, position=position_dodge(width=0.3))+
  #theme_classic()+
  #scale_fill_manual(values=c("Tomato","steelblue3"))

library(ggplot2)
library(scales)

##SOIL MOISTURE PLOT##

ggplot() + geom_line(aes(x = soilmoisturedata$Date, y = soilmoisturedata$Logger1_probe1, colour= "blue")) +
  geom_line(aes(x = soilmoisturedata$Date, y = soilmoisturedata$Logger1_probe2, colour= "blue"))+
  geom_line(aes(x = soilmoisturedata$Date, y = soilmoisturedata$Logger2_probe1, colour="red"))+
  geom_line(aes(x = soilmoisturedata$Date, y = soilmoisturedata$Logger2_probe2, colour="red"))+
  ylab("Soil Moisture Content (m³/m³)")+
  xlab("Time")+
  labs(colour = "Treatment")+
  scale_x_discrete(labels=c(1,2,3))+
  scale_color_manual(labels = c("Drought", "Control"),  values = c("red","darkblue"))+
  theme_classic()


  
xaxt = ("n") +
  scale_x_discrete(labels=c("22/10", "27/10", "01/11", "06/11","11/11"))+
                      
scale_x_discrete(labels=c(22/10, 27/10, 01/11, 06/11, 11/11))+


ggplot(soilmoisturedata, aes(y = Logger1_probe1, x = Date))+
  geom_point(aes(colour = ))+
  geom_line(aes(colour = ))+
  ylab("Soil Moisture")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))

##SURVIVAL##
#Error bars and mean probability
ggplot(growthdata, aes(x = Grass_Treatment, y = Survival, group = Watering))+
  geom_jitter(position = position_jitterdodge(), alpha = 0.3, aes(colour = Watering))+
  geom_errorbar(position = position_dodge(width = 0.75), stat="summary", fun.data="mean_se",width=0,size=.8, aes(colour = Watering)) +
  geom_point(position = position_dodge(width = 0.75), stat="summary",size=2.5, shape = 4, stroke = 1.2, aes(colour = Watering)) +
  ylab("Survival probability")+
  xlab("Grass treatment")+
  scale_x_discrete(labels=c("Grass", "Control", "Grass", "Control", "Grass", "Control"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_color_manual(values=c("red", "darkblue"))+
  facet_wrap(~Species)

#long formating data example

df$timestamp = as.Date(strptime(df$timestamp, "%Y-%m-%dT%H:%M:%S"))
#This works then:
  
  plot(df$timestamp,df$pages,xaxt="n")
axis.Date(1,at=df$timestamp,labels=format(df$timestamp,"%b-%d"),las=2)
dm$DateTime <- as.POSIXct(dm$timestamp, format="%Y-%m-%dT%H:%M:%S")

