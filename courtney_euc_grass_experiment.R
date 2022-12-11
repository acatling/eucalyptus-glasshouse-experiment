##### Courtney Peirce's Masters Experiment
### September 2021


#Note that naming columns numbers (e.g. 26/08/21) is not great practice
# since you need to call it in these: `` otherwise it won't run
# but we do that here anyway

#Loading in tidyverse package
library(tidyverse)
library(lmerTest)
library(emmeans)
#Reading data in from my R project location
germinationdata <- read_csv("Data/courtney_germination_270921.csv")

#### Checking sample sizes ####
#A = AMYG = E. amygdalina
#B = OBLI = E. obliqua
#C = OVAT = E. ovata
#D = VIMI = E. viminalis

#Tallying number of pots with at least one germinated eucalypt
#Grouped by the number of seeds sown (e.g. intended A or AAAA) and presence or absence of grass
samplesizes <- germinationdata %>% group_by(Identification, Grass_presence) %>% 
  tally(`27/09/21` > "0")

#Tally for each species in presence or absence of grass
#First assigning Species names (making a new column called Species)
#e.g. A or AAAA has Species "AMYG"
germinationdata$Species[germinationdata$Identification == "A" | germinationdata$Identification == "AAAA"] <- "AMYG"
germinationdata$Species[germinationdata$Identification == "B" | germinationdata$Identification == "BBBB"] <- "OBLI"
germinationdata$Species[germinationdata$Identification == "C" | germinationdata$Identification == "CCCC"] <- "OVAT"
germinationdata$Species[germinationdata$Identification == "D" | germinationdata$Identification == "DDDD"] <- "VIMI"
germinationbyspecies <- germinationdata %>% group_by(Species, Grass_presence) %>% 
  tally(`27/09/21` > "0")

## Number that have at least two germinants
atleasttwosamplesize <- germinationdata %>% group_by(Species, Grass_presence) %>% 
  tally(`27/09/21` > "1")

#### Analysing germination over time ####
## Need to convert data to long format, using gather or pivot longer functions
# It will have 2560 rows because there are 16 dates and 160 rows/pots (16*160=2560)
longdata <- gather(data = germinationdata, key = Date, value = Number_germinants, '26/08/21', '28/08/21', '30/08/21', '01/09/21', '03/09/21', '05/09/21', '07/09/21',
       '09/09/21', '11/09/21', '13/09/21', '15/09/21', '17/09/21', '19/09/21', '21/09/21', '23/09/21', '27/09/21')

#Summing the total number of germinants by species in the presence or absence of grass
# 128 rows because 4 species * 2 levels of grass * 16 dates
totalgerm <- longdata %>% group_by(Species, Grass_presence, Date) %>% 
  mutate(group_germinants = sum(Number_germinants)) %>% filter(row_number() == 1)
#Dates need to be formatted as dates so that they plot in order
str(totalgerm)
totalgerm$Date = as.Date(totalgerm$Date, format="%d/%m/%Y")

#Plotting it
ggplot(totalgerm, aes(y = group_germinants, x = Date))+
  geom_point(aes(colour = Grass_presence))+
  geom_line(aes(colour = Grass_presence))+
  ylab("Grouped number of germinants")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))+
  facet_wrap(~Species)

#### Does the presence of grass influence the total amount of germination? ####
#Need to calculate germination rates for each pot: number seeds germinated/number of seeds sown
#We can tell how many seeds were sown by 'Identification':
#A/B/C/D had x number of seeds, AAAA/BBBB/CCCC/DDDD had y number of seeds. x=5 and y=20???

## Was time to germination influenced by the presence of grass? ####
# For each pot, calculate the mean time to germination (MTG).
# e.g. If five plants germinated, 1 on day 5, 1 on day 7, and 3 on day 8:
#MTG is (1*5+1*7+3*8)/5 = 7.2 days.
#Then make a boxplot of MTG in the presence and absence of grass for each species
# And can do a t-test to answer this!

#### Problem with knowing the amount of germination:
# Do we know the number of seeds that germinated for each pot? Was mortality tracked?
# For example, if a pot had 5 plants one week and 3 plants the next...
# Are we sure that 2 plants died, or do we have the info to know if 3 plants 
#died and another 1 germinated for example??
# This is a problem for calculating MTG too

#08/11/2021
#Need long format, columns for pot #, time (days since start of exp), 
# # plants in the pot at that time, # new germinants (between that time point and the previous time point)
# calculate total_germination as the sum of the number of new germinants per pot.
# calculate mean time to germination (MTG) as the # germinants at a given point in time * that time, as an average

#Reordering by pot number so that it's easier to check calculations
longdata <- longdata[order(longdata$Pot_number), ]
#This is calculating the difference between the number of germinants in the current row and the previous row
longdata <- longdata %>% group_by(Pot_number) %>%
  mutate(new_germ = Number_germinants - lag(Number_germinants, default = first(Number_germinants)))
#Not interested in where it goes down for now, so changing negative values to zero
# so that it's easier to calculate the total germination
longdata <- within(longdata, new_germ[new_germ<0] <- 0)
#Oh! Problem. This hasn't been including pots that started with germination on first check
# Pulling out these cases and computing total_germ and mtg for them separately. This is inefficient but not sure how else to do it
# Filtering for where germination > 0 on 26/08/21
initialgerm <- filter(germinationdata, `26/08/21` > 0)
# This creates a new column called germ_to_add and fills it with the same values that are in the column 26/08/21
initialgerm$germ_to_add <- initialgerm$`26/08/21`
#Selecting germ_to_add values only, with pot number
germtoadd <- initialgerm %>% select("Pot_number", "germ_to_add")
#Merging this info back with longdata
#left_join means that you are adding the values in the dataframe on the right to the dataframe on the left
longdata <- left_join(longdata, germtoadd)
#Making all NAs zeros so that we can sum them later
longdata <- longdata %>% replace(is.na(.),0)
#Total germination is total_germ plus germ_to_add
longdata <- longdata %>% group_by(Pot_number) %>% mutate(total_germ = sum(new_germ)+germ_to_add)

#Need to calculate germination fractions based on number of seeds that were sown
#Make a column for seeds_sown unless they were all the same?? 15 seeds?
#mutate(germ_fraction = total_germ/seeds_sown) or /15

#Calculating mean time to germination
#Need a column for the number of days (rather than date) to calculate MTG
#Cold stratifying of seeds was done on 16/08/2021 so this is day 0
#This is a slow way to do it but it works!
longdata <- longdata %>% mutate(days = case_when(Date == '26/08/21' ~ "10",
                                              Date == '28/08/21' ~ "12",
                                              Date == '30/08/21' ~ "14",
                                              Date == '01/09/21' ~ "16",
                                              Date == '03/09/21' ~ "18",
                                              Date == '05/09/21' ~ "20",
                                              Date == '07/09/21' ~ "22",
                                              Date == '09/09/21' ~ "24",
                                              Date == '11/09/21' ~ "26",
                                              Date == '13/09/21' ~ "28",
                                              Date == '15/09/21' ~ "30",
                                              Date == '17/09/21' ~ "32",
                                              Date == '19/09/21' ~ "34",
                                              Date == '21/09/21' ~ "36",
                                              Date == '23/09/21' ~ "38",
                                              Date == '27/09/21' ~ "42"))
#Don't want to include records of zero in calculation of mtg so will filter to
#where new_germ > 0, calculate it, then merge values back
#Currently working on this!! Not finished
#Pot 6 should be (2*14+1*16+1*20)/4 = 16 days.
filteredlongdata <- longdata %>% filter(new_germ != 0)
#Now need to make a new column for MTG
filteredlongdata$days <- as.numeric(filteredlongdata$days)
filteredlongdata$days_germ <- filteredlongdata$new_germ * filteredlongdata$days
#Make a column for days_germ_extra for those with germination on the first day, 26/08/21
# Then mtg will actually be days_germ (which is new_germ*days) PLUS days_germ_extra / total_germ (where total_germ has already added in germ on 26/08/21)
#Times ten because that is the number of days at 26/08/21
mtgtoadd <- germtoadd %>% mutate(days_germ_extra = germ_to_add*10) %>% select("Pot_number", "days_germ_extra")
#Merging it back with filteredlongdata
filteredlongdata <- left_join(filteredlongdata, mtgtoadd)
#Making NAs zeros
filteredlongdata <- filteredlongdata %>% replace(is.na(.),0)

#Calculates MTG as a new column and then filters to one value per pot
filteredlongdata <- filteredlongdata %>% group_by(Pot_number) %>% 
  mutate(mtg = (sum(days_germ)+days_germ_extra)/total_germ) %>% filter(row_number() == 1)
#Seems to be working!
#Merging the mtg values back into long dataset
longdata$days <- as.numeric(longdata$days)
germdatalong <- left_join(longdata, filteredlongdata)
#Problem above is that the values haven't filled out for entire rows of a given pot

allgermdata <- left_join(germinationdata, filteredlongdata)
#Replacing NAs with zeros for total_germ and mtg columns
allgermdata[c("total_germ", "mtg")][is.na(allgermdata[c("total_germ", "mtg")])] <- 0

#I think this is a good time to remove ovata from the dataset! :(
allgermdata <- allgermdata %>% filter(!Species == "OVAT")

# To plot and analyse mtg, want only pots that had germination, so filtering out rows where mtg = 0
mtgdata <- allgermdata %>% filter(mtg != 0)

#Plotting mtg for each species
ggplot(mtgdata, aes(x = Species, y = mtg, fill = Grass_presence))+
  geom_boxplot()+
  geom_jitter(position = position_jitterdodge(), alpha = 0.3)+
  theme_classic()+
  scale_fill_manual(values=c("gold", "deepskyblue"))
#Probably want to remove the pots that didn't germinate before plotting and analysing mtg?

# Plotting total_germ
ggplot(allgermdata, aes(x = Species, y = total_germ, fill = Grass_presence))+
  geom_boxplot()+
  geom_jitter(position = position_jitterdodge(), alpha = 0.3)+
  theme_classic()+
  scale_fill_manual(values=c("gold", "deepskyblue"))

## Question: How does the presence of grass influence germination for each species?
#One model per species
#Model of total germination
#using cbind so that it is scaled by the number of seeds that were sown
# Need a column for the number of seeds that didn't germinate.
#Were there 20 seeds sown?? Assuming so but update this later*
allgermdata <- allgermdata %>% mutate(total_no_germ = 20-total_germ)
#Filtering the datasets for each species
germamyg <- allgermdata %>% filter(Species == "AMYG")
germobli <- allgermdata %>% filter(Species == "OBLI")
germvimi <- allgermdata %>% filter(Species == "VIMI")
#Remember that mtg dataset is different

#Modelling using cbind with generalised negative binomial model
#What are the random effects? If any...
### AMYG
amyggermmodel <- glm(cbind(total_germ, total_no_germ) ~ Grass_presence, family = binomial, data = germamyg)
summary(amyggermmodel)
#Q: When would you use a linear model over a t-test/anova?
#Here I could just use a t-test for each species...
#Q: When would you run model with all species vs. individual models for each species? And include them as random of fixed effects?
### OBLI
obligermmodel <- glm(cbind(total_germ, total_no_germ) ~ Grass_presence, family = binomial, data = germobli)
summary(obligermmodel)
### VIMI
vimigermmodel <- glm(cbind(total_germ, total_no_germ) ~ Grass_presence, family = binomial, data = germvimi)
summary(vimigermmodel)

## The presence of grass did not have a significant influence on the germination fraction for any species
## All species in one model, as random effects?
germmodel <- glmer(cbind(total_germ, total_no_germ) ~ Grass_presence + (1|Species), family = binomial, data = allgermdata)
summary(germmodel)
#Species as fixed effects
germmodel2 <- glm(cbind(total_germ, total_no_germ) ~ Grass_presence + Species, family = binomial, data = allgermdata)
summary(germmodel2)
## Need to look at the fit of all of these models. Using dharma.

#Species as fixed effects is telling me that the germination of vimi is significantly different from other species?
#Testing whether the interaction of germination with grasses is dependent on species
germmodel3 <- glm(cbind(total_germ, total_no_germ) ~ Grass_presence + Species + Grass_presence*Species, family = binomial, data = allgermdata)
summary(germmodel3)
#Conclusion: no, the impact of grass presence on germination does not vary by species?

###Need to look at whether variables need to be transformed!! 
# No continuous variables
#Need to look at model fits

## Research questions:
######### 1. Does the presence of grass affect eucalypt seedling germination? #######

##### Probably of germination:
#germmodel <- glmer(cbind(total_germ, total_no_germ) ~ Grass_presence + Species + Grass_presence:Species, family = binomial, data = allgermdata)
#summary(germmodel)
#DO THIS BY SPECIES!:
germmodel <- glmer(cbind(total_germ, total_no_germ) ~ Grass_presence + (1|Pot_number), family = binomial, data = allgermdata)
summary(germmodel)
##### Mean time to germination (have to describe why this is an important variable):
#Suspect I have to subset the data by what actually germinated*
#mtgmodel <- lm(mtg ~ Grass_presence + Species + Grass_presence:Species, data = mtgdata)
#summary(mtgmodel)
#Sanity check that obli response isn't different
# oblimtg <- mtgdata %>% filter(Species == "OBLI")
# oblimtgmodel <- lm(mtg ~ Grass_presence, oblimtg)
# summary(oblimtgmodel)

#### 2. How does water availability and the presence of grass influence survival and growth? ####

###### Survival: same as germ
#Need to create a column with binary survival or not ... 1s and 0s
#survmodel <- glm(Survival ~ Grass_presence + Watering + Grass:Watering, family = binomial, data = df)

###### Growth: aboveground biomass
# biomassmodel <- lm(AGBM ~ Grass_presence + Watering + Species, data = df)
#FOR GROWTH... I expect species to differ in their overall growth because of different growth rates.
#So that isn't interesting. What would be interesting is whether their response to grass/watering differs.
#But in order to model this do we need to justify why we expect species to differ in their responses?
#DO I NEED TO SCALE GROWTH ACROSS SPECIES THIS? As fraction of maximum (like I did with seeds for WA stuff)

###### Growth: height, through time
#HOW TO MODEL INCREMENTS THROUGH TIME?* Especially when severity of watering increased through time... no idea.

#use emmeans to see if species differ in their average values of response
test <- emmeans(germmodel, "Species")
pairs(test)
#emmeans prob scale, backtransform and plot

##### 3. Does the impact of grass change with water availability? #####
# e.g. does biomass change with grass presence, watering, or their interaction?
#Species as fixed or random effect?**
# biomassmodel <- glmer(biomass ~ Grass_presence + Watering + Grass_presence*Watering + (1|Species), data = df)
# Test by a positive interaction between grass presence and watering
#May need to be logged?

#### Not a core research Q - more of a methods Q: ####
#Did stomatal conductance change in presence or absence of drought?
#Proxy for: were plants closing stomata in response to drought?
# Visually match these to probe readings over time, just lines of stomatal conductance and lines of water availability
# probably two panels in one graph




