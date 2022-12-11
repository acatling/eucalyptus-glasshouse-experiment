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