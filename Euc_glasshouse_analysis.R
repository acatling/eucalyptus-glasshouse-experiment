####### Euc glasshouse analysis
#July 2021

library(tidyverse)
library(ggplot2)

#Starting with 2020 plants

data2020plants <- read_csv("Data/Final_harvest_data_2020_plants.csv")
leafarea2020plants <- read_csv("Data/leaf_area_2020_plants.csv")

#Merging leaf area from leaf scans for each individual
#Need to filter out the one NA first
#Filtering to one row at the end
leafareadata <- leafarea2020plants %>% filter(!is.na(Leaf_area)) %>% group_by(Species, Pot_number, Plant_part) %>% 
  mutate(leaf_area_total = sum(Leaf_area)) %>% filter(row_number() == 1) %>%
  select(Species, Pot_number, Plant_part, leaf_area_total)
alldata <- left_join(data2020plants, leafareadata)

traitdata <- alldata %>% mutate(SLA = leaf_area_total/Dry_mass_g,
                           LDMC = Fresh_mass_g/Dry_mass_g)

#Import data on control or drought
treatment <- read_csv("Data/treatments_2020_plants.csv")
alltraitdata <- left_join(traitdata, treatment)

#Chrissy calculated SLA and LDMC as one value per plant - total leaf area from branches
# and remaining. So I need to combine leaf areas from terminal branches and Rest_leaves
#CC7C - didn't scan leaves, but have fresh and dry weights for calculating LDMC.

sladata <- alltraitdata %>% filter(!is.na(leaf_area_total)) %>%
  group_by(Species, Pot_number) %>% mutate(leaf_area_total_plant = sum(leaf_area_total)) %>%
  filter(row_number() == 1)

## LDMC data
#Note that there are a lot of NAs here for now, because some dry weights missing from Rest_leaves
#If I don't get those values, can filter by dry mass too and just use what we have
ldmcdata <- alltraitdata %>% filter(!is.na(Fresh_mass_g)) %>% group_by(Species, Pot_number) %>%
  mutate(ldmc_total_plant = sum(LDMC)) %>% filter(row_number() == 1)

ldmcdatanona <- alltraitdata %>% filter(!is.na(Fresh_mass_g)) %>% filter(!is.na(Dry_mass_g)) %>%
  group_by(Species, Pot_number) %>% mutate(ldmc_total_plant = sum(LDMC)) %>% filter(row_number() == 1)

#### Plotting
sladata %>% filter(Treatment == "C") %>%
ggplot(aes(x = Species, y = log(SLA)))+
  geom_boxplot()+
  geom_jitter(alpha=0.4)+
  theme_classic()

ggplot(aes(x = Species, y = log(SLA)), data = sladata)+
  geom_boxplot()+
  geom_jitter(alpha=0.4)+
  theme_classic()

test <- aov(SLA ~ Species, sladata)
summary(test)
TukeyHSD(test)

ggplot(aes(x = Species, y = log(LDMC)), data = ldmcdatanona)+
  geom_boxplot()+
  geom_jitter(alpha=0.4)+
  theme_classic()

test <- aov(LDMC ~ Species, ldmcdatanona)
summary(test)
TukeyHSD(test)

ggplot(aes(x = Treatment, y = log(SLA)), data = alltraitdata)+
  geom_boxplot()+
  geom_jitter(alpha = 0.4)+
  theme_classic()+
  facet_wrap(~Species)

ggplot(aes(x = Treatment, y = log(LDMC)), data = alltraitdata)+
  geom_boxplot()+
  geom_jitter(alpha = 0.4)+
  theme_classic()+
  facet_wrap(~Species)

#Wood density #volume/dry mass
#Need to enter volume data?

##########AL:As
#Filtering out a few rows where notes say it's not a TB or has no leaf area or sapwood diameter (NAs)
alasdata <- alltraitdata %>% filter(Plant_part == "A" | Plant_part =="B" | Plant_part =="C") %>% 
  group_by(Species, Pot_number, Plant_part) %>% 
       mutate(sapwood_area = pi*(Diameter_TB_sapwood_mm/2)^2,
        al_as = leaf_area_total/sapwood_area) %>% filter(!is.na(al_as))
#Calculating mean AL:As per plant
meanalas <- alasdata %>% group_by(Species, Pot_number) %>% 
  summarise(meanalas = mean(al_as), sd = sd(al_as))

ggplot(aes(x = Species, y = log(meanalas)), data = meanalas)+
  geom_boxplot()+
  geom_jitter(alpha = 0.4)+
  theme_classic()+
  facet_wrap(~Species)

#Root traits
#Above:below ground biomass


