####### Eucalyptus drought experiment - GROWTH DATA
### Single species drought responses
# Plants I inherited.
# E. marginata, E. ovata, E. salmonophloia, Corymbia calophylla 
# 2019

library(tidyverse)
library(ggplot2)
library(lubridate) # for formatting date

eucgrowthdata <- read_csv("Data/euc_growth_first_group.csv")

eucgrowthdataclean <- eucgrowthdata %>% select(Date, Species, 
                                      Pot = 'Pot Number', 
                                Height = 'Height first plant (mm)',
                                No_leaves = 'Number of leaves first plant')
ggplot(eucgrowthdataclean, aes(x = Species, y = Height))+
  geom_boxplot()+
  geom_jitter(aes(colour = Species))+
  theme_bw()

# Need to isolate each species
# Eucalyptus marginata
### View height at one time point

eucdataem <- eucgrowthdataclean %>% filter(Species == "Eucalyptus marginata") %>%
  filter(Date == '22/01/2020')

  ggplot(eucdataem, aes(x = Species, y = Height))+
  geom_boxplot()+
  geom_jitter(aes(colour = Species))+
  theme_bw()

# Line graph, growth over time
  #Note that I needed dates to be recognised as dates

eucdataem$Date <- as.Date(eucdataem$Date, "%d/%m/%y")
eucgrowthdataclean$Date <- as.Date(eucgrowthdataclean$Date, "%d/%m/%y")
str(eucdataem)
str(eucgrowthdataclean)

ggplot(eucgrowthdataclean, aes(x = Date, y = Height, colour = Species))+
  geom_jitter() +
  geom_smooth(method = "lm")+
  theme_bw()

#Pots are currently numeric, no idea why I wanted to do the below though (03/10/2021)
eucgrowthdataclean$Pot <- as.factor(eucgrowthdataclean$Pot)

ggplot(eucgrowthdataclean, aes(x = Date, y = Height, colour = Pot))+
  geom_point() +
  theme_bw()

ggplot(eucgrowthdataclean, aes(x = Date, y = No_leaves, colour = Pot))+
  geom_jitter(position=position_jitter(0.05)) +
  theme_bw()
         

#### Second lot of plants

eucnewgrowthdata <- read_csv("Data/Euc_new_plants_growth_data.csv")
str(eucnewgrowthdata)

ggplot(eucnewgrowthdata, aes(x = Species, y = Height))+
  geom_boxplot()+
  geom_jitter(aes(colour = Species))+
  theme_bw()

# 33 ovatas!
eucnewgrowthdata %>% filter(Species == "Eucalyptus ovata") %>% tally()
# 25 marginatas! 17 are old, 8 new
eucnewgrowthdata %>% filter(Species == "Eucalyptus marginata") %>% tally()
# 4 salmonophloias!
eucnewgrowthdata %>% filter(Species == "Eucalyptus salmonophloia") %>% tally()
# 42 Corymbia calophyllas!
eucnewgrowthdata %>% filter(Species == "Corymbia calophylla") %>% tally()

#Chrissy's Eucs - initial heights

chrissyeucsize <- read_csv("Data/chrissy_euc_height_data.csv")
head(chrissyeucsize)

ggplot(chrissyeucsize, aes(x = species, y = wk_1))+
  geom_boxplot()+
  geom_jitter(aes(colour = species))+
  theme_bw()

## Testing date format imports

testdate <- read_csv("Data/testing_date.csv")
str(testdate)

testdate <- as.Date(testdate$Date, "%m-%d-%y")

