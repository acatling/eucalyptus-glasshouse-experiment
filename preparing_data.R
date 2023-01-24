####### Eucalyptus Species Interactions Drought Experiment
## Species interactions experiment 2021
#Eucalyptus amygdalina, Eucalyptus obliqua, Eucalyptus ovata and Eucalyptus viminalis

### Importing packages ####
library(tidyverse)
library(ggplot2)

### Preparing data ####
heightdata <- read_csv("Data/sp_interactions_exp/height_data_2021.csv")
#height data for each individual plant in each pot
mortalitydata <- read_csv("Data/sp_interactions_exp/mortality_data_2021.csv")

#Why difference in number of rows?
# The difference is the mystery 5th plant in pot 3 AACC. Row 7 in height data - 
# The order is correct for all of these but we only have height data for one of the As
# on the last date (no initial height, was likely very small).
#Will left_join only mortalitydata but add this strange row back in
rowtoadd <- heightdata %>% filter(Pot_number == 3 & Plant_number == 5)

## Making sure species ID is correct 
mortalitydata$Species <- mortalitydata$Species_where_mixed
mortalitydata <- within(mortalitydata, Species[is.na(Species) & Composition == "AMYG-A"] <- 'A')
mortalitydata <- within(mortalitydata, Species[is.na(Species) & Composition == "OBLI-B"] <- 'B')
mortalitydata <- within(mortalitydata, Species[is.na(Species) & Composition == "OVAT-C"] <- 'C')
mortalitydata <- within(mortalitydata, Species[is.na(Species) & Composition == "VIMI-D"] <- 'D')
mortalitydata <- within(mortalitydata, Species[is.na(Species) & Composition == "AAAA"] <- 'A')
mortalitydata <- within(mortalitydata, Species[is.na(Species) & Composition == "AAA"] <- 'A')
mortalitydata <- within(mortalitydata, Species[is.na(Species) & Composition == "BBBB"] <- 'B')
mortalitydata <- within(mortalitydata, Species[is.na(Species) & Composition == "BB"] <- 'B')
mortalitydata <- within(mortalitydata, Species[is.na(Species) & Composition == "CCCC"] <- 'C')
mortalitydata <- within(mortalitydata, Species[is.na(Species) & Composition == "DDDD"] <- 'D')
mortalitydata <- within(mortalitydata, Species[is.na(Species) & Composition == "DD"] <- 'D')
#Pot 84AB, two rows but only one plant so removing row with nothing
mortalitydata <- mortalitydata %>% filter(!(is.na(Species) & Pot_number==84))
### Removing rows with no plants, 5 of them
#This doesn't work because it only filters to things with notes (except Nothing there)!
#test <- mortalitydata %>% filter(!(Notes == "Nothing there"))
toremove <- mortalitydata %>% filter(Notes == "Nothing there")
mortalitydata <- anti_join(mortalitydata, toremove)
#Removing 39 CCDD repeated rows (two of them)
mortalitydata <- mortalitydata %>% filter(!(is.na(Species)))

#Merging height and mortality data
alldata <- left_join(mortalitydata, heightdata)
alldata <- left_join(alldata, rowtoadd)
alldata <- alldata %>% select(Pot_number, Plant_number, Species, Composition, Notes, Percent_death_0209,
                              Percent_death_0909, Percent_death_1609, Percent_death_2309, Percent_death_3009,
                              Percent_death_0710, Percent_death_1410, Percent_death_2110, Height_1907,
                              Notes_1907, Height_0209, Notes_0209, Height_0909, Height_2110, Notes_2110, Green_stem_2110)

##Renaming Species
alldata <- within(alldata, Species[Species == "A"] <- "AMYG")
alldata <- within(alldata, Species[Species == "B"] <- "OBLI")
alldata <- within(alldata, Species[Species == "C"] <- "OVAT")
alldata <- within(alldata, Species[Species == "D"] <- "VIMI")

# Making plant-within-pot ID, keeping species to be informative
alldata <- alldata %>% unite("plantid", Pot_number:Plant_number:Species, remove = FALSE)

### Importing soil moisture data ####
moisturedataraw <- read_csv("Data/sp_interactions_exp/soil_moisture2.csv")
###### Calculating sample sizes 22/08/2021 ####
samplesizedata <- read_csv("Data/sp_interactions_exp/sample_sizes.csv")
#samplesizes <- samplesizedata %>% group_by(Composition) %>% tally()
#Sample sizes of droughts and control pots - allocated starting with C
# by composition then C/D/C/D based on first two days of transpiration
samplesizeswatering <- samplesizedata %>% group_by(Composition, C_or_D) %>% tally()
##Updated sample sizes December 2022 ####
onerowpot <- alldata %>% group_by(Pot_number) %>% filter(row_number()==1)
samplesizestable <- onerowpot %>% group_by(Composition) %>% tally()

## Merging with watering treatment data
treatmentdata <- samplesizedata %>% select(Pot_number, C_or_D)
alldata <- left_join(alldata, treatmentdata)

### Need to make data long for Height and survival by Date
#Splitting height and mortality to pivot separately then merge
heightdata <- alldata %>% select(plantid, Pot_number, Plant_number, Species, Composition, Height_1907,
                                 Height_0209, Height_0909, Height_2110, Green_stem_2110)
heightdata$Height_0909 <- as.numeric(heightdata$Height_0909)
heightdata$Height_2110 <- as.numeric(heightdata$Height_2110)
#89-1 says 'Dead' instead of height, this line makes that an NA. Height 2110 same plant says 10?
heightlong <- heightdata %>% pivot_longer(cols = starts_with('Height_'), names_to = "date", values_to = "height")
#Removing Height_ from date column and converting it into a date
heightlong$date <- gsub("Height_", "", heightlong$date)
heightlong$year <- '2021'
#Rearranging
heightlong <- heightlong %>% select(plantid, Pot_number, Plant_number, Species, Composition, Green_stem_2110, height, date, year)
heightlong <- heightlong %>% unite("date", date:year, sep = "")
heightlong$date <- as.Date(heightlong$date, "%d%m%Y")

## And for height notes
heightnotes <- alldata %>% select(plantid, Pot_number, Plant_number, Species, Composition, Notes_1907, Notes_0209, Notes_2110)
heightnoteslong <- heightnotes %>% pivot_longer(cols = starts_with('Notes_'), names_to = "date", values_to = "height_notes")
heightnoteslong$date <- gsub("Notes_", "", heightnoteslong$date)
heightnoteslong$year <- '2021'
#Rearranging
heightnoteslong <- heightnoteslong %>% select(plantid, Pot_number, Plant_number, Species, Composition, height_notes, date, year)
heightnoteslong <- heightnoteslong %>% unite("date", date:year, sep = "")
heightnoteslong$date <- as.Date(heightnoteslong$date, "%d%m%Y")

## And for mortality ##
mortalitydata <- alldata %>% select(plantid, Pot_number, Plant_number, Species, Composition, C_or_D, Notes, Percent_death_0209,
                                    Percent_death_0909, Percent_death_1609, Percent_death_2309, Percent_death_3009, 
                                    Percent_death_0710, Percent_death_1410, Percent_death_2110)
mortalitylong <- mortalitydata %>% pivot_longer(cols = starts_with('Percent_death_'), names_to = "date", values_to = "percent_mortality")
mortalitylong$date <- gsub("Percent_death_", "", mortalitylong$date)
mortalitylong$year <- '2021'
mortalitylong <- mortalitylong %>% select(plantid, Pot_number, Plant_number, Species, Composition, C_or_D, Notes, percent_mortality, date, year)
mortalitylong <- mortalitylong %>% unite("date", date:year, sep = "")
mortalitylong$date <- as.Date(mortalitylong$date, "%d%m%Y")

### Merging all the long dataframes together
alldatalong <- left_join(mortalitylong, heightlong)
alldatalong <- left_join(alldatalong, heightnoteslong)

## Create a column for time/week since onset of drought treatment
dates <- alldatalong %>% group_by(date) %>% filter(row_number()==1) %>% select(date)
dates$week <- 123
weeks <- c(0,1,2,3,4,5,6,7)
dates[,2] <- weeks
## Merging weeks info back into dataset
alldatalong <- left_join(alldatalong, dates)

#### Calculate time to mortality ####
#time to 100%, 80%, 50% or 95% leaf mortality
#note that this doesn't account for recovering (control plants mainly)
#This will filter to the first row where percent_mortality == 100. 
#Confirmed: 3_1 is week 2. #3_4 is week 3.
plants_100m <- alldatalong %>% group_by(plantid) %>% filter(percent_mortality == 100) %>% 
  filter(row_number()==1)
#Then assign time to this level of mortality
plants_100m$tt100m <- ifelse(plants_100m$percent_mortality == 100, plants_100m$week, NA)
#Then merge it back
plants_100m <- plants_100m %>% select(plantid, tt100m)
alldatalong <- left_join(alldatalong, plants_100m)
#80%
plants_80m <- alldatalong %>% group_by(plantid) %>% filter(percent_mortality >= 80) %>% 
  filter(row_number()==1)
plants_80m$tt80m <- ifelse(plants_80m$percent_mortality >= 80, plants_80m$week, NA)
plants_80m <- plants_80m %>% select(plantid, tt80m)
alldatalong <- left_join(alldatalong, plants_80m)
#95% same as 100 for number of plants.
plants_95m <- alldatalong %>% group_by(plantid) %>% filter(percent_mortality >= 95) %>% 
  filter(row_number()==1)
plants_95m$tt95m <- ifelse(plants_95m$percent_mortality >= 95, plants_95m$week, NA)
plants_95m <- plants_95m %>% select(plantid, tt95m)
alldatalong <- left_join(alldatalong, plants_95m)
#50%
plants_50m <- alldatalong %>% group_by(plantid) %>% filter(percent_mortality >= 50) %>% 
  filter(row_number()==1)
plants_50m$tt50m <- ifelse(plants_50m$percent_mortality >= 50, plants_50m$week, NA)
plants_50m <- plants_50m %>% select(plantid, tt50m)
alldatalong <- left_join(alldatalong, plants_50m)

### Extracting time to 100% mortality and 50% mortality and merging back to alldata
ttmdata <- alldatalong %>% select(plantid, tt100m, tt50m) %>% 
  group_by(plantid) %>% filter(row_number()==1)
alldata <- left_join(alldata, ttmdata)

### Calculate relative growth rate
## CHECK HEIGHT NAS in height data sheets -- so many weren't recorded?! **
# Final height - initial height / time, in cm per day
#Final height: 21/10/2022. Initial height: 19/07. # 02/09 is the start of the drought treatment
#02/09 - 21/10 is 49 days.
#19/07 - 21/10 is 63 days
#Predrought is between 19/07 and 02/09. 14 days.
alldata$Height_2110 <- as.numeric(alldata$Height_2110)
#Check this NA*
alldata$Height_1907 <- as.numeric(alldata$Height_1907)
alldata <- alldata %>% group_by(plantid) %>% 
  mutate(RGR_overall=(Height_2110-Height_1907)/63,
         RGR_duringdrought=(Height_2110-Height_0209)/49,
         RGR_predrought=(Height_0209-Height_1907)/14)

## Height reduction not biologically meaningful in the context of this study
# So changing negative relative growth rates to zero
alldata <- within(alldata, RGR_duringdrought[RGR_duringdrought<0] <- '0')
alldata <- within(alldata, RGR_overall[RGR_overall<0] <- '0')
alldata <- within(alldata, RGR_predrought[RGR_predrought<0] <- '0')
alldata$RGR_overall <- as.numeric(alldata$RGR_overall)
alldata$RGR_predrought <- as.numeric(alldata$RGR_predrought)
alldata$RGR_duringdrought <- as.numeric(alldata$RGR_duringdrought)

## Make a column for competition treatment - solo, intraspecific, conspecific
# maybe specifically number of neighbours too
# Composition maybe is already that but I need to make sure this reflects
#what was actually there
## ** do this

#### Transforming data to be normally distributed
alldata <- alldata %>% mutate(sqrt_RGR_overall = sqrt(RGR_overall))

### Pot 52 has two plants - AA - composition says AMYG-A, updating
alldata <- within(alldata, Composition[Pot_number==52] <- 'AA')
#Updated pot 33 to BBDD
alldata <- within(alldata, Composition[Pot_number==33] <- 'BBDD')
#pot 39 should be CD
alldata <- within(alldata, Composition[Pot_number==39] <- 'CD')
#Pot 84 should be just B
alldata <- within(alldata, Composition[Pot_number==84] <- 'OBLI-B')
#Pot 112 is all D
alldata <- within(alldata, Composition[Pot_number==112] <- 'DDDD')
#Pot 113 is all A
alldata <- within(alldata, Composition[Pot_number==113] <- 'AAAA')
#Pot 9 is all D
alldata <- within(alldata, Composition[Pot_number==9] <- 'DDDD')
#Pot 24 is all C
alldata <- within(alldata, Composition[Pot_number==24] <- 'CCCC')

### Pulling out control data only
controldata <- alldata %>% filter(C_or_D == 'C')
### Pulling out solo plants only
solodata <- alldata %>% filter(Composition =="AMYG-A" | Composition == "OBLI-B" | Composition =="OVAT-C" | Composition =="VIMI-D")

#Removing unknown Species *check this latre
alldata <- alldata %>% filter(!Species=='?')

### Create dataset for each species
amygdata <- alldata %>% filter(Species=="AMYG")
oblidata <- alldata %>% filter(Species=="OBLI")
ovatdata <- alldata %>% filter(Species=="OVAT")
vimidata <- alldata %>% filter(Species=="VIMI")

#Creating solodroughtdata for plotting because a control plant 100% died because
#of desiccation from fan
solodroughtdata <- solodata %>% filter(C_or_D=="D")
