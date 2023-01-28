####### Eucalyptus Species Interactions Drought Experiment
## Species interactions experiment 2021
#Eucalyptus amygdalina, Eucalyptus obliqua, Eucalyptus ovata and Eucalyptus viminalis

### Importing packages and data ####
library(tidyverse)
library(ggplot2)

heightdataraw <- read_csv("Data/sp_interactions_exp/height_data_2021.csv")
#height data for each individual plant in each pot
mortalitydataraw <- read_csv("Data/sp_interactions_exp/mortality_data_2021.csv")
### soil moisture data
moisturedataraw <- read_csv("Data/sp_interactions_exp/soil_moisture2.csv")

#Why difference in number of rows?
# The difference is the mystery 5th plant in pot 3 AACC which is in height data but not mort data
# The order is correct for all of these but we only have height data for one of the As
# on the last date (no initial height, was likely very small).
#removing it from both
heightdata <- heightdataraw
heightdata <- heightdata %>% filter(!(Pot_number == 3))
mortalitydata <- mortalitydataraw
mortalitydata <- mortalitydata %>% filter(!(Pot_number == 3))

#Select relevant height data to merge
heightdata <- heightdata %>% select(Pot_number, Plant_number, Height_1907, Notes_1907,
                                 Height_0209, Notes_0209, Height_0909,
                                 Height_2110, Notes_2110)

#Merging height and mortality data
#Merging by pot number and plant number.... different composition info is the 
#problem, resulting in height NAs atm
alldata <- left_join(mortalitydata, heightdata)

#### Naming Species, making sure species ID is correct ####
alldata$Species <- alldata$Species_where_mixed
alldata <- within(alldata, Species[is.na(Species) & Composition == "AMYG-A"] <- 'A')
alldata <- within(alldata, Species[is.na(Species) & Composition == "OBLI-B"] <- 'B')
alldata <- within(alldata, Species[is.na(Species) & Composition == "OVAT-C"] <- 'C')
alldata <- within(alldata, Species[is.na(Species) & Composition == "VIMI-D"] <- 'D')
alldata <- within(alldata, Species[is.na(Species) & Composition == "AAAA"] <- 'A')
alldata <- within(alldata, Species[is.na(Species) & Composition == "AAA"] <- 'A')
alldata <- within(alldata, Species[is.na(Species) & Composition == "BBBB"] <- 'B')
alldata <- within(alldata, Species[is.na(Species) & Composition == "BB"] <- 'B')
alldata <- within(alldata, Species[is.na(Species) & Composition == "CCCC"] <- 'C')
alldata <- within(alldata, Species[is.na(Species) & Composition == "DDDD"] <- 'D')
alldata <- within(alldata, Species[is.na(Species) & Composition == "DD"] <- 'D')

alldata <- within(alldata, Species[Species == "A"] <- "AMYG")
alldata <- within(alldata, Species[Species == "B"] <- "OBLI")
alldata <- within(alldata, Species[Species == "C"] <- "OVAT")
alldata <- within(alldata, Species[Species == "D"] <- "VIMI")

#### Adjusting plant IDs and fixing pot specific issues ####

#Pot 84AB, two rows but only one plant so removing row with nothing
alldata <- alldata %>% filter(!(is.na(Species) & Pot_number==84))
### Removing rows with no plants, 5 of them
#This doesn't work because it only filters to things with notes (except Nothing there)!
#test <- alldata %>% filter(!(Notes == "Nothing there"))
toremove <- alldata %>% filter(Notes == "Nothing there")
alldata <- anti_join(alldata, toremove)
#Removing 39 CCDD repeated rows (two of them)
alldata <- alldata %>% filter(!(is.na(Species)))

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

#Pot 100 says Species AMYG but composition VIMI-D, checked notes and believe
#this is a mistake, incorrectly entered A meant for line above. Adjusting
alldata <- within(alldata, Species[Pot_number == 100] <- 'VIMI')

#Pots 2 and 19 are actually DDDA with very unhealthy As (buried in beads or tiny)
alldata <- within(alldata, Composition[Pot_number == 2] <- 'ADDD')
alldata <- within(alldata, Composition[Pot_number == 19] <- 'ADDD')

#### Removing data for pots with at least one plant with noted desiccation from fan
#104_1_OVAT. OVAT-C
#41_4_AMYG. AADD
alldata <- alldata %>% filter(!(Pot_number==104))
alldata <- alldata %>% filter(!(Pot_number==41))

### Two pots with unknown species, removing them (15 AACC and 27 BC)
alldata <- alldata %>% filter(!Species=='?')

#### Making plant-within-pot ID, keeping species to be informative ####
#Reordering for unite function
alldata <- alldata %>% select(Pot_number, Plant_number, Species, Composition, Notes, Percent_death_0209,
                              Percent_death_0909, Percent_death_1609, Percent_death_2309, Percent_death_3009,
                              Percent_death_0710, Percent_death_1410, Percent_death_2110, Height_1907,
                              Notes_1907, Height_0209, Notes_0209, Height_0909, Height_2110, Notes_2110)
alldata <- alldata %>% unite("plantid", Pot_number:Plant_number:Species, remove = FALSE)

#### Calculating sample sizes ####
onerowpot <- alldata %>% group_by(Pot_number) %>% filter(row_number()==1)
samplesizestable <- onerowpot %>% group_by(Composition) %>% tally()

#### Merging with watering treatment data ####
#Entering sample size data for C_or_D pot allocations
samplesizedata <- read_csv("Data/sp_interactions_exp/sample_sizes.csv")
treatmentdata <- samplesizedata %>% select(Pot_number, C_or_D)
alldata <- left_join(alldata, treatmentdata)

#### Making data long for Height and survival by Date ####
#Splitting height and mortality to pivot separately then merge
heightdata2 <- alldata %>% select(plantid, Pot_number, Plant_number, Species, Composition, Height_1907,
                                 Height_0209, Height_0909, Height_2110)
heightdata2$Height_0909 <- as.numeric(heightdata2$Height_0909)
heightdata2$Height_2110 <- as.numeric(heightdata2$Height_2110)
#89-1 says 'Dead' instead of height, this line makes that an NA. Height 2110 same plant says 10?
heightlong <- heightdata2 %>% pivot_longer(cols = starts_with('Height_'), names_to = "date", values_to = "height")
#Removing Height_ from date column and converting it into a date
heightlong$date <- gsub("Height_", "", heightlong$date)
heightlong$year <- '2021'
#Rearranging
heightlong <- heightlong %>% select(plantid, Pot_number, Plant_number, Species, Composition, height, date, year)
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
mortalitydata2 <- alldata %>% select(plantid, Pot_number, Plant_number, Species, Composition, C_or_D, Notes, Percent_death_0209,
                                    Percent_death_0909, Percent_death_1609, Percent_death_2309, Percent_death_3009, 
                                    Percent_death_0710, Percent_death_1410, Percent_death_2110)
mortalitylong <- mortalitydata2 %>% pivot_longer(cols = starts_with('Percent_death_'), names_to = "date", values_to = "percent_mortality")
mortalitylong$date <- gsub("Percent_death_", "", mortalitylong$date)
mortalitylong$year <- '2021'
mortalitylong <- mortalitylong %>% select(plantid, Pot_number, Plant_number, Species, Composition, C_or_D, Notes, percent_mortality, date, year)
mortalitylong <- mortalitylong %>% unite("date", date:year, sep = "")
mortalitylong$date <- as.Date(mortalitylong$date, "%d%m%Y")

### Merging all the long dataframes together
alldatalong <- left_join(mortalitylong, heightlong)
alldatalong <- left_join(alldatalong, heightnoteslong)

#### Create a column for time/week since onset of drought treatment ####
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
# Final height - initial height / time, in mm per day
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
#BUT some issues with data entry for these plants, to resolve*
#Initial and final heights strange:
#test <- alldata %>% filter(RGR_overall<0)

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

### Pulling out control data only
controldata <- alldata %>% filter(C_or_D == 'C')
### Pulling out solo plants only
solodata <- alldata %>% filter(Composition =="AMYG-A" | Composition == "OBLI-B" | Composition =="OVAT-C" | Composition =="VIMI-D")

### Create dataset for each species
amygdata <- alldata %>% filter(Species=="AMYG")
oblidata <- alldata %>% filter(Species=="OBLI")
ovatdata <- alldata %>% filter(Species=="OVAT")
vimidata <- alldata %>% filter(Species=="VIMI")

#Creating solodroughtdata for plotting because a control plant 100% died because
#of desiccation from fan
solodroughtdata <- solodata %>% filter(C_or_D=="D")
solowatereddata <- solodata %>% filter(C_or_D=="C")

#### Calculate mean and sd ttm for D plants only ####
#same sample sizes for ttm100 and ttm50
ttmmeans <- solodata %>% filter(C_or_D == "D") %>% group_by(Species) %>% 
  summarise(mean_tt100m = mean(tt100m, na.rm=T),
            sd_tt100m = sd(tt100m, na.rm=T),
            mean_tt50m = mean(tt50m, na.rm=T),
            sd_tt50m = sd(tt50m, na.rm=T),
            n_ttm = n())
#Making sd 0 for obliqua tt100m and 50m
ttmmeans <- within(ttmmeans, sd_tt100m[Species=="OBLI"] <- 0)
ttmmeans <- within(ttmmeans, sd_tt50m[Species=="OBLI"] <- 0)
#Calculate mean and sd growth rates, this is predrought so C and D plants
growthmeans <- solodata %>% group_by(Species) %>% 
  summarise(mean_RGRpredrought = mean(RGR_predrought, na.rm=T),
            sd_RGRpredrought = sd(RGR_predrought, na.rm=T),
            n_rgr = n())
#Merge ttm and growth rates
meandata <- left_join(growthmeans, ttmmeans)

#Calculating confidence intervals for growth and time to mortality
# Need means, sd and sample size
# CI is: average - error
# where error is: qnorm(0.75) * sd/sqrt(n)
#sd/sqrt(n) is the standard error. qnorm(0.75) = 1.96
meandata <- meandata %>% mutate(upper_ttm100 = (mean_tt100m + qnorm(0.75) * sd_tt100m/sqrt(n_ttm)),
                                lower_ttm100 = (mean_tt100m - qnorm(0.75) * sd_tt100m/sqrt(n_ttm)),
                                upper_rgr = (mean_RGRpredrought + qnorm(0.75) * sd_RGRpredrought/sqrt(n_rgr)),
                                lower_rgr = (mean_RGRpredrought - qnorm(0.75) * sd_RGRpredrought/sqrt(n_rgr)))
### Calculate growth rates for well-watered control plants alone, with cons or hets ####
wateredmeans <- alldata %>% filter(C_or_D=="C") %>% group_by(Species, Composition) %>% 
  summarise(mean_RGRoverall = mean(RGR_overall, na.rm=T),
            sd_RGRoverall = sd(RGR_overall, na.rm=T))
# Want to avoid pseudo-replication of different individuals within same pot, i.e.
#avoid growth of A1 and A2 in AACC pot.
# So first need to summarise A_avg and C_avg for AACC pot.
#make plant number a factor first
alldata$Plant_number <- as.factor(alldata$Plant_number)
#Height_0909 is not numeric because of word dead
alldata <- alldata %>% mutate(Height_0909_numeric = Height_0909)
alldata$Height_0909_numeric <- as.numeric(alldata$Height_0909_numeric)
#Filtering out unusual compositions first so that I know this is only 
#plants alone or with four in the pot
#do this * there is one AAAA that just says check IDs, values currently NAs so fine
alone_or_four <- alldata %>% filter(Composition == "AMYG-A" | Composition == "OVAT-C" 
                                    | Composition == "VIMI-D"
                                    | Composition == "AAAA" | Composition == "CCCC"
                                      | Composition ==  "DDDD" | Composition == "AACC"
                                      | Composition == "AADD" | Composition == "CCDD")
#average for a species within a pot
alone_four_avg_pot <- alone_or_four %>% group_by(Pot_number, Species) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
#summarise_all will return NAs for everything that isn't numeric - this is okay
#Gives the same output:
# alone_four_avg_pot2 <- alone_or_four %>% group_by(Pot_number, Species) %>%
#   summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
compositions <- as.data.frame(alldata) %>% select(Pot_number, Composition, C_or_D) %>%
  group_by(Pot_number) %>% filter(row_number() == 1)
#Add in composition info
alone_four_avg_pot <- left_join(alone_four_avg_pot, compositions)  
#Extracting data for RGR overall for all pots, pre-drought
alone_four_avg_pot <- alone_four_avg_pot %>%
  select(Pot_number, Species, Composition, mean_pot_rgr_predrought = RGR_predrought)
#Need long format with cons or hets as a column*

#Summarising means and sds for plants pre-drought
comprgrmeans <- alone_four_avg_pot %>% group_by(Composition, Species) %>% 
  summarise(mean_rgr_comp = mean(mean_pot_rgr_predrought, na.rm=T),
            sd_rgr_comp = sd(mean_pot_rgr_predrought, na.rm=T),
            n_rgr_comp = n())
#Calculate upper and lower confidence intervals
comprgrmeans <- comprgrmeans %>% mutate(upper_rgr_comp = (mean_rgr_comp + qnorm(0.75) * sd_rgr_comp/sqrt(n_rgr_comp)),
                                lower_rgr_comp = (mean_rgr_comp - qnorm(0.75) * sd_rgr_comp/sqrt(n_rgr_comp)))
#Want AACC from the perspective of A and C so using unite to give these unique row name
comprgrmeans <- comprgrmeans %>% unite("compspecies", Composition:Species, remove = FALSE)
alone_four_avg_pot <- alone_four_avg_pot %>% unite("compspecies", Composition:Species, remove = FALSE)

#Alone and cons only
pot_rgr_cons <- alone_four_avg_pot %>% filter(Composition == "AMYG-A" | Composition == "OVAT-C" 
                                          | Composition == "VIMI-D" | Composition == "AAAA"
                                          | Composition == "CCCC" | Composition == "DDDD")
comp_rgr_cons <- comprgrmeans %>% filter(Composition == "AMYG-A" | Composition == "OVAT-C" 
                                               | Composition == "VIMI-D" | Composition == "AAAA"
                                               | Composition == "CCCC" | Composition == "DDDD")
#Same thing but for competition/hets pots only
pot_rgr_hets <- alone_four_avg_pot %>% filter(Composition == "AACC" | Composition == "AADD" 
                                              | Composition == "CCDD")
comp_rgr_hets <- comprgrmeans %>% filter(Composition == "AACC" | Composition == "AADD" 
                                              | Composition == "CCDD")

