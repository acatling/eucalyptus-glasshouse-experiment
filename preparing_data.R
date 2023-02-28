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
#and removing the culprit
alldata <- within(alldata, Composition[Pot_number == 2] <- 'ADDD')
alldata <- within(alldata, Composition[Pot_number == 19] <- 'ADDD')
#Pot 89 has one unusually small and dead plant, different composition, functionally CCC
#removing the dead plant
alldata <- within(alldata, Composition[Pot_number == 89] <- 'CCC')
alldata <- alldata %>% filter(!(Pot_number == 89 & Plant_number == 1)) 

#### Removing data for pots with at least one plant with noted desiccation from fan
#104_1_OVAT. OVAT-C
#41_4_AMYG. AADD
#71_2_AMYG. AACC
alldata <- alldata %>% filter(!(Pot_number==104))
alldata <- alldata %>% filter(!(Pot_number==41))
alldata <- alldata %>% filter(!(Pot_number==71))

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
#drought treatment sample sizes
#samplesizesdrought <- onerowpot %>% group_by(Composition, C_or_D) %>% tally()

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

### Calculate height growth rate
# Final height - initial height / time, in mm per day
#Final height: 21/10/2022. Initial height: 19/07. # 02/09 is the start of the drought treatment
#02/09 - 21/10 is 49 days.
#19/07 - 21/10 is 63 days
#Predrought is between 19/07 and 02/09. 45 days.
alldata$Height_2110 <- as.numeric(alldata$Height_2110)
#Check this NA*
alldata$Height_1907 <- as.numeric(alldata$Height_1907)
alldata <- alldata %>% group_by(plantid) %>% 
  mutate(gr_overall=(Height_2110-Height_1907)/63,
         gr_duringdrought=(Height_2110-Height_0209)/49,
         gr_predrought=(Height_0209-Height_1907)/45)

## Height reduction not biologically meaningful in the context of this study
# So changing negative height growth rates to zero
#BUT some issues with data entry for these plants, to resolve*
#Initial and final heights strange:
test <- alldata %>% filter(gr_overall<0)
#pot 8 and 47 and 102 - okay, variation in measuring height.
#pot 52 - fixed issue, one plant (2) didn't have initial height recorded.
#fixed order for 92 and 95, fixed 116-2 - typo from 21 to 210

alldata <- within(alldata, gr_duringdrought[gr_duringdrought<0] <- '0')
alldata <- within(alldata, gr_overall[gr_overall<0] <- '0')
alldata <- within(alldata, gr_predrought[gr_predrought<0] <- '0')
alldata$gr_overall <- as.numeric(alldata$gr_overall)
alldata$gr_predrought <- as.numeric(alldata$gr_predrought)
alldata$gr_duringdrought <- as.numeric(alldata$gr_duringdrought)

## Make a column for competition treatment - solo, intraspecific, conspecific
# maybe specifically number of neighbours too
# Composition maybe is already that but I need to make sure this reflects
#what was actually there
## ** do this

#### Transforming data to be normally distributed
alldata <- alldata %>% mutate(sqrt_gr_overall = sqrt(gr_overall))

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

#### Calculate mean and sd ttm for drought plants only ####
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
  summarise(mean_grpredrought = mean(gr_predrought, na.rm=T),
            sd_grpredrought = sd(gr_predrought, na.rm=T),
            n_gr = n())
#Merge ttm and growth rates
meandata <- left_join(growthmeans, ttmmeans)

#Calculating confidence intervals for growth and time to mortality
# Need means, sd and sample size
# CI is: average - error
# where error is: qnorm(0.75) * sd/sqrt(n)
#sd/sqrt(n) is the standard error. qnorm(0.75) = 1.96
meandata <- meandata %>% mutate(upper_ttm100 = (mean_tt100m + qnorm(0.75) * sd_tt100m/sqrt(n_ttm)),
                                lower_ttm100 = (mean_tt100m - qnorm(0.75) * sd_tt100m/sqrt(n_ttm)),
                                upper_ttm50 = (mean_tt50m + qnorm(0.75) * sd_tt50m/sqrt(n_ttm)),
                                lower_ttm50 = (mean_tt50m - qnorm(0.75) * sd_tt50m/sqrt(n_ttm)),
                                upper_gr = (mean_grpredrought + qnorm(0.75) * sd_grpredrought/sqrt(n_gr)),
                                lower_gr = (mean_grpredrought - qnorm(0.75) * sd_grpredrought/sqrt(n_gr)))
### TTM by composition ####
ttmcompfour <- alldata %>% filter(C_or_D=="D") %>% filter(Composition == "AMYG-A" | Composition == "OVAT-C" 
                                                      | Composition == "VIMI-D"
                                                      | Composition == "AAAA" | Composition == "CCCC"
                                                      | Composition ==  "DDDD" | Composition == "AACC"
                                                      | Composition == "AADD" | Composition == "CCDD") %>%
  group_by(Pot_number, Species) %>%
  summarise(potmean_tt100m = mean(tt100m, na.rm = TRUE),
            potmean_tt50m = mean(tt50m, na.rm = TRUE))
compositions <- as.data.frame(alldata) %>% select(Pot_number, Composition, C_or_D) %>%
  group_by(Pot_number) %>% filter(row_number() == 1)
#Join with compositions info
ttmcompfour <- left_join(ttmcompfour, compositions)  
#Rearrange
ttmcompfour <- ttmcompfour %>% select(Pot_number, Composition, Species, potmean_tt100m)
#Summarising means and sds
ttmcompfourmeans <- ttmcompfour %>% group_by(Composition, Species) %>% 
  summarise(mean_tt100m = mean(potmean_tt100m, na.rm=T),
            sd_tt100m = sd(potmean_tt100m, na.rm=T),
            n_tt100m = n())
#Calculate upper and lower confidence intervals
ttmcompfourmeans <- ttmcompfourmeans %>% mutate(upper_tt100m = (mean_tt100m + qnorm(0.75) * sd_tt100m/sqrt(n_tt100m)),
                                        lower_tt100m = (mean_tt100m - qnorm(0.75) * sd_tt100m/sqrt(n_tt100m)))
#Want AACC from the perspective of A and C so using unite to give these unique row name
ttmcompfourmeans <- ttmcompfourmeans %>% unite("compspecies", Composition:Species, remove = FALSE)
ttmcompfour <- ttmcompfour %>% unite("compspecies", Composition:Species, remove = FALSE)

#Alone and cons only
pot_ttm_cons <- ttmcompfour %>% filter(Composition == "AMYG-A" | Composition == "OVAT-C" 
                                              | Composition == "VIMI-D" | Composition == "AAAA"
                                              | Composition == "CCCC" | Composition == "DDDD")
mean_ttm_cons <- ttmcompfourmeans %>% filter(Composition == "AMYG-A" | Composition == "OVAT-C" 
                                         | Composition == "VIMI-D" | Composition == "AAAA"
                                         | Composition == "CCCC" | Composition == "DDDD")
#Same thing but for competition/hets pots only
pot_ttm_hets <- ttmcompfour %>% filter(Composition == "AACC" | Composition == "AADD" 
                                              | Composition == "CCDD")
mean_ttm_hets <- ttmcompfourmeans %>% filter(Composition == "AACC" | Composition == "AADD" 
                                         | Composition == "CCDD")

### Calculate growth rates for well-watered control plants alone, with cons or hets ####
wateredmeans <- alldata %>% filter(C_or_D=="C") %>% group_by(Species, Composition) %>% 
  summarise(mean_groverall = mean(gr_overall, na.rm=T),
            sd_groverall = sd(gr_overall, na.rm=T))
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
#Extracting data for gr overall for all pots, pre-drought
alone_four_avg_pot <- alone_four_avg_pot %>%
  select(Pot_number, Species, Composition, mean_pot_gr_predrought = gr_predrought)

#Summarising means and sds for plants pre-drought
compgrmeans <- alone_four_avg_pot %>% group_by(Composition, Species) %>% 
  summarise(mean_gr_comp = mean(mean_pot_gr_predrought, na.rm=T),
            sd_gr_comp = sd(mean_pot_gr_predrought, na.rm=T),
            n_gr_comp = n())
#Calculate upper and lower confidence intervals
compgrmeans <- compgrmeans %>% mutate(upper_gr_comp = (mean_gr_comp + qnorm(0.75) * sd_gr_comp/sqrt(n_gr_comp)),
                                lower_gr_comp = (mean_gr_comp - qnorm(0.75) * sd_gr_comp/sqrt(n_gr_comp)))
#Want AACC from the perspective of A and C so using unite to give these unique row name
compgrmeans <- compgrmeans %>% unite("compspecies", Composition:Species, remove = FALSE)
alone_four_avg_pot <- alone_four_avg_pot %>% unite("compspecies", Composition:Species, remove = FALSE)

#Alone and cons only
pot_gr_cons <- alone_four_avg_pot %>% filter(Composition == "AMYG-A" | Composition == "OVAT-C" 
                                          | Composition == "VIMI-D" | Composition == "AAAA"
                                          | Composition == "CCCC" | Composition == "DDDD")
comp_gr_cons <- compgrmeans %>% filter(Composition == "AMYG-A" | Composition == "OVAT-C" 
                                               | Composition == "VIMI-D" | Composition == "AAAA"
                                               | Composition == "CCCC" | Composition == "DDDD")
#Same thing but for competition/hets pots only
pot_gr_hets <- alone_four_avg_pot %>% filter(Composition == "AACC" | Composition == "AADD" 
                                              | Composition == "CCDD")
comp_gr_hets <- compgrmeans %>% filter(Composition == "AACC" | Composition == "AADD" 
                                              | Composition == "CCDD")
## Calculating pre-drought growth rates for OBLI, only 2 plants in cons and hets
obli_comp <- alldata %>% filter(Composition == "OBLI-B" | Composition == "BB" 
                                    | Composition == "AB" | Composition == "BC" | Composition == "BD")
obli_comp_avg <- obli_comp %>% group_by(Pot_number, Species) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
#Add in composition info
obli_comp_avg <- left_join(obli_comp_avg, compositions)  
obli_comp_avg <- obli_comp_avg %>%
  select(Pot_number, Species, Composition, mean_pot_gr_predrought = gr_predrought)
#summarise means
oblicompgrmeans <- obli_comp_avg %>% group_by(Composition, Species) %>% 
  summarise(mean_gr_comp = mean(mean_pot_gr_predrought, na.rm=T),
            sd_gr_comp = sd(mean_pot_gr_predrought, na.rm=T),
            n_gr_comp = n())
#Calculate upper and lower confidence intervals
oblicompgrmeans <- oblicompgrmeans %>% mutate(upper_gr_comp = (mean_gr_comp + qnorm(0.75) * sd_gr_comp/sqrt(n_gr_comp)),
                                        lower_gr_comp = (mean_gr_comp - qnorm(0.75) * sd_gr_comp/sqrt(n_gr_comp)))
#unite
oblicompgrmeans <- oblicompgrmeans %>% unite("compspecies", Composition:Species, remove = FALSE)
obli_comp_avg <- obli_comp_avg %>% unite("compspecies", Composition:Species, remove = FALSE)
# Stats for obli - want grouped alone, cons and hets
obli_comp_avg$grouped_comp <- 1
obli_comp_avg <- within(obli_comp_avg, grouped_comp[Composition == "OBLI-B"] <- 'alone')
obli_comp_avg <- within(obli_comp_avg, grouped_comp[Composition == "BB"] <- 'cons')
obli_comp_avg <- within(obli_comp_avg, grouped_comp[Composition != "OBLI-B" & Composition != "BB"] <- 'hets')
#Do it for obli_comp too, for models
obli_comp$grouped_comp <- 1
obli_comp <- within(obli_comp, grouped_comp[Composition == "OBLI-B"] <- 'alone')
obli_comp <- within(obli_comp, grouped_comp[Composition == "BB"] <- 'cons')
obli_comp <- within(obli_comp, grouped_comp[Composition != "OBLI-B" & Composition != "BB"] <- 'hets')

#### Import trait data ####
rawtraitspartone <- read_csv("Data/sp_interactions_exp/trait_data_sp_int_exp_part_one.csv")
rawtraitsparttwo <- read_csv("Data/sp_interactions_exp/trait_data_sp_int_exp_part_two.csv")
#all are control well-watered plants

#Join dataframes, adding two to one
traitdata <- left_join(rawtraitspartone, rawtraitsparttwo, by = c("Pot_number", "Composition"))

#pot 120 fresh_mass_FE was recorded in mg, others in grams, adjusting
traitdata <- within(traitdata, Fresh_mass_FE[Pot_number == "120" & Terminal_branch == "A"] <- 0.7214)
traitdata <- within(traitdata, Fresh_mass_FE[Pot_number == "120" & Terminal_branch == "B"] <- 0.5632)
traitdata <- within(traitdata, Fresh_mass_FE[Pot_number == "120" & Terminal_branch == "C"] <- 0.5512)

#Pot 84 is just OBLI-B
traitdata <- within(traitdata, Composition[Pot_number==84] <- 'OBLI-B')
#Pot 73 is just AMYG-A
traitdata <- within(traitdata, Composition[Pot_number==73] <- 'AMYG-A')
#Removing pot 104 becaus this plant had noted dessication from fan and is removed from other datasets
#had two TBs
traitdata <- traitdata %>% filter(!(Pot_number==104))

## Allocating species names
traitdata$Species <- 1
traitdata <- within(traitdata, Species[Composition == "AMYG-A"] <- 'AMYG')
traitdata <- within(traitdata, Species[Composition == "OBLI-B"] <- 'OBLI')
traitdata <- within(traitdata, Species[Composition == "OVAT-C"] <- 'OVAT')
traitdata <- within(traitdata, Species[Composition == "VIMI-D"] <- 'VIMI')

#Calculate LDMC as fresh mass fully expanded healthy leaves/dry mass from TBs
#sla #Al:As / Huber value # wd / wood density
#wood dry mass was measured in mg, need g/cm^3 #wood volume was measured as g
#wood density units should be kg/m^3, 400-600 makes sense
#fresh and dry mass in grams
#LMA units should be g/m^2
#SLA units should be m^2/kg
#think leaf area was recorded as mm squared
#ldmc is g per g
#Huber value units should be m^2 per m^2 (sapwood to leaf area), need to convert sapwood diameter to area
#sapwood diameter mm. area is in mm^2. Leaf area is cm^2
#to convert mm^2 per cm^2 to m^2 per m^2, divide by 100
#HV of 0.0002 is within normal range m^2/m^2
#LMA normal range 40-120 g/m^2
#SLA normal range 8-23 m^2/kg
traitdata <- traitdata %>% mutate(ldmc_g_g = Fresh_mass_FE/Dry_mass_FE,
                                  sla_cm2_g = Fully_exp_leaf_area/Dry_mass_FE,
                                  sla_m2_kg = sla_cm2_g/10,
                                  lma_g_cm2 = Dry_mass_FE/Fully_exp_leaf_area,
                                  lma_g_m2 = lma_g_cm2*10000,
                                  sapwood_area = pi*(Sapwood_diameter/2)^2,
                                  huber_mm2_cm2 = sapwood_area/Leaf_area,
                                  hv_m2_m2 = huber_mm2_cm2/100,
                                  Wood_dry_mass_g = Wood_dry_mass/1000,
                                  wd_g_cm3 = Wood_dry_mass_g/Wood_volume,
                                  wd_kg_m3 = wd_g_cm3*1000)

#Calculate average huber per plant
mean_traits_plant <- traitdata %>% group_by(Pot_number) %>% 
  summarise(mean_huber_plant = mean(hv_m2_m2, na.rm=T),
            sd_huber = sd(hv_m2_m2, na.rm=T),
            mean_ldmc_plant = mean(ldmc_g_g, na.rm=T),
            sd_ldmc = sd(ldmc_g_g, na.rm=T),
            mean_sla_plant = mean(sla_m2_kg, na.rm=T),
            sd_sla = sd(sla_m2_kg, na.rm=T),
            mean_lma_plant = mean(lma_g_m2, na.rm=T),
            sd_lma = sd(lma_g_m2, na.rm=T))
#Merge huber back in
traitdata <- left_join(traitdata, mean_traits_plant)

sometraits <- traitdata %>% select(Pot_number, Composition, Species, mean_ldmc_plant, mean_sla_plant, mean_lma_plant, mean_huber_plant, wd_kg_m3) %>%
  group_by(Pot_number) %>% filter(row_number() == 1)

### Add plant-level data to solo well-watered and drought data
solowatereddata <- left_join(solowatereddata, sometraits)

### Add mortality data to solowatereddata
tt100m_means <- ttmmeans %>% select(Species, mean_tt100m)
solowatereddata <- left_join(solowatereddata, tt100m_means)

#Calculate species means and confidence intervals
speciestraits <- sometraits %>% group_by(Species) %>% 
  summarise(mean_huber_sp = mean(mean_huber_plant, na.rm=T),
            sd_huber_sp = sd(mean_huber_plant, na.rm=T),
            mean_ldmc_sp = mean(mean_ldmc_plant, na.rm=T),
            sd_ldmc_sp = sd(mean_ldmc_plant, na.rm=T),
            mean_sla_sp = mean(mean_sla_plant, na.rm=T),
            sd_sla_sp = sd(mean_sla_plant, na.rm=T),
            mean_lma_sp = mean(mean_lma_plant, na.rm=T),
            sd_lma_sp = sd(mean_lma_plant, na.rm=T),
            mean_wd_sp = mean(wd_kg_m3, na.rm=T),
            sd_wd_sp = sd(wd_kg_m3, na.rm=T),
            n = n())
            
###Merge speciestraits with ttmmeans 
speciestraits <- left_join(speciestraits, ttmmeans)
###Merge individual wd with ttmmeans
wddata <- speciestraits %>% select(Species, mean_wd_sp)
solodroughtdata <- left_join(solodroughtdata, wddata)

#Calculate CIs
speciestraits <- speciestraits %>% group_by(Species) %>% mutate(upper_huber = (mean_huber_sp + qnorm(0.75) * sd_huber_sp/sqrt(n)),
                                lower_huber = (mean_huber_sp - qnorm(0.75) * sd_huber_sp/sqrt(n)),
                                upper_sla = (mean_sla_sp + qnorm(0.75) * sd_sla_sp/sqrt(n)),
                                lower_sla = (mean_sla_sp - qnorm(0.75) * sd_sla_sp/sqrt(n)),
                                upper_lma = (mean_lma_sp + qnorm(0.75) * sd_lma_sp/sqrt(n)),
                                lower_lma = (mean_lma_sp - qnorm(0.75) * sd_lma_sp/sqrt(n)),
                                upper_wd = (mean_wd_sp + qnorm(0.75) * sd_wd_sp/sqrt(n)),
                                lower_wd = (mean_wd_sp - qnorm(0.75) * sd_wd_sp/sqrt(n)))

#Merge with species gr and mort data in meandata
meandata <- left_join(meandata, speciestraits)

## pots with neighbours only
nbhdata <- alldata %>% filter(!(Composition =="AMYG-A" | Composition == "OBLI-B" | Composition =="OVAT-C" | Composition =="VIMI-D"))
nbhdroughtdata <- nbhdata %>% filter(C_or_D == "D")



