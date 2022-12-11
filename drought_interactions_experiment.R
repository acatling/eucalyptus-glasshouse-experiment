####### Eucalyptus Species Interactions Drought Experiment
## Species interactions experiment 2021
#Eucalyptus amygdalina, Eucalyptus obliqua, Eucalyptus ovata and Eucalyptus viminalis

###Load in data and packages
source("preparing_data.R")
library(kableExtra)

#### Plotting distributions of continuous variables ####
hist(alldata$Height_2110)
hist(alldata$Height_1907)
hist(alldata$RGR_predrought)
hist(alldata$RGR_duringdrought)
hist(sqrt(alldata$RGR_overall))
hist(sqrt(controldata$RGR_overall))

#### Plotting basic trends ####
## Mortality ####
#Setting a seed makes sure the line matches the jittered point #seed =123. In theory....
alldatalong %>% filter(!(Species == '?')) %>%
  ggplot(aes(x = date, y = percent_mortality, colour = plantid))+
  geom_point(alpha=0.3, position = "jitter")+
  # stat_smooth(geom="line", alpha = 0.3)+
  #geom_smooth(formula = y ~ poly(x), alpha=0.3, se = FALSE)+
  geom_line(stat="smooth", alpha = 0.3, position = "jitter")+
  theme_classic()+
  ylim(-15,115)+
  theme(legend.position = 'none')+
  facet_wrap(~Species)

## Coloured by watering treatment
alldatalong %>% filter(!(Species == '?')) %>%
  ggplot(aes(x = date, y = percent_mortality, colour = C_or_D))+
  geom_point(alpha=0.3, position = "jitter")+
  # stat_smooth(geom="line", alpha = 0.3)+
  #geom_smooth(formula = y ~ poly(x), alpha=0.3, se = FALSE)+
  geom_line(stat="smooth", alpha = 0.3, position = "jitter")+
  theme_classic()+
  ylim(-15,115)+
  facet_wrap(~Species)

## Growth rate ####
alldata %>% filter(!(Species == '?')) %>%
  ggplot(aes(x = Species, y = RGR_duringdrought, colour = C_or_D))+
  geom_boxplot()+
  geom_jitter()+
  theme_classic()
#14 days
alldata %>% filter(!(Species == '?')) %>%
  ggplot(aes(x = Species, y = RGR_predrought))+
  geom_boxplot()+
  geom_jitter()+
  theme_classic()
#14 days
alldata %>% filter(!(Species == '?')) %>%
  ggplot(aes(x = Species, y = RGR_overall))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4)+
  theme_classic()
#RGR overall for well-watered plants only
alldata %>% filter(!(Species == '?')) %>% filter(C_or_D == 'C') %>%
  ggplot(aes(x = Species, y = RGR_overall))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4)+
  theme_classic()
#For solo plants only
  ggplot(solodata, aes(x = Species, y = sqrt(RGR_overall)))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4)+
  theme_classic()
#For solo well-watered plants only -- too few 
solodata %>% filter(C_or_D == "C") %>%
  ggplot(aes(x = Species, y = sqrt(RGR_overall)))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4, width=0.05)+
  theme_classic()
#Solo plants before drought? May be best
ggplot(solodata, aes(x = Species, y = sqrt(RGR_predrought)))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4, width=0.05)+
  theme_classic()
#Has the same trend as solo well-watered plant, so I think solo plants before
#drought makes sense

#One-way ANOVA to see if there are differences in growth rate between species
modelgr1 <- aov(sqrt(RGR_predrought) ~ Species, solodata)
summary(modelgr1)
#No
TukeyHSD(modelgr1)

## Differences between species in time to mortality
alldata %>% filter(!Species=='?') %>%
ggplot(aes(x = Species, y = tt100m))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4)+
  theme_classic()

alldata %>% filter(Species=='AMYG') %>% summarise(mean=mean(tt100m))

alldata %>% filter(!Species=='?') %>%
  ggplot(aes(x = Species, y = tt50m))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4, height=0.05)+
  theme_classic()

hist(alldata$tt100m)
hist(sqrt(alldata$tt100m))
hist(alldata$tt50m)
hist(sqrt(alldata$tt50m))

### Heights between drought and control at start of drought
alldata %>% filter(!(Species == '?')) %>%
  ggplot(aes(x = C_or_D, y = Height_0209))+
  geom_boxplot()+
  geom_jitter()+
  theme_classic()+
  facet_wrap(~Species)
# and at the end #surprising!!! -- the drought didn't reduce growth?
#but it definitely did, from RGR_duringdrought
#so it reduced growth but not enough to change their final heights before they died?
#did competition reduce growth then?
alldata %>% filter(!(Species == '?')) %>%
  ggplot(aes(x = C_or_D, y = Height_2110))+
  geom_boxplot()+
  geom_jitter()+
  theme_classic()+
  facet_wrap(~Species)
#Height through time, by control and drought
alldatalong %>% filter(!(Species == '?')) %>%
  ggplot(aes(x = date, y = height, colour = C_or_D))+
  geom_jitter(alpha=0.3, width = 0.7)+
  #geom_boxplot()+
  theme_classic()+
  facet_wrap(~Species)

ggplot(alldata, aes(x = Composition, y = tt100m, colour=Species))+
  geom_jitter(alpha=0.3, width=0.05)+
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90))

ggplot(alldata, aes(x = Composition, y = tt100m, colour=Species))+
  geom_jitter(alpha=0.3, width=0.05)+
  geom_point(stat="summary", fun.y="mean_se",size=2) +
  geom_errorbar(stat="summary", fun.data="mean_se",width=0,size=0.8)+
  theme_classic()
#drought plants only - pretty much the same
alldata %>% filter(C_or_D=="D") %>%
  ggplot(aes(x = Composition, y = tt100m, colour=Species))+
  geom_jitter(alpha=0.3, width=0.05)+
  geom_point(stat="summary", fun.y="mean_se",size=2) +
  geom_errorbar(stat="summary", fun.data="mean_se",width=0,size=0.8)+
  theme_classic() 
#solo plants only - obli only has two solo plants under drought
ggplot(solodata, aes(x = Composition, y = tt100m))+
  geom_jitter(alpha=0.3, width=0.05)+
  geom_point(stat="summary", fun.y="mean_se",size=2) +
  geom_errorbar(stat="summary", fun.data="mean_se",width=0,size=0.8)+
  theme_classic()
#tt50m
ggplot(alldata, aes(x = Composition, y = tt50m, colour=Species))+
  geom_jitter(alpha=0.3, width=0.05)+
  geom_point(stat="summary", fun.y="mean_se",size=2) +
  geom_errorbar(stat="summary", fun.data="mean_se",width=0,size=0.8)+
  theme_classic()

#growth rate
ggplot(solodata, aes(x = Species, y = RGR_predrought))+
  geom_jitter(alpha=0.2, width=0.05)+
  geom_point(stat="summary", fun.y="mean_se",size=2) +
  geom_errorbar(stat="summary", fun.data="mean_se",width=0,size=0.8)+
  ylab("RGR pre-drought (mm/day)")+
  theme_classic()

#Check this pot - is it amyg or vimi? 100 
test <- alldata %>% filter(Species=="AMYG" & Composition == "VIMI-D")

#### Table of sample sizes ####
site_table %>%
  kbl(caption = "<b>Table 1</b>. Number of pots in each species composition. A, B, C and D respectively refer to
  one individual of E. amygdalina, E. obliqua, E. ovata and E. viminalis. AMYG-A, OBLI-B, OVAT-C and VIMI-D 
  represent a single plant of the respective species.", 
      digits = 0, align = "lccccc") %>%
  kable_classic(full_width = T, html_font = "Times", font_size = 16) %>%
  row_spec(0, italic = T) %>%
  add_header_above(c(" " = 2, "Number of focal trees" = 4))

### Plotting soil moisture data ####

## Models:
#by species:
# RGR/ttm ~ C_or_D + comp_treatment + (1|Pot_number)

## Look at size too?

#### Layout script ####
#Want to randomly assign numbers to treatments for layout
#Importing dataframe with designs listed
# layoutdesign <-read_csv("Data/sp_int_GH_exp_labels.csv")
# head(layoutdesign)
# layoutdesign$Pot_ID <- sample(1:140, nrow(layoutdesign))
# write_csv(layoutdesign,"Output/layoutdesign.csv")
#Note that I need to use the Excel button 'text to columns' with commas selected to separate
# data into correct columns - under Data tab

### from layout_GH_2021 script, merged
#Randomising layout of pots in glasshouse for 2021 competition eucalypt experiment
#10 pots of each composition (where possible)
#A = E. amygdalina
#B = E. obliqua
#C = E. ovata
#D = E. viminalis
#Examples: AMYG-A indicates one plant only of E. amygdalina
#AAAA indicates four plants of E. amygdalina
#BC indicates one plant each of E. obliqua and E. ovata

#This makes a list of all of my desired pot compositions
# Groups <- c("AMYG-A", "OBLI-B", "OVAT-C", "VIMI-D",
#             "AMYG-A", "OBLI-B", "OVAT-C", "VIMI-D",
#             "AMYG-A", "OBLI-B", "OVAT-C", "VIMI-D",
#             "AMYG-A", "OBLI-B", "OVAT-C", "VIMI-D",
#             "AMYG-A", "OBLI-B", "OVAT-C", "VIMI-D",
#             "AMYG-A", "OVAT-C", "VIMI-D",
#             "AMYG-A", "OVAT-C", "VIMI-D",
#             "AMYG-A", "OVAT-C", "VIMI-D",
#             "AMYG-A", "OVAT-C", "VIMI-D",
#             "AMYG-A", "OVAT-C", "VIMI-D",
#             "AAAA", "AAAA", "AAAA", "AAAA", "AAAA", "AAAA", "AAAA", "AAAA", "AAAA", "AAAA",
#             "BB", "BB", "BB", "BB", "BB",
#             "CCCC", "CCCC", "CCCC", "CCCC", "CCCC", "CCCC", "CCCC", "CCCC", "CCCC", "CCCC",
#             "DDDD", "DDDD", "DDDD", "DDDD", "DDDD", "DDDD", "DDDD", "DDDD", "DDDD", "DDDD",
#             "AB", "AB", "AB",
#             "AACC", "AACC", "AACC", "AACC", "AACC", "AACC", "AACC", "AACC", "AACC", "AACC",
#             "AADD", "AADD", "AADD", "AADD", "AADD", "AADD", "AADD", "AADD", "AADD", "AADD",
#             "BC", "BC", "BC",
#             "BD", "BD", "BD", "BD",
#             "CCDD", "CCDD", "CCDD", "CCDD", "CCDD", "CCDD", "CCDD", "CCDD", "CCDD", "CCDD")
# #This sets a seed of '2021' so that the randomisation can be replicated
# #I think my seed was actually different, my labels are different anyway!
# set.seed(2021)
# #This randomises the list by randomly sampling it without replacement and recording the order in a new list/vector
# RandomisedVector <- sample(Groups, replace = FALSE)
# #This puts the randomised list into a matrix with one column, so that I can export it
# MatrixVector <- matrix(RandomisedVector, ncol = 1)
# #This exports the randomised list into a csv file
# write.csv(MatrixVector, file = "Output/Euc_GH_layout_2021.csv")




