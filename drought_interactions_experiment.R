####### Eucalyptus Species Interactions Drought Experiment
## Species interactions experiment 2021
#Eucalyptus amygdalina, Eucalyptus obliqua, Eucalyptus ovata and Eucalyptus viminalis

### Load in data and packages ####
source("preparing_data.R")
library(kableExtra)
library(cowplot)
library(lme4)
library(lmerTest)
library(DHARMa)
library(MuMIn)
##Colours I like
#cornflowerblue
#thistle

#### Plotting distributions of continuous variables ####
hist(alldata$Height_2110)
hist(alldata$Height_1907)
hist(alldata$RGR_predrought)
hist(alldata$RGR_duringdrought)
hist(sqrt(alldata$RGR_overall))
hist(sqrt(controldata$RGR_overall))

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

## Stats - mortality model ####
hist(solodroughtdata$tt100m)
hist(solodroughtdata$tt50m)

mort50mod <- lm(tt50m ~ Species, solodroughtdata)
summary(mort50mod)
mortdharm <- simulateResiduals(mort50mod)
plot(mortdharm)
r.squaredGLMM(mort50mod)

mort100mod <- lm(tt100m ~ Species, solodroughtdata)
summary(mort100mod)
mortdharm <- simulateResiduals(mort100mod)
plot(mortdharm)
r.squaredGLMM(mort100mod)

solodroughtdata$Species <- factor(solodroughtdata$Species, levels = c("VIMI", "AMYG", "OBLI", "OVAT"))
solodroughtdata$Species <- factor(solodroughtdata$Species, levels = c("AMYG", "OBLI", "OVAT", "VIMI"))


## Growth rate ####
solodata %>% filter(!(Species == '?')) %>%
  ggplot(aes(x = Species, y = sqrt(RGR_duringdrought), colour = C_or_D))+
  geom_boxplot()+
  geom_jitter(width=0.05, height=0.05)+
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

## Stats - growth rate model ####
#Two weeks before drought
growthmod <- lm(sqrt(RGR_predrought) ~ Species, solodata)
summary(growthmod)
growthdharm <- simulateResiduals(growthmod)
plot(growthdharm)
r.squaredGLMM(growthmod)
solodata$Species <- factor(solodata$Species, levels = c("AMYG", "OBLI", "OVAT", "VIMI"))

ggplot(solodata, aes(x = Species, y = sqrt(RGR_predrought)))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4, width=0.05)+
  theme_classic()
#Over the drought period (seven weeks)
growthmod2 <- lm(sqrt(RGR_duringdrought) ~ Species + C_or_D, solodata)
summary(growthmod2)
growthdharm2 <- simulateResiduals(growthmod2)
plot(growthdharm2)
r.squaredGLMM(growthmod2)

growthmod2 <- lmer(sqrt(RGR_duringdrought) ~ C_or_D + (1|Species), solodata)
summary(growthmod2)
growthdharm2 <- simulateResiduals(growthmod2)
plot(growthdharm2)
r.squaredGLMM(growthmod2)

growthmod3 <- lm(sqrt(RGR_duringdrought) ~ Species, solowatereddata)
summary(growthmod3)
growthdharm2 <- simulateResiduals(growthmod2)
plot(growthdharm2)
r.squaredGLMM(growthmod2)

#units mm/day
solodata %>% filter(!(Species == '?')) %>%
  ggplot(aes(x = Species, y = sqrt(RGR_duringdrought), colour = C_or_D))+
  geom_boxplot()+
  geom_jitter(width=0.05, height=0.05)+
  theme_classic()
## Stats - trade-off between drought tolerance and growth rate ####
#regression
tradeoff <- lm(tt100m ~ sqrt(RGR_predrought) + Species, solodroughtdata)
summary(tradeoff)
growthdharm <- simulateResiduals(growthmod)
plot(growthdharm)
r.squaredGLMM(growthmod)

#### Table of sample sizes ####

#Where are 5 NA compositions coming from? In samplesizeswatering
#samplesizes table is composition from alldata
#Just made one manually
# site_table %>%
#   kbl(caption = "<b>Table 1</b>. Number of pots in each species composition. A, B, C and D respectively refer to
#   one individual of E. amygdalina, E. obliqua, E. ovata and E. viminalis. AMYG-A, OBLI-B, OVAT-C and VIMI-D 
#   represent a single plant of the respective species.", 
#       digits = 0, align = "lccccc") %>%
#   kable_classic(full_width = T, html_font = "Times", font_size = 16) %>%
#   row_spec(0, italic = T) %>%
#   add_header_above(c(" " = 2, "Number of focal trees" = 4))

### Plotting soil moisture data ####
moisturedata <- moisturedataraw %>% select(date_time2, probe_1, probe_2, probe_3, probe_4)
#Need data to be in long format
#Date, Probe, C_or_D, value
#str(moisturelong$Date)
moisturelong <- moisturedata %>% pivot_longer(cols = starts_with('probe_'), names_to = "probe", values_to = "value")
#Specify date/time format for Date column
moisturelong$date_time2 <- as.POSIXct(moisturelong$date_time2,format="%m/%d/%y %I:%M:%S %p",tz=Sys.timezone())
#Assign probe numbers to drought or control
moisturelong$D_or_C_probe <- "D"
moisturelong <- within(moisturelong, D_or_C_probe[probe=="probe_3"] <- "C")
moisturelong <- within(moisturelong, D_or_C_probe[probe=="probe_4"] <- "C")

#Setting range limits to cut off start and end where I set them up
lims <- as.POSIXct(strptime(c("2021-09-02 17:05:51","2021-10-21 09:05:51"), format = "%Y-%m-%d %H:%M:%S"))

group.colours <- c(probe_1 = "grey60", probe_2 = "grey80", probe_3 ="slateblue1", probe_4 = "lightblue")

#Soil moisture through time, two control and two drought
dev.off()
pdf("Output/soil_moisture.pdf", width=8, height=5)
ggplot(moisturelong, aes(x = date_time2, y = value, colour = probe))+
  geom_point(alpha=0.6)+
  ylab("Soil water content (m^3/m^3)")+
  xlab("Date")+
  ylim(c(-0.08, 0.4))+
  geom_smooth()+
  theme_classic()+
  scale_x_datetime(limits = lims)+
  scale_colour_manual(values=group.colours, name = "Drought (D) or \ncontrol (C) probes", 
                      labels = c("D1", "D2", "C3", "C4"))+
  theme(legend.background = NULL, axis.text=element_text(size=12), 
        axis.title=element_text(size=14), legend.text=element_text(size=12),
        legend.title=element_text(size=12))
dev.off()

#### Plots average ttm and growth ####
#mean and SE ttm 50% drought only plants
solodata %>% filter(C_or_D=="D") %>%
ggplot(aes(x = Species, y = tt50m))+
  geom_jitter(alpha=0.2, width=0.05, height=0.05)+
  geom_point(stat="summary", fun.y="mean_se",size=2) +
  geom_errorbar(stat="summary", fun.data="mean_se",width=0,size=0.8)+
  ylab("Time to 50% leaf mortality (weeks)")+
  theme_classic()
#mean and SD ttm 50% drought only plants
ggplot(ttmmeans, aes(x = Species, y = mean_tt50m))+
  geom_point(position = position_dodge(0.8), cex=2.5)+
  geom_errorbar(aes(ymin = mean_tt50m-sd_tt50m, ymax = mean_tt50m+sd_tt50m, width = 0.3), position = position_dodge(0.8), cex=1)+
  ylab("Time to 50% leaf mortality (weeks)")+
  theme_classic()+
  theme(axis.text.x = element_text(face = "italic"))
#tt100m
solodata %>% filter(C_or_D=="D") %>%
ggplot(aes(x = Species, y = tt100m))+
  geom_jitter(alpha=0.2, width=0.05, height=0.05)+
  geom_point(stat="summary", fun.y="mean_se",size=2) +
  geom_errorbar(stat="summary", fun.data="mean_se",width=0,size=0.8)+
  ylab("Time to 100% leaf mortality (weeks)")+
  theme_classic()
ggplot(ttmmeans, aes(x = Species, y = mean_tt100m))+
  geom_point(position = position_dodge(0.8), cex=2.5)+
  geom_errorbar(aes(ymin = mean_tt100m-sd_tt100m, ymax = mean_tt100m+sd_tt100m, width = 0.3), position = position_dodge(0.8), cex=1)+
  ylab("Time to 100% leaf mortality (weeks)")+
  theme_classic()+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(face = "italic"))

### change shape of mean data points and save - fixes errors bars for obli
#### Figure 1 - ttm and growth rates of species ####
#100 m 
c <- ggplot() + 
  geom_jitter(data=solodroughtdata, aes(x=Species, y=tt100m), alpha=0.3, width=0.05, height=0.05) + 
  geom_point(data=ttmmeans, aes(x=Species, y=mean_tt100m), cex=2)+
  geom_errorbar(data=ttmmeans, aes(x=Species, ymin = mean_tt100m-sd_tt100m, ymax = mean_tt100m+sd_tt100m, width = 0.15), cex=1)+
  ylab("Time to 100% mortality (wks)")+
  theme_classic()+
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12))
#50m
b <- ggplot() + 
  geom_jitter(data=solodroughtdata, aes(x=Species, y=tt50m), alpha=0.3, width=0.05, height=0.05) + 
  geom_point(data=ttmmeans, aes(x=Species, y=mean_tt50m), cex=2)+
  geom_errorbar(data=ttmmeans, aes(x=Species, ymin = mean_tt50m-sd_tt50m, ymax = mean_tt50m+sd_tt50m, width = 0.15), cex=1)+
  ylab("Time to 50% mortality (wks)")+
  theme_classic()+
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12),
        axis.title.x=element_blank(),
        axis.text.x=element_blank())
#growth rate
a <- ggplot(solodata, aes(x = Species, y = RGR_predrought))+
  geom_jitter(alpha=0.2, width=0.05)+
  geom_point(stat="summary", fun.y="mean_se",size=2) +
  geom_errorbar(stat="summary", fun.data="mean_se",width=0.15,size=1)+
  ylab("RGR (mm/day)")+
  theme_classic()+
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12),
        axis.title.x=element_blank(),
        axis.text.x=element_blank())

#Organise them as a three panel with common species labels
#using cowplot
plot_grid(a,b,c, align="hv", ncol=1, labels = c('A)', 'B)', 'C)'), hjust=-3)

#### Figure 2 - ttm against growth rate ####
#meandata using SD
plot(mean_tt100m ~ mean_RGRpredrought, ylab="Mean time to 100% mortality (wks)", ylim=c(0,6), xlim=c(0,30), 
     xlab="Mean RGR pre-drought (mm/day)", tck=-0.02, pch=19, cex=1.2, bty="l", meandata)
#add error bars
#y axis 
arrows(x0=meandata$mean_RGRpredrought, y0=meandata$mean_tt100m-meandata$sd_tt100m,
       x1=meandata$mean_RGRpredrought, y1=meandata$mean_tt100m+meandata$sd_tt100m, code=3, angle=90, length=0.1)
#x axis
arrows(x0=meandata$mean_RGRpredrought-meandata$sd_RGRpredrought, y0=meandata$mean_tt100m,
       x1=meandata$mean_RGRpredrought+meandata$sd_RGRpredrought, y1=meandata$mean_tt100m, code=3, angle=90, length=0.1)

#mgp c(left, bottom, title) but this does the same thing to both axes
#have to use mgp.axis.labels(x,y) to set them separately
#mgp=c(0,3.5,0) is perfect for x
dev.off()
pdf("Output/ttm~rgr.pdf", width=21, height=21)
par(oma=c(8,8,1,1), mgp=c(0,3,0), mar=c(3, 3, 1, 1))
#mgp.axis.labels(3.5, type='x')
#mgp.axis.labels(100, type='y')
#meandata using 95% CIs
plot(mean_tt100m ~ mean_RGRpredrought, ylab="", ylim=c(0,5), xlim=c(0,22), 
     xlab="", tck=-0.01, pch=19, cex=4, cex.axis=5, bty="n", meandata)
box(bty="l", lwd=4)
#add error bars
#y axis 
arrows(x0=meandata$mean_RGRpredrought, y0=meandata$lower_ttm100,
       x1=meandata$mean_RGRpredrought, y1=meandata$upper_ttm100, code=3, cex = 4, angle=90, length=0.1, lwd=5)
#x axis
arrows(x0=meandata$lower_rgr, y0=meandata$mean_tt100m,
       x1=meandata$upper_rgr, y1=meandata$mean_tt100m, code=3, angle=90, cex = 4, length=0.1, lwd=5)
#Add axis labels
mtext("Mean time to 100% mortality (weeks)", side=2, outer=T, cex=5, line=4)
mtext("Mean RGR pre-drought (mm/day)", side=1, outer=T, cex=5, line=5)
dev.off()
#### Figure 3 - watered growth rates alone or with cons or hets ####
ggplot(pot_rgr_cons, aes(x = Composition, y = mean_pot_rgr_predrought))+
  geom_jitter(alpha=0.4, width=0.05)+
  theme_classic()+
  facet_wrap(~Species)
#Plotted as mean and sd
boxplot(mean_rgr_comp ~ Composition, ylab="blah", ylim=c(0,20),
     xlab="yes", tck=-0.02, pch=19, cex=1.2, bty="l", alone_cons_rgr)
###Using base R so I need to assign dummy x values to distribute along x axis
#alone_cons_rgr$x <- c(0.5, 1, 2, 2.5, 3.5, 4)
#plot(mean_rgr_comp ~ x, ylab="blah", ylim=c(0,20),
#     xlab="yes", tck=-0.02, pch=19, cex=1.2, bty="l", alone_cons_rgr)

#Reordering Composition
pot_rgr_cons$Composition <- factor(pot_rgr_cons$Composition, level = c("AMYG-A", "AAAA", "OVAT-C", "CCCC", "VIMI-D", "DDDD"))
comp_rgr_cons$Composition <- factor(comp_rgr_cons$Composition, level = c("AMYG-A", "AAAA", "OVAT-C", "CCCC", "VIMI-D", "DDDD"))
##
a <- ggplot() + 
  geom_jitter(data=pot_rgr_cons, aes(x=Composition, y=sqrt(mean_pot_rgr_predrought)), alpha=0.3, width=0.05, height=0.05) + 
  geom_point(data=comp_rgr_cons, aes(x=Composition, y=sqrt(mean_rgr_comp)), cex=2)+
  geom_errorbar(data=comp_rgr_cons, aes(x=Composition, ymin = sqrt(lower_rgr_comp), ymax = sqrt(upper_rgr_comp), width = 0.15), cex=1)+
  ylab("sqrt(RGR pre-drought (mm/day))")+
  ylim(-0.1,7)+
  theme_classic()+
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=14))
#Note that one of the 0 RGRs for ovat-c pot 8 is legitimate, stopped growing
#Reordering Composition:Species
pot_rgr_cons$Composition <- factor(pot_rgr_cons$Composition, level = c("AMYG-A", "AAAA", "OVAT-C", "CCCC", "VIMI-D", "DDDD"))
comp_rgr_cons$Composition <- factor(comp_rgr_cons$Composition, level = c("AMYG-A", "AAAA", "OVAT-C", "CCCC", "VIMI-D", "DDDD"))

#growth rate
b <- ggplot() + 
  geom_jitter(data=pot_rgr_hets, aes(x=compspecies, y=sqrt(mean_pot_rgr_predrought)), alpha=0.3, width=0.05, height=0.05) + 
  geom_point(data=comp_rgr_hets, aes(x=compspecies, y=sqrt(mean_rgr_comp)), cex=2)+
  geom_errorbar(data=comp_rgr_hets, aes(x=compspecies, ymin = sqrt(lower_rgr_comp), ymax = sqrt(upper_rgr_comp), width = 0.15), cex=1)+
  ylab("sqrt(RGR pre-drought (mm/day))")+
  ylim(-0.1,7)+
  xlab("Composition_Species")+
  theme_classic()+
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=14))

#Plot together
plot_grid(a,b, align="hv", ncol=1, labels = c('A)', 'B)'), hjust=-3)

#### Supp figure - Fig 3 but for OBLI B, BB, AB, BC, BD ####
#Do this*

## Stats - growth rate by composition ####
#By species, pot as a random effect, alone_or_four is for e.g. A or AAAA only
amygcompdata <- alone_or_four %>% filter(Species=="AMYG")
#Reorder levels so alone is the reference
#amyg
amygcompdata$Composition <- factor(amygcompdata$Composition, levels = c("AMYG-A", "AAAA", "AACC", "AADD"))
amygcompmod <- lmer(sqrt(RGR_predrought) ~ Composition + (1|Pot_number), amygcompdata)
summary(amygcompmod)
dharm <- simulateResiduals(amygcompmod)
plot(dharm)
r.squaredGLMM(amygcompmod)
#ovat
ovatcompdata <- alone_or_four %>% filter(Species=="OVAT")
ovatcompdata$Composition <- factor(ovatcompdata$Composition, levels = c("OVAT-C", "CCCC", "AACC", "CCDD"))
ovatcompmod <- lmer(sqrt(RGR_predrought) ~ Composition + (1|Pot_number), ovatcompdata)
summary(ovatcompmod)
dharm <- simulateResiduals(ovatcompmod)
plot(dharm)
r.squaredGLMM(ovatcompmod)
#vimi
vimicompdata <- alone_or_four %>% filter(Species=="VIMI")
vimicompdata$Composition <- factor(vimicompdata$Composition, levels = c("VIMI-D", "DDDD", "AADD", "CCDD"))
vimicompmod <- lmer(sqrt(RGR_predrought) ~ Composition + (1|Pot_number), vimicompdata)
summary(vimicompmod)
dharm <- simulateResiduals(vimicompmod)
plot(dharm)
r.squaredGLMM(vimicompmod)
### Stats table Fig 3 by species ####

#### Fig 3 as a coef plot ####
#do this*
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




