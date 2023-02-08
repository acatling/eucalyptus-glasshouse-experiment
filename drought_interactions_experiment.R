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
library(sjPlot)
library(emmeans)
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
hist(sqrt(alldata$tt100m))
hist(alldata$tt50m)


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

## Stats - mortality model (fig 1) ####
hist(log(solodroughtdata$tt100m))
hist(solodroughtdata$tt50m)

mort50mod <- lm(tt50m ~ Species, solodroughtdata)
summary(mort50mod)
mortdharm <- simulateResiduals(mort50mod)
plot(mortdharm)
r.squaredGLMM(mort50mod)
emmeans(mort50mod, list(pairwise ~ Species), adjust="tukey")


mort100mod <- lm(tt100m ~ Species, solodroughtdata)
summary(mort100mod)
mortdharm <- simulateResiduals(mort100mod)
plot(mortdharm)
r.squaredGLMM(mort100mod)
emmeans(mort100mod, list(pairwise ~ Species), adjust="tukey")


##07/02/23
#family = Gamma(link=log)
#mort100mod2 <- glm(tt100m ~ Species, family = Gamma, solodroughtdata)
#summary(mort100mod2)
#mortdharm <- simulateResiduals(mort100mod2)
#plot(mortdharm)
##

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

## Stats - growth rate model (fig 1) ####
#Two weeks before drought
solodata$Species <- factor(solodata$Species, levels = c("AMYG", "OBLI", "OVAT", "VIMI"))
growthmod <- lm(sqrt(RGR_predrought) ~ Species, solodata)
summary(growthmod)
growthdharm <- simulateResiduals(growthmod)
plot(growthdharm)
r.squaredGLMM(growthmod)
emmeans(growthmod, list(pairwise ~ Species), adjust="tukey")

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
## Stats - fig 2 - trade-off between drought tolerance and growth rate ####
#regression
tradeoff <- lm(tt100m ~ sqrt(RGR_predrought) + Species, solodroughtdata)
summary(tradeoff)
growthdharm <- simulateResiduals(tradeoff)
plot(growthdharm)
r.squaredGLMM(tradeoff)

tradeoff2 <- lmer(tt100m ~ sqrt(RGR_predrought) + (1|Species), solodroughtdata)
summary(tradeoff2)
growthdharm <- simulateResiduals(tradeoff2)
plot(growthdharm)
r.squaredGLMM(tradeoff2)

tradeoff3 <- lm(tt100m ~ sqrt(RGR_predrought), solodroughtdata)
summary(tradeoff3)
growthdharm <- simulateResiduals(tradeoff3)
plot(growthdharm)
r.squaredGLMM(tradeoff3)

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

## for the y axis letterings, can also specify y = 1+mean_tt100m for example!
#to put it at a standardised height
c <- ggplot() + 
  geom_jitter(data=solodroughtdata, aes(x=Species, y=tt100m), alpha=0.2, col = "grey10", width=0.1, cex = 2.5) + 
  geom_point(data=meandata, aes(x=Species, y=mean_tt100m), cex=3)+
  geom_errorbar(data=meandata, aes(x=Species, ymin = lower_ttm100, ymax = upper_ttm100, width = 0.15), cex=1)+
  ylab("Time to 100% leaf mortality\n(weeks)")+
  theme_classic()+
  ylim(0, 5.6)+
  scale_x_discrete(labels=c("E. amygdalina", "E. obliqua", "E. ovata", "E. viminalis"))+
  geom_text(data=meandata, aes(x=Species, y=5.6, label=c('ab', 'ab', 'a', 'b')), size =5)+
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=14),
        axis.text.x = element_text(face = "italic"))
#50m
b <- ggplot() + 
  geom_jitter(data=solodroughtdata, aes(x=Species, y=tt50m), alpha = 0.2, col = "grey10", width=0.1, cex = 2.5) + 
  geom_point(data=meandata, aes(x=Species, y=mean_tt50m), cex=3)+
  geom_errorbar(data=meandata, aes(x=Species, ymin = lower_ttm50, ymax = upper_ttm50, width = 0.15), cex=1)+
  ylab("Time to 50% leaf mortality\n(weeks)")+
  theme_classic()+
  ylim(0, 5.3)+
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=14),
        axis.title.x=element_blank(),
        axis.text.x=element_blank())
#growth rate
a <- ggplot()+
  geom_jitter(data=solodata, aes(x = Species, y = sqrt(RGR_predrought)), alpha = 0.2, col = "grey10", width=0.1, cex = 2.5)+
  geom_point(data=meandata, aes(x=Species, y=sqrt(mean_RGRpredrought)), cex=3) +
  geom_errorbar(data=meandata, aes(x=Species, ymin = sqrt(lower_rgr), ymax = sqrt(upper_rgr), width = 0.15), cex=1)+
  ylab("sqrt(Relative growth rate) \n(mm/day)")+
  theme_classic()+
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=14),
        axis.title.x=element_blank(),
        axis.text.x=element_blank())

#Organise them as a three panel with common species labels
#using cowplot
pdf("Output/figure_1.pdf", width=8, height=8)
cowplot::plot_grid(a,b,c, align="hv", ncol=1, labels = c('A)', 'B)', 'C)'), hjust=-3.5)
dev.off()


#as boxplots instead:
a <- ggplot(solodata, aes(x = Species, y = sqrt(RGR_predrought)))+
  geom_boxplot(cex=0.6, outlier.size=2.5) + 
  geom_jitter(alpha=0.2, width=0.05)+
  ylab("sqrt(RGR (mm/day))")+
  theme_classic()+
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=14),
        axis.title.x=element_blank(),
        axis.text.x=element_blank())
b <- ggplot(solodroughtdata, aes(x=Species, y=tt50m)) + 
  geom_boxplot(cex=0.6, outlier.size=2.5) + 
  geom_jitter(alpha=0.3, width=0.05, height=0.05) + 
  ylab("Time to 50% mort. (wks)")+
  theme_classic()+
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=14),
        axis.title.x=element_blank(),
        axis.text.x=element_blank())
c <- ggplot(solodroughtdata, aes(x=Species, y=tt100m)) + 
  geom_boxplot(cex=0.6, outlier.size=2.5) + 
  geom_jitter(data=, alpha=0.3, width=0.05, height=0.05) + 
  ylab("Time to 100% mort. (wks)")+
  theme_classic()+
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=14))
cowplot::plot_grid(a,b,c, align="hv", ncol=1, labels = c('A)', 'B)', 'C)'), hjust=-3.2)

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
#colour by species
#sqrt RGR* do this
#plot(meandata$mean_tt100m, meandata$mean_RGRpredrought, col=as.factor(meandata$Species))
#both of these work:
#col=c('red', 'blue', 'green', 'purple')[as.factor(meandata$Species)]
#col=as.factor(meandata$Species)
#col=as.factor(Species)
#col= alpha("lightgrey",0.6)

#cols <- c("red", "blue", "darkgreen", "purple")
plot(mean_tt100m ~ sqrt(mean_RGRpredrought), col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], 
     ylab="", ylim=c(0,5), xlim=c(0,5), 
     xlab="", tck=-0.01, pch=19, cex=4, cex.axis=5, bty="n", meandata)
#Add axis labels
mtext("Mean time to 100% mortality (weeks)", side=2, outer=T, cex=5, line=4)
mtext("sqrt(Mean RGR pre-drought (mm/day))", side=1, outer=T, cex=5, line=5)
#add error bars
#y axis 
arrows(x0=sqrt(meandata$mean_RGRpredrought), y0=meandata$lower_ttm100,
       x1=sqrt(meandata$mean_RGRpredrought), y1=meandata$upper_ttm100, code=3, cex = 4, angle=90, length=0.1, lwd=5)
#x axis
arrows(x0=sqrt(meandata$lower_rgr), y0=meandata$mean_tt100m,
       x1=sqrt(meandata$upper_rgr), y1=meandata$mean_tt100m, code=3, angle=90, cex = 4, length=0.1, lwd=5)
box(bty="l", lwd=4)
points(mean_tt100m ~ sqrt(mean_RGRpredrought), pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], meandata)
#add legend
legend("bottomleft", inset = 0.05, title = "Species", unique(meandata$Species), pch=19, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], cex = 4)
#add points underneath, jittered and coloured by species
#Doesn't make sense to have points underneath, that isn't how the data work
#points(jitter(tt100m, amount = 0.1) ~ jitter(sqrt(RGR_predrought), amount = 0.1), 
#       col=alpha(c('red', 'blue', 'green', 'purple')[as.factor(meandata$Species)], 0.3), pch = 19, cex = 3, solodroughtdata)
dev.off()

### No square root RGR
dev.off()
pdf("Output/ttm~rgr_no_sqrt.pdf", width=21, height=21)
par(oma=c(8,8,1,1), mgp=c(0,3,0), mar=c(3, 3, 1, 1))
plot(mean_tt100m ~ mean_RGRpredrought, col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], 
     ylab="", ylim=c(0,5), xlim=c(0,18), 
     xlab="", tck=-0.01, pch=19, cex=5, cex.axis=5, bty="n", meandata)
mtext("Mean time to 100% mortality (weeks)", side=2, outer=T, cex=5, line=4)
mtext("Mean RGR pre-drought (mm/day)", side=1, outer=T, cex=5, line=5)
arrows(x0=meandata$mean_RGRpredrought, y0=meandata$lower_ttm100,
       x1=meandata$mean_RGRpredrought, y1=meandata$upper_ttm100, code=3, cex = 4, angle=90, length=0.1, lwd=5)
arrows(x0=meandata$lower_rgr, y0=meandata$mean_tt100m,
       x1=meandata$upper_rgr, y1=meandata$mean_tt100m, code=3, angle=90, cex = 4, length=0.1, lwd=5)
box(bty="l", lwd=4)
points(mean_tt100m ~ mean_RGRpredrought, pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], meandata)
legend("bottomleft", inset = 0.05, title = "Species", c(expression(italic("E. amygdalina")), expression(italic("E. obliqua")), expression(italic("E. ovata")), expression(italic("E. viminalis"))), pch=19, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], cex = 4)
dev.off()

## with underlying data points
### Not working properly **
dev.off()
pdf("Output/ttm~rgr_data.pdf", width=21, height=21)
par(oma=c(8,8,1,1), mgp=c(0,3,0), mar=c(3, 3, 1, 1))
plot(mean_tt100m ~ mean_RGRpredrought, col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], 
     ylab="", ylim=c(0,5), xlim=c(0,18), 
     xlab="", tck=-0.01, pch=19, cex=5, cex.axis=5, bty="n", meandata)
points(tt100m ~ RGR_predrought, col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(solodroughtdata$Species)], 
       pch = 19, cex = 3, solodroughtdata)
mtext("Mean time to 100% mortality (weeks)", side=2, outer=T, cex=5, line=4)
mtext("Mean RGR pre-drought (mm/day)", side=1, outer=T, cex=5, line=5)
arrows(x0=meandata$mean_RGRpredrought, y0=meandata$lower_ttm100,
       x1=meandata$mean_RGRpredrought, y1=meandata$upper_ttm100, code=3, cex = 4, angle=90, length=0.1, lwd=5)
arrows(x0=meandata$lower_rgr, y0=meandata$mean_tt100m,
       x1=meandata$upper_rgr, y1=meandata$mean_tt100m, code=3, angle=90, cex = 4, length=0.1, lwd=5)
box(bty="l", lwd=4)
points(mean_tt100m ~ mean_RGRpredrought, pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], meandata)
legend("bottomleft", inset = 0.05, title = "Species", c(expression(italic("E. amygdalina")), expression(italic("E. obliqua")), expression(italic("E. ovata")), expression(italic("E. viminalis"))), pch=19, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], cex = 4)
dev.off()

#### Figure 2 - watered growth rates with nbhs means and CIs ####
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
cowplot::plot_grid(a,b, align="hv", ncol=1, labels = c('A)', 'B)'), hjust=-3)
### as boxplots instead ###
a <- ggplot() + 
  geom_boxplot(data=pot_rgr_cons, aes(x=Composition, y=sqrt(mean_pot_rgr_predrought)), cex=0.6, outlier.size=2.5) + 
  geom_jitter(data=pot_rgr_cons, aes(x=Composition, y=sqrt(mean_pot_rgr_predrought)), alpha=0.2, cex=1.5, width=0.05) + 
  ylab("sqrt(RGR pre-drought (mm/day))")+
  ylim(-0.1,7)+
  theme_classic()+
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=14))
#growth rate
b <- ggplot() + 
  geom_boxplot(data=pot_rgr_hets, aes(x=compspecies, y=sqrt(mean_pot_rgr_predrought)), cex=0.6, outlier.size=2.5) + 
  geom_jitter(data=pot_rgr_hets, aes(x=compspecies, y=sqrt(mean_pot_rgr_predrought)), alpha=0.3, width=0.05, height=0.05) + 
  ylab("sqrt(RGR pre-drought (mm/day))")+
  ylim(-0.1,7)+
  xlab("Composition_Species")+
  theme_classic()+
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=14))
cowplot::plot_grid(a,b, align="hv", ncol=1, labels = c('A)', 'B)'), hjust=-3)

#### Supp figure - Fig 2 but for OBLI B, BB, AB, BC, BD ####
obli_comp_avg <- within(obli_comp_avg, compspecies[compspecies=="OBLI-B_OBLI"] <- 'OBLI-B')
obli_comp_avg <- within(obli_comp_avg, compspecies[compspecies=="BB_OBLI"] <- 'BB')
oblicomprgrmeans <- within(oblicomprgrmeans, compspecies[compspecies=="OBLI-B_OBLI"] <- 'OBLI-B')
oblicomprgrmeans <- within(oblicomprgrmeans, compspecies[compspecies=="BB_OBLI"] <- 'BB')

oblicomprgrmeans$compspecies <- factor(oblicomprgrmeans$compspecies, level = c("OBLI-B", 
                                          "BB", "AB_OBLI", "AB_AMYG", "BC_OBLI", "BC_OVAT",
                                          "BD_OBLI", "BD_VIMI"))
obli_comp_avg$compspecies <- factor(obli_comp_avg$compspecies, 
                                    level = c("OBLI-B", 
                  "BB", "AB_OBLI", "AB_AMYG", "BC_OBLI", "BC_OVAT",
                  "BD_OBLI", "BD_VIMI"))
#means with CIs
ggplot() + 
  geom_jitter(data=obli_comp_avg, aes(x=compspecies, y=sqrt(mean_pot_rgr_predrought)), alpha=0.3, width=0.05, height=0.05) + 
  geom_point(data=oblicomprgrmeans, aes(x=compspecies, y=sqrt(mean_rgr_comp)), cex=2)+
  geom_errorbar(data=oblicomprgrmeans, aes(x=compspecies, ymin = sqrt(lower_rgr_comp), ymax = sqrt(upper_rgr_comp), width = 0.15), cex=1)+
  ylab("sqrt(RGR pre-drought (mm/day))")+
  #ylim(-0.1,7)+
  theme_classic()
#boxplot
ggplot() + 
  geom_boxplot(data=obli_comp_avg, aes(x=compspecies, y=sqrt(mean_pot_rgr_predrought)), outlier.size=3)+
  geom_point(data=obli_comp_avg, aes(x=compspecies, y=sqrt(mean_pot_rgr_predrought)), alpha=0.3, cex = 3) + 
  ylab("sqrt(RGR pre-drought (mm/day))")+
  ylim(-0.1,7)+
  xlab("Composition_Species")+
  theme_classic()+
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=16))

## Stats - Fig 2 - growth rate by composition ####
#overall
test <- alldata %>% unite("speciescomp", Species:Composition, remove = FALSE)
test <- test %>% filter(Composition == "AACC" | Composition == "AADD" 
                        | Composition == "AMYG-A" | Composition == "AAAA")
growthoverallmod <- lmer(sqrt(RGR_predrought) ~ speciescomp + (1|Pot_number), test)
summary(growthoverallmod)
dharm <- simulateResiduals(growthoverallmod)
plot(dharm)
emmeans(growthoverallmod, list(pairwise ~ speciescomp), adjust="tukey")


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
#using emmeans to compare groups, Tukey posthoc test
emmeans(amygcompmod, list(pairwise ~ Composition), adjust="tukey")
#These two below lines do the same thing as above
test <- emmeans(amygcompmod, ~Composition)
pairs(test)
#another method
library(multcomp)
summary(glht(amygcompmod, linfct = mcp(Composition = "Tukey")), test = adjusted("holm"))

##obli
obli_comp$Composition <- factor(obli_comp$Composition, levels = c("OBLI-B", "BB", "AB", "BC", "BD"))
oblicompmod <- lmer(sqrt(RGR_predrought) ~ Composition + (1|Pot_number), obli_comp)
dharm <- simulateResiduals(oblicompmod)
plot(dharm)
summary(oblicompmod)
tab_model(oblicompmod)
#grouped hets
oblicompmod2 <- lmer(sqrt(RGR_predrought) ~ grouped_comp + (1|Pot_number), obli_comp)
summary(oblicompmod2)
dharm <- simulateResiduals(oblicompmod2)
plot(dharm)
r.squaredGLMM(oblicompmod2)
tab_model(oblicompmod2)
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

### Fig 2 coef plot of growth rates with nbhs ####
#recall that labels are inverted
specieslist <- c("AMYG", "OVAT", "VIMI")
growthmods <- c(amygcompmod, ovatcompmod, vimicompmod)
plot_models(growthmods, transform = NULL, vline.color = "grey", legend.title = "Species",
            dot.size = 2, line.size = 1)+
  ylab("Estimate")+
  theme_classic()+
  scale_colour_discrete(labels = c("VIMI", "OVAT", "AMYG"))

###with obli too
#need to rework amyg/ovat/vimi models to include AB, BC and BD responses
amygcompdata2 <- alldata %>% filter(Species == "AMYG")
amygcompdata2$Composition <- factor(amygcompdata2$Composition, levels = c("AMYG-A", "AAAA", "AB", "AACC", "AADD"))
amygcompmod2 <- lmer(sqrt(RGR_predrought) ~ Composition + (1|Pot_number), amygcompdata2)
summary(amygcompmod2)
ovatcompdata2 <- alldata %>% filter(Species == "OVAT")
ovatcompdata2$Composition <- factor(ovatcompdata2$Composition, levels = c("OVAT-C", "CCCC", "BC", "AACC", "CCDD"))
ovatcompmod2 <- lmer(sqrt(RGR_predrought) ~ Composition + (1|Pot_number), ovatcompdata2)
summary(ovatcompmod2)
vimicompdata2 <- alldata %>% filter(Species == "VIMI")
vimicompdata2$Composition <- factor(vimicompdata2$Composition, levels = c("VIMI-D", "DDDD", "AADD", "BD", "CCDD"))
vimicompmod2 <- lmer(sqrt(RGR_predrought) ~ Composition + (1|Pot_number), vimicompdata2)
summary(vimicompmod2)

specieslist2 <- c("AMYG", "OBLI", "OVAT", "VIMI")
growthmods2 <- c(amygcompmod2, oblicompmod, ovatcompmod2, vimicompmod2)
plot_models(growthmods2, transform = NULL, vline.color = "grey", legend.title = "Species",
            dot.size = 2, line.size = 1)+
  ylab("Estimate")+
  theme_classic()+
  scale_colour_discrete(labels = c("VIMI", "OVAT", "OBLI", "AMYG"))

### Reworking labels
amygcompdata2 <- alone_or_four %>% filter(Species=="AMYG")
amygcompdata2 <- within(amygcompdata2, Composition[Composition == "AMYG-A"] <- 'alone')
amygcompdata2 <- within(amygcompdata2, Composition[Composition == "AAAA"] <- 'conspecific')
amygcompdata2 <- within(amygcompdata2, Composition[Composition == "AACC"] <- 'amygdalina-ovata')
amygcompdata2 <- within(amygcompdata2, Composition[Composition == "AADD"] <- 'amygdalina-viminalis')
amygcompdata2$Composition <- factor(amygcompdata2$Composition, levels = c("alone", "conspecific", "amygdalina-ovata", "amygdalina-viminalis"))
amygcompmod3 <- lmer(sqrt(RGR_predrought) ~ Composition + (1|Pot_number), amygcompdata2)

ovatcompdata2 <- alone_or_four %>% filter(Species=="OVAT")
ovatcompdata2 <- within(ovatcompdata2, Composition[Composition == "OVAT-C"] <- 'alone')
ovatcompdata2 <- within(ovatcompdata2, Composition[Composition == "CCCC"] <- 'conspecific')
ovatcompdata2 <- within(ovatcompdata2, Composition[Composition == "AACC"] <- 'amygdalina-ovata')
ovatcompdata2 <- within(ovatcompdata2, Composition[Composition == "CCDD"] <- 'ovata-viminalis')
ovatcompdata2$Composition <- factor(ovatcompdata2$Composition, levels = c("alone", "conspecific", "amygdalina-ovata", "ovata-viminalis"))
ovatcompmod3 <- lmer(sqrt(RGR_predrought) ~ Composition + (1|Pot_number), ovatcompdata2)

vimicompdata2 <- alone_or_four %>% filter(Species=="VIMI")
vimicompdata2 <- within(vimicompdata2, Composition[Composition == "VIMI-D"] <- 'alone')
vimicompdata2 <- within(vimicompdata2, Composition[Composition == "DDDD"] <- 'conspecific')
vimicompdata2 <- within(vimicompdata2, Composition[Composition == "AADD"] <- 'amygdalina-viminalis')
vimicompdata2 <- within(vimicompdata2, Composition[Composition == "CCDD"] <- 'ovata-viminalis')
vimicompdata2$Composition <- factor(vimicompdata2$Composition, levels = c("alone", "conspecific", "amygdalina-viminalis", "ovata-viminalis"))
vimicompmod3 <- lmer(sqrt(RGR_predrought) ~ Composition + (1|Pot_number), vimicompdata2)

growthmods3 <- c(amygcompmod3, ovatcompmod3, vimicompmod3)

dev.off()
pdf("Output/figure_2.pdf", width=8, height=4)
plot_models(growthmods3, transform = NULL, vline.color = "grey", legend.title = "Species",
            dot.size = 2, line.size = 1)+
  ylab("Estimate")+
  xlab("Competition treatment")+
  theme_classic()+
  ylim(-3.2,1.2)+
  scale_colour_discrete(labels = c(expression(italic("E. viminalis")), expression(italic("E. ovata")), expression(italic("E. amygdalina"))))+
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=16),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16),
        legend.text.align = 0,
        legend.position = "right")
dev.off()

#### Fig 2 coef plot with obli ####
amygcompdata3 <- alldata %>% filter(Species == "AMYG")
amygcompdata3 <- within(amygcompdata3, Composition[Composition == "AMYG-A"] <- 'alone')
amygcompdata3 <- within(amygcompdata3, Composition[Composition == "AAAA"] <- 'conspecific')
amygcompdata3 <- within(amygcompdata3, Composition[Composition == "AB"] <- 'amygdalina-obliqua')
amygcompdata3 <- within(amygcompdata3, Composition[Composition == "AACC"] <- 'amygdalina-ovata')
amygcompdata3 <- within(amygcompdata3, Composition[Composition == "AADD"] <- 'amygdalina-viminalis')
amygcompdata3$Composition <- factor(amygcompdata3$Composition, levels = c("alone", "conspecific", "amygdalina-obliqua", "amygdalina-ovata", "amygdalina-viminalis"))
amygcompmod4 <- lmer(sqrt(RGR_predrought) ~ Composition + (1|Pot_number), amygcompdata3)

oblicompdata2 <- obli_comp
oblicompdata2 <- within(oblicompdata2, Composition[Composition == "OBLI-B"] <- 'alone')
oblicompdata2 <- within(oblicompdata2, Composition[Composition == "BB"] <- 'conspecific')
oblicompdata2 <- within(oblicompdata2, Composition[Composition == "AB"] <- 'amygdalina-obliqua')
oblicompdata2 <- within(oblicompdata2, Composition[Composition == "BC"] <- 'obliqua-ovata')
oblicompdata2 <- within(oblicompdata2, Composition[Composition == "BD"] <- 'obliqua-viminalis')
oblicompdata2$Composition <- factor(oblicompdata2$Composition, levels = c("alone", "conspecific", "amygdalina-obliqua", "obliqua-ovata", "obliqua-viminalis"))
oblicompmod4 <- lmer(sqrt(RGR_predrought) ~ Composition + (1|Pot_number), oblicompdata2)

ovatcompdata3 <- alldata %>% filter(Species=="OVAT")
ovatcompdata3 <- within(ovatcompdata3, Composition[Composition == "OVAT-C"] <- 'alone')
ovatcompdata3 <- within(ovatcompdata3, Composition[Composition == "CCCC"] <- 'conspecific')
ovatcompdata3 <- within(ovatcompdata3, Composition[Composition == "AACC"] <- 'amygdalina-ovata')
ovatcompdata3 <- within(ovatcompdata3, Composition[Composition == "BC"] <- 'obliqua-ovata')
ovatcompdata3 <- within(ovatcompdata3, Composition[Composition == "CCDD"] <- 'ovata-viminalis')
ovatcompdata3$Composition <- factor(ovatcompdata3$Composition, levels = c("alone", "conspecific", "amygdalina-ovata", "obliqua-ovata", "ovata-viminalis"))
ovatcompmod4 <- lmer(sqrt(RGR_predrought) ~ Composition + (1|Pot_number), ovatcompdata3)

vimicompdata3 <- alldata %>% filter(Species=="VIMI")
vimicompdata3 <- within(vimicompdata3, Composition[Composition == "VIMI-D"] <- 'alone')
vimicompdata3 <- within(vimicompdata3, Composition[Composition == "DDDD"] <- 'conspecific')
vimicompdata3 <- within(vimicompdata3, Composition[Composition == "AADD"] <- 'amygdalina-viminalis')
vimicompdata3 <- within(vimicompdata3, Composition[Composition == "BD"] <- 'obliqua-viminalis')
vimicompdata3 <- within(vimicompdata3, Composition[Composition == "CCDD"] <- 'ovata-viminalis')
vimicompdata3$Composition <- factor(vimicompdata3$Composition, levels = c("alone", "conspecific", "amygdalina-viminalis", "obliqua-viminalis", "ovata-viminalis"))
vimicompmod4 <- lmer(sqrt(RGR_predrought) ~ Composition + (1|Pot_number), vimicompdata3)

growthmods4 <- c(amygcompmod4, oblicompmod4, ovatcompmod4, vimicompmod4)

dev.off()
pdf("Output/figure_2+obli.pdf", width=8, height=6)
#show.p=T, p.shape=T
#function for backtransforming data - not working, also doesn't work for neg value
#square <- function(x){
#  return(x**2)
#}
plot_models(growthmods4, transform = NULL, vline.color = "grey", legend.title = "Species",
            dot.size = 2, line.size = 1, ci.lvl=0.95)+
  ylab("Estimate")+
  xlab("Competition treatment")+
  theme_classic()+
  ylim(-3.2,2.5)+
  scale_colour_discrete(labels = c(expression(italic("E. viminalis")), expression(italic("E. ovata")), expression(italic("E. obliqua")), expression(italic("E. amygdalina"))))+
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=16),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16),
        legend.text.align = 0,
        legend.position = "right",
        legend.title.align = 0.5)
dev.off()

### Stats table Fig 3 by species ####
#sJplot
#Doesn't work - lose negative sign, not sure how to avoid this
#myfun <- function(x) x^2
#tab_model(amygcompmod, ovatcompmod, vimicompmod, transform = "myfun")
tab_model(amygcompmod, ovatcompmod, vimicompmod, transform = NULL)

## Extracting values for all in a loop
model_list <- list(amygcompmod, ovatcompmod, vimicompmod)
effects = lapply(1:length(model_list), function(x) {
  as.data.frame(coef(summary(model_list[[x]]))) %>% mutate(Species=paste0(x))})
effects_table <- do.call("rbind", effects)

#Make rownames a column 
effects_table <- cbind(Effect = rownames(effects_table), effects_table)
#Remove rownames
rownames(effects_table) <- NULL

#Renaming effects since loop adding values to ends
effects_table$Effect[startsWith(effects_table$Effect, '(Intercept)')] <- 'Intercept'
effects_table$Effect[startsWith(effects_table$Effect, 'CompositionAACC')] <- 'AACC'
effects_table$Effect[startsWith(effects_table$Effect, 'CompositionAADD')] <- 'AADD'
effects_table$Effect[startsWith(effects_table$Effect, 'CompositionCCDD')] <- 'CCDD'
effects_table$Effect[startsWith(effects_table$Effect, 'CompositionAAAA')] <- 'AAAA'
effects_table$Effect[startsWith(effects_table$Effect, 'CompositionCCCC')] <- 'CCCC'
effects_table$Effect[startsWith(effects_table$Effect, 'CompositionDDDD')] <- 'DDDD'

effects_table <- within(effects_table, Species[Species == '1'] <- 'E. amygdalina')
effects_table <- within(effects_table, Species[Species == '2'] <- 'E. ovata')
effects_table <- within(effects_table, Species[Species == '3'] <- 'E. viminalis')
#Renaming columns
effects_table <- effects_table %>% select(Species, Effect, Estimate, 'SE' = 'Std. Error', 'p_value' = 'Pr(>|t|)')

#Making column with Estimate (+/- SE) and p value asterisks all combined
#effects_table$collated <- sprintf("%1.1f ± %1.1f", effects_table$Estimate, effects_table$SE)

#Add column for asterisks based on below function
effects_table <- effects_table %>% mutate(p_asterisks = case_when(p_value >=0.05~"",
                                                                  p_value <0.001~"***",
                                                                  p_value <0.01~"**",
                                                                  p_value <0.05~"*"))
effects_table$collated <- sprintf("%1.3f ± %1.2f%s", effects_table$Estimate, effects_table$SE, effects_table$p_asterisks)

growth_effects_kbl <- effects_table %>% select(Species, Effect, collated)

# #This made the table wider but some of the column aren't shared here so many NAs
# growth_effects_kbl <- growth_effects_kbl %>% group_by(Species) %>% mutate(row = row_number()) %>%
#   pivot_wider(names_from = Species, values_from = collated) %>% select(-row)

#Want to include RE info too
#Split out each sp
amygtable <- growth_effects_kbl %>% filter(Species == "E. amygdalina") %>% select(Effect, collated)
#wider
amyg_kbl <- amygtable %>% pivot_wider(names_from = Effect, values_from = collated)
#Plotting with kableR
#Marginal r squareds from below table
#caption: <b>Table 2</b>. Model output with Estimate ± SE (standard error) for each species modelled separately. Marginal R-squared values reported. NCI is neighbourhood crowding index from all neighbours.
# Mean MD is long-term mean moisture deficit summed over the growth period. MD anomaly is the difference between mean MD and observed MD. Preceding DBH is the size of each focal stem at the start of each growth period.
# Low values of PC1 represent low soil fertility and conductivity.
growth_effects_kbl %>% mutate(Effect = c("Intercept", "Total NCI", "Mean MD", "MD anomaly", "Preceding DBH", "PC1", "Total NCI:Mean MD", "Total NCI:Preceding DBH", "Mean MD:Preceding DBH", "test", "testing", "Tests")) %>%
  kbl(align = 'lcccc', caption = "") %>%
 # add_header_above(c(" "=1, "R^2=0.20"=1, "R^2=0.21"=1, "R^2=0.25"=1, "R^2=0.19"=1), align = c("l", "c", "c", "c", "c")) %>%
  kable_classic(full_width = T, html_font = "Times", font_size = 12) %>%
  kable_styling(font_size = 14) %>%
 # row_spec(0, italic = T) %>%
  footnote(general = "Continuous predictors are scaled to a mean of 0. Mean MD is 308.8 mm per growth period. Mean MD anomaly is -336.6 mm per growth period. Mean preceding DBH is 368.2 mm.", general_title="")

#### Figure 3 - effect of nbhs on time to mortality means and CIs ####
#geom_point(data=mean_ttm_cons, aes(x=Composition, y=mean_tt100m), cex=0.6, outlier.size=2.5) + 
##CIS and points

##boxplots
#Reordering Composition for cons
pot_ttm_cons$Composition <- factor(pot_ttm_cons$Composition, level = c("AMYG-A", "AAAA", "OVAT-C", "CCCC", "VIMI-D", "DDDD"))
mean_ttm_cons$Composition <- factor(mean_ttm_cons$Composition, level = c("AMYG-A", "AAAA", "OVAT-C", "CCCC", "VIMI-D", "DDDD"))

a <- ggplot() + 
  geom_boxplot(data=pot_ttm_cons, aes(x=Composition, y=potmean_tt100m), cex=0.6, outlier.size=2.5) + 
  geom_jitter(data=pot_ttm_cons, aes(x=Composition, y=potmean_tt100m), alpha=0.2, cex=2, width=0.05) + 
  ylab("Time to 100% mortality (weeks)")+
  ylim(-0.1,5.5)+
  theme_classic()+
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=14))
#growth rate
b <- ggplot() + 
  geom_boxplot(data=pot_ttm_hets, aes(x=compspecies, y=potmean_tt100m), cex=0.6, outlier.size=2.5) + 
  geom_jitter(data=pot_ttm_hets, aes(x=compspecies, y=potmean_tt100m), alpha=0.2, cex=1.5, width=0.05) + 
  ylab("Time to 100% mortality (weeks)")+
  ylim(-0.1,5.5)+
  xlab("Composition_Species")+
  theme_classic()+
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=14))
cowplot::plot_grid(a,b, align="hv", ncol=1, labels = c('A)', 'B)'), hjust=-3)
#### Supp figure - Fig 3 but for OBLI B, BB, AB, BC, BD ####
#just not enough data
#### Stats - Fig 3 mortality and comp ####
#Reorder levels so alone is the reference
#amyg
amygttmcomp <- alone_or_four %>% filter(Species=="AMYG", C_or_D == "D")
hist(amygttmcomp$tt100m)
amygttmcomp$Composition <- factor(amygttmcomp$Composition, levels = c("AMYG-A", "AAAA", "AACC", "AADD"))
amygttmmod <- lmer(tt100m ~ Composition + (1|Pot_number), amygttmcomp)
summary(amygttmmod)
dharm <- simulateResiduals(amygttmmod)
plot(dharm)

#trying with gamma distribution instead
#can't have 0 values...
#amygttmmod2 <- glmer(tt100m ~ Composition + (1|Pot_number), family = Gamma, amygttmcomp)
#summary(amygttmmod)
#dharm <- simulateResiduals(amygttmmod)
#plot(dharm)

r.squaredGLMM(amygttmmod)
emmeans(amygttmmod, list(pairwise ~ Composition), adjust="tukey")

##obli
##not enough data

##ovat
ovatttmcomp <- alone_or_four %>% filter(Species=="OVAT", C_or_D == "D")
hist((ovatttmcomp$tt100m))
ovatttmcomp$Composition <- factor(ovatttmcomp$Composition, levels = c("OVAT-C", "CCCC", "AACC", "CCDD"))
ovatttmmod <- lmer(tt100m ~ Composition + (1|Pot_number), ovatttmcomp)
summary(ovatttmmod)
dharm <- simulateResiduals(ovatttmmod)
plot(dharm)
r.squaredGLMM(ovatttmmod)
emmeans(ovatttmmod, list(pairwise ~ Composition), adjust="tukey")


##vimi
vimittmcomp <- alone_or_four %>% filter(Species=="VIMI", C_or_D == "D")
hist((vimittmcomp$tt100m))
vimittmcomp$Composition <- factor(vimittmcomp$Composition, levels = c("VIMI-D", "DDDD", "AADD", "CCDD"))
vimittmmod <- lmer(tt100m ~ Composition + (1|Pot_number), vimittmcomp)
dharm <- simulateResiduals(vimittmmod)
plot(dharm)
summary(vimittmmod)
r.squaredGLMM(vimittmmod)
emmeans(vimittmmod, list(pairwise ~ Composition), adjust="tukey")

##stats table ##
tab_model(amygttmmod, ovatttmmod, vimittmmod, transform = NULL)

#testing differences between compositions, e.g. does AACC_ovat take longer
# to die than AACC_amyg?
alone_or_four <- alone_or_four %>% unite("compspecies", Composition:Species, remove = FALSE)
AACC <- alone_or_four %>% filter(Composition == "AACC")
ttmcompmod <- lmer(tt100m ~ compspecies + (1|Pot_number), AACC)
summary(ttmcompmod)
dharm <- simulateResiduals(ttmcompmod)
plot(dharm)
r.squaredGLMM(ttmcompmod)
emmeans(ttmcompmod, list(pairwise ~ compspecies), adjust="tukey")

## and what about for growth?
rgrcompmod <- lmer(sqrt(RGR_predrought) ~ compspecies + (1|Pot_number), AACC)
summary(rgrcompmod)
dharm <- simulateResiduals(rgrcompmod)
plot(dharm)
r.squaredGLMM(rgrcompmod)
emmeans(rgrcompmod, list(pairwise ~ compspecies), adjust="tukey")

### Figure 3 - coef plotmortality ~ nbhs ####
#recall that labels are inverted
amygttmcomp <- alone_or_four %>% filter(Species=="AMYG", C_or_D == "D")
amygttmcomp <- within(amygttmcomp, Composition[Composition == "AMYG-A"] <- 'alone')
amygttmcomp <- within(amygttmcomp, Composition[Composition == "AAAA"] <- 'conspecific')
amygttmcomp <- within(amygttmcomp, Composition[Composition == "AACC"] <- 'amygdalina-ovata')
amygttmcomp <- within(amygttmcomp, Composition[Composition == "AADD"] <- 'amygdalina-viminalis')
amygttmcomp$Composition <- factor(amygttmcomp$Composition, levels = c("alone", "conspecific", "amygdalina-ovata", "amygdalina-viminalis"))
amygttmmod <- lmer(tt100m ~ Composition + (1|Pot_number), amygttmcomp)

ovatttmcomp <- alone_or_four %>% filter(Species=="OVAT", C_or_D == "D")
ovatttmcomp <- within(ovatttmcomp, Composition[Composition == "OVAT-C"] <- 'alone')
ovatttmcomp <- within(ovatttmcomp, Composition[Composition == "CCCC"] <- 'conspecific')
ovatttmcomp <- within(ovatttmcomp, Composition[Composition == "AACC"] <- 'amygdalina-ovata')
ovatttmcomp <- within(ovatttmcomp, Composition[Composition == "CCDD"] <- 'ovata-viminalis')
ovatttmcomp$Composition <- factor(ovatttmcomp$Composition, levels = c("alone", "conspecific", "amygdalina-ovata", "ovata-viminalis"))
ovatttmmod <- lmer(tt100m ~ Composition + (1|Pot_number), ovatttmcomp)

vimittmcomp <- alone_or_four %>% filter(Species=="VIMI", C_or_D == "D")
vimittmcomp <- within(vimittmcomp, Composition[Composition == "VIMI-D"] <- 'alone')
vimittmcomp <- within(vimittmcomp, Composition[Composition == "DDDD"] <- 'conspecific')
vimittmcomp <- within(vimittmcomp, Composition[Composition == "AADD"] <- 'amygdalina-viminalis')
vimittmcomp <- within(vimittmcomp, Composition[Composition == "CCDD"] <- 'ovata-viminalis')
vimittmcomp$Composition <- factor(vimittmcomp$Composition, levels = c("alone", "conspecific", "amygdalina-viminalis", "ovata-viminalis"))
vimittmmod <- lmer(tt100m ~ Composition + (1|Pot_number), vimittmcomp)
summary(vimittmmod)

ttmmods <- c(amygttmmod, ovatttmmod, vimittmmod)
dev.off()
pdf("Output/figure_3.pdf", width=8, height=6)
#Can specify colors=c('red', 'forestgreen', 'purple') in plot_models but overridden by scale_colour_discrete
plot_models(ttmmods, transform = NULL, vline.color = "grey", legend.title = "Species",
            dot.size = 2, line.size = 1, ci.lvl=0.95)+
  ylab("Estimate")+
  xlab("Competition treatment")+
  theme_classic()+
  scale_colour_discrete(labels = c(expression(italic("E. viminalis")), expression(italic("E. ovata")), expression(italic("E. amygdalina"))))+
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=16),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16),
        legend.text.align = 0,
        legend.position = "right",
        legend.title.align = 0.5)
dev.off()

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





#### Traits plots #####
#Hist of traits
hist(sometraits$mean_ldmc_plant)
hist(sometraits$mean_sla_plant)
hist(sometraits$mean_huber_plant)
hist(sometraits$wd)
#huber
ggplot(sometraits, aes(x = Species, y = mean_huber_plant))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, width = 0.1)+
  theme_classic()
#sla
ggplot(sometraits, aes(x = Species, y = mean_sla_plant))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, width = 0.1)+
  theme_classic()
#ldmc
ggplot(sometraits, aes(x = Species, y = mean_ldmc_plant))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, width = 0.1)+
  theme_classic()
#wd
ggplot(sometraits, aes(x = Species, y = wd))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, width = 0.1)+
  theme_classic()

### Plot - ttm against sla, wd and huber
#ttm ~ wd
dev.off()
pdf("Output/ttm~wd.pdf", width=21, height=21)
par(oma=c(8,8,1,1), mgp=c(0,3,0), mar=c(3, 3, 1, 1))
plot(mean_tt100m ~ mean_wd_sp, col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], 
     ylab="", ylim=c(0,5), xlim=c(0.45,0.52), 
     xlab="", tck=-0.01, pch=19, cex=5, cex.axis=4, bty="n", meandata)
mtext("Mean time to 100% mortality (weeks)", side=2, outer=T, cex=4, line=4)
mtext("Mean wood density (g/cm^3)", side=1, outer=T, cex=5, line=5)
arrows(x0=meandata$mean_wd_sp, y0=meandata$lower_ttm100,
       x1=meandata$mean_wd_sp, y1=meandata$upper_ttm100, code=3, cex = 4, angle=90, length=0.1, lwd=5)
arrows(x0=meandata$lower_wd, y0=meandata$mean_tt100m,
       x1=meandata$upper_wd, y1=meandata$mean_tt100m, code=3, angle=90, cex = 4, length=0.1, lwd=5)
points(mean_tt100m ~ mean_wd_sp, pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], meandata)
box(bty="l", lwd=4)
legend("bottomleft", inset = 0.05, title = "Species", c(expression(italic("E. amygdalina")), expression(italic("E. obliqua")), expression(italic("E. ovata")), expression(italic("E. viminalis"))), pch=19, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], cex = 4)
dev.off()

#ttm ~ wd
dev.off()
pdf("Output/ttm~huber.pdf", width=21, height=21)
par(oma=c(8,8,1,1), mgp=c(0,3,0), mar=c(3, 3, 1, 1))
plot(mean_tt100m ~ mean_huber_sp, col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], 
     ylab="", ylim=c(0,5), xlim=c(20,60), 
     xlab="", tck=-0.01, pch=19, cex=5, cex.axis=4, bty="n", meandata)
mtext("Mean time to 100% mortality (weeks)", side=2, outer=T, cex=4, line=4)
mtext("Mean Huber value", side=1, outer=T, cex=4, line=5)
arrows(x0=meandata$mean_huber_sp, y0=meandata$lower_ttm100,
       x1=meandata$mean_huber_sp, y1=meandata$upper_ttm100, code=3, cex = 4, angle=90, length=0.1, lwd=5)
arrows(x0=meandata$lower_huber, y0=meandata$mean_tt100m,
       x1=meandata$upper_huber, y1=meandata$mean_tt100m, code=3, angle=90, cex = 4, length=0.1, lwd=5)
points(mean_tt100m ~ mean_huber_sp, pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], meandata)
box(bty="l", lwd=4)
legend("bottomleft", inset = 0.05, title = "Species", c(expression(italic("E. amygdalina")), expression(italic("E. obliqua")), expression(italic("E. ovata")), expression(italic("E. viminalis"))), pch=19, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], cex = 4)
dev.off()

### Plot - rgr against sla and wood density
dev.off()
pdf("Output/rgr~sla.pdf", width=21, height=21)
par(oma=c(8,8,1,1), mgp=c(0,3,0), mar=c(3, 3, 1, 1))
plot(mean_RGRpredrought ~ mean_sla_sp, col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], 
     ylab="", ylim=c(0,18), xlim=c(80,150), 
     xlab="", tck=-0.01, pch=19, cex=5, cex.axis=4, bty="n", meandata)
mtext("Mean RGR pre-drought (mm/day)", side=2, outer=T, cex=4, line=4)
mtext("Mean specific leaf area (cm^2/g)", side=1, outer=T, cex=4, line=5)
arrows(x0=meandata$mean_sla_sp, y0=meandata$lower_rgr,
       x1=meandata$mean_sla_sp, y1=meandata$upper_rgr, code=3, cex = 4, angle=90, length=0.1, lwd=5)
arrows(x0=meandata$lower_sla, y0=meandata$mean_RGRpredrought,
       x1=meandata$upper_sla, y1=meandata$mean_RGRpredrought, code=3, angle=90, cex = 4, length=0.1, lwd=5)
box(bty="l", lwd=4)
points(mean_RGRpredrought ~ mean_sla_sp, pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], meandata)
legend("bottomleft", inset = 0.05, title = "Species", c(expression(italic("E. amygdalina")), expression(italic("E. obliqua")), expression(italic("E. ovata")), expression(italic("E. viminalis"))), pch=19, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], cex = 4)
dev.off()

#with underlying points
dev.off()
pdf("Output/rgr~sla+points.pdf", width=21, height=21)
par(oma=c(8,8,1,1), mgp=c(0,3,0), mar=c(3, 3, 1, 1))
plot(mean_RGRpredrought ~ mean_sla_sp, col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], 
     ylab="", ylim=c(0,18), xlim=c(70,190), 
     xlab="", tck=-0.01, pch=19, cex=5, cex.axis=4, bty="n", meandata)
points(RGR_predrought ~ mean_sla_plant, cex=3, pch=19, col = alpha(c('red', 'blue', 'forestgreen', 'purple')[as.factor(solowatereddata$Species)], 0.4), solowatereddata)
mtext("Mean RGR pre-drought (mm/day)", side=2, outer=T, cex=4, line=4)
mtext("Mean specific leaf area (cm^2/g)", side=1, outer=T, cex=4, line=5)
arrows(x0=meandata$mean_sla_sp, y0=meandata$lower_rgr,
       x1=meandata$mean_sla_sp, y1=meandata$upper_rgr, code=3, cex = 4, angle=90, length=0.1, lwd=5)
arrows(x0=meandata$lower_sla, y0=meandata$mean_RGRpredrought,
       x1=meandata$upper_sla, y1=meandata$mean_RGRpredrought, code=3, angle=90, cex = 4, length=0.1, lwd=5)
box(bty="l", lwd=4)
points(mean_RGRpredrought ~ mean_sla_sp, pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], meandata)
legend("bottomleft", inset = 0.05, title = "Species", c(expression(italic("E. amygdalina")), expression(italic("E. obliqua")), expression(italic("E. ovata")), expression(italic("E. viminalis"))), pch=19, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], cex = 4)
dev.off()

dev.off()
pdf("Output/rgr~wd.pdf", width=21, height=21)
par(oma=c(8,8,1,1), mgp=c(0,3,0), mar=c(3, 3, 1, 1))
plot(mean_RGRpredrought ~ mean_wd_sp, col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], 
     ylab="", ylim=c(0,18), xlim=c(0.45,0.52), 
     xlab="", tck=-0.01, pch=19, cex=5, cex.axis=4, bty="n", meandata)
mtext("Mean RGR pre-drought (mm/day)", side=2, outer=T, cex=4, line=4)
mtext("Mean wood density (g/cm^3)", side=1, outer=T, cex=4, line=5)
arrows(x0=meandata$mean_wd_sp, y0=meandata$lower_rgr,
       x1=meandata$mean_wd_sp, y1=meandata$upper_rgr, code=3, cex = 4, angle=90, length=0.1, lwd=5)
arrows(x0=meandata$lower_wd, y0=meandata$mean_RGRpredrought,
       x1=meandata$upper_wd, y1=meandata$mean_RGRpredrought, code=3, angle=90, cex = 4, length=0.1, lwd=5)
points(mean_RGRpredrought ~ mean_wd_sp, pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], meandata)
box(bty="l", lwd=4)
legend("bottomleft", inset = 0.05, title = "Species", c(expression(italic("E. amygdalina")), expression(italic("E. obliqua")), expression(italic("E. ovata")), expression(italic("E. viminalis"))), pch=19, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], cex = 4)
dev.off()

### Figure 5 - traits ####
dev.off()
pdf("Output/traits_panel.pdf", width=21, height=21)
par(mfrow=c(2,2), oma=c(8,8,1,1), mgp=c(0,3,0), mar=c(3, 3, 7, 10))
#ttm ~ wd
plot(mean_tt100m ~ mean_wd_sp, col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], 
     ylab="", ylim=c(0,5), xlim=c(0.45,0.52), 
     xlab="", tck=-0.01, pch=19, cex=5, cex.axis=4, bty="n", meandata)
mtext("Mean time to 100% mortality (wks)", side=2, outer=F, cex=3.5, line=7)
mtext("Mean wood density (g/cm^3)", side=1, outer=F, cex=3.5, line=7)
arrows(x0=meandata$mean_wd_sp, y0=meandata$lower_ttm100,
       x1=meandata$mean_wd_sp, y1=meandata$upper_ttm100, code=3, cex = 4, angle=90, length=0.1, lwd=5)
arrows(x0=meandata$lower_wd, y0=meandata$mean_tt100m,
       x1=meandata$upper_wd, y1=meandata$mean_tt100m, code=3, angle=90, cex = 4, length=0.1, lwd=5)
points(mean_tt100m ~ mean_wd_sp, pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], meandata)
box(bty="l", lwd=4)
#ttm ~ wd
plot(mean_tt100m ~ mean_huber_sp, col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], 
     ylab="", ylim=c(0,5), xlim=c(20,60), 
     xlab="", tck=-0.01, pch=19, cex=5, cex.axis=4, bty="n", meandata)
mtext("Mean time to 100% mortality (wks)", side=2, outer=F, cex=3.5, line=7)
mtext("Mean Huber value", side=1, outer=F, cex=3.5, line=7)
arrows(x0=meandata$mean_huber_sp, y0=meandata$lower_ttm100,
       x1=meandata$mean_huber_sp, y1=meandata$upper_ttm100, code=3, cex = 4, angle=90, length=0.1, lwd=5)
arrows(x0=meandata$lower_huber, y0=meandata$mean_tt100m,
       x1=meandata$upper_huber, y1=meandata$mean_tt100m, code=3, angle=90, cex = 4, length=0.1, lwd=5)
points(mean_tt100m ~ mean_huber_sp, pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], meandata)
box(bty="l", lwd=4)
#rgr ~ wd
plot(mean_RGRpredrought ~ mean_wd_sp, col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], 
     ylab="", ylim=c(0,18), xlim=c(0.42,0.55), 
     xlab="", tck=-0.01, pch=19, cex=5, cex.axis=4, bty="n", meandata)
points(RGR_predrought ~ wd, cex=3, pch=19, col = alpha(c('red', 'blue', 'forestgreen', 'purple')[as.factor(solowatereddata$Species)], 0.4), solowatereddata)
mtext("Mean RGR pre-drought (mm/day)", side=2, outer=F, cex=3.5, line=7)
mtext("Mean wood density (g/cm^3)", side=1, outer=F, cex=3.5, line=7)
arrows(x0=meandata$mean_wd_sp, y0=meandata$lower_rgr,
       x1=meandata$mean_wd_sp, y1=meandata$upper_rgr, code=3, cex = 4, angle=90, length=0.1, lwd=5)
arrows(x0=meandata$lower_wd, y0=meandata$mean_RGRpredrought,
       x1=meandata$upper_wd, y1=meandata$mean_RGRpredrought, code=3, angle=90, cex = 4, length=0.1, lwd=5)
points(mean_RGRpredrought ~ mean_wd_sp, pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], meandata)
box(bty="l", lwd=4)
#rgr ~ SLA
plot(mean_RGRpredrought ~ mean_sla_sp, col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], 
     ylab="", ylim=c(0,18), xlim=c(70,190), 
     xlab="", tck=-0.01, pch=19, cex=5, cex.axis=4, bty="n", meandata)
points(RGR_predrought ~ mean_sla_plant, cex=3, pch=19, col = alpha(c('red', 'blue', 'forestgreen', 'purple')[as.factor(solowatereddata$Species)], 0.4), solowatereddata)
mtext("Mean RGR pre-drought (mm/day)", side=2, outer=F, cex=3.5, line=7)
mtext("Mean specific leaf area (cm^2/g)", side=1, outer=F, cex=3.5, line=7)
arrows(x0=meandata$mean_sla_sp, y0=meandata$lower_rgr,
       x1=meandata$mean_sla_sp, y1=meandata$upper_rgr, code=3, cex = 4, angle=90, length=0.1, lwd=5)
arrows(x0=meandata$lower_sla, y0=meandata$mean_RGRpredrought,
       x1=meandata$upper_sla, y1=meandata$mean_RGRpredrought, code=3, angle=90, cex = 4, length=0.1, lwd=5)
box(bty="l", lwd=4)
points(mean_RGRpredrought ~ mean_sla_sp, pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], meandata)
dev.off()

############# WITH legend
dev.off()
pdf("Output/traits_panel+legend.pdf", width=21, height=21)
par(mfrow=c(2,2), oma=c(15,8,1,1), mgp=c(0,2.5,0), mar=c(3, 3, 10, 12))
#ttm ~ wd
plot(mean_tt100m ~ mean_wd_sp, col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], 
     ylab="", ylim=c(0,5), xlim=c(0.45,0.52), 
     xlab="", tck=-0.01, pch=19, cex=5, cex.axis=4, bty="n", meandata)
mtext("Mean time to 100% mortality (wks)", side=2, outer=F, cex=3.5, line=7)
mtext("Mean wood density (g/cm^3)", side=1, outer=F, cex=3.5, line=7)
arrows(x0=meandata$mean_wd_sp, y0=meandata$lower_ttm100,
       x1=meandata$mean_wd_sp, y1=meandata$upper_ttm100, code=3, cex = 4, angle=90, length=0.1, lwd=5)
arrows(x0=meandata$lower_wd, y0=meandata$mean_tt100m,
       x1=meandata$upper_wd, y1=meandata$mean_tt100m, code=3, angle=90, cex = 4, length=0.1, lwd=5)
points(mean_tt100m ~ mean_wd_sp, pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], meandata)
box(bty="l", lwd=4)
mtext(expression(bold("A)")), side=1, cex=3.8, adj=0.01, padj=-11, line=-2)
#ttm ~ wd
plot(mean_tt100m ~ mean_huber_sp, col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], 
     ylab="", ylim=c(0,5), xlim=c(20,60), 
     xlab="", tck=-0.01, pch=19, cex=5, cex.axis=4, bty="n", meandata)
mtext("Mean time to 100% mortality (wks)", side=2, outer=F, cex=3.5, line=7)
mtext("Mean Huber value", side=1, outer=F, cex=3.5, line=7)
arrows(x0=meandata$mean_huber_sp, y0=meandata$lower_ttm100,
       x1=meandata$mean_huber_sp, y1=meandata$upper_ttm100, code=3, cex = 4, angle=90, length=0.1, lwd=5)
arrows(x0=meandata$lower_huber, y0=meandata$mean_tt100m,
       x1=meandata$upper_huber, y1=meandata$mean_tt100m, code=3, angle=90, cex = 4, length=0.1, lwd=5)
points(mean_tt100m ~ mean_huber_sp, pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], meandata)
box(bty="l", lwd=4)
mtext(expression(bold("B)")), side=1, cex=3.8, adj=0.005, padj= -11, line=-2)
#rgr ~ wd
plot(mean_RGRpredrought ~ mean_wd_sp, col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], 
     ylab="", ylim=c(0,18), xlim=c(0.42,0.55), 
     xlab="", tck=-0.01, pch=19, cex=5, cex.axis=4, bty="n", meandata)
points(RGR_predrought ~ wd, cex=3, pch=19, col = alpha(c('red', 'blue', 'forestgreen', 'purple')[as.factor(solowatereddata$Species)], 0.4), solowatereddata)
mtext("RGR pre-drought (mm/day)", side=2, outer=F, cex=3.5, line=7)
mtext("Wood density (g/cm^3)", side=1, outer=F, cex=3.5, line=7)
arrows(x0=meandata$mean_wd_sp, y0=meandata$lower_rgr,
       x1=meandata$mean_wd_sp, y1=meandata$upper_rgr, code=3, cex = 4, angle=90, length=0.1, lwd=5)
arrows(x0=meandata$lower_wd, y0=meandata$mean_RGRpredrought,
       x1=meandata$upper_wd, y1=meandata$mean_RGRpredrought, code=3, angle=90, cex = 4, length=0.1, lwd=5)
points(mean_RGRpredrought ~ mean_wd_sp, pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], meandata)
box(bty="l", lwd=4)
mtext(expression(bold("C)")), side=1, cex=3.8, adj=0.005, padj= -11, line=-2)
#rgr ~ SLA
plot(mean_RGRpredrought ~ mean_sla_sp, col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], 
     ylab="", ylim=c(0,18), xlim=c(70,190), 
     xlab="", tck=-0.01, pch=19, cex=5, cex.axis=4, bty="n", meandata)
points(RGR_predrought ~ mean_sla_plant, cex=3, pch=19, col = alpha(c('red', 'blue', 'forestgreen', 'purple')[as.factor(solowatereddata$Species)], 0.4), solowatereddata)
mtext("RGR pre-drought (mm/day)", side=2, outer=F, cex=3.5, line=7)
mtext("Specific leaf area (cm^2/g)", side=1, outer=F, cex=3.5, line=7)
arrows(x0=meandata$mean_sla_sp, y0=meandata$lower_rgr,
       x1=meandata$mean_sla_sp, y1=meandata$upper_rgr, code=3, cex = 4, angle=90, length=0.1, lwd=5)
arrows(x0=meandata$lower_sla, y0=meandata$mean_RGRpredrought,
       x1=meandata$upper_sla, y1=meandata$mean_RGRpredrought, code=3, angle=90, cex = 4, length=0.1, lwd=5)
box(bty="l", lwd=4)
points(mean_RGRpredrought ~ mean_sla_sp, pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], meandata)
mtext(expression(bold("D)")), side=1, cex=3.8, adj=0.005, padj= -11, line=-2)
#add legend beneath all of them
### Need this reset function before legend:
reset <- function() {
  par(mfrow=c(1, 1), oma=rep(0, 4), mar=rep(0, 4), new=TRUE)
  plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)
}
reset()
legend("bottom", horiz=T, title = NULL, c(expression(italic("E. amygdalina")), expression(italic("E. obliqua")), expression(italic("E. ovata")), expression(italic("E. viminalis"))), pch=19, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], cex = 3.5, bty="n")
dev.off()

### Stats - traits ####
#SPECIES DIFFERENCES
wdmod <- lm(wd ~ Species, sometraits)
wddharm <- simulateResiduals(wdmod)
plot(wddharm)
summary(wdmod)
emmeans(wdmod, list(pairwise ~ Species), adjust="tukey")

slamod <- lm(mean_sla_plant ~ Species, sometraits)
sladharm <- simulateResiduals(slamod)
plot(sladharm)
summary(slamod)
emmeans(slamod, list(pairwise ~ Species), adjust="tukey")

hubermod <- lm(mean_huber_plant ~ Species, sometraits)
huberdharm <- simulateResiduals(hubermod) 
plot(huberdharm)
summary(hubermod)
emmeans(hubermod, list(pairwise ~ Species), adjust="tukey")

tab_model(hubermod, slamod, wdmod)

#CORRELATIONS BETWEEN TRAITS AND RGR
rgrslamod <- lmer(sqrt(RGR_predrought) ~ mean_sla_plant + (1|Species), solowatereddata)
summary(rgrslamod)
dharm <- simulateResiduals(rgrslamod) 
plot(dharm)
r.squaredGLMM(rgrslamod)

ggplot(solowatereddata, aes(x = mean_sla_plant, y = sqrt(RGR_predrought)))+
  geom_point(alpha=0.4)+
  geom_smooth(method="lm")+
  theme_classic()

rgrwdmod <- lmer(sqrt(RGR_predrought) ~ wd + (1|Species), solowatereddata)
summary(rgrwdmod)
dharm <- simulateResiduals(rgrwdmod) 
plot(dharm)
r.squaredGLMM(rgrwdmod)
#plot
ggplot(solowatereddata, aes(x = wd, y = sqrt(RGR_predrought)))+
  geom_point(alpha=0.4)+
  geom_smooth(method="lm")+
  theme_classic()

#CORRELATIONS BETWEEN TRAITS AND TTM and RGR
ttmwdmod <- lm(mean_tt100m ~ mean_wd_sp, speciestraits)
summary(ttmwdmod)
dharm <- simulateResiduals(ttmwdmod) 
plot(dharm)
r.squaredGLMM(ttmwdmod)

ttmwdmod2 <- lm(tt100m ~ mean_wd_sp, solodroughtdata)
summary(ttmwdmod2)
dharm <- simulateResiduals(ttmwdmod2) 
plot(dharm)
r.squaredGLMM(ttmwdmod2)

ttmwdmod3 <- lm(mean_tt100m ~ wd, solowatereddata)
summary(ttmwdmod3)
dharm <- simulateResiduals(ttmwdmod3) 
plot(dharm)
r.squaredGLMM(ttmwdmod3)

ttmhubermod <- lm(mean_tt100m ~ mean_huber_sp, speciestraits)
summary(ttmhubermod)
dharm <- simulateResiduals(ttmhubermod) 
plot(dharm)
r.squaredGLMM(rgrwdmod)
