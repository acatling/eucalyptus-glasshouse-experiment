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
hist((alldata$Height_1907))
hist(sqrt(alldata$gr_predrought))
hist(alldata$gr_duringdrought)
hist(sqrt(alldata$gr_overall))
hist(sqrt(controldata$gr_overall))
hist(sqrt(alldata$tt100m))
hist(alldata$tt50m)

### Does growth rate change as a function of size? ####
sizegrowthmod <- lmer(sqrt(gr_predrought) ~ Height_1907 + (1|Species) + (1|Composition) + (1|Pot_number), alldata)
dharm <- simulateResiduals(sizegrowthmod)
plot(dharm)
#Bad residuals :/
summary(sizegrowthmod)

ggplot(alldata, aes(x = Height_1907, y= sqrt(gr_predrought), colour=Species))+
  geom_point(alpha=0.3)+
  geom_smooth(method="lm")+
  ylab("Height growth (mm/day)")+
  xlab("Initial height (mm)")+
  theme_classic()

ggplot(solodata, aes(x = Height_1907, y= sqrt(gr_predrought), colour=Species))+
  geom_point(alpha=0.7)+
  geom_smooth(method="lm")+
  ylab("Height growth (mm/day)")+
  xlab("Initial height (mm)")+
  theme_classic()
#Yes, growth rate increases with initial height...

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

### Extract 95% CI using emmeans for plotting
emmeans50m <- emmeans(mort50mod, "Species")
emmeans_CIs <- confint(emmeans50m, type = "response")
mort50_emmeans <- as.data.frame(emmeans_CIs)

mort100mod <- lm(tt100m ~ Species, solodroughtdata)
summary(mort100mod)
mortdharm <- simulateResiduals(mort100mod)
plot(mortdharm)
r.squaredGLMM(mort100mod)
emmeans(mort100mod, list(pairwise ~ Species), adjust="tukey")

### Extract 95% CI using emmeans for plotting
emmeans100m <- emmeans(mort100mod, "Species")
emmeans_CIs <- confint(emmeans100m, type = "response")
mort100_emmeans <- as.data.frame(emmeans_CIs)


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
  ggplot(aes(x = Species, y = sqrt(gr_duringdrought), colour = C_or_D))+
  geom_boxplot()+
  geom_jitter(width=0.05, height=0.05)+
  theme_classic()
#14 days
alldata %>% filter(!(Species == '?')) %>%
  ggplot(aes(x = Species, y = gr_predrought))+
  geom_boxplot()+
  geom_jitter()+
  theme_classic()
#14 days
alldata %>% filter(!(Species == '?')) %>%
  ggplot(aes(x = Species, y = gr_overall))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4)+
  theme_classic()
#gr overall for well-watered plants only
alldata %>% filter(!(Species == '?')) %>% filter(C_or_D == 'C') %>%
  ggplot(aes(x = Species, y = gr_overall))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4)+
  theme_classic()
#For solo plants only
  ggplot(solodata, aes(x = Species, y = sqrt(gr_overall)))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4)+
  theme_classic()
#For solo well-watered plants only -- too few 
solodata %>% filter(C_or_D == "C") %>%
  ggplot(aes(x = Species, y = sqrt(gr_overall)))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4, width=0.05)+
  theme_classic()
#Solo plants before drought? May be best
ggplot(solodata, aes(x = Species, y = sqrt(gr_predrought)))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4, width=0.05)+
  theme_classic()
#Has the same trend as solo well-watered plant, so I think solo plants before
#drought makes sense

#One-way ANOVA to see if there are differences in growth rate between species
modelgr1 <- aov(sqrt(gr_predrought) ~ Species, solodata)
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
#but it definitely did, from gr_duringdrought
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
ggplot(solodata, aes(x = Species, y = gr_predrought))+
  geom_jitter(alpha=0.2, width=0.05)+
  geom_point(stat="summary", fun.y="mean_se",size=2) +
  geom_errorbar(stat="summary", fun.data="mean_se",width=0,size=0.8)+
  ylab("gr pre-drought (mm/day)")+
  theme_classic()

#Check this pot - is it amyg or vimi? 100 
test <- alldata %>% filter(Species=="AMYG" & Composition == "VIMI-D")

## Stats - growth rate model (fig 1) ####
#Seven weeks before drought
solodata$Species <- factor(solodata$Species, levels = c("AMYG", "OBLI", "OVAT", "VIMI"))
growthmod <- lm(sqrt(gr_predrought) ~ Species + Height_1907, solodata)
summary(growthmod)
growthdharm <- simulateResiduals(growthmod)
plot(growthdharm)
r.squaredGLMM(growthmod)
emmeans(growthmod, list(pairwise ~ Species), adjust="tukey")

ggplot(solodata, aes(x = Species, y = sqrt(gr_predrought)))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4, width=0.05)+
  theme_classic()

ggplot(solodata, aes(x = Species, y = gr_predrought))+
 # geom_boxplot()+
  ylim(-0.1,10)+
  geom_jitter(alpha = 0.4, width=0.05)+
  theme_classic()

### Extract 95% CI using emmeans for plotting
testemmeans <- emmeans(growthmod, "Species")
#confint(testemmeans)
#type = response allows us to backtransform data from same scale used in model
emmeans_CIs <- confint(testemmeans, type = "response")
#Convert to dataframe so that we can use marginal means and CIs for plotting
growth_emmeans <- as.data.frame(emmeans_CIs)
#spgrowthforplot <- speciestraits %>% select(Species)
#spgrowthforplot$emmeans_lower <- emmeans_CIs[,5]
#spgrowthforplot$emmeans_upper<- emmeans_CIs[,6]

#Over the drought period (seven weeks)
growthmod2 <- lm(sqrt(gr_duringdrought) ~ Species + C_or_D + Height_0209, solodata)
summary(growthmod2)
growthdharm2 <- simulateResiduals(growthmod2)
plot(growthdharm2)
r.squaredGLMM(growthmod2)
#way more comparisons than needed here, probably need to do species specific models
# but clearly differences between C and D for all species
emmeans(growthmod2, list(pairwise ~ Species:C_or_D), adjust="tukey")


growthmod2 <- lmer(sqrt(gr_duringdrought) ~ C_or_D + Height_0209 + (1|Species), solodata)
summary(growthmod2)
growthdharm2 <- simulateResiduals(growthmod2)
plot(growthdharm2)
r.squaredGLMM(growthmod2)

growthmod3 <- lm(sqrt(gr_duringdrought) ~ Species + Height_0209, solowatereddata)
summary(growthmod3)
growthdharm2 <- simulateResiduals(growthmod2)
plot(growthdharm2)
r.squaredGLMM(growthmod2)

### Plotting for supplement
#try making row for species:C_or_D
solodata$specieswatering <- paste(solodata$Species,solodata$C_or_D)
dev.off()
pdf("Output/supp_control_drought_growth.pdf", width=10, height=5)
group.colours <- c("C" = "cornflowerblue", "D" = "red2")
  ggplot(data=solodata, aes(x = Species, y = sqrt(gr_duringdrought), colour = C_or_D))+
  geom_boxplot(data=solodata)+
  ylab("Height growth rate (mm/day, sqrt)")+
  geom_point(data=solodata, position=position_jitterdodge(), alpha = 0.7)+
  scale_colour_manual(values = c("C" = "cornflowerblue", "D" = "red2"))+
  scale_x_discrete(labels=c(expression(italic("E. amygdalina")), 
                            expression(italic("E. obliqua")), expression(italic("E. ovata")), 
                            expression(italic("E. viminalis"))))+
  scale_colour_manual(values = group.colours, name = "Watering treatment", 
                      labels = c("Well-watered", "Drought"))+
  theme_classic()+
    theme(legend.background = NULL, axis.text=element_text(size=16), 
          axis.title=element_text(size=16), legend.text=element_text(size=16),
          legend.title=element_text(size=16))
dev.off()

## Stats - fig 2 - trade-off between drought tolerance and growth rate ####
#regression
tradeoff <- lm(tt100m ~ sqrt(gr_predrought) + Species, solodroughtdata)
summary(tradeoff)
growthdharm <- simulateResiduals(tradeoff)
plot(growthdharm)
r.squaredGLMM(tradeoff)

tradeoff2 <- lmer(tt100m ~ sqrt(gr_predrought) + (1|Species), solodroughtdata)
summary(tradeoff2)
growthdharm <- simulateResiduals(tradeoff2)
plot(growthdharm)
r.squaredGLMM(tradeoff2)

tradeoff3 <- lm(tt100m ~ sqrt(gr_predrought), solodroughtdata)
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
#### Figure 1 - ggplot old - ttm and growth rates of species ####
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
  geom_text(data=meandata, aes(x=Species, y=5.6, label=c('ab', 'ab', 'a', 'b')), size =6)+
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=16),
        axis.text.x = element_text(face = "italic"))
#50m
b <- ggplot() + 
  geom_jitter(data=solodroughtdata, aes(x=Species, y=tt50m), alpha = 0.2, col = "grey10", width=0.1, cex = 2.5) + 
  geom_point(data=meandata, aes(x=Species, y=mean_tt50m), cex=3)+
  geom_errorbar(data=meandata, aes(x=Species, ymin = lower_ttm50, ymax = upper_ttm50, width = 0.15), cex=1)+
  geom_text(data=meandata, aes(x=Species, y=5.3, label=c('a', 'a', 'a', 'a')), size =6)+
  ylab("Time to 50% leaf mortality\n(weeks)")+
  theme_classic()+
  ylim(0, 5.3)+
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=16),
        axis.title.x=element_blank(),
        axis.text.x=element_blank())
#growth rate
a <- ggplot()+
  geom_jitter(data=solodata, aes(x = Species, y = sqrt(gr_predrought)), alpha = 0.2, col = "grey10", width=0.1, cex = 2.5)+
  geom_point(data=meandata, aes(x=Species, y=sqrt(mean_grpredrought)), cex=3) +
  geom_errorbar(data=meandata, aes(x=Species, ymin = sqrt(lower_gr), ymax = sqrt(upper_gr), width = 0.15), cex=1)+
  geom_text(data=meandata, aes(x=Species, y=5.6, label=c('a', 'a', 'a', 'a')), size =6)+
  ylab("sqrt(height growth rate) \n(mm/day)")+
  theme_classic()+
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=16),
        axis.title.x=element_blank(),
        axis.text.x=element_blank())

#Organise them as a three panel with common species labels
#using cowplot
pdf("Output/figure_1.pdf", width=8, height=9)
cowplot::plot_grid(a,b,c, align="hv", ncol=1, labels = c('A)', 'B)', 'C)'), hjust=-4)
dev.off()

#as boxplots instead:
a <- ggplot(solodata, aes(x = Species, y = sqrt(gr_predrought)))+
  geom_boxplot(cex=0.6, outlier.size=2.5) + 
  geom_jitter(alpha=0.2, width=0.05)+
  ylab("sqrt(gr (mm/day))")+
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

#### Figure 1 - new using emmeans ####

c <- ggplot() + 
  geom_jitter(data=solodroughtdata, aes(x=Species, y=tt100m), alpha=0.2, col = "grey10", width=0.1, cex = 2.5) + 
  geom_point(data=mort100_emmeans, aes(x=Species, y=emmean), cex=3)+
  geom_errorbar(data=mort100_emmeans, aes(x=Species, ymin = lower.CL, ymax = upper.CL, width = 0.15), cex=1)+
  ylab("Time to 100% leaf mortality\n(weeks)")+
  theme_classic()+
  ylim(0, 5.7)+
  scale_x_discrete(labels=c("E. amygdalina", "E. obliqua", "E. ovata", "E. viminalis"))+
  geom_text(data=mort100_emmeans, aes(x=Species, y=5.7, label=c('ab', 'ab', 'a', 'b')), size =6)+
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=16),
        axis.text.x = element_text(face = "italic"))
#50m
b <- ggplot() + 
  geom_jitter(data=solodroughtdata, aes(x=Species, y=tt50m), alpha = 0.2, col = "grey10", width=0.1, cex = 2.5) + 
  geom_point(data=mort50_emmeans, aes(x=Species, y=emmean), cex=3)+
  geom_errorbar(data=mort50_emmeans, aes(x=Species, ymin = lower.CL, ymax = upper.CL, width = 0.15), cex=1)+
  geom_text(data=mort50_emmeans, aes(x=Species, y=5.3, label=c('a', 'a', 'a', 'a')), size =6)+
  ylab("Time to 50% leaf mortality\n(weeks)")+
  theme_classic()+
  ylim(0, 5.3)+
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=16),
        axis.title.x=element_blank(),
        axis.text.x=element_blank())
#growth rate
a <- ggplot()+
  geom_jitter(data=solodata, aes(x = Species, y = gr_predrought), alpha = 0.2, col = "grey10", width=0.1, cex = 2.5)+
  geom_point(data=growth_emmeans, aes(x=Species, y=response), cex=3) +
  geom_errorbar(data=growth_emmeans, aes(x=Species, ymin = lower.CL, ymax = upper.CL, width = 0.15), cex=1)+
  geom_text(data=growth_emmeans, aes(x=Species, y=10, label=c('a', 'a', 'a', 'a')), size =6)+
  ylab("Height growth rate \n(mm/day)")+
  ylim(-0.01, 10)+
  theme_classic()+
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=16),
        axis.title.x=element_blank(),
        axis.text.x=element_blank())

pdf("Output/figure_1_new.pdf", width=8, height=9)
cowplot::plot_grid(a,b,c, align="hv", ncol=1, labels = c('A)', 'B)', 'C)'), hjust=-5.5)
dev.off()

#### Figure 4 - ttm against growth rate ####
#meandata using SD
plot(mean_tt100m ~ mean_grpredrought, ylab="Mean time to 100% leaf mortality (weeks)", ylim=c(0,6), xlim=c(0,30), 
     xlab="Mean gr pre-drought (mm/day)", tck=-0.02, pch=19, cex=1.2, bty="l", meandata)
#add error bars
#y axis 
arrows(x0=meandata$mean_grpredrought, y0=meandata$mean_tt100m-meandata$sd_tt100m,
       x1=meandata$mean_grpredrought, y1=meandata$mean_tt100m+meandata$sd_tt100m, code=3, angle=90, length=0.1)
#x axis
arrows(x0=meandata$mean_grpredrought-meandata$sd_grpredrought, y0=meandata$mean_tt100m,
       x1=meandata$mean_grpredrought+meandata$sd_grpredrought, y1=meandata$mean_tt100m, code=3, angle=90, length=0.1)

#mgp c(left, bottom, title) but this does the same thing to both axes
#have to use mgp.axis.labels(x,y) to set them separately
#mgp=c(0,3.5,0) is perfect for x
dev.off()
pdf("Output/ttm~gr.pdf", width=21, height=21)
par(oma=c(8,8,1,1), mgp=c(0,3,0), mar=c(3, 3, 1, 1))
#mgp.axis.labels(3.5, type='x')
#mgp.axis.labels(100, type='y')
#meandata using 95% CIs
#colour by species
#sqrt gr* do this
#plot(meandata$mean_tt100m, meandata$mean_grpredrought, col=as.factor(meandata$Species))
#both of these work:
#col=c('red', 'blue', 'green', 'purple')[as.factor(meandata$Species)]
#col=as.factor(meandata$Species)
#col=as.factor(Species)
#col= alpha("lightgrey",0.6)

#cols <- c("red", "blue", "darkgreen", "purple")
plot(mean_tt100m ~ sqrt(mean_grpredrought), col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], 
     ylab="", ylim=c(0,5), xlim=c(0,5), 
     xlab="", tck=-0.01, pch=19, cex=4, cex.axis=5, bty="n", meandata)
#Add axis labels
mtext("Mean time to 100% mortality (weeks)", side=2, outer=T, cex=5, line=4)
mtext("sqrt(Mean gr pre-drought (mm/day))", side=1, outer=T, cex=5, line=5)
#add error bars
#y axis 
arrows(x0=sqrt(meandata$mean_grpredrought), y0=meandata$lower_ttm100,
       x1=sqrt(meandata$mean_grpredrought), y1=meandata$upper_ttm100, code=3, cex = 4, angle=90, length=0.1, lwd=5)
#x axis
arrows(x0=sqrt(meandata$lower_gr), y0=meandata$mean_tt100m,
       x1=sqrt(meandata$upper_gr), y1=meandata$mean_tt100m, code=3, angle=90, cex = 4, length=0.1, lwd=5)
box(bty="l", lwd=4)
points(mean_tt100m ~ sqrt(mean_grpredrought), pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], meandata)
#add legend
legend("bottomleft", inset = 0.05, title = "Species", unique(meandata$Species), pch=19, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], cex = 4)
#add points underneath, jittered and coloured by species
#Doesn't make sense to have points underneath, that isn't how the data work
#points(jitter(tt100m, amount = 0.1) ~ jitter(sqrt(gr_predrought), amount = 0.1), 
#       col=alpha(c('red', 'blue', 'green', 'purple')[as.factor(meandata$Species)], 0.3), pch = 19, cex = 3, solodroughtdata)
dev.off()

### No square root gr
dev.off()
pdf("Output/ttm~gr_no_sqrt.pdf", width=21, height=21)
par(oma=c(8,8,1,1), mgp=c(0,3,0), mar=c(3, 3, 1, 1))
plot(mean_tt100m ~ mean_grpredrought, col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], 
     ylab="", ylim=c(0,5), xlim=c(0,18), 
     xlab="", tck=-0.01, pch=19, cex=5, cex.axis=5, bty="n", meandata)
mtext("Mean time to 100% leaf mortality (weeks)", side=2, outer=T, cex=5, line=4)
mtext("Mean height growth rate (mm/day)", side=1, outer=T, cex=5, line=5)
arrows(x0=meandata$mean_grpredrought, y0=meandata$lower_ttm100,
       x1=meandata$mean_grpredrought, y1=meandata$upper_ttm100, code=3, cex = 4, angle=90, length=0.1, lwd=5)
arrows(x0=meandata$lower_gr, y0=meandata$mean_tt100m,
       x1=meandata$upper_gr, y1=meandata$mean_tt100m, code=3, angle=90, cex = 4, length=0.1, lwd=5)
box(bty="l", lwd=4)
points(mean_tt100m ~ mean_grpredrought, pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], meandata)
legend("bottomleft", inset = 0.05, title = "Species", c(expression(italic("E. amygdalina")), expression(italic("E. obliqua")), expression(italic("E. ovata")), expression(italic("E. viminalis"))), pch=19, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], cex = 4)
dev.off()

### Trying again with emmeans data
#growth_emmeans
#mort100_emmeans
mort100emmeans <- mort100_emmeans %>% select(Species, mort_mean = emmean, mort_lower = lower.CL, mort_upper = upper.CL)
growthemmeans <- growth_emmeans %>% select(Species, growth_mean = response, growth_lower = lower.CL, growth_upper = upper.CL)
#merge
ttm_growth_emmeans <- left_join(mort100emmeans, growthemmeans)

dev.off()
pdf("Output/ttm~gr_no_sqrt.pdf", width=21, height=21)
par(oma=c(8,8,1,1), mgp=c(0,3,0), mar=c(3, 3, 1, 1))
plot(mort_mean ~ growth_mean, col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(ttm_growth_emmeans$Species)], 
     ylab="", ylim=c(0,7), xlim=c(0,7), 
     xlab="", tck=-0.01, pch=19, cex=5, cex.axis=5, bty="n", ttm_growth_emmeans)
mtext("Mean time to 100% leaf mortality (weeks)", side=2, outer=T, cex=5, line=4)
mtext("Mean height growth rate (mm/day)", side=1, outer=T, cex=5, line=5)
arrows(x0=ttm_growth_emmeans$growth_mean, y0=ttm_growth_emmeans$mort_lower,
       x1=ttm_growth_emmeans$growth_mean, y1=ttm_growth_emmeans$mort_upper, code=3, cex = 4, angle=90, length=0.1, lwd=5)
arrows(x0=ttm_growth_emmeans$growth_lower, y0=ttm_growth_emmeans$mort_mean,
       x1=ttm_growth_emmeans$growth_upper, y1=ttm_growth_emmeans$mort_mean, code=3, angle=90, cex = 4, length=0.1, lwd=5)
box(bty="l", lwd=4)
points(mort_mean ~ growth_mean, pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(ttm_growth_emmeans$Species)], ttm_growth_emmeans)
legend("topright", inset = 0.05, title = "Species", c(expression(italic("E. amygdalina")), expression(italic("E. obliqua")), expression(italic("E. ovata")), expression(italic("E. viminalis"))), pch=19, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(ttm_growth_emmeans$Species)], cex = 4)
dev.off()

#### Watered growth rates with nbhs means and CIs ####
ggplot(pot_gr_cons, aes(x = Composition, y = mean_pot_gr_predrought))+
  geom_jitter(alpha=0.4, width=0.05)+
  theme_classic()+
  facet_wrap(~Species)
#Plotted as mean and sd
boxplot(mean_gr_comp ~ Composition, ylab="blah", ylim=c(0,20),
     xlab="yes", tck=-0.02, pch=19, cex=1.2, bty="l", alone_cons_gr)
###Using base R so I need to assign dummy x values to distribute along x axis
#alone_cons_gr$x <- c(0.5, 1, 2, 2.5, 3.5, 4)
#plot(mean_gr_comp ~ x, ylab="blah", ylim=c(0,20),
#     xlab="yes", tck=-0.02, pch=19, cex=1.2, bty="l", alone_cons_gr)

#Reordering Composition
pot_gr_cons$Composition <- factor(pot_gr_cons$Composition, level = c("AMYG-A", "AAAA", "OVAT-C", "CCCC", "VIMI-D", "DDDD"))
comp_gr_cons$Composition <- factor(comp_gr_cons$Composition, level = c("AMYG-A", "AAAA", "OVAT-C", "CCCC", "VIMI-D", "DDDD"))
##
a <- ggplot() + 
  geom_jitter(data=pot_gr_cons, aes(x=Composition, y=sqrt(mean_pot_gr_predrought)), alpha=0.3, width=0.05, height=0.05) + 
  geom_point(data=comp_gr_cons, aes(x=Composition, y=sqrt(mean_gr_comp)), cex=2)+
  geom_errorbar(data=comp_gr_cons, aes(x=Composition, ymin = sqrt(lower_gr_comp), ymax = sqrt(upper_gr_comp), width = 0.15), cex=1)+
  ylab("sqrt(gr pre-drought (mm/day))")+
  ylim(-0.1,7)+
  theme_classic()+
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=14))
#Note that one of the 0 grs for ovat-c pot 8 is legitimate, stopped growing
#growth rate
b <- ggplot() + 
  geom_jitter(data=pot_gr_hets, aes(x=compspecies, y=sqrt(mean_pot_gr_predrought)), alpha=0.3, width=0.05, height=0.05) + 
  geom_point(data=comp_gr_hets, aes(x=compspecies, y=sqrt(mean_gr_comp)), cex=2)+
  geom_errorbar(data=comp_gr_hets, aes(x=compspecies, ymin = sqrt(lower_gr_comp), ymax = sqrt(upper_gr_comp), width = 0.15), cex=1)+
  ylab("sqrt(gr pre-drought (mm/day))")+
  ylim(-0.1,7)+
  xlab("Composition_Species")+
  theme_classic()+
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=14))
#Plot together
cowplot::plot_grid(a,b, align="hv", ncol=1, labels = c('A)', 'B)'), hjust=-3)
### as boxplots instead ###
a <- ggplot() + 
  geom_boxplot(data=pot_gr_cons, aes(x=Composition, y=sqrt(mean_pot_gr_predrought)), cex=0.6, outlier.size=2.5) + 
  geom_jitter(data=pot_gr_cons, aes(x=Composition, y=sqrt(mean_pot_gr_predrought)), alpha=0.2, cex=1.5, width=0.05) + 
  ylab("sqrt(gr pre-drought (mm/day))")+
  ylim(-0.1,7)+
  theme_classic()+
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=14))
#growth rate
b <- ggplot() + 
  geom_boxplot(data=pot_gr_hets, aes(x=compspecies, y=sqrt(mean_pot_gr_predrought)), cex=0.6, outlier.size=2.5) + 
  geom_jitter(data=pot_gr_hets, aes(x=compspecies, y=sqrt(mean_pot_gr_predrought)), alpha=0.3, width=0.05, height=0.05) + 
  ylab("sqrt(gr pre-drought (mm/day))")+
  ylim(-0.1,7)+
  xlab("Composition_Species")+
  theme_classic()+
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=14))
cowplot::plot_grid(a,b, align="hv", ncol=1, labels = c('A)', 'B)'), hjust=-3)

#### Figure not used - grs for OBLI B, BB, AB, BC, BD ####
obli_comp_avg <- within(obli_comp_avg, compspecies[compspecies=="OBLI-B_OBLI"] <- 'OBLI-B')
obli_comp_avg <- within(obli_comp_avg, compspecies[compspecies=="BB_OBLI"] <- 'BB')
oblicompgrmeans <- within(oblicompgrmeans, compspecies[compspecies=="OBLI-B_OBLI"] <- 'OBLI-B')
oblicompgrmeans <- within(oblicompgrmeans, compspecies[compspecies=="BB_OBLI"] <- 'BB')

oblicompgrmeans$compspecies <- factor(oblicompgrmeans$compspecies, level = c("OBLI-B", 
                                          "BB", "AB_OBLI", "AB_AMYG", "BC_OBLI", "BC_OVAT",
                                          "BD_OBLI", "BD_VIMI"))
obli_comp_avg$compspecies <- factor(obli_comp_avg$compspecies, 
                                    level = c("OBLI-B", 
                  "BB", "AB_OBLI", "AB_AMYG", "BC_OBLI", "BC_OVAT",
                  "BD_OBLI", "BD_VIMI"))
#means with CIs
ggplot() + 
  geom_jitter(data=obli_comp_avg, aes(x=compspecies, y=sqrt(mean_pot_gr_predrought)), alpha=0.3, width=0.05, height=0.05) + 
  geom_point(data=oblicompgrmeans, aes(x=compspecies, y=sqrt(mean_gr_comp)), cex=2)+
  geom_errorbar(data=oblicompgrmeans, aes(x=compspecies, ymin = sqrt(lower_gr_comp), ymax = sqrt(upper_gr_comp), width = 0.15), cex=1)+
  ylab("sqrt(gr pre-drought (mm/day))")+
  #ylim(-0.1,7)+
  theme_classic()
#boxplot
ggplot() + 
  geom_boxplot(data=obli_comp_avg, aes(x=compspecies, y=sqrt(mean_pot_gr_predrought)), outlier.size=3)+
  geom_point(data=obli_comp_avg, aes(x=compspecies, y=sqrt(mean_pot_gr_predrought)), alpha=0.3, cex = 3) + 
  ylab("sqrt(gr pre-drought (mm/day))")+
  ylim(-0.1,7)+
  xlab("Composition_Species")+
  theme_classic()+
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=16))

### (fig 2 not used) coef plot of growth rates with nbhs ####
#recall that labels are inverted
specieslist <- c("AMYG", "OVAT", "VIMI")
growthmods <- c(amygcompmod, ovatcompmod, vimicompmod)
plot_models(growthmods, transform = NULL, vline.color = "grey", legend.title = "Species",
            dot.size = 2, line.size = 1)+
  ylab("Effect size")+
  theme_classic()+
  scale_colour_discrete(labels = c("VIMI", "OVAT", "AMYG"))

###with obli too
#need to rework amyg/ovat/vimi models to include AB, BC and BD responses
amygcompdata2 <- alldata %>% filter(Species == "AMYG")
amygcompdata2$Composition <- factor(amygcompdata2$Composition, levels = c("AMYG-A", "AAAA", "AB", "AACC", "AADD"))
amygcompmod2 <- lmer(sqrt(gr_predrought) ~ Composition + (1|Pot_number), amygcompdata2)
summary(amygcompmod2)
ovatcompdata2 <- alldata %>% filter(Species == "OVAT")
ovatcompdata2$Composition <- factor(ovatcompdata2$Composition, levels = c("OVAT-C", "CCCC", "BC", "AACC", "CCDD"))
ovatcompmod2 <- lmer(sqrt(gr_predrought) ~ Composition + (1|Pot_number), ovatcompdata2)
summary(ovatcompmod2)
vimicompdata2 <- alldata %>% filter(Species == "VIMI")
vimicompdata2$Composition <- factor(vimicompdata2$Composition, levels = c("VIMI-D", "DDDD", "AADD", "BD", "CCDD"))
vimicompmod2 <- lmer(sqrt(gr_predrought) ~ Composition + (1|Pot_number), vimicompdata2)
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
amygcompdata2 <- within(amygcompdata2, Composition[Composition == "AACC"] <- 'amygdalina+ovata')
amygcompdata2 <- within(amygcompdata2, Composition[Composition == "AADD"] <- 'amygdalina+viminalis')
amygcompdata2$Composition <- factor(amygcompdata2$Composition, levels = c("alone", "conspecific", "amygdalina+ovata", "amygdalina+viminalis"))
amygcompmod3 <- lmer(sqrt(gr_predrought) ~ Composition + (1|Pot_number), amygcompdata2)

ovatcompdata2 <- alone_or_four %>% filter(Species=="OVAT")
ovatcompdata2 <- within(ovatcompdata2, Composition[Composition == "OVAT-C"] <- 'alone')
ovatcompdata2 <- within(ovatcompdata2, Composition[Composition == "CCCC"] <- 'conspecific')
ovatcompdata2 <- within(ovatcompdata2, Composition[Composition == "AACC"] <- 'amygdalina+ovata')
ovatcompdata2 <- within(ovatcompdata2, Composition[Composition == "CCDD"] <- 'ovata+viminalis')
ovatcompdata2$Composition <- factor(ovatcompdata2$Composition, levels = c("alone", "conspecific", "amygdalina+ovata", "ovata+viminalis"))
ovatcompmod3 <- lmer(sqrt(gr_predrought) ~ Composition + (1|Pot_number), ovatcompdata2)

vimicompdata2 <- alone_or_four %>% filter(Species=="VIMI")
vimicompdata2 <- within(vimicompdata2, Composition[Composition == "VIMI-D"] <- 'alone')
vimicompdata2 <- within(vimicompdata2, Composition[Composition == "DDDD"] <- 'conspecific')
vimicompdata2 <- within(vimicompdata2, Composition[Composition == "AADD"] <- 'amygdalina+viminalis')
vimicompdata2 <- within(vimicompdata2, Composition[Composition == "CCDD"] <- 'ovata+viminalis')
vimicompdata2$Composition <- factor(vimicompdata2$Composition, levels = c("alone", "conspecific", "amygdalina+viminalis", "ovata+viminalis"))
vimicompmod3 <- lmer(sqrt(gr_predrought) ~ Composition + (1|Pot_number), vimicompdata2)

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
amygcompdata3 <- within(amygcompdata3, Composition[Composition == "AB"] <- 'amygdalina+obliqua')
amygcompdata3 <- within(amygcompdata3, Composition[Composition == "AACC"] <- 'amygdalina+ovata')
amygcompdata3 <- within(amygcompdata3, Composition[Composition == "AADD"] <- 'amygdalina+viminalis')
amygcompdata3$Composition <- factor(amygcompdata3$Composition, levels = c("alone", "conspecific", "amygdalina+obliqua", "amygdalina+ovata", "amygdalina+viminalis"))
#this line makes everything that isn't the named compositions, e.g. ADDDD, NAs. good.
amygcompmod4 <- lmer(sqrt(gr_predrought) ~ Composition + Height_1907 + (1|Pot_number), amygcompdata3)

oblicompdata2 <- obli_comp
oblicompdata2 <- within(oblicompdata2, Composition[Composition == "OBLI-B"] <- 'alone')
oblicompdata2 <- within(oblicompdata2, Composition[Composition == "BB"] <- 'conspecific')
oblicompdata2 <- within(oblicompdata2, Composition[Composition == "AB"] <- 'amygdalina+obliqua')
oblicompdata2 <- within(oblicompdata2, Composition[Composition == "BC"] <- 'obliqua+ovata')
oblicompdata2 <- within(oblicompdata2, Composition[Composition == "BD"] <- 'obliqua+viminalis')
oblicompdata2$Composition <- factor(oblicompdata2$Composition, levels = c("alone", "conspecific", "amygdalina+obliqua", "obliqua+ovata", "obliqua+viminalis"))
oblicompmod4 <- lmer(sqrt(gr_predrought) ~ Composition + Height_1907 + (1|Pot_number), oblicompdata2)

ovatcompdata3 <- alldata %>% filter(Species=="OVAT")
ovatcompdata3 <- within(ovatcompdata3, Composition[Composition == "OVAT-C"] <- 'alone')
ovatcompdata3 <- within(ovatcompdata3, Composition[Composition == "CCCC"] <- 'conspecific')
ovatcompdata3 <- within(ovatcompdata3, Composition[Composition == "AACC"] <- 'amygdalina+ovata')
ovatcompdata3 <- within(ovatcompdata3, Composition[Composition == "BC"] <- 'obliqua+ovata')
ovatcompdata3 <- within(ovatcompdata3, Composition[Composition == "CCDD"] <- 'ovata+viminalis')
ovatcompdata3$Composition <- factor(ovatcompdata3$Composition, levels = c("alone", "conspecific", "amygdalina+ovata", "obliqua+ovata", "ovata+viminalis"))
ovatcompmod4 <- lmer(sqrt(gr_predrought) ~ Composition + Height_1907 + (1|Pot_number), ovatcompdata3)

vimicompdata3 <- alldata %>% filter(Species=="VIMI")
vimicompdata3 <- within(vimicompdata3, Composition[Composition == "VIMI-D"] <- 'alone')
vimicompdata3 <- within(vimicompdata3, Composition[Composition == "DDDD"] <- 'conspecific')
vimicompdata3 <- within(vimicompdata3, Composition[Composition == "AADD"] <- 'amygdalina+viminalis')
vimicompdata3 <- within(vimicompdata3, Composition[Composition == "BD"] <- 'obliqua+viminalis')
vimicompdata3 <- within(vimicompdata3, Composition[Composition == "CCDD"] <- 'ovata+viminalis')
vimicompdata3$Composition <- factor(vimicompdata3$Composition, levels = c("alone", "conspecific", "amygdalina+viminalis", "obliqua+viminalis", "ovata+viminalis"))
vimicompmod4 <- lmer(sqrt(gr_predrought) ~ Composition + Height_1907 + (1|Pot_number), vimicompdata3)

growthmods4 <- c(amygcompmod4, oblicompmod4, ovatcompmod4, vimicompmod4)

dev.off()
pdf("Output/figure_2+obli.pdf", width=8, height=6)
#show.p=T, p.shape=T
#function for backtransforming data - not working, also doesn't work for neg value
#square <- function(x){
#  return(x**2)
#}
plot_models(growthmods4, rm.terms = "Height_1907", transform = NULL, vline.color = "grey", legend.title = "Species",
            dot.size = 2, line.size = 1, ci.lvl=0.95)+
  ylab("Effect size")+
  xlab("Competition treatment")+
  theme_classic()+
  ylim(-1.9,1.9)+
  scale_colour_discrete(labels = c(expression(italic("E. viminalis")), expression(italic("E. ovata")), expression(italic("E. obliqua")), expression(italic("E. amygdalina"))))+
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=16),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16),
        legend.text.align = 0,
        legend.position = "right",
        legend.title.align = 0.5)
dev.off()

## Stats - Fig 2 - growth rate by composition ####
#By species, pot as a random effect, alone_or_four is for e.g. A or AAAA only
###amyg
#amygcompdata <- alone_or_four %>% filter(Species=="AMYG")
#Reorder levels so alone is the reference
#amygcompdata$Composition <- factor(amygcompdata$Composition, levels = c("AMYG-A", "AAAA", "AACC", "AADD"))

#Using data from previous section, fig. 2
amygcompmod <- lmer(sqrt(gr_predrought) ~ Composition + Height_1907 + (1|Pot_number), amygcompdata3)
summary(amygcompmod)
dharm <- simulateResiduals(amygcompmod)
plot(dharm)
r.squaredGLMM(amygcompmod)
#Only matters how they differ from reference, alone

#These two below lines do the same thing as above
# test <- emmeans(amygcompmod, ~Composition)
# pairs(test)
# #another method
# library(multcomp)
# summary(glht(amygcompmod, linfct = mcp(Composition = "Tukey")), test = adjusted("holm"))
#

##obli
oblicompmod <- lmer(sqrt(gr_predrought) ~ Composition + Height_1907 + (1|Pot_number), oblicompdata2)
dharm <- simulateResiduals(oblicompmod)
plot(dharm)
summary(oblicompmod)

#grouped hets
#oblicompmod2 <- lmer(sqrt(gr_predrought) ~ grouped_comp + (1|Pot_number), obli_comp)
# summary(oblicompmod2)
# dharm <- simulateResiduals(oblicompmod2)
# plot(dharm)
# r.squaredGLMM(oblicompmod2)
# tab_model(oblicompmod2)

###ovat
ovatcompmod <- lmer(sqrt(gr_predrought) ~ Composition + Height_1907 + (1|Pot_number), ovatcompdata3)
summary(ovatcompmod)
dharm <- simulateResiduals(ovatcompmod)
plot(dharm)
r.squaredGLMM(ovatcompmod)

###vimi
vimicompmod <- lmer(sqrt(gr_predrought) ~ Composition + Height_1907 + (1|Pot_number), vimicompdata3)
summary(vimicompmod)
dharm <- simulateResiduals(vimicompmod)
plot(dharm)
r.squaredGLMM(vimicompmod)

### Stats table Fig 2 - Supp Info 4 - by species ####
#sJplot
#Doesn't work - lose negative sign, not sure how to avoid this
#myfun <- function(x) x^2
#tab_model(amygcompmod, ovatcompmod, vimicompmod, transform = "myfun")

tab_model(amygcompmod4, oblicompmod4, ovatcompmod4, vimicompmod4, transform = NULL, 
          pred.labels = c('Intercept (alone)', 'conspecific', 'amygdalina+obliqua', 'amygdalina+ovata', 'amygdalina+viminalis', 'Initial height', 'obliqua+ovata', 'obliqua+viminalis', 'ovata+viminalis'),
          dv.labels = c('E. amygdalina', 'E. obliqua', 'E. ovata', 'E. viminalis'))

# ## Extracting values for all in a loop
# model_list <- list(amygcompmod, ovatcompmod, vimicompmod)
# effects = lapply(1:length(model_list), function(x) {
#   as.data.frame(coef(summary(model_list[[x]]))) %>% mutate(Species=paste0(x))})
# effects_table <- do.call("rbind", effects)
# 
# #Make rownames a column 
# effects_table <- cbind(Effect = rownames(effects_table), effects_table)
# #Remove rownames
# rownames(effects_table) <- NULL
# 
# #Renaming effects since loop adding values to ends
# effects_table$Effect[startsWith(effects_table$Effect, '(Intercept)')] <- 'Intercept'
# effects_table$Effect[startsWith(effects_table$Effect, 'CompositionAACC')] <- 'AACC'
# effects_table$Effect[startsWith(effects_table$Effect, 'CompositionAADD')] <- 'AADD'
# effects_table$Effect[startsWith(effects_table$Effect, 'CompositionCCDD')] <- 'CCDD'
# effects_table$Effect[startsWith(effects_table$Effect, 'CompositionAAAA')] <- 'AAAA'
# effects_table$Effect[startsWith(effects_table$Effect, 'CompositionCCCC')] <- 'CCCC'
# effects_table$Effect[startsWith(effects_table$Effect, 'CompositionDDDD')] <- 'DDDD'
# 
# effects_table <- within(effects_table, Species[Species == '1'] <- 'E. amygdalina')
# effects_table <- within(effects_table, Species[Species == '2'] <- 'E. ovata')
# effects_table <- within(effects_table, Species[Species == '3'] <- 'E. viminalis')
# #Renaming columns
# effects_table <- effects_table %>% select(Species, Effect, Estimate, 'SE' = 'Std. Error', 'p_value' = 'Pr(>|t|)')
# 
# #Making column with Estimate (+/- SE) and p value asterisks all combined
# #effects_table$collated <- sprintf("%1.1f ± %1.1f", effects_table$Estimate, effects_table$SE)
# 
# #Add column for asterisks based on below function
# effects_table <- effects_table %>% mutate(p_asterisks = case_when(p_value >=0.05~"",
#                                                                   p_value <0.001~"***",
#                                                                   p_value <0.01~"**",
#                                                                   p_value <0.05~"*"))
# effects_table$collated <- sprintf("%1.3f ± %1.2f%s", effects_table$Estimate, effects_table$SE, effects_table$p_asterisks)
# 
# growth_effects_kbl <- effects_table %>% select(Species, Effect, collated)
# 
# # #This made the table wider but some of the column aren't shared here so many NAs
# # growth_effects_kbl <- growth_effects_kbl %>% group_by(Species) %>% mutate(row = row_number()) %>%
# #   pivot_wider(names_from = Species, values_from = collated) %>% select(-row)
# 
# #Want to include RE info too
# #Split out each sp
# amygtable <- growth_effects_kbl %>% filter(Species == "E. amygdalina") %>% select(Effect, collated)
# #wider
# amyg_kbl <- amygtable %>% pivot_wider(names_from = Effect, values_from = collated)
# #Plotting with kableR
# #Marginal r squareds from below table
# #caption: <b>Table 2</b>. Model output with Estimate ± SE (standard error) for each species modelled separately. Marginal R-squared values reported. NCI is neighbourhood crowding index from all neighbours.
# # Mean MD is long-term mean moisture deficit summed over the growth period. MD anomaly is the difference between mean MD and observed MD. Preceding DBH is the size of each focal stem at the start of each growth period.
# # Low values of PC1 represent low soil fertility and conductivity.
# growth_effects_kbl %>% mutate(Effect = c("Intercept", "Total NCI", "Mean MD", "MD anomaly", "Preceding DBH", "PC1", "Total NCI:Mean MD", "Total NCI:Preceding DBH", "Mean MD:Preceding DBH", "test", "testing", "Tests")) %>%
#   kbl(align = 'lcccc', caption = "") %>%
#  # add_header_above(c(" "=1, "R^2=0.20"=1, "R^2=0.21"=1, "R^2=0.25"=1, "R^2=0.19"=1), align = c("l", "c", "c", "c", "c")) %>%
#   kable_classic(full_width = T, html_font = "Times", font_size = 12) %>%
#   kable_styling(font_size = 14) %>%
#  # row_spec(0, italic = T) %>%
#   footnote(general = "Continuous predictors are scaled to a mean of 0. Mean MD is 308.8 mm per growth period. Mean MD anomaly is -336.6 mm per growth period. Mean preceding DBH is 368.2 mm.", general_title="")

#### (fig 3 boxplots) - effect of nbhs on time to mortality means and CIs ####
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
### Figure 3 - coef plot mortality ~ nbhs ####
#recall that labels are inverted
amygttmcomp <- alone_or_four %>% filter(Species=="AMYG", C_or_D == "D")
amygttmcomp <- within(amygttmcomp, Composition[Composition == "AMYG-A"] <- 'alone')
amygttmcomp <- within(amygttmcomp, Composition[Composition == "AAAA"] <- 'conspecific')
amygttmcomp <- within(amygttmcomp, Composition[Composition == "AACC"] <- 'amygdalina+ovata')
amygttmcomp <- within(amygttmcomp, Composition[Composition == "AADD"] <- 'amygdalina+viminalis')
amygttmcomp$Composition <- factor(amygttmcomp$Composition, levels = c("alone", "conspecific", "amygdalina+ovata", "amygdalina+viminalis"))
amygttmmod <- lmer(tt100m ~ Composition + (1|Pot_number), amygttmcomp)

ovatttmcomp <- alone_or_four %>% filter(Species=="OVAT", C_or_D == "D")
ovatttmcomp <- within(ovatttmcomp, Composition[Composition == "OVAT-C"] <- 'alone')
ovatttmcomp <- within(ovatttmcomp, Composition[Composition == "CCCC"] <- 'conspecific')
ovatttmcomp <- within(ovatttmcomp, Composition[Composition == "AACC"] <- 'amygdalina+ovata')
ovatttmcomp <- within(ovatttmcomp, Composition[Composition == "CCDD"] <- 'ovata+viminalis')
ovatttmcomp$Composition <- factor(ovatttmcomp$Composition, levels = c("alone", "conspecific", "amygdalina+ovata", "ovata+viminalis"))
ovatttmmod <- lmer(tt100m ~ Composition + (1|Pot_number), ovatttmcomp)

vimittmcomp <- alone_or_four %>% filter(Species=="VIMI", C_or_D == "D")
vimittmcomp <- within(vimittmcomp, Composition[Composition == "VIMI-D"] <- 'alone')
vimittmcomp <- within(vimittmcomp, Composition[Composition == "DDDD"] <- 'conspecific')
vimittmcomp <- within(vimittmcomp, Composition[Composition == "AADD"] <- 'amygdalina+viminalis')
vimittmcomp <- within(vimittmcomp, Composition[Composition == "CCDD"] <- 'ovata+viminalis')
vimittmcomp$Composition <- factor(vimittmcomp$Composition, levels = c("alone", "conspecific", "amygdalina+viminalis", "ovata+viminalis"))
vimittmmod <- lmer(tt100m ~ Composition + (1|Pot_number), vimittmcomp)
summary(vimittmmod)

ttmmods <- c(amygttmmod, ovatttmmod, vimittmmod)
dev.off()
pdf("Output/figure_3.pdf", width=8, height=4)
##"#F8766D" - vimi "#7CAE00" "#C77CFF" - amyg
plot_models(ttmmods, transform = NULL, vline.color = "grey", legend.title = "Species",
            dot.size = 2, line.size = 1, ci.lvl=0.95)+
  ylab("Effect size")+
  xlab("Competition treatment")+
  theme_classic()+
  scale_colour_manual(labels = c(expression(italic("E. viminalis")), expression(italic("E. ovata")), expression(italic("E. amygdalina"))),
                      values=c('#F8766D', '#7CAE00', '#C77CFF'))+
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=16),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16),
        legend.text.align = 0,
        legend.position = "right",
        legend.title.align = 0.5)
dev.off()

#This is what colours are being used! When factor with 4 levels
#scales::hue_pal()(4)
#"#F8766D" - vimi "#7CAE00" "#00BFC4" "#C77CFF" - amyg

#### Stats - Fig 3 mortality by composition ####
#amyg
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
summary(ovatttmmod)
dharm <- simulateResiduals(ovatttmmod)
plot(dharm)
r.squaredGLMM(ovatttmmod)


##vimi
summary(vimittmmod)
dharm <- simulateResiduals(vimittmmod)
plot(dharm)
r.squaredGLMM(vimittmmod)

##stats table ##
tab_model(amygttmmod, ovatttmmod, vimittmmod, transform = NULL,
          pred.labels = c('Intercept (alone)', 'conspecific', 'amygdalina+ovata', 'amygdalina+viminalis', 'ovata+viminalis'),
          dv.labels = c('E. amygdalina', 'E. ovata', 'E. viminalis'))

#testing differences between compositions, e.g. does AACC_ovat take longer
# to die than AACC_amyg?
# alone_or_four <- alone_or_four %>% unite("compspecies", Composition:Species, remove = FALSE)
# AACC <- alone_or_four %>% filter(Composition == "AACC")
# ttmcompmod <- lmer(tt100m ~ compspecies + (1|Pot_number), AACC)
# summary(ttmcompmod)
# dharm <- simulateResiduals(ttmcompmod)
# plot(dharm)
# r.squaredGLMM(ttmcompmod)
# emmeans(ttmcompmod, list(pairwise ~ compspecies), adjust="tukey")
# 
# ## and what about for growth?
# grcompmod <- lmer(sqrt(gr_predrought) ~ compspecies + (1|Pot_number), AACC)
# summary(grcompmod)
# dharm <- simulateResiduals(grcompmod)
# plot(dharm)
# r.squaredGLMM(grcompmod)
# emmeans(grcompmod, list(pairwise ~ compspecies), adjust="tukey")

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
hist(sometraits$wd_kg_m3)
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
mtext("Mean wood density (kg/m^3)", side=1, outer=T, cex=5, line=5)
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
mtext("Mean HV", side=1, outer=T, cex=4, line=5)
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

### Plot - gr against sla and wood density
dev.off()
pdf("Output/gr~sla.pdf", width=21, height=21)
par(oma=c(8,8,1,1), mgp=c(0,3,0), mar=c(3, 3, 1, 1))
plot(mean_grpredrought ~ mean_sla_sp, col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], 
     ylab="", ylim=c(0,18), xlim=c(80,150), 
     xlab="", tck=-0.01, pch=19, cex=5, cex.axis=4, bty="n", meandata)
mtext("Mean gr pre-drought (mm/day)", side=2, outer=T, cex=4, line=4)
mtext("Mean specific leaf area (cm^2/g)", side=1, outer=T, cex=4, line=5)
arrows(x0=meandata$mean_sla_sp, y0=meandata$lower_gr,
       x1=meandata$mean_sla_sp, y1=meandata$upper_gr, code=3, cex = 4, angle=90, length=0.1, lwd=5)
arrows(x0=meandata$lower_sla, y0=meandata$mean_grpredrought,
       x1=meandata$upper_sla, y1=meandata$mean_grpredrought, code=3, angle=90, cex = 4, length=0.1, lwd=5)
box(bty="l", lwd=4)
points(mean_grpredrought ~ mean_sla_sp, pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], meandata)
legend("bottomleft", inset = 0.05, title = "Species", c(expression(italic("E. amygdalina")), expression(italic("E. obliqua")), expression(italic("E. ovata")), expression(italic("E. viminalis"))), pch=19, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], cex = 4)
dev.off()

#with underlying points
dev.off()
pdf("Output/gr~sla+points.pdf", width=21, height=21)
par(oma=c(8,8,1,1), mgp=c(0,3,0), mar=c(3, 3, 1, 1))
plot(mean_grpredrought ~ mean_sla_sp, col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], 
     ylab="", ylim=c(0,18), xlim=c(70,190), 
     xlab="", tck=-0.01, pch=19, cex=5, cex.axis=4, bty="n", meandata)
points(gr_predrought ~ mean_sla_plant, cex=3, pch=19, col = alpha(c('red', 'blue', 'forestgreen', 'purple')[as.factor(solowatereddata$Species)], 0.4), solowatereddata)
mtext("Mean gr pre-drought (mm/day)", side=2, outer=T, cex=4, line=4)
mtext("Mean specific leaf area (cm^2/g)", side=1, outer=T, cex=4, line=5)
arrows(x0=meandata$mean_sla_sp, y0=meandata$lower_gr,
       x1=meandata$mean_sla_sp, y1=meandata$upper_gr, code=3, cex = 4, angle=90, length=0.1, lwd=5)
arrows(x0=meandata$lower_sla, y0=meandata$mean_grpredrought,
       x1=meandata$upper_sla, y1=meandata$mean_grpredrought, code=3, angle=90, cex = 4, length=0.1, lwd=5)
box(bty="l", lwd=4)
points(mean_grpredrought ~ mean_sla_sp, pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], meandata)
legend("bottomleft", inset = 0.05, title = "Species", c(expression(italic("E. amygdalina")), expression(italic("E. obliqua")), expression(italic("E. ovata")), expression(italic("E. viminalis"))), pch=19, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], cex = 4)
dev.off()

dev.off()
pdf("Output/gr~wd.pdf", width=21, height=21)
par(oma=c(8,8,1,1), mgp=c(0,3,0), mar=c(3, 3, 1, 1))
plot(mean_grpredrought ~ mean_wd_sp, col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], 
     ylab="", ylim=c(0,18), xlim=c(0.45,0.52), 
     xlab="", tck=-0.01, pch=19, cex=5, cex.axis=4, bty="n", meandata)
mtext("Mean gr pre-drought (mm/day)", side=2, outer=T, cex=4, line=4)
mtext("Mean wood density (g/cm^3)", side=1, outer=T, cex=4, line=5)
arrows(x0=meandata$mean_wd_sp, y0=meandata$lower_gr,
       x1=meandata$mean_wd_sp, y1=meandata$upper_gr, code=3, cex = 4, angle=90, length=0.1, lwd=5)
arrows(x0=meandata$lower_wd, y0=meandata$mean_grpredrought,
       x1=meandata$upper_wd, y1=meandata$mean_grpredrought, code=3, angle=90, cex = 4, length=0.1, lwd=5)
points(mean_grpredrought ~ mean_wd_sp, pch=19, cex=4, 
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
     ylab="", ylim=c(0,5), xlim=c(450,520), 
     xlab="", tck=-0.01, pch=19, cex=5, cex.axis=4, bty="n", meandata)
mtext("Mean time to 100% mortality (weeks)", side=2, outer=F, cex=3.5, line=7)
mtext("Mean WD (kg/m^3)", side=1, outer=F, cex=3.5, line=7)
arrows(x0=meandata$mean_wd_sp, y0=meandata$lower_ttm100,
       x1=meandata$mean_wd_sp, y1=meandata$upper_ttm100, code=3, cex = 4, angle=90, length=0.1, lwd=5)
arrows(x0=meandata$lower_wd, y0=meandata$mean_tt100m,
       x1=meandata$upper_wd, y1=meandata$mean_tt100m, code=3, angle=90, cex = 4, length=0.1, lwd=5)
points(mean_tt100m ~ mean_wd_sp, pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], meandata)
box(bty="l", lwd=4)
#ttm ~ wd
plot(mean_tt100m ~ mean_huber_sp, col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], 
     ylab="", ylim=c(0,5), xlim=c(0.00015,0.0003), 
     xlab="", tck=-0.01, pch=19, cex=5, cex.axis=4, bty="n", meandata)
mtext("Mean time to 100% mortality (weeks)", side=2, outer=F, cex=3.5, line=7)
mtext("Mean HV", side=1, outer=F, cex=3.5, line=7)
arrows(x0=meandata$mean_huber_sp, y0=meandata$lower_ttm100,
       x1=meandata$mean_huber_sp, y1=meandata$upper_ttm100, code=3, cex = 4, angle=90, length=0.1, lwd=5)
arrows(x0=meandata$lower_huber, y0=meandata$mean_tt100m,
       x1=meandata$upper_huber, y1=meandata$mean_tt100m, code=3, angle=90, cex = 4, length=0.1, lwd=5)
points(mean_tt100m ~ mean_huber_sp, pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], meandata)
box(bty="l", lwd=4)
#gr ~ wd
plot(mean_grpredrought ~ mean_wd_sp, col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], 
     ylab="", ylim=c(0,7), xlim=c(420,550), 
     xlab="", tck=-0.01, pch=19, cex=5, cex.axis=4, bty="n", meandata)
points(gr_predrought ~ wd_kg_m3, cex=3, pch=19, col = alpha(c('red', 'blue', 'forestgreen', 'purple')[as.factor(solowatereddata$Species)], 0.4), solowatereddata)
mtext("Mean height growth rate (mm/day)", side=2, outer=F, cex=3.5, line=7)
mtext("Mean WD (kg/m^3)", side=1, outer=F, cex=3.5, line=7)
arrows(x0=meandata$mean_wd_sp, y0=meandata$lower_gr,
       x1=meandata$mean_wd_sp, y1=meandata$upper_gr, code=3, cex = 4, angle=90, length=0.1, lwd=5)
arrows(x0=meandata$lower_wd, y0=meandata$mean_grpredrought,
       x1=meandata$upper_wd, y1=meandata$mean_grpredrought, code=3, angle=90, cex = 4, length=0.1, lwd=5)
points(mean_grpredrought ~ mean_wd_sp, pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], meandata)
box(bty="l", lwd=4)
#gr ~ SLA
plot(mean_grpredrought ~ mean_lma_sp, col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], 
     ylab="", ylim=c(0,7), xlim=c(50,135), 
     xlab="", tck=-0.01, pch=19, cex=5, cex.axis=4, bty="n", meandata)
points(gr_predrought ~ mean_lma_plant, cex=3, pch=19, col = alpha(c('red', 'blue', 'forestgreen', 'purple')[as.factor(solowatereddata$Species)], 0.4), solowatereddata)
mtext("Mean height growth rate (mm/day)", side=2, outer=F, cex=3.5, line=7)
mtext("Mean LMA (g/m^2)", side=1, outer=F, cex=3.5, line=7)
arrows(x0=meandata$mean_lma_sp, y0=meandata$lower_gr,
       x1=meandata$mean_lma_sp, y1=meandata$upper_gr, code=3, cex = 4, angle=90, length=0.1, lwd=5)
arrows(x0=meandata$lower_lma, y0=meandata$mean_grpredrought,
       x1=meandata$upper_lma, y1=meandata$mean_grpredrought, code=3, angle=90, cex = 4, length=0.1, lwd=5)
box(bty="l", lwd=4)
points(mean_grpredrought ~ mean_sla_sp, pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], meandata)
dev.off()

############# WITH legend
dev.off()
pdf("Output/traits_panel+legend.pdf", width=21, height=21)
par(mfrow=c(2,2), oma=c(15,12,1,1), mgp=c(0,2.5,0), mar=c(3, 5, 7, 12))
#ttm ~ wd
plot(mean_tt100m ~ mean_wd_sp, col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], 
     ylab="", ylim=c(0,5), xlim=c(450,520), 
     xlab="", tck=-0.01, pch=19, cex=5, cex.axis=4, bty="n", meandata)
mtext("Mean time to 100% leaf mortality \n(weeks)", side=2, outer=F, cex=3.5, line=7)
mtext("Mean WD (kg/m^3)", side=1, outer=F, cex=3.5, line=7)
mtext("A)", side = 1, outer=F, cex = 3.5, line = -44, adj=0.01)
arrows(x0=meandata$mean_wd_sp, y0=meandata$lower_ttm100,
       x1=meandata$mean_wd_sp, y1=meandata$upper_ttm100, code=3, cex = 4, angle=90, length=0.1, lwd=5)
arrows(x0=meandata$lower_wd, y0=meandata$mean_tt100m,
       x1=meandata$upper_wd, y1=meandata$mean_tt100m, code=3, angle=90, cex = 4, length=0.1, lwd=5)
points(mean_tt100m ~ mean_wd_sp, pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], meandata)
box(bty="l", lwd=4)
#ttm ~ hv
plot(mean_tt100m ~ mean_huber_sp, col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], 
     ylab="", ylim=c(0,5), xlim=c(0.00015,0.00035), 
     xlab="", tck=-0.01, pch=19, cex=5, cex.axis=4, bty="n", meandata)
mtext("Mean time to 100% leaf mortality \n(weeks)", side=2, outer=F, cex=3.5, line=7)
mtext("Mean HV", side=1, outer=F, cex=3.5, line=7)
mtext("B)", side = 1, outer=F, cex = 3.5, line = -44, adj=0.01)
arrows(x0=meandata$mean_huber_sp, y0=meandata$lower_ttm100,
       x1=meandata$mean_huber_sp, y1=meandata$upper_ttm100, code=3, cex = 4, angle=90, length=0.1, lwd=5)
arrows(x0=meandata$lower_huber, y0=meandata$mean_tt100m,
       x1=meandata$upper_huber, y1=meandata$mean_tt100m, code=3, angle=90, cex = 4, length=0.1, lwd=5)
points(mean_tt100m ~ mean_huber_sp, pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], meandata)
box(bty="l", lwd=4)
#gr ~ wd
plot(mean_grpredrought ~ mean_wd_sp, col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], 
     ylab="", ylim=c(0,7), xlim=c(420,550), 
     xlab="", tck=-0.01, pch=19, cex=5, cex.axis=4, bty="n", meandata)
points(gr_predrought ~ wd_kg_m3, cex=3, pch=19, col = alpha(c('red', 'blue', 'forestgreen', 'purple')[as.factor(solowatereddata$Species)], 0.4), solowatereddata)
mtext("Height growth rate \n(mm/day)", side=2, outer=F, cex=3.5, line=7)
mtext("WD (kg/m^3)", side=1, outer=F, cex=3.5, line=7)
mtext("C)", side = 1, outer=F, cex = 3.5, line = -44, adj=0.01)
arrows(x0=meandata$mean_wd_sp, y0=meandata$lower_gr,
       x1=meandata$mean_wd_sp, y1=meandata$upper_gr, code=3, cex = 4, angle=90, length=0.1, lwd=5)
arrows(x0=meandata$lower_wd, y0=meandata$mean_grpredrought,
       x1=meandata$upper_wd, y1=meandata$mean_grpredrought, code=3, angle=90, cex = 4, length=0.1, lwd=5)
points(mean_grpredrought ~ mean_wd_sp, pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], meandata)
box(bty="l", lwd=4)
#gr ~ LMA
plot(mean_grpredrought ~ mean_lma_sp, col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], 
     ylab="", ylim=c(0,7), xlim=c(50,135), 
     xlab="", tck=-0.01, pch=19, cex=5, cex.axis=4, bty="n", meandata)
points(gr_predrought ~ mean_lma_plant, cex=3, pch=19, col = alpha(c('red', 'blue', 'forestgreen', 'purple')[as.factor(solowatereddata$Species)], 0.4), solowatereddata)
mtext("Height growth rate \n(mm/day)", side=2, outer=F, cex=3.5, line=7)
mtext("LMA (g/m^2)", side=1, outer=F, cex=3.5, line=7)
mtext("D)", side = 1, outer=F, cex = 3.5, line = -44, adj=0.01)
arrows(x0=meandata$mean_lma_sp, y0=meandata$lower_gr,
       x1=meandata$mean_lma_sp, y1=meandata$upper_gr, code=3, cex = 4, angle=90, length=0.1, lwd=5)
arrows(x0=meandata$lower_lma, y0=meandata$mean_grpredrought,
       x1=meandata$upper_lma, y1=meandata$mean_grpredrought, code=3, angle=90, cex = 4, length=0.1, lwd=5)
box(bty="l", lwd=4)
points(mean_grpredrought ~ mean_sla_sp, pch=19, cex=4, 
       col=c('red', 'blue', 'forestgreen', 'purple')[as.factor(meandata$Species)], meandata)
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
#Not enough data to look at this statistically!
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

#CORRELATIONS BETWEEN TRAITS AND gr
grslamod <- lmer(sqrt(gr_predrought) ~ mean_sla_plant + (1|Species), solowatereddata)
summary(grslamod)
dharm <- simulateResiduals(grslamod) 
plot(dharm)
r.squaredGLMM(grslamod)

ggplot(solowatereddata, aes(x = mean_sla_plant, y = sqrt(gr_predrought)))+
  geom_point(alpha=0.4)+
  geom_smooth(method="lm")+
  theme_classic()

grwdmod <- lmer(sqrt(gr_predrought) ~ wd + (1|Species), solowatereddata)
summary(grwdmod)
dharm <- simulateResiduals(grwdmod) 
plot(dharm)
r.squaredGLMM(grwdmod)
#plot
ggplot(solowatereddata, aes(x = wd, y = sqrt(gr_predrought)))+
  geom_point(alpha=0.4)+
  geom_smooth(method="lm")+
  theme_classic()

#CORRELATIONS BETWEEN TRAITS AND TTM and gr
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
r.squaredGLMM(grwdmod)
