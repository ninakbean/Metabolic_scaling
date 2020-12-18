library(popbio)
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggpmisc) #part of ggplot2? Formula on graoh
library(dplyr)
library(tidyr)
library(forcats) 
library(lme4)
library(writexl) #write_xlsx(list(Sheet1=df1,Sheet2=df2),"mydata.xlsx")
library(stringr)
library(metafor)
library(car)
library(nortest)
library(GAD)
library(AICcmodavg) #load AICcmodavg package
library(DescTools) #winsorizing
library(robustHD) #winsorizing
library(MASS)
library(survival)
library(RColorBrewer)
library(patchwork)
library(rmarkdown)
library(ggpubr)
library(rstatix)
library(broom)
library(WRS2)
library(lsmeans)
library(growthrates)

resp <-read_excel("Data/Juveniles/Data/Juve_resp_size_2020.xlsx",sheet="Respiration")
biomass <-read_excel("Data/Juveniles/Data/Juve_resp_size_2020.xlsx",sheet="Biomass")
info <-read_excel("Data/Juveniles/Data/Juve_resp_size_2020.xlsx",sheet="Temp_oxygen")

resp$ID <- as.factor(resp$ID)
biomass$ID <- as.factor(biomass$ID)
info$ID <- as.factor(info$ID)

#taking out the first 3 minutes of C2 and joining it back to the main dataset
control2 <- resp%>%
  filter(ID=="C2")%>% 
  filter(Elapsed_time_min>3)

resp_noC2 <- resp%>%
  filter(ID!="C2")

resp <- rbind(control2, resp_noC2)

#Fixing master sheet

combined <- merge(resp, info, by="ID")

Master <- combined%>%
  mutate(volume=Chamber.mL-Displacement.mL)%>%
  mutate(micromol.L=((Oxygen/100)*Oxygen_saturation))%>% #in micromol O2
  mutate(micromol.mL = (micromol.L/1000))%>%
  mutate(micromol.coral = micromol.mL*volume)

#grouping the data based on trial
by_ID <- 
  group_by(Master, ID, Species,Max.mm, Min.mm, Height.mm, Date.x)

#deriving the slope for each run
Regression <- do(by_ID,tidy(lm(micromol.coral ~ Elapsed_time_min, data = .)))%>%
  filter(term == "Elapsed_time_min")%>%
  select(-term)%>%
  mutate(estimate=(estimate*-1)) #estimate is percent of oxygen every 10 sec, 
                                 #to make oxygen positive

R2 <- do(by_ID,glance(lm(micromol.coral ~ Elapsed_time_min, data = .)))

#combining data so I have the slope and R2
Master_Slopes <- merge(Regression, R2, by="ID")%>%
  rename(Species=Species.x)%>%
  rename(Max.mm=Max.mm.x)%>%
  rename(Min.mm=Min.mm.x)%>%
  rename(Height.mm=Height.mm.x)%>%
  rename(Date=Date.x.x)%>%
  rename(micromol.coral.min=estimate)%>%
  select(Date, Species, ID, Max.mm, Min.mm, Height.mm, micromol.coral.min, std.error, p.value.x, adj.r.squared)

#correcting all measurements on 11/09/2020 based on control slope
nineth <- Master_Slopes%>%
  filter(Date=="11092020")%>%
  mutate(micromol.coral.min=micromol.coral.min-0.009689647)%>% #C2
  filter(ID!="C2")

#correcting all measurements on 11/10/2020 based on control slope
tenth <- Master_Slopes%>%
  filter(Date=="11102020")%>%
  mutate(micromol.coral.min=micromol.coral.min-0.024504653)%>% #C4
  filter(ID!= c("C3", "C4")) #C3 was not a straight line

#corrected for control
Slopes_corrected <- rbind(nineth, tenth)

weight <- biomass%>%
  select(Dry_weight.g, ID)

MASTER <- merge(weight, Slopes_corrected, by="ID")

################ ALL DATA #############################
#ggplot(Master,aes(x=Elapsed_time_min, y= micromol.coral, group = ID))+
 # geom_point(aes(color = ID), size=2, stroke=1, alpha = 1)+
#  theme(legend.position="right")+
#  labs(x="Elapsed Time (min)",y="Oxygen")+
#  theme_classic(base_size=12)+
#  geom_smooth(method="lm", color="black", size=0.5)

################### PORITES ########################################
#assuming a hemi-ellipsoid= (2*3.14*height*radius1*radius2)
#dip 1.13 cm2 = estimated 1 cm2

Porites <- MASTER%>%  
  filter(Species=="Porites")%>%
  mutate(Diameter=rowMeans(cbind(Max.mm,Min.mm), na.rm=T))%>%
  mutate(SA.shape.cm2.hemi=(2*3.14*(Height.mm/10)*((Max.mm/2)/10)*((Min.mm/2)/10)))%>% #hemi-ellipsoid
  mutate(SA.cm2.hemi=SA.shape.cm2.hemi*1.13) #converting estimated SA based on shape to actual using dip
                                 
  #filter(ID!="20")

#Log log
ggplot(Porites,aes(x=log10(Dry_weight.g*1000), y=log10(micromol.coral.min*60)))+
  geom_point(size=2, stroke=1, alpha = 1)+
  #scale_shape_manual(values=c(2, 1))+
  #scale_color_manual(values=c("#F8A42F", "#FF4605", "#860111"))+
  theme(legend.position="right")+
  labs(x="Log Dry Tissue (mg)",y=(expression(paste("Log Respiration (", mu , "mol", " ", O[2], " ", h^-1,")"))))+
  theme_classic(base_size=12)+
  geom_smooth(method="lm", color="black", size=0.5)
  #scale_x_continuous(breaks=seq(0,2000,200),limits=c(0,2000))
  #annotate("text", x=2.3, y=2.5, size=5, label= expression(paste(R^2 , " = 0.025, p=0.52")))+ #brooding

Porites_model <-  lm(log10(micromol.coral.min*60)~log10(Dry_weight.g*1000), data=Porites)
summary(Porites_model)

#Porites, resp: adj R2:0.8386, p = 0.000123, y= 0.65493(+- 0.09477)x-0.32735
#Overlaps with 0.75
#0.65493-0.09477 = 0.56016
#0.65493+0.09477 = 0.7497
#0.56-0.75


#SA, Dry_weight
ggplot(Porites,aes(x=log10(SA.cm2.hemi), y=log10(Dry_weight.g*1000)))+
  geom_point(size=2, stroke=1, alpha = 1)+
  theme(legend.position="right")+
  labs(x=(expression(paste("Log Surface Area ", (mm^2)))), y="Log Dry Weight (mg)")+
  theme_classic(base_size=12)+
  geom_smooth(method="lm", color="black", size=0.5)

Porites_dry <-  lm(log10(Dry_weight.g*1000)~log10(SA.cm2.hemi), data=Porites)
summary(Porites_dry)

#Porites, dry: adj R2:0.9255, p = 5.396e-06, 
#y= 1.01264(+-  0.09532)x + 0.48072

################### POCILLOPORA ########################################
#assuming a hemi-ellipsoid= (2*3.14*height*radius1*radius2)
#dip 1.63 cm2 = estimated 1 cm2

Pocillopora <- MASTER%>%
  filter(Species=="Pocillopora")%>%
  mutate(Diameter=rowMeans(cbind(Max.mm,Min.mm), na.rm=T))%>%
  mutate(SA.shape.cm2.hemi=(2*3.14*(Height.mm/10)*((Max.mm/2)/10)*((Min.mm/2)/10)))%>% #hemi-ellipsoid
  mutate(SA.cm2.hemi=SA.shape.cm2.hemi*1.63) #converting estimated SA based on shape to actual using dip

#LOG LOG
ggplot(Pocillopora,aes(x=log10(Dry_weight.g*1000), y=log10(micromol.coral.min*60)))+
  geom_point( size=2, stroke=1, alpha = 1)+
  #scale_shape_manual(values=c(2, 1))+
  #scale_color_manual(values=c("#F8A42F", "#FF4605", "#860111"))+
  theme(legend.position="right")+
  labs(x="Log Dry Tissue (mg)",y=(expression(paste("Log Respiration (", mu , "mol", " ", O[2], " ", h^-1,")"))))+
  theme_classic(base_size=12)+
  geom_smooth(method="lm", color="black", size=0.5)
#scale_y_continuous(breaks=seq(80,105,10),limits=c(80,105))
#annotate("text", x=2.3, y=2.5, size=5, label= expression(paste(R^2 , " = 0.025, p=0.52")))+ #brooding

Pocillopora_model <-  lm(log10(micromol.coral.min*60)~log10(Dry_weight.g*1000), data=Pocillopora)
summary(Pocillopora_model)

#Pocillopora, resp: adj R2:0.9158 , p <0.001, y=0.99492 (+-0.09497)x-0.46673 
#overlaps 1
#0.99492-0.09497=0.89995
#0.99492+0.09497=1.08989
#0.90-1.1

#SA, Dry_weight
ggplot(Pocillopora, aes(x=log10(SA.cm2.hemi), y=log10(Dry_weight.g*1000)))+
  geom_point(size=2, stroke=1, alpha = 1)+
  theme(legend.position="right")+
  labs(x=(expression(paste("Log Surface Area ", (mm^2)))), y="Log Dry Weight (mg)")+
  theme_classic(base_size=12)+
  geom_smooth(method="lm", color="black", size=0.5)

Pocillopora_dry <-  lm(log10(Dry_weight.g*1000)~log10(SA.cm2.hemi), data=Pocillopora)
summary(Pocillopora_dry)

#Pocillopora, dry: adj R2:0.8951, p <0.001, 
#y=0.7414 (+-0.07979)x+0.30990

################### CONSOLIDATED ########################################
#I need to consolidate because I got the surface area separately
Consolidated <- rbind(Pocillopora, Porites)%>%
  select(-Diameter,-SA.shape.cm2.hemi)

#write_xlsx(list(data=Consolidated),"Data/Juveniles/Data/R_juve_resp_size.xlsx")

#NOTES
#SA.cm2.hemi: Surface area of coral assuming the shape of a hemi-ellipsoid= (2*3.14*height*radius1*radius2)
#and getting more accurate estimates by relating geometric surface area to wax dipped surface area from calibration curve. 
#Collected corals on 11/08/2020 at 8:10 am
#Preserved corals in 10% formalin for 48 hours
#Decalcified corals in 5% HCL over multiple days (1-7 days)
#Delayed darkness from night of 11/08 (sunset 18:10)
#Corals dark acclimated from 14 hrs 24 min - 53 hrs 37 min
#Temp ~27C
#Chamber 240 mL
#Salinity 34 ppt

ggplot(Consolidated,aes(x=log10(Dry_weight.g*1000), y=log10(micromol.coral.min*60), group =Species))+
  geom_point(aes(color = Species), size=2, stroke=1, alpha = 1)+
  scale_color_manual(values=c("#F8A42F", "#FF4605"))+ 
                     #labels = c((expression(paste(italic("Pocillopora spp.")))), 
                      #          (expression(paste(italic("Porites spp."))))))+
  theme(legend.position="right")+
  labs(x="Log Dry Tissue (mg)",y=(expression(paste("Log Respiration (", mu , "mol", " ", O[2], " ", coral^-1, " ", h^-1,")"))))+
  theme_classic(base_size=12)+
  geom_smooth(method="lm", color="black", size=0.5)
  annotate("text", x=0.9, y=1.1, size=5, colour="#bf9000", label= (expression(paste("y=0.99x-0.47"))))+
  annotate("text", x=0.9, y=1, size=5, colour="#bf9000", label= (expression(paste(r^2, "=0.92"))))+
  annotate("text", x=1.5, y=0.1, size=5, colour="#FF4605", label= (expression(paste("y=0.65x-0.33"))))+
  annotate("text", x=1.5, y=0, size=5, colour="#FF4605", label= (expression(paste(r^2, "=0.84"))))
             
#Pocillopora: y=0.99492 (+-0.09497), overlaps 1
#Porites: y= 0.65493(+- 0.09477)x, Overlaps with 0.75

#SA, Dry_weight
ggplot(Consolidated, aes(x=log10(SA.cm2.hemi), y=log10(Dry_weight.g*1000), group = Species))+
  geom_point(aes(color = Species), size=2, stroke=1, alpha = 1)+
  scale_color_manual(values=c("#F8A42F", "#FF4605"))+
  theme(legend.position="right")+
  labs(x=(expression(paste("Log Surface Area ", (cm^2)))), y="Log Dry Tissue (mg)")+
  theme_classic(base_size=12)+
  geom_smooth(method="lm", color="black", size=0.5)+
  annotate("text", x=1.6, y=0.8, size=5, colour="#bf9000", 
           label= "y=0.74x+0.31")+ #Pocillopora
  annotate("text", x=1.6, y=0.7, size=5, colour="#bf9000", 
           label= "R^{2}==0.90", parse=T)+ #Pocillopora
  annotate("text", x=0.3, y=1.8, size=5, colour="#FF4605", 
           label= "y=1.0x+0.48")+ #Porites
  annotate("text", x=0.3, y=1.7, size=5, colour="#FF4605", 
           label= "R^{2}==0.93", parse=T) #Porites
# geom_abline(intercept = 0, slope = 1, color="red", 
#              linetype="dashed", size=1.5)
  

  #Pocillopora, dry: adj R2:0.8951, p <0.001, 
  #y=0.7414 (+-0.07979)x+0.30990
  
  #Porites, dry: adj R2:0.9255, p = 5.396e-06, 
  #y= 1.01264(+-  0.09532)x + 0.48072

################### RUNNING ANCOVA WITH SA AS COVARIATE##################
#COMPARE SLOPES WITH ANCOVA
#All did not have significant interaction between respiration and data sheet, so can tease apart resp and age
#Best model is to not include interaction
#Age has an effect on respiration, datasheet doesn't
#Pairwise says the datasheets are significantly the same

#### RUNNING ANCOVA #### RAW DATA
#The orders of variables matters when computing ANCOVA. 
#You want to remove the effect of the covariate first - 
#that is, you want to control for it - prior to entering your main variable or interest.

#For ANCOVA, need to look at type I because the first variable is continuous
#Type I: 
#Balanced data, sequential (first variable considered, then the next for error)
#aov defaults to type I
#Use Anova function for type II and III
#Type 2: Unbalanced data, principle of marginality, does not consider interactions. 
#Don't use if interactions are present. If no interaction, more powerful than type 3. 
#Type 3: Unbalanced data, use when sig. interactions, you have to make orthogonal contrasts?

# Type I ANOVA - aov()
#aov(time.lm)

# Type II ANOVA - Anova(type = 2)
#car::Anova(time.lm, type = 2)

# Type III ANOVA - Anova(type = 3)
#car::Anova(time.lm, type = 3)
###
#ASSUMPTIONS
#Linearity between the covariate and the outcome variable at each level of the grouping variable. 
#This can be checked by creating a grouped scatter plot of the covariate and the outcome variable.
#Homogeneity of regression slopes. 
#check for no significant interaction between the covariate and the grouping variable
#The slopes of the regression lines, formed by the covariate and the 
#outcome variable, should be the same for each group. 
#This assumption evaluates that there is no interaction between the outcome and the covariate. 
#The plotted regression lines by groups should be parallel.
#The outcome variable should be approximately normally distributed. 
#This can be checked using the Shapiro-Wilk test of normality on the model residuals.
#Homoscedasticity or homogeneity of residuals variance for all groups. 
#The residuals are assumed to have a constant variance (homoscedasticity)
#No significant outliers in the groups

#Linearity #ASSUMPTION MET

ggscatter(
  Consolidated, x = "log.Dry_weight.mg", y = "log.micromol.coral.hr",
  color = "Species", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Species)
  )

#Homogeneity of slopes
#lines are parallel
#LOOK AT THESE SITES
#https://www.researchgate.net/post/How_can_continue_ANCOVA_when_assumption_of_homogeneity_of_regression_slopes_is_violated
#https://www.theanalysisfactor.com/assumptions-of-ancova/


Consolidated %>% anova_test(log.micromol.coral.hr ~ Species*log.Dry_weight.mg)
# Interaction term is significant
#There is a significant effect of species on dry weight, so the slopes are not parallel
#Although not parallel, slopes are similar, indicating that the homogeneity of slopes assumption is met.

#Normality of residuals
# Fit the model, the covariate goes first
model <- lm(log.micromol.coral.hr ~ log.Dry_weight.mg + Species, data = Consolidated)
# Inspect the model diagnostic metrics
model.metrics <- augment(model) %>%
  select(-.hat, -.sigma, -.fitted, -.se.fit) # Remove details
head(model.metrics, 3)
# Assess normality of residuals using shapiro wilk test
shapiro_test(model.metrics$.resid)
#>0.05, normal data 

#Homogeneity of variances #ASSUMPTION MET
model.metrics %>% levene_test(.resid ~ Species)

#outliers
model.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()
#There was one outier

res.aov <- Consolidated %>% anova_test(log.micromol.coral.hr ~ log.Dry_weight.mg + Species)
get_anova_table(res.aov)


# Pairwise comparisons
library(emmeans)
pwc <- Consolidated%>% 
  emmeans_test(
    log.micromol.coral.hr ~ Species, covariate = log.Dry_weight.mg,
    p.adjust.method = "bonferroni"
  )
pwc
