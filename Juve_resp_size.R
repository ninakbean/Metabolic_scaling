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
  select(Date, Species, ID, Max.mm, Min.mm, Height.mm, micromol.coral.min, std.error, p.value.x, adj.r.squared, r.squared)

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
ggplot(Master,aes(x=Elapsed_time_min, y= micromol.coral, group = ID))+
  geom_point(aes(color = ID), size=2, stroke=1, alpha = 1)+
  theme(legend.position="right")+
  labs(x="Elapsed Time (min)",y="Oxygen")+
  theme_classic(base_size=12)+
  geom_smooth(method="lm", color="black", size=0.5)

################### PORITES ########################################
#assuming a hemi-ellipsoid= (2*3.14*height*radius1*radius2)
#dip 1.13 cm2 = estimated 1 cm2
#when you add elevation, the slope changes and R2 changes

Porites <- MASTER%>%  
  filter(Species=="Porites")%>%
  mutate(Diameter=rowMeans(cbind(Max.mm,Min.mm), na.rm=T))%>%
  mutate(SA.shape.cm2.hemi=(2*3.14*(Height.mm/10)*((Max.mm/2)/10)*((Min.mm/2)/10)))%>% #hemi-ellipsoid
  mutate(SA.cm2.hemi=(SA.shape.cm2.hemi*1.13)+6.51)%>% #converting estimated SA based on shape to actual using dip
  mutate(x=log10(SA.cm2.hemi))%>%
  mutate(y=log10(Dry_weight.g*1000))

Porites.dry.no.outlier <- Porites%>%
  filter(ID!="20") #this is better, but R2 is worse! 

model <-  lm(y~x, data=Porites)
model.no.outlier <-  lm(y~x, data=Porites.dry.no.outlier)
Anova(model, model.no.outlier, type=3)
AIC(model, model.no.outlier)

summary(model, type=II)
summary(model.no.outlier, type = II)

out <- as.data.frame(augment(model))
cooks <- cooks.distance(model)
plot(cooks)
#4/n(10) = 0.4
#y=0.3222193, x=0.8705974, ID=20

#full data one
#t=(estimated value-hypothesized value)/standard error of the estimator
t=(2.0715-(1))/0.3568
t
2*pt(abs(t),df=9, lower=FALSE) #gives you p value

#outlier one
#t=(estimated value-hypothesized value)/standard error of the estimator
t=(1.5659-(1))/0.3261
t
2*pt(abs(t),df=8, lower=FALSE) #gives you p value
#not significantly different from 1

################### POCILLOPORA ########################################
#assuming a hemi-ellipsoid= (2*3.14*height*radius1*radius2)
#dip 1.63 cm2 = estimated 1 cm2

Pocillopora <- MASTER%>%
  filter(Species=="Pocillopora")%>%
  mutate(Diameter=rowMeans(cbind(Max.mm,Min.mm), na.rm=T))%>%
  mutate(SA.shape.cm2.hemi=(2*3.14*(Height.mm/10)*((Max.mm/2)/10)*((Min.mm/2)/10)))%>% #hemi-ellipsoid
  mutate(SA.cm2.hemi=((SA.shape.cm2.hemi*1.63)+6.94))%>% #converting estimated SA based on shape to actual using dip
  mutate(x = log10(SA.cm2.hemi))%>%
  mutate(y=log10(Dry_weight.g*1000))

Pocillopora.dry.no.outlier <- Pocillopora%>%
  filter(ID!="23") #this improves the model

model <-  lm(y~x, data=Pocillopora)
model.no.outlier <-  lm(y~x, data=Pocillopora.dry.no.outlier)
Anova(model, model.no.outlier, type=3)
AIC(model, model.no.outlier)

summary(model, type=II)
summary(model.no.outlier, type=II)

out <- as.data.frame(augment(model))
cooks <- cooks.distance(model)
plot(cooks)
#4/n(11) = 0.36
#y=0.7634280, x=, ID=23

#full data set
#t=(estimated value-hypothesized value)/standard error of the estimator
t=(0.9794-(1))/0.1185
t
2*pt(abs(t),df=10, lower=FALSE) #gives you p value
#not significantly different from 3/4 or 1

#outlier one
#t=(estimated value-hypothesized value)/standard error of the estimator
t=(0.89785-(2/3))/0.11351
t
2*pt(abs(t),df=9, lower=FALSE) #gives you p value
#not significantly different from 3/4 or 1 or 2/3

################### CONSOLIDATED ########################################
#I need to consolidate because I got the surface area separately
Consolidated <- rbind(Pocillopora, Porites)%>%
  select(-Diameter,-SA.shape.cm2.hemi)

Consolidated.no.outliers <- rbind(Pocillopora.dry.no.outlier,Porites.dry.no.outlier)

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
#Finished processing corals after 87 hours 37 min from collection time. Round up to 88
#Juveniles incubated from 19-60 min. 


# Step 1: Call the pdf command to start the plot
#pdf(file = "Figs/Empirical/Juveniles/size.pdf",   # The directory you want to save the file in
#    width = 7, # The width of the plot in inches
#    height = 6) # The height of the plot in inches

# Step 2: Create the plot with R code
#SA, Dry_weight
ggplot(Consolidated, aes(x=log10(SA.cm2.hemi), y=log10(Dry_weight.g*1000), group = Species))+
  geom_point(aes(color = Species), size=2, stroke=1, alpha = 1)+
  scale_color_manual(values=c("#F8A42F", "#FF4605"),name="Genera", 
                     labels = c((expression(paste(italic("Pocillopora spp.")))),
                                expression(paste(italic("Porites spp.")))))+ 
  theme(legend.position="right")+
  labs(x=(expression(paste("Log Surface Area ", (cm^2)))), y="Log Dry Tissue (mg)")+
  theme_classic(base_size=12)+
  geom_smooth(method="lm", color="black", size=0.5)+
  theme(legend.text.align = 0,
        legend.position = c(0.8, 0.15))+
  geom_abline(intercept=-0.7, slope=1, colour = "black", size = 0.5, lty=5)+
  geom_abline(intercept=1.1, slope=(2/3), colour = "black", size = 0.5, lty=5)+
  annotate("text", x=1, y=2, size=5, colour="black", label= "b=2/3")+
  annotate("text", x=1.25, y=0.4, size=5, colour="black", label= "b=1")

# Step 3: Run dev.off() to create the file!
#“null device”<- saying now create plots in the main R plotting window again.
dev.off()

  #annotate("text", x=1.6, y=0.8, size=5, colour="#bf9000", 
  #         label= "y=0.74x+0.31")+ #Pocillopora
  #annotate("text", x=1.6, y=0.7, size=5, colour="#bf9000", 
  #         label= "R^{2}==0.90", parse=T)+ #Pocillopora
  #annotate("text", x=0.3, y=1.8, size=5, colour="#FF4605", 
  #         label= "y=1.0x+0.48")+ #Porites
  #annotate("text", x=0.3, y=1.7, size=5, colour="#FF4605", 
  #         label= "R^{2}==0.93", parse=T) #Porites
# geom_abline(intercept = 0, slope = 1, color="red", 
#              linetype="dashed", size=1.5)
  

################## ANCOVA ###################################
#ASSUMPTION 1
#Linearity between the covariate and the outcome variable at each level of the grouping variable. 
#This can be checked by creating a grouped scatter plot of the covariate and the outcome variable.

ggscatter(
  data=Consolidated, x = "x", y = "y",
  color = "Species", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Species)
  )

ggscatter(
  data=Consolidated.no.outliers, x = "x", y = "y",
  color = "Species", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Species)
  )

#Homogeneity of slopes
#The slopes of the regression lines should be the same for each group. 
#This assumption checks that there is no significant interaction between the covariate and the grouping variable.
#The plotted regression lines by groups should be parallel.

model <- lm(y ~ Species*x, data=Consolidated)
model.no.outlier <- lm(y ~ Species*x, data=Consolidated.no.outliers)
Anova(model, type=3)
Anova(model.no.outlier, type=3)

Anova(model, model.no.outlier, type=3) #tests significantly different
AIC(model, model.no.outlier) #removing outliers is better

model1 <- lm(y ~ Species+x, data=Consolidated) #saying lines are parall
model2 <- lm(y ~ + x, data=Consolidated) #saying type has no effect

Anova(model, model1, type=3) #keep interaction term, because sig difference
Anova(model, model2, type=3) #groups are significant
AIC(model, model1, model2) #reinforces full model is needed

#x has effect on y, there is a difference in slope of this relationship
#by type, as indicated by interaction term. 
step(model) #tries removing the most complicated term and gives you AIC
#If take out interaction term, AIC gets worse


#ASSUMPTION 3
#Normality of residuals
#The outcome variable should be approximately normally distributed. 
#This can be checked using the Shapiro-Wilk test of normality on the model residuals.

# Inspect the model diagnostic metrics
model.metrics <- augment(model)
# Assess normality of residuals using shapiro wilk test
shapiro_test(model.metrics$.resid)
#>0.05, normal data 

#ASSUMPTION 4
#Homogeneity of variances (aka homoscedasticity)
#ANCOVA assumes that the variance of the residuals is equal for all groups. 
#This can be checked using the Levene’s test
model.metrics %>% levene_test(.resid ~ Species)
#The Levene’s test was not significant (p > 0.05)
#assume homogeneity of the residual variances for all groups.





