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

#Eror bars, log plot: https://faculty.washington.edu/stuve/log_error.pdf

###################### HOW GOOD ARE MY SIZE ESTIMATES? #######################
size <-read_excel("Data/Larvae/larvae_resp_size_2020.xlsm",sheet="Size")
str(size)

size$ID <- as.factor(size$ID)

size_ID <- size%>%
  mutate(volume.mm3 = ((3.14/6)*length.mm*width.mm^2))%>%
  group_by(ID)%>%
  summarise(volume_mean.mm3=mean(volume.mm3),
            volume_std.mm3=sd(volume.mm3),
            volume_se.mm3=(sd(volume.mm3)/sqrt(length(volume.mm3))))

#volume of a spheroid (aka ellipsoid) = π/6*max length*max width^2 (Van Moorsel 1983)

ggplot(size_ID)+
  aes(ID, volume_mean.mm3)+
  geom_bar(width = 0.7, position = position_dodge(width=0.7), stat="identity")+
  theme_classic(base_size=12)+
  geom_errorbar(aes(ymin=volume_mean.mm3-volume_se.mm3, ymax=volume_mean.mm3+volume_se.mm3, width=0),position=position_dodge(1))+
  labs(x="Vial ID",y="Volume (mm^3) Assuming the shape of an ellipsoid")
  
###################### RESPIRATION #######################
resp_OG <-read_excel("Data/Larvae/larvae_resp_size_2020.xlsm",sheet="Respiration")
str(resp_OG)

resp <- merge(resp_OG, size_ID, by="ID")

long_resp <- resp%>%
  filter(ID!="101")%>% #not long enough incubation, really low number
  filter(ID!="42")%>% #42 no size
  mutate(log_resp = log10(pikomol.larva.min))%>% #y axis
  mutate(log_volume.mm3 = log10(volume_mean.mm3))%>% #x axis
  mutate(log_volume_min.mm3 = log10(volume_mean.mm3-volume_se.mm3))%>% #Can use this for error bars too, but asymetrical
  mutate(log_volume_max.mm3 = log10(volume_mean.mm3+volume_se.mm3))%>% #Can use this for error bars too, but asymetrical
  mutate(divide_se.mm3 = volume_se.mm3/volume_mean.mm3)%>%
  mutate(c_divide_se.mm3 = 0.434*divide_se.mm3)%>%
  mutate(CV=volume_std.mm3/volume_mean.mm3)%>%
  mutate(Perc=volume_std.mm3/volume_mean.mm3*100)


larvae_data <- long_resp%>%
  select(ID, pikomol.larva.min, volume_mean.mm3, volume_se.mm3, log_resp, log_volume.mm3)
#Notes
#data excluding
#Date: 10/26/2020
#Temp: 26.7C
#Genotype: E
#Number of larvae: 6
#Volume: 2 mL
#larvae as spheroid (aka ellipsoid) = π/6*max length*max width^2 (Van Moorsel 1983)


#write_xlsx(list(data=larvae_data),"Data/Larvae/R_larvae_resp_size.xlsx") 

#Checking data
mod <- lm(log_resp~log_volume.mm3, data=long_resp)
summary(mod)

#Keeping all data
#Residual standard error: 0.082 on 14 degrees of freedom
#Multiple R-squared:  0.4098,	Adjusted R-squared:  0.3705 
#F-statistic: 10.42 on 1 and 15 DF,  p-value: 0.005639

#(y=0.42309x+1.68972; Adj R2: 0.37, P=0.005) +- 0.13110

qqPlot(mod)
plot(mod)

#### OUTLIERS
cooks <- cooks.distance(lm(log_resp~log_volume.mm3, data=long_resp))
cooks
plot(cooks)

#4/N
#N=number of observations
#K=number of explanatory variables 
#4/17 = 0.235
#5, 15, taking these points didn't do anything to R2 or P value, so left in

#long_resp_no_outliers <- long_resp%>%
#  filter(ID!=23)%>%
#  filter(ID!=12)

ggplot(long_resp, aes(x=log_volume.mm3, y= log_resp))+
  geom_point(size=1, stroke=1, alpha = 1)+
  theme_classic(base_size=12)+
  geom_errorbar(aes(xmin=log_volume.mm3-c_divide_se.mm3, xmax=log_volume.mm3+c_divide_se.mm3), position = "identity", stat = "identity")+
  labs(x=(expression(paste("Log Volume ", (mm^3)))), y=(expression(paste("Log Respiration (", "pmol", " ", O[2], " ",larva^-1, " ", min^-1,")"))))+
  #scale_x_continuous(breaks=seq(-0.3,0.8,0.3),limits=c(-0.3,0.8))+
  #scale_y_continuous(breaks=seq(1.5,2,0.1),limits=c(1.5,2))+
  geom_smooth(method="lm", color="black", size=0.5)+
  annotate("text", x=0.5, y=1.58, size=5, colour="black", 
           label= "y=0.42x+1.69")+
  annotate("text", x=0.5, y=1.55, size=5, colour="black",
           label="R^{2}==0.37", parse=T)


#ANOVA STUFF
anova(mod)
aov(mod)
Anova(mod)


# Type I ANOVA - aov()
#aov(time.lm)

# Type II ANOVA - Anova(type = 2)
#car::Anova(time.lm, type = 2)

# Type III ANOVA - Anova(type = 3)
#car::Anova(time.lm, type = 3)





