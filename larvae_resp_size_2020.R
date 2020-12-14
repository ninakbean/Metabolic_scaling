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
setwd("/Users/nina/Projects/Moorea Oct 2020/Larvae") 
size <-read_excel("Respiration_size_2020.xlsm",sheet="Size")
str(size)

size$ID <- as.factor(size$ID)

model <- lm(volume~ID, data=size)
anova(model)
groups <- tukey_hsd(model)

size_ID <- size%>%
  group_by(ID)%>%
  summarise(mean=mean(volume),
            stdev=sd(volume),
            se=(sd(volume)/sqrt(length(volume))))

ggplot(size_ID)+
  aes(ID, mean)+
  geom_bar(width = 0.7, position = position_dodge(width=0.7), stat="identity")+
  theme_classic(base_size=12)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, width=0),position=position_dodge(1))+
  labs(x="Vial ID",y="Volume (mm^3) Assuming the shape of an ellipsoid")
  
###################### RESPIRATION #######################
setwd("/Users/nina/Projects/Moorea Oct 2020/Larvae")
resp <-read_excel("Respiration_size_2020.xlsm",sheet="Respiration")
str(resp)

long_resp <- resp%>%
  filter(ID!="101")%>% #not long enough incubation, really low number
  filter(ID!="42")%>% #42 no size
  filter(Category!="control")%>%
  #mutate(volume_SE_wins = Winsorize(volume_SE, na.rm=TRUE))%>% #this told me that >0.7 is an outlier (also the 0.12, but we want less variation)
  #select(volume_SE, volume_SE_wins)
 #filter(!volume_SE>0.7)%>% #Taking out corals that have too large of a SE value. How to pick this number without chosing? Tukey hsd?
                            #ones that are significantly similar with high p value. 
  mutate(log_resp = log10(pikomol.larva.min))%>% #y axis
  mutate(log_volume = log10(volume))%>% #x axis
  mutate(log_volume_min = log10(volume-volume_SE))%>% #Can use this for error bars too, but asymetrical
  mutate(log_volume_max = log10(volume+volume_SE))%>% #Can use this for error bars too, but asymetrical
  mutate(divide_SE = volume_SE/volume)%>%
  mutate(c_divide_SE = 0.434*divide_SE)%>%
  mutate(CV=volume_STDEV/volume)%>%
  mutate(Perc=volume_STDEV/volume*100)

#Checking data
mod <- lm(log_resp~log_volume, data=long_resp)
summary(mod) 



#Keeping all data
#Residual standard error: 0.082 on 14 degrees of freedom
#Multiple R-squared:  0.4098,	Adjusted R-squared:  0.3705 
#F-statistic: 10.42 on 1 and 15 DF,  p-value: 0.005639

#(y=0.42309x+1.68972; Adj R2: 0.37, P=0.005) +- 0.13110

qqPlot(mod)
plot(mod)

#### OUTLIERS
cooks <- cooks.distance(lm(log_resp~log_volume, data=long_resp))
cooks
plot(cooks)

#4/N
#N=number of observations
#K=number of explanatory variables 
#4/17 = 0.235
#5, 15, taking these points didn't do anything to R2 or P value, so left in

long_resp_no_outliers <- long_resp%>%
  filter(ID!=23)%>%
  filter(ID!=12)

ggplot(long_resp, aes(x=log_volume, y= log_resp))+
  geom_point(size=1, stroke=1, alpha = 1)+
  theme_classic(base_size=12)+
  geom_errorbar(aes(xmin=log_volume-c_divide_SE, xmax=log_volume+c_divide_SE), position = "identity", stat = "identity")+
  labs(x=(expression(paste("Log Volume ", (mm^3)))), y=(expression(paste("Log Respiration (", "pmol", " ", O[2], " ",larva^-1, " ", min^-1,")"))))+
  scale_x_continuous(breaks=seq(-0.3,0.8,0.3),limits=c(-0.3,0.8))+
  scale_y_continuous(breaks=seq(1.5,2,0.1),limits=c(1.5,2))+
  geom_smooth(method="lm", color="black", size=0.5)+
  annotate("text", x=0.5, y=1.58, size=5, colour="black", label= (expression(paste("y=0.42x+1.69"))))+
  annotate("text", x=0.5, y=1.55, size=5, colour="black", label= (expression(paste(r^2, "=0.37"))))


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





