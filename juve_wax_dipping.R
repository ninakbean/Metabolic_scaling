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

setwd("/Users/nina/Projects/Moorea Oct 2020/Juveniles")
wax <-read_excel("Wax_dipping.xlsx",sheet="Master")
str(wax)

############ CONTROLS ##################################################
Control <- wax%>% 
  filter(Species=="Control")%>%
  mutate(SA.shape.cm2=2*3.14*(((max1.mm/2)/10)*(height.mm/10)+2*3.14*(((max1.mm/2)/10)^2))) #cylinder + one circle

#Reduced data set: 1st dip, higher R2 value
Control_model <-  lm(dip1_change~SA.shape.cm2, data=Control)
summary(Control_model)

#Reduced data set: 2nd dip, for corals, 2 dips is better. Similar R2
Control_model <-  lm(dip2_change~SA.shape.cm2, data=Control)
summary(Control_model)

#y= 0.0134144 x-0.0236067 
# 0.0134144 g = 1 cm^2

ggplot(Control,aes(x=SA.shape.cm2, y=dip2_change))+
  geom_point(size=2, stroke=1, alpha = 1)+
  theme(legend.position="right")+
  labs(x="Geometric Shape SA (cm^2)",y="Wax Dip (g)")+
  theme_classic(base_size=12)+
  geom_smooth(method="lm", color="black", size=0.5)

# Transforming grams to known surface area
master <- wax%>%
  mutate(SA.dip2.cm2=dip2_change/0.0134144)

############ PORITES ##################################################
Porites <- master%>% 
  filter(Species=="Porites")%>%
  mutate(Diameter=rowMeans(cbind(max1.mm,max2.mm), na.rm=T))%>%
  mutate(SA.shape.cm2=(2*3.14*(height.mm/10)*(((Diameter/2)/10)))) #same as pocillopora?

Port_dip1 <- Porites%>%
  filter(weight.g!="NA") #Taking out samples where I fogot to get OG weight

#Reduced data set: 1st dip
Porites_model <-  lm(dip1_change~SA.shape.cm2, data=Port_dip1)
summary(Porites_model)

#Reduced data set: 2nd dip, higher R2 value
Porites_model <-  lm(dip2_change~SA.shape.cm2, data=Port_dip1)
summary(Porites_model)

ggplot(Porites,aes(x=SA.shape.cm2, y=SA.dip2.cm2))+
  geom_point(size=2, stroke=1, alpha = 1)+
  #scale_shape_manual(values=c(2, 1))+
  #scale_color_manual(values=c("#F8A42F", "#FF4605", "#860111"))+
  theme(legend.position="right")+
  labs(x="Geometric Shape SA (cm^2)",y="Wax Dip SA (cm^2)")+
  theme_classic(base_size=12)+
  geom_smooth(method="lm", color="black", size=0.5)

Porites_model <-  lm(SA.dip2.cm2~SA.shape.cm2, data=Porites)
summary(Porites_model)

#y=1.6199x+1.1680 
#dip 1.6199 cm2 = estimated 1 cm2

############ POCILLOPORA ##################################################

Pocillopora <- master%>%
  filter(Species=="Pocillopora")%>%
  mutate(Diameter=rowMeans(cbind(max1.mm,max2.mm), na.rm=T))%>%
  mutate(SA.shape.cm2=(2*3.14*(height.mm/10)*(((Diameter/2)/10))))

Poc_dip1 <- Pocillopora%>%
  filter(weight.g!="NA") #Taking out samples where I fogot to get OG weight

#Reduced data set: 1st dip
Pocillopora_model <-  lm(dip1_change~SA.shape.cm2, data=Poc_dip1)
summary(Pocillopora_model)

#Reduced data set: 2nd dip, higher R2 value
Pocillopora_model <-  lm(dip2_change~SA.shape.cm2, data=Poc_dip1)
summary(Pocillopora_model)

ggplot(Pocillopora,aes(x=SA.shape.cm2, y=SA.dip2.cm2))+
  geom_point(size=2, stroke=1, alpha = 1)+
  #scale_shape_manual(values=c(2, 1))+
  #scale_color_manual(values=c("#F8A42F", "#FF4605", "#860111"))+
  theme(legend.position="right")+
  labs(x="Geometric Shape SA (cm2)",y="Wax Dip SA (cm2)")+
  theme_classic(base_size=12)+
  geom_smooth(method="lm", color="black", size=0.5)

#All data
Pocillopora_model <-  lm(SA.dip2.cm2~SA.shape.cm2, data=Pocillopora)
summary(Pocillopora_model)

#y=2.9009x-5.6441
#2.9009 wax dip cm2 = 1 estimate cm2
