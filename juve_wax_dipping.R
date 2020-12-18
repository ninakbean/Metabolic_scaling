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

wax <-read_excel("Data/Juveniles/Data/Juve_wax_dipping_2020.xlsx",sheet="Master")
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
  mutate(SA.shape.cm2.hemi=(2*3.14*(height.mm/10)*((max1.mm/2)/10)*((max2.mm/2)/10)))%>% #hemi-ellopsoid, R2:~0.89
  mutate(SA.shape.cm2.dome=(2*3.14*(height.mm/10)*(((Diameter/2)/10)))) #dome, R2: ~0.84

hemi <- lm(SA.dip2.cm2~SA.shape.cm2.hemi, data=Porites)
dome <- lm(SA.dip2.cm2~SA.shape.cm2.dome, data=Porites)

AIC(hemi, dome) #hemi is better
  
Port_dip1 <- Porites%>%
filter(weight.g!="NA") #Taking out samples where I forgot to get OG weight
                         #not necessary for dip 2
#Reduced data set: 1st dip
Porites_model.dip1 <-  lm(dip1_change~SA.shape.cm2.hemi, data=Port_dip1)
summary(Porites_model.dip1)

#Reduced data set: 2nd dip, higher R2 value
Porites_model.dip2 <-  lm(dip2_change~SA.shape.cm2.hemi, data=Port_dip1)
summary(Porites_model.dip2)

AIC(Porites_model.dip1, Porites_model.dip2) #very similar, within 1 point

ggplot(Porites,aes(x=SA.shape.cm2.hemi, y=SA.dip2.cm2))+
  geom_point(size=2, stroke=1, alpha = 1)+
  #scale_shape_manual(values=c(2, 1))+
  #scale_color_manual(values=c("#F8A42F", "#FF4605", "#860111"))+
  theme(legend.position="right")+
  labs(x=(expression(paste("Geometric Shape SA (", cm^2,")"))),
       y=(expression(paste("Wax Dip SA (",cm^2,")"))))+
  theme_classic(base_size=12)+
  geom_smooth(method="lm", color="black", size=0.5)+
  annotate("text", x=30, y=10, size=5, colour="black", 
           label= "y=1.1x+4.75")+
  annotate("text", x=30, y=6, size=5, colour="black",
           label="R^{2}==0.90", parse=T)

Porites_model <-  lm(SA.dip2.cm2~SA.shape.cm2.hemi, data=Porites)
summary(Porites_model)

#now: y=1.13x+4.74755
#now: dip 1.13 cm2 = estimated 1 cm2

############ POCILLOPORA ##################################################

Pocillopora <- master%>%
  filter(Species=="Pocillopora")%>%
  mutate(Diameter=rowMeans(cbind(max1.mm,max2.mm), na.rm=T))%>%
  mutate(SA.shape.cm2.hemi=(2*3.14*(height.mm/10)*((max1.mm/2)/10)*((max2.mm/2)/10)))%>% #hemi-ellopsoid, R2:~0.945
  mutate(SA.shape.cm2.dome=(2*3.14*(height.mm/10)*(((Diameter/2)/10)))) #dome, R2: ~0.93

hemi <- lm(SA.dip2.cm2~SA.shape.cm2.hemi, data=Pocillopora)
dome <- lm(SA.dip2.cm2~SA.shape.cm2.dome, data=Pocillopora)

AIC(hemi,dome) #hemi is better

Poc_dip1 <- Pocillopora%>%
  filter(weight.g!="NA") #Taking out samples where I fogot to get OG weight

#Reduced data set: 1st dip
Pocillopora_model.dip1 <-  lm(dip1_change~SA.shape.cm2.hemi, data=Poc_dip1)
summary(Pocillopora_model.dip1)

#Reduced data set: 2nd dip, higher R2 value
Pocillopora_model.dip2 <-  lm(dip2_change~SA.shape.cm2.hemi, data=Poc_dip1)
summary(Pocillopora_model)

AIC(Pocillopora_model.dip1, Pocillopora_model.dip2)
#dip 2 is way better

ggplot(Pocillopora,aes(x=SA.shape.cm2.hemi, y=SA.dip2.cm2))+
  geom_point(size=2, stroke=1, alpha = 1)+
  #scale_shape_manual(values=c(2, 1))+
  #scale_color_manual(values=c("#F8A42F", "#FF4605", "#860111"))+
  theme(legend.position="right")+
  labs(x=(expression(paste("Geometric Shape SA (", cm^2,")"))),
       y=(expression(paste("Wax Dip SA (",cm^2,")"))))+
  theme_classic(base_size=12)+
  geom_smooth(method="lm", color="black", size=0.5)+
  annotate("text", x=30, y=13, size=5, colour="black", 
         label= "y=1.6x+5.2")+
  annotate("text", x=30, y=6, size=5, colour="black",
           label="R^{2}==0.95", parse=T)

#All data
Pocillopora_model <-  lm(SA.dip2.cm2~SA.shape.cm2.hemi, data=Pocillopora)
summary(Pocillopora_model)

#Now: y=1.628x+5.18
#Now: 1.628 wax dip cm2 = 1 estimate cm2

consolidated <- rbind(Pocillopora, Porites)%>%
  select(-Notebook_order, -ID, -Diameter, -dip1_change, -SA.shape.cm2.dome)

#Notes
#SA.dip2.cm2 (Surface area from change in wax weight)=weight of dip 2 in grams* calibration curve slope 
#SA.shape.cm2.hemi (Surface area estimated from geometry assuming both corals are a
#hemi-ellipsoid= (2*3.14*height*radius1*radius2)

#write_xlsx(list(data=consolidated),"Data/Juveniles/Data/R_wax_dipping.xlsx")

ggplot(consolidated,aes(x=SA.shape.cm2.hemi, y=SA.dip2.cm2, group=Species))+
  geom_point(aes(color = Species), size=2, stroke=1, alpha = 1)+  #scale_shape_manual(values=c(2, 1))+
  scale_color_manual(values=c("#F8A42F", "#FF4605"))+
  theme(legend.position="right")+
  labs(x=(expression(paste("Geometric Shape SA (", cm^2,")"))),
       y=(expression(paste("Wax Dip SA (",cm^2,")"))))+
  theme_classic(base_size=12)+
  geom_smooth(method="lm", color="black", size=0.5)+
  annotate("text", x=10, y=75, size=5, colour="#bf9000", 
           label= "y=1.6x+5.2")+ #Pocillopora
  annotate("text", x=10, y=68, size=5, colour="#bf9000",
           label="R^{2}==0.95", parse=T)+ #Pocillopora
  annotate("text", x=40, y=13, size=5, colour="#FF4605", 
           label= "y=1.1x+4.75")+ #Porites
  annotate("text", x=40, y=6, size=5, colour="#FF4605",
           label="R^{2}==0.90", parse=T) #Porites


