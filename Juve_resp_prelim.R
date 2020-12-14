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


setwd("/Users/nina/Projects/Moorea Oct 2020/Juveniles/Respiration_Prelim")
resp <-read_excel("practice_consolidated.xlsx",sheet="11072020")
str(resp)

resp$ID <- as.factor(resp$ID)

results <- resp %>% 
  group_by(ID,Surface_area) %>%
  do(tidy(lm(Oxygen ~ Elapsed_time_min, data = .)))%>%
  filter(term == "Elapsed_time_min") %>%
  select(-term)


ggplot(resp,aes(x=Surface_area, y=Oxygen, group =ID))+
  geom_point(aes(color = ID), size=2, stroke=1, alpha = 1)+
  #scale_shape_manual(values=c(2, 1))+
  #scale_color_manual(values=c("#F8A42F", "#FF4605", "#860111"))+
  theme(legend.position="right")+
  labs(x="Time (min)",y="Oxygen")+
  theme_classic(base_size=12)
  geom_smooth(method="lm", color="black", size=0.5)
  scale_y_continuous(breaks=seq(80,105,10),limits=c(80,105))
  #annotate("text", x=2.3, y=2.5, size=5, label= expression(paste(R^2 , " = 0.025, p=0.52")))+ #brooding

ancova <- aov(Oxygen ~ ID, data= resp) #ANCOVA mod w/ interaction
summary(ancova)
tukey_hsd(ancova)

#Pocillopora 1
poc1 <- resp%>%
  filter(ID=="Pocillopora_1")%>%
  filter(Elapsed_time_min<50)

poc1_model <-  lm(Oxygen~Elapsed_time_min, data=poc1)
summary(poc1_model)
#10 min: R2= 0.96
#20 min: R2= 0.99
#30 min: R2= 0.996
#40 min: R2= 0.997, only ran for 40 min

#pocillopora 3
poc3 <- resp%>%
  filter(ID=="Pocillopora_3")
  filter(Elapsed_time_min<60)

poc3_model <-  lm(Oxygen~Elapsed_time_min, data=poc3)
summary(poc3_model)

#10 min: R2= 87
#20 min: R2= 978
#30 min: R2= 9915
#40 min: R2= 9959
#50 min: R2= 9976
#60 min: R2= 9986

#Porites #2
porit2 <- resp%>%
  filter(ID=="Porites_2")%>%
  filter(Elapsed_time_min<60)

porit2_model <-  lm(Oxygen~Elapsed_time_min, data=porit2)
summary(porit2_model)

#10 min: R2= 9494
#20 min: R2= 9838
#30 min: R2= 9925
#40 min: R2= 9959
#50 min: R2= 9976
#60 min: R2= 9976

#Control
control <- resp%>%
  filter(ID=="control")%>%
  filter(Elapsed_time_min>0)

control_model <-  lm(Oxygen~Elapsed_time_min, data=control)
summary(control_model)

#10 min: R2= 2637
#20 min: R2= 3749
#30 min: R2= 6243
#40 min: R2= 7792
#50 min: R2= 8642
#60 min: R2= 908

#Run controls for an hour..
#30 min incubaton for corals is good.. 

resp_sum <- resp%>% 
  group_by(ID) %>% 
  mutate(reg = map(resp, ~lm(Oxygen~Elapsed_time_min, .)))   # do the regression
  mutate(intercept=map_dbl(reg, ~coefficients(.)[1]),         # get values form regression
         slope=map_dbl(reg, ~coefficients(.)[2]))
  
  
  
  
  
  
  nest() %>%
  mutate(slope = map(resp, ~lm(Oxygen ~ Elapsed_time_min, data = resp))) %>% 
  unnest(slope)


