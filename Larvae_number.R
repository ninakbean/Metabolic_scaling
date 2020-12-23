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

###################### HOW GOOD ARE MY SIZE ESTIMATES? #######################
number <-read_excel("Data/Larvae/larvae_number_respiration.xlsx",sheet="Respiration")
str(number)

long_num <- number%>%
  filter(Date!="10272020")%>% #this day, the larvae were all jumbled in age and genotype
  mutate(raw.nmol.vial.min=raw.umol.vial.min*1000)
  
ggplot(long_num, aes(x=Number_larvae, y= raw.nmol.vial.min))+
  geom_point(size=1, stroke=1, alpha = 1)+
  theme_classic(base_size=12)+
  labs(x="Number of larvae", y=(expression(paste("Respiration (", "nmol", " ", O[2], " ",vial^-1, " ", min^-1,")"))))+
  scale_x_continuous(breaks=seq(0,10,1),limits=c(0,10))+
  #scale_y_continuous(breaks=seq(1.5,2,0.1),limits=c(1.5,2))+
  geom_smooth(method="lm", color="black", size=0.5)+
  annotate("text", x=6, y=0.2, size=5, colour="black", 
           label= "y=0.078x+0.045")+
  annotate("text", x=6, y=0.10, size=5, colour="black",
           label="R^{2}==0.95", parse=T)

model <- lm(raw.nmol.vial.min~Number_larvae, data=long_num)
summary(model)

test <- long_num%>%
  filter(!Number_larvae %in% c(0,1,2))

model <- lm(raw.nmol.vial.min~Number_larvae, data=test)
summary(model)
#as you take out the smaller trials, the R2 gets worse

