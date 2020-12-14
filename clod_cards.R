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


setwd("/Users/nina/Projects/Moorea Oct 2020/Clod_cards")
clod <-read_excel("Clod_Cards_2020.xlsx",sheet="Data")
str(clod)

clod$Treatment <- as.factor(clod$Treatment)

mod <- lm(Difference~Treatment*Rep, data=clod)
anova(mod)
tukey_hsd(mod)

clod_sum<- clod%>%
  dplyr::filter(Rate_g.hr!="NA")%>% #filtering out clods I lost
  group_by(Treatment)%>%
  summarise(mean=mean(Rate_g.hr),
            stdev=sd(Rate_g.hr),
            se=(sd(Rate_g.hr)/sqrt(length(Rate_g.hr))))%>%
  mutate(Treatment = fct_relevel(Treatment, "control","fine", "medium", "coarse", "ambient"))

 
ggplot(clod_sum)+
  aes(Treatment, mean, color=Treatment)+
  geom_bar(width = 0.7, position = position_dodge(width=0.7), stat="identity")+
  theme_classic(base_size=12)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, width=0),position=position_dodge(1))+
  labs(x="Treatment",y="Grams lost per hour")+
  theme(legend.position="none")
  
  





