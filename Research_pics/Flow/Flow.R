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
setwd("/Users/nina/Projects/Moorea Oct 2020/Flow")
flow <-read_excel("Flow.xlsm",sheet="Flow")
str(flow)

flow$Category <- as.factor(flow$Category)

model <- lm(speed~Category, data=flow)
anova(model)
groups <- tukey_hsd(model)

flow_sum <- flow%>%
  group_by(Category)%>%
  summarise(mean=mean(speed),
            stdev=sd(speed),
            se=(sd(speed)/sqrt(length(speed))))

ggplot(flow_sum)+
  aes(Category, mean)+
  geom_bar(width = 0.7, position = position_dodge(width=0.7), stat="identity")+
  theme_classic(base_size=12)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, width=0),position=position_dodge(1))

###################### RESPIRATION #######################
setwd("/Users/nina/Projects/Moorea Oct 2020/Larvae")
resp <-read_excel("Respiration_size_2020.xlsm",sheet="Respiration")
str(resp)

long_resp <- resp%>%
  filter(ID!="101")%>% #not long enough incubation, really low number
  filter(ID!="42")%>% #42 no size
  #mutate(volume_SE_wins = Winsorize(volume_SE, na.rm=TRUE))%>% #this told me that >0.7 is an outlier (also the 0.12, but we want less variation)
  #select(volume_SE, volume_SE_wins)
  filter(!volume_SE>0.7)%>% #Taking out corals that have too large of a SE value. How to pick this number without chosing? Tukey hsd?
  #ones that are significantly similar with high p value. 
  
  mutate(log_resp = log10(pikomol.larva.min))%>%
  mutate(log_size = log10(volume))%>%
  mutate(log_size_SE = log10(volume_SE))%>%
  mutate(CV=volume_STDEV/volume)%>%
  mutate(Perc=volume_STDEV/volume*100)


ggplot(long_resp, aes(x=log_size, y= log_resp))+
  geom_point()+
  theme_classic(base_size=12)+
  geom_errorbar(aes(xmin=log_size-volume_SE, xmax=log_size+volume_SE), position = "identity", stat = "identity")+
  labs(x="Age",y="Normalized Respiraation Rate \n (pmol O2 μg DW min)")+
  scale_x_continuous(breaks=seq(-0.1,0.1,0.7),limits=c(-1,1))+
  scale_y_continuous(breaks=seq(0,1,2),limits=c(1,2))+
  geom_smooth(method="lm")

mod <- lm(log_resp~log_size, data=long_resp)
summary(mod) 

anova(mod)
aov(mod)
Anova(mod)


# Type I ANOVA - aov()
#aov(time.lm)

# Type II ANOVA - Anova(type = 2)
#car::Anova(time.lm, type = 2)

# Type III ANOVA - Anova(type = 3)
#car::Anova(time.lm, type = 3)
