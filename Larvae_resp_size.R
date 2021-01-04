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

#volume of a spheroid (aka ellipsoid) = Pi/6*max length*max width^2 (Van Moorsel 1983)

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

#Edmunds 2011: Density of larva is slightly above that of seawater (Spencer Davies, 1989)
#density of tropical seawater (and larva) ~1.023 mg/mm^3 or ~1023 micrograms/mm^3
#(Henry and Torres 2013): Flabellum impensum (avg): 
#tissue weight was 20% of total buoyant weight

long_resp <- resp%>%
  filter(ID!="101")%>% #not long enough incubation, really low number
  filter(ID!="42")%>% #42 no size
  #Error bars for volume
  mutate(divide_se.mm3 = volume_se.mm3/volume_mean.mm3)%>%
  mutate(c_divide_se.mm3 = 0.434*divide_se.mm3)%>% 
  #Converting volume -> wet weight -> dry weight 
  #for MEAN
  mutate(Wet_weight.mg = (volume_mean.mm3*1.023))%>%
  mutate(Dry_weight.mg=Wet_weight.mg*0.2)%>%
  #for SE
  mutate(Wet_weight.mg.SE = (volume_se.mm3*1.023))%>%
  mutate(Dry_weight.mg.SE=Wet_weight.mg.SE*0.2)%>%
  #Error bars for dry weight
  mutate(divide_SE.mg = Dry_weight.mg.SE/Dry_weight.mg)%>% 
  mutate(c_divide_SE.mg = 0.434*divide_SE.mg)
  
larvae_data <- long_resp%>% 
  select(ID, pikomol.larva.min, volume_mean.mm3, volume_se.mm3, Dry_weight.mg,
         Dry_weight.mg.SE, c_divide_SE.mg)

#Notes
#data excluding, so put in notes tab
#Date: 10/26/2020
#Temp: 26.7C
#Genotype: E
#Number of larvae: 6
#Volume: 2 mL
#larvae as spheroid (aka ellipsoid) = Pi/6*max length*max width^2 (Van Moorsel 1983)

#write_xlsx(list(data=larvae_data),"Data/Larvae/R_larvae_resp_size.xlsx") 
