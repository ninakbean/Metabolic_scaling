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

juve <-read_excel("Data/Juveniles/Data/R_juve_resp_size.xlsx",sheet="data")
larvae <-read_excel("Data/Larvae/R_larvae_resp_size.xlsx",sheet="data")
data_comp <- read_excel("Data/Data_comp/my_data/Meta_analysis.xlsx", sheet="Repiration.Photosynthesis")

#Edmunds 2011: Density of larva is slightly above that of seawater (Spencer Davies, 1989)
#density of tropical seawater (and larva) ~1.023 mg/mm^3 or ~1023 micrograms/mm^3
#(Henry and Torres 2013): Flabellum impensum (avg): 
#tissue weight was 20% of total buoyant weight

larv <- larvae %>%
  mutate(Wet_weight.mg = (volume_mean.mm3*1.023))%>%
  mutate(Dry_weight.mg=Wet_weight.mg*0.8)%>%
  mutate(Dry_weight.g=Dry_weight.mg/1000)%>%
  mutate(micromol.larva.min=pikomol.larva.min/1000000)%>%
  rename(micromol.ind.min = micromol.larva.min)%>%
  select(micromol.ind.min,Dry_weight.g)%>%
  mutate(Species=c("Pocillopora"))%>%
  mutate(Stage=c("larvae"))%>%
  unite(Type, Species, Stage, sep = "_", remove = FALSE, na.rm = FALSE)

  
juv <- juve %>%
  rename(micromol.ind.min = micromol.coral.min)%>%
  select(Species, micromol.ind.min, Dry_weight.g)%>%
  mutate(Stage=c("juvenile"))%>%
  unite(Type, Species, Stage, sep = "_", remove = FALSE, na.rm = FALSE)

combined <- rbind(larv, juv)

ggplot(combined,aes(x=log10(Dry_weight.g*1000), y=log10(micromol.ind.min*60), group = Type))+
  geom_point(aes(color = Species), size=2, stroke=1, alpha = 1)+
  scale_color_manual(values=c("#F8A42F", "#FF4605"))+ 
  theme(legend.position="right")+
  labs(x="Log Dry Tissue (mg)",y=(expression(paste("Log Respiration (", mu , "mol", " ", O[2], " ", coral^-1, " ", h^-1,")"))))+
  theme_classic(base_size=12)+
  geom_smooth(method="lm", color="black", size=0.5)

#Can't find any porites larvae in literature (based on a quick search and 
#my data comp)
past <- data_comp%>%
  filter(Species=="Porites astreoides")







