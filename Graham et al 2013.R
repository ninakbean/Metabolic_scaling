library(popbio)
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats) 
library(lme4)
library(writexl) #write_xlsx(list(Sheet1=df1,Sheet2=df2),"mydata.xlsx")
library(stringr)
library(metafor)

############ RESPIRATION #####################
setwd("/Users/nina/Research/Research Ideas/AA Larval Energy Budget")
resp <-read_excel("Graham et al 2013 resp.xlsx",sheet="Sheet1") #to read your file
str(resp)

resp <- resp%>%
  mutate(Day=Day_OG+1)

Anas <- resp%>%
  filter(Species=="Acropora nasuta")

Aspat <- resp%>%
  filter(Species=="Acropora spathulata")

Aten <- resp %>%
  filter(Species=="Acropora tenuis")

Gasp <- resp %>%
  filter(Species=="Goniastrea aspera")

ggplot(resp, aes(x=Day, y=Respiration, col=Species))+
  geom_point()+
  theme_classic()

ggplot(Anas, aes(x=Day, y=Respiration))+
  geom_point()+
  theme_classic()

ggplot(Aspat, aes(x=Day, y=Respiration))+
  geom_point()+
  theme_classic()

ggplot(Aten, aes(x=Day, y=Respiration))+
  geom_point()+
  theme_classic()

ggplot(Gasp, aes(x=Day, y=Respiration))+
  geom_point()+
  theme_classic()

#looks good

avgresp <- resp%>%
  group_by(Species,Day)%>%
  summarise(avg=mean(Respiration[!is.na(Respiration)],na.rm=TRUE),
            stdev=sd(Respiration[!is.na(Respiration)],na.rm=TRUE),
            se=(sd(Respiration[!is.na(Respiration)],na.rm=TRUE))/sqrt(NROW(na.omit(Respiration))))  #To count the number of cells after omitting the blank cells.                                                                                #Ex. I have 15 cells, 3 are blank. 12 have info, i.e. 12 are samples. This is important to know to get standard error.

#write_xlsx(list(Data=avgresp),"Graham et al 2013 resp avg.xlsx") 

############ TOTAL LIPIDS FROM SUPP #####################
setwd("/Users/nina/Research/Research Ideas/AA Larval Energy Budget")
lipids <-read_excel("Graham et al 2013 lipids.xlsx",sheet="Sheet1") #to read your file
str(lipids)

lipids <- lipids%>%
  mutate(Day=Day_OG+1)

Anas <- lipids%>%
  filter(Species=="Acropora nasuta")

Aspat <- lipids%>%
  filter(Species=="Acropora spathulata")

Aten <- lipids %>%
  filter(Species=="Acropora tenuis")

Gasp <- lipids %>%
  filter(Species=="Goniastrea aspera")

ggplot(lipids, aes(x=Day, y=Lipids, col=Species))+
  geom_point()+
  theme_classic()

ggplot(Anas, aes(x=Day, y=Lipids))+
  geom_point()+
  theme_classic()

ggplot(Aspat, aes(x=Day, y=Lipids))+
  geom_point()+
  theme_classic() 

ggplot(Aten, aes(x=Day, y=Lipids))+
  geom_point()+
  theme_classic()

ggplot(Gasp, aes(x=Day, y=Lipids))+
  geom_point()+
  theme_classic() 

avglipids <- lipids%>%
  group_by(Species,Day)%>%
  summarise(avg=mean(Lipids[!is.na(Lipids)],na.rm=TRUE),
            stdev=sd(Lipids[!is.na(Lipids)],na.rm=TRUE),
            se=(sd(Lipids[!is.na(Lipids)],na.rm=TRUE))/sqrt(NROW(na.omit(Lipids))))  #To count the number of cells after omitting the blank cells.                                                                                #Ex. I have 15 cells, 3 are blank. 12 have info, i.e. 12 are samples. This is important to know to get standard error.

#write_xlsx(list(Data=avglipids),"Graham et al 2013 lipids avg.xlsx") 

