library(popbio)
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats) 
library(lme4)
library(writexl) #write_xlsx(list(Sheet1=df1,Sheet2=df2),"mydata.xlsx")

setwd("/Users/nina/Research/Research Ideas/AA Larval Energy Budget")
meta <-read_excel("Meta_analysis.xlsx",sheet="Repiration.Photosynthesis") #to read your file
str(meta)

################### SIMPLIFYING DATA SHEET R->EXCEL ####################
datacomp <- meta%>%
  select(-Survivorship_duration_hrs,-survivorship_SE,-survivorship_perc,
         -settled_hrs,-settled_SE,-settled_perc,-Fv.Fm_SE,-Fv.Fm,
         -Symbiodinium.larva,-Symbiodinium.larva_SE,-larvae_resp, #larvae_resp is for the number of larvae used in resp trials
         -Days_post_settle, -Area_mm2, -Area_SE)%>% #Area was used to possibly include for biomass estimates
  filter(Life_stage=="larvae")%>%
  filter(OA_type=="ambient")%>%   #Not looking at OA
  select(-OA_type,-pCO2_microatm)


#only difference is that the rows are averaged across release day
#Did not include Life stage (they are all larvae)
datacompavg <- datacomp%>% 
  group_by(Study,Location,Species,Morph,Reproduction_mode,Symbionts,  
           Days_post_release,Treatment_Temp,Temp_exposure_min)%>%
  summarise_all(funs(mean), na.rm=TRUE) #averages all the data that are not grouped
                                        #NAs are not included


#Making species a replicate
#This only made the # of rows go from 134 to 132
#So I'll only make two sheets
#Took out Study, Location, Morph, Symbionts, Temp_exposure_min
datacomptrends <- datacompavg%>%
  group_by(Species, Treatment_Temp, Reproduction_mode, Days_post_release,Treatment_Temp)%>%
  summarise_all(funs(mean), na.rm=TRUE)%>%
  select(-Study,-Location,-Morph, -Symbionts,-Temp_exposure_min,-Life_stage,-n_resp,-Release_day)

str(datacomptrends)

#write_xlsx(list(Data=datacomp,Avg_across_releaseDay=datacompavg),"data_compilation.xlsx") 
#exporting data from R to excel
################### FORMATING ####################
meta <- meta %>%
  mutate(Temperature_K=Treatment_Temp+273.15)%>% #Celcius -> kelvin
  mutate("1/kT"=(1/(0.0000862*Temperature_K)))%>% #1/KT
  rename(kT="1/kT")%>% #Renaming 1/kT to kT for simplicity
  rename(Respiration="Respiration_nmolO2larvmin")%>%
  mutate(Respiration_mg = Respiration*0.032)%>%
  mutate(Respiration_g = Respiration_mg*0.000001)%>%
  mutate(Biomass_g = Biomass_micrograms.larva*0.000001)%>%
  mutate("ln(IM-3/4)"= log((Respiration_g*Biomass_g^(-3/4))))%>%
  rename(lnIM="ln(IM-3/4)")%>%
  mutate("ln(IeE/kT)"=log(Respiration_g*exp(3.25/(0.0000862*Temperature_K))))%>%
  rename(lnekt="ln(IeE/kT)") #Renaming ln(IeE/kT) to lnekt for simplicity
  replace_na(Days_post_release = 1)

str(meta)

meta$Study <- as.factor(meta$Study)
meta$Location <- as.factor(meta$Location)
meta$Species <- as.factor(meta$Species)
meta$Reproduction_mode <- as.factor(meta$Reproduction_mode)
meta$Life_stage <- as.factor(meta$Life_stage)
meta$Days_post_release <- as.numeric(meta$Days_post_release)
meta$Holding_temp <- as.numeric(meta$Holding_temp)
meta$Temp_exposure_min <- as.numeric(meta$Temp_exposure_min)
meta$settled_perc <- as.numeric(meta$settled_perc)
meta$Symbiodinium.larva <- as.numeric(meta$Symbiodinium.larva)
meta$Respiration <- as.numeric(meta$Respiration)

#TO DO:
#respiration over temp seperated by age



################### CREATING DATA SHEETS ####################
#### Just larvae, normal pCO2
larvae <-meta%>%
  filter(Life_stage %in% c("larvae"))%>%
  filter(OA_type=="ambient")

#### Just larvae, normal pCO2 & temp
ambientlarvae <- larvae%>%
  filter(Temp_type=="ambient")

symambientspecies <- ambientlarvae%>%
  group_by(Species, Days_post_release, Symbionts)%>%
  summarise(avg=mean(Respiration[!is.na(Respiration)],na.rm=TRUE),
            stdev=sd(Respiration[!is.na(Respiration)],na.rm=TRUE),
            se=(sd(Respiration[!is.na(Respiration)],na.rm=TRUE))/sqrt(NROW(na.omit(Respiration))))  #To count the number of cells after omitting the blank cells.                                                                                #Ex. I have 15 cells, 3 are blank. 12 have info, i.e. 12 are samples. This is important to know to get standard error.

symambientmodes <- symambientspecies%>%
  group_by(Days_post_release, Symbionts)%>%
  summarise(average=mean(avg[!is.na(avg)],na.rm=TRUE),
            stdev=sd(avg[!is.na(avg)],na.rm=TRUE),
            se=(sd(avg[!is.na(avg)],na.rm=TRUE))/sqrt(NROW(na.omit(avg))))  #To count the number of cells after omitting the blank cells.                                                                                #Ex. I have 15 cells, 3 are blank. 12 have info, i.e. 12 are samples. This is important to know to get standard error.


#respiration averaged within species
ambientspecies <- ambientlarvae%>%
  group_by(Species, Days_post_release, Reproduction_mode)%>%
  summarise(avg=mean(Respiration[!is.na(Respiration)],na.rm=TRUE),
            stdev=sd(Respiration[!is.na(Respiration)],na.rm=TRUE),
            se=(sd(Respiration[!is.na(Respiration)],na.rm=TRUE))/sqrt(NROW(na.omit(Respiration))))  #To count the number of cells after omitting the blank cells.                                                                                #Ex. I have 15 cells, 3 are blank. 12 have info, i.e. 12 are samples. This is important to know to get standard error.

#Respiration averaged among reproduction modes
ambientmodes <- ambientspecies%>%
  group_by(Days_post_release, Reproduction_mode)%>%
  summarise(average=mean(avg[!is.na(avg)],na.rm=TRUE),
            stdev=sd(avg[!is.na(avg)],na.rm=TRUE),
            se=(sd(avg[!is.na(avg)],na.rm=TRUE))/sqrt(NROW(na.omit(avg))))  #To count the number of cells after omitting the blank cells.                                                                                #Ex. I have 15 cells, 3 are blank. 12 have info, i.e. 12 are samples. This is important to know to get standard error.

#Biomass averaged within species
biomassspecies <- ambientlarvae%>%
  group_by(Species, Days_post_release, Reproduction_mode)%>%
  summarise(avg=mean(Biomass_micrograms.larva[!is.na(Biomass_micrograms.larva)],na.rm=TRUE),
            stdev=sd(Biomass_micrograms.larva[!is.na(Biomass_micrograms.larva)],na.rm=TRUE),
            se=(sd(Biomass_micrograms.larva[!is.na(Biomass_micrograms.larva)],na.rm=TRUE))/sqrt(NROW(na.omit(Biomass_micrograms.larva))))  #To count the number of cells after omitting the blank cells.                                                                                #Ex. I have 15 cells, 3 are blank. 12 have info, i.e. 12 are samples. This is important to know to get standard error.

#Respiration averaged among reproduction modes
biomassmodes <- biomassspecies%>%
  group_by(Days_post_release, Reproduction_mode)%>%
  summarise(average=mean(avg[!is.na(avg)],na.rm=TRUE),
            stdev=sd(avg[!is.na(avg)],na.rm=TRUE),
            se=(sd(avg[!is.na(avg)],na.rm=TRUE))/sqrt(NROW(na.omit(avg))))  #To count the number of cells after omitting the blank cells.                                                                                #Ex. I have 15 cells, 3 are blank. 12 have info, i.e. 12 are samples. This is important to know to get standard error.


#Biomass averaged within species
symbiomassspecies <- ambientlarvae%>%
  group_by(Species, Days_post_release, Symbionts)%>%
  summarise(avg=mean(Biomass_micrograms.larva[!is.na(Biomass_micrograms.larva)],na.rm=TRUE),
            stdev=sd(Biomass_micrograms.larva[!is.na(Biomass_micrograms.larva)],na.rm=TRUE),
            se=(sd(Biomass_micrograms.larva[!is.na(Biomass_micrograms.larva)],na.rm=TRUE))/sqrt(NROW(na.omit(Biomass_micrograms.larva))))  #To count the number of cells after omitting the blank cells.                                                                                #Ex. I have 15 cells, 3 are blank. 12 have info, i.e. 12 are samples. This is important to know to get standard error.

#Respiration averaged among reproduction modes
symbiomassmodes <- symbiomassspecies%>%
  group_by(Days_post_release, Symbionts)%>%
  summarise(average=mean(avg[!is.na(avg)],na.rm=TRUE),
            stdev=sd(avg[!is.na(avg)],na.rm=TRUE),
            se=(sd(avg[!is.na(avg)],na.rm=TRUE))/sqrt(NROW(na.omit(avg))))  #To count the number of cells after omitting the blank cells.                                                                                #Ex. I have 15 cells, 3 are blank. 12 have info, i.e. 12 are samples. This is important to know to get standard error.



#### Studies looking across temperatures, same age
larvae_temps <- larvae%>%
  filter(Days_post_release %in% c("1","1.166"))%>%
  filter(Study %in% c("Haryanti and Hidaka 2015", "Rivest and Hofmann 2014",
                      "Edmunds et al 2011", "Edmunds et al 2001",
                      "Rodriguez-Lanetty et al 2009","Olsen et al 2013",
                      "Ross et al 2013"))

## Just looking at protein with ambient larvae
#6 studies
protein <- ambientlarvae%>%
  filter(Study %in% c("Rivest and Hofmann 2014",
                      "Edmunds et al 2013", "Edmunds et al 2001",
                      "Cumbo et al 2012","Cumbo et al 2013b",
                      "Richmond 1987"))%>%
  select(Study, Species,Protein_micrograms.larva)

avgprotein <- protein %>%
  group_by(Study, Species)%>%
  summarise(avg=mean(Protein_micrograms.larva[!is.na(Protein_micrograms.larva)],na.rm=TRUE),
  stdev=sd(Protein_micrograms.larva[!is.na(Protein_micrograms.larva)],na.rm=TRUE),
  se=(sd(Protein_micrograms.larva[!is.na(Protein_micrograms.larva)],na.rm=TRUE))/sqrt(NROW(na.omit(Protein_micrograms.larva))))  #To count the number of cells after omitting the blank cells.                                                                                #Ex. I have 15 cells, 3 are blank. 12 have info, i.e. 12 are samples. This is important to know to get standard error.

#All are brooding and they have lots of variation

#Just looking at lipids
#2 studies
lipid <- ambientlarvae%>%
  filter(Study %in% c("Harii et al 2010","Graham et al 2013"))%>%
  select(Study, Species,Lipid_micrograms.larva)

avglipid <- lipid %>%
  group_by(Study, Species)%>%
  summarise(avg=mean(Lipid_micrograms.larva[!is.na(Lipid_micrograms.larva)],na.rm=TRUE),
            stdev=sd(Lipid_micrograms.larva[!is.na(Lipid_micrograms.larva)],na.rm=TRUE),
            se=(sd(Lipid_micrograms.larva[!is.na(Lipid_micrograms.larva)],na.rm=TRUE))/sqrt(NROW(na.omit(Lipid_micrograms.larva))))  #To count the number of cells after omitting the blank cells.                                                                                #Ex. I have 15 cells, 3 are blank. 12 have info, i.e. 12 are samples. This is important to know to get standard error.

#So P dam has about 7 times more lipids than acropora

#: length, width = volume 
#2 studies
#Edmunds 2013 
#Edmunds 2011

#dry
#2 studies
#Gaither and rowen
#Richmond

################### TEMPERATURE GRAPHS ####################
################### BROWN GRAPHS ####################
#### Brown: size corrected respiration
  #### Fig 1 ln(IM^-3/4) vs. 1/kT
    #### Seperated by species
ggplot(larvae,aes(x=kT,y=lnIM, color=Species))+
  geom_point()+
  labs(x="Temp",y="Respiration")+
  theme_classic(base_size=12)+
  geom_smooth(method="lm")

    #### Not seperated by species
ggplot(larvae,aes(x=kT,y=lnIM))+
  geom_point()+
  labs(x="Temp",y="Respiration")+
  theme_classic(base_size=12)+
  geom_smooth(method="lm")

# Getting the slope, not seperated by species
fitlarv <- lm(lnIM ~ kT, data=larvae)
summary(fitlarv)
#intercept: -137.5695
#slope: 3.2456

#intercept: -60.4607
#slope: 1.2448

#### Brown: temperature corrected respiration
  #### Fig 2 ln(Ie^(E/kT) vs. ln(mass)
    #### Seperated by species

ggplot(larvae,aes(x=log(Biomass_g),y=lnekt,color=Species))+
  geom_point()+
  labs(x="ln(Biomass)",y="Temp corrected resp")+
  theme_classic(base_size=12)+
  geom_smooth(method="lm")

#### Seperated by reproductive mode
ggplot(larvae,aes(x=log(Biomass_g),y=lnekt,color=Reproduction_mode))+
  geom_point()+
  labs(x="ln(Biomass)",y="Temp corrected resp")+
  theme_classic(base_size=12)+
  geom_smooth(method="lm")

#### Not seperated by species
ggplot(larvae,aes(x=log(Biomass_g),y=lnekt))+
  geom_point()+
  labs(x="ln(Biomass)",y="Temp corrected resp")+
  theme_classic(base_size=12)+
  geom_smooth(method="lm")

fitlnekt <- lm(lnekt ~ log(Biomass_g), data=larvae)
summary(fitlnekt)
#intercept: 106.63318
#slope:   0.12054

#Getting slopes and intercepts of just broadcasting species
brownlarvae <- larvae%>%
  filter(Reproduction_mode=="broadcasting")

fitbrownlnekt <- lm(lnekt ~ log(Biomass_g), data=brownlarvae)
summary(fitbrownlnekt)
#intercept: 114.3257
#slope:   0.7540

################### RESPIRATION vs TREATMENT TEMPERATURE ####################
#### SEPERATED BY AGE
#Making dataframe: Scatter plot: averages
graph_tempage <- larvae%>%
  group_by(Days_post_release, Treatment_Temp)%>%
  summarise(avg=mean(Respiration[!is.na(Respiration)],na.rm=TRUE),
            stdev=sd(Respiration[!is.na(Respiration)],na.rm=TRUE),
            se=(sd(Respiration[!is.na(Respiration)],na.rm=TRUE))/sqrt(NROW(na.omit(Respiration))))  #To count the number of cells after omitting the blank cells.                                                                                #Ex. I have 15 cells, 3 are blank. 12 have info, i.e. 12 are samples. This is important to know to get standard error.

#Scatter plot: averages 
ggplot(graph_tempage,aes(x=Treatment_Temp,y=avg,color=as.factor(Days_post_release)))+
  geom_point()+
  labs(x="Temp",y="Respiration")+
  theme_classic(base_size=12)+
  geom_smooth(method="auto")

#### SEPERATED BY SPECIES
#Scatter plot: all data
ggplot(larvae,aes(x=Treatment_Temp,y=Respiration,color=Species))+
  geom_point()+
  labs(x="Temp",y="Respiration")+
  theme_classic(base_size=12)+
  geom_smooth(method="lm")

#Making dataframe: Scatter plot: averages
graph_temps <- larvae%>%
  group_by(Species, Treatment_Temp)%>%
  summarise(avg=mean(Respiration[!is.na(Respiration)],na.rm=TRUE),
            stdev=sd(Respiration[!is.na(Respiration)],na.rm=TRUE),
            se=(sd(Respiration[!is.na(Respiration)],na.rm=TRUE))/sqrt(NROW(na.omit(Respiration))))  #To count the number of cells after omitting the blank cells.                                                                                #Ex. I have 15 cells, 3 are blank. 12 have info, i.e. 12 are samples. This is important to know to get standard error.

#Scatter plot: averages
ggplot(graph_temps,aes(x=Treatment_Temp,y=avg,color=Species))+
  geom_point()+
  labs(x="Temp",y="Respiration")+
  theme_classic(base_size=12)+
  geom_smooth(method="auto")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se, width=0),position=position_dodge(0))

###
##TEMP STUFF # For actual temp
#respiration averaged within species for temperature
tempspecies <- larvae%>%
  group_by(Species, Treatment_Temp, Reproduction_mode)%>%
  summarise(avg=mean(Respiration[!is.na(Respiration)],na.rm=TRUE),
            stdev=sd(Respiration[!is.na(Respiration)],na.rm=TRUE),
            se=(sd(Respiration[!is.na(Respiration)],na.rm=TRUE))/sqrt(NROW(na.omit(Respiration))))  #To count the number of cells after omitting the blank cells.                                                                                #Ex. I have 15 cells, 3 are blank. 12 have info, i.e. 12 are samples. This is important to know to get standard error.

#Respiration averaged among reproduction modes
tempmodes <- tempspecies%>%
  group_by(Treatment_Temp, Reproduction_mode)%>%
  summarise(average=mean(avg[!is.na(avg)],na.rm=TRUE),
            stdev=sd(avg[!is.na(avg)],na.rm=TRUE),
            se=(sd(avg[!is.na(avg)],na.rm=TRUE))/sqrt(NROW(na.omit(avg))))  #To count the number of cells after omitting the blank cells.                                                                                #Ex. I have 15 cells, 3 are blank. 12 have info, i.e. 12 are samples. This is important to know to get standard error.

#Averaged resp over temp, species as replicate
ggplot(tempmodes,aes(x=Treatment_Temp,y=average,color=Reproduction_mode))+
  geom_point()+
  labs(x="Temp",y="Respiration")+
  theme_classic(base_size=12)+
  geom_smooth(method="auto")+
  geom_errorbar(aes(ymin=average-se, ymax=average+se, width=0),position=position_dodge(0))

reltempspecies <- larvae%>%
  group_by(Species, Temp_relative, Reproduction_mode)%>%
  summarise(avg=mean(Respiration[!is.na(Respiration)],na.rm=TRUE),
            stdev=sd(Respiration[!is.na(Respiration)],na.rm=TRUE),
            se=(sd(Respiration[!is.na(Respiration)],na.rm=TRUE))/sqrt(NROW(na.omit(Respiration))))  #To count the number of cells after omitting the blank cells.                                                                                #Ex. I have 15 cells, 3 are blank. 12 have info, i.e. 12 are samples. This is important to know to get standard error.

#Respiration averaged among reproduction modes
reltempmodes <- reltempspecies%>%
  group_by(Temp_relative, Reproduction_mode)%>%
  summarise(average=mean(avg[!is.na(avg)],na.rm=TRUE),
            stdev=sd(avg[!is.na(avg)],na.rm=TRUE),
            se=(sd(avg[!is.na(avg)],na.rm=TRUE))/sqrt(NROW(na.omit(avg))))  #To count the number of cells after omitting the blank cells.                                                                                #Ex. I have 15 cells, 3 are blank. 12 have info, i.e. 12 are samples. This is important to know to get standard error.

ggplot(reltempmodes,aes(x=Temp_relative,y=average,color=Reproduction_mode))+
  geom_point()+
  labs(x="Temp",y="Respiration")+
  theme_classic(base_size=12)+
  geom_smooth(method="auto")+
  geom_errorbar(aes(ymin=average-se, ymax=average+se, width=0),position=position_dodge(0))


################### RESPIRATION vs RELATIVE TEMPERATURE ####################
#Scatter plot: all data
ggplot(larvae,aes(x=Temp_relative,y=Respiration,color=Species))+
  geom_point()+
  labs(x="Temp",y="Respiration")+
  theme_classic(base_size=12)+
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE)
  #^ What does this do?

#Making dataframe: Scatter plot: averages
graph_relative_temps <- larvae%>%
  group_by(Species,Temp_relative)%>%
  summarise(avg=mean(Respiration[!is.na(Respiration)],na.rm=TRUE),
            stdev=sd(Respiration[!is.na(Respiration)],na.rm=TRUE),
            se=(sd(Respiration[!is.na(Respiration)],na.rm=TRUE))/sqrt(NROW(na.omit(Respiration))))  #To count the number of cells after omitting the blank cells.                                                                                #Ex. I have 15 cells, 3 are blank. 12 have info, i.e. 12 are samples. This is important to know to get standard error.

#Scatter plot: averages
ggplot(graph_relative_temps,aes(x=Temp_relative,y=avg,color=Species))+
  geom_point()+
  labs(x="Temp",y="Respiration")+
  theme_classic(base_size=12)+
  geom_smooth(method="auto")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se, width=0),position=position_dodge(0))

################### RESPIRATION vs LARVAL AGE UNDER AMBIENT TEMP ####################
#Scatter plot: all data
#Seperated by species
ggplot(ambientlarvae,aes(x=as.numeric(Days_post_release),y=Respiration, col=Species))+
  geom_point()+
  labs(x="Days post release",y="Respiration")+
  theme_classic(base_size=12)+
  scale_y_continuous(breaks=seq(0,0.4,0.1),limits=c(0,0.4))+
  geom_smooth(method="auto",se=FALSE)

#Scatter plot: all data
#Seperated by reproduction mode
ggplot(ambientlarvae,aes(x=as.numeric(Days_post_release),y=Respiration,col=Reproduction_mode))+
  geom_point()+
  labs(x="Days post release",y="Respiration")+
  theme_classic(base_size=12)+
  scale_y_continuous(breaks=seq(0,0.4,0.1),limits=c(0,0.4))+
  geom_smooth(method="auto",se=FALSE)

#Using species as a replicate for brooding and broadcasting: all raw data
#Seperated by species
ggplot(ambientspecies,aes(x=as.numeric(Days_post_release),y=avg,col=Species))+
  geom_point()+
  labs(x="Days post release",y="Respiration")+
  theme_classic(base_size=12)+
  scale_y_continuous(breaks=seq(0,0.4,0.1),limits=c(0,0.4))+
  geom_smooth(method="auto",se=FALSE)

#Using species as a replicate for brooding and broadcasting: all raw data
#Seperated by reproductive mode
ggplot(ambientspecies,aes(x=as.numeric(Days_post_release),y=avg,col=Reproduction_mode))+
  geom_point()+
  labs(x="Days post release",y="Respiration")+
  theme_classic(base_size=12)+
  scale_y_continuous(breaks=seq(0,0.4,0.1),limits=c(0,0.4))+
  geom_smooth(method="auto",se=FALSE)


#Using species as a replicate for brooding and broadcasting: averaged out
ggplot(ambientmodes,aes(x=as.numeric(Days_post_release),y=average,col=Reproduction_mode))+
  geom_point()+
  labs(x="Days post release",y="Respiration")+
  theme_classic(base_size=12)+
  scale_y_continuous(breaks=seq(0,0.4,0.1),limits=c(0,0.4))+
  geom_errorbar(aes(ymin=average-se, ymax=average+se, width=0),position=position_dodge(0))+
  geom_smooth(method="auto",se=FALSE)

#By symbiont, the same as reproductive mode
ggplot(symambientmodes,aes(x=as.numeric(Days_post_release),y=average,col=Symbionts))+
  geom_point()+
  labs(x="Days post release",y="Respiration")+
  theme_classic(base_size=12)+
  scale_y_continuous(breaks=seq(0,0.4,0.1),limits=c(0,0.4))+
  geom_errorbar(aes(ymin=average-se, ymax=average+se, width=0),position=position_dodge(0))+
  geom_smooth(method="auto",se=FALSE)

#Making a datasheet to seperate brooders and broadcasters
broodamblarv <- ambientlarvae%>%
  filter(Reproduction_mode=="brooding")

#linear doesn't explain much of the data, it is more curved
lmbrood <- lm(Respiration ~ Days_post_release, data=broodamblarv, method="")
summary(lmbrood)
  
broadamblarv <- ambientlarvae%>%
  filter(Reproduction_mode=="broadcasting")

lmbroad <- lm(Respiration ~ Days_post_release, data=broadamblarv)
summary(lmbroad)

#Making dataframe: Scatter plot: averages
graph_resp <- ambientlarvae%>%
  group_by(Study,Species,Days_post_release)%>%
  summarise(avg=mean(Respiration[!is.na(Respiration)],na.rm=TRUE),
            stdev=sd(Respiration[!is.na(Respiration)],na.rm=TRUE),
            se=(sd(Respiration[!is.na(Respiration)],na.rm=TRUE))/sqrt(NROW(na.omit(Respiration))))  #To count the number of cells after omitting the blank cells.                                                                                #Ex. I have 15 cells, 3 are blank. 12 have info, i.e. 12 are samples. This is important to know to get standard error.

#Scatter plot: averages
ggplot(graph_resp,aes(x=as.numeric(Days_post_release),y=avg,col=Species))+
  geom_point(size=1.5)+
  labs(x="Days post release", y="Respiration")+
  theme_classic(base_size=12)+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se, width=0),position=position_dodge(0))+
  geom_smooth(method="auto",se=FALSE,size=0.7)+
  scale_y_continuous(breaks=seq(0,0.4,0.1),limits=c(0,0.4))+
  scale_color_manual(name="Species",values=c("grey","gray36", "grey","gray36", "grey","gray36", "grey","gray36",
                                             "red","pink", "red","pink", "red","pink"), 
                     labels=c("A.digitifera","A. intermedia", "A. millepora","A. nasuta","A. spathulata",
                              "A. tenuis", "G. aspera", "M. digitata", "O. faveolata","P. damicornis",
                              "P. astreoides","S. caliendrum","S. hystrix","S. pistillata"))

################### RESPIRATION OVER BIOMASS ####################
#with ambient temperature
ggplot(ambientlarvae,aes(x=Biomass_micrograms.larva,y=Respiration,color=Species))+
  geom_point()+
  labs(x="Biomass (micrograms)",y="Respiration")+
  theme_classic(base_size=12)+
  geom_smooth(method = "auto")+
  scale_y_continuous(breaks=seq(0,0.4,0.1),limits=c(0,0.4))

ggplot(ambientlarvae,aes(x=Biomass_micrograms.larva,y=Respiration,color=Reproduction_mode))+
  geom_point()+
  labs(x="Biomass (micrograms)",y="Respiration")+
  theme_classic(base_size=12)+
  geom_smooth(method = "auto")+
  scale_y_continuous(breaks=seq(0,0.4,0.1),limits=c(0,0.4))

ggplot(ambientlarvae,aes(x=Biomass_micrograms.larva,y=Respiration))+
  geom_point()+
  labs(x="Biomass (micrograms)",y="Respiration")+
  theme_classic(base_size=12)+
  geom_smooth(method = "auto")+
  scale_y_continuous(breaks=seq(0,0.4,0.1),limits=c(0,0.4))

################### BIOMASS OVER AGE ####################
#Seperated by species
ggplot(ambientlarvae,aes(y=Biomass_micrograms.larva,x=Days_post_release,col=Species))+
  geom_point(size=1.5)+
  theme_classic(base_size=12)+
  geom_smooth(method="auto",se=FALSE,size=0.7)
#Biomass and days post release

#Seperated by reproductive mode
ggplot(ambientlarvae,aes(y=Biomass_micrograms.larva,x=Days_post_release,col=Reproduction_mode))+
  geom_point(size=1.5)+
  theme_classic(base_size=12)+
  geom_smooth(method="auto",se=FALSE,size=0.7)

#reproductive mode with species as replicate
ggplot(biomassmodes,aes(y=average,x=Days_post_release,col=Reproduction_mode))+
  geom_point(size=1.5)+
  theme_classic(base_size=12)+
  geom_smooth(method="auto",se=FALSE,size=0.7)+
  labs(x="Days post release",y="Biomass (micrograms)")+
  geom_errorbar(aes(ymin=average-se, ymax=average+se, width=0),position=position_dodge(0))
  
#symbionts, a bit different
ggplot(symbiomassmodes,aes(y=average,x=Days_post_release,col=Symbionts))+
  geom_point(size=1.5)+
  theme_classic(base_size=12)+
  geom_smooth(method="auto",se=FALSE,size=0.7)+
  labs(x="Days post release",y="Biomass (micrograms)")+
  geom_errorbar(aes(ymin=average-se, ymax=average+se, width=0),position=position_dodge(0))

################### SETTLEMENT RATE OVER AGE ####################
ggplot(ambientlarvae,aes(y=settled_perc,x=Days_post_release,col=Species))+
  geom_point(size=1.5)+
  theme_classic(base_size=12)+
  geom_smooth(method="auto",se=FALSE,size=0.7)
#Settlement rate and days post release

################### BROODING vs BROADCASTING ####################
#### Respiration under ambient temp
t.test(Respiration~Reproduction_mode,alternative="two.sided",paired=FALSE, var.equal=TRUE, conf.level=0.95, data=ambientlarvae)
#p-value = 0.0006522, brooding and broadcasting have different respiration rates
#Brooding have higher respiration rates, all things being equal

#Making dataframe: Scatter plot: averages
respirationBB <- ambientlarvae%>%
  group_by(Reproduction_mode)%>%
  summarise(avg=mean(Respiration[!is.na(Respiration)],na.rm=TRUE),
            stdev=sd(Respiration[!is.na(Respiration)],na.rm=TRUE),
            se=(sd(Respiration[!is.na(Respiration)],na.rm=TRUE))/sqrt(NROW(na.omit(Respiration))))  #To count the number of cells after omitting the blank cells.                                                                                #Ex. I have 15 cells, 3 are blank. 12 have info, i.e. 12 are samples. This is important to know to get standard error.

ggplot(respirationBB, aes(x=Reproduction_mode, y=avg))+
  geom_bar(stat="identity")+
  theme_classic()+
  theme(legend.position="right")+
  labs(x="Reproduction mode", y="Respiration (nmol O2/larva/min)")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se, width=0),position=position_dodge(1))

#### Biomass (micrograms) under ambient temp
t.test(Biomass_micrograms.larva~Reproduction_mode,alternative="two.sided",paired=FALSE, var.equal=TRUE, conf.level=0.95, data=ambientlarvae)
#p-value = 1.307e-13
#Brooding are larger than broadcasting species, all things being equal

#Making dataframe: Scatter plot: averages
biomassBB <- ambientlarvae%>%
  group_by(Reproduction_mode)%>%
  summarise(avg=mean(Biomass_micrograms.larva[!is.na(Biomass_micrograms.larva)],na.rm=TRUE),
            stdev=sd(Biomass_micrograms.larva[!is.na(Biomass_micrograms.larva)],na.rm=TRUE),
            se=(sd(Biomass_micrograms.larva[!is.na(Biomass_micrograms.larva)],na.rm=TRUE))/sqrt(NROW(na.omit(Biomass_micrograms.larva))))  #To count the number of cells after omitting the blank cells.                                                                                #Ex. I have 15 cells, 3 are blank. 12 have info, i.e. 12 are samples. This is important to know to get standard error.

ggplot(biomassBB, aes(x=Reproduction_mode, y=avg))+
  geom_bar(stat="identity")+
  theme_classic()+
  theme(legend.position="right")+
  labs(x="Reproduction mode", y="Biomass (micrograms/larva)")+
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se, width=0),position=position_dodge(1))

################### BIOMASS OVER SURVIVORSHIP ####################
ggplot(ambientlarvae,aes(x=Biomass_micrograms.larva,y=survivorship_perc,col=Species))+
  geom_point(size=1.5)+
  theme_classic(base_size=12)+
  geom_smooth(method="auto",se=FALSE,size=0.7)

################### BIOMASS OVER SETTLEMENT RATE ####################
ggplot(ambientlarvae,aes(x=Biomass_micrograms.larva,y=settled_perc,col=Species))+
  geom_point(size=1.5)+
  theme_classic(base_size=12)+
  labs(x="Biomass_micrograms.larva", y="settled_perc")+
  geom_smooth(method="auto",se=FALSE,size=0.7)
#No trend of biomass to settle percentage

################### EXPLORING MY DATA ####################
#What is the oldest larva and what temp was it held at?
ggplot(larvae,aes(x=Treatment_Temp,y=Days_post_release,color=Species))+
  geom_point()+
  labs(x="Treatment temperature",y="Days post release")+
  theme_classic(base_size=12)

#What is the oldest larva and what relative  temp was it held at?
ggplot(larvae,aes(x=Temp_relative,y=Days_post_release,color=Species))+
  geom_point()+
  labs(x="Treatment temperature",y="Days post release")+
  theme_classic(base_size=12)


