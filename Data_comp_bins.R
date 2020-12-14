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
library(car)
library(nortest)
library(GAD)
library(AICcmodavg) #load AICcmodavg package
library(DescTools) #winsorizing
library(robustHD) #winsorizing
library(MASS)
library(survival)

setwd("/Users/nina/Research/Research Ideas/AA Larval Energy Budget")
meta <-read_excel("Meta_analysis.xlsx",sheet="Repiration.Photosynthesis") #to read your file

str(meta)

################ SIMPLIFYING DATA SHEET #############################
cols_to_be_rectified <- names(meta)[vapply(meta, is.character, logical(1))] #To get rid of spaces before and after
meta[,cols_to_be_rectified] <- lapply(meta[,cols_to_be_rectified], trimws)

meta$Study <- as.factor(meta$Study)
meta$Species <- as.factor(meta$Species)
meta$Respiration_OG <- as.numeric(meta$Respiration_OG)
meta$Reproduction_mode <- as.factor(meta$Reproduction_mode)

#Renaming for simplification
#Making SD column

meta <- meta%>%
  dplyr::rename(Respiration="Respiration_nmolO2larvmin")%>%
  dplyr::rename(Age="Days_post_release_fertilization")%>%
  mutate(Respiration=Respiration*1000)%>%
  mutate(Respiration_SE=Respiration_SE*1000) #making nmolO2/larva -> pikomols

str(meta)


#didn't include: Source_resp_normalized, Morph, Symbionts, larvae_resp, Release_day,
#Days_post_settle, Area_mm2, Area_SE, Lipid_SE_dry, Protein_SE_dry, Biomass_SE_wet,
#pCO2_microatm, Temp_exposure_min, Respiration_mass_corrected, Respiration_SE_mass_corrected,
#Units_resp_mass_corrected, Gross_photosynthesis_nmolO2larvmin, Gross_photosynthesis_SE, 
#Net_photosynthesis_nmolO2larvmin, Net_photosynthesis_SE, Symbiodinium.larva, Symbiodinium.larva_SE,
#Fv.Fm, Fv.Fm_SE, settled_perc, settled_SE, settled_hrs, survivorship_perc, survivorship_SE,
#Survivorship_duration_hrs
#larvae_biomass

meta <- meta%>%
  dplyr::select(Study, Source_resp, Source_biomass, Ocean, Location, Family, Species, 
                Reproduction_mode, Life_stage, 
                n_resp, Season, Age, n_biomass, 
                Energy_method, Lipid_microg.larva_dry, Protein_microg.larva_dry, Biomass_microg.larva_wet,
                Biomass_microg.larva_dry, Biomass_SE_dry, Biomass_assumptions, 
                OA_type, Temp_type, Treatment_Temp,
                Respiration_OG, Respiration_OG_SE, Respiration_OG_units, Respiration_conversion_assumptions,
                Respiration, Respiration_SE, Notes)%>% #Area was used to possibly include for biomass estimates
  dplyr::filter(Life_stage=="larvae")%>% #Excludes the first few days in Graham et al 2013 b/c they were eggs/embryos
  dplyr::filter(OA_type=="ambient")%>%
  dplyr::filter(Temp_type=="ambient")%>% #Not lookint at OA
  dplyr::select(-OA_type,-Temp_type, -Life_stage)


#Taking the lower number of replicates
#In the study, ___, if the n_resp column says ___, change it to <-____
meta["n_resp"][meta["Study"]=="Edmunds 2013"&meta["n_resp"]=="4-6"] <- "4" 
meta["n_resp"][meta["Study"]=="Serrano et al 2018"&meta["n_resp"]=="8-10"] <- "8"
meta["n_resp"][meta["Study"]=="Edmunds et al 2011"&meta["n_resp"]=="3-4"] <- "3"
meta["n_resp"][meta["Study"]=="Edmunds et al 2011"&meta["n_resp"]=="5-6"] <- "5"

meta$n_resp <- as.numeric(meta$n_resp) #Making respiration numeric

meta["Treatment_Temp"][meta["Study"]=="Edmunds et al 2011"&meta["Treatment_Temp"]==26.40] <- 26.7
meta["Treatment_Temp"][meta["Study"]=="Edmunds et al 2011"&meta["Treatment_Temp"]==27.00] <- 26.7
#Because I want to keep treatment temp as a column and each species was exposed to an 
#Average of 26.7C 

meta["Age"][meta["Study"]=="Edmunds et al 2001"&meta["Age"]=="1-2"] <- "2"

#meta$Treatment_Temp <- round(as.numeric(meta$Treatment_Temp)) #Rounding temperature

meta1 <- meta%>% 
  mutate("Respiration_SD"= (Respiration_SE * sqrt(n_resp)))%>%
  mutate("Respiration_var"= (Respiration_SD)^2)%>%
  mutate("Respiration_SS"= (Respiration_var)*(n_resp-1))%>% #For sample variance, SS, stdev
  mutate("Inverse_var"=(1/Respiration_var))

meta1 <- meta

#"Edmunds et al 2001" in one group, larvae were 2 days old in another group, they were 1-2 days old. 
#Can still bin it

str(meta1)

age <- meta1%>%
  filter(!Study %in% c("Haryanti and Hidaka 2015", #Haryanti & Hidaka 2015: Days post release is variable
                       "Richmond 1987"))%>%           #Richmond 1987: Days post release is not given
  filter(Respiration!="NA")%>% #Taking out studies with missing respiration
  filter(!Respiration<0) #Taking out negative respiration rates beause graph couldn't resolve them. Graham et al 2013: Age 15 & 24

age$Age <- as.numeric(age$Age) #Making respiration numeric

#Making days post release  into integers, rounding everything down
age$Age <- floor(as.numeric(age$Age))
#NA introduced because Edmunds et al 2001 had 1-2 day old larvae. 
#I won't use that data so NA is good

#### rawage is the raw data I'm working with
ageavg <- age

ageavg["Source_resp"][ageavg["Study"]=="Cumbo et al 2012"&ageavg["Source_resp"]=="Fig. 1"] <- "Fig. 1 & 3"
ageavg["Source_resp"][ageavg["Study"]=="Cumbo et al 2012"&ageavg["Source_resp"]=="Fig. 3"] <- "Fig. 1 & 3"
ageavg["Source_biomass"][ageavg["Study"]=="Cumbo et al 2012"&ageavg["Source_biomass"]=="Fig. 1"] <- "Fig. 1 & 3"
ageavg["Source_biomass"][ageavg["Study"]=="Cumbo et al 2012"&ageavg["Source_biomass"]=="Fig. 3"] <- "Fig. 1 & 3"

#Need to definitely group by study, age, and species
#didn't group by n_resp, so Cumbo et al 2013 had two different n_resp that got merged
#didn't include n_biomass... , Biomass_microg.larva_wet, Biomass_SE_dry, 

#include
#Study, Source_resp, Source_biomass, Ocean, Location, Family, Species, 
#Reproduction_mode,  Season,
#Age, Treatment_Temp, 
#Energy_method, Biomass_assumptions,
#Respiration_OG_units, Respiration_conversion_assumptions, Notes

str(ageavg)

ageavg1 <- ageavg%>%
  group_by(Study,Species,Age, Reproduction_mode)%>%
  dplyr::summarize(Respiration=mean(Respiration), 
                   Respiration_SE=mean(Respiration_SE), 
                   Respiration_OG=mean(Respiration_OG),
                   Respiration_OG_SE=mean(Respiration_OG_SE), 
                   Biomass_microg.larva_dry=mean(Biomass_microg.larva_dry, na.rm=TRUE),
                   Protein_microg.larva_dry=mean(Protein_microg.larva_dry),
                   Lipid_microg.larva_dry=mean(Lipid_microg.larva_dry))

#test1 <- ageavg%>%
#  group_by(Study,Species,Age)%>%
# dplyr::summarize(Respiration=mean(Respiration))

levels(ageavg1$Study) #This shows that the studies aren't really taken out
ageavg1 <- droplevels(ageavg1)

ageavg1 <- ageavg1%>%
  ungroup(Study, Species, Age)%>%
  mutate(normresp_nmolO2.microgramDW.min=(Respiration/Biomass_microg.larva_dry))

################ STUDY EXPLORATION #########################################
#Number of studies looking at which days
hist(ageavg1$Age, xlim=c(0,64), breaks=64, freq=TRUE) #TOTAL
hist(brooding$Age, xlim=c(0,30), breaks=30) #BROODING
hist(broadcasting$Age, xlim=c(0,64), breaks=64) #studies looking on what days

ggplot(ageavg1,aes(x=Age,y=Respiration, col=Reproduction_mode))+
  geom_point(col=Reproduction_mode)+
  labs(x="Age",y="Respiration (nmolO2/larva/min)")+
  theme_classic(base_size=12)

################ BROODING BINNING #########################################
brooding <- ageavg1%>%
  filter(Reproduction_mode=="brooding")%>%
  mutate(winsorizedresp = Winsorize(Respiration))%>%
  mutate(winsorizednormresp=Winsorize(normresp_nmolO2.microgramDW.min, na.rm=TRUE))
         
brooding["Age"][brooding["Age"]>0&brooding["Age"]<5] <- 5
brooding["Age"][brooding["Age"]>5&brooding["Age"]<30] <- 30

hist(brooding$Age, xlim=c(0,30), breaks=30) #BROODING

avgbrooding <- brooding%>%
  ungroup(Study, Species, Age, Reproduction_mode)%>%
  mutate(Age=as.factor(case_when(Age=="5"~"0-5",
                                 Age=="30"~"5-30")))

norm <- avgbrooding %>%
  filter(!winsorizednormresp==NA)

anova <- aov(winsorizedresp ~ Age, data = avgbrooding)
summary(anova)
TukeyHSD(anova)

plot(avgbrooding$winsorizedresp ~ avgbrooding$Age, ylab="Respiration (pikomolO2/larva/min)", xlab="Age")
#0.639

anova <- aov(winsorizednormresp ~ Age, data = avgbrooding, na.rm=TRUE)
summary(anova)
TukeyHSD(anova)

plot(avgbrooding$winsorizednormresp ~ avgbrooding$Age, ylab="Respiration (pikomolO2/larva/min)", xlab="Age")
#0.639







normresp_nmolO2.microgramDW.min






################ BROADCASTING BINNING #########################################
broadcasting <- ageavg1%>%
  filter(Reproduction_mode=="broadcasting")%>%
  mutate(winsorizedresp = Winsorize(Respiration)) 

broadcasting["Age"][broadcasting["Age"]>0&broadcasting["Age"]<5] <- 5
broadcasting["Age"][broadcasting["Age"]>5&broadcasting["Age"]<30] <- 30
broadcasting["Age"][broadcasting["Age"]>30&broadcasting["Age"]<64] <- 64

hist(broadcasting$Age, xlim=c(0,64), breaks=64) #studies looking on what days

avgbroadcasting <- broadcasting%>%
  ungroup(Study, Species, Age, Reproduction_mode)%>%
  mutate(Age=as.factor(case_when(Age=="5"~"0-5",
                                 Age=="30"~"5-30",
                                 Age=="64"~"30-64")))

avgbroadcasting$Age <- factor(avgbroadcasting$Age , levels=c("0-5", "5-30", "30-64"))

anova <- aov(winsorizedresp ~ Age, data = avgbroadcasting)
summary(anova)
TukeyHSD(anova)
plot(avgbroadcasting$winsorizedresp ~ avgbroadcasting$Age, ylab="Respiration (pikomolO2/larva/min)", xlab="Age")

#Making data sheet to bin zscore data
#write_xlsx(list(broadcasting=avgbroadcasting, brooding = avgbrooding),"data_compilation_divided_520.xlsx") 


################ BROODING BINNING WITH Z SCORE #########################################
setwd("/Users/nina/Research/Research Ideas/AA Larval Energy Budget")
brood <-read_excel("data_compilation_divided_520.xlsx",sheet="brooding") #to read your file
str(brood)

brooding <- brood%>%
  mutate(winsorizedZresp = Winsorize(Zscore_resp))

anova <- aov(winsorizedZresp ~ Age, data = brooding)
summary(anova)
TukeyHSD(anova)
str(brooding)
brooding$Age <- as.factor(brooding$Age)

plot(brooding$winsorizedZresp ~ brooding$Age, ylab="Respiration (pikomolO2/larva/min)", xlab="Age")
#0.9

setwd("/Users/nina/Research/Research Ideas/AA Larval Energy Budget")
broadcasting <-read_excel("data_compilation_divided_520.xlsx",sheet="broadcasting") #to read your file
str(broadcasting)

broadcasting <- broadcasting%>%
  mutate(winsorizedZresp = Winsorize(Zscore_resp))

anova <- aov(winsorizedZresp ~ Age, data = broadcasting)
summary(anova)
TukeyHSD(anova)
str(brooding)
broadcasting$Age <- as.factor(broadcasting$Age)

broadcasting$Age <- factor(broadcasting$Age , levels=c("0-5", "5-30", "30-64"))

plot(broadcasting$winsorizedZresp ~ broadcasting$Age, ylab="Respiration (pikomolO2/larva/min)", xlab="Age")
#0.01


