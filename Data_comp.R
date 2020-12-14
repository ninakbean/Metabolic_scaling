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

# log x and y and get the slope and use that for your decay function for exponential

#NOTES
#Raftery (1995) presented nice guidelines for BIC differences: 
#0-2 is weak, 2-4 is positive evidence for one model being better

#COOL CODE
#floor rounds everything down
#ceiling rounds everything up
#round just rounds normally
#How to round to the tenth: round(data, digits=2)
#How to round down to the nearest tenth: floor(a*100)/100
#zoo package: If you have NAs in the column, R can guess what they should be based on data surrounding it
#meta$Treatment_Temp <- round(as.numeric(meta$Treatment_Temp)) #Rounding temperature
#In the study, ___, if the n_resp column says ___, change it to <-____

#K <- abs(min(y))
#y <- y + K*(1+10^-15)

#fit0 <- nls(y ~ I(a*exp(-b*x)), start=list(a=max(y), b=0.1), trace=T)

#plot(y~x)
#lines(x,predict(fit0,x),col="red")

#RSS.p <- sum(residuals(fit0)^2)  # Residual sum of squares
#TSS <- sum((y - mean(y))^2)  # Total sum of squares
#1 - (RSS.p/TSS)  # R-squared measure


#QUESTIONS
#I took averages within studies, does that skew things? Like if one study took 5 measurements
#That for me go in the same category, can I just average it, or do I take the middle value?
#So far I averaged it
#For Graham, age 64, there was only one point, so I said the se was 0.0000000001
#When I collapse the averages, I should also add the sample size?

setwd("/Users/nina/Projects/Larval-Respiration")
meta <-read_excel("Meta_analysis.xlsx",sheet="Repiration.Photosynthesis")
str(meta)

test <- meta%>%
  select(Study,Species, Biomass_microg.larva_dry)

experiments <- meta%>%
  group_by(Study, Species)%>%
  dplyr::summarize(Respiration_nmolO2larvmin=mean(Respiration_nmolO2larvmin, na.rm=TRUE))

studies <- experiments%>%
  group_by(Study)%>%
  dplyr::summarize(Respiration_nmolO2larvmin=mean(Respiration_nmolO2larvmin, na.rm=TRUE))

#36 experiemnts
#26 studies

################ SIMPLIFYING DATA SHEET #############################
cols_to_be_rectified <- names(meta)[vapply(meta, is.character, logical(1))] #To get rid of spaces before and after
meta[,cols_to_be_rectified] <- lapply(meta[,cols_to_be_rectified], trimws)

meta$Study <- as.factor(meta$Study)
meta$Species <- as.factor(meta$Species)
meta$Respiration_OG <- as.numeric(meta$Respiration_OG)
meta$Reproduction_mode <- as.factor(meta$Reproduction_mode)

#Renaming for simplification
#Converting rates to pikomols
meta <- meta%>%
  dplyr::rename(Respiration="Respiration_nmolO2larvmin")%>%
  dplyr::rename(Age="Days_post_release_fertilization")%>%
  mutate(Respiration=Respiration*1000)%>% 
  mutate(Respiration_SE=Respiration_SE*1000)
  
str(meta)

#didn't include: Source_resp_normalized, Morph, Symbionts, larvae_resp, Release_day,
#Days_post_settle, Area_mm2, Area_SE, Lipid_SE_dry, Protein_SE_dry, Biomass_SE_wet,
#pCO2_microatm, Temp_exposure_min, Respiration_mass_corrected, Respiration_SE_mass_corrected,
#Units_resp_mass_corrected, Gross_photosynthesis_nmolO2larvmin, Gross_photosynthesis_SE, 
#Net_photosynthesis_nmolO2larvmin, Net_photosynthesis_SE, Symbiodinium.larva, Symbiodinium.larva_SE,
#Fv.Fm, Fv.Fm_SE, settled_perc, settled_SE, settled_hrs, survivorship_perc, survivorship_SE,
#Survivorship_duration_hrs
#larvae_biomass

#Filtering for just larvae under ambient seawater chemistry and temperatures
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

#The only study/experiment that got taken completly out is Edmunds 2013 because only spat measurements

#Taking the lower number of replicates
#THESE DON'T MATTER BECAUSE NOT INCLUDED AT THE END ANYWAYS
meta["n_resp"][meta["Study"]=="Edmunds 2013"&meta["n_resp"]=="4-6"] <- "4" 
meta["n_resp"][meta["Study"]=="Serrano et al 2018"&meta["n_resp"]=="8-10"] <- "8"
meta["n_resp"][meta["Study"]=="Edmunds et al 2011"&meta["n_resp"]=="3-4"] <- "3"
meta["n_resp"][meta["Study"]=="Edmunds et al 2011"&meta["n_resp"]=="5-6"] <- "5"

#Making respiration numeric
meta$n_resp <- as.numeric(meta$n_resp) 

#Because I want to keep treatment temp as a column and each species was exposed to an 
#Average of 26.7C 
meta["Treatment_Temp"][meta["Study"]=="Edmunds et al 2011"&meta["Treatment_Temp"]==26.40] <- 26.7
meta["Treatment_Temp"][meta["Study"]=="Edmunds et al 2011"&meta["Treatment_Temp"]==27.00] <- 26.7

#Creating SD column, have to do it after getting n_resp -> numeric
meta1 <- meta%>%
  mutate("Respiration_SD"= (Respiration_SE * sqrt(n_resp)))%>%
  mutate("Respiration_var"= (Respiration_SD)^2)%>%
  mutate("Respiration_SS"= (Respiration_var)*(n_resp-1))%>% #For sample variance, SS, stdev
  mutate("Inverse_var"=(1/Respiration_var))

#Taking out studies with days post release not given, or variable
#Or respiration not given
age1 <- meta1%>%
  filter(!Study %in% c("Haryanti and Hidaka 2015", #Haryanti & Hidaka 2015: Days post release is variable
                       "Richmond 1987",            #Richmond 1987: Days post release is not given
                       "Edmunds et al 2001",      #Edmunds 2001 is removed because in one group, larvae were 2 days old
                                                  #In another group, they were 1-2 days old. I don't have a 1 day old data point alone     
                       "Zhou et al 2016"))%>%     #not given...?
  filter(Respiration!="NA")%>% #Taking out studies with missing respiration
  filter(!Respiration<0) #Taking out negative respiration rates beause graph couldn't resolve them. Graham et al 2013: Age 15 & 24

  
#write_xlsx(list(meta1=meta1, age1=age1),"testing_deletelater.xlsx") 

#respiration missing becase
#Edmunds 2001: Not taken, settlement was
#Harii 2010 & Graham et al 2013: Not taken, lipid, weight was
#Serrano 2018: Not taken, settlement and survivorship was

#Making days post release  into integers, rounding everything down
age1$Age <- floor(as.numeric(age1$Age))
#NA introduced because Edmunds et al 2001 had 1-2 day old larvae. 
#I won't use that data so NA is good

#Taking the average within studies, pooling day of release
str(age1)
#age is just the data points without any summarizing right now...

####### rawage is the raw data
#Didn't include: n_biomass, Biomass_microg.larva_wet, Biomass_SE_dry

col_order <- c("Study", "Source_resp", "Source_biomass", "Ocean", "Location", "Season", "Family", "Species", 
  "Reproduction_mode", "Age", "Treatment_Temp", 
  "Energy_method", "Lipid_microg.larva_dry", "Protein_microg.larva_dry",
  "Biomass_microg.larva_dry", "Biomass_assumptions", "n_resp" , "Respiration_OG", 
  "Respiration_OG_SE", "Respiration_OG_units",
  "Respiration_conversion_assumptions", "Respiration", "Respiration_SE", "Respiration_SD",
  "Respiration_var", "Inverse_var","Respiration_SS", "Notes")
rawage <- age1[, col_order]
####### rawage is the raw data
ageavg <- rawage

ageavg["Source_resp"][ageavg["Study"]=="Cumbo et al 2012"&ageavg["Source_resp"]=="Fig. 1"] <- "Fig. 1 & 3"
ageavg["Source_resp"][ageavg["Study"]=="Cumbo et al 2012"&ageavg["Source_resp"]=="Fig. 3"] <- "Fig. 1 & 3"
ageavg["Source_biomass"][ageavg["Study"]=="Cumbo et al 2012"&ageavg["Source_biomass"]=="Fig. 1"] <- "Fig. 1 & 3"
ageavg["Source_biomass"][ageavg["Study"]=="Cumbo et al 2012"&ageavg["Source_biomass"]=="Fig. 3"] <- "Fig. 1 & 3"

ageavg$n_resp <- as.character(ageavg$n_resp)

ageavg["n_resp"][ageavg["Study"]=="Cumbo et al 2013"&ageavg["n_resp"]=="2"] <- "2-4"
ageavg["n_resp"][ageavg["Study"]=="Cumbo et al 2013"&ageavg["n_resp"]=="4"] <- "2-4"

#This assumption is true, but won't collapse anyways..
#Cumbo 2012, Average from study isn't used anywayas because it is taken out later down below
ageavg <- ageavg%>%
  filter(!(Study == "Cumbo et al 2012" & Biomass_assumptions == "Average from study"))%>% #How to filter out the row with these conditions, 1 row
  filter(!(Study == "Cumbo et al 2012" & Biomass_assumptions == "Repeated last value taken"))%>% #1 row
  filter(!(Study == "Harii et al 2010" & Biomass_assumptions == "Average from study")) #2 rows

#Need to definitely group by study, age, and species
#didn't group by n_resp, so Cumbo et al 2013 had two different n_resp that got merged
str(ageavg)

ageavg1 <- ageavg%>%
  group_by(Study, Source_resp, Source_biomass, Ocean, Location, Season, Family, Species, 
           Reproduction_mode, 
           Age, Treatment_Temp, 
           Energy_method, Biomass_assumptions,
           Respiration_OG_units, Respiration_conversion_assumptions, Notes, n_resp
           )%>%
  dplyr::summarize(Respiration=mean(Respiration), 
                   Respiration_SD=mean(Respiration_SD),
                   Respiration_SE=mean(Respiration_SE),
                   Respiration_var=mean(Respiration_var),
                   Respiration_SS=mean(Respiration_SS),
                   Respiration_OG=mean(Respiration_OG),
                   Respiration_OG_SE=mean(Respiration_OG_SE), 
                   Inverse_var=mean(Inverse_var),
                   Biomass_microg.larva_dry=mean(Biomass_microg.larva_dry, na.rm=TRUE),
                   Protein_microg.larva_dry=mean(Protein_microg.larva_dry),
                   Lipid_microg.larva_dry=mean(Lipid_microg.larva_dry))


#test1 <- ageavg%>%
#  group_by(Study,Species,Age)%>%
# dplyr::summarize(Respiration=mean(Respiration))

str(ageavg1)

#Filtering out studies or parts of studies that did not look among days with one species
ageavg2 <- ageavg1%>%
  ungroup(Study, Source_resp, Source_biomass, Ocean, Location, Season, Family, Species, 
           Reproduction_mode, 
           Age, Treatment_Temp, 
           Energy_method, Biomass_assumptions,
           Respiration_OG_units, Respiration_conversion_assumptions, Notes, n_resp
  )%>%
  filter(!(Study == "Cumbo et al 2012" & Species == "Seriatopora caliendrum"))%>% #How to filter out the row with these conditions
  filter(!Study %in% c("Cumbo et al 2013", 
                       "Edmunds et al 2011",
                       "Rivest and Hofmann 2014", 
                       "Rodriguez-Lanetty et al 2009", 
                       "Ross et al 2010", 
                       "Ross et al 2013", 
                       "Serrano et al 2018",
                       "Putnam and Gates 2015",
                       "Putnam et al 2013",
                       "Titlyanov et al 1998",
                       "Nakamura et al 2011",
                       "Kitchen et al 2020"))

#"Titlyanov et al 1998" <- The day 7->8 respiration was of a non swimming larva with an exoskeleton
                                        #Then I don't have two points from this study
#Nakamura study: orders of magnitude smaller resp rates. Probably wrong

experiments <- ageavg2%>%
  group_by(Study, Species)%>%
  dplyr::summarize(Respiration=mean(Respiration, na.rm=TRUE))

studies <- experiments%>%
  group_by(Study)%>%
  dplyr::summarize(Respiration=mean(Respiration, na.rm=TRUE))

levels(ageavg2$Study) #This shows that the studies aren't really taken out
ageavg2 <- droplevels(ageavg2) #Dropping levels for real

#Actual data missing for Harii 2010: Montipora digitata Day: 5 and 7
#Actual data missing for Cumbo 2012: Pdam day 11
#Harii 2010, Pdam lipids taken on day 31, respiration on day 30. I put day 31 lipids for day 30

#ageavgtest <-ageavg%>%
#  ungroup(Study, Species, Age)%>%
#  select(Study, Species, Age)

#Studies with just 2 points:
#Albright and Langdon 2011
#Gaither and Rowan 2010

#Respiration, normresp_nmolO2.microgramDW.min, Age

data_age <- ageavg2%>%
  ungroup(Study, Species, Age)%>%
  mutate(norm_resp=(Respiration/Biomass_microg.larva_dry))%>%
  mutate(normresp_SE=(Respiration_SE/Biomass_microg.larva_dry))%>%
  mutate(normresp_Ivar=(Inverse_var/Biomass_microg.larva_dry))%>%
  mutate(Age_category=cut(Age, breaks=c(0, 10, 20, 64), labels=c("low","middle","high")))%>%
  mutate(log_normresp = log10(norm_resp),
         log_biomass = log10(Biomass_microg.larva_dry),
         log1_biomass = log10(Biomass_microg.larva_dry+1),
         log_resp = log10(Respiration))%>%
  filter(log_resp!="-Inf") #IDK what this is...


#Use justbiomass for stats
table <- data_age

table$Respiration <- round(as.numeric(table$Respiration), digits=1)
table$Respiration_SE <- round(as.numeric(table$Respiration_SE), digits=1)
table$Respiration_OG <- signif(as.numeric(table$Respiration_OG), digits=3)
table$Respiration_OG_SE <- signif(as.numeric(table$Respiration_OG_SE), digits=3)

str(table)

#making assumptions letters
#This was already explained in the methods table["Respiration_conversion_assumptions"][table["Study"]=="Cumbo et al 2013b"&table["Respiration_conversion_assumptions"]=="Using mg protein per larva from study measurements"] <- "A"
table["Biomass_assumptions"][table["Biomass_assumptions"]=="(Richmond 1987) P. damicornis: 17% protein of total dry weight"] <- "A"
table["Biomass_assumptions"][table["Biomass_assumptions"]=="(Harii et al. 2007): two Acropora sp.: 59.5% lipids of total dry weight"] <- "B"
table["Biomass_assumptions"][table["Biomass_assumptions"]=="(Harii et al. 2007) & (Richmond 1987): P. damicornis: 69.05% lipids of total dry weight"] <- "C"
table["Biomass_assumptions"][table["Biomass_assumptions"]=="(Edmunds 2001): P. astreoides: protein = 3.55 micrograms & (Richmond 1987) P. damicornis: 17% protein of total dry weight"] <- NA
table["Biomass_assumptions"][table["Biomass_assumptions"]=="(Graham 2007): three Acropora species: 34 microgram dry weight"] <- NA
table["Biomass_assumptions"][table["Biomass_assumptions"]=="(Gaither and Rowan 2010) & (Richmond 1987): P. damicornis (avg): 125 micrograms dry weight"] <- NA

table["Biomass_microg.larva_dry"][table["Study"]=="Olsen et al 2013"&table["Biomass_microg.larva_dry"]==20.88] <- 0
table["Biomass_microg.larva_dry"][table["Study"]=="Albright and Langdon 2011"&table["Biomass_microg.larva_dry"]==20.88] <- 0
table["Biomass_microg.larva_dry"][table["Study"]=="Okubo et al 2008"&table["Biomass_microg.larva_dry"]==34] <- 0
table["Biomass_microg.larva_dry"][table["Study"]=="Jiang et al 2020"&table["Biomass_microg.larva_dry"]==125] <- 0

#making biomass a factor
table$Biomass_microg.larva_dry <- as.factor(table$Biomass_microg.larva_dry) 

table["Biomass_microg.larva_dry"][table["Biomass_microg.larva_dry"]==0] <- NA

#combining respiration +- and assumptions
table <- table%>%
  dplyr::rename(Assumptions="Biomass_assumptions")%>%
  #No resp assumptions unite("Assumptions", Biomass_assumptions, Respiration_conversion_assumptions, sep=", ", na.rm=TRUE)%>%
  unite("Respiration_OG", Respiration_OG, Respiration_OG_SE, sep="±", na.rm=TRUE)%>%
  unite("Respiration", Respiration, Respiration_SE, sep="±", na.rm=TRUE)
  
str(table)

#reordering
col_order <- c("Study", "Source_resp", "Source_biomass", "Ocean", "Location", "Season", "Family", "Species", 
               "Reproduction_mode", "Age", "Treatment_Temp", 
               "Energy_method", "Lipid_microg.larva_dry", "Protein_microg.larva_dry",
               "Biomass_microg.larva_dry", "n_resp", "Respiration_OG", "Respiration_OG_units",
               "Respiration", "norm_resp","Assumptions",
               "normresp_SE", "normresp_Ivar",
               "Respiration_SD",
               "Respiration_var", "Inverse_var","Respiration_SS", "Notes")
table <- table[, col_order]

str(table)

#Didn't include: Ocean, Source_resp, Source_biomass, 
#Reproduction_mode, Energy_method, Lipid_microg.larva_dry, Protein_microg.larva_dry, normresp_SE, 
#Respiration_SD, Respiration_var, Inverse_var, Respiration_SS, Notes
#normresp_Ivar
#Notes only talk about Graham and how I took points directly off graph 

#Just picking values I want for my table
final_table <- table%>%
  dplyr::select(Study, Location, Season, Family, Species,
                Age, Treatment_Temp, Reproduction_mode,
                Biomass_microg.larva_dry, n_resp, Respiration_OG, Respiration_OG_units,
                Respiration, Assumptions)%>%
  dplyr::rename(Temperature="Treatment_Temp")%>%
  dplyr::rename("Life history"="Reproduction_mode")%>%
  dplyr::rename(Biomass="Biomass_microg.larva_dry")%>%
  dplyr::rename(n="n_resp")%>%
  dplyr::rename("Orig. Respiration"="Respiration_OG")%>%
  dplyr::rename(Units="Respiration_OG_units")
  
#table_broodingbio <- table %>%
#  filter(Reproduction_mode=="brooding")%>%
#  dplyr::select(-Reproduction_mode)


#table_broadcastingbio <- table %>%
#  filter(Reproduction_mode=="broadcasting")%>%
#  dplyr::select(-Reproduction_mode)

#table <- table%>%
#  filter(Reproduction_mode=="broadcasting")

#write_xlsx(list(data=final_table),"Publication_table.xlsx") 

broadcasting <- data_age%>%
  filter(Reproduction_mode=="broadcasting")%>%
  dplyr::select(-Reproduction_mode)

    experiments <- broadcasting%>%
      group_by(Study, Species)%>%
     dplyr::summarize(Respiration=mean(Respiration, na.rm=TRUE))

    studies <- experiments%>%
      group_by(Study)%>%
      dplyr::summarize(Respiration=mean(Respiration, na.rm=TRUE))
  
brooding <- data_age %>%
  filter(Reproduction_mode=="brooding")%>%
  dplyr::select(-Reproduction_mode)

    experiments <- brooding%>%
      group_by(Study, Species)%>%
      dplyr::summarize(Respiration=mean(Respiration, na.rm=TRUE))

    studies <- experiments%>%
      group_by(Study)%>%
      dplyr::summarize(Respiration=mean(Respiration, na.rm=TRUE))
    
#Taking out studies that don't look at biomass
MTE <- data_age%>%
  filter(!Study %in% c("Olsen et al 2013",      
                       "Albright and Langdon 2011",
                       "Okubo et al 2008",
                       "Jiang et al 2020"))

experiments <- MTE%>%
  group_by(Study, Species)%>%
  dplyr::summarize(log_normresp=mean(log_normresp, na.rm=TRUE))

studies <- experiments%>%
  group_by(Study)%>%
  dplyr::summarize(log_normresp=mean(log_normresp, na.rm=TRUE))

# MASTER GRAPH
#  dplyr::select(log_resp, log_biomass, log_normresp, Age_category, Study, Species, Age, Reproduction_mode)%>%

#BIOMASS STUDIES
pred_broad <- MTE%>% 
  filter(Reproduction_mode=="broadcasting")%>%
  mutate(predlog_resp = (0.90*(log_biomass)+0.88),
        pred_resp = (10^predlog_resp))

y <- pred_broad$Biomass_microg.larva_dry
x <- pred_broad$Age
plot(x,y)

y <- pred_broad$Respiration
x <- pred_broad$Age
plot(x,y)

y <- pred_broad$norm_resp #resp/biomass, not logged
x <- pred_broad$Age
plot(x,y)

y <- pred_broad$norm_resp 
x <- pred_broad$Age
plot(x,y)


#  mutate(predlog_resp = (0.90*(log_biomass+log10(0.88))),
 #    (log_biomass*0.90)+(0.9*log10(0.88))),

pred_brood <- MTE %>%
  filter(Reproduction_mode=="brooding")%>%
  mutate(predlog_resp = ((-0.06*log_biomass)+(2.15)),
         pred_resp = (10^predlog_resp))

y <- pred_brood$Biomass_microg.larva_dry
x <- pred_brood$Age
plot(x,y)

y <- pred_brood$Respiration
x <- pred_brood$Age
plot(x,y)

y <- pred_brood$norm_resp #resp/biomass, not logged
x <- pred_brood$Age
plot(x,y)

y <- pred_brood$norm_resp #log(resp)/log(biomass)
x <- pred_brood$Age
plot(x,y)

pred_all <- rbind(pred_broad, pred_brood) #just biomass measures

y <- pred_all$Biomass_microg.larva_dry
x <- pred_all$Age
plot(x,y)

y <- pred_all$Respiration
x <- pred_all$Age
plot(x,y)

y <- pred_all$norm_resp #resp/biomass, not logged
x <- pred_all$Age
plot(x,y)

y <- pred_all$norm_resp #log(resp)/log(biomass)
x <- pred_all$Age
plot(x,y)


experiments <- pred_brood%>%
  group_by(Study, Species)%>%
  dplyr::summarize(log_normresp=mean(log_normresp, na.rm=TRUE))

studies <- experiments%>%
  group_by(Study)%>%
  dplyr::summarize(log_normresp=mean(log_normresp, na.rm=TRUE))

#### CREATING DATA SHEETS FOR COMPARING SLOPES: REAL VS PREDICTED BASED ON BIOMASS
# CREATING DATA SHEET FOR T.TEST #ALL DATA
t.test_data_age <- data_age%>%
  rename(y_resp=Respiration)%>%
  select(Study, Species, Reproduction_mode,Age, Age_category, Biomass_microg.larva_dry, y_resp)%>%
  mutate(data_sheet=c("all"))%>%
  mutate(Age005 = Age*-0.05)


t.test_pred_all <- pred_all%>%
  ungroup(Study, Species)%>%
  rename(y_resp=pred_resp)%>%
  select(Study, Species, Reproduction_mode,Age, Age_category, Biomass_microg.larva_dry, y_resp)%>%
  mutate(data_sheet=c("biomass"))%>%
  mutate(Age005 = Age*-0.03)

t.test <- rbind(t.test_data_age, t.test_pred_all)%>%
  mutate(y_resp.ln = log(y_resp))%>%
  filter(!(Study=="Graham et al 2013" & Age=="30" & data_sheet == "all")) #TOOK OUTLIER OUT


# CREATING DATA SHEET FOR T.TEST #BROADCASTING
t.test_broadcasting <- broadcasting%>%
  rename(y_resp=Respiration)%>%
  select(Study, Species, Age, Age_category, Biomass_microg.larva_dry, y_resp)%>%
  mutate(data_sheet=c("all"))%>%
  mutate(Age005 = Age*-0.12)

t.test_pred_broad <- pred_broad%>%
  ungroup(Reproduction_mode)%>%
  rename(y_resp=pred_resp)%>%
  select(Study, Species, Age, Age_category, Biomass_microg.larva_dry, y_resp)%>%
  mutate(data_sheet=c("biomass"))%>%
  mutate(Age005 = Age*-0.06)

t.test_broad <- rbind(t.test_broadcasting, t.test_pred_broad)%>%
  mutate(y_resp.ln = log(y_resp))


# CREATING DATA SHEET FOR T.TEST #BROODING
t.test_brooding <- brooding%>%
  rename(y_resp=Respiration)%>%
  select(Study, Species, Age, Age_category, Biomass_microg.larva_dry, y_resp)%>%
  mutate(data_sheet=c("all"))%>%
  mutate(Age005 = Age*-0.7)

t.test_pred_brood <- pred_brood%>%
  ungroup(Reproduction_mode)%>%
  rename(y_resp=pred_resp)%>%
  select(Study, Species, Age, Age_category, Biomass_microg.larva_dry, y_resp)%>%
  mutate(data_sheet=c("biomass"))%>%
  mutate(Age005 = Age*0.01)

t.test_brood <- rbind(t.test_brooding, t.test_pred_brood)%>%
  mutate(y_resp.ln = log(y_resp))

############# FITTING LINE: SIZE VS AGE CORRELATION ##############################
y <- pred_all$Biomass_microg.larva_dry
x <- pred_all$Age
plot(x,y)

shapiro.test(x) 
shapiro.test(y)
lillie.test(x)
lillie.test(y)

cor.test(x, y, method=c("pearson"), conf.level=TRUE)
cor.test(x, y, method=c("kendall"))
cor.test(x, y, method=c("spearman"), exact=F) #for monotonic data, p values don't have to be exact
nlcor(x,y, refine=0.5, plt = T) #same result as pearsons..

fit1 <- lm(y~x)
fit2 <- lm(y~poly(x,2,raw=TRUE))
fit3 <- lm(y~(exp(-0.06*x))) #best fit, and the linear becomes best fit after linearizing
fit4 <- lm(y~(exp(-0.04*x)))
fit5 <- lm(y~(exp(-0.05*x)))

AIC(fit1, fit2, fit3, fit4, fit5)
summary(fit1)

plot(cooks.distance(fit1))
cooks.distance(fit1)
any(cooks.distance(fit1) > 1) #proposed thresold
any(cooks.distance(fit1) > (4 * nrow(sizes)))  #proposed thresold


#brooding
y <- pred_brood$Biomass_microg.larva_dry
x <- pred_brood$Age
plot(x,y)

shapiro.test(x) 
shapiro.test(y)
lillie.test(x)
lillie.test(y)

cor.test(x, y, method=c("pearson"), conf.level=TRUE)
cor.test(x, y, method=c("kendall"))
cor.test(x, y, method=c("spearman"), exact=F) #for monotonic data
nlcor(x,y, refine=0.5, plt = T) #same result as pearsons..

fit1 <- lm(y~x)
fit2 <- lm(y~poly(x,2,raw=TRUE))
fit3 <- lm(y~(exp(-0.06*x))) #best fit, and the linear becomes best fit after linearizing
fit4 <- lm(y~(exp(-0.04*x)))
fit5 <- lm(y~(exp(-0.05*x)))

AIC(fit1, fit2, fit3, fit4, fit5)
summary(fit1)

plot(cooks.distance(fit1))
cooks.distance(fit1)
any(cooks.distance(fit1) > 1) #proposed thresold
any(cooks.distance(fit1) > (4 * nrow(sizes)))  #proposed thresold

#broadcasting
y <- pred_broad$Biomass_microg.larva_dry
x <- pred_broad$Age
plot(x,y)

shapiro.test(x) 
shapiro.test(y)
lillie.test(x)
lillie.test(y)

cor.test(x, y, method=c("pearson"), conf.level=TRUE)
cor.test(x, y, method=c("kendall"),exact=F)
cor.test(x, y, method=c("spearman"),exact=F) #for monotonic data
nlcor(x,y, refine=0.5, plt = T) #same result as pearsons..

fit1 <- lm(y~x)
fit2 <- lm(y~poly(x,2,raw=TRUE))
fit3 <- lm(y~(exp(-0.06*x))) #best fit, and the linear becomes best fit after linearizing
fit4 <- lm(y~(exp(-0.04*x)))
fit5 <- lm(y~(exp(-0.05*x)))

AIC(fit1, fit2, fit3, fit4, fit5)
summary(fit1)

plot(cooks.distance(fit1))
cooks.distance(fit1)
any(cooks.distance(fit1) > 1) #proposed thresold
any(cooks.distance(fit1) > (4 * nrow(sizes)))  #proposed thresold


############# FITTING LINE: RESP VS AGE ##############################
#including the biomass doesn't improve ANY of the models. And some of the biomass is made up
#No effect of reproduction mode...so should keep all data together

#Only display results from R2, not adjusted because adjusted is to adjust for 
#multiple linear regression...to account for the variation being explained by chance
#as you add more variables
#https://feliperego.github.io/blog/2015/10/23/Interpreting-Model-Output-In-R
#explains linear output well

### DON'T DO 2nd ORDER POLYNOMIAL BECAUSE WHERE I HAVE SEEN IT, IT WAS RESPIRATION OVER TEMPERATURE
#NOT EVEN TIME, SO I DON'T HAVE JUSTIFICATION TO USE IT IN MY STUDY

#exponential is not even that much better
y <- data_age$Respiration
x <- data_age$Age

#to lineralize exponential
y <- log(y) #log is the natural logarithm which is the same as log base e
x <- -0.05*x

fit0 <- lm(Respiration~Age*Biomass_microg.larva_dry, data=data_age)
fit  <- lm(Respiration~Age*Reproduction_mode, data=data_age)
fit1 <- lm(Respiration~Age, data=data_age)

fit1 <- lm(y~x)
fit2 <- lm(y~poly(x,2,raw=TRUE))
fit3 <- lm(y~(exp(-0.06*x))) #best fit, and the linear becomes best fit after linearizing
fit4 <- lm(y~(exp(-0.04*x)))
fit5 <- lm(y~(exp(-0.05*x)))


AIC(fit1, fit2, fit3, fit4, fit5)
AIC(fit0, fit, fit1, fit2, fit3, fit4)
AIC(fit1, fit2, fit3)
AIC(fit0, fit, fit1)

summary(fit3)
summary(fit5)

859.6201 - 857.4248
#[1] 2.1953

#linear
#Multiple R-squared:  0.1567,	Adjusted R-squared:  0.145 
#F-statistic: 13.38 on 1 and 72 DF,  p-value: 0.0004816

#exp
#Multiple R-squared:  0.1784,	Adjusted R-squared:  0.1669 
#F-statistic: 15.63 on 1 and 72 DF,  p-value: 0.0001782

ggplot(data_age, aes(x=Age, y= Respiration)) +  
  geom_point()+
  geom_smooth(method="lm", aes(color="Exp Model"), formula= (y ~ (exp(-0.05*x))))

####
y <- broadcasting$Respiration
x <- broadcasting$Age

fit <- nls(y ~ exp(b * x), start = c(b = 0), alg = "plinear")
fit

y <- log(y) #log is the natural logarithm which is the same as log base e
x <- -0.12*x
plot(x,y)

y <- broadcasting$Respiration
x <- broadcasting$Age

fit  <- lm(Respiration~Age*Biomass_microg.larva_dry, data=broadcasting)
fit1 <- lm(y~x)
fit2 <- lm(y~poly(x,2,raw=TRUE))
fit3 <- lm(y~(exp(-0.12*x))) #best fit
fit4 <- lm(y~(exp(-0.11*x))) 
fit5 <- lm(y~(exp(-0.1*x))) 


AIC(fit, fit1, fit2, fit3, fit4,fit5)
summary(fit3)

557.7834 - 554.7210
#[1] 3.0624

#linear
#Multiple R-squared:  0.1454,	Adjusted R-squared:  0.1264 
#F-statistic: 7.654 on 1 and 45 DF,  p-value: 0.008188

#exponential
#Multiple R-squared:  0.1993,	Adjusted R-squared:  0.1815 
#F-statistic:  11.2 on 1 and 45 DF,  p-value: 0.001659

ggplot(broadcasting, aes(x=Age, y= Respiration)) +  
  geom_point()+
  geom_smooth(method="lm", aes(color="Exp Model"), formula= (y ~ (exp(-0.1*x))))

####
y <- brooding$Respiration
x <- brooding$Age
plot(x,y)

fit <- nls(y ~ exp(b * x), start = c(b = 0), alg = "plinear")
fit

y <- log(y) #log is the natural logarithm which is the same as log base e
x <- -0.7*x

plot(x,y)

fit  <- lm(Respiration~Age*Biomass_microg.larva_dry, data=brooding)
fit1 <- lm(y~x)
fit2 <- lm(y~poly(x,2,raw=TRUE))
fit3 <- lm(y~(exp(-0.7*x)))
fit4 <- lm(y~(exp(-0.8*x)))


AIC(fit, fit1, fit2, fit3,fit4)
summary(fit3)

297.8115 - 295.7956
#2.0159

#linear
#Multiple R-squared:  0.006366,	Adjusted R-squared:  -0.03338 
#F-statistic: 0.1602 on 1 and 25 DF,  p-value: 0.6924

#exponential
#Multiple R-squared:  0.007011,	Adjusted R-squared:  -0.03271 
#F-statistic: 0.1765 on 1 and 25 DF,  p-value: 0.678

########## PREDICTED RESPIRATION RATES 
#Do I even need to do this? Or not because I already assumed a linear relationship prior?
y <- pred_all$pred_resp
x <- pred_all$Age

y <- log(y) #log is the natural logarithm which is the same as log base e
x <- -0.03*x

plot(x,y)

fit1 <- lm(y~x)
fit2 <- lm(y~poly(x,2,raw=TRUE))
fit3 <- lm(y~(exp(-0.02*x)))
fit4 <- lm(y~(exp(-0.03*x)))
fit5 <- lm(y~(exp(-0.04*x)))

AIC(fit1, fit2, fit3, fit4, fit5)
summary(fit4)
527.3439-525.4671
#[1] 1.8768
#exp best, linearizing it

#Multiple R-squared:  0.3548,	Adjusted R-squared:  0.3424 
#F-statistic: 28.59 on 1 and 52 DF,  p-value: 2.022e-06

####
y <- pred_broad$pred_resp
x <- pred_broad$Age

y <- log(y) #log is the natural logarithm which is the same as log base e
x <- -0.06*x

plot(x,y)

fit1 <- lm(y~x)
fit2 <- lm(y~poly(x,2,raw=TRUE))
fit3 <- lm(y~(exp(-0.05*x)))
fit4 <- lm(y~(exp(-0.06*x)))
fit5 <- lm(y~(exp(-0.07*x)))


AIC(fit1, fit2, fit3, fit4,fit5)
summary(fit4)
summary(fit3)

372.9283-369.2657
#3.6626
#exp best

####
y <- pred_brood$pred_resp
x <- pred_brood$Age

y <- log(y) #log is the natural logarithm which is the same as log base e
x <- -0.01*x

plot(x,y)

fit1 <- lm(y~x)
fit2 <- lm(y~poly(x,2,raw=TRUE))
fit3 <- lm(y~(exp(-0.01*x)))
fit4 <- lm(y~(exp(-0.02*x)))

AIC(fit1, fit2, fit3, fit4)
summary(fit3)

89.66075-87.68607
#[1] 1.97468
#linear best

#### Mass normalized
#pred_broad has broadcasting that takes out studies that didn't look at biomass
y <- pred_broad$norm_resp
x <- pred_broad$Age
plot(x,y)

fit1 <- lm(y~x)
fit2 <- lm(y~poly(x,2,raw=TRUE))
fit3 <- lm(y~(exp(-0.08*x)))
fit4 <- lm(y~(exp(-0.07*x))) #best
fit5 <- lm(y~(exp(-0.09*x))) 

AIC(fit1, fit2, fit3, fit4, fit5)
summary(fit4)
summary(fit3)



y <- pred_all$norm_resp
x <- pred_all$Age
plot(x,y)

fit1 <- lm(y~x)
fit2 <- lm(y~poly(x,2,raw=TRUE))
fit3 <- lm(y~(exp(-1.1*x))) #best
fit4 <- lm(y~(exp(-1.2*x))) 
fit5 <- lm(y~(exp(-1.3*x))) 

AIC(fit1, fit2, fit3, fit4, fit5)
summary(fit6)
summary(fit3)

#Residual standard error: 0.8079 on 52 degrees of freedom
#Multiple R-squared:  0.1054,	Adjusted R-squared:  0.08821 
#F-statistic: 6.127 on 1 and 52 DF,  p-value: 0.01661

y <- pred_brood$norm_resp
x <-  pred_brood$Age
plot(x,y)

fit1 <- lm(y~x)
fit2 <- lm(y~poly(x,2,raw=TRUE))
fit3 <- lm(y~(exp(0.08*x)))
fit4 <- lm(y~(exp(0.09*x))) #best
fit5 <- lm(y~(exp(0.1*x)))

AIC(fit1, fit2, fit3, fit4, fit5)
summary(fit4)

#Residual standard error: 0.09499 on 13 degrees of freedom
#Multiple R-squared:  0.6619,	Adjusted R-squared:  0.6099 
#F-statistic: 12.73 on 2 and 13 DF,  p-value: 0.0008682

################# ANCOVA: COMPARE REAL VS PREDICTED BASED ON BIOMASS #######################
#log doesn't really make it better
#loging both sides makes broadcast and all data normal, but not brooding, variances is worse, still outliers
#t.test <- t.test%>%
#  mutate(y_resp=log(y_resp))%>%
#  mutate(Age=log(Age))

#t.test_brood <- t.test_brood%>%
#  mutate(y_resp=log(y_resp))%>%
#  mutate(Age=log(Age))


#t.test_broad <- t.test_broad%>%
# mutate(y_resp=log(y_resp))%>%
#  mutate(Age=log(Age))

#Data are not normal: all models
#not homogeneous, except for broadcasting larvae have homogeneity of variances
#1 outlier in combined and broadcasting data

#Use in THIS order
#1) t.test
#3) t.test_brood
#2) t.test_broad

#covariate: Age
#outcome: y_resp
#groups: data_sheet

#X: Age
#Y: y_resp 
# groups: data_sheet

#### TESTING FOR ASSUMPTIONS

#### LINEARITY 
#Needs to have a linear relationship between covariate and outcome
#If there is no linear relation between X and Y, then the analysis of 
#covariance offers no improvement over the one-way analysis of variance 
#in detecting differences between the group means.

ggscatter(t.test, x = "Age005", y = "y_resp.ln",
          color = "data_sheet", add = "reg.line")+
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., 
                                           sep = "~~~~"), color = data_sheet))

ggscatter(t.test_brood, x = "Age005", y = "y_resp.ln",
          color = "data_sheet", add = "reg.line")+
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., 
                                           sep = "~~~~"), color = data_sheet))

ggscatter(t.test_broad, x = "Age005", y = "y_resp.ln",
          color = "data_sheet", add = "reg.line")+
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., 
                                           sep = "~~~~"), color = data_sheet))

#### HOMOGENEITY OF REGRESSION SLOPES #is the outcome
#This assumption checks that there is no significant interaction between 
#the covariate and the grouping variable.
#grouping: pred and not
#covariate: age
t.test %>% anova_test(y_resp.ln ~ data_sheet * Age005)
t.test_brood %>% anova_test(y_resp.ln ~ data_sheet*Age005)
t.test_broad %>% anova_test(y_resp.ln ~ data_sheet*Age005)

#### NORMALITY OF RESIDUALS
# Fit the model, the covariate goes first
model1 <- lm(y_resp.ln ~ Age005 + data_sheet, data = t.test)
model.metrics1 <- augment(model1) %>% #add fitted values and residuals by using the function augment(model)
  dplyr::select(-.hat, -.sigma, -.fitted, -.se.fit) # Remove details
head(model.metrics1, 3)
shapiro_test(model.metrics1$.resid) #not normal??

model2 <- lm(y_resp.ln ~ Age005 + data_sheet, data = t.test_brood)
model.metrics2 <- augment(model2) %>% #add fitted values and residuals by using the function augment(model)
  dplyr::select(-.hat, -.sigma, -.fitted, -.se.fit) # Remove details
head(model.metrics2, 3)
shapiro_test(model.metrics2$.resid) #not normal??


model3 <- lm(y_resp.ln ~ Age005 + data_sheet, data = t.test_broad)
model.metrics3 <- augment(model3) %>% #add fitted values and residuals by using the function augment(model)
  dplyr::select(-.hat, -.sigma, -.fitted, -.se.fit) # Remove details
head(model.metrics3, 3)
shapiro_test(model.metrics3$.resid) #not normal??

qqPlot(model1)
qqPlot(model2)
qqPlot(model3)

#### HOMOGENEITY OF VARIANCES
#The variance of the residuals is equal for all groups. 
model.metrics1 %>% levene_test(.resid ~ data_sheet)
leveneTest(t.test$y_resp.ln, t.test$data_sheet)
fligner.test(y_resp.ln ~ data_sheet, data = t.test) #non parametric
#robust to departures from normality

model.metrics2 %>% levene_test(.resid ~ data_sheet)
leveneTest(t.test_brood$y_resp.ln, t.test_brood$data_sheet)
fligner.test(y_resp.ln ~ data_sheet, data = t.test_brood) #non parametric
#robust to departures from normality

model.metrics3 %>% levene_test(.resid ~ data_sheet)
leveneTest(t.test_broad$y_resp.ln, t.test_broad$data_sheet)
fligner.test(y_resp.ln ~ data_sheet, data = t.test_broad) #non parametric
#robust to departures from normality

#### OUTLIERS
#no cases = no outliers
#standardized residuals greater than 3 in absolute value.
#1 outlier
model.metrics1 %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()

model.metrics2 %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()

model.metrics3 %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()

#### RUNNING ANCOVA #### NOTES
#All did not have significant interaction between respiration and data sheet, so can tease apart resp and age
#Best model is to not include interaction
#Age has an effect on respiration, datasheet doesn't
#Pairwise says the datasheets are significantly the same

#### RUNNING ANCOVA #### RAW DATA
#The orders of variables matters when computing ANCOVA. 
#You want to remove the effect of the covariate first - 
#that is, you want to control for it - prior to entering your main variable or interest.

#For ANCOVA, need to look at type I because the first variable is continuous
#Type I: 
#Balanced data, sequential (first variable considered, then the next for error)
#aov defaults to type I
#Use Anova function for type II and III
#Type 2: Unbalanced data, principle of marginality, does not consider interactions. 
#Don't use if interactions are present. If no interaction, more powerful than type 3. 
#Type 3: Unbalanced data, use when sig. interactions, you have to make orthogonal contrasts?

# Type I ANOVA - aov()
#aov(time.lm)

# Type II ANOVA - Anova(type = 2)
#car::Anova(time.lm, type = 2)

# Type III ANOVA - Anova(type = 3)
#car::Anova(time.lm, type = 3)

with(t.test, table(Age, data_sheet)) #unbalanced
with(t.test_brood, table(Age, data_sheet)) #unbalanced
with(t.test_broad, table(Age, data_sheet)) #unbalanced

test <- aov(y_resp ~ data_sheet, data=t.test)
summary(test)

mod1 <- aov(y_resp.ln ~ Age005, data=t.test) #aov has same output (using anova) as lm
mod2 <- aov(y_resp.ln ~ Age005 * data_sheet, data=t.test) #ANCOVA mod w/ interaction #ORDER DOES MATTER
mod3 <- aov(y_resp.ln ~ Age005 + data_sheet, data=t.test) #ANCOVA mod w/OUT interaction

AIC(mod1, mod2, mod3) #the same, just take out interaction
summary(mod2) #not significant interaction

Anova(mod1,type=2)
#F(1, 124) = 2.39, p = 0.12


shapiro.test(residuals(mod2))
lillie.test(residuals(mod2))

#obtain and compare slopes
mod3$coefficients
m.lst <- lstrends(mod3, "data_sheet", var="Age")
pairs(m.lst)

# Pairwise comparisons
library(emmeans)
pwc <- t.test %>% 
  emmeans_test(
    y_resp.ln ~ data_sheet, covariate = Age005,
    p.adjust.method = "bonferroni"
  )
pwc

#### RUNNING ANCOVA #### BROODING
test <- aov(y_resp.ln ~ data_sheet, data=t.test_brood)
summary(test)
#no significance between respiration and data sheet, so can tease apart resp and age

mod1 <- aov(y_resp.ln ~ Age005, data=t.test_brood) #aov has same output (using anova) as lm
mod2 <- aov(y_resp.ln ~ Age005 * data_sheet, data=t.test_brood) #ANCOVA mod w/ interaction #ORDER DOES MATTER
mod3 <- aov(y_resp.ln ~ Age005 + data_sheet, data=t.test_brood) #ANCOVA mod w/OUT interaction

AIC(mod1, mod2, mod3) #no interaction is a better fit
summary(mod2) #not significant interaction

Anova(mod1,type=2)
#F(1, 39) = 3.16, p = 0.08

shapiro.test(residuals(mod1))
lillie.test(residuals(mod1))

# Pairwise comparisons
library(emmeans)
pwc <- t.test_brood %>% 
  emmeans_test(
    y_resp ~ data_sheet, covariate = Age,
    p.adjust.method = "bonferroni"
  )
pwc

#### RUNNING ANCOVA #### BROADCASTING
test <- aov(y_resp.ln ~ data_sheet, data=t.test_broad)
summary(test)
#no significance between respiration and data sheet, so can tease apart resp and age

mod1 <- aov(y_resp.ln ~ Age005, data=t.test_broad) #aov has same output (using anova) as lm
mod2 <- aov(y_resp.ln ~ Age005 * data_sheet, data=t.test_broad) #ANCOVA mod w/ interaction #ORDER DOES MATTER
mod3 <- aov(y_resp.ln ~ Age005 + data_sheet, data=t.test_broad) #ANCOVA mod w/OUT interaction

AIC(mod1, mod2, mod3) #no data sheet is the best
summary(mod2) #not significant interaction

Anova(mod1,type=2)
#F(1, 81) = 0.85, p = 0.36
shapiro.test(residuals(mod1))
lillie.test(residuals(mod1))

# Pairwise comparisons
library(emmeans)
pwc <- t.test_broad %>% 
  emmeans_test(
    y_resp ~ data_sheet, covariate = Age,
    p.adjust.method = "bonferroni"
  )
pwc



#If I take out made up biomass studies:::
#If not homogeneous, can't compare elevations

#In scaling, not asking what other variables drive it, just how size drives the relationship
#How size drives the covariate.

#ANCOVA: Tests did not meet assumptions, especially lines are not parallel
#LMER: Data heteroscedastic, difference in variance between groups
#You do LMER when trying to explain one line
#testing <- lmer(log_resp ~ log_biomass * Reproduction_mode + (1|Study), data=master_graph)
#Just do a regular one way ANOVA?

#If I keep in all the studies:::
#the more assumptions are met (except for variances)
#ANCOVA: after adjusting for biomass, there is no sig on reproduction mode on resp.
#^Anova says biomass is sig.
#Slopes are not sig. different
#There is interaction between resp & biomass
#Biomass and reproduction are sig, but interaction isn't. 


hist(master_biomass$log_resp,main="Histogram of observed data")
qqPlot(residuals(m.interaction))
#The Shapiro Wilk test was not significant (p > 0.05), 
#so we can assume normality of residuals
shapiro.test(residuals(m.interaction)) #p-value = 0.7563 #normality
lillie.test(residuals(m.interaction)) #p-value = 0.5984 #normality
leveneTest(master_biomass$log_resp, master_biomass$Reproduction_mode) #p-value = 4.376e-05, #homogeneity of variances

# POWER TRANSFORM
resp <- (master_biomass$log_resp)

PT <- powerTransform(abs(resp))
summary(PT)


test <- aov(log_biomass ~ Reproduction_mode, data = master_biomass) 
summary(test)
#Interaction is significant 
#Can't tease apart reproduction and biomass

ancova <- aov(log_resp ~ Reproduction_mode * log_biomass, data= master_biomass) #ANCOVA mod w/ interaction
summary(ancova)

#predicted is the line of best fit derived from statistical data. 
#The observed is a line that literally connects all the dots.
predancova<-predict(ancova) #Gets the predicted values from the regression lines in the ANCOVA
master_graph<-cbind(master_biomass, predancova) #attaches those predictions to the dataset
  
########## GRAPHS:RESP VS AGE #############
#### EXPLORATION

pdam <-
  data_age%>%
  filter(Species=="Pocillopora damicornis")

ggplot(pdam, aes(x=Age, y= Biomass_microg.larva_dry)) +                      
  geom_point(aes(y=Biomass_microg.larva_dry, color = Study))+
  scale_shape_manual(name="Reproduction mode", values=c(1,2,3,4,5,6,7,8,9))+
  #geom_point(aes(y=Respiration), color="blue")+  # first layer
  ###geom_point(aes(y=Biomass_microg.larva_dry/1.2, shape=Species), color = "#808080", size=1.3, stroke=1, alpha = 1)+
  #geom_smooth(aes(y=Biomass_microg.larva_dry/1.2), method = "lm", formula = y ~ poly(x, 2), se=FALSE, size=1, colour="light grey")+
  theme_classic(base_size=12)
  scale_y_continuous(sec.axis = sec_axis(~ .*1.2, name = "Biomass (micrograms/larva)"), limits=c(0,450))+
  labs(y="Respiration (pikomol/larva/min)", x="Days Post Release")+
  theme(axis.title.x=element_blank())+
  theme(legend.position = "right")

  geom_smooth(method="lm", aes(color="Exp Model"), formula= (y ~ exp(x)), se=FALSE, linetype = 1)
  geom_line(data = log.model.df, aes(x, y, color = "Log Model"), size = 1, linetype = 2)

#Respiration and biomass

lining <- lm(pred_resp~Age, pred_all)
summary(lining)

graph_all <- data_age

graph_all["Biomass_microg.larva_dry"][graph_all["Study"]=="Olsen et al 2013"&graph_all["Biomass_microg.larva_dry"]==20.88] <- NA
graph_all["Biomass_microg.larva_dry"][graph_all["Study"]=="Albright and Langdon 2011"&graph_all["Biomass_microg.larva_dry"]==20.88] <- NA
graph_all["Biomass_microg.larva_dry"][graph_all["Study"]=="Okubo et al 2008"&graph_all["Biomass_microg.larva_dry"]==34] <- NA
graph_all["Biomass_microg.larva_dry"][graph_all["Study"]=="Jiang et al 2020"&graph_all["Biomass_microg.larva_dry"]==125] <- NA

graph_broad <- graph_all%>%
  filter(Reproduction_mode=="broadcasting")

graph_brood <- graph_all%>%
  filter(Reproduction_mode=="brooding")

rawall<- ggplot(graph_all, aes(x=Age)) +        #justbiomass studies                 # basic graphical object
  geom_point(aes(y=Respiration, shape=Reproduction_mode, color = Age_category), size=1.3, stroke=1, alpha = 1)+
  #geom_abline(intercept = 6.38391, slope = -0.06667)+
  scale_shape_manual(name="Reproduction mode", values=c(2, 1),
      labels=c("Broadcasting", "Brooding"))+
  scale_color_manual(name="Age", values=c("#F8A42F", "#FF4605", "#860111"),
      labels=c("0-10","11-20","21-64"))+
  #geom_point(aes(y=Respiration), color="blue")+  # first layer
  geom_smooth(aes(y=Respiration), method = "lm", formula = (y ~ (exp(-0.05*x))), se=TRUE, size=0.5, colour="#e25822")+
  geom_point(aes(y=Biomass_microg.larva_dry/1.2, shape=Reproduction_mode), color = "#808080", size=1.3, stroke=1, alpha = 1)+
  #geom_smooth(aes(y=Biomass_microg.larva_dry/1.2), method = "lm", formula = y ~ poly(x, 2), se=FALSE, size=1, colour="light grey")+
  theme_classic(base_size=12)+
  scale_y_continuous(sec.axis = sec_axis(~ .*1.2, name = "Biomass (micrograms/larva)"), limits=c(0,450))+
  labs(y="Respiration (pikomol/larva/min)", x="Days Post Release")+
  theme(axis.title.x=element_blank())+
  theme(legend.position = "top", legend.direction="vertical")+
  theme(axis.title.y.left = element_text(color = "#e25822"),
        axis.text.y.left=element_text(color="black"),
        axis.line.y.left = element_line(color = "black"),
        axis.ticks.y.left = element_line(color = "black"),
        axis.title.y.right = element_text(color = "#808080"),
        axis.text.y.right=element_text(color="black"),
        axis.line.y.right = element_line(color = "black"),
        axis.ticks.y.right = element_line(color = "black"))+
  annotate("text", x=10, y=450, size=5, label= "A) All Raw Data")+
  annotate("text", x=50, y=440, size=5, label= expression(paste(R^2 , " =0.17, p<0.01")))+
  annotate("text", x=50, y=410, size=5, label= "y=e^-0.05x")

rawall

#### Brooding
lining <- lm(pred_resp~Age, pred_brood)
summary(lining)


rawbrooding <- ggplot(graph_brood, aes(Age)) +                         # basic graphical object
  geom_point(aes(y=Respiration, color = Age_category), shape=1, size=1.3, stroke=1, alpha = 1)+
  #geom_abline(intercept = 1.558680, slope = 0.005088)+
  scale_color_manual(name="Age", values=c("#F8A42F", "#FF4605", "#860111"),
                     labels=c("0-10","11-20","21-64"))+
  geom_smooth(aes(y=Respiration), method = "lm", formula = (y ~ (exp(-0.7*x))), se=TRUE, size=0.5, colour="#e25822")+
  geom_point(aes(y=Biomass_microg.larva_dry/2), shape=1, color = "#808080", size=1.3, stroke=1, alpha = 1)+
  #geom_smooth(aes(y=Biomass_microg.larva_dry/1.2), method = "lm", formula = y ~ poly(x, 2), se=FALSE, size=1, colour="light grey")+
  theme_classic(base_size=12)+
  scale_y_continuous(sec.axis = sec_axis(~ .*2, name = "Biomass (micrograms/larva)"), limits=c(0,250))+
  scale_x_continuous(breaks=seq(0,64,20),limits=c(0,64))+
  labs(y="Respiration (pikomol/larva/min)", x="Days Post Release")+
  theme(axis.title.x=element_blank())+
  theme(legend.position = "none")+
  theme(axis.title.y.left = element_text(color = "#e25822"),
        axis.text.y.left=element_text(color="black"),
        axis.line.y.left = element_line(color = "black"),
        axis.ticks.y.left = element_line(color = "black"),
        axis.title.y.right = element_text(color = "#808080"),
        axis.text.y.right=element_text(color="black"),
        axis.line.y.right = element_line(color = "black"),
        axis.ticks.y.right = element_line(color = "black"))+
  annotate("text", x=10, y=250, size=5, label= "B) Brooded larvae")+
  annotate("text", x=50, y=250, size=5, label= expression(paste(R^2 , " = -0.01, p=0.41")))+
  annotate("text", x=50, y=230, size=5, label= "y=e^-0.7x")

rawbrooding

#### BROAD
lining <- lm(pred_resp~Age, pred_broad)
summary(lining)


rawbroadcasting <- ggplot(graph_broad, aes(Age)) +                         # basic graphical object
  geom_point(aes(y=Respiration, shape=Reproduction_mode, color = Age_category), shape=2, size=1.3, stroke=1, alpha = 1)+
  #geom_abline(intercept = 9.83679, slope = -0.15274)+
  scale_shape_manual(name="Reproduction mode", values=c(2, 1),
                     labels=c("Broadcasting", "Brooding"))+
  scale_color_manual(name="Age", values=c("#F8A42F", "#FF4605", "#860111"),
                     labels=c("0-10","11-20","21-64"))+
  geom_smooth(aes(y=Respiration), method = "lm", formula = (y ~ (exp(-0.7*x))), se=TRUE, size=0.5, colour="#e25822")+
  geom_point(aes(y=Biomass_microg.larva_dry/0.1, shape=Reproduction_mode), shape=2, color = "#808080", size=1.3, stroke=1, alpha = 1)+
  #geom_smooth(aes(y=Biomass_microg.larva_dry/1.2), method = "lm", formula = y ~ poly(x, 2), se=FALSE, size=1, colour="light grey")+
  theme_classic(base_size=12)+
  scale_y_continuous(sec.axis = sec_axis(~ .*0.1, name = "Biomass (micrograms/larva)"), limits=c(0,450))+
  labs(y="Respiration (pikomol/larva/min)", x="Days Post Release")+
  theme(legend.position = "none")+
  theme(axis.title.y.left = element_text(color = "#e25822"),
        axis.text.y.left=element_text(color="black"),
        axis.line.y.left = element_line(color = "black"),
        axis.ticks.y.left = element_line(color = "black"),
        axis.title.y.right = element_text(color = "#808080"),
        axis.text.y.right=element_text(color="black"),
        axis.line.y.right = element_line(color = "black"),
        axis.ticks.y.right = element_line(color = "black"))+
  annotate("text", x=11, y=450, size=5, label= "C) Broadcasted larvae")+
  annotate("text", x=50, y=440, size=5, label= expression(paste(R^2 , " =0.20, p<0.01")))+
  annotate("text", x=50, y=410, size=5, label= "y=e^-0.12x")
  
rawbroadcasting

norm_all<-ggplot(pred_all, aes(x=Age, y= norm_resp)) +  
  geom_point(aes(y=norm_resp, shape=Reproduction_mode, color = Age_category), size=1.3, stroke=1, alpha = 1)+
  scale_shape_manual(name="Reproduction mode", values=c(2, 1),
                     labels=c("Broadcasting", "Brooding"))+
  scale_color_manual(name="Age", values=c("#F8A42F", "#FF4605", "#860111"),
                     labels=c("0-10","11-20","21-64"))+
  geom_smooth(method="lm", color="black",aes(color="Exp Model"), formula= (y ~ (exp(-1.1*x))))+
  labs(x="Age",y="Normalized Respiraation Rate \n (pmol O2 μg DW min)")+
  theme_classic(base_size=12)+
  theme(legend.position="none")+
  annotate("text", x=9, y=25, size=5, label= "D) All Raw Data")+
  scale_x_continuous(breaks=seq(0,64,20),limits=c(0,64))+
  annotate("text", x=50, y=25, size=5, label= expression(paste(R^2 , " =0.07, p=0.03")))+
  annotate("text", x=50, y=23, size=5, label= "y=e^-1.1x")

norm_all

norm_brood <- ggplot(pred_brood, aes(x=Age, y= norm_resp)) +  
  geom_point(aes(y=norm_resp, shape=Reproduction_mode, color = Age_category), shape=1, size=1.3, stroke=1, alpha = 1)+
  scale_shape_manual(name="Reproduction mode", values=c(2, 1),
                     labels=c("Broadcasting", "Brooding"))+
  scale_color_manual(name="Age", values=c("#F8A42F", "#FF4605", "#860111"),
                     labels=c("0-10","11-20","21-64"))+
  geom_smooth(method="lm", color="black", aes(color="Exp Model"), formula= (y ~ (exp(0.09*x))))+
  labs(x="Age",y="Normalized Respiraation Rate \n (pmol O2 μg DW min)")+
  theme_classic(base_size=12)+
  theme(legend.position="none")+
  annotate("text", x=8, y=4, size=5, label= "E) Brooded larvae")+
  scale_x_continuous(breaks=seq(0,64,20),limits=c(0,64))+
  annotate("text", x=50, y=4, size=5, label= expression(paste(R^2 , " =0.8, p<0.01")))+
  annotate("text", x=50, y=3.7, size=5, label= "y=e^0.09x")

norm_brood

norm_broad <- ggplot(pred_broad, aes(x=Age, y= norm_resp)) +  
  geom_point(aes(y=norm_resp, shape=Reproduction_mode, color = Age_category), shape=2, size=1.3, stroke=1, alpha = 1)+
  scale_shape_manual(name="Reproduction mode", values=c(2, 1),
                     labels=c("Broadcasting", "Brooding"))+
  scale_color_manual(name="Age", values=c("#F8A42F", "#FF4605", "#860111"),
                     labels=c("0-10","11-20","21-64"))+
  geom_smooth(method="lm", color="black",aes(color="Exp Model"), formula= (y ~ (exp(-0.07*x))))+
  labs(x="Age",y="Normalized Respiraation Rate \n (pmol O2 μg DW min)")+
  theme_classic(base_size=12)+
  theme(legend.position="none")+
  annotate("text", x=10, y=25, size=5, label= "F) Broadcasted larvae")+
  scale_x_continuous(breaks=seq(0,64,20),limits=c(0,64))+
  annotate("text", x=50, y=25, size=5, label= expression(paste(R^2 , " =0.1, p=0.03")))+
  annotate("text", x=50, y=23, size=5, label= "y=e^-0.07x")

norm_broad

(rawall)/(rawbrooding)/(rawbroadcasting)|(norm_all)/(norm_brood)/(norm_broad)

###############3#### SHEETS I DON'T REALLY NEED DOWN BELOW ###################





#Winsorizing and weighted means didn't really make it better
#NOT GOING TO DO IT
#1=raw data, 2=just winsorized, 3= weighted means+winsozing

#All data
#1) 15%, sig, AIC:859
#2) 16%, sig, AIC: 840
#3) 11%, sig: AIC: 401

#Broad
#1) 13%, sig, AIC: 579
#2) 14%, sig, AIC: 568
#3) 5%, non sig, AIC: 276


#Brood
#1) .1%, non sig, AIC: 273
#2) .1%, non sig, AIC: 270
#3) 1%, non sig, AIC: 114

#Just winsorizing
editW_master <- justbiomass%>%
  mutate(Respiration = Winsorize(Respiration, na.rm=TRUE))

editW_master_broad <- broadcastingbio%>%
  mutate(Respiration = Winsorize(Respiration, na.rm=TRUE))

editW_master_brood <- broodingbio%>%
  mutate(Respiration = Winsorize(Respiration, na.rm=TRUE))

#Weighted means+just winsorizing
#Better fit via AIC, but not a good R2 and p value
edit_master <- master%>%
  mutate(meanweight=Respiration*Inverse_var)%>%
  group_by(Age, Age_category, Reproduction_mode)%>%
  summarise(num=sum(meanweight, na.rm=TRUE), denom=sum(Inverse_var, na.rm=TRUE))%>%
  mutate(Wmean=num/denom)%>%
  mutate(Respiration = Winsorize(Wmean, na.rm=TRUE))

edit_master_broad <- master_broad%>%
  mutate(meanweight=Respiration*Inverse_var)%>%
  group_by(Age, Age_category)%>%
  summarise(num=sum(meanweight, na.rm=TRUE), denom=sum(Inverse_var, na.rm=TRUE))%>%
  mutate(Wmean=num/denom)%>%
  mutate(Respiration = Winsorize(Wmean, na.rm=TRUE))

edit_master_brood <- master_brood%>%
  mutate(meanweight=Respiration*Inverse_var)%>%
  group_by(Age, Age_category)%>%
  summarise(num=sum(meanweight, na.rm=TRUE), denom=sum(Inverse_var, na.rm=TRUE))%>%
  mutate(Wmean=num/denom)%>%
  mutate(Respiration = Winsorize(Wmean, na.rm=TRUE))

#### ENDDDD SHEETS I DON'T REALLY NEED DOWN BELOW

############# JUST GETTING WEIGHTED MEANS: Making sheets ###########################
#point 64 has na values??

#### ALL DATA
allWmean <- justbiomass%>%
  dplyr::select(Study,Age,Species, normresp_nmolO2.microgramDW.min, normresp_Ivar)%>%
  mutate(meanweight=normresp_nmolO2.microgramDW.min*normresp_Ivar)%>%
  group_by(Age)%>%
  summarise(num=sum(meanweight, na.rm=TRUE), denom=sum(normresp_Ivar, na.rm=TRUE))%>%
  mutate(Wmean=num/denom)%>%
  mutate(winsorized = Winsorize(Wmean, na.rm=TRUE))


subsetallWmean <- justbiomass%>%
  filter(!Study %in% c("Albright and Langdon 2011",
                       "Jiang et al 2020",
                       "Okubo et al 2008",
                       "Olsen et al 2013"))%>%
  dplyr::select(Study,Age,Species, normresp_nmolO2.microgramDW.min, normresp_Ivar)%>%
  mutate(meanweight=normresp_nmolO2.microgramDW.min*normresp_Ivar)%>%
  group_by(Age)%>%
  summarise(num=sum(meanweight, na.rm=TRUE), denom=sum(normresp_Ivar, na.rm=TRUE))%>%
  mutate(Wmean=num/denom)%>%
  mutate(winsorized = Winsorize(Wmean, na.rm=TRUE))

#### BROODING
broodingWmean <- broodingbio%>%
  dplyr::select(Study,Age,Species, normresp_nmolO2.microgramDW.min, normresp_Ivar)%>%
  mutate(meanweight= normresp_nmolO2.microgramDW.min*normresp_Ivar)%>%
  group_by(Age)%>%
  summarise(num=sum(meanweight, na.rm=TRUE), denom=sum(normresp_Ivar, na.rm=TRUE))%>%
  mutate(Wmean=num/denom)%>%
  mutate(winsorized = Winsorize(Wmean, na.rm=TRUE))

subsetbroodingWmean <- broodingbio%>%
  filter(!Study %in% c("Albright and Langdon 2011",
                       "Jiang et al 2020",
                       "Okubo et al 2008",
                       "Olsen et al 2013"))%>%
  dplyr::select(Study,Age,Species, normresp_nmolO2.microgramDW.min, normresp_Ivar)%>%
  mutate(meanweight= normresp_nmolO2.microgramDW.min*normresp_Ivar)%>%
  group_by(Age)%>%
  summarise(num=sum(meanweight, na.rm=TRUE), denom=sum(normresp_Ivar, na.rm=TRUE))%>%
  mutate(Wmean=num/denom)%>%
  mutate(winsorized = Winsorize(Wmean, na.rm=TRUE))

#### BROADCASTING
broadcastingWmean <- broadcastingbio%>%
  dplyr::select(Study,Age,Species, normresp_nmolO2.microgramDW.min, normresp_Ivar)%>%
  mutate(meanweight= normresp_nmolO2.microgramDW.min*normresp_Ivar)%>%
  group_by(Age)%>%
  summarise(num=sum(meanweight, na.rm=TRUE), denom=sum(normresp_Ivar, na.rm=TRUE))%>%
  mutate(Wmean=num/denom)%>%
  mutate(winsorized = Winsorize(Wmean, na.rm=TRUE))


subsetbroadcastingWmean <- broadcastingbio%>%
  filter(!Study %in% c("Albright and Langdon 2011",
                       "Jiang et al 2020",
                       "Okubo et al 2008",
                       "Olsen et al 2013"))%>%
  dplyr::select(Study,Age,Species, normresp_nmolO2.microgramDW.min, normresp_Ivar)%>%
  mutate(meanweight= normresp_nmolO2.microgramDW.min*normresp_Ivar)%>%
  group_by(Age)%>%
  summarise(num=sum(meanweight, na.rm=TRUE), denom=sum(normresp_Ivar, na.rm=TRUE))%>%
  mutate(Wmean=num/denom)%>%
  mutate(winsorized = Winsorize(Wmean, na.rm=TRUE))

############# JUST GETTING MEANS: Making sheets ###########################
allmean <- justbiomass%>%
  group_by(Age)%>%
  dplyr::summarize(normresp_mean=mean(normresp_nmolO2.microgramDW.min))%>%
  mutate(winsorized = Winsorize(normresp_mean))

subsetmean <- justbiomass%>%
  filter(!Study %in% c("Albright and Langdon 2011",
                       "Jiang et al 2020",
                       "Okubo et al 2008",
                       "Olsen et al 2013"))%>%
  group_by(Age)%>%
  dplyr::summarize(normresp_mean=mean(normresp_nmolO2.microgramDW.min))%>%
  mutate(winsorized = Winsorize(normresp_mean))
  

allbrooding <- broodingbio%>%
  group_by(Age)%>%
  dplyr::summarize(normresp_mean=mean(normresp_nmolO2.microgramDW.min))%>%
  mutate(winsorized = Winsorize(normresp_mean))

subsetbrooding <- broodingbio%>%
  filter(!Study %in% c("Albright and Langdon 2011",
                       "Jiang et al 2020",
                       "Okubo et al 2008",
                       "Olsen et al 2013"))%>%
  group_by(Age)%>%
  dplyr::summarize(normresp_mean=mean(normresp_nmolO2.microgramDW.min))%>%
  mutate(winsorized = Winsorize(normresp_mean))

allbroadcasting<- broadcastingbio%>%
  group_by(Age)%>%
  dplyr::summarize(normresp_mean=mean(normresp_nmolO2.microgramDW.min))%>%
  mutate(winsorized = Winsorize(normresp_mean))

subsetbroadcasting<- broadcastingbio%>%
  filter(!Study %in% c("Albright and Langdon 2011",
                       "Jiang et al 2020",
                       "Okubo et al 2008",
                       "Olsen et al 2013"))%>%
  group_by(Age)%>%
  dplyr::summarize(normresp_mean=mean(normresp_nmolO2.microgramDW.min))%>%
  mutate(winsorized = Winsorize(normresp_mean))

#If I take out studies that didn't look at biomass, it doesn't change much
#EXCEPT it makes exponential curve fit 80% for brooding
#If I use biomass as a constant, quadratic curve works as 50% 

############# WEIGHTED MEANS: Fitting lines ###########################
#allmean, allbroadcasting, allbrooding
#subsetmean, subsetbroadcasting, subsetbrooding: Studies that didn't look at biomass

y <- allWmean$winsorized
x <- allWmean$Age

K <- abs(min(y))
y <- y + K*(1+10^-15)

fit0 <- nls(y ~ I(a*exp(-b*x)), start=list(a=max(y), b=0.01), trace=T)

plot(y~x)
lines(x,predict(fit0,x),col="red")

RSS.p <- sum(residuals(fit0)^2)  # Residual sum of squares
TSS <- sum((y - mean(y))^2)  # Total sum of squares
1 - (RSS.p/TSS)  # R-squared measure
#  0.008136197

#fit first degree polynomial equation:
fit  <- lm(y~x)
#second degree
fit2 <- lm(y~poly(x,2,raw=TRUE))
#third degree
fit3 <- lm(y~poly(x,3,raw=TRUE))
#fourth degree
fit4 <- lm(y~poly(x,4,raw=TRUE))
#generate range of 50 numbers starting from 30 and ending at 160
xx <- seq(1,64,1)
plot(x,y,pch=19)
lines(xx, predict(fit, data.frame(x=xx)), col="red")
lines(xx, predict(fit2, data.frame(x=xx)), col="green")
lines(xx, predict(fit3, data.frame(x=xx)), col="blue")
lines(xx, predict(fit4, data.frame(x=xx)), col="purple")

AIC(fit0, fit, fit2, fit3, fit4)

summary(fit2)
#linear fit

##### BROODING

y <- subsetbroodingWmean$winsorized
x <- subsetbroodingWmean$Age

K <- abs(min(y))
y <- y + K*(1+10^-15)

fit0 <- nls(y ~ I(a*exp(-b*x)), start=list(a=max(y), b=0.01), trace=T)

plot(y~x)
lines(x,predict(fit0,x),col="red")

RSS.p <- sum(residuals(fit0)^2)  # Residual sum of squares
TSS <- sum((y - mean(y))^2)  # Total sum of squares
1 - (RSS.p/TSS)  # R-squared measure
#  0.008136197

#fit first degree polynomial equation:
fit  <- lm(y~x)
#second degree
fit2 <- lm(y~poly(x,2,raw=TRUE))
#third degree
fit3 <- lm(y~poly(x,3,raw=TRUE))
#fourth degree
fit4 <- lm(y~poly(x,4,raw=TRUE))
#generate range of 50 numbers starting from 30 and ending at 160
xx <- seq(1,64,1)
plot(x,y,pch=19)
lines(xx, predict(fit, data.frame(x=xx)), col="red")
lines(xx, predict(fit2, data.frame(x=xx)), col="green")
lines(xx, predict(fit3, data.frame(x=xx)), col="blue")
lines(xx, predict(fit4, data.frame(x=xx)), col="purple")

AIC(fit0, fit, fit2, fit3, fit4)

summary(fit)
#linear fit

#Taking last time point
no30 <- broodingWmean%>%
  filter(Age!=30)
  
y <- no30$winsorized
x <- no30$Age

K <- abs(min(y))
y <- y + K*(1+10^-15)

fit0 <- nls(y ~ I(a*exp(-b*x)), start=list(a=max(y), b=0.01), trace=T)

plot(y~x)
lines(x,predict(fit0,x),col="red")

RSS.p <- sum(residuals(fit0)^2)  # Residual sum of squares
TSS <- sum((y - mean(y))^2)  # Total sum of squares
1 - (RSS.p/TSS)  # R-squared measure
# 0.1406364

#fit first degree polynomial equation:
fit  <- lm(y~x)
#second degree
fit2 <- lm(y~poly(x,2,raw=TRUE))
#third degree
fit3 <- lm(y~poly(x,3,raw=TRUE))
#fourth degree
fit4 <- lm(y~poly(x,4,raw=TRUE))
#generate range of 50 numbers starting from 30 and ending at 160
xx <- seq(1,64,1)
plot(x,y,pch=19)
lines(xx, predict(fit, data.frame(x=xx)), col="red")
lines(xx, predict(fit2, data.frame(x=xx)), col="green")
lines(xx, predict(fit3, data.frame(x=xx)), col="blue")
lines(xx, predict(fit4, data.frame(x=xx)), col="purple")

AIC(fit0, fit, fit2, fit3, fit4)

summary(fit)
#With 30: quadratic: p-value: 0.01468, Multiple R-squared:  0.5701,	Adjusted R-squared:  0.4842 
#WithOUT 30: quadratic: p-value: 0.1513, Multiple R-squared:  0.3427,	Adjusted R-squared:  0.1967 

#### BROADCASTING

y <- subsetbroadcastingWmean$winsorized
x <- subsetbroadcastingWmean$Age

K <- abs(min(y))
y <- y + K*(1+10^-15)

fit0 <- nls(y ~ I(a*exp(-b*x)), start=list(a=max(y), b=0.01), trace=T)

plot(y~x)
lines(x,predict(fit0,x),col="red")

RSS.p <- sum(residuals(fit0)^2)  # Residual sum of squares
TSS <- sum((y - mean(y))^2)  # Total sum of squares
1 - (RSS.p/TSS)  # R-squared measure
# 0.008136197

#fit first degree polynomial equation:
fit  <- lm(y~x)
#second degree
fit2 <- lm(y~poly(x,2,raw=TRUE))
#third degree
fit3 <- lm(y~poly(x,3,raw=TRUE))
#fourth degree
fit4 <- lm(y~poly(x,4,raw=TRUE))
#generate range of 50 numbers starting from 30 and ending at 160
xx <- seq(1,64,1)
plot(x,y,pch=19)
lines(xx, predict(fit, data.frame(x=xx)), col="red")
lines(xx, predict(fit2, data.frame(x=xx)), col="green")
lines(xx, predict(fit3, data.frame(x=xx)), col="blue")
lines(xx, predict(fit4, data.frame(x=xx)), col="purple")

AIC(fit0, fit, fit2, fit3, fit4)
#linear
summary(fit)

############# JUST GETTING WEIGHTED MEANS: GGPLOT ###########################
#For exponential curve fitting: 
# geom_smooth(method="nls", formula=y~I(a*exp(x*-b)),
#method.args=list(start=c(a=max(y),b=0.01)), se=FALSE,
#size=1, colour="light blue")+

meannormall<- ggplot(allWmean, aes(x=Age,y=winsorized))+
  geom_point()+
  labs(x="Days post release",y="Mean Normalized Respiration \n (pikomolO2/microgramDW/min)")+
  theme_classic(base_size=12)+
  geom_smooth(method="lm", se=FALSE, size=1, colour="light blue")+
  ggtitle("All Data")
meannormall

meannormbrooding <- ggplot(broodingWmean, aes(x=Age,y=winsorized))+
  geom_point()+
  labs(x="Days post release",y="Mean Normalized Respiration \n (pikomolO2/microgramDW/min)")+
  theme_classic(base_size=12)+
  geom_smooth(method="lm", se=FALSE, size=1, colour="light blue")+
  ggtitle("Brooding")
meannormbrooding
# geom_smooth(method = "lm", formula = y ~ poly(x, 2), se=FALSE, size=1, colour="light blue")+
#to fit a quadratic

meannormbroadcasting <- ggplot(broadcastingWmean, aes(x=Age,y=winsorized))+
  geom_point()+
  labs(x="Days post release",y="Mean Normalized Respiration \n (pikomolO2/microgramDW/min)")+
  theme_classic(base_size=12)+
  geom_smooth(method="lm", se=FALSE, size=1, colour="light blue")+
  ggtitle("Broadcasting")
meannormbroadcasting

(rawall / rawbrooding / rawbroadcasting) | (meannormall / meannormbrooding / meannormbroadcasting)

################ ZSCORE #############################
setwd("/Users/nina/Research/Research Ideas/AA Larval Energy Budget")
Zscore <-read_excel("data_compilation.xlsx",sheet="Data") #to read your file
Zscore$Study <- as.factor(Zscore$Study)

y <- Zscore$Zscore
x <- Zscore$Age
study <- Zscore$Study

plot(y~x, col=study)

broodZscore <- Zscore%>%
  filter(Reproduction_mode=="brooding")

y <- broodZscore$Zscore
x <- broodZscore$Age
study <- broodZscore$Study

plot(y~x, main="Brooding", xlab="Age", ylab="Zscore Respiration")

broadZscore <- Zscore%>%
  filter(Reproduction_mode=="broadcasting")

y <- broadZscore$Zscore
x <- broadZscore$Age
study <- broadZscore$Study

plot(y~x, main="Broadcasting", xlab="Age", ylab="Zscore Respiration")

################ ZSCORE NORMALIZED #############################
setwd("/Users/nina/Research/Research Ideas/AA Larval Energy Budget")
Zscore <-read_excel("data_compilation_normalized.xlsx",sheet="Data") #to read your file
Zscore$Study <- as.factor(Zscore$Study)

broodZscore <- Zscore%>%
  filter(Reproduction_mode=="brooding")

y <- broodZscore$Zscore
x <- broodZscore$Age
study <- broodZscore$Study

plot(y~x, main="Brooding", xlab="Age", ylab="Zscore Respiration normalized")


y <- broodZscore$Respiration
x <- broodZscore$Age
study <- broodZscore$Study

plot(y~x, main="Brooding", xlab="Age", ylab="Respiration normalized")


broadZscore <- Zscore%>%
  filter(Reproduction_mode=="broadcasting")

y <- broadZscore$Zscore
x <- broadZscore$Age
study <- broadZscore$Study

plot(y~x, main="Broadcasting", xlab="Age", ylab="Respiration normalized")

y <- broadZscore$Respiration
x <- broadZscore$Age
study <- broadZscore$Study

plot(y~x, main="Broadcasting", xlab="Age", ylab="Respiration normalized")

################ ALL DATA, WEIGHTED MEANS #############################
setwd("/Users/nina/Research/Research Ideas/AA Larval Energy Budget")
effects <-read_excel("Data compilation.xlsx",sheet="Averages_total") #to read your file
str(effects)

effects <- effects%>%
  mutate(winsorized = Winsorize(weighted_mean)) 
#Up: Age 13, 24
#Down: Age 2, 23

#Faster way to do it
y <- effects$winsorized
x <- effects$Age

K <- abs(min(y))
y <- y + K*(1+10^-15)

fit0 <- nls(y ~ I(a*exp(-b*x)), start=list(a=max(y), b=0.01), trace=T)

plot(y~x)
lines(x,predict(fit0,x),col="red")
summary(m)

RSS.p <- sum(residuals(fit0)^2)  # Residual sum of squares
TSS <- sum((y - mean(y))^2)  # Total sum of squares
1 - (RSS.p/TSS)  # R-squared measure
# 0.2407926

#fit first degree polynomial equation:
fit  <- lm(y~x)
#second degree
fit2 <- lm(y~poly(x,2,raw=TRUE))
#third degree
fit3 <- lm(y~poly(x,3,raw=TRUE))
#fourth degree
fit4 <- lm(y~poly(x,4,raw=TRUE))
#generate range of 50 numbers starting from 30 and ending at 160
xx <- seq(1,64,1)
plot(x,y,pch=19)
lines(xx, predict(fit, data.frame(x=xx)), col="red")
lines(xx, predict(fit2, data.frame(x=xx)), col="green")
lines(xx, predict(fit3, data.frame(x=xx)), col="blue")
lines(xx, predict(fit4, data.frame(x=xx)), col="purple")

summary(fit) #linear, Multiple R2: 0.1036, Adj R2: 0.07162, p = 0.0827
summary(fit2) #Quadratic, Multiple R2: 0.2317, Adj R2:  0.1748, p = 0.02849
summary(fit3) #Cubic, multiple R2: 0.1372, Adj R2: 0.03769, p =  0.2714
summary(fit4) #Multiple: 0.1861, Adj R2: 0.05587, p = 0.2536


AIC(fit0, fit, fit2, fit3, fit4) #exp is best
#if Δ𝑖<2, then there is substantial support for the 𝑖-th model (or the evidence against it is worth only a bare mention
#), and the proposition that it is a proper description is highly probable;
#if 2<Δ𝑖<4, then there is strong support for the 𝑖-th model;
#if 4<Δ𝑖<7, then there is considerably less support for the 𝑖-th model;
#models with Δ𝑖>10 have essentially no support.


#Generally, if you have a large difference between your multiple and your adjusted Rsquared
#that indicates you may have overfit your model.
#The adjusted R-squared compensates for the addition of variables and only increases if the 
#new predictor enhances the model above what would be obtained by probability. 
#Conversely, it will decrease when a predictor improves the model less than what is predicted by chance.
#When too few data points are used in a statistical model it is called overfitting. Overfitting can return an unwarranted high R-squared value. 
#This incorrect figure can lead to a decreased ability to predict performance outcomes.

################ BROODING, WEIGHTED MEANS #############################
setwd("/Users/nina/Research/Research Ideas/AA Larval Energy Budget")
brooding_avg <-read_excel("data_compilation_divided_FINAL.xlsx",sheet="brooding_avg") #to read your file

brooding_avg <- brooding_avg%>%
  mutate(winsorized = Winsorize(Weighted_mean)) 
#UP: Age 3
#Down: Age 6

y <- brooding_avg$winsorized
x <- brooding_avg$Age

K <- abs(min(y))
y <- y + K*(1+10^-15)

fit0 <- nls(y ~ I(a*exp(-b*x)), start=list(a=max(y), b=0.1), trace=T)

plot(y~x)
lines(x,predict(fit0,x),col="red")
summary(m)

RSS.p <- sum(residuals(fit0)^2)  # Residual sum of squares
TSS <- sum((y - mean(y))^2)  # Total sum of squares
1 - (RSS.p/TSS)  # R-squared measure
#[1] 0.1094168

#fit first degree polynomial equation:
fit  <- lm(y~x)
#second degree
fit2 <- lm(y~poly(x,2,raw=TRUE))
#third degree
fit3 <- lm(y~poly(x,3,raw=TRUE))
#fourth degree
fit4 <- lm(y~poly(x,4,raw=TRUE))
#generate range of 50 numbers starting from 30 and ending at 160
xx <- seq(1,64,1)
plot(x,y,pch=19)
lines(xx, predict(fit, data.frame(x=xx)), col="red")
lines(xx, predict(fit2, data.frame(x=xx)), col="green")
lines(xx, predict(fit3, data.frame(x=xx)), col="blue")
lines(xx, predict(fit4, data.frame(x=xx)), col="purple")

summary(fit) #linear multiple R2: 0.09306, Adj R2: 0.01061 , p-value: 0.3108
summary(fit2) #Quadratic, multiple R2: 0.1625, Adj R2: -0.004964, p-value: 0.412
summary(fit3) #multiple R2: 0.2122, Adj R2: -0.0504 , p-value: 0.5206
summary(fit4) #multiple R2: 0.2125, Adj R2: -0.1812,  p-value: 0.7114


AIC(fit0, fit, fit2, fit3, fit4) #exp is best
#Multiple R-squared:  0.1035,	Adjusted R-squared:  0.02204 
#F-statistic:  1.27 on 1 and 11 DF,  p-value: 0.2837

################ BROADCASTING, WEIGHTED MEANS #############################
setwd("/Users/nina/Research/Research Ideas/AA Larval Energy Budget")
broadavg <-read_excel("data_compilation_divided.xlsx",sheet="broadcasting_avg") #to read your file

broadavg <- broadavg%>%
  mutate(winsorized = Winsorize(Weighted_mean)) 

#UP: Age 4, 13, 24
#DOWN: Age 23

y <- broadavg$winsorized
x <- broadavg$Age

K <- abs(min(y))
y <- y + K*(1+10^-15)

fit0 <- nls(y ~ I(a*exp(-b*x)), start=list(a=max(y), b=0.1), trace=T)

plot(y~x)
lines(x,predict(fit0,x),col="red")
summary(m)

RSS.p <- sum(residuals(fit0)^2)  # Residual sum of squares
TSS <- sum((y - mean(y))^2)  # Total sum of squares
1 - (RSS.p/TSS)  # R-squared measure
#[1] 0.05178886

#fit first degree polynomial equation:
fit  <- lm(y~x)
#second degree
fit2 <- lm(y~poly(x,2,raw=TRUE))
#third degree
fit3 <- lm(y~poly(x,3,raw=TRUE))
#fourth degree
fit4 <- lm(y~poly(x,4,raw=TRUE))
#generate range of 50 numbers starting from 30 and ending at 160
xx <- seq(1,64,1)
plot(x,y,pch=19)
lines(xx, predict(fit, data.frame(x=xx)), col="red")
lines(xx, predict(fit2, data.frame(x=xx)), col="green")
lines(xx, predict(fit3, data.frame(x=xx)), col="blue")
lines(xx, predict(fit4, data.frame(x=xx)), col="purple")

summary(fit) #linear multiple R2: 0.02149, Adj R2: -0.01765, p-value: 0.4656
summary(fit2) #Quadratic, multiple R2: 0.02462, Adj R2: -0.05666,  p-value: 0.7414
summary(fit3) #multiple R2: 0.02648, Adj R2: -0.1005, p-value: 0.8895
summary(fit4) #multiple R2: 0.03186, Adj R2: -0.1442, p-value: 0.9458

AIC(fit0, fit, fit2, fit3, fit4) #exp is best










################# CLASSIC META-ANALYSIS: FITTING EXP FOR RESPIRATION #################################
#only keeping studies that have 4 or more points per study
meta <- justbiomass%>%
  filter(!Study %in% c("Olsen et al 2013", 
                       "Gaither and Rowan 2010", 
                       "Albright and Langdon 2011"))

one <- meta%>%
  filter(Study=="Cumbo et al 2012")

y <- one$Respiration
x <- one$Age

K <- abs(min(y))
y <- y + K*(1+10^-15)

fit0 <- nls(y ~ I(a*exp(-b*x)), start=list(a=max(y), b=0.1), trace=T) #b=growth or decay rate
#a=inital value (the amount before measuring growth or decay)

plot(y~x)
lines(x,predict(fit0,x),col="red")
summary(fit0)

RSS.p <- sum(residuals(fit0)^2)  # Residual sum of squares
TSS <- sum((y - mean(y))^2)  # Total sum of squares
1 - (RSS.p/TSS)  # R-squared measure = effect size?
#0.1170883

two <- meta%>%
  filter(Study=="Cumbo et al 2013b")

y <- two$Respiration
x <- two$Age

K <- abs(min(y))
y <- y + K*(1+10^-15)

fit0 <- nls(y ~ I(a*exp(-b*x)), start=list(a=max(y), b=0.1), trace=T)

plot(y~x)
lines(x,predict(fit0,x),col="red")
summary(fit0)

RSS.p <- sum(residuals(fit0)^2)  # Residual sum of squares
TSS <- sum((y - mean(y))^2)  # Total sum of squares
1 - (RSS.p/TSS)  # R-squared measure = effect size?
#0.8667305

three <- meta%>%
  filter(Study=="Graham et al 2013" & Species == "Goniastrea aspera")

y <- three$Respiration
x <- three$Age

K <- abs(min(y))
y <- y + K*(1+10^-15)

fit0 <- nls(y ~ I(a*exp(-b*x)), start=list(a=max(y), b=0.1), trace=T)

plot(y~x)
lines(x,predict(fit0,x),col="red")
summary(fit0)

RSS.p <- sum(residuals(fit0)^2)  # Residual sum of squares
TSS <- sum((y - mean(y))^2)  # Total sum of squares
1 - (RSS.p/TSS)  # R-squared measure = effect size?
#0.6476196

four <- meta%>%
  filter(Study=="Graham et al 2013" & Species == "Acropora tenuis")

y <- four$Respiration
x <- four$Age

K <- abs(min(y))
y <- y + K*(1+10^-15)

fit0 <- nls(y ~ I(a*exp(-b*x)), start=list(a=max(y), b=0.1), trace=T)

plot(y~x)

lines(x,predict(fit0,x),col="red")
summary(fit0)

RSS.p <- sum(residuals(fit0)^2)  # Residual sum of squares
TSS <- sum((y - mean(y))^2)  # Total sum of squares
1 - (RSS.p/TSS)  # R-squared measure = effect size?
#0.9203376

five <- meta%>%
  filter(Study=="Graham et al 2013" & Species == "Acropora nasuta")

y <- five$Respiration
x <- five$Age

K <- abs(min(y))
y <- y + K*(1+10^-15)

fit0 <- nls(y ~ I(a*exp(-b*x)), start=list(a=max(y), b=0.1), trace=T)

plot(y~x)
lines(x,predict(fit0,x),col="red")
summary(fit0)

RSS.p <- sum(residuals(fit0)^2)  # Residual sum of squares
TSS <- sum((y - mean(y))^2)  # Total sum of squares
1 - (RSS.p/TSS)  # R-squared measure = effect size?
#0.8178182

six <- meta%>%
  filter(Study=="Graham et al 2013" & Species == "Acropora spathulata")

y <- six$Respiration
x <- six$Age

K <- abs(min(y))
y <- y + K*(1+10^-15)

fit0 <- nls(y ~ I(a*exp(-b*x)), start=list(a=max(y), b=0.1), trace=T)

plot(y~x)
lines(x,predict(fit0,x),col="red")
summary(fit0)

RSS.p <- sum(residuals(fit0)^2)  # Residual sum of squares
TSS <- sum((y - mean(y))^2)  # Total sum of squares
1 - (RSS.p/TSS)  # R-squared measure = effect size?
#0.2342509

seven <- meta%>%
  filter(Study=="Harii et al 2010" & Species == "Pocillopora damicornis")

y <- seven$Respiration
x <- seven$Age

K <- abs(min(y))
y <- y + K*(1+10^-15)

fit0 <- nls(y ~ I(a*exp(-b*x)), start=list(a=max(y), b=0.1), trace=T)

plot(y~x)
lines(x,predict(fit0,x),col="red")
summary(fit0)

RSS.p <- sum(residuals(fit0)^2)  # Residual sum of squares
TSS <- sum((y - mean(y))^2)  # Total sum of squares
1 - (RSS.p/TSS)  # R-squared measure = effect size?
#0.6000948

eight <- meta%>%
  filter(Study=="Harii et al 2010" & Species == "Montipora digitata")

y <- eight$Respiration
x <- eight$Age

K <- abs(min(y))
y <- y + K*(1+10^-15)

fit0 <- nls(y ~ I(a*exp(-b*x)), start=list(a=max(y), b=0.1), trace=T)

plot(y~x)
lines(x,predict(fit0,x),col="red")
summary(fit0)

RSS.p <- sum(residuals(fit0)^2)  # Residual sum of squares
TSS <- sum((y - mean(y))^2)  # Total sum of squares
1 - (RSS.p/TSS)  # R-squared measure = effect size?
#0.800597

nine <- meta%>%
  filter(Study=="Jiang et al 2020")

y <- nine$Respiration
x <- nine$Age

K <- abs(min(y))
y <- y + K*(1+10^-15)

fit0 <- nls(y ~ I(a*exp(-b*x)), start=list(a=max(y), b=0.1), trace=T)

plot(y~x)
lines(x,predict(fit0,x),col="red")
summary(fit0)

RSS.p <- sum(residuals(fit0)^2)  # Residual sum of squares
TSS <- sum((y - mean(y))^2)  # Total sum of squares
1 - (RSS.p/TSS)  # R-squared measure = effect size?
#0.1769152

ten <- meta%>%
  filter(Study=="Okubo et al 2008")

y <- ten$Respiration
x <- ten$Age

K <- abs(min(y))
y <- y + K*(1+10^-15)

fit0 <- nls(y ~ I(a*exp(-b*x)), start=list(a=max(y), b=0.1), trace=T)

plot(y~x)
lines(x,predict(fit0,x),col="red")
summary(fit0)

RSS.p <- sum(residuals(fit0)^2)  # Residual sum of squares
TSS <- sum((y - mean(y))^2)  # Total sum of squares
1 - (RSS.p/TSS)  # R-squared measure = effect size?
#0.2186531

################# CLASSIC META-ANALYSIS: FITTING EXP, LM AND QUAD FOR NORMALIZED RESPIRATION #################################
meta <- justbiomass%>%
  filter(!Study %in% c("Olsen et al 2013", 
                       "Gaither and Rowan 2010", 
                       "Albright and Langdon 2011"))

one <- meta%>%
  filter(Study=="Cumbo et al 2012")

y <- one$normresp_nmolO2.microgramDW.min
x <- one$Age

K <- abs(min(y))
y <- y + K*(1+10^-15)

fit0 <- nls(y ~ I(a*exp(-b*x)), start=list(a=max(y), b=0.1), trace=T) #b=growth or decay rate
#a=inital value (the amount before measuring growth or decay)

plot(y~x)
lines(x,predict(fit0,x),col="red")

RSS.p <- sum(residuals(fit0)^2)  # Residual sum of squares
TSS <- sum((y - mean(y))^2)  # Total sum of squares
1 - (RSS.p/TSS)  # R-squared measure = effect size?
#[1] +0.1748616

fit  <- lm(y~x)
fit2 <- lm(y~poly(x,2,raw=TRUE))

plot(x,y,pch=19)
lines(xx, predict(fit, data.frame(x=xx)), col="red")
lines(xx, predict(fit2, data.frame(x=xx)), col="green")

summary(fit)
summary(fit2)
AIC(fit0, fit,fit2)
#Fit 2, fit, fit 0 
#quad: -0.774

two <- meta%>%
  filter(Study=="Cumbo et al 2013b")

y <- two$normresp_nmolO2.microgramDW.min
x <- two$Age

K <- abs(min(y))
y <- y + K*(1+10^-15)

fit0 <- nls(y ~ I(a*exp(-b*x)), start=list(a=max(y), b=0.1), trace=T) #b=growth or decay rate
#a=inital value (the amount before measuring growth or decay)

plot(y~x)
lines(x,predict(fit0,x),col="red")

RSS.p <- sum(residuals(fit0)^2)  # Residual sum of squares
TSS <- sum((y - mean(y))^2)  # Total sum of squares
1 - (RSS.p/TSS)  # R-squared measure = effect size?
#[1] +0.1297843

fit  <- lm(y~x)
fit2 <- lm(y~poly(x,2,raw=TRUE))

plot(x,y,pch=19)
lines(xx, predict(fit, data.frame(x=xx)), col="red")
lines(xx, predict(fit2, data.frame(x=xx)), col="green")

summary(fit)
summary(fit2)
AIC(fit0, fit,fit2)
#fit2: +0.2287

three <- meta%>%
  filter(Study=="Graham et al 2013" & Species == "Goniastrea aspera")

y <- three$normresp_nmolO2.microgramDW.min
x <- three$Age

K <- abs(min(y))
y <- y + K*(1+10^-15)

fit0 <- nls(y ~ I(a*exp(-b*x)), start=list(a=max(y), b=0.1), trace=T) #b=growth or decay rate
#a=inital value (the amount before measuring growth or decay)

plot(y~x)
lines(x,predict(fit0,x),col="red")

RSS.p <- sum(residuals(fit0)^2)  # Residual sum of squares
TSS <- sum((y - mean(y))^2)  # Total sum of squares
1 - (RSS.p/TSS)  # R-squared measure = effect size?
#[1] -0.09562319

fit  <- lm(y~x)
fit2 <- lm(y~poly(x,2,raw=TRUE))

plot(x,y,pch=19)
lines(xx, predict(fit, data.frame(x=xx)), col="red")
lines(xx, predict(fit2, data.frame(x=xx)), col="green")

summary(fit)
summary(fit2)
AIC(fit0, fit,fit2)
#quad: +0.5077

four <- meta%>%
  filter(Study=="Graham et al 2013" & Species == "Acropora tenuis")

y <- four$normresp_nmolO2.microgramDW.min
x <- four$Age

K <- abs(min(y))
y <- y + K*(1+10^-15)

fit0 <- nls(y ~ I(a*exp(-b*x)), start=list(a=max(y), b=0.1), trace=T) #b=growth or decay rate
#a=inital value (the amount before measuring growth or decay)

plot(y~x)
lines(x,predict(fit0,x),col="red")

RSS.p <- sum(residuals(fit0)^2)  # Residual sum of squares
TSS <- sum((y - mean(y))^2)  # Total sum of squares
1 - (RSS.p/TSS)  # R-squared measure = effect size?
#[1] -0.4314008

fit  <- lm(y~x)
fit2 <- lm(y~poly(x,2,raw=TRUE))

plot(x,y,pch=19)
lines(xx, predict(fit, data.frame(x=xx)), col="red")
lines(xx, predict(fit2, data.frame(x=xx)), col="green")

summary(fit)
summary(fit2)
AIC(fit0, fit,fit2)
#quad: +0.4078

five <- meta%>%
  filter(Study=="Graham et al 2013" & Species == "Acropora nasuta")

y <- five$normresp_nmolO2.microgramDW.min
x <- five$Age

K <- abs(min(y))
y <- y + K*(1+10^-15)

fit0 <- nls(y ~ I(a*exp(-b*x)), start=list(a=max(y), b=0.1), trace=T) #b=growth or decay rate
#a=inital value (the amount before measuring growth or decay)

plot(y~x)
lines(x,predict(fit0,x),col="red")

RSS.p <- sum(residuals(fit0)^2)  # Residual sum of squares
TSS <- sum((y - mean(y))^2)  # Total sum of squares
1 - (RSS.p/TSS)  # R-squared measure = effect size?
#[1] -0.1888999

fit  <- lm(y~x)
fit2 <- lm(y~poly(x,2,raw=TRUE))

plot(x,y,pch=19)
lines(xx, predict(fit, data.frame(x=xx)), col="red")
lines(xx, predict(fit2, data.frame(x=xx)), col="green")
AIC(fit,fit2)

summary(fit)
summary(fit2)
AIC(fit0, fit,fit2)
#Quad: +0.7614

six <- meta%>%
  filter(Study=="Graham et al 2013" & Species == "Acropora spathulata")

y <- six$normresp_nmolO2.microgramDW.min
x <- six$Age

K <- abs(min(y))
y <- y + K*(1+10^-15)

fit0 <- nls(y ~ I(a*exp(-b*x)), start=list(a=max(y), b=0.1), trace=T) #b=growth or decay rate
#a=inital value (the amount before measuring growth or decay)

plot(y~x)
lines(x,predict(fit0,x),col="red")

RSS.p <- sum(residuals(fit0)^2)  # Residual sum of squares
TSS <- sum((y - mean(y))^2)  # Total sum of squares
1 - (RSS.p/TSS)  # R-squared measure = effect size?
#[1] -0.00310165

fit  <- lm(y~x)
fit2 <- lm(y~poly(x,2,raw=TRUE))

plot(x,y,pch=19)
lines(xx, predict(fit, data.frame(x=xx)), col="red")
lines(xx, predict(fit2, data.frame(x=xx)), col="green")

summary(fit)
summary(fit2)
AIC(fit0, fit,fit2)
#quad: +0.6693


seven <- meta%>%
  filter(Study=="Harii et al 2010" & Species == "Pocillopora damicornis")

y <- seven$normresp_nmolO2.microgramDW.min
x <- seven$Age

K <- abs(min(y))
y <- y + K*(1+10^-15)

fit0 <- nls(y ~ I(a*exp(-b*x)), start=list(a=max(y), b=0.1), trace=T) #b=growth or decay rate
#a=inital value (the amount before measuring growth or decay)

plot(y~x)
lines(x,predict(fit0,x),col="red")

RSS.p <- sum(residuals(fit0)^2)  # Residual sum of squares
TSS <- sum((y - mean(y))^2)  # Total sum of squares
1 - (RSS.p/TSS)  # R-squared measure = effect size?
#[1] +0.8888428

fit  <- lm(y~x)
fit2 <- lm(y~poly(x,2,raw=TRUE))

plot(x,y,pch=19)
lines(xx, predict(fit, data.frame(x=xx)), col="red")
lines(xx, predict(fit2, data.frame(x=xx)), col="green")

summary(fit)
summary(fit2)
AIC(fit0, fit,fit2)
#quad +0.9483

eight <- meta%>%
  filter(Study=="Harii et al 2010" & Species == "Montipora digitata")

y <- eight$normresp_nmolO2.microgramDW.min
x <- eight$Age

K <- abs(min(y))
y <- y + K*(1+10^-15)

fit0 <- nls(y ~ I(a*exp(-b*x)), start=list(a=max(y), b=0.1), trace=T) #b=growth or decay rate
#a=inital value (the amount before measuring growth or decay)

plot(y~x)
lines(x,predict(fit0,x),col="red")

RSS.p <- sum(residuals(fit0)^2)  # Residual sum of squares
TSS <- sum((y - mean(y))^2)  # Total sum of squares
1 - (RSS.p/TSS)  # R-squared measure = effect size?
#[1] +0.9559308

fit  <- lm(y~x)
fit2 <- lm(y~poly(x,2,raw=TRUE))

plot(x,y,pch=19)
lines(xx, predict(fit, data.frame(x=xx)), col="red")
lines(xx, predict(fit2, data.frame(x=xx)), col="green")

summary(fit)
summary(fit2)
AIC(fit0, fit,fit2)
#quad: +0.9938


nine <- meta%>%
  filter(Study=="Jiang et al 2020")

y <- nine$normresp_nmolO2.microgramDW.min
x <- nine$Age

K <- abs(min(y))
y <- y + K*(1+10^-15)

fit0 <- nls(y ~ I(a*exp(-b*x)), start=list(a=max(y), b=0.1), trace=T) #b=growth or decay rate
#a=inital value (the amount before measuring growth or decay)

plot(y~x)
lines(x,predict(fit0,x),col="red")

RSS.p <- sum(residuals(fit0)^2)  # Residual sum of squares
TSS <- sum((y - mean(y))^2)  # Total sum of squares
1 - (RSS.p/TSS)  # R-squared measure = effect size?
#[1] -0.1769152

fit  <- lm(y~x)
fit2 <- lm(y~poly(x,2,raw=TRUE))

plot(x,y,pch=19)
lines(xx, predict(fit, data.frame(x=xx)), col="red")
lines(xx, predict(fit2, data.frame(x=xx)), col="green")

summary(fit)
summary(fit2)
AIC(fit0, fit,fit2)
#quad: - 0.8333

ten <- meta%>%
  filter(Study=="Okubo et al 2008")

y <- ten$normresp_nmolO2.microgramDW.min
x <- ten$Age

K <- abs(min(y))
y <- y + K*(1+10^-15)

fit0 <- nls(y ~ I(a*exp(-b*x)), start=list(a=max(y), b=0.1), trace=T) #b=growth or decay rate
#a=inital value (the amount before measuring growth or decay)

plot(y~x)
lines(x,predict(fit0,x),col="red")

RSS.p <- sum(residuals(fit0)^2)  # Residual sum of squares
TSS <- sum((y - mean(y))^2)  # Total sum of squares
1 - (RSS.p/TSS)  # R-squared measure = effect size?
#[1] -0.2186531

fit  <- lm(y~x)
fit2 <- lm(y~poly(x,2,raw=TRUE))

plot(x,y,pch=19)
lines(xx, predict(fit, data.frame(x=xx)), col="red")
lines(xx, predict(fit2, data.frame(x=xx)), col="green")

summary(fit)
summary(fit2)
AIC(fit0, fit,fit2)
#quad +0.273

################# CLASSIC META-ANALYSIS: EXP_RESPIRATION  #################################
#Days post release is fixed...
vignette("diagram")

setwd("/Users/nina/Research/Research Ideas/AA Larval Energy Budget")
classic <-read_excel("Meta_analysis.xlsx",sheet="Classic_meta_exp_resp") #to read your file
str(classic)

#### MODELS WITH total n RAW RESPIRATION RATES 
#ri = vector with raw correlation coefficiencts
#ZCOR for fisher's r to z transformed correlation coefficient
dat <- escalc(measure="ZCOR", ri=R, ni=ntotal, data=classic, slab=Study) #slab=paste(authors, year, sep=", "
dat
#yi	= vector to specify the observed effect size or outcomes.
#vi	= vector to specify the corresponding sampling variances.

# Instead of specifying vi, one can specify the standard errors (the square root of the sampling variances) 
#via the sei argument. <- Don't want to do that because SE of exp curve?

#Fixed effects model
#Weighted estimation (with inverse-variance weights) is used by default.
fixed <- rma(measure="ZCOR", ri=R, ni=ntotal, method="FE", data=classic, slab=Study)
fixed

fixed1 <- rma(measure="ZCOR", ri=R, ni=ntotal, mods = ~ factor(Species), method="FE", data=classic, slab=Study)
fixed1

fixed2 <- rma(measure="ZCOR", ri=R, ni=ntotal, mods = ~ factor(Lab), method="FE", data=classic, slab=Study)
fixed2

fixed3 <- rma(measure="ZCOR", ri=R, ni=ntotal, mods = cbind(factor(Lab), factor(Species)), method="FE", data=classic, slab=Study)
fixed3

hc(fixed) #sensitivity analysis?

#method to one of the various estimators for the amount of heterogeneity
random <- rma(measure="ZCOR", ri=R, ni=ntotal, data=classic, slab=Study, method="ML")
random

random1 <- rma(measure="ZCOR", ri=R, ni=ntotal, mods = ~ factor(Species), data=classic, slab=Study, method="ML")
random1

#why not the same as above???
#randomtest <- rma(measure="ZCOR", ri=R, ni=ntotal, mods = cbind(factor(Species)), data=classic, slab=Study, method="ML")
random2 <- rma(measure="ZCOR", ri=R, ni=ntotal, mods = ~ factor(Lab), data=classic, slab=Study, method="ML")
random2
#Response: R
#Predictor: Study
#Random effects: Lab

### mixed-effects model with species as factor #DIFFERENT OUTPUT #on-sided formula
random3 <- rma(measure="ZCOR", ri=R, ni=ntotal, mods = cbind(factor(Species),factor(Lab)), data=classic, slab=Study, method="ML")
random3

AIC(fixed, fixed1, fixed2, fixed3, random, random1, random2, random3)
#Best model is random effects: random2

#I^2 is between study variance. lots of it! so random is def better. 
summary(fixed) #no mods
summary(fixed1) #Species
summary(fixed2) #Lab
summary(fixed3) #Species and lab
summary(random) #no mods
summary(random1) #Species: 177.0667, R2: 76.35% 
summary(random2) #Lab: I^2: 76.68%, R2: 90.16%%, p=0.2807 so not significant from 0?
summary(random3) #Species and lab: 40.8217 , R2: 36.04% <- why less when it has more variables?


summary(random2)
#intercept: which represents the mean of the first level of the factor
#A test for the mediation of a moderator effect examines whether the magnitude 
#of an overall interaction effect of the independent variable (X) (study) and the 
#moderator variable (Z) (Lab) on the dependent variable (Y) (Exp curve) is reduced once the 
#mediator is accounted for in the model (Muller et al. 2005).

#It is hetergeneous
#It is reduced by adding in the moderator
#Still lots of between study variance: too much to do meta-analysis?

### average correlation with 95% CI
predict(random2, digits=3, transf=transf.ztor)  #Why can't I get a value for a mixed effects model??? 
#Because different intercepts for each one?

### forest plot
forest(random2, addcred=TRUE, atransf=transf.ztor, cex=.9, header="Author(s), Year", digits=c(2,1))
#digits=c(2,1)
#xlim=c(-1.6,1.6)
#at=transf.rtoz(c(-.4,-.2,0,.2,.4,.6))
#addcred: logical specifying whether the bounds of the credibility/prediction interval 
#should be added to the plot 
#atransf: To go from z scores back to correlation coefficients

### funnel plot
funnel(random2) #no publication bias
qqnorm(random2) #looks normal
baujat(random2)
gosh(random2) #IDK what this does
plot(random2)
permutest(random2)
#Test of Moderators (coefficients 2:6):
#QM(df = 5) = 59.5217, p-val* = 0.0610
#p-value is significant so it means that the labs are making a significant difference
#As would be estimated based on 1000 permutations
#cute site about permutation testing: https://www.jwilber.me/permutationtest/

#not significant difference between curves among studies


################## CLASSIC META: BROODING: EXP_RESPIRATION ############################################
setwd("/Users/nina/Research/Research Ideas/AA Larval Energy Budget")
classic <-read_excel("Meta_analysis.xlsx",sheet="Classic_meta_brooding") #to read your file
str(classic)

dat <- escalc(measure="ZCOR", ri=R, ni=ntotal, data=classic, slab=Study) #slab=paste(authors, year, sep=", "
dat

fixed <- rma(measure="ZCOR", ri=R, ni=ntotal, method="FE", data=classic, slab=Study)
fixed

fixed2 <- rma(measure="ZCOR", ri=R, ni=ntotal, mods = ~ factor(Lab), method="FE", data=classic, slab=Study)
fixed2

hc(fixed) #sensitivity analysis?

#method to one of the various estimators for the amount of heterogeneity
random <- rma(measure="ZCOR", ri=R, ni=ntotal, data=classic, slab=Study, method="ML")
random

#randomtest <- rma(measure="ZCOR", ri=R, ni=ntotal, mods = cbind(factor(Species)), data=classic, slab=Study, method="ML")
random2 <- rma(measure="ZCOR", ri=R, ni=ntotal, mods = ~ factor(Lab), data=classic, slab=Study, method="ML")
random2

AIC(fixed,  fixed2, random,  random2)
#Best model is random effects: random2

#I^2 is between study variance. lots of it! so random is def better. 
summary(fixed2) #no mods

predict(fixed2, digits=3, transf=transf.ztor)  #Why can't I get a value for a mixed effects model??? 
#Because different intercepts for each one?

### forest plot
forest(fixed2, addcred=TRUE, atransf=transf.ztor, cex=.9, header="Author(s), Year", digits=c(2,1))
#digits=c(2,1)
#xlim=c(-1.6,1.6)
#at=transf.rtoz(c(-.4,-.2,0,.2,.4,.6))
#addcred: logical specifying whether the bounds of the credibility/prediction interval 
#should be added to the plot 
#atransf: To go from z scores back to correlation coefficients

### funnel plot
funnel(fixed2) #no publication bias
qqnorm(fixed2) #looks normal
plot(fixed2)

################## CLASSIC META: BROODING: EXP_RESPIRATION  ############################################
setwd("/Users/nina/Research/Research Ideas/AA Larval Energy Budget")
classic <-read_excel("Meta_analysis.xlsx",sheet="Classic_meta_broadcasting") #to read your file
str(classic)

dat <- escalc(measure="ZCOR", ri=R, ni=ntotal, data=classic, slab=Study) #slab=paste(authors, year, sep=", "
dat
#yi	= vector to specify the observed effect size or outcomes.
#vi	= vector to specify the corresponding sampling variances.

# Instead of specifying vi, one can specify the standard errors (the square root of the sampling variances) 
#via the sei argument. <- Don't want to do that because SE of exp curve?

#Fixed effects model
#Weighted estimation (with inverse-variance weights) is used by default.
fixed <- rma(measure="ZCOR", ri=R, ni=ntotal, method="FE", data=classic, slab=Study)
fixed

fixed2 <- rma(measure="ZCOR", ri=R, ni=ntotal, mods = ~ factor(Lab), method="FE", data=classic, slab=Study)
fixed2

hc(fixed) #sensitivity analysis?

#method to one of the various estimators for the amount of heterogeneity
random <- rma(measure="ZCOR", ri=R, ni=ntotal, data=classic, slab=Study, method="ML")
random

#why not the same as above???
#randomtest <- rma(measure="ZCOR", ri=R, ni=ntotal, mods = cbind(factor(Species)), data=classic, slab=Study, method="ML")
random2 <- rma(measure="ZCOR", ri=R, ni=ntotal, mods = ~ factor(Lab), data=classic, slab=Study, method="ML")
random2
#Response: R
#Predictor: Study
#Random effects: Lab

AIC(fixed, fixed2, random,  random2)
#Best model is random effects: random2

#I^2 is between study variance. lots of it! so random is def better. 
summary(fixed) #no mods
summary(fixed1) #Species
summary(fixed2) #Lab
summary(fixed3) #Species and lab
summary(random) #no mods
summary(random1) #Species: 177.0667, R2: 76.35% 
summary(random2) #Lab: I^2: 76.68%, R2: 90.16%%, p=0.2807 so not significant from 0?
summary(random3) #Species and lab: 40.8217 , R2: 36.04% <- why less when it has more variables?


summary(random2)
#intercept: which represents the mean of the first level of the factor
#A test for the mediation of a moderator effect examines whether the magnitude 
#of an overall interaction effect of the independent variable (X) (study) and the 
#moderator variable (Z) (Lab) on the dependent variable (Y) (Exp curve) is reduced once the 
#mediator is accounted for in the model (Muller et al. 2005).

#It is hetergeneous
#It is reduced by adding in the moderator
#Still lots of between study variance: too much to do meta-analysis?

### average correlation with 95% CI
predict(random2, digits=3, transf=transf.ztor)  #Why can't I get a value for a mixed effects model??? 
#Because different intercepts for each one?

### forest plot
forest(random2, addcred=TRUE, atransf=transf.ztor, cex=.9, header="Author(s), Year", digits=c(2,1))
#digits=c(2,1)
#xlim=c(-1.6,1.6)
#at=transf.rtoz(c(-.4,-.2,0,.2,.4,.6))
#addcred: logical specifying whether the bounds of the credibility/prediction interval 
#should be added to the plot 
#atransf: To go from z scores back to correlation coefficients

### funnel plot
funnel(random2) #no publication bias
qqnorm(random2) #looks normal
baujat(random2)
gosh(random2) #IDK what this does
plot(random2)
permutest(random2)

################# CLASSIC META-ANALYSIS: QUAD_NORM_RESPIRATION  #################################
#Days post release is fixed...
vignette("diagram")

setwd("/Users/nina/Research/Research Ideas/AA Larval Energy Budget")
classic <-read_excel("Meta_analysis.xlsx",sheet="Classic_meta_quad_norm_resp") #to read your file
str(classic)

dat <- escalc(measure="ZCOR", ri=R, ni=ntotal, data=classic, slab=Study) #slab=paste(authors, year, sep=", "
dat

fixed <- rma(measure="ZCOR", ri=R, ni=ntotal, method="FE", data=classic, slab=Study)
fixed

fixed1 <- rma(measure="ZCOR", ri=R, ni=ntotal, mods = ~ factor(Species), method="FE", data=classic, slab=Study)
fixed1

fixed2 <- rma(measure="ZCOR", ri=R, ni=ntotal, mods = ~ factor(Lab), method="FE", data=classic, slab=Study)
fixed2

fixed3 <- rma(measure="ZCOR", ri=R, ni=ntotal, mods = cbind(factor(Lab), factor(Species)), method="FE", data=classic, slab=Study)
fixed3

hc(fixed) #sensitivity analysis?

#method to one of the various estimators for the amount of heterogeneity
random <- rma(measure="ZCOR", ri=R, ni=ntotal, data=classic, slab=Study, method="ML")
random

random1 <- rma(measure="ZCOR", ri=R, ni=ntotal, mods = ~ factor(Species), data=classic, slab=Study, method="ML")
random1

#why not the same as above???
#randomtest <- rma(measure="ZCOR", ri=R, ni=ntotal, mods = cbind(factor(Species)), data=classic, slab=Study, method="ML")
random2 <- rma(measure="ZCOR", ri=R, ni=ntotal, mods = ~ factor(Lab), data=classic, slab=Study, method="ML")
random2
#Response: R
#Predictor: Study
#Random effects: Lab

### mixed-effects model with species as factor #DIFFERENT OUTPUT #on-sided formula
random3 <- rma(measure="ZCOR", ri=R, ni=ntotal, mods = cbind(factor(Species),factor(Lab)), data=classic, slab=Study, method="ML")
random3

AIC(fixed, fixed1, fixed2, fixed3, random, random1, random2, random3)
#Best model is random effects: random2
summary(fixed) #no mods
summary(fixed1) #Species
summary(fixed2) #Lab
summary(fixed3) #Species and lab
summary(random) #no mods
summary(random1) #Species: 177.0667, R2: 76.35% 
summary(random2) #Lab: I^2: 76.68%, R2: 90.16%%, p=0.2807 so not significant from 0?
summary(random3) #Species and lab: 40.8217 , R2: 36.04% <- why less when it has more variables?

summary(fixed2)
summary(random2)

#intercept: which represents the mean of the first level of the factor
#A test for the mediation of a moderator effect examines whether the magnitude 
#of an overall interaction effect of the independent variable (X) (study) and the 
#moderator variable (Z) (Lab) on the dependent variable (Y) (Exp curve) is reduced once the 
#mediator is accounted for in the model (Muller et al. 2005).

#It is hetergeneous
#It is reduced by adding in the moderator
#Still lots of between study variance: too much to do meta-analysis?

### average correlation with 95% CI
predict(random2, digits=3, transf=transf.ztor)  #Why can't I get a value for a mixed effects model??? 
#Because different intercepts for each one?

### forest plot
forest(random2, addcred=TRUE, atransf=transf.ztor, cex=.9, header="Author(s), Year", digits=c(2,1))
#digits=c(2,1)
#xlim=c(-1.6,1.6)
#at=transf.rtoz(c(-.4,-.2,0,.2,.4,.6))
#addcred: logical specifying whether the bounds of the credibility/prediction interval 
#should be added to the plot 
#atransf: To go from z scores back to correlation coefficients

### funnel plot
funnel(random2) #no publication bias
qqnorm(random2) #looks normal
baujat(random2)
gosh(random2) #IDK what this does
plot(random2)
permutest(random2)

################# CLASSIC META-ANALYSIS: QUAD_NORM_RESPIRATION: BROODING  #################################
#Days post release is fixed...
vignette("diagram")

setwd("/Users/nina/Research/Research Ideas/AA Larval Energy Budget")
classic <-read_excel("Meta_analysis.xlsx",sheet="Classic_quad_norm_resp_brooding") #to read your file
str(classic)

dat <- escalc(measure="ZCOR", ri=R, ni=ntotal, data=classic, slab=Study) #slab=paste(authors, year, sep=", "
dat

fixed <- rma(measure="ZCOR", ri=R, ni=ntotal, method="FE", data=classic, slab=Study)
fixed

hc(fixed) #sensitivity analysis?

#method to one of the various estimators for the amount of heterogeneity
random <- rma(measure="ZCOR", ri=R, ni=ntotal, data=classic, slab=Study, method="ML")
random

AIC(fixed, random)

summary(random)

#intercept: which represents the mean of the first level of the factor
#A test for the mediation of a moderator effect examines whether the magnitude 
#of an overall interaction effect of the independent variable (X) (study) and the 
#moderator variable (Z) (Lab) on the dependent variable (Y) (Exp curve) is reduced once the 
#mediator is accounted for in the model (Muller et al. 2005).

#It is hetergeneous
#It is reduced by adding in the moderator
#Still lots of between study variance: too much to do meta-analysis?

### average correlation with 95% CI
predict(random, digits=3, transf=transf.ztor)  #Why can't I get a value for a mixed effects model??? 
#Because different intercepts for each one?

### forest plot
forest(random2, addcred=TRUE, atransf=transf.ztor, cex=.9, header="Author(s), Year", digits=c(2,1))
#digits=c(2,1)
#xlim=c(-1.6,1.6)
#at=transf.rtoz(c(-.4,-.2,0,.2,.4,.6))
#addcred: logical specifying whether the bounds of the credibility/prediction interval 
#should be added to the plot 
#atransf: To go from z scores back to correlation coefficients

### funnel plot
funnel(random2) #no publication bias
qqnorm(random2) #looks normal
baujat(random2)
gosh(random2) #IDK what this does
plot(random2)
permutest(random2)


################# CLASSIC META-ANALYSIS: QUAD_NORM_RESPIRATION: BROADCASTING  #################################
#Days post release is fixed...
vignette("diagram")

setwd("/Users/nina/Research/Research Ideas/AA Larval Energy Budget")
classic <-read_excel("Meta_analysis.xlsx",sheet="Classic_quad_norm_resp_broadcas") #to read your file
str(classic)

dat <- escalc(measure="ZCOR", ri=R, ni=ntotal, data=classic, slab=Study) #slab=paste(authors, year, sep=", "
dat

fixed <- rma(measure="ZCOR", ri=R, ni=ntotal, method="FE", data=classic, slab=Study)
fixed

fixed2 <- rma(measure="ZCOR", ri=R, ni=ntotal, mods = ~ factor(Lab), method="FE", data=classic, slab=Study)
fixed2

hc(fixed) #sensitivity analysis?

#method to one of the various estimators for the amount of heterogeneity
random <- rma(measure="ZCOR", ri=R, ni=ntotal, data=classic, slab=Study, method="ML")
random

#why not the same as above???
#randomtest <- rma(measure="ZCOR", ri=R, ni=ntotal, mods = cbind(factor(Species)), data=classic, slab=Study, method="ML")
random2 <- rma(measure="ZCOR", ri=R, ni=ntotal, mods = ~ factor(Lab), data=classic, slab=Study, method="ML")
random2
#Response: R
#Predictor: Study
#Random effects: Lab

AIC(fixed, fixed2, random, random2)
#Best model is random effects: random2
summary(fixed2) #Lab
summary(random2) #Lab: I^2: 76.68%, R2: 90.16%%, p=0.2807 so not significant from 0?

### average correlation with 95% CI
predict(random2, digits=3, transf=transf.ztor)  #Why can't I get a value for a mixed effects model??? 
#Because different intercepts for each one?

### forest plot
forest(random2, addcred=TRUE, atransf=transf.ztor, cex=.9, header="Author(s), Year", digits=c(2,1))
#digits=c(2,1)
#xlim=c(-1.6,1.6)
#at=transf.rtoz(c(-.4,-.2,0,.2,.4,.6))
#addcred: logical specifying whether the bounds of the credibility/prediction interval 
#should be added to the plot 
#atransf: To go from z scores back to correlation coefficients

### funnel plot
funnel(random2) #no publication bias
qqnorm(random2) #looks normal
baujat(random2)
gosh(random2) #IDK what this does
plot(random2)
permutest(random2)