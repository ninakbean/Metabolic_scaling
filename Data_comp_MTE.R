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
library(RColorBrewer)
library(patchwork)
library(rmarkdown)
library(ggpubr)
library(rstatix)
library(broom)
library(WRS2)
library(lsmeans)

#For Graham, age 64, there was only one point, so I said the se was 0.0000000001

setwd("/Users/nina/Projects/Larval-Respiration")
meta <-read_excel("Meta_analysis.xlsx",sheet="Repiration.Photosynthesis") #to read your file
str(meta)

experiments <- meta%>%
  group_by(Study, Species)%>%
  dplyr::summarize(Respiration_nmolO2larvmin=mean(Respiration_nmolO2larvmin, na.rm=TRUE))

studies <- experiments%>%
  group_by(Study)%>%
  dplyr::summarize(Respiration_nmolO2larvmin=mean(Respiration_nmolO2larvmin, na.rm=TRUE))

#When I collapse the averages, I should also add the sample size?
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
  dplyr::select(Study, Source_resp, Source_biomass, Ocean, Location, Family, Species, Symbionts,
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
#Just larvae under ambient seawater chemistry and temperatures

#This just took out that one study with only spat
experiments <- meta%>%
  group_by(Study, Species)%>%
  dplyr::summarize(Respiration=mean(Respiration, na.rm=TRUE))

studies <- experiments%>%
  group_by(Study)%>%
  dplyr::summarize(Respiration=mean(Respiration, na.rm=TRUE))


#Taking the lower number of replicates
#THESE DON'T MATTER BECAUSE NOT INCLUDED AT THE END ANYWAYS
meta["n_resp"][meta["Study"]=="Edmunds 2013"&meta["n_resp"]=="4-6"] <- "4" 
meta["n_resp"][meta["Study"]=="Serrano et al 2018"&meta["n_resp"]=="8-10"] <- "8"
meta["n_resp"][meta["Study"]=="Edmunds et al 2011"&meta["n_resp"]=="3-4"] <- "3"
meta["n_resp"][meta["Study"]=="Edmunds et al 2011"&meta["n_resp"]=="5-6"] <- "5"

meta$n_resp <- as.numeric(meta$n_resp) #Making respiration numeric

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


#Taking out studies with respiration not given

age1 <- meta1%>%
  #In another group, they were 1-2 days old. I don't have a 1 day old data point alone     
  filter(Respiration!="NA")%>% #Taking out studies with missing respiration
  filter(!Respiration<0) #Taking out negative respiration rates beause graph couldn't resolve them. Graham et al 2013: Age 15 & 24

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
ageavg <- age1

ageavg["Source_resp"][ageavg["Study"]=="Cumbo et al 2012"&ageavg["Source_resp"]=="Fig. 1"] <- "Fig. 1 & 3"
ageavg["Source_resp"][ageavg["Study"]=="Cumbo et al 2012"&ageavg["Source_resp"]=="Fig. 3"] <- "Fig. 1 & 3"
ageavg["Source_biomass"][ageavg["Study"]=="Cumbo et al 2012"&ageavg["Source_biomass"]=="Fig. 1"] <- "Fig. 1 & 3"
ageavg["Source_biomass"][ageavg["Study"]=="Cumbo et al 2012"&ageavg["Source_biomass"]=="Fig. 3"] <- "Fig. 1 & 3"

ageavg$n_resp <- as.character(ageavg$n_resp)

ageavg["n_resp"][ageavg["Study"]=="Cumbo et al 2013"&ageavg["n_resp"]=="2"] <- "2-4"
ageavg["n_resp"][ageavg["Study"]=="Cumbo et al 2013"&ageavg["n_resp"]=="4"] <- "2-4"

ageavg <- ageavg%>%
  filter(!(Study == "Cumbo et al 2012" & Biomass_assumptions == "Average from study"))%>% #How to filter out the row with these conditions, 1 row
  filter(!(Study == "Cumbo et al 2012" & Biomass_assumptions == "Repeated last value taken"))%>% #1 row
  filter(!(Study == "Harii et al 2010" & Biomass_assumptions == "Average from study")) #2 rows
  
#Need to definitely group by study, age, and species
#didn't group by n_resp, so Cumbo et al 2013 had two different n_resp that got merged
str(ageavg)

#This assumption is true, but won't collapse anyways..
#Cumbo 2012, Average from study isn't used anywayas because it is taken out later down below
ageavg1 <- ageavg%>%
  group_by(Study, Location, Season, Family, Species, Age, 
           Treatment_Temp, Reproduction_mode,Symbionts,
           Biomass_assumptions, Respiration_OG_units, n_resp)%>%
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

experiments <- ageavg1%>%
  group_by(Study, Species)%>%
  dplyr::summarize(Respiration=mean(Respiration, na.rm=TRUE))

studies <- experiments%>%
  group_by(Study)%>%
  dplyr::summarize(Respiration=mean(Respiration, na.rm=TRUE))

levels(ageavg1$Study) #This shows that the studies aren't really taken out

ageavg1 <- droplevels(ageavg1) #Dropping levels for real

justbiomass <- ageavg1%>%
  ungroup(Study, Location, Season, Family, Species, Age, 
          Treatment_Temp, Reproduction_mode,Symbionts,
          Biomass_assumptions, Respiration_OG_units, n_resp)%>%
  mutate(normresp_nmolO2.microgramDW.min=(Respiration/Biomass_microg.larva_dry))%>%
  mutate(normresp_SE=(Respiration_SE/Biomass_microg.larva_dry))%>%
  mutate(normresp_Ivar=(Inverse_var/Biomass_microg.larva_dry))

#NOT INCLUDING <- no size measurement
#"Haryanti and Hidaka 2015"
#"Rodriguez-Lanetty et al 2009"
#"Olsen et al 2013", 
#"Ross et al 2013", 
#"Okubo et al 2008"
# "Nakamura et al 2011",  <= #Nakamura study: orders of magnitude smaller resp rates. Probably wrong
#"Cumbo et al 2013", 
#"Ross et al 2010", 
#"Serrano et al 2018", 
#"Albright and Langdon 2011", 
#"Jiang et al 2020", 
#"Zhou et al 2016"

#NOT INCLUDING <- length, width, area
#"Edmunds 2013" <- not good because don't know what respiration measure it corresponds to
#^^^ multiple respiration, single biomass for all, wet biomass
#"Edmunds et al 2011" <- not good because don't know what respiration measure it corresponds to
#^^^ multiple respiration, single biomass for all, wet biomass
#"Richmond 1987" <- dry biomass
#^^^two respiration rows and one biomass measure because type Y and type B
#"Titlyanov et al 1998" <- diameter
# "Putnam and Gates 2015" <- area
#"Putnam et al 2013", <- area
#"Kitchen et al 2020",  <- area

# Studies with biomass
# "Rivest and Hofmann 2014"
#"Cumbo et al 2012"
#"Edmunds et al 2001",
# "Harii et al 2010"
#"Graham et al 2013",
#"Gaither and Rowan 2010", <- dry biomass
#"Cumbo et al 2013b", 

#TABLE
#The only respiration assumption is going from protein normalized resp -> by larva

MTE <- justbiomass%>%
  filter(!(Study == "Edmunds et al 2001" & Protein_microg.larva_dry == "NA"))%>% #How to filter out the row with these conditions
  filter(Study %in% c("Rivest and Hofmann 2014",
                      "Cumbo et al 2012",
                      "Edmunds et al 2001",
                      "Harii et al 2010",
                      "Graham et al 2013",
                      "Gaither and Rowan 2010",
                      "Cumbo et al 2013b"))%>%
  group_by(Study, Location, Season, Family, Species, Age, 
           Treatment_Temp, Reproduction_mode,Symbionts,
           Biomass_assumptions, Respiration_OG_units, n_resp)%>%
  dplyr::summarize(normresp_nmolO2.microgramDW.min=mean(normresp_nmolO2.microgramDW.min), 
                   Biomass_microg.larva_dry=mean(Biomass_microg.larva_dry),
                   Respiration=mean(Respiration),
                   Respiration_SE=mean(Respiration_SE), 
                   Respiration_OG=mean(Respiration_OG),
                   Respiration_OG_SE=mean(Respiration_OG_SE))%>%
  mutate(log_normresp = log10(normresp_nmolO2.microgramDW.min),
         log_biomass = log10(Biomass_microg.larva_dry),
         log_resp = log10(Respiration))%>%
  filter(log_resp!="-Inf")%>% #IDK what this is...
  mutate(Age_category=cut(Age, breaks=c(0, 10, 20, 64), labels=c("low","middle","high")))%>%
  ungroup(Reproduction_mode)


experiments <- MTE%>%
  group_by(Study, Species)%>%
  dplyr::summarize(Respiration=mean(Respiration, na.rm=TRUE))

studies <- experiments%>%
  group_by(Study)%>%
  dplyr::summarize(Respiration=mean(Respiration, na.rm=TRUE))

#Raw data names
#MTE
#brood
#broad

#winsorized
#wins
#wins_broad
#wins_brood

#I have the whole raw data set, then I winsorized the whole data set. Then split between modes
#compare slopes between brood, broad vs wins_brood, wins_broad
#log_resp vs log_respW
#Probably better to not winsorize at all unless there is a big reason to
#winsorizing y axis of all the data made the fit for broadcasting data 4% better. 

brood <- MTE%>%
  filter(Reproduction_mode=="brooding")

broad <- MTE%>%
  filter(Reproduction_mode=="broadcasting")

plot(brood$Biomass_microg.larva_dry, brood$Respiration)
plot(broad$Biomass_microg.larva_dry, broad$Respiration)

#winsorized raw data 
wins <- MTE%>%
  mutate(log_respW = Winsorize(log_resp, na.rm=TRUE))

wins_broad <- wins%>%
  filter(Reproduction_mode=="broadcasting")

wins_brood <- wins%>%
  filter(Reproduction_mode=="brooding")

#Combined data with winsorized data of brooding and broadcasting SEPERATELY
#I don’t think I can winterize separately because then I’m analyzing together.. 
#and separating based on brooding and broadcasting is my own thing, but can get own slope
wins_brood_broad <- rbind(wins_brood,wins_broad)

yes_symb <- MTE%>% #(All brooding + M. digitata)
  filter(Symbionts=="yes")

no_symb <- MTE%>% #(all broadcasting except for M digitata)
  filter(Symbionts=="no")

table <- MTE

table$Respiration <- round(as.numeric(table$Respiration), digits=1)
table$Respiration_SE <- round(as.numeric(table$Respiration_SE), digits=1)
table$Respiration_OG <- signif(as.numeric(table$Respiration_OG), digits=3)
table$Respiration_OG_SE <- signif(as.numeric(table$Respiration_OG_SE), digits=3)

str(table)

#making assumptions letters
#This was already explained in the methods table["Respiration_conversion_assumptions"][table["Study"]=="Cumbo et al 2013b"&table["Respiration_conversion_assumptions"]=="Using mg protein per larva from study measurements"] <- "A"
table["Biomass_assumptions"][table["Biomass_assumptions"]=="(Richmond 1987) P. damicornis: 17% protein of total dry weight"] <- "A"
table["Biomass_assumptions"][table["Biomass_assumptions"]=="(Edmunds 2001): P. astreoides: protein = 3.55 micrograms & (Richmond 1987) P. damicornis: 17% protein of total dry weight"] <- "A" #because the protein measure is from that study
table["Biomass_assumptions"][table["Biomass_assumptions"]=="(Harii et al. 2007): two Acropora sp.: 59.5% lipids of total dry weight"] <- "B"
table["Biomass_assumptions"][table["Biomass_assumptions"]=="(Harii et al. 2007) & (Richmond 1987): P. damicornis: 69.05% lipids of total dry weight"] <- "C"

#combining respiration +- and assumptions
table <- table%>%
  dplyr::rename(Assumptions="Biomass_assumptions")%>%
  #No resp assumptions unite("Assumptions", Biomass_assumptions, Respiration_conversion_assumptions, sep=", ", na.rm=TRUE)%>%
  unite("Respiration_OG", Respiration_OG, Respiration_OG_SE, sep="±", na.rm=TRUE)%>%
  unite("Respiration", Respiration, Respiration_SE, sep="±", na.rm=TRUE)

str(table)

#reordering
col_order <- c("Study", "Location", "Season", "Family", "Species", 
               "Reproduction_mode", "Age", "Treatment_Temp", 
               "Biomass_microg.larva_dry", "n_resp", "Respiration_OG", "Respiration_OG_units",
               "Respiration", "normresp_nmolO2.microgramDW.min","Assumptions")
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

write_xlsx(list(data=final_table),"Publication_table_MTE.xlsx") 

################### MODEL FITTING ######################
y <- brood$log_resp
x <- brood$log_biomass
plot(x,y)

#same
fit1  <- lm(log_resp~log_biomass, data=brood)
fit2  <- lm(log_respW~log_biomass, data=wins_brood) #winsorized

fit3  <- lm(log_resp~log_biomass, data=broad)
fit4  <- lm(log_respW~log_biomass, data=wins_broad) #winsorized

AIC(fit1, fit2) #winsorizing did nothing to brood
AIC(fit3, fit4) #winsorizing was better fit for broadcasting

summary(fit1) #brooding, 0 is one standard error of the slope. 
summary(fit3) #broadcasting

#Yes symbionts
#R2: 0.15, p 0.05, slope: 0.14

#No symbionts
#R2: 0.45, p <0.01, slope: 0.96

#brooding
#Slope: -0.05545
#DF: 17
#SE: 0.08389
#Critical probability (1-alpha/2): (1-0.05/2):0.975
#T-score critical value: 2.110 <- my t-value (-0.661) #non significant
#Margin of error: critical value * standard error: 2.110 * 0.08389 = 0.177
#95% confidence interval is -0.05545 +- 0.177 = -0.232 ->  0.12155 
#overlaps with 0


#broadcasting
#Slope: 0.9044 
#DF: 36
#SE: 0.1835
#Critical probability (1-alpha/2): (1-0.05/2):0.975
#T-score critical value: 2.028 <- my t-value (4.928) #significant
#Margin of error: critical value * standard error: 2.028 * 0.1835 = 0.372138
#95% confidence interval is 0.9044  +- 0.372138 = 0.532262->  1.276538
#overlaps with 1

#Critical value calculator: https://stattrek.com/online-calculator/t-distribution.aspx

################# RESP VS BIOMASS #################################
#Does not have homogeneity of slopes using ANOVA. Symbionts and biomass are intertwined
#Data are normal
#Not homogeneity of variances
#No outliers, taken care of by winsorizing
#Since the chi square test says symbionts and reproduction mode are significantly the same, I can just use one as a parameter

#In scaling, not asking what other variables drive it, just how size drives the relationship
#How size drives the covariate.
#ANCOVA: Tests did not meet assumptions, especially lines are not parallel
#LMER: Data heteroscedastic, difference in variance between groups
#You do LMER when trying to explain one line
#ANOVA is a type of LMER?
#testing <- lmer(log_resp ~ log_biomass * Reproduction_mode + (1|Study), data=master_graph)

ggplot(MTE,aes(x=log_biomass, y=log_resp, group = Species))+
  geom_point(aes(shape=Species, color = Study))+
  scale_shape_manual(values=c(1,2,3,4,5,6,7,8))+
  theme(legend.position="right")+
  labs(x="Log Biomass",y="Log Respiration (pmolO2/larva/min)")+
  theme_classic(base_size=12)
  
ggplot(MTE,aes(x=log_biomass, y=log_resp, group = Reproduction_mode))+
  geom_point(aes(shape=Reproduction_mode, color = Age_category), size=2, stroke=1, alpha = 1)+
  scale_shape_manual(values=c(2, 1))+
  scale_color_manual(values=c("#F8A42F", "#FF4605", "#860111"))+
  theme(legend.position="right")+
  labs(x="Log Biomass",y="Log Respiration (nmolO2/larva/min)")+
  theme_classic(base_size=12)+
  geom_smooth(method="lm", color="black", size=0.5)+
  annotate("text", x=2.3, y=2.5, size=5, label= expression(paste(R^2 , " = 0.025, p=0.52")))+ #brooding
  annotate("text", x=2.3, y=2.4, size=5, label= "y=-0.06x+2.15")+                            #brooding
  annotate("text", x=0.45, y=2.2, size=5, label= expression(paste(R^2 , " = 0.40, p<0.001")))+  #broadcasting
  annotate("text", x=0.45, y=2.1, size=5, label= "y=0.90x+0.88")                                #broadcasting
  
#geom_line(aes(y=predancova), color="blue", size=0.5) #<- same thing

#### TESTING FOR ASSUMPTIONS
#winsorized
#raw data


#### LINEARITY
#Needs to have a linear relationship between covariate and outcome
#If there is no linear relation between X and Y, then the analysis of 
#covariance offers no improvement over the one-way analysis of variance 
#in detecting differences between the group means.

ggscatter(wins, x = "log_biomass", y = "log_respW", 
          color = "Reproduction_mode", add = "reg.line")+
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., 
                                           sep = "~~~~"), color = Reproduction_mode))

ggscatter(MTE, x = "log_biomass", y = "log_resp", 
          color = "Reproduction_mode", add = "reg.line")+
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., 
                                           sep = "~~~~"), color = Reproduction_mode))

summary(lm(log_resp~log_biomass,data=brood)) #not significant line
summary(lm(log_resp~log_biomass,data=broad))

#### HOMOGENEITY OF REGRESSION SLOPES
# No significant interaction between the covariate 
#and the grouping variable.

wins %>% anova_test(log_respW ~ Reproduction_mode*log_biomass*Symbionts) 
MTE %>% anova_test(log_resp ~ Reproduction_mode*log_biomass*Symbionts) #there is an interaction

#### NORMALITY OF RESIDUALS
# Fit the model, the covariate goes first
model <- lm(log_respW ~ log_biomass + Reproduction_mode +Symbionts, data = wins)
model.metrics <- augment(model) %>% #add fitted values and residuals by using the function augment(model)
  dplyr::select(-.hat, -.sigma, -.fitted, -.se.fit) # Remove details
head(model.metrics, 3)

shapiro_test(model.metrics$.resid)
shapiro.test(residuals(ancova)) #p-value = 0.7563 #normality
lillie.test(residuals(ancova)) #p-value = 0.5984 #normality

#### HOMOGENEITY OF VARIANCES
#The variance of the residuals is equal for all groups. 
model.metrics %>% levene_test(.resid ~ Reproduction_mode)
leveneTest(wins$log_respW, wins$Reproduction_mode)

#### OUTLIERS
#no cases = no outliers
#standardized residuals greater than 3 in absolute value.
model.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()

#Should not transform already transformed data...
#master_graph$log_resp <- (master_graph$log_resp)^2
#hist(master_graph$log_resp,main="Histogram of observed data")
#plot(density(master_graph$log_resp),main="Density estimate of data")
#plot(ecdf(master_graph$log_resp),main="Empirical cumulative distribution function")
#qqPlot(master_graph$log_resp)


#### RUNNING ANCOVA <- Doesn't meet assumptions
#The orders of variables matters when computing ANCOVA. 
#You want to remove the effect of the covariate first - 
#that is, you want to control for it - prior to entering your main variable or interest.

res.aov <- wins %>% anova_test(log_respW ~ log_biomass + Reproduction_mode + Symbionts)
get_anova_table(res.aov)
#There is not a statistically significant difference in resp between reproduction modes

# Pairwise comparisons
library(emmeans)
pwc <- wins %>% 
  emmeans_test(
    log_resp ~ Reproduction_mode, covariate = log_biomass,
    p.adjust.method = "bonferroni"
  )
pwc

# Display the adjusted means of each group
# Also called as the estimated marginal means (emmeans)
get_emmeans(pwc)

m.interaction <- lm(log_respW ~ log_biomass * Reproduction_mode * Symbionts, data = wins) #But reproduction mode and symbionts are correlated..
anova(m.interaction) #ANOVA output
summary(m.interaction)

# Obtain slopes
m.interaction$coefficients
m.lst <- lstrends(m.interaction, "Reproduction_mode", var="log_biomass")
m.lst

# Compare slopes
pairs(m.lst) #Significantly different

hist(MTE$log_resp,main="Histogram of observed data")
qqPlot(residuals(m.interaction))
#The Shapiro Wilk test was not significant (p > 0.05), 
#so we can assume normality of residuals
shapiro.test(residuals(m.interaction)) #p-value = 0.7563 #normality
lillie.test(residuals(m.interaction)) #p-value = 0.5984 #normality
leveneTest(MTE$log_resp, MTE$Reproduction_mode) #p-value = 4.376e-05, #homogeneity of variances

# POWER TRANSFORM
resp <- (MTE$log_resp)

PT <- powerTransform(abs(resp))
summary(PT)

str(wins)
wins$Symbionts <- as.factor(wins$Symbionts)
test <- aov(Symbionts ~ Reproduction_mode, data = wins) 
summary(test)
#Interaction is significant 
#Can't tease apart reproduction and biomass

ancova <- aov(log_resp ~ Reproduction_mode * log_biomass, data= MTE) #ANCOVA mod w/ interaction
summary(ancova)

#predicted is the line of best fit derived from statistical data. 
#The observed is a line that literally connects all the dots.
predancova<-predict(ancova) #Gets the predicted values from the regression lines in the ANCOVA
MTE<-cbind(MTE, predancova) #attaches those predictions to the dataset

library(MASS)
table <- table(wins$Reproduction_mode,wins$Symbionts)
table
chisq.test(table) #Row and column are statistically signiificantly associated

################ Lipids #############################
setwd("/Users/nina/Projects/Larval-Respiration")
lipids <-read_excel("Meta_analysis.xlsx",sheet="Lipids") #to read your file

str(lipids)

lipids <- lipids%>%
  filter(Type=="coral")

lipids$lipid_dry_weight_perc <- as.numeric(lipids$lipid_dry_weight_perc)

ggplot(lipids, aes(x=stage_num, y=lipid_dry_weight_perc, group=Species))+
  geom_point(aes(color=Species, shape=Reproduction_mode))+
  theme_classic(base_size=12)


############## EGGS DELETE LATER? ######################
setwd("/Users/nina/Projects/Larval-Respiration")
meta <-read_excel("Meta_analysis.xlsx",sheet="Repiration.Photosynthesis") #to read your file

str(meta)

meta["Life_stage"][meta["Life_stage"]=="embryo"] <- "egg"


#When I collapse the averages, I should also add the sample size?

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
  dplyr::select(Study, Source_resp, Source_biomass, Ocean, Location, Family, Species, Symbionts,
                Reproduction_mode, Life_stage, 
                n_resp, Season, Age, n_biomass, 
                Energy_method, Lipid_microg.larva_dry, Protein_microg.larva_dry, Biomass_microg.larva_wet,
                Biomass_microg.larva_dry, Biomass_SE_dry, Biomass_assumptions, 
                OA_type, Temp_type, Treatment_Temp,
                Respiration_OG, Respiration_OG_SE, Respiration_OG_units, Respiration_conversion_assumptions,
                Respiration, Respiration_SE, Notes)%>% #Area was used to possibly include for biomass estimates
  dplyr::filter(OA_type=="ambient")%>%
  dplyr::filter(Temp_type=="ambient")%>% #Not lookint at OA
  dplyr::select(-OA_type,-Temp_type)
#Just larvae under ambient seawater chemistry and temperatures

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

#meta$Treatment_Temp <- round(as.numeric(meta$Treatment_Temp)) #Rounding temperature

meta1 <- meta%>% 
  mutate("Respiration_SD"= (Respiration_SE * sqrt(n_resp)))%>%
  mutate("Respiration_var"= (Respiration_SD)^2)%>%
  mutate("Respiration_SS"= (Respiration_var)*(n_resp-1))%>% #For sample variance, SS, stdev
  mutate("Inverse_var"=(1/Respiration_var))

str(meta1)

#Taking out studies with respiration not given

age1 <- meta1%>%
  #In another group, they were 1-2 days old. I don't have a 1 day old data point alone     
  filter(Respiration!="NA")%>% #Taking out studies with missing respiration
  filter(!Respiration<0) #Taking out negative respiration rates beause graph couldn't resolve them. Graham et al 2013: Age 15 & 24

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
ageavg <- age1

ageavg["Source_resp"][ageavg["Study"]=="Cumbo et al 2012"&ageavg["Source_resp"]=="Fig. 1"] <- "Fig. 1 & 3"
ageavg["Source_resp"][ageavg["Study"]=="Cumbo et al 2012"&ageavg["Source_resp"]=="Fig. 3"] <- "Fig. 1 & 3"
ageavg["Source_biomass"][ageavg["Study"]=="Cumbo et al 2012"&ageavg["Source_biomass"]=="Fig. 1"] <- "Fig. 1 & 3"
ageavg["Source_biomass"][ageavg["Study"]=="Cumbo et al 2012"&ageavg["Source_biomass"]=="Fig. 3"] <- "Fig. 1 & 3"

ageavg$n_resp <- as.character(ageavg$n_resp)

ageavg["n_resp"][ageavg["Study"]=="Cumbo et al 2013"&ageavg["n_resp"]=="2"] <- "2-4"
ageavg["n_resp"][ageavg["Study"]=="Cumbo et al 2013"&ageavg["n_resp"]=="4"] <- "2-4"

#This assumption is true, but won't collapse anyways..
#Cumbo 2012, Average from study isn't used anywayas because it is taken out later down below
ageavg["Biomass_assumptions"][ageavg["Study"]=="Cumbo et al 2012"&ageavg["Biomass_assumptions"]=="Average from study"] <- "(Richmond 1987) P. damicornis: 17% protein of total dry weight"
ageavg["Biomass_assumptions"][ageavg["Study"]=="Cumbo et al 2012"&ageavg["Biomass_assumptions"]=="Repeated last value taken"] <- "(Richmond 1987) P. damicornis: 17% protein of total dry weight"
ageavg["Biomass_assumptions"][ageavg["Study"]=="Harii et al 2010"&ageavg["Biomass_assumptions"]=="Average from study"] <- "(Harii et al. 2007) & (Arai et al 1993): four broadcast species: 64% lipids of total dry weight"

#Need to definitely group by study, age, and species
#didn't group by n_resp, so Cumbo et al 2013 had two different n_resp that got merged
str(ageavg)

ageavg1 <- ageavg%>%
  group_by(Study, Species, Age, Reproduction_mode,Symbionts, Life_stage)%>%
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

levels(ageavg1$Study) #This shows that the studies aren't really taken out
ageavg1 <- droplevels(ageavg1) #Dropping levels for real

justbiomass <- ageavg1%>%
  ungroup(Study, Species, Age, Reproduction_mode,Symbionts, Life_stage)%>%
  mutate(normresp_nmolO2.microgramDW.min=(Respiration/Biomass_microg.larva_dry))%>%
  mutate(normresp_SE=(Respiration_SE/Biomass_microg.larva_dry))%>%
  mutate(normresp_Ivar=(Inverse_var/Biomass_microg.larva_dry))

#Studies without biomass
#	"Haryanti and Hidaka 2015", "Edmunds 2013", "Edmunds et al 2011",
#"Rodriguez-Lanetty et al 2009", "Olsen et al 2013", 
#"Ross et al 2013", "Okubo et al 2008"
# "Nakamura et al 2011",  <= #Nakamura study: orders of magnitude smaller resp rates. Probably wrong
#"Cumbo et al 2013", 
#"Ross et al 2010", 
#"Serrano et al 2018", "Titlyanov et al 1998"
#"Albright and Langdon 2011", 
# "Putnam and Gates 2015", "Putnam et al 2013", 
#"Jiang et al 2020", "Kitchen et al 2020", "Zhou et al 2016"


# Studies with biomass
# "Rivest and Hofmann 2014", "Cumbo et al 2012", "Edmunds et al 2001",
# "Harii et al 2010", "Graham et al 2013", "Gaither and Rowan 2010", 
#"Cumbo et al 2013b", "Richmond 1987"


#To add? (for now I'm not)
#Edmunds 2013, multiple respiration, single biomass for all
#"Edmunds et al 2011", multiple respiration for each species, single biomass for each of species

#Richmond 1987: two respiration rows and one biomass measure because type Y and type B


MTE <- justbiomass%>%
  filter(!(Study == "Edmunds et al 2001" & Protein_microg.larva_dry == "NA"))%>% #How to filter out the row with these conditions
  filter(Study %in% c("Rivest and Hofmann 2014",
                      "Cumbo et al 2012",
                      "Edmunds et al 2001",
                      "Harii et al 2010",
                      "Graham et al 2013",
                      "Gaither and Rowan 2010",
                      "Cumbo et al 2013b",
                      "Richmond 1987"))%>%
  group_by(Study, Species, Reproduction_mode, Age,Symbionts, Life_stage)%>%
  dplyr::summarize(normresp_nmolO2.microgramDW.min=mean(normresp_nmolO2.microgramDW.min), 
                   Biomass_microg.larva_dry=mean(Biomass_microg.larva_dry),
                   Respiration=mean(Respiration))%>%
  mutate(log_normresp = log10(normresp_nmolO2.microgramDW.min),
         log_biomass = log10(Biomass_microg.larva_dry),
         log_resp = log10(Respiration))%>%
  filter(log_resp!="-Inf")%>% #IDK what this is...
  mutate(Age_category=cut(Age, breaks=c(0, 10, 20, 64), labels=c("low","middle","high")))%>%
  ungroup(Reproduction_mode)

brood <- MTE%>%
  filter(Reproduction_mode=="brooding")%>%
  mutate(log_respW = Winsorize(log_resp, na.rm=TRUE))

broad <- MTE%>%
  filter(Reproduction_mode=="broadcasting")%>%
  mutate(log_respW = Winsorize(log_resp, na.rm=TRUE))

#Combined data with winsorized data of brooding and broadcasting SEPERATELY
#I don’t think I can winterize separately because then I’m analyzing together.. 
#and separating based on brooding and broadcasting is my own thing
sep_Winsorized <- rbind(brood,broad)

wins <- MTE%>%
  mutate(log_respW = Winsorize(log_resp, na.rm=TRUE))



yes_symb <- MTE%>% #(All brooding + M. digitata)
  filter(Symbionts=="yes")

no_symb <- MTE%>% #(all broadcasting except for M digitata)
  filter(Symbionts=="no")

y <- broad$log_respW
x <- broad$log_biomass

fit  <- lm(y~x)
summary(fit)

ggplot(wins,aes(x=log_biomass, y=log_respW, group = Species))+
  geom_point(aes(shape=Species, color = Study))+
  scale_shape_manual(values=c(1,2,3,4,5,6,7,8))+
  theme(legend.position="right")+
  labs(x="Log Biomass",y="Log Respiration (nmolO2/larva/min)")+
  theme_classic(base_size=12)


#Winsorized data!
ggplot(wins,aes(x=log_biomass, y=log_respW, group = Life_stage))+
  geom_point(aes(shape=Life_stage, color = Reproduction_mode), size=2, stroke=1, alpha = 1)+
  theme(legend.position="right")+
  labs(x="Log Biomass",y="Log Respiration (nmolO2/larva/min)")+
  theme_classic(base_size=12)+
  geom_smooth(method="lm", color="black", size=0.5)+
  annotate("text", x=2.3, y=2.5, size=5, label= expression(paste(R^2 , " = 0.24, p=0.55")))+
  annotate("text", x=2.3, y=2.4, size=5, label= "y=-0.05x+2.14")+
  annotate("text", x=0.45, y=2.2, size=5, label= expression(paste(R^2 , " = 0.46, p<0.001")))+
  annotate("text", x=0.45, y=2.1, size=5, label= "y=0.96x+0.83")


