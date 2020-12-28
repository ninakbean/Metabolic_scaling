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

larv <- larvae %>%
  select(ID, pikomol.larva.min,Dry_weight.mg, c_divide_SE.mg, -Dry_weight.mg.SE)%>%
  mutate(Species=c("Pocillopora"))%>% #column called species which is filled with poc
  mutate(Stage=c("larvae"))%>%
  unite(Type, Species, Stage, sep = "_", remove = FALSE, na.rm = FALSE)%>%
  mutate(micromol.ind.min=(pikomol.larva.min/1000000))%>%
  select(-pikomol.larva.min) #need to have same colum names as juveniles

juv <- juve %>%
  rename(micromol.ind.min = micromol.coral.min)%>%
  mutate(Dry_weight.mg=(Dry_weight.g*1000))%>%
  mutate(Stage=c("juvenile"))%>%
  mutate(c_divide_SE.mg=c(0))%>%
  unite(Type, Species, Stage, sep = "_", remove = FALSE, na.rm = FALSE)%>%
  select(Dry_weight.mg, c_divide_SE.mg, micromol.ind.min, Type, Species, Stage, ID)

combined <- rbind(larv, juv)

ggplot(combined,aes(x=log10(Dry_weight.mg), y=log10(micromol.ind.min*60), group = Type))+
  geom_point(aes(color = Species), size=2, stroke=1, alpha = 1)+
  geom_errorbar(aes(xmin=(log10(Dry_weight.mg))-c_divide_SE.mg, xmax=(log10(Dry_weight.mg))+c_divide_SE.mg), position = "identity", stat = "identity")+
  scale_color_manual(values=c("#F8A42F", "#FF4605"))+ 
  theme(legend.position="right")+
  labs(x="Log Dry Tissue (mg)",y=(expression(paste("Log Respiration (", mu , "mol", " ", O[2], " ", coral^-1, " ", h^-1,")"))))+
  theme_classic(base_size=12)+
  geom_smooth(method="lm", color="black", size=0.5)

#Can't find any porites larvae in literature (based on a quick search and 
#my data comp)
past <- data_comp%>%
  filter(Species=="Porites astreoides")

################### RUNNING ANCOVA WITH SA AS COVARIATE##################
#Best guide:
#https://www.datanovia.com/en/lessons/ancova-in-r/#:~:text=Homogeneity%20of%20regression%20slopes.,by%20groups%20should%20be%20parallel.

#When Assumptions of ANCOVA are Irrelevant:
#Case where the independent variable and the covariate are 
#independent of each other...
#https://www.theanalysisfactor.com/assumptions-of-ancova/

#ANCOVA Assumptions: When Slopes are Unequal
#Addressing assumption: There is no interaction between independent variable and the covariate.
#https://www.theanalysisfactor.com/ancova-assumptions-when-slopes-are-unequal/
#ANCOVA can only be used if the lines are parallel. <- This is true IF
#the purpose of ANCOVA is to reduce error variation and allow us to report a single, 
#overall effect of the independent variable on the dependent variable, at every value of the covariate.
#If you violate this assumption, all it means is that you can't
#1) Run the unique model without an interaction that many people mean when they say “ANCOVA.”
#2) describe the effect of the grouping on the DV without also including some information about age.
#What model to do? Read in article!
#Run the full model with the interaction, describe the results in detail, and don’t call it ANCOVA to appease reviewers.
#But including the interaction term is not wrong because it doesn’t meet an assumption.  
#The assumption is more about defining which models we can call ANCOVA than it is about which models best fit the data.

#DATA SETS: combined, larv, juv

#ANCOVA: 
#dependent variable (outcome)
#grouping variable
#Independent variable (covariate)

ANCOVA_data <- combined%>%
  mutate(x = log10(Dry_weight.mg))%>%
  mutate(y = log10(micromol.ind.min*60))

ANCOVA_data.no.outliers <- ANCOVA_data%>%
  filter(!(ID=="11" & Type == "Porites_juvenile"))
  #filter(!(ID=="20" & Type == "Porites_juvenile"))
  #filter(!(ID=="23" & Type == "Pocillopora_juvenile"))
  
#0.4639831, 0.7403627, Porites_juvenile #ID: 11
#-0.3381778, 0.3222193, Porites_juvenile #ID: 20
#0.2345787, 0.7634280, Pocillopora_juvenile #ID: 23

#ASSUMPTION 1
#Linearity between the covariate and the outcome variable at each level of the grouping variable. 
#This can be checked by creating a grouped scatter plot of the covariate and the outcome variable.

ggscatter(
  data=ANCOVA_data, x = "x", y = "y",
  color = "Type", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Type)
  )

ggscatter(
  data=ANCOVA_data.no.outliers, x = "x", y = "y",
  color = "Type", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Type)
  )

#taking out outliers makes the R2 go down for porites and larvae

#independence between the independent variable (aka grouping variable) and the covariate
#is violated because larvae are on the left of the graph
#the main categorical independent variable is observed and not manipulated, 
#the independence assumption between the covariate and the independent variable is irrelevant.
#It’s a design assumption. It’s not a model assumption.
#The only effect of the assumption of the independent variable and the covariate being independent is in how you interpret the results.

#The appropriate response is #2–keep the covariate in the analysis, but explain it well
#It doesn’t mean I can’t run the model at all, it just means I need to include 
#covariate in my model and explain the model with that in it. 
#The categorical variable effect on x was observed not manipulated in my case
#and don’t interpret results from an observational study as if they were manipulated

#ASSUMPTION 2
#Homogeneity of slopes, but also running the ancova
#The slopes of the regression lines should be the same for each group. 
#This assumption checks that there is no significant interaction between the covariate and the grouping variable.
#The plotted regression lines by groups should be parallel.

#RUNNING ANCOVA (calling it a general linear model because 
#assumption of covariate and grouping not interactions not met)

#For ANCOVA, need to look at type I because the first variable is continuous??
#Use type 1 because there is an interaction between categorical variable and predictor variable
#Type I: 
#Balanced data, sequential (first variable considered, then the next for error)
#Type 2: Unbalanced data, principle of marginality, does not consider interactions. 
#Don't use if interactions are present. If no interaction, more powerful than type 3. 
#Type 3: Unbalanced data, use when sig. interactions, you have to make orthogonal contrasts?

# Type I ANOVA - aov()
#aov defaults to type I
#aov(time.lm)

# Type II ANOVA - Anova(type = 2)
#car::Anova(time.lm, type = 2)

# Type III ANOVA - Anova(type = 3)
#car::Anova(time.lm, type = 3)

#The * indicates an interaction
#If no interaction, put + to take it out
#covariate first

model <- aov(y ~ x*Type, data=ANCOVA_data)
summary(model)

model_no.outliers <- aov(y ~ x*Type, data=ANCOVA_data.no.outliers)
summary(model_no.outliers)

AIC(model, model_no.outliers)
#no outlier is a better model

#after adjusting for covariate, if p<0.05, there is a significant difference in 
#dependent variable between groups
#Interaction term is significant, slopes are not parallel.
#slopes are statistically different from each other
#this is the answer, saying that they are different. Keep interaction in model 

#Pairwise comparisons
#which groups are different
#Emmeans stands for estimated marginal means (aka least square means or adjusted means).
library(emmeans)
pwc <- ANCOVA_data%>% 
  emmeans_test(
    y ~ Type, covariate = x,
    p.adjust.method = "bonferroni"
  )
pwc

pwc.no.outliers <- ANCOVA_data.no.outliers%>% 
  emmeans_test(
    y ~ Type, covariate = x,
    p.adjust.method = "bonferroni"
  )
pwc.no.outliers

#each group comparison of slope is statistically different from each other
# Display the adjusted means of each group 
# Also called as the estimated marginal means (emmeans)
get_emmeans(pwc)
get_emmeans(pwc.no.outliers)
#elevation significantly different from each other

#ASSUMPTION 3
#Normality of residuals
#The outcome variable should be approximately normally distributed. 
#This can be checked using the Shapiro-Wilk test of normality on the model residuals.

# Fit the model, the covariate goes first
#The orders of variables matters when computing ANCOVA. 
#You want to remove the effect of the covariate first - 
#that is, you want to control for it - prior to entering your main variable or interest.

model <- lm(y ~ x + Type, data = ANCOVA_data)
# Inspect the model diagnostic metrics
model.metrics <- augment(model)
# Assess normality of residuals using shapiro wilk test
shapiro_test(model.metrics$.resid)
#>0.05, normal data 

model.no.outliers <- lm(y ~ x + Type, data = ANCOVA_data.no.outliers)
model.metrics.no.outliers <- augment(model.no.outliers)
# Assess normality of residuals using shapiro wilk test
shapiro_test(model.metrics.no.outliers$.resid)

#ASSUMPTION 4
#Homogeneity of variances (aka homoscedasticity)
#ANCOVA assumes that the variance of the residuals is equal for all groups. 
#This can be checked using the Levene’s test
model.metrics %>% levene_test(.resid ~ Type)
model.metrics.no.outliers %>% levene_test(.resid ~ Type)

#The Levene’s test was not significant (p > 0.05)
#assume homogeneity of the residual variances for all groups.

#outliers
#About cooks distance: commonly used estimate of the influence of a data point 
#when performing a least-squares regression analysis.
#defined as the sum of all the changes in the regression model when observation i is removed from it. 

#Determined within the model as a whole, not with each line
#Outliers can be identified by examining the standardized residual (or studentized residual), 
#which is the residual divided by its estimated standard error. 
#Standardized residuals can be interpreted as the number of standard errors 
#away from the regression line.

Outlier <- model.metrics %>% 
  filter(abs(.std.resid) > 3) %>% #tells you which is an outlier, but if want to see all data, don't filter
  as.data.frame()

cooks <- cooks.distance(model)
cooks
plot(cooks)



#Reason & which were outliers:
#y,x outliers

#Observations whose standardized residuals are greater than 3 in absolute value are possible outliers.
#0.4639831, 0.7403627, Porites_juvenile #ID: 11

#Visually
#0.4639831, 0.7403627, Porites_juvenile #ID: 11
#-0.3381778, 0.3222193, Porites_juvenile #ID: 20

#4/N( N= # of observations, K= # of explanatory variables)
#4/38 = 0.10
#0.4639831, 0.7403627, Porites_juvenile #ID: 11
#-0.3381778, 0.3222193, Porites_juvenile #ID: 20
#0.2345787, 0.7634280, Pocillopora_juvenile #ID: 23
#I additionally took out the highest cooks distance outlier, then took out 2, then 3
#AIC model was lowest only when 1 outlier was removed (as opposed to 2 or 3)


