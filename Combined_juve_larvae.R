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

#CREATING DATA SETS and checking for outliers

larv <- larvae %>%
  select(ID, pikomol.larva.min,Dry_weight.mg, c_divide_SE.mg, -Dry_weight.mg.SE)%>%
  mutate(Species=c("Pocillopora"))%>% #column called species which is filled with poc
  mutate(Stage=c("larvae"))%>%
  unite(Type, Species, Stage, sep = "_", remove = FALSE, na.rm = FALSE)%>%
  mutate(micromol.ind.min=(pikomol.larva.min/1000000))%>%
  select(-pikomol.larva.min)%>% #need to have same colum names as juveniles
  mutate(x = log10(Dry_weight.mg))%>%
  mutate(y = log10(micromol.ind.min*60))

larvae.model <- lm(y ~ x, data=larv)
Anova(larvae.model, type=3)
summary(larvae.model, type=II)

out <- as.data.frame(augment(larvae.model))
cooks <- cooks.distance(larvae.model)
plot(cooks)
#4/n(17) = 0.23
#no outliers
#t=(estimated value-hypothesized value)/standard error of the estimator
t=(0.4885-(3/4))/0.1623
t
2*pt(abs(t),df=16, lower=FALSE) #gives you p value
#not significantly different from 2/3 or 3/4

juv <- juve %>%
  rename(micromol.ind.min = micromol.coral.min)%>%
  mutate(Dry_weight.mg=(Dry_weight.g*1000))%>%
  mutate(Stage=c("juvenile"))%>%
  mutate(c_divide_SE.mg=c(0))%>%
  unite(Type, Species, Stage, sep = "_", remove = FALSE, na.rm = FALSE)%>%
  select(Dry_weight.mg, c_divide_SE.mg, micromol.ind.min, Type, Species, Stage, ID)%>%
  mutate(x = log10(Dry_weight.mg))%>%
  mutate(y = log10(micromol.ind.min*60))

port <- juv %>%
  filter(Species=="Porites")

port.no.outlier <- port%>%
  filter(!(ID=="20" & Type == "Porites_juvenile"))

port.model <- lm(y ~ x, data=port)
port.model.no.outlier <- lm(y ~ x, data=port.no.outlier)

AIC(port.model, port.model.no.outlier)
Anova(port.model, port.model.no.outlier, type=3)

summary(port.model, type=3)
summary(port.model.no.outlier, type=3)

out <- as.data.frame(augment(port.model))
cooks <- cooks.distance(port.model)
plot(cooks)

#4/n(10) = 0.4
#-0.3381778, 0.3222193, Porites_juvenile #ID: 20
#didn't make 0.4 cut off, but visually...
#0.4639831, 0.7403627, Porites_juvenile #ID: 11

#full model
#t=(estimated value-hypothesized value)/standard error of the estimator
t=(0.65493-(1))/0.09477
t
2*pt(abs(t),df=9, lower=FALSE) #gives you p value
#not sig similar to any slope

#outlier one
#t=(estimated value-hypothesized value)/standard error of the estimator
t=(0.35245-(2/3))/0.05938
t
2*pt(abs(t),df=8, lower=FALSE) #gives you p value
#not sig similar to any slope

poc <- juv %>%
  filter(Species=="Pocillopora")

poc.model <- lm(y ~ x, data=poc)
summary(poc.model, type=3)
out <- as.data.frame(augment(poc.model))
cooks <- cooks.distance(poc.model)
plot(cooks)
#4/n(11) = 0.36
#no outliers

#t=(estimated value-hypothesized value)/standard error of the estimator
t=(0.99492-(1))/0.09497
t
2*pt(abs(t),df=10, lower=FALSE) #gives you p value
#similar to 1

combined <- rbind(larv, juv)

out <- as.data.frame(augment(combined.model))
cooks <- cooks.distance(combined.model)
plot(cooks)
#4/n(38) = 0.1
#-0.3381778, 0.3222193, Porites_juvenile #ID: 20
#0.4639831, 0.7403627, Porites_juvenile #ID: 11

combined.11.ol <- combined%>% #BEST MODEL
  filter(!(ID=="20" & Type == "Porites_juvenile"))

combined.12.ol <- combined%>%
  filter(!(ID=="11" & Type == "Porites_juvenile"))

combined.2.ol <- combined%>%
  filter(!(ID=="20" & Type == "Porites_juvenile"))%>%
  filter(!(ID=="11" & Type == "Porites_juvenile"))

model <- lm(y ~ Type*x, data=combined)
model.11.ol <- lm(y ~ Type*x, data=combined.11.ol)
model.12.ol <- lm(y ~ Type*x, data=combined.12.ol)
model.2.ol <- lm(y ~ Type*x, data=combined.2.ol)

Anova(model,model.11.ol, type =3) #tests significantly different
Anova(model.11.ol, model.12.ol, type =3) #tests significantly different
Anova(model, model.12.ol, type=3) #tests significantly different
AIC(model,model.11.ol, model.12.ol, model.2.ol) #removing one is the best, highest cooks distance one

#keep interaction term?
model1 <- lm(y ~ Type+x, data=combined) #saying lines are parallel
model2 <- lm(y ~ + x, data=combined) #saying type has no effect

Anova(model, model1, type=3) #keep interaction term, because sig difference
Anova(model, model2, type=3) #groups are significant
AIC(model, model1, model2) #reinforces full model is needed

Anova(model, type =3)
Anova(model.11.ol, type=3)
#x has effect on y, there is a difference in slope of this relationship
#by type, as indicated by interaction term. 
step(model) #tries removing the most complicated term and gives you AIC
#If take out interaction term, AIC gets worse

# Step 1: Call the pdf command to start the plot
pdf(file = "Figs/Empirical/Combined.pdf",   # The directory you want to save the file in
    width = 7, # The width of the plot in inches
    height = 6) # The height of the plot in inches

# Step 2: Create the plot with R code
ggplot(combined,aes(x=log10(Dry_weight.mg), y=log10(micromol.ind.min*60), group = Type))+
  geom_point(aes(color = Species, shape=Stage), size=2, stroke=1, alpha = 1)+
  geom_errorbar(aes(xmin=(log10(Dry_weight.mg))-c_divide_SE.mg, xmax=(log10(Dry_weight.mg))+c_divide_SE.mg), position = "identity", stat = "identity", size=0.4)+
  scale_color_manual(values=c("#F8A42F", "#FF4605"),name="Genera", 
                     labels = c((expression(paste(italic("Pocillopora spp.")))),
                                expression(paste(italic("Porites spp.")))))+ 
  labs(x="Log Dry Tissue (mg)",y=(expression(paste("Log Respiration (", mu , "mol", " ", O[2], " ", coral^-1, " ", h^-1,")"))))+
  theme_classic(base_size=12)+
  geom_smooth(method="lm", color="black", size=0.5)+
  theme(legend.text.align = 0,
        legend.position = c(0.8, 0.2))+
  geom_abline(intercept=-1.5, slope=1, colour = "black", size = 0.5, lty=5)+
  geom_abline(intercept=0.4, slope=(2/3), colour = "black", size = 0.5, lty=5)+
  annotate("text", x=-1.5, y=0, size=5, colour="black", label= "b=2/3")+
  annotate("text", x=-0.5, y=-2.5, size=5, colour="black", label= "b=1")

# Step 3: Run dev.off() to create the file!
#“null device”<- saying now create plots in the main R plotting window again.
dev.off()

#Can't find any porites larvae in literature (based on a quick search and 
#my data comp)
past <- data_comp%>%
  filter(Species=="Porites astreoides")

################### RUNNING ANCOVA WITH SA AS COVARIATE##################
#ASSUMPTION 1
#Linearity between the covariate and the outcome variable at each level of the grouping variable. 
#This can be checked by creating a grouped scatter plot of the covariate and the outcome variable.

ggscatter(
  data=combined, x = "x", y = "y",
  color = "Type", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Type)
  )

ggscatter(
  data=combined.11.ol, x = "x", y = "y",
  color = "Type", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Type)
  )

#ASSUMPTION 2
#Homogeneity of slopes
#The slopes of the regression lines should be the same for each group. 
#This assumption checks that there is no significant interaction between the covariate and the grouping variable.
#The plotted regression lines by groups should be parallel.

#Pairwise comparisons
#which groups are different
#Emmeans stands for estimated marginal means (aka least square means or adjusted means).
#Estimated Marginal Means adjust for the covariate by reporting the means of Y 
#for each level of the factor at the mean value of the covariate.

library(emmeans)
pwc <- combined.11.ol%>% 
  emmeans_test(
    y ~ Type, covariate = x,
    p.adjust.method = "bonferroni"
  )
pwc

# each group comparison of slope is statistically different from each other
# Display the adjusted means of each group 
# Also called as the estimated marginal means (emmeans)
get_emmeans(pwc)
#elevation significantly different from each other

#ASSUMPTION 3
#Normality of residuals
#The outcome variable should be approximately normally distributed. 
#This can be checked using the Shapiro-Wilk test of normality on the model residuals.

# Inspect the model diagnostic metrics
model.metrics <- augment(model)
# Assess normality of residuals using shapiro wilk test
shapiro_test(model.metrics$.resid)
#>0.05, normal data 

#ASSUMPTION 4
#Homogeneity of variances (aka homoscedasticity)
#ANCOVA assumes that the variance of the residuals is equal for all groups. 
#This can be checked using the Levene’s test
model.metrics %>% levene_test(.resid ~ Type)
#The Levene’s test was not significant (p > 0.05)
#assume homogeneity of the residual variances for all groups.










#For me
#s = sums, s2p = squares and products
#pocl=poc larvae, pocj = poc juv, portj = port juv
#o=overall
x.o.s <- sum(x)
x.o.s2p <- sum(x^2)
y.o.s <- sum(y)
y.o.s2p <- sum(y^2)
xy.o.s2p <- sum(x*y)
x.pocl.s <- sum(x[Type=="Pocillopora_larvae"])
x.pocl.s2p <- sum(x[Type=="Pocillopora_larvae"]^2)
x.pocj.s <- sum(x[Type=="Pocillopora_juvenile"])
x.poc.j.s2p <- sum(x[Type=="Pocillopora_juvenile"]^2)
x.portj.s <- sum(x[Type=="Porites_juvenile"])
x.portj.s2p <- sum(x[Type=="Porites_juvenile"]^2)
y.pocl.s <- sum(y[Type=="Pocillopora_larvae"])
y.pocl.s2p <- sum(y[Type=="Pocillopora_larvae"]^2)
y.pocj.s <- sum(y[Type=="Pocillopora_juvenile"])
y.pocj.s2p <- sum(y[Type=="Pocillopora_juvenile"]^2)
y.portj.s <- sum(y[Type=="Porites_juvenile"])
y.portj.s2p <- sum(y[Type=="Porites_juvenile"]^2)
#sums of products
xy.pocl.s2p <- sum(x[Type=="Pocillopora_larvae"]*y[Type=="Pocillopora_larvae"])
xy.pocj.s2p <- sum(x[Type=="Pocillopora_juvenile"]*y[Type=="Pocillopora_juvenile"])
xy.portj.s2p <- sum(x[Type=="Porites_juvenile"]*y[Type=="Porites_juvenile"])



