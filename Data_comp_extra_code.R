################ CHECKING FOR NORMALITY #########################################
package(patchwork)
p1+p2+p3,add labels


### METAFOR
### note: the interpretation of the results is difficult since all
### situational interviews were structured, almost all psychological
### interviews were unstructured, and actually for the majority of
### the psychological interviews it was unknown whether the interview
### was structured or unstructured
table(classic$Species, classic$Lab, useNA="always")


################# TRANSFORM EFFECT SIZE #################################
resp <- (classic$R)

hist(resp,main="Histogram of observed data")
plot(density(resp),main="Density estimate of data")
plot(ecdf(resp),main="Empirical cumulative distribution function")
qqPlot(resp)

PT <- powerTransform(abs(resp))
summary(PT)
#Est power 1.32

#negatively skewed data usually transformed through log, square or cube root
#Square root is just R^2 and takes away from the negative values
classic$transresp <- log10(classic$R+1) 
classic$transsresp <- classic$transresp-1 
hist(transresp,main="Histogram of observed data")
plot(density(transresp),main="Density estimate of data")
plot(ecdf(transresp),main="Empirical cumulative distribution function")
qqPlot(transresp)





cubevar <- (ageavg$Respiration_var)^(1/3)

mod2 <- lm(cuberesp~age)
qqPlot(residuals(mod2))

shapiro.test(residuals(mod2)) 
#p-value = 0.01331
nortest::lillie.test(residuals(mod2))
#p-value = 0.3282

ageavg$Respiration_log <- log(ageavg$Respiration+1)
ageavg$wins <- as.numeric(ageavg$wins)
ageavg$wins_log <- log(ageavg$wins)


mod1 <- lm(Respiration~Age,data = ageavg)
mod2 <- lm(Respiration_log~Age,data = ageavg)

qqplot(residuals(ageavg$Respiration))
qqPlot(residuals(mod2))


plot(mod1)

gamma_test(ageavg$Respiration)





################ GRAPH EXPLORATION #########################################
#### BROODING
plot(brooding$Age,brooding$Respiration,col=brooding$Study)

ggplot(brooding,aes(x=Age,y=Respiration, col=Study))+
  geom_point()+
  labs(x="Days post release",y="Respiration")+
  theme_classic(base_size=12)+
  geom_smooth(method="lm",se=FALSE)

#### BROADCASTING
plot(broadcasting$Age,broadcasting$Respiration, col=broadcasting$Study)

ggplot(broadcasting,aes(x=Age,y=Respiration, col=Study))+
  geom_point()+
  labs(x="Days post release",y="Respiration")+
  theme_classic(base_size=12)+
  geom_smooth(method="auto",se=FALSE)





################ Z SCORE = WRONG #########################################
setwd("/Users/nina/Research/Research Ideas/AA Larval Energy Budget")
Zscore <-read_excel("data_compilation.xlsx",sheet="Data") #to read your file
str(Zscore)

#IF you change the study mean to weigh it, then that is like changing the original number
#Outliars are all from Nakamura, those rates are correct, not sure why they are so different..
#SHOULD NOT BE SAMPLE SD

Zscore <- Zscore%>%
  mutate("Zscore"=((Respiration-Mean)/Respiration_SD))%>% #supposed to be population standard deviation
  mutate("Zscoreabs" = abs(Zscore))%>%
  mutate("weight" = (1/Respiration_var))%>%
  filter(!Study %in% c("Nakamura et al 2011","Cumbo et al 2013b")) #Took out outliars that had lots of influence...

plot(Zscore$Age,Zscore$Zscore)
plot(Zscore$Age,Zscore$Zscoreabs)
#mutate("weighted_mean" = (weight*Zscore)/weight) #Is this even a thing? Is this correct? Same as Zscore


############# TRYING THE OTHER MODELING THING ######################
effectsavg <- ageavg%>%
  group_by(Age)%>%
  summarise_all(mean, na.rm=TRUE)%>%
  select(Age,Respiration, Respiration_SE, Respiration_SD, Respiration_var, Respiration_SS)

#If row only had one entry, it didn't mean one value, it just kept the same value
#write_xlsx(list(Sheet1=effectsavg),"Data compilation effectsavg.xlsx")
#write_xlsx(list(Sheet1=ageavg),"Data compilation avgavg.xlsx")

setwd("/Users/nina/Research/Research Ideas/AA Larval Energy Budget")
effects <-read_excel("Data compilation avgavg.xlsx",sheet="Sheet1") #to read your file
str(effects)


Q <- effects %>%
  mutate("Q"= (1/Respiration_var)*((Respiration_SS-avgRespiration)^2))%>% #Q=total var
  group_by(Age)%>%
  dplyr::summarize(Q=sum(Q))

write_xlsx(list(Sheet1=Q),"Data compilation Q.xlsx")

setwd("/Users/nina/Research/Research Ideas/AA Larval Energy Budget")
effects <-read_excel("Data compilation avgavg.xlsx",sheet="Sheet1") #to read your file
str(effects)

Wi <- effects %>%
  mutate("Wi" = 1/effects$Respiration_var)%>%
  mutate("Wi2" = ((1/effects$Respiration_var)^2))%>%
  group_by(Age)%>%
  dplyr::summarize(Wi=sum(Wi),
                   Wi2=sum(Wi2))

write_xlsx(list(Sheet1=Wi),"Data compilation Wi.xlsx")

setwd("/Users/nina/Research/Research Ideas/AA Larval Energy Budget")
effects <-read_excel("Data compilation avgavg.xlsx",sheet="Sheet1") #to read your file
str(effects)
#Check the  mean again if this works

effects <- effects%>%
  mutate("Zscore"=((Respiration-Mean)/Respiration_SD))%>%
  mutate("C" = Wi-(Wi2/Wi))%>% #C= scaling factor
  mutate("tau2"= (Q-df)/C)%>% #Between study variance
  mutate("weight" = (1/(Respiration_var+tau2)))%>%
  mutate("effect&weight" = (Zscore*weight))







#Effect size? mutate("Ti"=(Zscore+avgRespiration_SE+Respiration_SE)) 



# Is Ti"=(avgRespiration+avgRespiration_SE+Respiration_SE)) or SS of each study? is it SE? or var?
#mutate("weight"=1/(Respiration_var+avgRespiration_var))%>%
#summarise(avg=mean(Respiration[!is.na(Respiration)],na.rm=TRUE), #(Percentage[!is.na(Percentage)],na.rm=TRUE) <- this is to take the mean even with blank cells in the data
#         stdev=sd(Respiration[!is.na(Respiration)],na.rm=TRUE),
#        se=(sd(Respiration[!is.na(Respiration)],na.rm=TRUE))/sqrt(NROW(na.omit(Respiration))))


################ META ANALYSIS PART #############################





## PRACTICE


# reference group BMD mean and standard deviation
bmd.mu <- 955
bmd.sd <- 123
# construct data for two groups with different BMD
set.seed(2803)
d1 <- data.frame(group=rep(0,100), bmd=rnorm(100,855,50))
d2 <- data.frame(group=rep(1,100), bmd=rnorm(100,1055,50))
df <- rbind(d1, d2)
# calculate zscores
df$zscore <- (df$bmd - bmd.mu) / bmd.sd
# calculate effect size based on unstandardized or standardized BMD
cohens.d.bmd <- (mean(df$bmd[df$group==0]) - mean(df$bmd[df$group==1])) / sd(df$bmd)
cohens.d.zscore <- (mean(df$zscore[df$group==0]) - mean(df$zscore[df$group==1])) / sd(df$zscore)




## Good code

#Linear, same as exponential??
line <- lm(Tmean ~ Tage)
summary(line)
plot(Tage, Tmean, pch=16, xlab = "Age", ylab = "Weighted mean", cex.lab = 1.3, col = "black") #cex.lab = axis title size
abline(lm(Tmean ~ Tage), col = "blue")
#Multiple R2: 0.1036
#Adj R2: 0.071

#Quadratic
quadratic.model <-lm(Tmean ~ Tage + Tage2)
summary(quadratic.model)
timevalues <- seq(1, 64, 1)
predictedcounts <- predict(quadratic.model,list(Tage=timevalues, Tage2=timevalues^2))
plot(Tage, Tmean, pch=16, xlab = "Age", ylab = "Weighted mean", cex.lab = 1.3, col = "black")
lines(timevalues, predictedcounts, col = "red", lwd = 3)
#Multiple R2: 0.1217
#Adj R2: 0.056






