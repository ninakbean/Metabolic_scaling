
library(readxl)
library(tidyverse)
library(ggplot2)
library(car)
library(OIdata)


setwd("/Users/nina/Research/Research Ideas")
symbiont <-read_excel("Costs and benefits of maternally inherited algal symbionts in coral larvae SUPP.xls",sheet="Symbiont provisionning") #to read your file

long_symbiont <- symbiont%>%
 select("Mother","Beige Small","Brown Small","Dark Small","Beige Medium","Brown Medium","Dark Medium","Beige Large","Brown Large","Dark Large")%>%
 filter(Mother!="NA")%>%
 gather(type,"number",-Mother)

avg_symbiont <- long_symbiont%>%
  select(-"Mother")%>%
  group_by(type)%>%
  summarise(avg=mean(number),
          stdev=sd(number),
          se=(sd(number)/sqrt(length(number))))

levels(avg_symbiont$type) <- gsub(" ", "\n", levels(avg_symbiont$type))

mod1 <- aov(number~type, data=long_symbiont)
summary(mod1)

TukeyHSD(mod1)

ggplot(long_symbiont)+ 
  geom_bar(aes(y = number, x = Mother, fill = type), data = long_symbiont,
                          stat="identity")+
  scale_fill_manual(values=c("#FFCDD2","#B3E5FC","#FFF9C4","#EF5350", "#42A5F5", "#FFF176","#B71C1C","#0D47A1","#F9A825"))+
  ylab("Abundance")+
  theme_classic()

#blank
ggplot(avg_symbiont)+ 
  geom_bar(aes(y = avg, x = type, fill = type), data = avg_symbiont,
           stat="identity")+
  scale_fill_manual(values=c("black","black","black","black", "black", "black","black","black","black"))+
  ylab("Abundance")+
  theme_classic()

#ambient
ggplot(avg_symbiont)+ 
  geom_bar(aes(y = avg, x = type, fill = type), data = avg_symbiont,
           stat="identity")+
  scale_fill_manual(values=c("#8faad9","#b5c7e6","#d9e1f1","#325694", "#4674c1", "#b5c7e6","#213863","#325694","#8faad9"))+
  ylab("Abundance")+
  theme_classic()
  
#hot
  ggplot(avg_symbiont)+ 
    geom_bar(aes(y = avg, x = type, fill = type), data = avg_symbiont,
             stat="identity")+
    scale_fill_manual(values=c("#213863","#325694","#4674c1","#8faad9", "#b5c7e6", "#b5c7e6","#d9e1f1","#d9e1f1","#d9e1f1"))+
    ylab("Abundance")+
    theme_classic()



#scale_x_continuous(name = 'Phenotype', 
               #    labels = c('Light\nLarge', 'Light\nMedium','Light\nSmall',
               #               'Brown\nLarge','Brown\nMedium','Brown\nSmall',
               #               'Dark Brown\nLarge','Dark Brown \n Medium','Dark Brown \n Small'))+

#labels = c('Light\nLarge', 'Light\nMedium','Light\nSmall',
 #          'Brown\nLarge','Brown\nMedium','Brown\nSmall',
  #         'Dark Brown\nLarge','Dark Brown \n Medium','Dark Brown \n Small'))+
  
  







