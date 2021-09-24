#Vi henter lidt godt og blandet pakker
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(readxl)
library(visdat)
library(GGally)
library(Hmisc)
library(DMwR2)
library(data.table)
library(mltools)
library(ggfortify)
library(ggiraphExtra)
library(coronavirus)
library(lubridate)
library(plotly)
library(latex2exp)
library(zoo)

##### bicarbonat data   #########

B_NaCl <- c(8.4,31,42,43,15,48)
B_CaCl2 <- c(6,15,18,20,7.8,20)

B_Na2OSiO2 <- c(126,128,141,157,196,205,61,212)
tid_lang <- (c(0,0,1,2,3.5,4,4,4))
tid_kort <- c(0,2,3.5,4,4,4)

SingleSalt_Cl <- data.frame(tid_kort,B_NaCl, B_CaCl2)
Singlesalt_SiO2 <- data.frame(tid_lang,B_Na2OSiO2)

## plot 

ggplot(data=SingleSalt_Cl, aes(x=tid_kort))+
  geom_point(aes( y=B_NaCl, color="NaCl"))+
  geom_point(aes( y=B_CaCl2,color="CaCl2"))+
  scale_color_manual(values=c("NaCl"="red","CaCl2"="green"),labels=c("NaCl", "CaCl2"))+
  labs(x="Time[h]",y="Bicarbonate  [mg/L]", color="Legend")+
  ggtitle("Bicarbonate concentration Singlesalt")

ggplot(data=Singlesalt_SiO2, aes(x=tid_lang))+
  geom_point(aes( y=B_Na2OSiO2, color="SiO2"))+
  scale_color_manual(values=c("SiO2"="black"),labels=c("SiO2"))+
  labs(x="Time[h]",y="Bicarbonate  [mg/L]", color="Legend")+
  ggtitle("Bicarbonate concentration Singlesalt")

### Plottet sammen ###

ggplot(data=SingleSalt_Cl, aes(x=tid_kort))+
  geom_point(aes( y=B_NaCl, color="NaCl"))+
  geom_point(aes( y=B_CaCl2,color="CaCl2"))+
  geom_point(data=Singlesalt_SiO2, aes(x=tid_lang, y=B_Na2OSiO2, color="SiO2"))+
  scale_color_manual(values=c("NaCl"="red","CaCl2"="green", "SiO2"="blue"),labels=c("NaCl", "CaCl2", "SiO2"))+
  labs(x="Time[h]",y="Bicarbonate  [mg/L]", color="Legend")+
  ggtitle("Bicarbonate concentration Singlesalt")












