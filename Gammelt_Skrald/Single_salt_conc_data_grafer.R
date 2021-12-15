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

##### Na2OSiO2 data  #########

tid <- c(1,2,3,3.5,4)

#concentrations data: 
SiO2_conc_feed <- c(60.7,60.3,63.6,63.6,63)
SiO2_conc_conc <- c(62,62.7,63.9,63.4,65.3)
SiO2_conc_permeate <- c(52.8,55.5,54.7,56.7,51.6)
SiO2_conc_inlet = (SiO2_conc_feed+SiO2_conc_conc)/2
SiO2_rejection = (1-(SiO2_conc_permeate/SiO2_conc_feed))*100

###pH data: 
SiO2_ph_feed <- c(8.29,8.26,8.29,8.35,8.37)
SiO2_ph_conc <- c(8.29,8.27,8.29,8.33,8.37)
SiO2_ph_permeate <- c(7.77,7.87,7.93,7.96,7.9)
SiO2_ph_inlet = (SiO2_ph_feed+SiO2_ph_conc)/2


### bi carbonate 

SiO2 <- data.frame(tid,SiO2_conc_feed,SiO2_conc_conc, SiO2_conc_permeate,SiO2_ph_feed,SiO2_ph_conc, SiO2_ph_permeate)



### raw data, feed, concentrate and permeate
ggplot(data=SiO2, aes(x=tid))+
  geom_line(aes( y=SiO2_conc_feed, color="Feed"))+
  geom_line(aes( y=SiO2_conc_conc,color="Concentrate"))+
  geom_line(aes( y=SiO2_conc_permeate,color="Permeate"))+
  scale_color_manual(values=c("Feed"="red","Concentrate"="blue","Permeate"="green"),labels=c("Feed", "Concentrate", "Permeate"))+
  labs(x="Time[h]",y="SiO2 concentration  [mg/L]", color="Legend")+
  ggtitle("Concentration, feed, concentrate, permeate")


ggplot(data=SiO2, aes(x=tid))+
  geom_line(aes( y=SiO2_conc_inlet, color="Inlet"))+
  geom_line(aes( y=SiO2_conc_permeate,color="Permeate"))+
  scale_color_manual(values=c("Inlet"="red","Permeate"="green"),labels=c("Inlet", "Permeate"))+
  labs(x="Time[h]",y="SiO2 concentration  [mg/L]", color="Legend")+
  ggtitle("Concentration 'inlet' and permeate")

### rejection 

ggplot(data=SiO2, aes(x=tid))+
  geom_line(aes(y=SiO2_rejection),color="black")+
  ylim(0,100)+
  ggtitle("Rejection %")

scalefactor_rej <- max(SiO2_conc_feed)/max(SiO2_rejection)
ggplot(data=SiO2, aes(x=tid))+
  geom_line(aes(y=SiO2_conc_feed,color="Feed"))+
  geom_line(aes( y=SiO2_conc_permeate,color="Permeate"))+
  geom_line(aes(y=SiO2_rejection*scalefactor_rej, color="Rejection"))+
  scale_y_continuous(name="SiO2 concentration [mg/L]",sec.axis = sec_axis(~./scalefactor_rej,name="Rejection [%]"))+
  scale_color_manual(values=c("Feed"="red","Permeate"="blue","Rejection"="green"),labels=c("Feed", "Permeate", "Rejection"))+
  theme(
    #axis.title.y.left = element_text(color="red"),
    #axis.text.y.left = element_text(color="red"),
    # axis.title.y.right = element_text(color="green"),
    axis.text.y.right = element_text(color="green"),
  )+
  labs(x="Time[h]", color="Legend")+
  ggtitle("Rejection og feed og perm concentration over tid")

## rejection af SiO2 og pH over tid. 
scalefactor_rej_pH <- max(SiO2_ph_feed)/max(SiO2_rejection)
ggplot(data=SiO2, aes(x=tid))+
  geom_line(aes(y=SiO2_ph_feed,color="pH Feed"))+
  geom_line(aes(y=SiO2_rejection*scalefactor_rej_pH, color="Rejection"))+
  scale_y_continuous(name="pH",sec.axis = sec_axis(~./scalefactor_rej_pH,name="Rejection [%]"))+
  scale_color_manual(values=c("pH Feed"="red","Rejection"="green"),labels=c("pH Feed", "Rejection"))+
  theme(
    #axis.title.y.left = element_text(color="red"),
    #axis.text.y.left = element_text(color="red"),
    #axis.title.y.right = element_text(color="green"),
    axis.text.y.right = element_text(color="green"),
  )+
  labs(x="Time[h]", color="Legend")+
  ggtitle("Rejection og pH over tid.")

### raw data pH. 
ggplot(data=SiO2, aes(x=tid))+
  geom_line(aes( y=SiO2_ph_inlet, color="Inlet"))+
  geom_line(aes( y=SiO2_ph_permeate,color="Permeate"))+
  scale_color_manual(values=c("Inlet"="red","Permeate"="green"),labels=c("Inlet", "Permeate"))+
  labs(x="Time[h]",y="SiO2 concentration  [mg/L]", color="Legend")+
  ggtitle("pH i indlet strøm og permeate strøm målt manuelt")

#### digital pH data mod målt pH data. 
#test= read.csv("data/CWF_12.4.2021_07-09-2021 084249.csv")
CWF <- read_delim("data/singlesalt_Na2OSiO2_1mM_22-09-2021.csv", 
                  delim = "\t", escape_double = FALSE, 
                  trim_ws = TRUE, skip = 5) #Hente data og få det til passe ned i en tabel
#str(CWF)
dat = as.data.frame(CWF) #Ændrer strukturen af dataen til noget vi kan arbejde med
old_names=colnames(CWF) #Vi finder de latterlige navne kolonnerne i dataen har 
new_names=c("time","level","con0201","pH0201","con0301","flow0301",
            "p0301","p0302","p0401","flow0401","p0501","con0501",
            "temp0501","p0601","temp0601","16","17","18","19","20",
            "21","pmp0201","pmp0301","pmp0401","pmp0501","flow_avg0301",
            "p_avg0301","p_avg0302","p_avg0401","flow_avg0401",
            "p_avg0501","FT0302","weight0302","flow0501","weight0501","36","pH0301",
            "pH0501","con0302","40") #Vi finder på nogle lidt bedre nogle
dat = dat %>% rename_at(vars(old_names),~new_names) #Vi ændrer navnene til de nye
dat$time = dmy_hms(dat$time)#Vi ændrer formatet af 'dato' kolonnen til et standard format for tid og datoer istedet for at det bare er noget tekst

ggplot(dat,aes(time,pH0201)) + geom_line() + theme_bw() +
  ylab("pH") + xlab("Time") + 
  scale_x_datetime(breaks = scales::date_breaks("30 mins"),date_labels = "%H:%M")

## combi af conc og pH. 

scalefactor_pH <- max(SiO2_conc_permeate)/max(SiO2_ph_permeate)
ggplot(data=SiO2, aes(x=tid))+
  geom_line(aes( y=SiO2_conc_permeate,color="Permeate"))+
  geom_line(aes(y=SiO2_ph_permeate*scalefactor_pH, color="Permeate pH"))+
  scale_y_continuous(name="SiO2 concentration [mg/L]",sec.axis = sec_axis(~./scalefactor,name="pH"))+
  scale_color_manual(values=c("Permeate"="red","Permeate pH"="green"),labels=c("Permeate", "Permeate pH"))+
  theme(
    #axis.title.y.left = element_text(color="red"),
    axis.text.y.left = element_text(color="red"),
   # axis.title.y.right = element_text(color="green"),
    axis.text.y.right = element_text(color="green"),
  )+
  labs(x="Time[h]", color="Legend")
  



  