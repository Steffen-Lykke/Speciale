library(dplyr)
library(tidyr)
library(ggplot2)
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
library(readxl)
require(readr)
library(zoo)
library(latex2exp)
library(fs)


CWF_Multi_9.2 = read_delim("data/test_vand_recovery_27_10_2021.csv",delim = "\t", escape_double = FALSE, 
                           trim_ws = TRUE, skip = 5) #Hente data og få det til passe ned i en tabel
#str(CWF)
dat_M_9.2 = as.data.frame(CWF_Multi_9.2) #Ændrer strukturen af dataen til noget vi kan arbejde med
old_names=colnames(CWF_Multi_9.2) #Vi finder de latterlige navne kolonnerne i dataen har 
new_names=c("Time","level","con0201","pH0201","con0301","flow0301",
            "p0301","p0302","p0401","flow0401","p0501","con0501",
            "temp0501","p0601","temp0601","16","17","18","19","20",
            "21","pmp0201","pmp0301","pmp0401","pmp0501","flow_avg0301",
            "p_avg0301","p_avg0302","p_avg0401","flow_avg0401",
            "p_avg0501","FT0302","weight0302","flow0501","weight0501","36","pH0301",
            "pH0501","con0302","40") #Vi finder på nogle lidt bedre nogle
dat_M_9.2 = dat_M_9.2 %>% rename_at(vars(old_names),~new_names) #Vi ændrer navnene til de nye
dat_M_9.2$Time = dmy_hms(dat_M_9.2$Time)#Vi ændrer formatet af 'dato' kolonnen til et standard format for tid og datoer istedet for at det bare er noget tekst
#str(dat)
dat_M_9.2=dat_M_9.2[-c(1:1147),]

ggplot()+
  geom_point(data=dat_M_9.2, aes(x=Time, y=flow0501), shape= 16)+
  labs(x="Time (18 okt 2021)",y=" Flow mL/min")+
  ylim(0,20)+
  ggtitle("permeate flow") 


### Finde t=0 (fosøgsstart = der hvor flow er over 16 mL/min)
flow_boolean = dat_M_9.2$flow0501>16&dat_M_9.2$flow0501<25
start_tid = min(which(flow_boolean))
dat_M_9.2=dat_M_9.2[-c(1:start_tid),]
flow_boolean_slut = dat_M_9.2$flow0501<1
slut_tid = min(which(flow_boolean_slut))
dat_M_9.2=dat_M_9.2[-c(slut_tid:(nrow(dat_M_9.2)) ),]
### Tid i s fra forsøgs start
dat_M_9.2 = dat_M_9.2%>%
  mutate(sek=(
    as.numeric(as.POSIXct(dat_M_9.2$Time))-as.numeric(dat_M_9.2$Time[1], format='%Y-%m-%d %H:%M:%S'))
  )  


ggplot()+
  geom_point(data=dat_M_9.2, aes(x=sek/3600, y=flow0501), shape= 16)+
  labs(x="Time (18 okt 2021)",y=" Flow mL/min")+
  ylim(0,20)+
  ggtitle("permeate flow") 

csumflow0501_9.2 <- cumsum((dat_M_9.2$flow0501)/60)
dat_M_9.2 <- cbind(dat_M_9.2,csumflow0501_9.2)

perm_volume_9.2 <- tail(dat_M_9.2$csumflow0501_9.2, n=1)/1000
initial_feed_volume <-4.850
perm_volume_målt <- 2.104
recovery_r <- perm_volume_9.2/initial_feed_volume*100
recovery_målt <- perm_volume_målt/initial_feed_volume*100
forskel_perm_vol <- perm_volume_målt-perm_volume_9.2

4.850-(2.700+2.104)
4.850-(2.700+perm_volume_9.2)
