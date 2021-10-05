###### Initialize #####
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

################ Hent Data ##################

hilda_1 <- read_delim("data/Hilda/hilda_1.csv", 
                  delim = "\t", escape_double = FALSE, 
                  trim_ws = TRUE, skip = 5) #Hente data og få det til passe ned i en tabel

hilda_2 <- read_delim("data/Hilda/hilda_2.csv", 
                      delim = "\t", escape_double = FALSE, 
                      trim_ws = TRUE, skip = 5) #Hente data og få det til passe ned i en tabel

hilda_3 <- read_delim("data/Hilda/hilda_3.csv", 
                      delim = "\t", escape_double = FALSE, 
                      trim_ws = TRUE, skip = 5) #Hente data og få det til passe ned i en tabel

hilda_4 <- read_delim("data/Hilda/hilda_4.csv", 
                      delim = "\t", escape_double = FALSE, 
                      trim_ws = TRUE, skip = 5) #Hente data og få det til passe ned i en tabel

hilda_5 <- read_delim("data/Hilda/hilda_5.csv", 
                      delim = "\t", escape_double = FALSE, 
                      trim_ws = TRUE, skip = 5) #Hente data og få det til passe ned i en tabel

hilda_6 <- read_delim("data/Hilda/hilda_6.csv", 
                      delim = "\t", escape_double = FALSE, 
                      trim_ws = TRUE, skip = 5) #Hente data og få det til passe ned i en tabel
#str(CWF)
dat = rbind(as.data.frame(hilda_2),as.data.frame(hilda_3),as.data.frame(hilda_4),as.data.frame(hilda_5),as.data.frame(hilda_6)) #Ændrer strukturen af dataen til noget vi kan arbejde med
old_names=colnames(hilda_1) #Vi finder de latterlige navne kolonnerne i dataen har 
new_names=c("time","level","con0201","pH0201","con0301","flow0301",
            "p0301","p0302","p0401","flow0401","p0501","con0501",
            "temp0501","p0601","temp0601","16","17","18","19","20",
            "21","pmp0201","pmp0301","pmp0401","pmp0501","flow_avg0301",
            "p_avg0301","p_avg0302","p_avg0401","flow_avg0401",
            "p_avg0501","FT0302","weight0302","flow0501","weight0501","36","pH0301",
            "pH0501","con0302","40") #Vi finder på nogle lidt bedre nogle
dat = dat %>% rename_at(all_of(old_names),~new_names) #Vi ændrer navnene til de nye
dat$time = dmy_hms(dat$time)#Vi ændrer formatet af 'dato' kolonnen til et standard format for tid og datoer istedet for at det bare er noget tekst

### Forsøg på automatisk at finde t=0

start_tid = min(which(dat$flow0501>16&dat$flow0501<25))
dat=dat[-c(1:start_tid),]
### Tid i s fra forsøgs start
dat = dat%>%
   mutate(sek=(
           as.numeric(as.POSIXct(time))-as.numeric(dat$time[1], format='%Y-%m-%d %H:%M:%S'))
                ) #den regner 2 timer for meget, tag højde for det manuelt checket hvornår flow rammer 17 ml/min. 
            


#str(dat)
####################### Plotting of Data ##########################
ggplot(dat,aes(sek/3600,p_avg0302)) + geom_line() + theme_bw() +
  ylab("Pressure [Bar]") + xlab("Time [h]")  
  #Et relativt simplet plot af noget tryk mod tid 


ggplot(dat,aes(sek/3600,flow0501)) + geom_line() + theme_bw() +
  ylab("Pressure [Bar]") + xlab("Time [h]")+xlim(c(0,80))+ylim(c(0,20))  




dat %>%#dplyr arbejder indenfor 'dat' data framen
  mutate(TMP=(p_avg0301+p_avg0302)/2-p_avg0501)%>%#Her regner vi TMP, som dplyr husker nu er en del af den data
  mutate(TMP_slid=rollapply(TMP,100,mean,align="right",fill=NA))%>%#Nu tager vi et glidende gennemsnit
  plot_ly(x=~time,
          y=~p_avg0301,
          name="Pressure before membrane",
          color='#1f77b4',
          type='scatter',
          mode='lines')%>%
  add_lines(y=~p_avg0302,
            name='Pressure after membrane',
            color='#E41317')%>%
  add_lines(y=~TMP_slid,
            name='TMP',
            color='#forestgreen')%>%
  layout(title="Pressure during day",
         legend=list(x=0.7,y=0.2),
         yaxis=list(title="Pressure [bar]"),
         xaxis=list(title=":)")) #Her plottet de to tryk på feed siden plus et TMP


ay <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "Rejection") #Her noget hokus-pokus for at kunne lave 2 y akser


dat %>%mutate(rejection=(1-(con0501/con0301))*100)%>%
  mutate(feed_slid=rollapplyr(con0301,30,mean,fill=NA))%>%
  mutate(rej_slid=rollapplyr(rejection,30,mean,fill=NA))%>%
  plot_ly(x=~time,
          y=~feed_slid,
          name="Feed",
          color="red",
          type='scatter',
          mode='lines',
          line=list(color=('#1f77b4')))%>%
  add_trace(y=~con0501,
            name='Permeate',
            line=list(color="#E41317"))%>%
  add_trace(y=~rej_slid,
            name='Rejection',
            yaxis="y2",
            line=list(color="#forestgreen"))%>%
  # Set figure title, x and y-axes titles
  layout(title="Conductivity",yaxis2=ay,
         legend=list(x=0.6,y=0.5),
         yaxis=list(title=expression(Omega))) #Her conductivity over tid + en form for rejection, 
#læg gerne mærke til farverne der nægter at se ud som de er blevet bedt.


