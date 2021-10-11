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
CWF <- read_delim("data/singlesalt_NaCl3mM_14-09-2021.csv", 
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
#str(dat)
####################### Plotting of Data ##########################
ggplot(dat,aes(time,p_avg0302)) + geom_line() + theme_bw() +
  ylab("Pressure [Bar]") + xlab("Time") + 
  scale_x_datetime(breaks = scales::date_breaks("30 mins"),date_labels = "%H:%M") #Et relativt simplet plot af noget tryk mod tid i ggplot, det er ikke så 'interaktivt' så vi prøver også med plotly

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


dat %>%
  mutate(rejection=(1-(con0501/con0301))*100)%>%
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
         yaxis=list(title="\U03BC S/cm")) #Her conductivity over tid + en form for rejection, 
#læg gerne mærke til farverne der nægter at se ud som de er blevet bedt.

############### plot af pH ################

lims <- as.POSIXct(strptime(c("2021-09-22 07:00:00","2021-09-22 13:00:00"), format = "%Y-%m-%d %H:%M:%S"))

ggplot(dat,aes(time,pH0201)) + geom_point() + theme_bw() +
  ylab("pH") + xlab("Time") +ylim(7,9)+
  scale_x_datetime(limits=ymd_hm(c("2021-09-22 07:48","2021-09-22 12:00")), breaks = scales::date_breaks("30 mins"),date_labels = "%H:%M")+
  ggtitle("pH Na2OSiO2")

ggplot(dat,aes(x=time,y=dat[,34])) + geom_line() + theme_bw() +
  scale_x_datetime(limits=ymd_hm(c("2021-09-27 12:00","2021-09-27 23:00")), breaks = scales::date_breaks("30 mins"),date_labels = "%H:%M")+
  ylab("Boolean") + xlab("Time")+ylim(0,100)
