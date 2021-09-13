###### Initialize #####
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
library(lobstr)

################ Hent Data ##################
test= read.csv("data/CWF_12.4.2021_07-09-2021 084249.csv")
CWF <- read_delim("data/CWF_12.4.2021_07-09-2021 084249.csv", 
                    delim = "\t", escape_double = FALSE, 
                    trim_ws = TRUE, skip = 5)
str(CWF)
dat = as.data.frame(CWF)
old_names=colnames(CWF)
new_names=c("time","level","con0201","pH0201","con0301","flow0301",
            "p0301","p0302","p0401","flow0401","p0501","con0501",
            "temp0501","p0601","temp0601","16","17","18","19","20",
            "21","pmp0201","pmp0301","pmp0401","pmp0501","flow_avg0301",
            "p_avg0301","p_avg0302","p_avg0401","flow_avg0401",
            "p_avg0501","FT0302","weight0302","flow0501","weight0501","36","pH0301",
            "pH0501","con0302","40")
dat = dat %>% rename_at(vars(old_names),~new_names)
dat$time = dmy_hms(dat$time)
str(dat)
####################### Plotting of Data ##########################
ggplot(dat,aes(time,c(p_avg0301,p_avg0302)))+geom_line()+theme_bw()+ylab("Pressure [Bar]")+xlab("Time")+scale_x_datetime(breaks = scales::date_breaks("30 mins"),date_labels = "%H:%M")

dat %>%
  mutate(TMP=(p_avg0301+p_avg0302)/2-p_avg0501)%>%
  plot_ly(x=~time,
          y=~p_avg0301,
          name="Pressure before membrane",
          color='#1f77b4',
          type='scatter',
          mode='lines')%>%
  add_lines(y=~p_avg0302,
            name='Pressure after membrane',
            color='#E41317')%>%
  add_lines(y=~TMP,
            name='TMP',
            color='#forestgreen')%>%
  layout(title="Pressure during day",
         legend=list(x=0.7,y=0.2),
         yaxis=list(title="Pressure [bar]"),
         xaxis=list(title=":)"))
