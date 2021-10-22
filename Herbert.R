#Der skal ske noget syg programmering her
#CT model ud fra COC



#### Initialize Packages ####
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
library(fcuk)



##### CT parameters #####
V_CT = 8 # m^3 | Hvad er reservoir volumet
Q_vap = 2 #m^3 / day | Hvor meget fordamper
Q_blowdown = 1 #m^3 /d hvor meget fjernes fra resevooiret



c_makeup = c(
  1.6, #Na
  0.6, #Cl
  0.5, #SO4
  0.4 #SiO2
  ) #En vektor med de forskellige koncentrationer


c_CT_ini = c_makeup


c_guideline =c(
  12, #Na
  7.1, #Cl
  2.5, #SO4
  2.5 #SiO2
) #Vektor med grænseværdier for ioner

COC_max = c_guideline/c_makeup
COC = min(COC_max)
drift=0
##### Model Parameters #####
dt=1 #minutes
dt=dt/1440
run_time = 1 #days
max_time = run_time*60*24 #i minutter
n_time_step = run_time/dt+1
tid = 0

#### Data frames ####
df = data.frame(
  tid=double(),  
  V_CT=double(),
  stringsAsFactors = FALSE
)

nf = data.frame(
  tid=double(),
  Na=double(),
  Cl=double(),
  SO4=double(),
  SiO2=double()
)

cf = data.frame(
  tid=double(),
  Na=double(),
  Cl=double(),
  SO4=double(),
  SiO2=double()
)
#Initial Values
df[1:n_time_step,]=0
nf[1:n_time_step,]=0
cf[1:n_time_step,]=0
df[1,]=c(tid,V_CT)
nf[1,]=c(tid,c_makeup)
cf[1,]=nf[1,]/df$V_CT[1]
###### CT Model ######
for (i in 2:n_time_step) {
  
  ## Volume flow
  
  Q_vap = Q_vap*dt #big math
  Q_blowdown=(Q_vap/(-1+COC))
  Q_makeup = Q_vap + Q_blowdown # hvor meget vand skal ind i systemet
  Q = Q_makeup-Q_blowdown-Q_vap
  ## Current volume
  df$V_CT[i]=df$V_CT[i-1]+Q
  
  
  ## mass flow ##
  n_mu = Q_makeup*c_makeup
  n_bd = Q_blowdown*cf[i-1,2:5]
  n_vap = Q_vap*cf[i-1,2:5]*drift
  n_flow=n_mu-n_bd-n_vap

  ## Current Concentration ##

  nf[i,2:5] = nf[i-1,2:5]+n_flow
  cf[i,2:5] = nf[i,2:5]/df$V_CT[i]  

  df$tid[i]=df$tid[i-1]+dt
  nf$tid[i]=nf$tid[i-1]+dt
  cf$tid[i]=cf$tid[i-1]+dt
  
}

########## plot #############
cf.long = cf %>% 
  gather(,value,Na,Cl,SO4,SiO2)

ggplot(cf.long,aes(x=tid,y=value,color=key))+geom_line()+
  scale_color_brewer(palette="Set1",labels = c("Na", "Cl","SO4","SiO2"))+
  theme_bw()+labs( y = "Concentration [mM?]", x = "Time [days]", color = "")
