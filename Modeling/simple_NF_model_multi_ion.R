## Simple(ish) NF model with constant rejection for multiple ions
############ Initialize ########################
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
library(ggthemes)



##### TEST ENVIRONMENT #####
A=0.05
V=10
dt=60
rec=80
conc=c(17,3.4,0.7,0.7,0.09)
J=20
test = NF(0.05,10,60,95,20,c(10,10,5,5))


############# Operational Parameters ###############
R = 0.08314
Temp = 25
flux_LMH = J #

membrane_area = A #m^2

Q_feed_mL_min = 1160 #mL/min

initial_feed_tank_volume = feed_tank_volume = V #L
initial_permeate_tank_volume = permeate_tank_volume = 0 #L

ion_values = data.frame(
  ions = c("Na", "Cl", "SO4", "SiO2","Ca"),
  feed = conc,#Initial concentration [mM]
  molar_con = c(50.1, 76.4, 160, NA,NA),# Molær konduktivitet[S*cm^2*mol^-1]
  R = c(0.8,  -0.1,  0.95,0.7,0.8)
  )
rejection = ion_values[,4]
initial_feed_conc = ion_values[,2]
initial_con = sum(initial_feed_conc*ion_values[,3],na.rm = T)
con_f = initial_con
os_ini = sum(initial_feed_conc)*10^-3*R*(Temp+273)
############# model parameters #############
dt=dt #s
run_time = 8 #hours
max_time = run_time*3600
n_time_step = max_time/dt
tid = 0
flux = flux_LMH/3600 #L m^-2 s^-1 
Q_feed_L_s = Q_feed_mL_min/1000/60#omregning til L/s
Q_feed = Q_feed_L_s*dt
###### Dataframes ######
df = data.frame(
  tid=double(),  
  feed_tank_volume=double(),
  permeate_tank_volume=double(),
  con_f=double(),
  con_p=double(),
  os=double(),
  rec=double(),
  stringsAsFactors = FALSE
)

nf_f = data.frame(
  Na=double(),
  Cl=double(),
  SO4=double(),
  SiO2=double(),
  Ca=double()
)#Dataframe med masse (mol) flow i feed tank [mol]

cf_f = data.frame(
  Na=double(),
  Cl=double(),
  SO4=double(),
  SiO2=double(),
  Ca=double()
)#Dataframe med concentrationer i feed tank [mM]

#Nu de samme data frames for permeate tank
nf_p=nf_f
cf_p=cf_f
#Preallocate and fill in initial values
df[1:(n_time_step+100),]=NA
cf_p[1:(n_time_step+100),]=NA
cf_f=nf_f=nf_p=cf_p

df[1,]=c(0,feed_tank_volume,permeate_tank_volume,con_f,NA,os_ini,0)

cf_f[1,]=initial_feed_conc # i mM
nf_f[1,]=cf_f[1,]*feed_tank_volume # i mmol

nf_p[1,]=0 # i mmol

current_rec = 0
i=2
#cf_p
#nf_p

############ Model #####################
while (current_rec < rec) {
  #Volume flow
  Q_permeate = membrane_area*flux*dt
  Q_retentate = Q_feed- Q_permeate
  
  df$feed_tank_volume[i] = df$feed_tank_volume[i-1] - Q_permeate
  df$permeate_tank_volume[i] = df$permeate_tank_volume[i-1] + Q_permeate
  
  #Concentration of Retentate and Permeate
  
  retentate_conc = -((cf_f[i-1,]*(Q_permeate*rejection + 2*Q_feed - Q_permeate))/(Q_permeate*rejection - Q_permeate - 2*Q_retentate))
  permeate_conc=cf_f[i-1,]*(Q_feed*rejection + Q_retentate*rejection - Q_feed - Q_retentate)/(Q_permeate*rejection - Q_permeate - 2*Q_retentate)
  
  #Mass flow
  n_retentate = retentate_conc * Q_retentate
  n_permeate =  permeate_conc * Q_permeate
  
  nf_f[i,] = nf_f[i-1,] - n_permeate
  nf_p[i,] = nf_p[i-1,] + n_permeate
  
  
  #Current concentration
  cf_f[i,] = nf_f[i,]/df$feed_tank_volume[i]
  cf_p[i,] = nf_p[i,]/df$permeate_tank_volume[i]
  
  ## Osmotic Pressure
  df$os[i]=sum(cf_f[i,1:4])*10^-3*R*(Temp+273)
  
  ##Conductivity
  #con_f = 
  #con_p = 
  
  
  #df$con_f = sum(cf_f[1,-(ncol(cf_f))]*ion_values[,3],na.rm = T)
  #df$con_p[i] = con_p
  
  
  df$tid[i] = df$tid[i-1] + dt 
  current_rec = df$rec[i] = df$permeate_tank_volume[i]/initial_feed_tank_volume*100
  i=i+1
}
df$con_f=rowSums(data.frame(mapply(`*`,cf_f[,1:3],ion_values$molar_con[1:3])),na.rm=T)
df$con_p=rowSums(data.frame(mapply(`*`,cf_p[,1:3],ion_values$molar_con[1:3])),na.rm=T)
rm(tid)
cf_f$tid = cf_p$tid = df$tid
cf_p[1,]=NA
df$con_p[1]=NA
#rm(feed_tank_volume,feed_tank_mass,feed_tank_conc,permeate_tank_volume,permeate_tank_mass,permeate_tank_conc)


########## plot #############
cf_f.long = cf_f %>%
  gather(key,value, Na,Cl,SO4,SiO2,Ca)
cf_p.long = cf_p %>%
  gather(key,value, Na,Cl,SO4,SiO2,Ca)
df.long = df %>%
  gather(key,value, feed_tank_volume,permeate_tank_volume,con_f,con_p)

level_order = c('Na','Cl','SO4','SiO2','Ca')

fig1=
  ggplot(cf_f.long,aes(x=tid/3600,y=value,color=factor(key,level=level_order)))+geom_line()+
  scale_color_brewer(palette="Set1",labels=level_order)+
  theme_bw()+labs( y = "Concentration [mM]", x = "Time [h]", color = "")

fig2=
  ggplot(cf_p.long,aes(x=tid/3600,y=value,color=factor(key,level=level_order)))+geom_line()+
  scale_color_brewer(palette="Set1")+
    theme_bw()+labs( y = "Concentration [mM]", x = "Time [h]", color = "")

fig3=
  ggplot(df.long,aes(x=tid/3600,y=value,color=key))+geom_line()+
  scale_color_brewer(palette="Set1",labels=c("Feed Conductivity","Permeate Conductivity","Feed Volume","Permeate volume"))+
  theme_bw()+labs( y = "Concentration [mM]", x = "Time [h]", color = "")

fig4=
  ggplot(df.long,aes(x=tid/3600,y=value,color=key))+geom_line()+
  scale_color_brewer(palette="Set1",labels=c("Feed Conductivity","Permeate Conductivity","Feed Volume","Permeate volume"))+
  theme_bw()+labs( y = "Value", x = "Time [h]", color = "")#+ylim(c(NA,NA))

ggplotly(fig1)
