#CT model med diskontinuert blowdown



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
Q_blowdown = 0 #m^3 /d hvor meget fjernes fra resevooiret

c_makeup = c(
  1.6, #Na
  0.6, #Cl
  0.5, #SO4
  0.4 #SiO2
) #En vektor med de forskellige koncentrationer [mM]


## https://www.researchgate.net/figure/Ion-mobility-in-infinitely-diluted-aqueous-solutions-at-temperature-of-25-S_tbl1_26547884
#SO4  - 80
#Ca   - 119
#K    - 73.5
#Al   - 63
#Mg   - 53.1
##
ion_values = data.frame(
  Ions = c("Na", "Cl", "SO4", "SiO2"),
  value = c(6, 7.1, 2.5, 2.5),
  #Grænseværdier [mM]
  molar_con = c(50.1, 76.4, 160, NA)
)#[S*cm^2*mol^-1]

c_guideline = ion_values[,2] #Vektor med grænseværdier for ioner
con_ini = sum(c_makeup*ion_values[,3],na.rm=T)+600
con=con_ini
con_lim = 1000#conductivity grænseværdi [uS/cm]

COC_max = c_guideline/c_makeup
COC = min(COC_max)
o = which(COC_max == COC)
oi = ion_values[1,o]
ci=con_lim/con_ini

para = function(x){
  x1=x/run_time
  y=(x1+Q_vap/dt)/x1
paste('Model run with COC: ',y,'Conductivity limit: ',con_lim,'uS/cm')
}
##### Model Parameters #####
dt=10 #minutes
dt=dt/1440 #minutter i dage
run_time = 60 #days
max_time = run_time*60*24 #i minutter
n_time_step = run_time/dt+1
tid = 0
drift = 0


#df=mat.or.vec(n_time_step,2)
#nf=mat.or.vec(n_time_step,5)
#cf=nf
#### Data frames ####
df = data.frame(
  tid=double(),  
  V_CT=double(),
  con=double(),
  stringsAsFactors = FALSE
)

nf = data.frame(
  tid=double(),
  Na=double(),
  Cl=double(),
  SO4=double(),
  SiO2=double()
)#Dataframe med masse (mol) flow i systemet [mol?]

cf = data.frame(
  tid=double(),
  Na=double(),
  Cl=double(),
  SO4=double(),
  SiO2=double()
)#Dataframe med concentrationer i systemet [mM]

#Initial Values
df[1:n_time_step,]=0
nf[1:n_time_step,]=0
cf[1:n_time_step,]=0

df[1,]=c(tid,V_CT,con_ini)
cf[1,]=c(tid,c_makeup)
nf[1,]=cf[1,]*df$V_CT[1]
num_bd=0
i=2


Q_vap = Q_vap*dt #Hvor meget fordamper per tidsskridt
#Q_blowdown=(Q_vap/(-1+COC))
###### CT Model ######
while(i < n_time_step){
    while(con < con_lim){
      ##Volume flow
      Q_makeup = Q_vap + Q_blowdown # hvor meget vand skal ind i systemet
      Q = Q_makeup-Q_blowdown-Q_vap
      df$V_CT=df$V_CT[1]
      ## mass flow ##
      n_mu = Q_makeup*c_makeup
      n_bd = Q_blowdown*cf[i-1,2:5]
      n_vap = Q_vap*cf[i-1,2:5]*drift
      n_flow=n_mu-n_bd-n_vap
      
      
      ## Current Concentration & Conductivity ##
      nf[i,2:5] = nf[i-1,2:5]+n_flow
      cf[i,2:5] = nf[i,2:5]/df$V_CT[i]  
      con=sum(cf[i,2:5]*ion_values[,3],na.rm=T)
      df$con[i] = con
      
      df$tid[i]=df$tid[i-1]+dt
      nf$tid[i]=nf$tid[i-1]+dt
      cf$tid[i]=cf$tid[i-1]+dt
      i=i+1
    }
  nf[i,2:5] = nf[i-1,2:5]*(1-0.1667)+1*c_makeup
  cf[i,2:5] = nf[i,2:5]/df$V_CT[i]  
  con=sum(cf[i,2:5]*ion_values[,3],na.rm=T)
  df$con[i] = con
  num_bd=num_bd+1
  df$tid[i]=df$tid[i-1]+dt
  nf$tid[i]=nf$tid[i-1]+dt
  cf$tid[i]=cf$tid[i-1]+dt
  i=i+1
}


########## plot #############
cf.long = cf %>% 
  gather(key,value,Na,Cl,SO4,SiO2)
df.long = df %>% 
  gather(key,value,V_CT,con)

level_order = c('Na','Cl','SO4','SiO2')
ggplot(cf.long,aes(x=tid,y=value,color=factor(key,level=level_order)))+geom_line()+
  scale_color_brewer(palette="Set1",labels = level_order)+
  theme_bw()+labs(y = "Concentration [mM]", x = "Time [days]", color = "Ion")

ggplot(df,aes(x=tid,y=con))+geom_line()+
  scale_color_brewer(palette="Set1")+
  theme_bw()+labs(y = "Conductivity [uS/cm]", x = "Time [days]")+ylim(c(0,NA))
para(num_bd)
