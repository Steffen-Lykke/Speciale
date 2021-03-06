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
source("Modeling/simple_NF_model_multi_ion.R")


##### CT parameters #####
V_CT = 8 # m^3 | Hvad er reservoir volumet
Q_vap = 2 #m^3 / day | Hvor meget fordamper
Q_blowdown = 0 #m^3 /d hvor meget fjernes fra resevooiret

c_makeup = c(
  1.6, #Na
  0.6, #Cl
  0.5, #SO4
  0.4, #SiO2
  0.01#Ca, noget med blødgøring
) #En vektor med de forskellige koncentrationer [mM]


## https://www.researchgate.net/figure/Ion-mobility-in-infinitely-diluted-aqueous-solutions-at-temperature-of-25-S_tbl1_26547884
#SO4  - 80
#Ca   - 119
#K    - 73.5
#Al   - 63
#Mg   - 53.1
##
ion_values = data.frame(
  Ions = c("Na", "Cl", "SO4", "SiO2","Ca"),
  value = c(6, 7.1, 2.5, 2.5,1),
  #Grænseværdier [mM]
  molar_con = c(50.1, 76.4, 160, NA,NA)
)#[S*cm^2*mol^-1]

c_guideline = ion_values[,2] #Vektor med grænseværdier for ioner
con_ini = sum(c_makeup*ion_values[,3],na.rm=T)
con=con_ini
con_lim = 2000#conductivity grænseværdi [uS/cm]

COC_max = c_guideline/c_makeup
COC = min(COC_max)
o = which(COC_max == COC)
oi = ion_values[1,o]
ci=con_lim/con_ini

para = function(x){
  x1=x/run_time
  y=(x1+Q_vap/dt)/x1
  paste('Model run with COC: ',y,'Conductivity limit: ',con_lim,'uS/cm')
} # En funktion der fortæller hvad COC var for CT 

##### Model Parameters #####
dt=10 #tidsstep i minutter
dt=dt/1440 #minutter i dage
run_time = 60 #Total operating time i dage
max_time = run_time*60*24 #i minutter
n_time_step = run_time/dt+1 #antal tidsskridt 
tid = 0 # start tid?
drift = 0 #en drift factor der har noget med om ioner kommer med evaporation
recovery_factor_volume = 0.05 #Hvor meget makeup skal CT tage ind når der laves BD til batch  

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
  SiO2=double(),
  Ca=double()
)#Dataframe med masse (mol) flow i systemet [mol?]

cf = nf#Dataframe med concentrationer i systemet [mM]

#Initial Values
df[1:n_time_step,]=0
nf[1:n_time_step,]=0
cf[1:n_time_step,]=0

df[1,]=c(tid,V_CT,con_ini)
cf[1,]=c(tid,c_makeup)
nf[1,]=cf[1,]*df$V_CT[1]
num_bd=0
i=2
toggle = F

Q_vap = Q_vap*dt #Hvor meget fordamper per tidsskridt !FIX!
#Q_blowdown=(Q_vap/(-1+COC))
###### CT Model ######
while(i < n_time_step){
  while(con < con_lim){ ##### CT OPERATION #######
    if (toggle == T) {
      #her skal der være noget smart kode der får permeate fra NF ind i CT
      
    }
    ##Volume flow
    Q_makeup = Q_vap + Q_blowdown # hvor meget vand skal ind i systemet
    Q = Q_makeup-Q_blowdown-Q_vap
    df$V_CT=df$V_CT[1]
    
    ## mass flow ##
    n_mu = Q_makeup*c_makeup
    n_bd = Q_blowdown*cf[i-1,2:6]
    n_vap = Q_vap*cf[i-1,2:6]*drift
    n_flow=n_mu-n_bd-n_vap
    
    
    ## Current Concentration & Conductivity ##
    nf[i,2:6] = nf[i-1,2:6]+n_flow
    cf[i,2:6] = nf[i,2:6]/df$V_CT[i]  
    con=sum(cf[i,2:6]*ion_values[,3],na.rm=T)
    df$con[i] = con
    
    df$tid[i]=df$tid[i-1]+dt
    i=i+1
    g=1
  } ##### FILTRERING #####
  nf[i,2:6] = nf[i-1,2:6]*(1-((V_CT-V)/V_CT))+V*recovery_factor*c_makeup
  cf[i,2:6] = nf[i,2:6]/df$V_CT[i]  
  con=sum(cf[i,2:5]*ion_values[,3],na.rm=T)
  df$con[i] = con
  num_bd=num_bd+1
  df$tid[i]=df$tid[i-1]+dt
  i=i+1
   toggle = T
  vec_perm = NF(A,V,dt,rec,J,conc) #funktionen 'NF' med de rigtige argumenter returnerer en vektor af værdier for permeatet af en NF filtrering, som så skal smide tilbage i køletånet
  g=1
  

}
nf$tid=df$tid
cf$tid=df$tid

########## plot #############
cf.long = cf %>% 
  gather(key,value,Na,Cl,SO4,SiO2,Ca)
df.long = df %>% 
  gather(key,value,V_CT,con)

level_order = c('Ca','Cl','Na','SO4','SiO2')
ggplot(cf.long,aes(x=tid,y=value,color=factor(key,level=level_order)))+geom_line()+
  scale_color_brewer(palette="Set1",labels = level_order)+
  theme_bw()+labs(y = "Concentration [mM]", x = "Time [days]", color = "Ion")

ggplot(df,aes(x=tid,y=con))+geom_line()+
  scale_color_brewer(palette="Set1")+
  theme_bw()+labs(y = "Conductivity [uS/cm]", x = "Time [days]")+ylim(c(0,NA))
para(num_bd)
