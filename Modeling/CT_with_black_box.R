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
#source("Modeling/simple_NF_model_multi_ion.R")


##### CT parameters #####
V_CT = 8 # m^3 | Hvad er reservoir volumet
Q_vap = 2 #m^3 / day | Hvor meget fordamper
Q_blowdown = 1 #m^3 /d hvor meget fjernes fra resevoiret itf. af blowdown
V_BD=1
c_makeup = c(
  4.1, #Na
  0.6, #Cl
  0.5, #SO4
  0.45, #SiO2
  0.03  #Ca
) #En vektor med de forskellige koncentrationer [mM]


ion_values = data.frame(
  Ions = c("Na", "Cl", "SO4", "SiO2","Ca"),
  value = c(NA, 7.1, 2.5, 2.6,2),#Grænseværdier [mM=mol/m^3]
  molar_con = c(50.1, 76.4, 160, NA,119)
)#[S*cm^2*mol^-1]

c_guideline = ion_values[,2] #Vektor med grænseværdier for ioner
con_ini = sum(c_makeup*ion_values[,3],na.rm=T)
con=con_ini
con_lim = 1500#conductivity grænseværdi [uS/cm]

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
dt_timer=1 #tidsstep i timer
dt=dt_timer/24 #timer i dage
run_time = 90 #Total operating time i dage
max_time = run_time*24 #i timer
n_time_step = run_time/dt #antal tidsskridt 
start_tid = 0 # start tid?
drift = 0 #en drift factor der har noget med om ioner kommer med evaporation
recovery_factor_volume = 0.05 #Hvor meget makeup skal CT tage ind når der laves BD til batch  


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
vandforbrug=vector()
vand_NF=vector()
vand_BD=vector()
vand_vap=vector()

#Initial Values
df[0:n_time_step+1,]=0
nf[0:n_time_step+1,]=0
cf[0:n_time_step+1,]=0
vandforbrug[0:n_time_step+1]=0
vand_NF[0:n_time_step+1]=0
vand_BD[0:n_time_step+1]=0
vand_vap[0:n_time_step+1]=0

df[1,]=c(start_tid,V_CT,con_ini)
cf[1,]=c(start_tid,c_makeup)
nf[1,]=cf[1,]*df$V_CT[1]
num_nf=0
i=2
toggle = F
V_NF1=0.9#antal m^3 filtreret ad gangen
V_NF2=V_NF1*0.77777 #hvor meget vand der kommer tilbage afhægit af recovery

Q_vap = Q_vap*dt #Hvor meget fordamper per tidsskridt m^3/dt(dage)

num_bd=0
Rej=data.frame(
  Na  =  0.38419491,
  Cl=-0.30411843,
  SO4=0.78812522,
  SiO2=  0.27315914,
  Ca  =  0.26783120
)
###### CT Model ######
while(i < n_time_step){
  while(con < con_lim){ ##### CT OPERATION #######
    if (i>n_time_step) {
      break
    }
    if (cf$Cl[i-1]>=5) {
      nf[i,2:6] = cf[i-1,2:6]*V_CT-V_BD*cf[i-1,2:6]+V_BD*c_makeup+Q_makeup*c_makeup
      cf[i,2:6] = nf[i,2:6]/df$V_CT[i]  
      con=sum(cf[i,2:5]*ion_values[,3],na.rm=T)
      df$con[i] = con
      num_bd=num_bd+1
      vandforbrug[i]=vandforbrug[i-1]+V_BD+Q_vap
      vand_NF[i]=vand_NF[i-1]
      vand_BD[i]=vand_BD[i-1]+V_BD
      vand_vap[i]=vand_vap[i-1]+Q_vap
      df$tid[i]=df$tid[i-1]+dt
      nf$tid[i]=df$tid[i]
      cf$tid[i]=df$tid[i]
      i=i+1
    }
    if (i>n_time_step) {
      break
    }
    ##Volume flow
    Q_makeup = Q_vap# hvor meget vand skal ind i systemet
    Q = Q_makeup-Q_vap
    df$V_CT=df$V_CT[i-1]+Q
    vandforbrug[i]=vandforbrug[i-1]+Q_makeup
    ## mass flow ##
    n_mu = Q_makeup*c_makeup
    n_vap = Q_vap*cf[i-1,2:6]*drift
    n_flow=n_mu-n_vap
    
    
    ## Current Concentration & Conductivity ##
    nf[i,2:6] = nf[i-1,2:6]+n_flow #mol
    cf[i,2:6] = nf[i,2:6]/df$V_CT[i]   #mol/m^3
    con=sum(cf[i,2:6]*ion_values[,3],na.rm=T)
    df$con[i] = con
    
    df$tid[i]=df$tid[i-1]+dt
    nf$tid[i]=df$tid[i]
    cf$tid[i]=df$tid[i]
    vandforbrug[i]=vandforbrug[i-1]+Q_vap
    vand_NF[i]=vand_NF[i-1]
    vand_BD[i]=vand_BD[i-1]
    vand_vap[i]=vand_vap[i-1]+Q_vap
    i=i+1
    

  } ##### FILTRERING #####
  c_NF=cf[i-1,2:6]*(-Rej[1:5]+1)#få permeat koncentration af filtreringen
  nf[i,2:6] = nf[i-1,2:6]-cf[i-1,2:6]*(V_NF1)+c_NF*V_NF2+(V_NF1-V_NF2)*c_makeup+Q_makeup*c_makeup
  cf[i,2:6] = nf[i,2:6]/df$V_CT[i]  
  con=sum(cf[i,2:5]*ion_values[,3],na.rm=T)
  df$con[i] = con
  vandforbrug[i]=vandforbrug[i-1]+(V_NF1-V_NF2)+Q_vap
  vand_NF[i]=vand_NF[i-1]+(V_NF1-V_NF2)
  vand_BD[i]=vand_BD[i-1]
  vand_vap[i]=vand_vap[i-1]+Q_vap
  num_nf=num_nf+1
  df$tid[i]=df$tid[i-1]+dt
  nf$tid[i]=df$tid[i]
  cf$tid[i]=df$tid[i]
  i=i+1
  
}

#####
water_bd_NF=num_nf*(V_NF1-V_NF2)
df$tid[nrow(df)]

total_vand=data.frame(time=df$tid,
                      water=vandforbrug,
                      water_NF=vand_NF,
                      water_BD=vand_BD,
                      water_evap=vand_vap)

COC_water=(vand_vap[length(vand_vap)]+vand_BD[length(vand_BD)]+vand_NF[length(vand_NF)])/(vand_BD[length(vand_BD)]+vand_NF[length(vand_NF)])


########## plot #############
cf.long = cf %>% 
  gather(key,value,Na,Cl,SO4,SiO2,Ca)
df.long = df %>% 
  gather(key,value,V_CT,con)
vand.long = total_vand %>% 
  gather(key,value,water_evap,water_NF,water_BD)

level_order = c('Ca','Cl','Na','SiO2','SO4')


ggplotly(
  ggplot(df,aes(x=tid,y=con))+geom_line()+
  scale_color_brewer(palette="Set1")+
  theme_bw()+labs(y = "Conductivity [uS/cm]", x = "Time [days]")+ylim(c(0,NA))
)
ggplotly(
  ggplot(cf.long,aes(x=tid,y=value,color=key))+geom_line()+
    scale_color_brewer(palette="Set1",labels = level_order)+
    theme_bw()+labs(y = "Concentration [mM]", x = "Time [days]", color = "Ion")+
    geom_hline(aes(yintercept=as.numeric(ion_values%>%filter(Ions=="Ca")%>%select(value))),linetype='dashed',color="red")+
    geom_hline(aes(yintercept=as.numeric(ion_values%>%filter(Ions=="Cl")%>%select(value))),linetype='dashed',color="blue")+
    geom_hline(aes(yintercept=as.numeric(ion_values%>%filter(Ions=="SiO2")%>%select(value))),linetype='dashed',color="purple")+
    geom_hline(aes(yintercept=as.numeric(ion_values%>%filter(Ions=="SO4")%>%select(value))),linetype='dashed',color="orange")
)

total_vand%>%plot_ly(x=~time,
        y=~water_evap,
        name="Evaporation",
        fillcolor='#1f77b4',
        type='scatter',
        mode='none',
        stackgroup='one')%>%
  add_trace(y=~water_NF,
            name='Nanofiltration',
            fillcolor='#E41317')%>%
  add_trace(y=~water_BD,
            name='Forced Blowdown',
            fillcolor='#forestgreen')%>%
  layout(title="Water usage CT",
         legend=list(x=0.1,y=0.9),
         yaxis=list(title="Water usage [m^3]"),
         xaxis=list(title="Time [days]"))

#ggplot(vand.long, aes(x=time, y=value))+ geom_area(aes(colour=key, fill=key))+geom_line(aes(time, water_evap, color="total charge"), total_vand,color="black")
