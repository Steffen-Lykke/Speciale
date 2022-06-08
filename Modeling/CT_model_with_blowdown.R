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
Q_blowdown = 1 #m^3 /d hvor meget fjernes fra resevooiret
V_BD=1#m^3 volume til discondinous bd
c_makeup = c(
  4.1, #Na
  0.6, #Cl
  0.5, #SO4
  0.45, #SiO2
  0.03  #Ca
) #En vektor med de forskellige koncentrationer [mM]

#beskidt
#c_makeup = c(
#  4.1, #Na
#  50/35.45, #Cl
#  110/96, #SO4
# 18/60, #SiO2
#  0.03  #Ca
#)



ion_values = data.frame(
  Ions = c("Na", "Cl", "SO4", "SiO2","Ca"),
  value = c(NA, 7.1, 2.5, 2.6,2),#Grænseværdier [mM=mol/m^3]
  molar_con = c(50.1, 76.4, 160, NA,119)
)#[S*cm^2*mol^-1]
c_ini=c(6,2.25,1.88,1.5,0.5)

c_guideline = ion_values[,2] #Vektor med grænseværdier for ioner
#con_ini = sum(c_ini*ion_values[,3],na.rm=T) start med værdier tættere på ss
con_ini = sum(c_makeup*ion_values[,3],na.rm=T)
con=con_ini
con_lim = 1675#conductivity grænseværdi [uS/cm]

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
dt_timer=1 #tidsstep i timer
dt=dt_timer/24 #timer i dage
run_time = 1000 #Total operating time i dage
max_time = run_time*24 #i timer
n_time_step = run_time/dt #antal tidsskridt 
start_tid = 0 # start tid?
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

cf = data.frame(
  tid=double(),
  Na=double(),
  Cl=double(),
  SO4=double(),
  SiO2=double(),
  Ca=double()
)#Dataframe med concentrationer i systemet [mM]

vand_BD=vector()
vand_vap=vector()
c_BD=cf

#Initial Values
df[0:n_time_step+1,]=0
nf[0:n_time_step+1,]=0
cf[0:n_time_step+1,]=0



vand_BD[0:n_time_step+1]=NA
vand_vap[0:n_time_step+1]=NA

df[1,]=c(start_tid,V_CT,con_ini)
cf[1,]=c(start_tid,c_makeup)
nf[1,]=cf[1,]*df$V_CT[1]
num_bd=0
i=2

vand_BD[1]=0
vand_vap[1]=0
Q_vap = Q_vap*dt #Hvor meget fordamper per tidsskridt
#Q_blowdown=(Q_vap/(-1+COC))
###### CT Model ######
while(i <= n_time_step+1){
    while(con < con_lim){
      if (i>n_time_step+1) {
        break
      }
      ##Volume flow
      Q_makeup = Q_vap# hvor meget vand skal ind i systemet
      Q = Q_makeup-Q_vap
      df$V_CT=df$V_CT[i-1]+Q
      ## mass flow ##
      n_mu = Q_makeup*c_makeup
      n_vap = Q_vap*cf[i-1,2:6]*drift
      n_flow=n_mu-n_vap
      
      
      ## Current Concentration & Conductivity ##
      nf[i,2:6] = nf[i-1,2:6]+n_flow
      cf[i,2:6] = nf[i,2:6]/df$V_CT[i]  
      con=sum(cf[i,2:6]*ion_values[,3],na.rm=T)
      df$con[i] = con
      
      df$tid[i]=df$tid[i-1]+dt
      nf$tid[i]=nf$tid[i-1]+dt
      cf$tid[i]=cf$tid[i-1]+dt
      
      vand_BD[i]=vand_BD[i-1]
      vand_vap[i]=vand_vap[i-1]+Q_vap
      i=i+1
    }
  num_bd=num_bd+1
  c_BD[num_bd,2:6]=cf[i-1,2:6]
  c_BD[num_bd,1]=df$tid[i-1]
  nf[i,2:6] = cf[i-1,2:6]*(V_CT-V_BD)+V_BD*c_makeup+Q_makeup*c_makeup
  cf[i,2:6] = nf[i,2:6]/df$V_CT[i]  
  con=sum(cf[i,2:6]*ion_values[,3],na.rm=T)
  df$con[i] = con
  df$tid[i]=df$tid[i-1]+dt
  nf$tid[i]=nf$tid[i-1]+dt
  cf$tid[i]=cf$tid[i-1]+dt
  
  vand_BD[i]=vand_BD[i-1]+V_BD
  vand_vap[i]=vand_vap[i-1]+Q_vap
  i=i+1
}
######## COC beregninger #####
COC_water=(vand_vap[length(vand_vap)]+vand_BD[length(vand_BD)])/(vand_BD[length(vand_BD)])
COC_ion=c_BD[num_bd-1,2:6]/c_makeup
COC_con=con_lim/con_ini
########## plot #############
cf.long = cf %>% 
  gather(key,value,Na,Cl,SO4,SiO2,Ca)
df.long = df %>% 
  gather(key,value,V_CT,con)
plot_data = rbind(cf.long,df.long)%>%filter(key!="V_CT"& key!="con")

total_vand=data.frame(time=df$tid,
                      water_BD=vand_BD,
                      water_evap=vand_vap)

level_order = c('Na','Cl','SO4','SiO2')

ggplotly(
  ggplot(cf.long,aes(x=tid,y=value,color=factor(key,level=level_order)))+geom_line()+
  scale_color_brewer(palette="Set1",labels = level_order)+
  theme_bw()+labs(y = "Concentration [mM]", x = "Time [days]", color = "Ion")
)

ggplotly(
  ggplot(df,aes(x=tid,y=con))+geom_line()+
  scale_color_brewer(palette="Set1")+
  theme_bw()+labs(y = "Conductivity [uS/cm]", x = "Time [days]")+ylim(c(0,NA))
)

ioner=c(expression(Ca^{textstyle("2+")}),expression(Cl^{textstyle("-")}),expression(Na^{textstyle("+")}),expression(SiO[2]),expression(SO[4]^{textstyle("2-")}))
colors2=c( 
  '#d62728',  # brick red
  '#1f77b4',  # muted blue 
  '#2ca02c',  # cooked asparagus green
  '#9467bd',  # muted purple  
  '#ff7f0e',  # safety orange
  '#8c564b',  # chestnut brown
  '#e377c2',  # raspberry yogurt pink
  '#7f7f7f',  # middle gray
  '#bcbd22',  # curry yellow-green
  '#17becf'   # blue-teal
)

ggplotly(
  ggplot(plot_data,aes(x=tid,y=value,color=key))+geom_line()+
  scale_color_brewer(palette="Set1")+
  theme_bw()+labs(y = "Concentration [mM]", x = "Time [days]",colour = "Species")+ylim(c(0,NA))+
    geom_hline(aes(yintercept=as.numeric(ion_values%>%filter(Ions=="Ca")%>%select(value))),linetype='dashed',color='#d62728')+
    geom_hline(aes(yintercept=as.numeric(ion_values%>%filter(Ions=="Cl")%>%select(value))),linetype='dashed',color='#1f77b4')+
    geom_hline(aes(yintercept=as.numeric(ion_values%>%filter(Ions=="SiO2")%>%select(value))),linetype='dashed',color='#9467bd')+
    geom_hline(aes(yintercept=as.numeric(ion_values%>%filter(Ions=="SO4")%>%select(value))),linetype='dashed',color='#ff7f0e')+
    scale_color_manual(labels = ioner, values =colors2)+theme(legend.text.align=0)+
    scale_y_continuous(limits=c(0, 21), breaks=c(0,2.5,5,7.5,10,12.5,15,17.5,20 ))+
    scale_x_continuous(limits=c(0, 90), breaks=c(0,20,40,60,80))
   )


#annotate("text", x = "Feb", y = 40, label = "Previous Level", vjust = -0.5)

total_vand%>%plot_ly(x=~time,
                     y=~water_evap,
                     name="Evaporation",
                     fillcolor='#1f77b4',
                     type='scatter',
                     mode='none',
                     stackgroup='one')%>%
  add_trace(y=~water_BD,
            name='Blowdown',
            fillcolor='#forestgreen')%>%
  layout(title="Water usage CT",
         legend=list(x=0.1,y=0.9),
         yaxis=list(title="Water usage [m^3]"),
         xaxis=list(title="Time [days]"))
