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

recovery=0.93 #1filtreirng#
#recovery=0.78 #"2filtreringer"
c_makeup = c(
  4.1, #Na
  0.6, #Cl
  0.5, #SO4
  0.45, #SiO2
  0.03  #Ca
) #En vektor med de forskellige koncentrationer [mM]

#beskidt vand
#c_makeup = c(
#  4.1, #Na
#  50/35.45, #Cl
#  110/96, #SO4
#  18/60, #SiO2
#  110/40  #Ca
#)

fBD = T #Skal der foregå "forced" blowdown ved grænse værdi? T/F

#Rej=data.frame(
#  Na  =  0.38419491,
#  Cl=0.064,
#  SO4=0.78812522,
#  SiO2=  0.27315914,
#  Ca  =  0.26783120
#)#total rejection af NF system 2 filtreringer. 

 Rej=data.frame(
   Na  =  0.24,
   Cl=-0.068,
   SO4=0.61,
   SiO2=  0.23,
   Ca  =  0.026
 )#pilot scale 1. filtration
ion_values = data.frame(
  Ions = c("Na", "Cl", "SO4", "SiO2","Ca"),
  value = c(NA, 7.1, 2.6, 2.5,2),#Grænseværdier [mM=mol/m^3]
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
run_time = 1000 #Total operating time i dage
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
df[0:n_time_step+1,]=NA
nf[0:n_time_step+1,]=NA
cf[0:n_time_step+1,]=NA
vandforbrug[0:n_time_step+1]=NA
vand_NF[0:n_time_step+1]=NA
vand_BD[0:n_time_step+1]=NA
vand_vap[0:n_time_step+1]=NA

df[1,]=c(start_tid,V_CT,con_ini)
cf[1,]=c(start_tid,c_makeup)
nf[1,]=cf[1,]*df$V_CT[1]
num_nf=0
i=2
V_NF1=1#antal m^3 filtreret ad gangen
V_NF2=V_NF1*recovery #hvor meget vand der kommer tilbage afhægit af recovery

Q_vap = Q_vap*dt #Hvor meget fordamper per tidsskridt m^3/dt(dage)

num_bd=0

vand_BD[1]=0
vand_vap[1]=0
vand_NF[1]=0
vandforbrug[1]=0


###### CT Model ######
while(i < n_time_step){
  while(con < con_lim){ ##### CT OPERATION #######
    if (i>n_time_step) {
      break
    }
    if (cf$Cl[i-1]>=7&fBD==T) {
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


total_vand=data.frame(time=df$tid,
                      water=vandforbrug,
                      water_NF=vand_NF,
                      water_BD=vand_BD,
                      water_evap=vand_vap)

COC_water=(max(vand_vap)+max(vand_BD)+max(vand_NF))/(max(vand_BD)+max(vand_NF))


########## plot #############
cf.long = cf[-nrow(cf),] %>% 
  gather(key,value,Na,Cl,SO4,SiO2,Ca)
df.long = df[-nrow(df),] %>% 
  gather(key,value,V_CT,con)
vand.long = total_vand %>% 
  gather(key,value,water_evap,water_NF,water_BD)

level_order = c('Ca','Cl','Na','SiO2','SO4')


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
ggplot(cf.long,aes(x=tid,y=value,color=key))+geom_line()+
  #scale_color_brewer(palette="Set1")+
  theme_bw()+labs(y = "Concentration [mM]", x = "Time [days]",colour = "Species")+ylim(c(0,NA))+
  geom_hline(aes(yintercept=as.numeric(ion_values%>%filter(Ions=="Ca")%>%select(value))),linetype='dashed',color='#d62728')+
  geom_hline(aes(yintercept=as.numeric(ion_values%>%filter(Ions=="Cl")%>%select(value))),linetype='dashed',color='#1f77b4')+
  geom_hline(aes(yintercept=as.numeric(ion_values%>%filter(Ions=="SiO2")%>%select(value))),linetype='dashed',color='#9467bd')+
  geom_hline(aes(yintercept=as.numeric(ion_values%>%filter(Ions=="SO4")%>%select(value))),linetype='dashed',color='#ff7f0e')+
  scale_color_manual(labels = ioner, values =colors2)+theme(legend.text.align=0)+
  scale_y_continuous(limits=c(0, 21), breaks=c(0,2.5,5,7.5,10,12.5,15,17.5,20 ))+
 # scale_x_continuous(limits=c(0, 365), breaks=c(0,20,40,60,80))+
  ggtitle("1 Filtration")#+theme(legend.position = "top")
     )

#ggplotly(
#  ggplot(cf.long,aes(x=tid,y=value,color=key))+geom_line()+
#    scale_color_brewer(palette="Set1",labels = level_order)+
#    theme_bw()+labs(y = "Concentration [mM]", x = "Time [days]", color = "Ion")+
#    geom_hline(aes(yintercept=as.numeric(ion_values%>%filter(Ions=="Ca")%>%select(value))),linetype='dashed',color="red")+
#    geom_hline(aes(yintercept=as.numeric(ion_values%>%filter(Ions=="Cl")%>%select(value))),linetype='dashed',color="blue")+
#    geom_hline(aes(yintercept=as.numeric(ion_values%>%filter(Ions=="SiO2")%>%select(value))),linetype='dashed',color="purple")+
#    geom_hline(aes(yintercept=as.numeric(ion_values%>%filter(Ions=="SO4")%>%select(value))),linetype='dashed',color="orange")
#)

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
            name='Blowdown',
            fillcolor='#forestgreen')%>%
  layout(legend=list(x=0.1,y=0.9),
         yaxis=list(title="Water usage [m^3]",range=c(0,NA)),
         xaxis=list(title="Time [days]"))

#ggarrange(fil_1_BD,fil_2,common.legend = TRUE,labels = c("1 Filtration","2 Filtrations"))


level_order_2 = c('Evaporation','NF Blowdown','Blowdown')


###### plot af water usage for alle: 

water_usage_df=as.data.frame(c("1Blowdown","3Evaporation","2NF Blowdown"))
water_usage_df$CT=(c(37,180,0))
water_usage_df$NF_1=(c(9,180,7.21))
water_usage_df$NF_2=(c(0,180,14.96))
colnames(water_usage_df)=c("Stream","No Filtration", "1 Filtration","2 Filtrations")
water_usage_df_plot = water_usage_df%>%gather(key,value, "No Filtration", "1 Filtration","2 Filtrations")


sejt_plot_ok=ggplot(water_usage_df_plot, aes(fill=Stream, y=value, x=key)) + 
  geom_bar(position="stack", stat="identity")+
  labs(y = "Water usage [m^3]", x = "")+
  scale_fill_discrete( labels = c("Blowdown", "NF Blowdown", "Evaporation"))+
  theme(legend.position="top")
sejt_plot_ok+scale_fill_manual(values=c('#d62728', '#2ca02c','#1f77b4'   ))
#scale_color_manual(values=c('#999999','#E69F00','#56B4E9'))

