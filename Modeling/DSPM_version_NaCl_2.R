###### Setup ######
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
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
#library(naniar)
library(readxl)
require(readr)
library(zoo)
library(latex2exp)
library(fs)
library(plyr)
library(matrixStats)
library(haven)
library(sjmisc)
require(gridExtra)
require(ggpubr)
library(beepr)

colors = c(
  '#1f77b4',  # muted blue
  '#ff7f0e',  # safety orange
  '#2ca02c',  # cooked asparagus green
  '#d62728',  # brick red
  '#9467bd',  # muted purple
  '#8c564b',  # chestnut brown
  '#e377c2',  # raspberry yogurt pink
  '#7f7f7f',  # middle gray
  '#bcbd22',  # curry yellow-green
  '#17becf'   # blue-teal
)


                            ##### Konstanter #####
E_b = 78.4
E_la = 31
delta_DE = 0.28*10^-9 #nm
e = 1.602*10^-19 #C
E_0 = 8.85*10^-12 #F m^-1
k_B = 1.3806*10^-23 #J/mol?
Temp=25 #C
Temp=Temp+273.15 #K
Faraday = 96485 #C/mol
R_gas = 8.314 #J/K/mol
viscosity = 8.9*10^-9 # bar*S

                            ##### Parameters #####
    ##Membrane :
rp=2*10^-9 #nm
Le = 1800*10^-9 #nm
sigma=-0.0/1000 #Cm^2
X=(2*sigma)/(rp*Faraday)/1000#mol/L

## Operation
P=2.5 #bar
Temp=298 #kelvin

    ##Solutes
ion_data = data.frame(
  ion = c("Na", "Cl", "SO4", "SiO2","Ca","HCO3"),
  Mw=c(23,35,96,60,40,61),
  molar_con = c(50.1, 76.4, 160, NA,119,NA),# Molær konduktivitet[S*cm^2*mol^-1]
  stokes=c(1.84,1.21,2.3,1.84,3.1,NA)*10^-10,
  pauling=c(0.95,1.81,2.9,NA,0.99,NA)*10^-10,
  hydrated=c(3.58,3.32,3.82,3.58,3.12,NA)*10^-10,
  z=c(1,-1,-2,-1,2,-1),
  Diff=c(1.33*10^-9,2.03*10^-9,1.065*10^-9,1.33*10^-9,0.792*10^-9,NA)
)

feed = data.frame(
  ion = c("Na", "Cl", "SO4", "SiO2","Ca","HCO3"),
  concentration = c(3*10^-3,
                    3*10^-3,
                    1.4*10^-3,
                    1.6*10^-3,
                    0.77*10^-3,NA)
)
#uden sio2 and bicarb
ion_data=ion_data[c(1,2),]
feed=feed[c(1,2),] 
feed$concentration=feed$concentration




                                ##### Prep ####
#radius="stokes"
radii = ion_data%>%select(c(ion,"stokes","pauling","hydrated",z))
##bruges til DE exclusion
E_p=E_la+(E_b-E_la)*(1-(delta_DE/rp))^2



radius_diff="stokes"
lambda = (radii%>%select(radius_diff)/rp)


#dphi(x)=sum((z*J_v/D_P)*K_C*c-c_p)/((F/RT)*sum(z^2*c)) #potential gradient sum er fra i=1 til n

# if (lambda > 0.95) {
#   K_d = 0.984*((1-lambda)/lambda)^(5/2)
# }else{
#   K_d=(1+(9/8)*lambda*log(lambda)-1.56*lambda+0.53*lambda^2+1.95*lambda^3-2.82*lambda^4+0.27*lambda^5+1.1*lambda^6-0.44*lambda^7)/(1-lambda)
#   
#   }
K_d=(1+(9/8)*lambda*log(lambda)-1.56*lambda+0.53*lambda^2+1.95*lambda^3-2.82*lambda^4+0.27*lambda^5+1.1*lambda^6-0.44*lambda^7)
K_a=(1+3.867*lambda-1.907*lambda^2-0.834*lambda^3)/(1+1.867*lambda-0.741*lambda^2)*(1-lambda)^2
ion_data$kd=K_d
ion_data$ka=K_a


data = data.frame(cp_guess=feed$concentration*c(0.2,0.2))
#cp_guess
rownames(data)=c("Na","Cl")


                            ##### Modeling start #####

  

                      ###### Concentration Polarization ######
#det må komme senere eller aldrig. 


                      ###### Steric exclusion #####
steric=(1-(radii$stokes/rp))^2
data$steric = steric

                       ###### Dielectric exclusion #####

dW = (radii$z^2*e^2)/(8*pi*E_0*radii$hydrated)*(1/E_p-1/E_b)
DE = exp(-dW/(k_B*Temp))
data$DE = DE
                      ###### Donnan exclusion#####

#med de rigtige udtryk for ion data
#f = function (x) x^ion_data$z[1]*phi_S_Na*phi_DE_Na*c_Na*z_Na + x^z_Cl*phi_S_Cl*phi_DE_Cl*c_Cl*z_Cl + x^z_SO4*phi_S_SO4*phi_DE_SO4*c_SO4*z_SO4 + x^z_Ca*phi_S_Ca*phi_DE_Ca*c_Ca*z_Ca + X

#NaCl
f1 = function (x) x^ion_data$z[1]*data$steric[1]*data$DE[1]*feed$concentration[1]*ion_data$z[1]+ 
                  x^ion_data$z[2]*data$steric[2]*data$DE[2]*feed$concentration[2]*ion_data$z[2]+
                  X

Donnan_ind = uniroot(f1,c(0,1000))$root
Donnan_ind

#cm=feed$concentration*(Donnan_ind^ion_data$z) #some times better results
cm=feed$concentration*(Donnan_ind^ion_data$z)*steric*DE    #idk man 
data$cm=cm

                          ######## ENP ########
g=0
good_cp=F
while (good_cp==F) {

osmotisk=R_gas*10^-2*Temp*(sum(feed$concentration)-sum(data$cp_guess))

J_volumen = ((rp)^2/(8*viscosity*Le)*(P-osmotisk))  #m/S

##### Control Volume Approach

dybde=1000
var_kon=1
var_diff=1
var_potential=2
N=10 #antal stykker af membran
 dx = (Le)/N#længde af stykker
 dn = 0.00001 #den virker med 0.00001 


vec=c("n","j.0")
for (number in 1:N) {
  name=paste("j.",(as.character(number)),sep="")
  vec=append(vec,name)
}
vec=append(vec,"j.inf")

ini_values=c(rep(0,length(vec)))



ENP_df=data.frame(t(ini_values))
colnames(ENP_df)=vec
ENP_df[2:(dybde),]=NA
ENP_df$j.inf[1:(dybde)]=0
ENP_df_Na=ENP_df_Cl=pot=ENP_df

ENP_df_Na[1:(dybde),2]=cm[1]
ENP_df_Cl[1:(dybde),2]=cm[2]



#pot=-((log(data$cp_guess/feed$concentration)*R_gas*Temp)/(ion_data$z*Faraday))
pot[1:dybde,2]=0
pot[1,-c(1,2,ncol(pot))]=X


total_df=list(ENP_df_Na,ENP_df_Cl)
n=2
while (n<=dybde) {
  for (ion in 1:length(ion_data$ion)) {
   ENP_df=total_df[[ion]]
   
    for (j in 1:N+2) {
      ENP_df[n,j]=var_kon*(ENP_df[n-1,j]-J_volumen*K_a[ion,]*(dn/dx)*(ENP_df[n-1,j]-ENP_df[n-1,j-1]))+var_diff*(
        ion_data$Diff[ion]*ion_data$kd[ion,]*(dn/(dx^2))*(ENP_df[n-1,j+1]-2*ENP_df[n-1,j]+ENP_df[n-1,j-1]))+
        var_potential*(((ion_data$z[ion]*ENP_df[n-1,j]*ion_data$Diff[ion]*Faraday)/(R_gas*Temp))*dn/(dx)^2)*(pot[n-1,j+1]-2*pot[n-1,j]+pot[n-1,j-1])
      
    }
   total_df[[ion]]=ENP_df
   for (j in 1:N+2) {
     pot[n,j]=ion_data$z[1]*total_df[[1]][[c(j,n)]]+
             ion_data$z[2]*total_df[[2]][[c(j,n)]]+
             X
   }
  }
  total_df[[1]][["n"]][n]=total_df[[1]][["n"]][n-1]+dn
  n=n+1
}

c_N=vector()
for (i in 1:nrow(ion_data)) {
  c_N[i]=total_df[[i]][[c(ncol(total_df[[i]])-1,nrow(total_df[[i]]))]]
}
# if (any(c_N<0)) {
#   print("Error Negative Concentrations")
#   break
# }
# if (any(is.nan(c_N))) {
#   print("Error No Number")
#   break
# }
# if (any(is.na(c_N))) {
#   print("Error No Data")
#   break
#}
####### Donnan ud af membran #####
f2 = function (x) (x^ion_data$z[1])*c_N[1]*ion_data$z[1]+ 
                  (x^ion_data$z[2])*c_N[2]*ion_data$z[2] #husk cm

Donnan_ud = uniroot(f2,c(0,100))$root
Donnan_ud

cp=c_N*Donnan_ud^ion_data$z
if (any(cp<0)) {
  print("Error Negative Concentrations")
  break
}
if (any(is.nan(cp))) {
  print("Error No Number")
  break
}
#### noget med at tjekke cp #####
err=sum((cp-data$cp_guess)^2)
if (err<1*10^-29) {
  good_cp = T
}
data$cp_guess = cp
g=g+1
print(g)
print(err)
}

rejection = 1-(cp/feed$concentration)
print(rejection)




##Plotting of CVM
Nth.retain<-function(dataframe, n)dataframe[(seq(1,to=nrow(dataframe),by=n)),]

plot_Na=t(Nth.retain(total_df[[1]],nrow(total_df[[1]])/100)[,-c(1,2,length(total_df[[1]]))])
plot_Cl=t(Nth.retain(total_df[[2]],nrow(total_df[[2]])/100)[,-c(1,2,length(total_df[[2]]))])

par(mfrow=c(1,2))
matplot(plot_Na, type = "l",ylab="Koncentration [M]",xlab="Membran Stykke")
matplot(plot_Cl, type = "l",ylab="Koncentration [M]",xlab="Membran Stykke")
par(mfrow=c(1,1))


ENP_c=data.frame(rbind(total_df[[1]][nrow(total_df[[1]]),-c(1,2,length(total_df[[1]]))],
                       total_df[[2]][nrow(total_df[[2]]),-c(1,2,length(total_df[[2]]))]))
all_c=data.frame(cbind(feed$concentration,cm,ENP_c,cp))
all_c=rbind(all_c,1:length(all_c))
all_c=rbind(all_c,c(0,rep(X,length(all_c)-2),0))

all_c=data.frame(t(all_c))
colnames(all_c)=c("Na","Cl","pos","X")
all_c=all_c%>%mutate(ladning=(Na-Cl)+X)
plot_all=all_c%>%gather(key="key",value="value",Na,Cl)

  ggplot(plot_all,aes(x=pos,y=value*10^3,color=key))+geom_point()+geom_line()+
    scale_color_manual(labels = ioner, values =colors2)+ylab("Concentration [mM]")+xlab("Position")+
    geom_vline(xintercept=c(2,N+2),linetype = "longdash")+
    geom_line(aes(pos, ladning*10^3, color="total charge"), all_c,color="black")+theme(legend.text.align=0)+
    scale_x_continuous(breaks = c(1,2,12,13),labels=c("cf","c0","cN","cp"))+labs(colour = "Species")


ioner=c(expression(Cl^{textstyle("-")}),expression(Na^{textstyle("+")}))
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


  # ggplot(plot_all,aes(x=pos,y=value*10^3,color=key))+geom_point()+geom_line()+
  #   scale_color_manual(labels = ioner, values =colors2)+labs(colour = "Species")+
  #   theme(legend.text.align=0)+ylab("Concentration [mM]")+xlab("Position")+
  #   geom_vline(xintercept=c(2,N+2),linetype = "longdash")+scale_x_continuous(breaks = c(1,2,12,13),labels=c("cf","c0","cN","cp"))
  # #+    ylim(0,0.26)

ggplotly(
  ggplot(plot_all,aes(x=pos,y=value*10^3,color=key))+geom_point()+geom_line()+
    scale_color_manual(labels = ioner, values =colors2)+ylab("Concentration [mM]")+xlab("Position")+
    geom_vline(xintercept=c(2,N+2),linetype = "longdash")+
    geom_line(aes(pos, ladning*10^3, color="total charge"), all_c,color="black")+theme(legend.text.align=0)+
    scale_x_continuous(breaks = c(1,2,12,13),labels=c("cf","c0","cN","cp"))+labs(colour = "Species")
  
)

