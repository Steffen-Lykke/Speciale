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
delta = 0.28 #nm
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
rp=0.5 #nm
Le = 2000 #nm
sigma=-1.2#mC m^-2
delta_x=5


    ##Solutes
ion_data = data.frame(
  ion = c("Na", "Cl", "SO4", "SiO2","Ca","HCO3"),
  Mw=c(23,35,96,60,40,61),
  molar_con = c(50.1, 76.4, 160, NA,119,NA),# Molær konduktivitet[S*cm^2*mol^-1]
  stokes=c(1.84,1.21,2.3,NA,3.1,NA)*0.1,
  pauling=c(0.95,1.81,2.9,NA,0.99,NA)*0.1,
  hydrated=c(3.58,3.32,3.82,NA,3.12,NA)*0.1,
  z=c(1,-1,-2,-1,2,-1),
  Diff=c(1.33*10^-9,2.03*10^-9,NA,NA,NA,NA)
)

feed = data.frame(
  ion = c("Na", "Cl", "SO4", "SiO2","Ca","HCO3"),
  concentration = c(50*10^-3,50*10^-3,5*10^-3,1.25*10^-3,0.5*10^-3,NA)
)
#Kun NaCl
ion_data=ion_data[1:2,]
feed=feed[1:2,] 
   ## Operation
P=10 #bar
Temp=298

                                ##### Prep ####
radius="stokes"
radii = ion_data%>%select(c(ion,radius,z))
X=-10*10^-3
#X=(2*sigma/1000)/(rp*10^-9*Faraday)#mmol

E_p=E_la+(E_b-E_la)*(1-(delta/rp))^2


#c_0 = (y_m*c_m*phi_S*phi_DE*phi_Don)/y_0



lambda = (1-radii[,2]/rp)

#dphi(x)=sum((z*J_v/D_P)*K_C*c-c_p)/((F/RT)*sum(z^2*c)) #potential gradient sum er fra i=1 til n

# if (lambda > 0.95) {
#   K_d = 0.984*((1-lambda)/lambda)^(5/2)
# }else{
#   K_d=(1+(9/8)*lambda*log(lambda)-1.56*lambda+0.53*lambda^2+1.95*lambda^3-2.82*lambda^4+0.27*lambda^5+1.1*lambda^6-0.44*lambda^7)/(1-lambda)
#   
#   }
K_d=(1+(9/8)*lambda*log(lambda)-1.56*lambda+0.53*lambda^2+1.95*lambda^3-2.82*lambda^4+0.27*lambda^5+1.1*lambda^6-0.44*lambda^7)/(1-lambda)

K_a=(1+3.867*lambda-1.907*lambda^2-0.834*lambda^3)/(1+1.867*lambda+0.741*lambda^2)
ion_data$kd=K_d
ion_data$ka=K_a

#c_perm = (y_N*c_N*phi_S*phi_DE*phi_Don)/y_perm


data = data.frame(cp_guess=feed$concentration*0.2)
rownames(data)=c("Na","Cl")



                            ##### Modeling start #####

                      ###### Concentration Polarization ######
#det må komme senere


                      ###### Steric exclusion #####
steric=(1-(radii[,2]/rp))^2
data$steric = steric

                       ###### Dielectric exclusion #####

dW = (radii$z^2*e^2)/(8*pi*E_0*radii[,2]*10^-9)*(1/E_p-1/E_b)
DE = exp(-dW/(k_B*Temp))
data$DE = DE
                      ###### Donnan exclusion#####


#Manual (solver) solution
# x = 1
# err = 1
# while (err>0.001) {
#   y=x^z_Na*phi_S_Na*phi_DE_Na*c_Na*z_Na + x^z_Cl*phi_S_Cl*phi_DE_Cl*c_Cl*z_Cl + 
#     x^z_SO4*phi_S_SO4*phi_DE_SO4*c_SO4*z_SO4 + x^z_Ca*phi_S_Ca*phi_DE_Ca*c_Ca*z_Ca + X
#   err = abs(y)
#   x = x+0.00001
# }

#Using Uniroot
##Set up function and find current donnan potential
#f = function (x) x^z_Na*phi_S_Na*phi_DE_Na*c_Na*z_Na + x^z_Cl*phi_S_Cl*phi_DE_Cl*c_Cl*z_Cl + x^z_SO4*phi_S_SO4*phi_DE_SO4*c_SO4*z_SO4 + x^z_Ca*phi_S_Ca*phi_DE_Ca*c_Ca*z_Ca + X

#med de rigtige udtryk for ion data
#f = function (x) x^ion_data$z[1]*phi_S_Na*phi_DE_Na*c_Na*z_Na + x^z_Cl*phi_S_Cl*phi_DE_Cl*c_Cl*z_Cl + x^z_SO4*phi_S_SO4*phi_DE_SO4*c_SO4*z_SO4 + x^z_Ca*phi_S_Ca*phi_DE_Ca*c_Ca*z_Ca + X

#NaCl
f = function (x) x^ion_data$z[1]*data$steric[1]*data$DE[1]*feed$concentration[1]*ion_data$z[1] + x^ion_data$z[2]*data$steric[2]*data$DE[2]*feed$concentration[2]*ion_data$z[2]+ X

Donnan = uniroot(f,c(0,100))$root
Donnan

cm=feed$concentration*Donnan^ion_data$z*steric*DE
data$cm=cm

                          ######## ENP ########
#J=J_v*c_perm # Simpel flux for ioner

#J_diff=-K_d*D_infty*dc/dx
#J_convec = K_a*c*J_v #Convection term for flux through pore
#J_electro = (z*c*K_d*D_infty*F)/R*T*dphi(x)/dx
#J_ENP=J_diff+J_convec-J_electro

osmotisk=R_gas*10^-2*Temp*(sum(feed$concentration)-sum(data$cp_guess))

J_volumen = ((rp*10^-9)^2*(P-osmotisk))/(8*viscosity*Le*10^-9)  #m/S

data$c_i_1=data$cm
#laaaangt udtryk hvor vi gerne vil løse for c_i
#c_i skal være en vector. 
#c_i_na=3
#c_i_cl=3

#d_potential=((ion_data$z[1]*J_volumen/ion_data$Diff[1])*(ion_data$ka[1]*(c_i_na)-data$cp_guess[1]/1000))/((Faraday/R_gas*Temp)*(ion_data$z[1]^2*c_i_na))+
#  ((ion_data$z[2]*J_volumen/ion_data$Diff[2])*(ion_data$ka[2]*(c_i_cl)-data$cp_guess[2]/1000))/((Faraday/R_gas*Temp)*(ion_data$z[2]^2*c_i_cl))


#data$cp_guess[1]*J_volumen = 
#  (-ion_data$kd[1]*ion_data$Diff[1]*((data$c_i_1[1]-c_i_na)/(Le*10^-9/delta_x)))+
#                        (ion_data$ka[1]*c_i_na*J_volumen)-
#                        ((ion_data$z[1]*c_i_na*ion_data$kd[1]*ion_data$Diff[1]*Faraday)/(R_gas*Temp))*
#  (((ion_data$z[1]*J_volumen/ion_data$Diff[1])*(ion_data$ka[1]*(c_i_na)-data$cp_guess[1]/1000))/((Faraday/R_gas*Temp)*(ion_data$z[1]^2*c_i_na))+
#                                                                                             ((ion_data$z[2]*J_volumen/ion_data$Diff[2])*(ion_data$ka[2]*(c_i_cl)-data$cp_guess[2]/1000))/((Faraday/R_gas*Temp)*(ion_data$z[2]^2*c_i_cl))
#                        )
##### Control Volume Approach

dybde=10000
var_kon=1
var_diff=1
var_potential=1
Xd=-10*10^-3
N=10 #antal stykker af membran
 dx = (Le*10^-9)/N#længde af stykker
 dn = 0.00001

#Fra Excel
 # dx = 0.005
 # dn = 13

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
ENP_df_Na=ENP_df
ENP_df_Cl=ENP_df
pot=ENP_df
ENP_df_Na[1:(dybde),2]=cm[1]
ENP_df_Cl[1:(dybde),2]=cm[2]
pot[1:dybde,2]=0

total_df=list(ENP_df_Na,ENP_df_Cl)
n=2
while (n<=dybde) {
  for (ion in 1:length(ion_data$ion)) {
   ENP_df=total_df[[ion]]
   
    for (j in 1:N+2) {
      ENP_df[n,j]=var_kon*(ENP_df[n-1,j]-J_volumen*K_a[ion]*(dn/dx)*(ENP_df[n-1,j]-ENP_df[n-1,j-1]))+var_diff*(
        ion_data$Diff[ion]*ion_data$kd[ion]*(dn/(dx^2))*(ENP_df[n-1,j+1]-2*ENP_df[n-1,j]+ENP_df[n-1,j-1]))+
        var_potential*(((ion_data$z[ion]*ENP_df[n-1,j]*ion_data$Diff[ion]*Faraday)/(R_gas*Temp))*dn/(dx)^2)*(pot[n-1,j+1]-2*pot[n-1,j]+pot[n-1,j-1])
     
    }

   total_df[[ion]]=ENP_df
   for (j in 1:N+2) {
     pot[n,j]=ion_data$z[1]*total_df[[1]][[c(j,n)]]+ion_data$z[2]*total_df[[2]][[c(j,n)]]+Xd
   }    
  }
  total_df[[1]][["n"]][n]=total_df[[1]][["n"]][n-1]+dn
  n=n+1
}

##Plotting of CVM
Nth.retain<-function(dataframe, n)dataframe[(seq(1,to=nrow(dataframe),by=n)),]

plot_Na=t(Nth.retain(total_df[[1]],nrow(total_df[[1]])/100)[,-c(1,2,length(total_df[[1]]))])
plot_Cl=t(Nth.retain(total_df[[2]],nrow(total_df[[2]])/100)[,-c(1,2,length(total_df[[2]]))])

par(mfrow=c(1,2))
matplot(plot_Na, type = "l",ylab="Koncentration [M]",xlab="Membran Stykke")
matplot(plot_Cl, type = "l",ylab="Koncentration [M]",xlab="Membran Stykke")
par(mfrow=c(1,1))

c_na=total_df[[1]][[c(ncol(total_df[[1]])-1,nrow(total_df[[1]]))]]
c_cl=total_df[[2]][[c(ncol(total_df[[2]])-1,nrow(total_df[[2]]))]]

