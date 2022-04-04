##### Konstanter #####
J_v
c_p

K_a
K_d

D_infty
RT
lambda = (1-r/r_p)^2
##### Formler #####
#Concentration Polarization

#DSPM-DE
phi_S = (1-r/r_p)^2
phi_DE = exp(-dW/k_B*T)
dW = (z^2*e^2)/(8*pi*epsilon_0*r)*(1/epsilon_b-1/epsilon_p)
phi_Don = exp(- z*F/RT*dphi(x))

c_0 = (y_m*c_m*phi_S*phi_DE*phi_Don)/y_0

#ENP
J=J_v*c_perm # Simpel flux for ioner
J_convec = K_a*c*J_v #Convection term for flux through pore
J_diff=-K_d*D_infty*dc/dx
J_electro = (z*c*K_d*D_infty*F)/R*T*dphi(x)/dx
J_ENP=J_diff+J_convec-J_electro

dphi(x)=sum((z*J_v/D_P)*K_C*c-c_p)/((F/RT)*sum(z^2*c)) #potential gradient sum er fra i=1 til n
K_d=
K_a=
#DSPM-DE? (måske uden steric)

c_perm = (y_N*c_N*phi_S*phi_DE*phi_Don)/y_perm


#### Nyt efter vi er blevet super kloge ########

###### Basic data  ######
ion_data = data.frame(
  ion = c("Na", "Cl", "SO4", "SiO2","Ca","HCO3"),
  Mw=c(23,35,96,60,40,61),
  molar_con = c(50.1, 76.4, 160, NA,119,NA),# Molær konduktivitet[S*cm^2*mol^-1]
  stokes=c(1.84,1.21,2.3,NA,3.1,NA)*0.1,
  pauling=c(0.95,1.81,2.9,NA,0.99,NA)*0.1,
  hydrated=c(3.58,3.32,3.82,NA,3.12,NA)*0.1,
  z=c(1,-1,-2,-1,2,-1),
  Diff=c(NA,NA,NA,NA,NA,NA)
)

feed = data.frame(
  ion = c("Na", "Cl", "SO4", "SiO2","Ca","HCO3"),
  concentration = c(15.25,5,5,1.25,0.5,NA)
)
###### Constants ######
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


###### Membrane Parameters #####
rp=0.5 #nm
Le = 2 #um
sigma=-1.2#mC m^-2


###### Other Variables #####
P=3#bar




###### Prep ####
radius="stokes"
radii = ion_data%>%select(c(ion,radius,z))
X=(2*sigma/1000)/(rp*10^-9*Faraday)#mmol

##### Modeling start #####

###### Steric exclusion #####
steric=(1-(radii[,2]/rp))^2


###### Dielectric exclsion #####
E_p=E_la+(E_b-E_la)*(1-(delta/rp))^2
dW = (radii$z^2*e^2)/(8*pi*E_0*radii[,2]*10^-9)*(1/E_p-1/E_b)
DE = exp(-dW/(k_B*Temp))

###### Donnan exclusion?#####

####### Learning maths in R: #####
fnToFindRoot = function(x) {
  return((x*(x*(x*(x + 15.25) - 50) - 5) - 10)/x^2)
}
l0 = 1
rootSearch = optim(l0, fnToFindRoot,method = 'BFGS', hessian=TRUE)
str(rootSearch)
fnToFindRoot(rootSearch$par)



(x*(x*(x*(x + 15.25) - 50) - 5) - 10)/x^2


