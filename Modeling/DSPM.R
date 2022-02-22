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




