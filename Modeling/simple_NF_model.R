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

############# Operational Parameters ###############
c_nacl_initial = (187/35)*10^-3 #mol/L
flux_LMH = 20 #
flux = flux_LMH/3600 #L m^-2 s^-1 


############### System Parameters #################
## Membrane properties ##
membrane_area = 0.05 #m^2
R_na = 0.2 #Rejection Na+
R_cl=0.2 #Rejection Cl-

## Feed values ##
feed_tank_volume = 10 #L
feed_tank_mass = feed_tank_volume*c_nacl_initial #mol?
feed_tank_conc = feed_tank_mass/feed_tank_volume
Q_feed = 0.01933 # L/s --- svarer til 1160 mL/min

permeate_tank_volume = 0 #L
permeate_tank_mass = 0#mol
permeate_tank_conc = NA #Burde dette ikke være permeat_tank_mass/permeat_tank_volume
############# model parameters #############
dt=60 #s
run_time = 8 #hours
max_time_step = run_time*3600
n_time_step = max_time_step/dt
tid = 0

df = data.frame(
  tid=double(),  
  feed_tank_volume=double(),
  feed_tank_mass=double(),
  feed_tank_conc=double(), 
  permeate_tank_volume=double(),
  permeate_tank_mass=double(),
  permeate_tank_conc=double(), 
  permeate_conc=double(), 
  retentate_conc=double(),
  stringsAsFactors = FALSE
    )
df[1:n_time_step,]=0
df[1,] = c(tid,feed_tank_volume,feed_tank_mass,feed_tank_conc,permeate_tank_volume,
           permeate_tank_mass,permeate_tank_conc,0,0)

############ Model #####################
for (i in 2:n_time_step+1) {
  #Volume flow
  Q_permeate = membrane_area*flux
  Q_retentate = Q_feed- Q_permeate
  
  df$feed_tank_volume[i] = df$feed_tank_volume[i-1] - Q_permeate*dt
  df$permeate_tank_volume[i] = df$permeate_tank_volume[i-1] + Q_permeate*dt
  
  #Flow concentration
  df$retentate_conc[i] = (Q_retentate*df$feed_tank_conc[i-1]+Q_permeate*df$feed_tank_conc[i-1]*R_na)/Q_retentate
  df$permeate_conc[i]=(1-R_na)*(df$feed_tank_conc[i-1]+df$retentate_conc[i-1])/2
  
  #Mass flow
  df$feed_tank_mass[i] = df$feed_tank_mass[i-1] - (Q_permeate*df$permeate_conc[i-1])*dt
  df$permeate_tank_mass[i] = df$permeate_tank_mass[i-1] +(Q_permeate*df$permeate_conc[i-1])*dt
  
  
  #Current concentration
  df$feed_tank_conc[i] = df$feed_tank_mass[i-1]/df$feed_tank_volume[i-1]
  df$permeate_tank_conc[i] = df$permeate_tank_mass[i-1]/df$permeate_tank_volume[i-1]
  
  df$tid[i] = df$tid[i-1] + dt 
  i=i+1
}
rm(feed_tank_volume,feed_tank_mass,feed_tank_conc,permeate_tank_volume,
   permeate_tank_mass,permeate_tank_conc)
########## plot #############
df.long = df %>% 
  gather(key,value, feed_tank_conc,permeate_tank_conc)
  
fig=ggplot(df.long,aes(x=tid/3600,y=value*10^3,color=key))+geom_line()+
  scale_color_brewer(palette="Set1",labels = c("Feed Tank", "Permeate Tank"))+
  theme_bw()+labs( y = "Concentration [mM]", x = "Time [h]", color = "")


ggplotly(fig)
