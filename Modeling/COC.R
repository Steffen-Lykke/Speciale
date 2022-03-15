######## Pakker ###### 
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

######


ion_data = data.frame(
  ion = c("Na", "Cl", "SO4", "SiO2","Ca","HCO3"),
  Mw=c(23,35,96,60,40,61),
  molar_con = c(50.1, 76.4, 160, NA,119,NA),# Molær konduktivitet[S*cm^2*mol^-1]
  limit = c(NA,250,250,150,150,NA),
  MU_concentration_mgL = c(95,21,48,25,1.2,210)
)

conductvity_max = 1200






#### Normal Operation (no filtration)
ion_data=ion_data%>%mutate(MU_concentration_mmol=MU_concentration_mgL/Mw)
ion_data=ion_data%>%mutate(CF = limit/MU_concentration_mgL)
