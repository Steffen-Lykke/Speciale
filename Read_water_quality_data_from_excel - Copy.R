#### This scripts reads water quality analysis data from excel file and saves this into and Rdata file
pacman::p_load(readxl, tidyverse)

remove(list=ls())

setwd("C:/Users/37040/Grundfos/Nanofiltration NXF - Cooling tower/Test runs 2020/Water analysis from analytical lab")

#### Get data from Excell and tidy up ####
{
  Analysis_dat <- read_excel("C:/Users/37040/Grundfos/Nanofiltration NXF - Cooling tower/Test runs 2020/Water analysis from analytical lab/NF Pilot_Analyser_Cooling_towers.xlsx", 
                             col_types = c("text","text", "text", "date", 
                                           "date", "date", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", "numeric", 
                                           "numeric","numeric","numeric","numeric","numeric", 
                                           "numeric","numeric","numeric","numeric","numeric", 
                                           "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), skip = 8)
  
  # Remove unnecessary rows and columns
  Analysis_dat <- Analysis_dat[-1,]
  Analysis_dat <- Analysis_dat %>% dplyr::select(-c("TDS", "SS"))
  
  
  #Create column "sampling_time" from existing columns
  Analysis_dat$Sampling <- format(Analysis_dat$Sampling, "%H:%M:%S")
  Analysis_dat$Sampling_Time <- as.POSIXct(paste(Analysis_dat$Sample, Analysis_dat$Sampling), format="%Y-%m-%d %H:%M:%S")
  Analysis_dat <- Analysis_dat %>% dplyr::select(-c("Sample", "Sampling", "Analysis")) #Remove unnecessary columns
  
  # Fill in missing data for the variable "Water_type"
  Analysis_dat <- Analysis_dat %>% fill(Water_type)
  Analysis_dat <- Analysis_dat %>%  rename(Sample_ID = 'Sample ID')
  names(Analysis_dat) <- sub(" .+", "", names(Analysis_dat))
}




#### Read in data for the different ions ####

basic_data_ions <- read_excel("C:/Users/37040/Grundfos/Nanofiltration NXF - Cooling tower/Test runs 2020/Water analysis from analytical lab/basic_data_ions.xlsx",
                              col_types = c("text", "numeric", "skip", 
                                            "skip", "skip"), skip=3)


### Converting from mg/l to mmol for main ions
{
  HCO3_MW <- basic_data_ions$Molar_mass[1]
  F_MW <- basic_data_ions$Molar_mass[2]
  Cl_MW <- basic_data_ions$Molar_mass[3]
  Br_MW <- basic_data_ions$Molar_mass[4]
  NO3_MW <- basic_data_ions$Molar_mass[5]
  SO4_MW <- basic_data_ions$Molar_mass[6]
  PO4_MW <- basic_data_ions$Molar_mass[7]
  Na_MW <- basic_data_ions$Molar_mass[8]
  K_MW <- basic_data_ions$Molar_mass[9]
  Mg_MW <- basic_data_ions$Molar_mass[10]
  Ca_MW <- basic_data_ions$Molar_mass[11]
  Fe_MW <- basic_data_ions$Molar_mass[12]
  SiO2_MW <- basic_data_ions$Molar_mass[13]
}

Analysis_dat <- Analysis_dat %>% 
  mutate("Hydrogencarbonate_mM" = Hydrogencarbonate/HCO3_MW) %>% 
  mutate("Fluoride_mM" = Fluoride/F_MW) %>%
  mutate("Chloride_mM" = Chloride/Cl_MW) %>% 
  mutate("Bromide_mM" = Bromide/Br_MW) %>% 
  mutate("Nitrate_mM" = Nitrate/NO3_MW) %>% 
  mutate("Sulphate_mM" = Sulphate/SO4_MW) %>% 
  mutate("Phosphate_mM" = Phosphate/PO4_MW) %>%
  mutate("Phosphate_test_kit_mM" = Phosphate_test_kit/PO4_MW) %>%
  mutate("Sodium_mM" = Sodium/Na_MW) %>% 
  mutate("Potassium_mM" = Potassium/K_MW) %>% 
  mutate("Magnesium_mM" = Magnesium/Mg_MW) %>% 
  mutate("Calcium_mM"= Calcium/Ca_MW) %>% 
  mutate("Total_Iron_mM"= Total_Iron/Fe_MW) %>%
  mutate("SiO2_mM"= SiO2/SiO2_MW) %>%
  mutate("SiO2_dissolved_mM"= Dissolved_SiO2/SiO2_MW) %>%
  
  #summing all positive ions
  mutate("Sum_pos_mM" = Sodium_mM + Potassium_mM + Magnesium_mM + Calcium_mM + Total_Iron_mM) %>% 
  #summing Ca + Na
  mutate("Sum_Ca_Na_mM" = Sodium_mM + Calcium_mM) %>% 
  #summing all negative ions
  mutate("Sum_neg_mM" = Hydrogencarbonate_mM + Fluoride_mM + Chloride_mM + Bromide_mM + Nitrate_mM + Sulphate_mM + Phosphate_mM) %>% 
  #summing HCO + SO4 + Cl
  mutate("Sum_HCO_SO4_Cl_mM" = Hydrogencarbonate_mM + Chloride_mM + Sulphate_mM) %>% 
  #summing SO4 + Cl
  mutate("Sum_SO4_Cl_mM" = Chloride_mM + Sulphate_mM) %>% 
  
  
  #Calculating mol percents
  mutate("Na_mol%" = (Sodium_mM)/Sum_Ca_Na_mM) %>% 
  mutate("Ca_mol%" = (Calcium_mM)/Sum_Ca_Na_mM) %>% 
  mutate("Cl_mol%" = (Chloride_mM)/Sum_SO4_Cl_mM) %>% 
  mutate("SO4_mol%" = (Sulphate_mM)/Sum_SO4_Cl_mM) %>%
  
  mutate("SO4_HCO_mol%_2" = (Hydrogencarbonate_mM+Sulphate_mM)/Sum_HCO_SO4_Cl_mM) %>% 
  mutate("Cl_mol%_2" = (Chloride_mM)/Sum_HCO_SO4_Cl_mM) %>% 
  mutate("SO4_mol%_2" = (Sulphate_mM)/Sum_HCO_SO4_Cl_mM) %>% 
  mutate("HCO3_mol%_2" = (Hydrogencarbonate_mM)/Sum_HCO_SO4_Cl_mM) %>% 

  mutate("divalent_mol%" = (Sulphate_mM + Calcium_mM)/(Chloride_mM + Sodium_mM+Sulphate_mM + Calcium_mM)) %>% 
  mutate("divalent_mol%_2" = (Sulphate_mM + Calcium_mM)/(Chloride_mM + Sodium_mM+Hydrogencarbonate_mM+Sulphate_mM + Calcium_mM))  


setwd("C:/Users/37040/Grundfos/Nanofiltration NXF - Cooling tower/NF R-data files/Rdata_files")
saveRDS(Analysis_dat, file = "Water_Quality_Analysis_dat.rds")
setwd("./..")
