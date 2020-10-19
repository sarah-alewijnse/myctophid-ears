#### M Conversions ####

# Converts oxygen consumption from C_resp to mg/kg/h, and from ul/mg/h to mg/kg/h

library(tidyverse)

# Read in file

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")

# Select M values (C_resp)

M_oto <- select(myct, sciname, Weight.x, mean_M, Dir_mean_Metabol)

#### Convert M Values to Oxygen Consumption (mg/kg/h) ####

# Create function (from equation 4)

mg_kg <- function(C_resp){
  C <- 27.46 # Upper bound - Martino et al. 2020
  k <- 0.004 # Decay constant - Martino et al. 2020
  a <- C_resp/C
  mg_kg <- -(log(1-a)/k)
  print(mg_kg)
}

# Get species means of M_oto (from model)

M_output <- read.csv("Outputs/02_Linear_Models_Among_Species/01_M_Body_Mass_Temperature/M_T_W_precis.csv")
M_means <- M_output[4:9, 1:2]

# Convert to oxgyen consumption

M_means$mg_kg <- mg_kg(M_means$mean * 100)
M_means$mg_kg <-  round(M_means$mg_kg, 2)

write.csv(M_means, "Outputs/01_Parameter_Calculations/04_Conversions/This_Study_Otolith_Derived.csv")

#### Convert ul/mg/h to mg/k/h ####

# Create function

ul_mg <- function(ul){
  b <- ul * 1e6
  mg <- b * 0.0014 # Density of oxygen (mg/m^3) at 1 bar and 2 C
  print(mg)
}

# Get Belcher averages per species

Bel_means <- aggregate(M_oto[,4], list(M_oto$sciname), mean, na.rm = TRUE)

# Convert to mg_kg

Bel_means$mg_kg <- ul_mg(Bel_means$x)

#### Convert Oxygen Consumption (mg/kg/h) to C_resp ####

C_resp <- function(mg_kg){
  C <- 27.46 # Upper bound - Martino et al. 2020
  k <- 0.004 # Decay constant - Martino et al. 2020
  C_resp <- C*(1 - exp(1)^(-k*mg_kg))
  print(C_resp)
}

# Convert Belcher averages to C_resp

Bel_means$C_resp <- C_resp(Bel_means$mg_kg)/100

colnames(Bel_means) <- c("Species", "ul_mg", "mg_kg", "C_resp")
Bel_means$ul_mg <- round(Bel_means$ul_mg, 3)
Bel_means$C_resp <- round(Bel_means$C_resp, 3)
Bel_means$mg_kg <- round(Bel_means$mg_kg, 2)

write.csv(Bel_means, "Outputs/01_Parameter_Calculations/04_Conversions/This_Study_Equation_Derived.csv")
