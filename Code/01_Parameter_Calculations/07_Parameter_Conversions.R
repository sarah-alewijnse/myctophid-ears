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

M_means <- data.frame(Species = c("ELN", "GYR", "KRA", "ELC", "PRM", "GYN"),
                      C_resp = c(0.2124, 0.2009, 0.1934, 0.1730, 0.1714, 0.1442))

# Convert to oxgyen consumption

M_means$mg_kg <- mg_kg(M_means$C_resp * 100)

#### Convert ul/mg/h to mg/k/h ####

# Create function

ul_mg <- function(ul){
  b <- ul * 1e6
  mg <- b * 0.001309 # Density of oxygen (mg/m^3) at 1 bar and 21 C
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
