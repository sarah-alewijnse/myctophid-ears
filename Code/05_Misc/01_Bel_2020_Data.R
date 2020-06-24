#### Belcher 2020 Data ####

library(tidyverse)
library(readr)

Bel_2020 <- read.csv("Data/Belcher_2020_Data/Belcher_SupplementaryData_COMICS.csv")

# Conver from ul/mg/d to mg/kg/h

ul_mg <- function(ul){
  b <- ul * 1e6
  mg <- (b * 0.001309) / 24 # Density of oxygen (mg/m^3) at 1 bar and 21 C
  print(mg)
}

Bel_2020$mg_kg_h <- ul_mg(Bel_2020$R_WM..µL.O2..mg.WW.d.)

# Get means and SDs

mg_kg_means <- aggregate(Bel_2020, by = list(Bel_2020$Species), FUN = mean)
mg_kg_sd <- aggregate(Bel_2020, by = list(Bel_2020$Species), FUN = sd)

# Convert to C_resp

C_resp <- function(mg_kg){
  C <- 27.46 # Upper bound - Martino et al. 2020
  k <- 0.004 # Decay constant - Martino et al. 2020
  C_resp <- C*(1 - exp(1)^(-k*mg_kg))
  print(C_resp)
}

C_resp(629.37)
C_resp(177.76)
C_resp(76.83)