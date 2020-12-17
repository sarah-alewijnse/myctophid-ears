#### Belcher 2020 Data ####

library(tidyverse)
library(readr)

Bel_2020 <- read.csv("Archive/Belcher_2020_Data/Belcher_SupplementaryData_COMICS.csv")

#### Convert from ul/mg/d to mg/kg/h ####

ul_mg <- function(ul){
  b <- ul * 1e6
  mg <- (b * 0.0014) / 24 # Density of oxygen (mg/m^3) at 1 bar and 21 C
  print(mg)
}

Bel_2020$mg_kg_h <- ul_mg(Bel_2020$R_WM..µL.O2..mg.WW.d.)

#### Convert to C_resp ####

C_resp <- function(mg_kg){
  C <- 27.46 # Upper bound - Martino et al. 2020
  k <- 0.004 # Decay constant - Martino et al. 2020
  C_resp <- C*(1 - exp(1)^(-k*mg_kg))
  print(C_resp)
}

Bel_2020$C_resp <- C_resp(Bel_2020$mg_kg_h)

# Get means, SDs and range

mg_kg_means <- aggregate(Bel_2020, by = list(Bel_2020$Species), FUN = mean)
mg_kg_sd <- aggregate(Bel_2020, by = list(Bel_2020$Species), FUN = sd)
mg_kg_min <- aggregate(Bel_2020, by = list(Bel_2020$Species), FUN = min)
mg_kg_max <- aggregate(Bel_2020, by = list(Bel_2020$Species), FUN = max)

# Tidy

oxygen_consumption <- data.frame(Taxa = mg_kg_means$Group.1,
                                 mg_kg_means = mg_kg_means$mg_kg_h,
                                 mg_kg_sd = mg_kg_sd$mg_kg_h,
                                 mg_kg_min = mg_kg_min$mg_kg_h,
                                 mg_kg_max = mg_kg_max$mg_kg_h,
                                 C_resp_mean = mg_kg_means$C_resp,
                                 C_resp_sd = mg_kg_sd$C_resp,
                                 C_resp_min = mg_kg_min$C_resp,
                                 C_resp_max = mg_kg_max$C_resp)

oxygen_consumption[,c(2:5)] <- round(oxygen_consumption[,c(2:5)], digits = 2)
oxygen_consumption[,c(6:9)] <- oxygen_consumption[,c(6:9)]/100
oxygen_consumption[,c(6:9)] <- round(oxygen_consumption[,c(6:9)], digits = 3)

write.csv(oxygen_consumption, "Outputs/04_Misc/03_Bel_2020_Data_Conversion/Bel_2020_Conversion.csv", row.names = F)
