#### Comparison between Methods ####

# Compares mean oxygen consumptions among different methods

# Load required packages

library(tidyverse)

# Load data from this study and from Belcher et al. 2020

This_Study <- read.csv("Outputs/01_Parameter_Calculations/04_Conversions/Summary.csv")

# Get differences

diff <- data.frame(Species = This_Study$Species)

diff$mg_kg_abs <- abs(This_Study$oto_mg_kg_mean - This_Study$equ_mg_kg_mean)

# Get as a percent of otolith derived

diff$mg_kg_percent <- (diff$mg_kg_abs/This_Study$oto_mg_kg_mean) * 100
diff$mg_kg_percent <- round(diff$mg_kg_percent, 2)

# Write into CSV file

write.csv(diff, "Outputs/01_Parameter_Calculations/04_Conversions/Method_Comparison.csv", row.names = F)
