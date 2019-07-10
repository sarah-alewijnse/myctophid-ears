#### Sherwood-Rose Weights ####

library(tidyverse)
library(rfishbase)

sher_rose <- read.csv("Data/Sherwood_Rose_Myct.csv")

Species_List <- select(sher_rose, sciname) # Get just the species list
Ecol_Data <- apply(Species_List, 2, species) # Use the species function to get species data
Ecol_Data_E <- Ecol_Data[["sciname"]] # Extract the species table
Ecol_Data_E <- rename(Ecol_Data_E, sciname = Species)
Whole <- left_join(Species_List, Ecol_Data_E, by = "sciname") # Join with the species list

weights <- select(Whole, sciname, Weight)
