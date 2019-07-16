#### Data Arranging ####

library(tidyverse)

myct <- read.csv("Data/Extra/Study_Myctophids.csv")

### Get cruise data ###

stn_comp <- read.csv("Data/Metadata/Station_Data.csv")

### Join to data ####

myct_comp <- left_join(myct, stn_comp, by = "Station")

write.csv(myct_comp, "Data/Extra/Study_Myctophids_Full.csv", row.names = F)

### Join to isotope data ###

iso <- read.csv("Data/Extra/Otolith_Isotopes.csv")
myct <- read.csv("Data/Extra/Study_Myctophids_Full.csv")

myct_iso <- left_join(myct, iso, by = "MyNumber")

write.csv(myct_iso, "Data/Myctophids_Master.csv", row.names = F)

### Join to ecology data ###

myct <- read.csv("Data/Myctophids_Master.csv")
ecol <- read.csv("Data/Extra/Ecol_Table.csv")

myct_ecol <- left_join(myct, ecol, by = "sciname")

write.csv(myct_ecol, "Data/Myctophids_Master.csv", row.names = F)
