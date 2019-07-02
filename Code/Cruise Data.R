#### Data Arranging ####

library(tidyverse)

myct <- read.csv("Data/Study_Myctophids.csv")

### Get cruise data ###

stn_comp <- read.csv("Data/Metadata/Station_Data.csv")

### Join to data ####

myct_tidy <- select(myct,
                    MyNumber,
                    ID,
                    sciname,
                    Label,
                    Station,
                    Notes,
                    Muscle,
                    Otolith,
                    Weight,
                    Weight_SD,
                    Sex)

myct_comp <- left_join(myct_tidy, stn_comp, by = "Station")

write.csv(myct_comp, "Data/Study_Myctophids_Full.csv")

### Join to isotope data ###

iso <- read.csv("Data/Otolith_Isotopes.csv")
myct <- read.csv("Data/Study_Myctophids_Full.csv")

myct_iso <- left_join(myct, iso, by = "MyNumber")

write.csv(myct_iso, "Data/Myctophids_Master.csv")

### Join to ecology data ###

myct <- read.csv("Data/Myctophids_Master.csv")
ecol <- read.csv("Data/Extra/Ecol_Table.csv")

myct_ecol <- left_join(myct, ecol, by = "sciname")

write.csv(myct_ecol, "Data/Myctophids_Master.csv")
