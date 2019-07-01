#### Cruise Data ####

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
