#### Summary Code ####

library(dplyr)
library(readr)

Myctophids <- read_csv("Data/Extra/Myctophids.csv")
Study_Myctophids  <- filter(Myctophids, Otolith != "NA")
Count_Myctophids <- count(Study_Myctophids, sciname)

write.csv(Study_Myctophids, "Data/Study_Myctophids.csv", row.names = F)

# Need metadata

Metadata_Myctophids <- filter(Study_Myctophids, is.na(Weight))
Metadata_Myctophids <- select(Metadata_Myctophids, ID, sciname, Label, Station, Notes)

# Write into CSV file

write.csv(Metadata_Myctophids, "Data/Myctophids_wo_Metadata.csv", row.names = F)
