#### Summary Code ####

library(dplyr)
library(readr)

Myctophids <- read_csv("Data/Myctophids.csv")
Study_Myctophids  <- filter(Myctophids, Otolith != "NA")
Count_Myctophids <- count(Study_Myctophids, sciname)
KRA <- filter(Study_Myctophids, Label == "KRA")
