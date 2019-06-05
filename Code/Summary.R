#### Summary Code ####

library(dplyr)

Myctophids <- read.csv("Data/Myctophids.csv")
Milled_Myctophids <- filter(Myctophids, Milled == "Y")
Count_Myctophids <- count(Milled_Myctophids, sciname)

Study_Myctophids  <- filter(Myctophids, Milled != "N")
KRA <- filter(Study_Myctophids, Label == "KRA")
