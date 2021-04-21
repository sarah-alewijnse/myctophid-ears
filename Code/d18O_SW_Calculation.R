#### d18O Calculation ####

library(tidyverse)

JCR <- read.csv("Data/JCR_Station_Dat.csv")
glimpse(JCR)

0.24* 33 - 8.45

#### Calculate d18O_SW ####

# Values from LeGrande & Schmidt 2006 - Southern Ocean

a <- -8.45
b <- 0.24

JCR$d18O_SW <- a + b*JCR$Salinity

hist(JCR$d18O_SW)

# Write into file

write.csv(JCR, "Data/JCR_Station_Dat.csv", row.names = F)
