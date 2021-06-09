### Age Estimates ###

# Load tidyverse

library(tidyverse)

# Load and check data

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")
glimpse(myct)

#### GYN ####

# Subset GYN

GYN <- filter(myct, Label == "GYN")

# South Shetland VBGF Parameters - Linkowski 1985

length.2.age <- function(length, k, Loo, to){
  e <- exp(1)
  a <- Loo / (Loo - length)
  age <- (1/k) * log(a, base = e) + to
  return(age)
}

GYN$Est_Age <- length.2.age(GYN$SL, 0.41, 163.8, 0.081)

min(GYN$Est_Age, na.rm = TRUE)
max(GYN$Est_Age, na.rm = TRUE)
mean(GYN$Est_Age, na.rm = TRUE)
sd(GYN$Est_Age, na.rm = TRUE)

#### ELN ####

# Subset ELN

ELN <- filter(myct, Label == "ELN")

# Data from Linkowski 1987 (FishBase)

ELN$SL <- ELN$SL/10
ELN_k <- mean(c(0.25, 0.17))
ELN_Loo <- mean(c(9.7, 12.9))
ELN_to <- mean(c(0.01, 0.13))

ELN$Est_Age <- length.2.age(ELN$SL, ELN_k, ELN_Loo, ELN_to)

min(ELN$Est_Age, na.rm = TRUE)
max(ELN$Est_Age, na.rm = TRUE)
mean(ELN$Est_Age, na.rm = TRUE)
sd(ELN$Est_Age, na.rm = TRUE)

#### ELC ####

# Subset ELC

ELC <- filter(myct, Label == "ELC")

ELC$SL <- ELC$SL/10

# Data from Linkowski 1987 (FishBase)

ELC$Est_Age <- length.2.age(ELC$SL, 0.55, 9.7, -0.6)

min(ELC$Est_Age, na.rm = TRUE)
max(ELC$Est_Age, na.rm = TRUE)
mean(ELC$Est_Age, na.rm = TRUE)
sd(ELC$Est_Age, na.rm = TRUE)

#### GYR ####

GYR <- filter(myct, Label == "GYR")

# Data from Saunders et al. 2020

GYR$Est_Age <- length.2.age(GYR$SL, 0.29, 133.22, -0.21)

min(GYR$Est_Age, na.rm = TRUE)
max(GYR$Est_Age, na.rm = TRUE)
mean(GYR$Est_Age, na.rm = TRUE)
sd(GYR$Est_Age, na.rm = TRUE)

#### KRA ####

KRA <- filter(myct, Label == "KRA")

# Data from Saunders et al. 2020

KRA$Est_Age <- length.2.age(KRA$SL, 0.71, 68.6, -0.49)

min(KRA$Est_Age, na.rm = TRUE)
max(KRA$Est_Age, na.rm = TRUE)
mean(KRA$Est_Age, na.rm = TRUE)
sd(KRA$Est_Age, na.rm = TRUE)

# One NAN

