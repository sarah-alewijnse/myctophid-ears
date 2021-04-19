#### Summary Table ####

library(tidyverse)

# Read in data

myct <- read.csv("Data/Myctophids_M_Temp_Length_Maturity.csv")

#### ELN ####

ELN <- filter(myct, Label == "ELN")

# Year

ELN %>% count(Year.x)

# Lenght range

min(ELN$SL, na.rm = TRUE)
max(ELN$SL, na.rm = TRUE)
mean(ELN$SL, na.rm = TRUE)

# Weight range

min(ELN$Weight.x, na.rm = TRUE)
max(ELN$Weight.x, na.rm = TRUE)
mean(ELN$Weight, na.rm = TRUE)

#### ELC ####

ELC <- filter(myct, Label == "ELC")

# Year

unique(ELC$Year.x)

# Lenght range

min(ELC$SL, na.rm = TRUE)
max(ELC$SL, na.rm = TRUE)
mean(ELC$SL, na.rm = TRUE)

# Weight range

min(ELC$Weight.x, na.rm = TRUE)
max(ELC$Weight.x, na.rm = TRUE)
mean(ELC$Weight, na.rm = TRUE)

#### GYR ####

GYR <- filter(myct, Label == "GYR")

# Year

GYR %>% count(Year.x)

# Lenght range

min(GYR$SL, na.rm = TRUE)
max(GYR$SL, na.rm = TRUE)
mean(GYR$SL, na.rm = TRUE)

# Weight range

min(GYR$Weight.x, na.rm = TRUE)
max(GYR$Weight.x, na.rm = TRUE)
mean(GYR$Weight, na.rm = TRUE)

#### GYN ####

GYN <- filter(myct, Label == "GYN")

# Year

GYN %>% count(Year.x)

# Lenght range

min(GYN$SL, na.rm = TRUE)
max(GYN$SL, na.rm = TRUE)
mean(GYN$SL, na.rm = TRUE)

# Weight range

min(GYN$Weight.x, na.rm = TRUE)
max(GYN$Weight.x, na.rm = TRUE)
mean(GYN$Weight, na.rm = TRUE)

#### KRA ####

KRA <- filter(myct, Label == "KRA")

# Year

KRA %>% count(Year.x)

# Lenght range

min(KRA$SL, na.rm = TRUE)
max(KRA$SL, na.rm = TRUE)
mean(KRA$SL, na.rm = TRUE)

# Weight range

min(KRA$Weight.x, na.rm = TRUE)
max(KRA$Weight.x, na.rm = TRUE)
mean(KRA$Weight, na.rm = TRUE)

#### PRM ####

PRM <- filter(myct, Label == "PRM")

# Year

PRM %>% count(Year.x)

# Lenght range

min(PRM$SL, na.rm = TRUE)
max(PRM$SL, na.rm = TRUE)
mean(PRM$SL, na.rm = TRUE)

# Weight range

min(PRM$Weight.x, na.rm = TRUE)
max(PRM$Weight.x, na.rm = TRUE)
mean(PRM$Weight, na.rm = TRUE)

PRM %>% count(Crushed)

#### Haul Types ####

JCR <- read.csv("Data/JCR_Station_Dat.csv")

# Join

myct_stations <- left_join(myct, JCR, by = "Station")

# ELN

ELN <- filter(myct_stations, Label == "ELN")
ELN %>% count(Gear)

# ELC

ELC <- filter(myct_stations, Label == "ELC")
ELC %>% count(Gear)

# GYR

GYR <- filter(myct_stations, Label == "GYR")
GYR %>% count(Gear)

# GYN

GYN <- filter(myct_stations, Label == "GYN")
GYN %>% count(Gear)

# KRA

KRA <- filter(myct_stations, Label == "KRA")
KRA %>% count(Gear)

# PRM

PRM <- filter(myct_stations, Label == "PRM")
PRM %>% count(Gear)
