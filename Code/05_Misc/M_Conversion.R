#### M Conversion ####

library(tidyverse)

# Read in file

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")

# Select M values

M_oto <- select(myct, sciname, Weight.x, mean_M, mean_Metabol)

# Convert

C <- 27.46 # Upper bound
k <- 0.004 # Decay constant
M_oto$a <- M_oto$mean_M*100/C
M_oto$oxy <- -(log(1-M_oto$a)/k)

# Function

mg_kg <- function(C_resp){
  C <- 27.46 # Upper bound
  k <- 0.004 # Decay constant
  a <- C_resp/C
  mg_kg <- -(log(1-a)/k)
  print(mg_kg)
}

mg_kg(14.42)

# Basic stats

M_oto <- na.omit(M_oto)
min(M_oto$oxy)
max(M_oto$oxy)
mean(M_oto$oxy)
sd(M_oto$oxy)

# Plot

ggplot(M_oto, aes(x = mean_M, y = oxy, colour = sciname)) +
  geom_point()

ggplot(M_oto, aes(x = mean_Metabol, y = oxy, colour = sciname)) +
  geom_point()

# Conversion

0.39 # mean calculated from Belcher ul/mg
b <- 0.39 * 1e+6 # ul/kg
b * 0.001309 # mg/kg

ul_mg <- function(ul){
  b <- ul * 1e6
  mg <- b * 0.001309
  print(mg)
}

ul_mg(0.0587)

ul_mg(min(M_oto$mean_Metabol))
ul_mg(max(M_oto$mean_Metabol))
ul_mg(mean(M_oto$mean_Metabol))
ul_mg(sd(M_oto$mean_Metabol))

# Convert to Cresp

C_resp <- function(mg_kg){
  C <- 27.46
  k <- 0.004
  C_resp <- C*(1 - exp(1)^(-k*mg_kg))
  print(C_resp)
}

C_resp(445.85)

# Get Belcher averages per species

Bel_means <- aggregate(myct[,72], list(myct$sciname), mean, na.rm = TRUE)

# Convert to mg_kg

Bel_means$mg_kg <- ul_mg(Bel_means$x)

# Convert to Cresp

Bel_means$C_resp <- C_resp(Bel_means$mg_kg)
