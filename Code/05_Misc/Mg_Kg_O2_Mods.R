#### LME4 With Conversions ####

library(tidyverse)
library(lme4)
library(lmerTest)

# Read in file

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")

#### Convert M Values to Oxygen Consumption (mg/kg/h) ####

# Create function

mg_kg <- function(C_resp){
  C <- 0.2746 # Upper bound - Martino et al. 2020
  k <- 0.004 # Decay constant - Martino et al. 2020
  a <- C_resp/C
  mg_kg <- -(log(1-a)/k)
  print(mg_kg)
}

myct$mg_kg <- mg_kg(myct$mean_M)

# Plot

plot(myct$mean_M, myct$mg_kg)

#### Do Models ####

# Main model

M_T_W_mod <- lmer(mg_kg ~ log(Weight.x) + mean_Temp + (1|sciname), myct)
summary(M_T_W_mod)
ranef(M_T_W_mod)
ranova(M_T_W_mod)

plot(log(myct$Weight.x), myct$mg_kg)
plot(myct$mean_Temp, myct$mg_kg)

# Belcher model

Bel_Mod <- lm(mg_kg ~ mean_Metabol, myct)
summary(Bel_Mod)
plot(myct$mean_Metabol, myct$mg_kg)
abline(346, -124)
