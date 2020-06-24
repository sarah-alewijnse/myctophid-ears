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

plot(myct$mg_kg, myct$mean_M)
plot(myct$mean_M, myct$mg_kg)

ggplot(aes(sciname, mg_kg), data = myct) +
  geom_boxplot()

ggplot(aes(sciname, mean_M), data = myct) +
  geom_boxplot()

# Compare ranges among species

means <- aggregate(myct, by = list(myct$sciname), FUN = mean, na.rm = TRUE)

myct_max <- aggregate(myct, by = list(myct$sciname), FUN = max, na.rm = TRUE)
myct_min <- aggregate(myct, by = list(myct$sciname), FUN = min, na.rm = TRUE)
myct_range <- myct_max$mean_M - myct_min$mean_M

myct_range <- myct_max$mg_kg - myct_min$mg_kg

#### Do Models ####

# Main model

M_T_W_mod <- lmer(log(mg_kg) ~ log(Weight.x) + mean_Temp + (1|sciname), myct)
summary(M_T_W_mod)
ranef(M_T_W_mod)
ranova(M_T_W_mod)

plot(log(myct$Weight.x), log(myct$mg_kg))
plot(myct$mean_Temp, log(myct$mg_kg))

# Belcher model

Bel_Mod <- lm(log(mg_kg) ~ mean_log_Metabol, myct)
summary(Bel_Mod)
plot(myct$mean_log_Metabol, log(myct$mg_kg))

#### Within Species ####

# ELN

ELN <- filter(myct, sciname == "Electrona antarctica")
ELN_mod <- lm(mg_kg ~ log(Weight.x) + mean_Temp, ELN)
summary(ELN_mod)

# ELC

ELC <- filter(myct, sciname == "Electrona carlsbergi")
ELC_mod <- lm(log(mg_kg) ~ log(Weight.x) + mean_Temp, ELC)
summary(ELC_mod)

# GYR

GYR <- filter(myct, sciname == "Gymnoscopelus braueri")
GYR_mod <- lm(log(mg_kg) ~ log(Weight.x) + mean_Temp, GYR)
summary(GYR_mod)

plot(log(GYR$Weight.x), log(GYR$mg_kg))
abline(6.22, -0.166)     

# GYN

GYN <- filter(myct, sciname == "Gymnoscopelus nicholsi")
GYN_mod <- lm(log(mg_kg) ~ log(Weight.x) + mean_Temp, GYN)
summary(GYN_mod)

# PRM

PRM <- filter(myct, sciname == "Protomyctophum bolini")
PRM_mod <- lm(log(mg_kg) ~ log(Weight.x) + mean_Temp, PRM)
summary(PRM_mod)

plot(log(PRM$Weight.x), log(PRM$mg_kg))
abline(5.56, 0.220)    

# KRA

KRA <- filter(myct, sciname == "Krefftichthys anderssoni")
KRA_mod <- lm(log(mg_kg) ~ log(Weight.x) + mean_Temp, KRA)
summary(KRA_mod)
