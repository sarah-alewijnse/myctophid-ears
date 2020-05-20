#### LME4 Models ####

# Load packages

library(lme4)
library(lmerTest)
library(tidyverse)

# Load and check data

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")

#### Overall Model with Weight and Temp ####

myct_tidy <- filter(myct, Weight_SD == "0")
myct_tidy <- filter(myct, !is.na(mean_M))
myct_tidy$log_Weight <- log(myct_tidy$Weight.x)
glimpse(myct_tidy)

M_T_W_list <- list(
  M_obs = myct_tidy$mean_M,
  M_se = myct_tidy$se_M,
  Weight = myct_tidy$log_Weight,
  Temp_obs = myct_tidy$mean_Temp,
  Temp_se = myct_tidy$se_Temp,
  Species = myct_tidy$sciname
)

## Convert to z-scores

# Weight

Weight_mean <- mean(M_T_W_list$Weight)
Weight_sd <- sd(M_T_W_list$Weight)

for(i in 1:length(M_T_W_list$Weight)){
  M_T_W_list$Weight_Z[i] <- (M_T_W_list$Weight[i] - Weight_mean) / Weight_sd
}

# Temp_obs

Temp_obs_mean <- mean(M_T_W_list$Temp_obs)
Temp_obs_sd <- sd(M_T_W_list$Temp_obs)

for(i in 1:length(M_T_W_list$Temp_obs)){
  M_T_W_list$Temp_Obs_Z[i] <- (M_T_W_list$Temp_obs[i] - Temp_obs_mean) / Temp_obs_sd
}

glimpse(M_T_W_list)

# Do model

M_T_W_mod <- lmer(M_obs ~ Weight_Z + Temp_Obs_Z + (1|Species), M_T_W_list)
summary(M_T_W_mod)
ranef(M_T_W_mod)
ranova(M_T_W_mod)

#### Belcher Oxygen Consumption ####

myct_tidy <- filter(myct, !is.na(mean_Metabol))
myct_tidy <- filter(myct_tidy, !is.na(mean_M))
glimpse(myct_tidy)

M_Metabol_list <- list(
  M_obs = myct_tidy$mean_M,
  M_se = myct_tidy$se_M,
  Metabol_obs = myct_tidy$mean_Metabol,
  Metabol_se = myct_tidy$se_Metabol
)

## Convert to z-scores

# Metabol_obs

Metabol_obs_mean <- mean(M_Metabol_list$Metabol_obs)
Metabol_obs_sd <- sd(M_Metabol_list$Metabol_obs)

for(i in 1:length(M_Metabol_list$Metabol_obs)){
  M_Metabol_list$Metabol_Obs_Z[i] <- (M_Metabol_list$Metabol_obs[i] - Metabol_obs_mean) / Metabol_obs_sd
}

glimpse(M_Metabol_list)

# Do model

mod_Bel <- lm(M_obs ~ Metabol_Obs_Z, data = M_Metabol_list)
summary(mod_Bel)

##### Within Species ####

M_T_W_data <- as.data.frame(M_T_W_list)
M_T_W_data <- select(M_T_W_data, -Weight_Z, -Temp_Obs_Z)

##### ELN ####

ELN <- filter(M_T_W_data, Species == "Electrona antarctica")
ELN <- as.list(ELN)

# Convert weight to z-score

ELN_weight_mean <- mean(ELN$Weight) # Get species mean of weight
ELN_weight_sd <- sd(ELN$Weight) # Get species standard deviation of weight
for(i in 1:length(ELN$Weight)){ # Loop to get z-scores
  ELN$Weight_Z[i] <- (ELN$Weight[i] - ELN_weight_mean) / ELN_weight_sd
}

# Convert temperature to z-score

ELN_temp_mean <- mean(ELN$Temp_obs) # Get species mean of temperature
ELN_temp_sd <- sd(ELN$Temp_obs) # Get species standard deviation of temperature
for(i in 1:length(ELN$Temp_obs)){ # Loop to get z-scores
  ELN$Temp_Z[i] <- (ELN$Temp_obs[i] - ELN_temp_mean) / ELN_temp_sd
}

glimpse(ELN) # Check data

# Do model

mod_ELN <- lm(M_obs ~ Weight_Z + Temp_Z, data = ELN)
summary(mod_ELN)

##### ELC ####

ELC <- filter(M_T_W_data, Species == "Electrona carlsbergi")
ELC <- as.list(ELC)

# Convert weight to z-score

ELC_weight_mean <- mean(ELC$Weight) # Get species mean of weight
ELC_weight_sd <- sd(ELC$Weight) # Get species standard deviation of weight
for(i in 1:length(ELC$Weight)){ # Loop to get z-scores
  ELC$Weight_Z[i] <- (ELC$Weight[i] - ELC_weight_mean) / ELC_weight_sd
}

# Convert temperature to z-score

ELC_temp_mean <- mean(ELC$Temp_obs) # Get species mean of temperature
ELC_temp_sd <- sd(ELC$Temp_obs) # Get species standard deviation of temperature
for(i in 1:length(ELC$Temp_obs)){ # Loop to get z-scores
  ELC$Temp_Z[i] <- (ELC$Temp_obs[i] - ELC_temp_mean) / ELC_temp_sd
}

glimpse(ELC) # Check data

# Do model

mod_ELC <- lm(M_obs ~ Weight_Z + Temp_Z, data = ELC)
summary(mod_ELC)

##### GYR ####

GYR <- filter(M_T_W_data, Species == "Gymnoscopelus braueri")
GYR <- as.list(GYR)

# Convert weight to z-score

GYR_weight_mean <- mean(GYR$Weight) # Get species mean of weight
GYR_weight_sd <- sd(GYR$Weight) # Get species standard deviation of weight
for(i in 1:length(GYR$Weight)){ # Loop to get z-scores
  GYR$Weight_Z[i] <- (GYR$Weight[i] - GYR_weight_mean) / GYR_weight_sd
}

# Convert temperature to z-score

GYR_temp_mean <- mean(GYR$Temp_obs) # Get species mean of temperature
GYR_temp_sd <- sd(GYR$Temp_obs) # Get species standard deviation of temperature
for(i in 1:length(GYR$Temp_obs)){ # Loop to get z-scores
  GYR$Temp_Z[i] <- (GYR$Temp_obs[i] - GYR_temp_mean) / GYR_temp_sd
}

glimpse(GYR) # Check data

# Do model

mod_GYR <- lm(M_obs ~ Weight_Z + Temp_Z, data = GYR)
summary(mod_GYR)

##### GYN ####

GYN <- filter(M_T_W_data, Species == "Gymnoscopelus nicholsi")
GYN <- as.list(GYN)

# Convert weight to z-score

GYN_weight_mean <- mean(GYN$Weight) # Get species mean of weight
GYN_weight_sd <- sd(GYN$Weight) # Get species standard deviation of weight
for(i in 1:length(GYN$Weight)){ # Loop to get z-scores
  GYN$Weight_Z[i] <- (GYN$Weight[i] - GYN_weight_mean) / GYN_weight_sd
}

# Convert temperature to z-score

GYN_temp_mean <- mean(GYN$Temp_obs) # Get species mean of temperature
GYN_temp_sd <- sd(GYN$Temp_obs) # Get species standard deviation of temperature
for(i in 1:length(GYN$Temp_obs)){ # Loop to get z-scores
  GYN$Temp_Z[i] <- (GYN$Temp_obs[i] - GYN_temp_mean) / GYN_temp_sd
}

glimpse(GYN) # Check data

# Do model

mod_GYN <- lm(M_obs ~ Weight_Z + Temp_Z, data = GYN)
summary(mod_GYN)

##### KRA ####

KRA <- filter(M_T_W_data, Species == "Krefftichthys anderssoni")
KRA <- as.list(KRA)

# Convert weight to z-score

KRA_weight_mean <- mean(KRA$Weight) # Get species mean of weight
KRA_weight_sd <- sd(KRA$Weight) # Get species standard deviation of weight
for(i in 1:length(KRA$Weight)){ # Loop to get z-scores
  KRA$Weight_Z[i] <- (KRA$Weight[i] - KRA_weight_mean) / KRA_weight_sd
}

# Convert temperature to z-score

KRA_temp_mean <- mean(KRA$Temp_obs) # Get species mean of temperature
KRA_temp_sd <- sd(KRA$Temp_obs) # Get species standard deviation of temperature
for(i in 1:length(KRA$Temp_obs)){ # Loop to get z-scores
  KRA$Temp_Z[i] <- (KRA$Temp_obs[i] - KRA_temp_mean) / KRA_temp_sd
}

glimpse(KRA) # Check data

# Do model

mod_KRA <- lm(M_obs ~ Weight_Z + Temp_Z, data = KRA)
summary(mod_KRA)

##### PRM ####

PRM <- filter(M_T_W_data, Species == "Protomyctophum bolini")
PRM <- as.list(PRM)

# Convert weight to z-score

PRM_weight_mean <- mean(PRM$Weight) # Get species mean of weight
PRM_weight_sd <- sd(PRM$Weight) # Get species standard deviation of weight
for(i in 1:length(PRM$Weight)){ # Loop to get z-scores
  PRM$Weight_Z[i] <- (PRM$Weight[i] - PRM_weight_mean) / PRM_weight_sd
}

# Convert temperature to z-score

PRM_temp_mean <- mean(PRM$Temp_obs) # Get species mean of temperature
PRM_temp_sd <- sd(PRM$Temp_obs) # Get species standard deviation of temperature
for(i in 1:length(PRM$Temp_obs)){ # Loop to get z-scores
  PRM$Temp_Z[i] <- (PRM$Temp_obs[i] - PRM_temp_mean) / PRM_temp_sd
}

glimpse(PRM) # Check data

# Do model

mod_PRM <- lm(M_obs ~ Weight_Z + Temp_Z, data = PRM)
summary(mod_PRM)
