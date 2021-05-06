# Load required packages

library(tidyverse)
library(rethinking) # Used to interface with rstan
library(bayesplot) # Gives nice plots

# Print out all results

options(max.print=999999)

# Load and check data

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")
glimpse(myct)

# Tidy data ready for model input

myct_tidy <- filter(myct, !is.na(Weight.x)) # Remove those without weights
myct_tidy <- filter(myct_tidy, !is.na(mean_M)) # Remove those without C_resp
myct_tidy$log_Weight <- log(myct_tidy$Weight.x) # Log weights
glimpse(myct_tidy) # Check

# Create new Year column

myct_tidy$Station <- as.factor(myct_tidy$Station)

# Put into tidy list

M_T_W_list <- list(
  M_obs = myct_tidy$mean_M,
  M_se = myct_tidy$se_M,
  Weight = myct_tidy$log_Weight,
  Temp_obs = myct_tidy$mean_Temp,
  Temp_se = myct_tidy$se_Temp,
  Species = myct_tidy$sciname,
  Station = myct_tidy$Station
)
str(M_T_W_list)

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

# Tidy list

mod_list <- list(
  M_obs = M_T_W_list$M_obs,
  M_se = M_T_W_list$M_se,
  Weight = M_T_W_list$Weight_Z,
  Temp_obs = M_T_W_list$Temp_Obs_Z,
  Temp_se = M_T_W_list$Temp_se,
  Species = M_T_W_list$Species,
  Station = M_T_W_list$Station
)
str(mod_list)
mod_list

## Model

model_M_T_W <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model (equation 6)
    
    mu <- a + a_Spp[Species] + a_St[Station] +
      b_W*Weight +
      b_T*Temp_est[i],
    
    # Data uncertainties
    
    M_obs ~ dnorm(M_est, M_se),
    Temp_obs ~ dnorm(Temp_est, Temp_se),
    
    # Parameters
    
    a ~ dnorm(0, 1),
    b_W ~ dnorm(0, 1),
    b_T ~ dnorm(0, 1),
    a_Spp[Species] ~ dnorm(0, sigma_Species),
    a_St[Station] ~ dnorm(0, sigma_Station),
    sigma_Species ~ dcauchy(0, 1),
    sigma_Station ~ dcauchy(0, 1),
    sigma ~ dexp(1)
  ),
  data = mod_list,
  start = list(M_est = mod_list$M_obs,
               Temp_est = mod_list$Temp_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 5000,
  control = list(adapt_delta = 0.90))

## Run diagnostics

check_energy(model_M_T_W@stanfit)
check_treedepth(model_M_T_W@stanfit)

divergent <- get_sampler_params(model_M_T_W@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

## Check output

precis(model_M_T_W, digits = 4, prob = 0.95, depth = 2)
plot(precis(model_M_T_W))

## Save stanfit as RDS

saveRDS(model_M_T_W, "Outputs/04_Misc/03_Among_Species/M_T_W_Station_model.rds")
