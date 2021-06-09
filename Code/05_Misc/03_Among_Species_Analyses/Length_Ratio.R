#### Length Ratio Analysis ####

library(tidyverse)

# Read in data

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")

#### Length Ratio ####

# Create a table of max length (SL, mm)

max_SL <- data.frame(Label = c("ELN", "ELC", "GYR", "GYN", "KRA", "PRM"),
                     max_SL = c("115", "93", "162", "165", "74", "66"))

# Filter out crushed otoliths

myct <- filter(myct, Crushed == 0)

# Join to data

myct_SL <- left_join(myct, max_SL, by = "Label")
str(myct_SL)

# Get ratio

myct_SL$Ratio <- myct_SL$SL / as.numeric(myct_SL$max_SL)


##### Bayesian Linear Model - Ratio only ####

# Laod packages

library(rethinking) # Used to interface with rstan
library(bayesplot) # Gives nice plots

# Print out all results

options(max.print=999999)

myct_tidy <- filter(myct_SL, !is.na(mean_M))
myct_tidy <- filter(myct_tidy, !is.na(Ratio))

mod_list <- list(
  M_obs = myct_tidy$mean_M,
  M_se = myct_tidy$se_M,
  Ratio = myct_tidy$Ratio,
  Species = myct_tidy$sciname
)

str(mod_list)

mod_tab <- as.data.frame(mod_list)

# Model

model_all_ratio <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + a_Var[Species] + b_R*Ratio,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_se),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_R ~ dnorm(0, 1),
    a_Var[Species] ~ dnorm(0 , sigma_Species),
    sigma_Species ~ dcauchy(0, 1),
    sigma ~ dexp(1)
  ),
  data = mod_list,
  start = list(M_est = mod_list$M_obs),
  WAIC = FALSE,
  iter = 20000,
  warmup = 10000,
  control = list(adapt_delta = 0.90))

# Check model

check_energy(model_all_ratio@stanfit)
check_treedepth(model_all_ratio@stanfit)

divergent <- get_sampler_params(model_all_ratio@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_all_ratio, digits = 4, prob = 0.95, depth = 2)

# Save model

saveRDS(model_all_ratio, "Outputs/04_Misc/03_Among_Species/Length_Ratio_Cresp.rds")

#### Bayesian Linear Model - Temp & Ratio ####

# Put into tidy list

M_T_W_list <- list(
  M_obs = myct_tidy$mean_M,
  M_se = myct_tidy$se_M,
  Ratio = myct_tidy$Ratio,
  Temp_obs = myct_tidy$mean_Temp,
  Temp_se = myct_tidy$se_Temp,
  Species = myct_tidy$sciname
)
str(M_T_W_list)

## Convert to z-scores

# Ratio

Ratio_mean <- mean(M_T_W_list$Ratio)
Ratio_sd <- sd(M_T_W_list$Ratio)

for(i in 1:length(M_T_W_list$Ratio)){
  M_T_W_list$Ratio_Z[i] <- (M_T_W_list$Ratio[i] - Ratio_mean) / Ratio_sd
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
  Ratio = M_T_W_list$Ratio_Z,
  Temp_obs = M_T_W_list$Temp_Obs_Z,
  Temp_se = M_T_W_list$Temp_se,
  Species = M_T_W_list$Species
)
str(mod_list)

## Model

model_M_T_R <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model (equation 6)
    
    mu <- a + a_Var[Species] +
      b_R*Ratio +
      b_T*Temp_est[i],
    
    # Data uncertainties
    
    M_obs ~ dnorm(M_est, M_se),
    Temp_obs ~ dnorm(Temp_est, Temp_se),
    
    # Parameters
    
    a ~ dnorm(0, 1),
    b_R ~ dnorm(0, 1),
    b_T ~ dnorm(0, 1),
    a_Var[Species] ~ dnorm(0 , sigma_Species),
    sigma_Species ~ dcauchy(0, 1),
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

check_energy(model_M_T_R@stanfit)
check_treedepth(model_M_T_R@stanfit)

divergent <- get_sampler_params(model_M_T_R@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

## Check output

precis(model_M_T_R, digits = 4, prob = 0.95, depth = 2)

## Save stanfit as RDS

saveRDS(model_M_T_R, "Outputs/04_Misc/03_Among_Species/Length_Ratio_Temp_Cresp.rds")
