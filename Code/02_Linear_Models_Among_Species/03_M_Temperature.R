#### Bayesian Linear Models - Temp Only ####

library(tidyverse)
library(rethinking)
library(bayesplot)

options(max.print=999999)

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")
glimpse(myct)

#### Overall Model with Temperature ####

myct_tidy <- filter(myct, !is.na(mean_M))
glimpse(myct_tidy)

M_T_list <- list(
  M_obs = myct_tidy$mean_M,
  M_se = myct_tidy$se_M,
  Temp_obs = myct_tidy$mean_Temp,
  Temp_se = myct_tidy$se_Temp,
  Species = myct_tidy$sciname
)

## Convert to z-scores

# Temp_obs

Temp_obs_mean <- mean(M_T_list$Temp_obs)
Temp_obs_sd <- sd(M_T_list$Temp_obs)

for(i in 1:length(M_T_list$Temp_obs)){
  M_T_list$Temp_Obs_Z[i] <- (M_T_list$Temp_obs[i] - Temp_obs_mean) / Temp_obs_sd
}

# Tidy list

mod_list <- list(
  M_obs = M_T_list$M_obs,
  M_se = M_T_list$M_se,
  Temp_obs = M_T_list$Temp_Obs_Z,
  Temp_se = M_T_list$Temp_se,
  Species = M_T_list$Species
)

## Model

model_M_T <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + a_Var[Species] +
      b_T*Temp_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_se),
    Temp_obs ~ dnorm(Temp_est, Temp_se),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_T ~ dnorm(0, 1),
    a_Var[Species] ~ dnorm(0 , sigma_Species),
    sigma_Species ~ dcauchy(0, 1),
    sigma ~ dexp(1)
  ),
  data = mod_list,
  start = list(M_est = mod_list$M_obs,
               Temp_est = mod_list$Temp_obs),
  WAIC = FALSE,
  iter = 3000,
  warmup = 1500)

## Run diagnostics

check_energy(model_M_T@stanfit)
check_treedepth(model_M_T@stanfit)

divergent <- get_sampler_params(model_M_T@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_M_T, digits = 4, prob = 0.95, depth = 2)

## Save stanfit

saveRDS(model_M_T, "Outputs/02_Linear_Models_Among_Species/03_M_Temperature/M_T_model.rds")


