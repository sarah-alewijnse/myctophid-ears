#### Bayesian Linear Models - Belcher RMR Estimates ####

library(tidyverse)
library(rethinking)
library(bayesplot)

options(max.print=999999)

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")
glimpse(myct)

#### Overall Model with Weight and Temp ####

myct_tidy <- filter(myct, !is.na(mean_Metabol))
myct_tidy <- filter(myct_tidy, !is.na(mean_M))
glimpse(myct_tidy)

M_Metabol_list <- list(
  M_obs = myct_tidy$mean_M,
  M_se = myct_tidy$se_M,
  Metabol_obs = myct_tidy$Dir_mean_Metabol,
  Metabol_se = myct_tidy$se_Metabol
)

## Convert to z-scores

# Metabol_obs

Metabol_obs_mean <- mean(M_Metabol_list$Metabol_obs)
Metabol_obs_sd <- sd(M_Metabol_list$Metabol_obs)

for(i in 1:length(M_Metabol_list$Metabol_obs)){
  M_Metabol_list$Metabol_Obs_Z[i] <- (M_Metabol_list$Metabol_obs[i] - Metabol_obs_mean) / Metabol_obs_sd
}

# Tidy list

mod_list <- list(
  M_obs = M_Metabol_list$M_obs,
  M_se = M_Metabol_list$M_se,
  Metabol_obs = M_Metabol_list$Metabol_Obs_Z,
  Metabol_se = M_Metabol_list$Metabol_se
)

## Model

  model_M_Metabol <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b * Metabol_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_se),
    Metabol_obs ~ dnorm(Metabol_est, Metabol_se),
    
    # Parameters
    a ~ dnorm(0, 1),
    b ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = mod_list,
  start = list(M_est = mod_list$M_obs,
               Metabol_est = mod_list$Metabol_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 5000)

  ## Run diagnostics
  
  check_energy(model_M_Metabol@stanfit)
  check_treedepth(model_M_Metabol@stanfit)
  
  divergent <- get_sampler_params(model_M_Metabol@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
  sum(divergent)
  
  pairs(model_M_Metabol, pars = c("a", "b", "sigma"))
  
  precis(model_M_Metabol, digits = 4, prob = 0.95, depth = 2)
  
  ## Save stanfit
  
  saveRDS(model_M_Metabol, "Outputs/02_Linear_Models_Among_Species/02_M_Oxygen_Consumption/M_Belcher_model.rds")
  
  


