#### Bayesian Linear Models - Crushed vs. Milled Comparison ####

# Load required packages

library(tidyverse)
library(rethinking) # Used to interface with rstan
library(bayesplot) # Gives nice plots

# Print out all results

options(max.print=999999)

# Load and check data

myct <- read.csv("Data/Myctophids_M_Temp_Length_Maturity.csv")
glimpse(myct)

#### Within PRM ####

# Subset to just PRM

PRM_tidy <- filter(myct, Label == "PRM")
glimpse(PRM_tidy)

# Tidy into list

mod_list <- list(
  M_obs = PRM_tidy$mean_M,
  M_se = PRM_tidy$se_M,
  Crushed = PRM_tidy$Crushed
)

# Model

model_PRM_crush <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b_C*Crushed,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_se),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_C ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = mod_list,
  start = list(M_est = mod_list$M_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 5000,
  control = list(adapt_delta = 0.90))

# Check model

check_energy(model_PRM_crush@stanfit)
check_treedepth(model_PRM_crush@stanfit)

divergent <- get_sampler_params(model_PRM_crush@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_PRM_crush, digits = 4, prob = 0.95, depth = 2)

# Save model

saveRDS(model_PRM_crush, "Outputs/04_Misc/02_Supplementary_Crush_Mill/PRM_Crush_model.rds")

#### All with Species ####

myct_tidy <- filter(myct, !is.na(mean_M))

mod_list <- list(
  M_obs = myct_tidy$mean_M,
  M_se = myct_tidy$se_M,
  Crushed = myct_tidy$Crushed,
  Species = myct_tidy$sciname
)

mod_tab <- as.data.frame(mod_list)

# Model

model_all_crush <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + a_Var[Species] + b_C*Crushed,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_se),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_C ~ dnorm(0, 1),
    a_Var[Species] ~ dnorm(0 , sigma_Species),
    sigma_Species ~ dcauchy(0, 1),
    sigma ~ dexp(1)
  ),
  data = mod_list,
  start = list(M_est = mod_list$M_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 5000,
  control = list(adapt_delta = 0.90))

# Check model

check_energy(model_all_crush@stanfit)
check_treedepth(model_all_crush@stanfit)

divergent <- get_sampler_params(model_all_crush@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_all_crush, digits = 4, prob = 0.95, depth = 2)

# Save model

saveRDS(model_all_crush, "Outputs/04_Misc/02_Supplementary_Crush_Mill/All_Crush_model.rds")


