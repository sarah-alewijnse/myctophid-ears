#### Bayesian Linear Models - Year within Spp. ####

# Load required packages

library(tidyverse)
library(rethinking) # Used to interface with rstan
library(bayesplot) # Gives nice plots

# Print out all results

options(max.print=999999)

# Load and check data

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")
glimpse(myct)

# Create dummy variable for year

# Create new year column

myct$Year <- 0
myct$Year[myct$Year.x == "1998"] <- -1
myct$Year[myct$Year.x == "2016"] <- 1

#### Within ELN ####

ELN_tidy <- filter(myct, Label == "ELN")
glimpse(ELN_tidy)

# Tidy into list

mod_list <- list(
  M_obs = ELN_tidy$mean_M,
  M_se = ELN_tidy$se_M,
  Year = ELN_tidy$Year
)
str(mod_list)

# Model

model_ELN_year <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b_Y*Year,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_se),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_Y ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = mod_list,
  start = list(M_est = mod_list$M_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 5000,
  control = list(adapt_delta = 0.90))

pairs(model_ELN_year, pars = c("a", "b_Y", "sigma"))

# Check model

check_energy(model_ELN_year@stanfit)
check_treedepth(model_ELN_year@stanfit)

divergent <- get_sampler_params(model_ELN_year@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_ELN_year, digits = 4, prob = 0.95, depth = 2)

# Save model

saveRDS(model_ELN_year, "Outputs/04_Misc/03_Within_Spp/01_Year/ELN_Year_model.rds")

#### Within ELC ####

# Subset to just ELC

ELC_tidy <- filter(myct, Label == "ELC")
glimpse(ELC_tidy)

# Tidy into list

mod_list <- list(
  M_obs = ELC_tidy$mean_M,
  M_se = ELC_tidy$se_M,
  Year = ELC_tidy$Year
)
str(mod_list)

# Model

model_ELC_year <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b_Y*Year,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_se),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_Y ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = mod_list,
  start = list(M_est = mod_list$M_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 5000,
  control = list(adapt_delta = 0.90))

pairs(model_ELC_year, pars = c("a", "b_Y", "sigma"))

# Check model

check_energy(model_ELC_year@stanfit)
check_treedepth(model_ELC_year@stanfit)

divergent <- get_sampler_params(model_ELC_year@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_ELC_year, digits = 4, prob = 0.95, depth = 2)

# Save model

saveRDS(model_ELC_year, "Outputs/04_Misc/03_Within_Spp/01_Year/ELC_Year_model.rds")


#### Within GYR ####

# Subset to just GYR

GYR_tidy <- filter(myct, Label == "GYR")
glimpse(GYR_tidy)

# Tidy into list

mod_list <- list(
  M_obs = GYR_tidy$mean_M,
  M_se = GYR_tidy$se_M,
  Year = GYR_tidy$Year
)
str(mod_list)

# Model

model_GYR_year <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b_Y*Year,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_se),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_Y ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = mod_list,
  start = list(M_est = mod_list$M_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 5000,
  control = list(adapt_delta = 0.90))

pairs(model_GYR_year, pars = c("a", "b_Y", "sigma"))

# Check model

check_energy(model_GYR_year@stanfit)
check_treedepth(model_GYR_year@stanfit)

divergent <- get_sampler_params(model_GYR_year@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_GYR_year, digits = 4, prob = 0.95, depth = 2)

# Save model

saveRDS(model_GYR_year, "Outputs/04_Misc/03_Within_Spp/01_Year/GYR_Year_model.rds")

#### Within GYN ####

# Subset to just GYN

GYN_tidy <- filter(myct, Label == "GYN")
glimpse(GYN_tidy)

# Tidy into list

mod_list <- list(
  M_obs = GYN_tidy$mean_M,
  M_se = GYN_tidy$se_M,
  Year = GYN_tidy$Year
)
str(mod_list)

# Model

model_GYN_year <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b_Y*Year,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_se),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_Y ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = mod_list,
  start = list(M_est = mod_list$M_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 5000,
  control = list(adapt_delta = 0.90))

pairs(model_GYN_year, pars = c("a", "b_Y", "sigma"))

# Check model

check_energy(model_GYN_year@stanfit)
check_treedepth(model_GYN_year@stanfit)

divergent <- get_sampler_params(model_GYN_year@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_GYN_year, digits = 4, prob = 0.95, depth = 2)

# Save model

saveRDS(model_GYN_year, "Outputs/04_Misc/03_Within_Spp/01_Year/GYN_Year_model.rds")

#### Within KRA ####

# Subset to just KRA

KRA_tidy <- filter(myct, Label == "KRA")
glimpse(KRA_tidy)

# Tidy into list

mod_list <- list(
  M_obs = KRA_tidy$mean_M,
  M_se = KRA_tidy$se_M,
  Year = KRA_tidy$Year
)
str(mod_list)

# Model

model_KRA_year <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b_Y*Year,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_se),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_Y ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = mod_list,
  start = list(M_est = mod_list$M_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 5000,
  control = list(adapt_delta = 0.90))

pairs(model_KRA_year, pars = c("a", "b_Y", "sigma"))

# Check model

check_energy(model_KRA_year@stanfit)
check_treedepth(model_KRA_year@stanfit)

divergent <- get_sampler_params(model_KRA_year@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_KRA_year, digits = 4, prob = 0.95, depth = 2)

# Save model

saveRDS(model_KRA_year, "Outputs/04_Misc/03_Within_Spp/01_Year/KRA_Year_model.rds")

#### Within PRM ####

# Subset to just PRM

PRM_tidy <- filter(myct, Label == "PRM")
glimpse(PRM_tidy)

# Tidy into list

mod_list <- list(
  M_obs = PRM_tidy$mean_M,
  M_se = PRM_tidy$se_M,
  Year = PRM_tidy$Year
)
str(mod_list)

# Model

model_PRM_year <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b_Y*Year,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_se),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_Y ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = mod_list,
  start = list(M_est = mod_list$M_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 5000,
  control = list(adapt_delta = 0.90))

pairs(model_PRM_year, pars = c("a", "b_Y", "sigma"))

# Check model

check_energy(model_PRM_year@stanfit)
check_treedepth(model_PRM_year@stanfit)

divergent <- get_sampler_params(model_PRM_year@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_PRM_year, digits = 4, prob = 0.95, depth = 2)

# Save model

saveRDS(model_PRM_year, "Outputs/04_Misc/03_Within_Spp/01_Year/PRM_Year_model.rds")


