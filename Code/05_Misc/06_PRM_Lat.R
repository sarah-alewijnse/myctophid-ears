#### Bayesian Linear Models - PRM Latitude ####

library(tidyverse)
library(rethinking)
library(bayesplot)

options(max.print=999999)

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")
glimpse(myct)

#### Within PRM ####

PRM_tidy <- filter(myct, Label == "PRM")
glimpse(PRM_tidy)

# Z-score latitude

Lat_mean <- mean(PRM_tidy$Lat_dec.x.x)
Lat_sd <- sd(PRM_tidy$Lat_dec.x.x)

for(i in 1:length(PRM_tidy$Lat_dec.x.x)){
  PRM_tidy$Lat_Z[i] <- (PRM_tidy$Lat_dec.x.x[i] - Lat_mean) / Lat_sd
}

mod_list <- list(
  M_obs = PRM_tidy$mean_M,
  M_se = PRM_tidy$se_M,
  Lat = PRM_tidy$Lat_Z
)

# Model

model_PRM_lat <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b_L*Lat,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_se),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_L ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = mod_list,
  start = list(M_est = mod_list$M_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 5000,
  control = list(adapt_delta = 0.90))

# Check model

check_energy(model_PRM_lat@stanfit)
check_treedepth(model_PRM_lat@stanfit)

divergent <- get_sampler_params(model_PRM_lat@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_PRM_lat, digits = 4, prob = 0.95, depth = 2)

# Save model

saveRDS(model_PRM_lat, "Outputs/04_Misc/07_PRM_Lat/PRM_Lat_model.rds")

#### All with Species ####

myct_tidy <- filter(myct, !is.na(mean_M))

# Z-score latitude

Lat_mean <- mean(myct_tidy$Lat_dec.x.x)
Lat_sd <- sd(myct_tidy$Lat_dec.x.x)

for(i in 1:length(myct_tidy$Lat_dec.x.x)){
  myct_tidy$Lat_Z[i] <- (myct_tidy$Lat_dec.x.x[i] - Lat_mean) / Lat_sd
}

mod_list <- list(
  M_obs = myct_tidy$mean_M,
  M_se = myct_tidy$se_M,
  Lat = myct_tidy$Lat_Z,
  Species = myct_tidy$sciname
)

mod_tab <- as.data.frame(mod_list)

# Model

model_all_lat <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + a_Var[Species] + b_L*Lat,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_se),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_L ~ dnorm(0, 1),
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

check_energy(model_all_lat@stanfit)
check_treedepth(model_all_lat@stanfit)

divergent <- get_sampler_params(model_all_lat@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_all_lat, digits = 4, prob = 0.95, depth = 2)

# Save model

saveRDS(model_all_lat, "Outputs/04_Misc/07_PRM_Lat/All_Lat_model.rds")
