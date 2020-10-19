#### Bayesian Linear Models - PRM Years ####

library(tidyverse)
library(rethinking)
library(bayesplot)

options(max.print=999999)

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")
glimpse(myct)

#### Within PRM ####

PRM_tidy <- filter(myct, Label == "PRM")
glimpse(PRM_tidy)

PRM_tidy$Year <- ifelse(PRM_tidy$Year.x == "2008", 0, 1)

mod_list <- list(
  M_obs = PRM_tidy$mean_M,
  M_se = PRM_tidy$se_M,
  Year = PRM_tidy$Year
)

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

# Check model

check_energy(model_PRM_year@stanfit)
check_treedepth(model_PRM_year@stanfit)

divergent <- get_sampler_params(model_PRM_year@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_PRM_year, digits = 4, prob = 0.95, depth = 2)

# Save model

saveRDS(model_PRM_year, "Outputs/04_Misc/04_PRM_Year/PRM_Year_model.rds")

#### All with Species ####

myct_tidy <- filter(myct, !is.na(mean_M))
myct_tidy <- filter(myct_tidy, Year.x != "1998")

myct_tidy$Year <- ifelse(myct_tidy$Year.x == "2008", 0, 1)

mod_list <- list(
  M_obs = myct_tidy$mean_M,
  M_se = myct_tidy$se_M,
  Year = myct_tidy$Year,
  Species = myct_tidy$sciname
)

mod_tab <- as.data.frame(mod_list)

# Model

model_all_year <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + a_Var[Species] + b_Y*Year,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_se),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_Y ~ dnorm(0, 1),
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

check_energy(model_all_year@stanfit)
check_treedepth(model_all_year@stanfit)

divergent <- get_sampler_params(model_all_year@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_all_year, digits = 4, prob = 0.95, depth = 2)

# Save model

saveRDS(model_all_year, "Outputs/04_Misc/04_PRM_Year/All_Year_model.rds")
