#### Bayesian Models - Species ####

library(tidyverse)
library(rethinking)

myct <- read.csv("Outputs/Combined.csv")
glimpse(myct)

#### Overall Model with Species ####

M_S_list <- list(
  M_obs = myct$M,
  M_sd = myct$sd_M,
  Species = myct$sciname
)

model_S <- map2stan(
  alist(
    M_est ~ dnorm(mu, sigma),
    mu <- a + a_Var[Species],
    M_obs ~ dnorm(M_est, M_sd),
    a ~ dnorm(0.25, 1),
    a_Var[Species] ~ dnorm(0, sigma_Species),
    sigma_Species ~ dunif(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = M_S_list,
  start = list(M_est = M_S_list$M_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99))

## Precis Tables

precis(model_S)
precis(model_S, depth = 2, digits = 4)

## Check chains

plot(model_S)

#### Overall Model with Group ####

M_S_list <- list(
  M_obs = myct$M,
  M_sd = myct$sd_M,
  Species = myct$sciname,
  Group = myct$Group
)

model_S <- map2stan(
  alist(
    M_est ~ dnorm(mu, sigma),
    mu <- a + a_Var[Group],
    M_obs ~ dnorm(M_est, M_sd),
    a ~ dnorm(0.25, 1),
    a_Var[Group] ~ dnorm(0, sigma_Group),
    sigma_Group ~ dunif(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = M_S_list,
  start = list(M_est = M_S_list$M_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99))

## Precis Tables

precis(model_S)
precis(model_S, depth = 2, digits = 4)

## Check chains

plot(model_S)
