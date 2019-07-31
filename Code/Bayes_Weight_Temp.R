#### Bayesian Linear Models - Weight and Temp ####

library(tidyverse)
library(rethinking)

myct <- read.csv("Outputs/Combined.csv")
glimpse(myct)

#### Overall Model with Weight and Temp ####

myct_tidy <- filter(myct, Weight_SD == "0")
myct_tidy$log10_Weight <- log10(myct_tidy$Weight.x)

M_T_W_list <- list(
  M_obs = myct_tidy$M,
  M_sd = myct_tidy$sd_M,
  Weight = myct_tidy$log10_Weight,
  Temp_obs = myct_tidy$temp,
  Temp_sd = myct_tidy$sd_temp,
  Species = myct_tidy$sciname
)

model_M_T_W <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    mu <- a +
      b_W*Weight +
      b_T*Temp_est[i] +
      a_Var[Species],
    M_obs ~ dnorm(M_est, M_sd),
    Temp_obs ~ dnorm(Temp_est, Temp_sd),
    a ~ dnorm(0.25, 1),
    b_W ~ dnorm(0, 1),
    b_T ~ dnorm(0, 1),
    a_Var[Species] ~ dnorm(0 , sigma_Species),
    sigma_Species ~ dunif(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = M_T_W_list,
  start = list(M_est = M_T_list$M_obs,
               Temp_est = M_T_list$Temp_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 1000,
  chains = 1,
  cores = 1,
  control = list(adapt_delta = 0.99))

## Precis Tables

precis(model_M_T)
precis(model_M_W, depth = 2)

## Check chains

plot(model_M_T)