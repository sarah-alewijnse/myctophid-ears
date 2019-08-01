#### Bayesian Linear Models - Respiration ####

library(tidyverse)
library(rethinking)

myct <- read.csv("Outputs/Combined.csv")
myct_tidy <- filter(myct, Weight_SD == "0")
myct_tidy$ln_Weight <- log(myct_tidy$Weight.x)
resp <- read.csv("Outputs/Belcher_MR.csv")
myct_tidy <- cbind(myct_tidy, M_values)

#### Overall Model with Temperature ####

M_R_list <- list(
  M_obs = myct_tidy$M,
  M_sd = myct_tidy$sd_M,
  Resp_obs = myct_tidy$MR,
  Resp_sd = myct_tidy$sd_MR,
  Species = myct_tidy$sciname
)

model_M_R <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    mu <- a +
      b*Resp_est[i] +
      a_Var[Species],
    M_obs ~ dnorm(M_est, M_sd),
    Resp_obs ~ dnorm(Resp_est, Resp_sd),
    a ~ dnorm(0.25, 1),
    b ~ dnorm(0, 10),
    a_Var[Species] ~ dnorm(0 , sigma_Species),
    sigma_Species ~ dunif(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = M_R_list,
  start = list(M_est = M_R_list$M_obs,
               Resp_est = M_R_list$Resp_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99))

## Precis Tables

precis(model_M_R)
precis(model_M_R, depth = 2, digits = 4)

## Check chains

plot(model_M_R)
