#### Bayesian Linear Models - Temperature ####

library(tidyverse)
library(rethinking)

myct <- read.csv("Outputs/Combined.csv")
glimpse(myct)

#### Overall Model with Temperature ####

M_T_list <- list(
  M_obs = myct$M,
  M_sd = myct$sd_M,
  Temp_obs = myct$temp,
  Temp_sd = myct$sd_temp,
  Species = myct$sciname
)

model_M_T <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    mu <- a +
      b*Temp_est[i] +
      a_Var[Species],
    M_obs ~ dnorm(M_est, M_sd),
    Temp_obs ~ dnorm(Temp_est, Temp_sd),
    a ~ dnorm(0.25, 1),
    b ~ dnorm(0, 1),
    a_Var[Species] ~ dnorm(0 , sigma_Species),
    sigma_Species ~ dunif(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = M_T_list,
  start = list(M_est = M_T_list$M_obs,
               Temp_est = M_T_list$Temp_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99))

## Precis Tables

precis(model_M_T)
precis(model_M_W, depth = 2)

## Check chains

plot(model_M_T)

#### Overall Model with Temperature ####

## ELN

ELN <- filter(myct, Label == "ELN")

ELN_M_T_list <- list(
  M_obs = ELN$M,
  M_sd = ELN$sd_M,
  Temp_obs = ELN$temp,
  Temp_sd = ELN$sd_temp
)

ELN_M_T <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    mu <- a +
      b*Temp_est[i],
    M_obs ~ dnorm(M_est, M_sd),
    Temp_obs ~ dnorm(Temp_est, Temp_sd),
    a ~ dnorm(0.25, 1),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = ELN_M_T_list,
  start = list(M_est = ELN_M_T_list$M_obs,
               Temp_est = ELN_M_T_list$Temp_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99))

## Precis Tables

precis(ELN_M_T)

## Check chains

plot(ELN_M_T)

## Plot

temp_seq <- seq(from = min(ELN_M_T_list$Temp_obs), to = max(ELN_M_T_list$Temp_obs), by = 0.001) # Horizontal axis
mu <- link(ELN_M_T, data = data.frame(Temp_est = temp_seq)) # Link model to mu

# Summarise the distribution of mu

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.95)

# Plot with model and raw data

par(mfrow = c(1,1))
par(mar=c(4,4,3,3))
plot(M_obs ~ Temp_obs, data = ELN_M_T_list, pch = 16, cex = 1.5, xlab = "", ylab = "", xlim = c(-3.7, 2.6), ylim = c(0.17, 0.35), tck = -0.01)
mtext(text = expression(paste("Temperature (", degree, "C)")), side = 1, line = 2)
mtext(text = "M Value", side = 2, line = 2)
with(ELN_M_T_list, arrows(Temp_obs, M_obs - M_sd, Temp_obs, M_obs + M_sd, length = 0.05, angle = 90 , code = 3))
with(ELN_M_T_list, arrows(Temp_obs - Temp_sd, M_obs, Temp_obs + Temp_sd, M_obs, length = 0.05, angle = 90, code = 3))
lines(temp_seq, mu.mean, lwd = 3)
shade(mu.HPDI, temp_seq)

