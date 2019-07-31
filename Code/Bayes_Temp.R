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
precis(model_M_T, depth = 2, digits = 4)

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

precis(ELN_M_T, digits = 4)

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

## ELC

ELC <- filter(myct, Label == "ELC")

ELC_M_T_list <- list(
  M_obs = ELC$M,
  M_sd = ELC$sd_M,
  Temp_obs = ELC$temp,
  Temp_sd = ELC$sd_temp
)

ELC_M_T <- map2stan(
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
  data = ELC_M_T_list,
  start = list(M_est = ELC_M_T_list$M_obs,
               Temp_est = ELC_M_T_list$Temp_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99))

## Precis Tables

precis(ELC_M_T, digit = 4)

## Check chains

plot(ELC_M_T)

## Plot

temp_seq <- seq(from = min(ELC_M_T_list$Temp_obs), to = max(ELC_M_T_list$Temp_obs), by = 0.001) # Horizontal axis
mu <- link(ELC_M_T, data = data.frame(Temp_est = temp_seq)) # Link model to mu

# Summarise the distribution of mu

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.95)

# Plot with model and raw data

par(mfrow = c(1,1))
par(mar=c(4,4,3,3))
plot(M_obs ~ Temp_obs, data = ELC_M_T_list, pch = 16, cex = 1.5, xlab = "", ylab = "", xlim = c(-0.1, 5.3), ylim = c(0.15, 0.27), tck = -0.01)
mtext(text = expression(paste("Temperature (", degree, "C)")), side = 1, line = 2)
mtext(text = "M Value", side = 2, line = 2)
with(ELC_M_T_list, arrows(Temp_obs, M_obs - M_sd, Temp_obs, M_obs + M_sd, length = 0.05, angle = 90 , code = 3))
with(ELC_M_T_list, arrows(Temp_obs - Temp_sd, M_obs, Temp_obs + Temp_sd, M_obs, length = 0.05, angle = 90, code = 3))

## GYR

GYR <- filter(myct, Label == "GYR")

GYR_M_T_list <- list(
  M_obs = GYR$M,
  M_sd = GYR$sd_M,
  Temp_obs = GYR$temp,
  Temp_sd = GYR$sd_temp
)

GYR_M_T <- map2stan(
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
  data = GYR_M_T_list,
  start = list(M_est = GYR_M_T_list$M_obs,
               Temp_est = GYR_M_T_list$Temp_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99))

## Precis Tables

precis(GYR_M_T, digits = 4)

## Check chains

plot(GYR_M_T)

## Plot

temp_seq <- seq(from = min(GYR_M_T_list$Temp_obs), to = max(GYR_M_T_list$Temp_obs), by = 0.001) # Horizontal axis
mu <- link(GYR_M_T, data = data.frame(Temp_est = temp_seq)) # Link model to mu

# Summarise the distribution of mu

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.95)

# Plot with model and raw data

par(mfrow = c(1,1))
par(mar=c(4,4,3,3))
plot(M_obs ~ Temp_obs, data = GYR_M_T_list, pch = 16, cex = 1.5, xlab = "", ylab = "", xlim = c(-5, 1.5), ylim = c(0.18, 0.33), tck = -0.01)
mtext(text = expression(paste("Temperature (", degree, "C)")), side = 1, line = 2)
mtext(text = "M Value", side = 2, line = 2)
with(GYR_M_T_list, arrows(Temp_obs, M_obs - M_sd, Temp_obs, M_obs + M_sd, length = 0.05, angle = 90 , code = 3))
with(GYR_M_T_list, arrows(Temp_obs - Temp_sd, M_obs, Temp_obs + Temp_sd, M_obs, length = 0.05, angle = 90, code = 3))

## GYN

GYN <- filter(myct, Label == "GYN")

GYN_M_T_list <- list(
  M_obs = GYN$M,
  M_sd = GYN$sd_M,
  Temp_obs = GYN$temp,
  Temp_sd = GYN$sd_temp
)

GYN_M_T <- map2stan(
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
  data = GYN_M_T_list,
  start = list(M_est = GYN_M_T_list$M_obs,
               Temp_est = GYN_M_T_list$Temp_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99))

## Precis Tables

precis(GYN_M_T, digits = 4)

## Check chains

plot(GYN_M_T)

## Plot

temp_seq <- seq(from = min(GYN_M_T_list$Temp_obs), to = max(GYN_M_T_list$Temp_obs), by = 0.001) # Horizontal axis
mu <- link(GYN_M_T, data = data.frame(Temp_est = temp_seq)) # Link model to mu

# Summarise the distribution of mu

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.95)

# Plot with model and raw data

par(mfrow = c(1,1))
par(mar=c(4,4,3,3))
plot(M_obs ~ Temp_obs, data = GYN_M_T_list, pch = 16, cex = 1.5, xlab = "", ylab = "", xlim = c(-0.5, 5), ylim = c(0.12, 0.24), tck = -0.01)
mtext(text = expression(paste("Temperature (", degree, "C)")), side = 1, line = 2)
mtext(text = "M Value", side = 2, line = 2)
with(GYN_M_T_list, arrows(Temp_obs, M_obs - M_sd, Temp_obs, M_obs + M_sd, length = 0.05, angle = 90 , code = 3))
with(GYN_M_T_list, arrows(Temp_obs - Temp_sd, M_obs, Temp_obs + Temp_sd, M_obs, length = 0.05, angle = 90, code = 3))
lines(temp_seq, mu.mean, lwd = 3)
shade(mu.HPDI, temp_seq)

## KRA

KRA <- filter(myct, Label == "KRA")

KRA_M_T_list <- list(
  M_obs = KRA$M,
  M_sd = KRA$sd_M,
  Temp_obs = KRA$temp,
  Temp_sd = KRA$sd_temp
)

KRA_M_T <- map2stan(
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
  data = KRA_M_T_list,
  start = list(M_est = KRA_M_T_list$M_obs,
               Temp_est = KRA_M_T_list$Temp_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99))

## Precis Tables

precis(KRA_M_T, digits = 4)

## Check chains

plot(KRA_M_T)

## Plot

temp_seq <- seq(from = min(KRA_M_T_list$Temp_obs), to = max(KRA_M_T_list$Temp_obs), by = 0.001) # Horizontal axis
mu <- link(KRA_M_T, data = data.frame(Temp_est = temp_seq)) # Link model to mu

# Summarise the distribution of mu

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.95)

# Plot with model and raw data

par(mfrow = c(1,1))
par(mar=c(4,4,3,3))
plot(M_obs ~ Temp_obs, data = KRA_M_T_list, pch = 16, cex = 1.5, xlab = "", ylab = "", xlim = c(-2.2, 4.5), ylim = c(0.19, 0.34), tck = -0.01)
mtext(text = expression(paste("Temperature (", degree, "C)")), side = 1, line = 2)
mtext(text = "M Value", side = 2, line = 2)
with(KRA_M_T_list, arrows(Temp_obs, M_obs - M_sd, Temp_obs, M_obs + M_sd, length = 0.05, angle = 90 , code = 3))
with(KRA_M_T_list, arrows(Temp_obs - Temp_sd, M_obs, Temp_obs + Temp_sd, M_obs, length = 0.05, angle = 90, code = 3))
lines(temp_seq, mu.mean, lwd = 3)
shade(mu.HPDI, temp_seq)

## PRM

PRM <- filter(myct, Label == "PRM")

PRM_M_T_list <- list(
  M_obs = PRM$M,
  M_sd = PRM$sd_M,
  Temp_obs = PRM$temp,
  Temp_sd = PRM$sd_temp
)

PRM_M_T <- map2stan(
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
  data = PRM_M_T_list,
  start = list(M_est = PRM_M_T_list$M_obs,
               Temp_est = PRM_M_T_list$Temp_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99))

## Precis Tables

precis(PRM_M_T, digits = 4)

## Check chains

plot(PRM_M_T)

## Plot

temp_seq <- seq(from = min(PRM_M_T_list$Temp_obs), to = max(PRM_M_T_list$Temp_obs), by = 0.001) # Horizontal axis
mu <- link(PRM_M_T, data = data.frame(Temp_est = temp_seq)) # Link model to mu

# Summarise the distribution of mu

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.95)

# Plot with model and raw data

par(mfrow = c(1,1))
par(mar=c(4,4,3,3))
plot(M_obs ~ Temp_obs, data = PRM_M_T_list, pch = 16, cex = 1.5, xlab = "", ylab = "", xlim = c(-2.5, 3.2), ylim = c(0.12, 0.29), tck = -0.01)
mtext(text = expression(paste("Temperature (", degree, "C)")), side = 1, line = 2)
mtext(text = "M Value", side = 2, line = 2)
with(PRM_M_T_list, arrows(Temp_obs, M_obs - M_sd, Temp_obs, M_obs + M_sd, length = 0.05, angle = 90 , code = 3))
with(PRM_M_T_list, arrows(Temp_obs - Temp_sd, M_obs, Temp_obs + Temp_sd, M_obs, length = 0.05, angle = 90, code = 3))
lines(temp_seq, mu.mean, lwd = 3)
shade(mu.HPDI, temp_seq)
