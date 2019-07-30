#### Bayesian Linear Models - Weight ####

library(tidyverse)
library(rethinking)

myct <- read.csv("Outputs/Combined.csv")
glimpse(myct)

#### Overall Model with Weight ####

myct_tidy <- filter(myct, Weight_SD == "0")
myct_tidy$log10_Weight <- log10(myct_tidy$Weight.x)

M_W_list <- list(
  M_obs = myct_tidy$M,
  M_sd = myct_tidy$sd_M,
  Weight = myct_tidy$log10_Weight,
  Species = myct_tidy$sciname
)

model_M_W <- map2stan(
  alist(
    M_est ~ dnorm(mu, sigma),
    mu <- a + b*Weight + a_Var[Species],
    M_obs ~ dnorm(M_est, M_sd),
    a ~ dnorm(0.25, 1),
    b ~ dnorm(0, 1),
    a_Var[Species] ~ dnorm(0 , sigma_Species),
    sigma_Species ~ dunif(0, 1),
    sigma ~ dunif(0, 1)
  ),
data = M_W_list,
start = list(M_est = M_W_list$M_obs),
WAIC = FALSE,
iter = 10000,
warmup = 1000,
chains = 4,
cores = 4,
control = list(adapt_delta = 0.99))

## Precis Tables

precis(model_M_W)
precis(model_M_W, depth = 2)

## Check chains

plot(model_M_W)

## Plot

weight_seq <- seq(from = min(M_W_list$Weight), to = max(M_W_list$Weight), by = 0.01) # Horizontal axis
mu <- link(model_M_W, data = data.frame(Weight = weight_seq)) # Link model to mu

# Summarise the distribution of mu

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.95)

# Plot with model and raw data

par(mfrow = c(1,1))
plot(M_obs ~ Weight, data = M_W_list, pch = 16, col = Species, cex = 1.5)
lines(weight_seq, mu.mean)
shade(mu.HPDI, weight_seq)

#### Model with Species ####

## ELN

ELN <- filter(myct_tidy, Label == "ELN")

ELN_M_W_list <- list(
  M_obs = ELN$M,
  M_sd = ELN$sd_M,
  Weight = ELN$log10_Weight
)

ELN_M_W <- map2stan(
  alist(
    M_est ~ dnorm(mu, sigma),
    mu <- a + b*Weight,
    M_obs ~ dnorm(M_est, M_sd),
    a ~ dnorm(0.25, 1),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = ELN_M_W_list,
  start = list(M_est = ELN_M_W_list$M_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99))

## Precis Tables

precis(ELN_M_W)

## Check chains

plot(ELN_M_W)

## Plot

weight_seq <- seq(from = min(ELN_M_W_list$Weight), to = max(ELN_M_W_list$Weight), by = 0.001) # Horizontal axis
mu <- link(ELN_M_W, data = data.frame(Weight = weight_seq)) # Link model to mu

# Summarise the distribution of mu

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.95)

# Plot with model and raw data

par(mfrow = c(1,1))
par(mar=c(4,4,3,3))
plot(M_obs ~ Weight, data = ELN_M_W_list, pch = 16, cex = 1.5, ylim = c(0.17, 0.36), xlab = "", ylab = "", tck = -0.01)
mtext(text = "log10(Weight) (g)", side = 1, line = 2)
mtext(text = "M Value", side = 2, line = 2)
with(ELN_M_W_list, arrows(Weight, M_obs - M_sd, Weight, M_obs + M_sd, length = 0.05, angle = 90 , code = 3))
lines(weight_seq, mu.mean, lwd = 3)
shade(mu.HPDI, weight_seq)

## ELC

ELC <- filter(myct_tidy, Label == "ELC")

ELC_M_W_list <- list(
  M_obs = ELC$M,
  M_sd = ELC$sd_M,
  Weight = ELC$log10_Weight
)

ELC_M_W <- map2stan(
  alist(
    M_est ~ dnorm(mu, sigma),
    mu <- a + b*Weight,
    M_obs ~ dnorm(M_est, M_sd),
    a ~ dnorm(0.25, 1),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = ELC_M_W_list,
  start = list(M_est = ELC_M_W_list$M_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99))

## Precis Tables

precis(ELC_M_W)

## Check chains

plot(ELC_M_W)

# Plot with model and raw data

par(mfrow = c(1,1))
par(mar=c(4,4,3,3))
plot(M_obs ~ Weight, data = ELC_M_W_list, pch = 16, cex = 1.5, xlab = "", ylab = "", xlim = c(0.65, 0.95), ylim = c(0.15, 0.25), tck = -0.01)
mtext(text = "log10(Weight) (g)", side = 1, line = 2)
mtext(text = "M Value", side = 2, line = 2)
with(ELC_M_W_list, arrows(Weight, M_obs - M_sd, Weight, M_obs + M_sd, length = 0.05, angle = 90 , code = 3))

## GYR

GYR <- filter(myct_tidy, Label == "GYR")

GYR_M_W_list <- list(
  M_obs = GYR$M,
  M_sd = GYR$sd_M,
  Weight = GYR$log10_Weight
)

GYR_M_W <- map2stan(
  alist(
    M_est ~ dnorm(mu, sigma),
    mu <- a + b*Weight,
    M_obs ~ dnorm(M_est, M_sd),
    a ~ dnorm(0.25, 1),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = GYR_M_W_list,
  start = list(M_est = GYR_M_W_list$M_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99))

## Precis Tables

precis(GYR_M_W)

## Check chains

plot(GYR_M_W)

## Plot

weight_seq <- seq(from = min(GYR_M_W_list$Weight), to = max(GYR_M_W_list$Weight), by = 0.001) # Horizontal axis
mu <- link(GYR_M_W, data = data.frame(Weight = weight_seq)) # Link model to mu

# Summarise the distribution of mu

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.95)

# Plot with model and raw data

par(mfrow = c(1,1))
par(mar=c(4,4,3,3))
plot(M_obs ~ Weight, data = GYR_M_W_list, pch = 16, cex = 1.5, xlab = "", ylab = "", ylim = c(0.17, 0.35), tck = -0.01)
mtext(text = "log10(Weight) (g)", side = 1, line = 2)
mtext(text = "M Value", side = 2, line = 2)
with(GYR_M_W_list, arrows(Weight, M_obs - M_sd, Weight, M_obs + M_sd, length = 0.05, angle = 90 , code = 3))
lines(weight_seq, mu.mean, lwd = 3)
shade(mu.HPDI, weight_seq)

## GYN

GYN <- filter(myct_tidy, Label == "GYN")

GYN_M_W_list <- list(
  M_obs = GYN$M,
  M_sd = GYN$sd_M,
  Weight = GYN$log10_Weight
)

GYN_M_W <- map2stan(
  alist(
    M_est ~ dnorm(mu, sigma),
    mu <- a + b*Weight,
    M_obs ~ dnorm(M_est, M_sd),
    a ~ dnorm(0.25, 1),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = GYN_M_W_list,
  start = list(M_est = GYN_M_W_list$M_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99))

## Precis Tables

precis(GYN_M_W)

## Check chains

plot(GYN_M_W)

## Plot

weight_seq <- seq(from = min(GYN_M_W_list$Weight), to = max(GYN_M_W_list$Weight), by = 0.001) # Horizontal axis
mu <- link(GYN_M_W, data = data.frame(Weight = weight_seq)) # Link model to mu

# Summarise the distribution of mu

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.95)

# Plot with model and raw data

par(mfrow = c(1,1))
par(mar=c(4,4,3,3))
plot(M_obs ~ Weight, data = GYN_M_W_list, pch = 16, cex = 1.5, xlab = "", ylab = "", ylim = c(0.12, 0.25), tck = -0.01)
mtext(text = "log10(Weight) (g)", side = 1, line = 2)
mtext(text = "M Value", side = 2, line = 2)
with(GYN_M_W_list, arrows(Weight, M_obs - M_sd, Weight, M_obs + M_sd, length = 0.05, angle = 90 , code = 3))
lines(weight_seq, mu.mean, lwd = 3)
shade(mu.HPDI, weight_seq)

## KRA

KRA <- filter(myct_tidy, Label == "KRA")

KRA_M_W_list <- list(
  M_obs = KRA$M,
  M_sd = KRA$sd_M,
  Weight = KRA$log10_Weight
)

KRA_M_W <- map2stan(
  alist(
    M_est ~ dnorm(mu, sigma),
    mu <- a + b*Weight,
    M_obs ~ dnorm(M_est, M_sd),
    a ~ dnorm(0.25, 1),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = KRA_M_W_list,
  start = list(M_est = KRA_M_W_list$M_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99))

## Precis Tables

precis(KRA_M_W)

## Check chains

plot(KRA_M_W)

# Plot with model and raw data

par(mfrow = c(1,1))
par(mar=c(4,4,3,3))
plot(M_obs ~ Weight, data = KRA_M_W_list, pch = 16, cex = 1.5, xlab = "", ylab = "", ylim = c(0.18, 0.35), tck = -0.01)
mtext(text = "log10(Weight) (g)", side = 1, line = 2)
mtext(text = "M Value", side = 2, line = 2)
with(KRA_M_W_list, arrows(Weight, M_obs - M_sd, Weight, M_obs + M_sd, length = 0.05, angle = 90 , code = 3))

## PRM

PRM <- filter(myct_tidy, Label == "PRM")

PRM_M_W_list <- list(
  M_obs = PRM$M,
  M_sd = PRM$sd_M,
  Weight = PRM$log10_Weight
)

PRM_M_W <- map2stan(
  alist(
    M_est ~ dnorm(mu, sigma),
    mu <- a + b*Weight,
    M_obs ~ dnorm(M_est, M_sd),
    a ~ dnorm(0.25, 1),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = PRM_M_W_list,
  start = list(M_est = PRM_M_W_list$M_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99))

## Precis Tables

precis(PRM_M_W)

## Check chains

plot(PRM_M_W)

## Plot

weight_seq <- seq(from = min(PRM_M_W_list$Weight), to = max(PRM_M_W_list$Weight), by = 0.001) # Horizontal axis
mu <- link(PRM_M_W, data = data.frame(Weight = weight_seq)) # Link model to mu

# Summarise the distribution of mu

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.95)

# Plot with model and raw data

par(mfrow = c(1,1))
par(mar=c(4,4,3,3))
plot(M_obs ~ Weight, data = PRM_M_W_list, pch = 16, cex = 1.5, xlab = "", ylab = "", ylim = c(0.13, 0.3), tck = -0.01)
mtext(text = "log10(Weight) (g)", side = 1, line = 2)
mtext(text = "M Value", side = 2, line = 2)
with(PRM_M_W_list, arrows(Weight, M_obs - M_sd, Weight, M_obs + M_sd, length = 0.05, angle = 90 , code = 3))
lines(weight_seq, mu.mean, lwd = 3)
shade(mu.HPDI, weight_seq)
