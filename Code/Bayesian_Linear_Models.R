#### Bayesian Linear Models ####

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
    mu <- a + b*Weight,
    M_obs ~ dnorm(M_est, M_sd),
    a ~ dnorm(0.25, 1),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
data = M_W_list,
start = list(M_est = M_W_list$M_obs),
WAIC = FALSE,
iter = 5000,
warmup = 1000,
chains = 1,
cores = 1,
control = list(adapt_delta = 0.95))

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
  iter = 5000,
  warmup = 1000,
  chains = 1,
  cores = 1,
  control = list(adapt_delta = 0.95))

## Precis Tables

precis(PRM_M_W)
precis(PRM_M_W, depth = 2)

## Check chains

plot(PRM_M_W)

## Plot

weight_seq <- seq(from = min(PRM_M_W_list$Weight), to = max(PRM_M_W_list$Weight), by = 0.01) # Horizontal axis
mu <- link(PRM_M_W, data = data.frame(Weight = weight_seq)) # Link model to mu

# Summarise the distribution of mu

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.95)

# Plot with model and raw data

par(mfrow = c(1,1))
plot(M_obs ~ Weight, data = PRM_M_W_list, pch = 16, cex = 1.5)
lines(weight_seq, mu.mean)
shade(mu.HPDI, weight_seq)
