#### Bayesian Test - Within Species ####

# Load required packages

library(tidyverse)
library(rethinking)
library(bayesplot)

options(max.print=999999) # Enables viewing of whole output

# Create dummy data

x <- seq(0, 100, 1)
x_var <- rep(c(0.001, 0.002, 0.003, 0.004), length.out = 101)
y <- seq(0, 100, 1)

dat <- data.frame(
  x_obs = x,
  x_var = x_var,
  y = y
)
glimpse(dat)

mod_example <- lm(dat$y ~ dat$x_obs)
summary(mod_example)

# Run model

model_test <- map2stan(
  alist(
    x_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + y*b_y,
    
    # Data uncertainties
    x_obs ~ dnorm(x_est, x_var),
    
    # Parameter priors
    a ~ dnorm(0, 1),
    b_y ~ dnorm(1, 1),
    sigma ~ dexp(1)
  ),
  data = dat,
  start = list(M_est = dat$x_obs),
  WAIC = FALSE,
  iter = 3000,
  warmup = 1500,
  control = list(adapt_delta = 0.90))

precis(model_test, depth = 2)

plot(dat$x_obs, dat$y)
abline(2.09,0.71)

post <- extract.samples(model_test)
post <- as.data.frame(post)

mean(post$M_est.2)

ests <- as.data.frame(colMeans(post))
ests_x <- as.data.frame(ests[1:101,])
ests_x <- data.frame(
  x_est = ests_x,
  y = y
)

colnames(ests_x) <- c("x_est", "y")

plot(ests_x$x_est, ests_x$y, col = "blue")
abline(2.09, 0.71)

# Do without variance

model_test <- map2stan(
  alist(
    x_obs ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + y*b_y,
    
    # Parameter priors
    a ~ dnorm(0, 1),
    b_y ~ dnorm(1, 1),
    sigma ~ dexp(1)
  ),
  data = dat,
  start = list(M_est = dat$x_obs),
  WAIC = FALSE,
  iter = 3000,
  warmup = 1500,
  control = list(adapt_delta = 0.90))

precis(model_test, depth = 2)
