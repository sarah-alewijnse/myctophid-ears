#### Model Comparison ####

library(tidyverse)
library(rethinking)

## Load data

myct <- read.csv("Outputs/Combined.csv")

## Create models

## Model 1: M ~ Temperature

set.seed(1)
results <- replicate(1000, {
  values_T <- data.frame()
  values_S <- data.frame()
  
  for(i in 1:nrow(myct)){
    
    # Get values
    val_T <- with(myct[i,], rnorm(1, M, M_HDI_range))
    sci <- with(myct[i,], rnorm(1, temp, temp_HDI_range))
    values_T <- rbind(values_T, val_T)
    values_S <- rbind(values_S, sci)
    values <- cbind(values_S, values_T)
    colnames(values) <- c("temp", "M")
  }
  
  # Do test
  
  model_1 <- map(
    alist(
      M ~ dnorm(mu, sigma),
      mu <- a + b * temp,
      a ~ dnorm(-0.5, 10),
      b ~ dnorm(0.75, 1),
      sigma ~ dunif(0, 10)
    ),
    data = values)
  
  coefs <- as.data.frame(model_1@coef)
  colnames(coefs) <- "Number"
  W <- WAIC(model_1)
  W <- as.data.frame(W)
  rownames(W) <- "WAIC"
  colnames(W) <- "Number"
  data <- rbind(coefs, W)
  return(data)
})

results <- as.data.frame(results)
results_t <- t(results)
results_t <- as.data.frame(results_t)
colnames(results_t) <- c("a", "b", "sigma", "WAIC")

write.csv(results_t, "Outputs/Bayesian_M_Temp.csv")

# Highest density for a

max <- which.max(density(results_t$a)$y)
density(results_t$a)$x[max]

hdi(results_t$a, credMass = 0.95)

# Highest density for b

max <- which.max(density(results_t$b)$y)
density(results_t$b)$x[max]

hdi(results_t$b, credMass = 0.95)

# Highest density for sigma

max <- which.max(density(results_t$sigma)$y)
density(results_t$sigma)$x[max]

hdi(results_t$sigma, credMass = 0.95)

# Highest density for WAIC

max <- which.max(density(results_t$WAIC)$y)
density(results_t$WAIC)$x[max]

hdi(results_t$WAIC, credMass = 0.95)

## Model 1: M ~ Temperature

set.seed(1)
results <- replicate(1000, {
  values_T <- data.frame()
  values_S <- data.frame()
  
  for(i in 1:nrow(myct)){
    
    # Get values
    val_T <- with(myct[i,], rnorm(1, M, M_HDI_range))
    sci <- with(myct[i,], rnorm(1, temp, temp_HDI_range))
    values_T <- rbind(values_T, val_T)
    values_S <- rbind(values_S, sci)
    values <- cbind(values_S, values_T)
    colnames(values) <- c("temp", "M")
  }
  
  # Do test
  
  model_1 <- map(
    alist(
      M ~ dnorm(mu, sigma),
      mu <- a + b * temp,
      a ~ dnorm(-0.5, 10),
      b ~ dnorm(0.75, 1),
      sigma ~ dunif(0, 10)
    ),
    data = values)
  
  coefs <- as.data.frame(model_1@coef)
  colnames(coefs) <- "Number"
  W <- WAIC(model_1)
  W <- as.data.frame(W)
  rownames(W) <- "WAIC"
  colnames(W) <- "Number"
  data <- rbind(coefs, W)
  return(data)
})

results <- as.data.frame(results)
results_t <- t(results)
results_t <- as.data.frame(results_t)
colnames(results_t) <- c("a", "b", "sigma", "WAIC")

write.csv(results_t, "Outputs/Bayesian_M_Temp.csv")

# Highest density for a

max <- which.max(density(results_t$a)$y)
density(results_t$a)$x[max]

hdi(results_t$a, credMass = 0.95)

# Highest density for b

max <- which.max(density(results_t$b)$y)
density(results_t$b)$x[max]

hdi(results_t$b, credMass = 0.95)

# Highest density for sigma

max <- which.max(density(results_t$sigma)$y)
density(results_t$sigma)$x[max]

hdi(results_t$sigma, credMass = 0.95)

# Highest density for WAIC

max <- which.max(density(results_t$WAIC)$y)
density(results_t$WAIC)$x[max]

hdi(results_t$WAIC, credMass = 0.95)