#### Rethinking Temperature ####

setwd("~/PhD/GitHub/mytophid-ears/MixSIAR")

library(tidyverse)
library(rethinking)

### Load and partition data

myct <- read.csv("Myctophids_Master.csv")
myct_1 <- filter(myct, MyNumber == "BAS_32")

## Try in JAGS

library(rjags)
library(coda)

iso_list <- list(
  iso = myct_1$d18O - myct_1$D18O_val,
  a_obs = 3.90,
  a_sd = 0.24,
  b_obs = -0.20,
  b_sd = 0.019,
  N = 1
)

inits <- list(Temp = 0.0)

cat("model
    {
    for (i in 1:N){
    iso[i] ~ dnorm(mu[i], sigma)
    mu[i] <- a_est - b_est * Temp
    }
    a_est ~ dnorm(a_obs, a_sd)
    b_est ~ dnorm(b_obs, b_sd)
    Temp ~ dunif(-10, 10)
    sigma ~ dunif(0, 1)
    }", file="Temp_Jags.txt")

jags_mod <- jags.model(file = "Temp_Jags.txt", data = iso_list, inits = inits, n.chains = 1, n.adapt = 500)

output <- coda.samples(jags_mod,
                       "Temp",
                       n.iter = 1000,
                       thin = 1)
summary(output)
