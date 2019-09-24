#### Rethinking Temperature ####

setwd("~/PhD/GitHub/mytophid-ears/MixSIAR")

library(tidyverse)
library(rjags)
library(coda)

### Load and partition data

myct <- read.csv("Myctophids_Master.csv")
myct$dif <- myct$d18O - myct$D18O_vals
myct_1 <- filter(myct, MyNumber == "BAS_111")


## Try in JAGS

iso_list <- list(
  iso = myct_1$dif,
  sigma = 0.33,
  a_obs = 3.90,
  a_var = 1/(0.24^2),
  b_obs = -0.20,
  b_var = 1/(0.019^2),
  N = 1
)

inits <- list(Temp = 0.0)

cat("model
    {
    for (i in 1:N){
    mu[i] <- a_est + b_est * Temp
    iso[i] ~ dnorm(mu[i], tau)
    }
    a_est ~ dnorm(a_obs, a_var)
    b_est ~ dnorm(b_obs, b_var)
    Temp ~ dnorm(0, 0.04)
    tau <- 1/(sigma^2)
    }", file="Temp_Jags.txt")

jags_mod <- jags.model(file = "Temp_Jags.txt", data = iso_list, inits = inits, n.chains = 4, n.adapt = 5000)

output <- coda.samples(jags_mod,
                       c("Temp", "a_est", "b_est"),
                       n.iter = 10000,
                       thin = 10)
summary(output)
plot(output)
