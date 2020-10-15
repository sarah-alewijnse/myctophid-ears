#### Bayesian Linear Models - Body Mass ####

library(tidyverse)
library(rethinking)
library(bayesplot)

options(max.print=999999)

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")
glimpse(myct)

#### Overall Model with Weight and Temp ####

myct_tidy <- filter(myct, Weight_SD == "0")
myct_tidy <- filter(myct, !is.na(mean_M))
myct_tidy$log_Weight <- log(myct_tidy$Weight.x)
glimpse(myct_tidy)

M_W_list <- list(
  M_obs = myct_tidy$mean_M,
  M_se = myct_tidy$se_M,
  Weight = myct_tidy$log_Weight,
  Species = myct_tidy$sciname
)

## Convert to z-scores

# Weight

Weight_mean <- mean(M_W_list$Weight)
Weight_sd <- sd(M_W_list$Weight)

for(i in 1:length(M_W_list$Weight)){
  M_W_list$Weight_Z[i] <- (M_W_list$Weight[i] - Weight_mean) / Weight_sd
}

# Tidy list

mod_list <- list(
  M_obs = M_W_list$M_obs,
  M_se = M_W_list$M_se,
  Weight = M_W_list$Weight_Z,
  Species = M_W_list$Species
)

mod_tab <- as.data.frame(mod_list)

## Model

model_M_W <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + a_Var[Species] +
      b_W*Weight,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_se),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_W ~ dnorm(0, 1),
    a_Var[Species] ~ dnorm(0 , sigma_Species),
    sigma_Species ~ dcauchy(0, 1),
    sigma ~ dexp(1)
  ),
  data = mod_list,
  start = list(M_est = mod_list$M_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 5000)

## Run diagnostics

check_energy(model_M_W@stanfit)
check_treedepth(model_M_W@stanfit)

divergent <- get_sampler_params(model_M_W@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_M_W, digits = 4, prob = 0.95, depth = 2)

## Save stanfit

saveRDS(model_M_W, "Outputs/02_Linear_Models_Among_Species/02_M_Body_Mass/M_W_model.rds")


