#### Size Analysis - Within Species ####

library(tidyverse)
library(rethinking) # Used to interface with rstan
library(bayesplot) # Gives nice plots

# Print out all results

options(max.print=999999)

# Read in data

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")

#### Length Ratio ####

# Create a table of max length (SL, mm)

max_SL <- data.frame(Label = c("ELN", "ELC", "GYR", "GYN", "KRA", "PRM"),
                     max_SL = c("115", "93", "162", "165", "74", "66"))

# Filter out crushed otoliths

myct <- filter(myct, Crushed == 0)

# Join to data

myct_SL <- left_join(myct, max_SL, by = "Label")
str(myct_SL)

# Get ratio

myct_SL$Ratio <- myct_SL$SL / as.numeric(myct_SL$max_SL)

myct_tidy <- filter(myct_SL, !is.na(mean_M))
myct_tidy <- filter(myct_tidy, !is.na(Ratio))

# Quick plot

plot(myct_tidy$Ratio, myct_tidy$mean_M)

##### ELN ####

ELN <- filter(myct_tidy, Label == "ELN")

ELN_list <- list(
  M_obs = ELN$mean_M,
  M_se = ELN$se_M,
  Ratio = ELN$Ratio
)

str(ELN_list)

# Model

model_ELN_ratio <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b_R*Ratio,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_se),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_R ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = ELN_list,
  start = list(M_est = ELN_list$M_obs),
  WAIC = FALSE,
  iter = 20000,
  warmup = 10000,
  control = list(adapt_delta = 0.90))

# Check model

check_energy(model_ELN_ratio@stanfit)
check_treedepth(model_ELN_ratio@stanfit)

divergent <- get_sampler_params(model_ELN_ratio@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_ELN_ratio, digits = 4, prob = 0.95, depth = 2)
plot(precis(model_ELN_ratio))

# Save model

saveRDS(model_ELN_ratio, "Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/ELN/Length_Ratio_Cresp.rds")

##### ELC ####

ELC <- filter(myct_tidy, Label == "ELC")

ELC_list <- list(
  M_obs = ELC$mean_M,
  M_se = ELC$se_M,
  Ratio = ELC$Ratio
)

str(ELC_list)

# Model

model_ELC_ratio <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b_R*Ratio,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_se),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_R ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = ELC_list,
  start = list(M_est = ELC_list$M_obs),
  WAIC = FALSE,
  iter = 20000,
  warmup = 10000,
  control = list(adapt_delta = 0.90))

# Check model

check_energy(model_ELC_ratio@stanfit)
check_treedepth(model_ELC_ratio@stanfit)

divergent <- get_sampler_params(model_ELC_ratio@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_ELC_ratio, digits = 4, prob = 0.95, depth = 2)
plot(precis(model_ELC_ratio))

# Save model

saveRDS(model_ELC_ratio, "Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/ELC/Length_Ratio_Cresp.rds")

##### GYR ####

GYR <- filter(myct_tidy, Label == "GYR")

GYR_list <- list(
  M_obs = GYR$mean_M,
  M_se = GYR$se_M,
  Ratio = GYR$Ratio
)

str(GYR_list)

# Model

model_GYR_ratio <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b_R*Ratio,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_se),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_R ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = GYR_list,
  start = list(M_est = GYR_list$M_obs),
  WAIC = FALSE,
  iter = 20000,
  warmup = 10000,
  control = list(adapt_delta = 0.90))

# Check model

check_energy(model_GYR_ratio@stanfit)
check_treedepth(model_GYR_ratio@stanfit)

divergent <- get_sampler_params(model_GYR_ratio@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_GYR_ratio, digits = 4, prob = 0.95, depth = 2)
plot(precis(model_GYR_ratio))

# Save model

saveRDS(model_GYR_ratio, "Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/GYR/Length_Ratio_Cresp.rds")

##### GYN ####

GYN <- filter(myct_tidy, Label == "GYN")

GYN_list <- list(
  M_obs = GYN$mean_M,
  M_se = GYN$se_M,
  Ratio = GYN$Ratio
)

str(GYN_list)

# Model

model_GYN_ratio <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b_R*Ratio,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_se),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_R ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = GYN_list,
  start = list(M_est = GYN_list$M_obs),
  WAIC = FALSE,
  iter = 20000,
  warmup = 10000,
  control = list(adapt_delta = 0.90))

# Check model

check_energy(model_GYN_ratio@stanfit)
check_treedepth(model_GYN_ratio@stanfit)

divergent <- get_sampler_params(model_GYN_ratio@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_GYN_ratio, digits = 4, prob = 0.95, depth = 2)
plot(precis(model_GYN_ratio))

# Save model

saveRDS(model_GYN_ratio, "Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/GYN/Length_Ratio_Cresp.rds")

##### PRM ####

PRM <- filter(myct_tidy, Label == "PRM")

PRM_list <- list(
  M_obs = PRM$mean_M,
  M_se = PRM$se_M,
  Ratio = PRM$Ratio
)

str(PRM_list)

# Model

model_PRM_ratio <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b_R*Ratio,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_se),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_R ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = PRM_list,
  start = list(M_est = PRM_list$M_obs),
  WAIC = FALSE,
  iter = 20000,
  warmup = 10000,
  control = list(adapt_delta = 0.90))

# Check model

check_energy(model_PRM_ratio@stanfit)
check_treedepth(model_PRM_ratio@stanfit)

divergent <- get_sampler_params(model_PRM_ratio@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_PRM_ratio, digits = 4, prob = 0.95, depth = 2)
plot(precis(model_PRM_ratio))

# Save model

saveRDS(model_PRM_ratio, "Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/PRM/Length_Ratio_Cresp.rds")
