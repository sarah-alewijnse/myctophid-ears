#### Difference in PRM d13C (diet) with Station ####

# Load required packages

library(tidyverse)
library(rethinking) # Used to interface with rstan
library(bayesplot) # Gives nice plots

# Print out all results

options(max.print=999999)

# Load and check data

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")
glimpse(myct)

#### Within PRM ####

PRM_tidy <- filter(myct, Label == "PRM")
glimpse(PRM_tidy)

# Z-score latitude

Lat_mean <- mean(PRM_tidy$Lat_dec.x.x)
Lat_sd <- sd(PRM_tidy$Lat_dec.x.x)

for(i in 1:length(PRM_tidy$Lat_dec.x.x)){
  PRM_tidy$Lat_Z[i] <- (PRM_tidy$Lat_dec.x.x[i] - Lat_mean) / Lat_sd
}

mod_list <- list(
  d13C_musc = PRM_tidy$d13C_musc,
  Lat = PRM_tidy$Lat_Z
)
str(mod_list)

# Model

model_PRM_lat <- map2stan(
  alist(
    d13C_musc ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b_L*Lat,
    
    # Parameters
    a ~ dnorm(0, 1),
    b_L ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = mod_list,
  WAIC = FALSE,
  iter = 10000,
  warmup = 5000,
  control = list(adapt_delta = 0.90))

# Check model

check_energy(model_PRM_lat@stanfit)
check_treedepth(model_PRM_lat@stanfit)

divergent <- get_sampler_params(model_PRM_lat@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_PRM_lat, digits = 4, prob = 0.95, depth = 2)

# Save model

saveRDS(model_PRM_lat, "Outputs/04_Misc/03_Within_Spp/02_PRM_Lat/PRM_Lat_musc_model.rds")
