#### Bayesian Linear Models - Weight - Within Species ####

# Load required packages

library(tidyverse)
library(rethinking)
library(bayesplot)

options(max.print=999999) # Enables viewing of whole output

# Load data

myct <- read.csv("Myctophids_M_Temp_Bel.csv")
glimpse(myct) # Inspect data

# Tidy data

myct_tidy <- filter(myct, Weight_SD == "0") # Exclude those with uncertain weights
myct_tidy <- filter(myct, !is.na(mean_M)) # Exclude those without an M_oto
myct_tidy$log_Weight <- log(myct_tidy$Weight.x) # Natural log weights
glimpse(myct_tidy) # Inspect data

# Put into data fame

M_W_data <- data.frame(
  M_obs = myct_tidy$mean_M,
  M_sd = myct_tidy$sd_M,
  Weight = myct_tidy$Weight.x,
  Species = myct_tidy$sciname
)
glimpse(M_W_data) # Inspect data

#### ELN - Electrona antarctica ####

# Subset by species

ELN <- filter(M_W_data, Species == "Electrona antarctica")
ELN <- as.list(ELN)

# Convert weight to z-score

ELN_weight_mean <- mean(ELN$Weight) # Get species mean of weight
ELN_weight_sd <- sd(ELN$Weight) # Get species standard deviation of weight
for(i in 1:length(ELN$Weight)){ # Loop to get z-scores
  ELN$Weight_Z[i] <- (ELN$Weight[i] - ELN_weight_mean) / ELN_weight_sd
}

# Build and run model

model_ELN <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b_W*Weight_Z,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    
    # Parameter priors
    a ~ dnorm(0, 1),
    b_W ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = ELN,
  start = list(M_est = ELN$M_obs),
  WAIC = FALSE,
  iter = 3000,
  warmup = 1500,
  control = list(adapt_delta = 0.90))

# Check diagnostics

check_energy(model_ELN@stanfit) # Calculates E-BFMI

check_treedepth(model_ELN@stanfit) # Gives number of iterations outside of maximum treedepth

divergent <- get_sampler_params(model_ELN@stanfit, inc_warmup=FALSE)[[1]][,'divergent__'] # Calculates number of divergent iterations
sum(divergent)

# Precis output 

precis(model_ELN, digits = 4, prob = 0.95, depth = 2) # Mean, standard deviation, upper and lower 95% limits to 4 decimal places for each parameter

# Check with data

plot(ELN$Weight_Z, ELN$M_obs, pch = 16)
abline(0.2148, 0.0011)

# Pairs plot

pairs(model_ELN, pars = c("a", "b_W", "sigma")) # Check for autocorrelation

# Save stanfit

saveRDS(model_ELN, "Outputs/Within_Species/ELN/M_W_model.rds")
model_ELN <- readRDS("Outputs/Within_Species/ELN/M_W_model.rds")

# Extract samples

post <- extract.samples(model_ELN)
post <- as.data.frame(post)

# Name columns to match variables

colnames(post)[20:22] <- c("a", "b_W", "sigma")

# Plot posterior intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b_W", "sigma"),
               prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
  labs(x = "Posterior Predictions", y = "Parameters") +
  # Style adjustments
  theme(panel.background = element_blank(),
        legend.position = "none", 
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 10, face = "italic"),
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 10),
        axis.text.y = element_text(colour = "black", face = "plain", size = 10))

# Plot traceplots

p <- mcmc_trace(post, pars = c("a", "b_W", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
# Style adjustments
p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))


#### ELC - Electrona carlsbergi ####

# Subset by species

ELC <- filter(M_W_data, Species == "Electrona carlsbergi")
ELC <- as.list(ELC)

# Convert weight to z-score

ELC_weight_mean <- mean(ELC$Weight) # Get species mean of weight
ELC_weight_sd <- sd(ELC$Weight) # Get species standard deviation of weight
for(i in 1:length(ELC$Weight)){ # Loop to get z-scores
  ELC$Weight_Z[i] <- (ELC$Weight[i] - ELC_weight_mean) / ELC_weight_sd
}

# Build and run model

model_ELC <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b_W*Weight_Z,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    
    # Parameter priors
    a ~ dnorm(0, 1),
    b_W ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = ELC,
  start = list(M_est = ELC$M_obs),
  WAIC = FALSE,
  iter = 6000,
  warmup = 3000,
  control = list(adapt_delta = 0.90))

# Check diagnostics

check_energy(model_ELC@stanfit) # Calculates E-BFMI

check_treedepth(model_ELC@stanfit) # Gives number of iterations outside of maximum treedepth

divergent <- get_sampler_params(model_ELC@stanfit, inc_warmup=FALSE)[[1]][,'divergent__'] # Calculates number of divergent iterations
sum(divergent)

# Precis output 

precis(model_ELC, digits = 4, prob = 0.95, depth = 2) # Mean, standard deviation, upper and lower 95% limits to 4 decimal places for each parameter

# Check with data

plot(ELC$Weight_Z, ELC$M_obs, pch = 16)
abline(0.1688, -0.0034)

# Pairs plot

pairs(model_ELC, pars = c("a", "b_W", "sigma")) # Check for autocorrelation

# Save stanfit

saveRDS(model_ELC, "Outputs/Within_Species/ELC/M_W_model.rds")
model_ELC <- readRDS("Outputs/Within_Species/ELC/M_W_model.rds")

# Extract samples

post <- extract.samples(model_ELC)
post <- as.data.frame(post)

# Name columns to match variables

colnames(post)[18:20] <- c("a", "b_W", "sigma")

# Plot posterior intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b_W", "sigma"),
               prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
  labs(x = "Posterior Predictions", y = "Parameters") +
  # Style adjustments
  theme(panel.background = element_blank(),
        legend.position = "none", 
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 10, face = "italic"),
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 10),
        axis.text.y = element_text(colour = "black", face = "plain", size = 10))

# Plot traceplots

p <- mcmc_trace(post, pars = c("a", "b_W", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
# Style adjustments
p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))

#### GYR - Gymnoscopelus braueri ####

# Subset by species

GYR <- filter(M_W_data, Species == "Gymnoscopelus braueri")
GYR <- as.list(GYR)

# Convert weight to z-score

GYR_weight_mean <- mean(GYR$Weight) # Get species mean of weight
GYR_weight_sd <- sd(GYR$Weight) # Get species standard deviation of weight
for(i in 1:length(GYR$Weight)){ # Loop to get z-scores
  GYR$Weight_Z[i] <- (GYR$Weight[i] - GYR_weight_mean) / GYR_weight_sd
}

# Build and run model

model_GYR <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b_W*Weight_Z,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    
    # Parameter priors
    a ~ dnorm(0, 1),
    b_W ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = GYR,
  start = list(M_est = GYR$M_obs),
  WAIC = FALSE,
  iter = 6000,
  warmup = 3000,
  control = list(adapt_delta = 0.90))

# Check diagnostics

check_energy(model_GYR@stanfit) # Calculates E-BFMI

check_treedepth(model_GYR@stanfit) # Gives number of iterations outside of maximum treedepth

divergent <- get_sampler_params(model_GYR@stanfit, inc_warmup=FALSE)[[1]][,'divergent__'] # Calculates number of divergent iterations
sum(divergent)

# Precis output 

precis(model_GYR, digits = 4, prob = 0.95, depth = 2) # Mean, standard deviation, upper and lower 95% limits to 4 decimal places for each parameter

# Check with data

plot(GYR$Weight_Z, GYR$M_obs, pch = 16)
abline(0.2072, -0.0046)

# Pairs plot

pairs(model_GYR, pars = c("a", "b_W", "sigma")) # Check for autocorrelation

# Save stanfit

saveRDS(model_GYR, "Outputs/Within_Species/GYR/M_W_model.rds")
model_GYR <- readRDS("Outputs/Within_Species/GYR/M_W_model.rds")

# Extract samples

post <- extract.samples(model_GYR)
post <- as.data.frame(post)

# Name columns to match variables

colnames(post)[21:23] <- c("a", "b_W", "sigma")

# Plot posterior intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b_W", "sigma"),
               prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
  labs(x = "Posterior Predictions", y = "Parameters") +
  # Style adjustments
  theme(panel.background = element_blank(),
        legend.position = "none", 
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 10, face = "italic"),
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 10),
        axis.text.y = element_text(colour = "black", face = "plain", size = 10))

# Plot traceplots

p <- mcmc_trace(post, pars = c("a", "b_W", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
# Style adjustments
p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))

#### GYN - Gymnoscopelus nicholsi ####

# Subset by species

GYN <- filter(M_W_data, Species == "Gymnoscopelus nicholsi")
GYN <- as.list(GYN)

# Convert weight to z-score

GYN_weight_mean <- mean(GYN$Weight) # Get species mean of weight
GYN_weight_sd <- sd(GYN$Weight) # Get species standard deviation of weight
for(i in 1:length(GYN$Weight)){ # Loop to get z-scores
  GYN$Weight_Z[i] <- (GYN$Weight[i] - GYN_weight_mean) / GYN_weight_sd
}

# Build and run model

model_GYN <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b_W*Weight_Z,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    
    # Parameter priors
    a ~ dnorm(0, 1),
    b_W ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = GYN,
  start = list(M_est = GYN$M_obs),
  WAIC = FALSE,
  iter = 6000,
  warmup = 3000,
  control = list(adapt_delta = 0.90))

# Check diagnostics

check_energy(model_GYN@stanfit) # Calculates E-BFMI

check_treedepth(model_GYN@stanfit) # Gives number of iterations outside of maximum treedepth

divergent <- get_sampler_params(model_GYN@stanfit, inc_warmup=FALSE)[[1]][,'divergent__'] # Calculates number of divergent iterations
sum(divergent)

# Precis output 

precis(model_GYN, digits = 4, prob = 0.95, depth = 2) # Mean, standard deviation, upper and lower 95% limits to 4 decimal places for each parameter

# Check with data

plot(GYN$Weight_Z, GYN$M_obs, pch = 16)
abline(0.1430, 0.0047)

# Pairs plot

pairs(model_GYN, pars = c("a", "b_W", "sigma")) # Check for autocorrelation

# Save stanfit

saveRDS(model_GYN, "Outputs/Within_Species/GYN/M_W_model.rds")
model_GYN <- readRDS("Outputs/Within_Species/GYN/M_W_model.rds")

# Extract samples

post <- extract.samples(model_GYN)
post <- as.data.frame(post)

# Name columns to match variables

colnames(post)[13:15] <- c("a", "b_W", "sigma")

# Plot posterior intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b_W", "sigma"),
               prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
  labs(x = "Posterior Predictions", y = "Parameters") +
  # Style adjustments
  theme(panel.background = element_blank(),
        legend.position = "none", 
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 10, face = "italic"),
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 10),
        axis.text.y = element_text(colour = "black", face = "plain", size = 10))

# Plot traceplots

p <- mcmc_trace(post, pars = c("a", "b_W", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
# Style adjustments
p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))

#### KRA - Kreffticthys anderssoni ####

# Subset by species

KRA <- filter(M_W_data, Species == "Krefftichthys anderssoni")
KRA <- as.list(KRA)

# Convert weight to z-score

KRA_weight_mean <- mean(KRA$Weight) # Get species mean of weight
KRA_weight_sd <- sd(KRA$Weight) # Get species standard deviation of weight
for(i in 1:length(KRA$Weight)){ # Loop to get z-scores
  KRA$Weight_Z[i] <- (KRA$Weight[i] - KRA_weight_mean) / KRA_weight_sd
}

# Build and run model

model_KRA <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b_W*Weight_Z,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    
    # Parameter priors
    a ~ dnorm(0, 1),
    b_W ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = KRA,
  start = list(M_est = KRA$M_obs),
  WAIC = FALSE,
  iter = 6000,
  warmup = 3000,
  control = list(adapt_delta = 0.90))

# Check diagnostics

check_energy(model_KRA@stanfit) # Calculates E-BFMI

check_treedepth(model_KRA@stanfit) # Gives number of iterations outside of maximum treedepth

divergent <- get_sampler_params(model_KRA@stanfit, inc_warmup=FALSE)[[1]][,'divergent__'] # Calculates number of divergent iterations
sum(divergent)

# Precis output 

precis(model_KRA, digits = 4, prob = 0.95, depth = 2) # Mean, standard deviation, upper and lower 95% limits to 4 decimal places for each parameter

# Check with data

plot(KRA$Weight_Z, KRA$M_obs, pch = 16)
abline(0.1907, 0.0020)

# Pairs plot

pairs(model_KRA, pars = c("a", "b_W", "sigma")) # Check for autocorrelation

# Save stanfit

saveRDS(model_KRA, "Outputs/Within_Species/KRA/M_W_model.rds")
model_KRA <- readRDS("Outputs/Within_Species/KRA/M_W_model.rds")

# Extract samples

post <- extract.samples(model_KRA)
post <- as.data.frame(post)

# Name columns to match variables

colnames(post)[21:23] <- c("a", "b_W", "sigma")

# Plot posterior intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b_W", "sigma"),
               prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
  labs(x = "Posterior Predictions", y = "Parameters") +
  # Style adjustments
  theme(panel.background = element_blank(),
        legend.position = "none", 
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 10, face = "italic"),
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 10),
        axis.text.y = element_text(colour = "black", face = "plain", size = 10))

# Plot traceplots

p <- mcmc_trace(post, pars = c("a", "b_W", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
# Style adjustments
p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))

#### PRM - Protomyctophum bolini ####

# Subset by species

PRM <- filter(M_W_data, Species == "Protomyctophum bolini")
PRM <- as.list(PRM)

# Convert weight to z-score

PRM_weight_mean <- mean(PRM$Weight) # Get species mean of weight
PRM_weight_sd <- sd(PRM$Weight) # Get species standard deviation of weight
for(i in 1:length(PRM$Weight)){ # Loop to get z-scores
  PRM$Weight_Z[i] <- (PRM$Weight[i] - PRM_weight_mean) / PRM_weight_sd
}

# Build and run model

model_PRM <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b_W*Weight_Z,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    
    # Parameter priors
    a ~ dnorm(0, 1),
    b_W ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = PRM,
  start = list(M_est = PRM$M_obs),
  WAIC = FALSE,
  iter = 3000,
  warmup = 1500,
  control = list(adapt_delta = 0.80))

# Check diagnostics

check_energy(model_PRM@stanfit) # Calculates E-BFMI

check_treedepth(model_PRM@stanfit) # Gives number of iterations outside of maximum treedepth

divergent <- get_sampler_params(model_PRM@stanfit, inc_warmup=FALSE)[[1]][,'divergent__'] # Calculates number of divergent iterations
sum(divergent)

# Precis output 

precis(model_PRM, digits = 4, prob = 0.95, depth = 2) # Mean, standard deviation, upper and lower 95% limits to 4 decimal places for each parameter

# Check with data

plot(PRM$Weight_Z, PRM$M_obs, pch = 16)
abline(0.1677, 0.0125)

# Pairs plot

pairs(model_PRM, pars = c("a", "b_W", "sigma")) # Check for autocorrelation

# Save stanfit

saveRDS(model_PRM, "Outputs/Within_Species/PRM/M_W_model.rds")
model_PRM <- readRDS("Outputs/Within_Species/PRM/M_W_model.rds")

# Extract samples

post <- extract.samples(model_PRM)
post <- as.data.frame(post)

# Name columns to match variables

colnames(post)[21:23] <- c("a", "b_W", "sigma")

# Plot posterior intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b_W", "sigma"),
               prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
  labs(x = "Posterior Predictions", y = "Parameters") +
  # Style adjustments
  theme(panel.background = element_blank(),
        legend.position = "none", 
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 10, face = "italic"),
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 10),
        axis.text.y = element_text(colour = "black", face = "plain", size = 10))

# Plot traceplots

p <- mcmc_trace(post, pars = c("a", "b_W", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
# Style adjustments
p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
