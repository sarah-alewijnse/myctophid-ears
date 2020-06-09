#### Bayesian Linear Models - Belcher RMR Estimates ####

library(tidyverse)
library(rethinking)
library(bayesplot)

# Load data

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")
glimpse(myct) # Inspect data

# Tidy data

myct_tidy <- filter(myct, !is.na(mean_M)) # Exclude those without an M_oto
myct_tidy <- filter(myct_tidy, !is.na(mean_Metabol)) # Exclude those without a metabol estimate

# Put into data fame

M_M_data <- data.frame(
  M_obs = myct_tidy$mean_M,
  M_sd = myct_tidy$sd_M,
  Metabol_obs = myct_tidy$Dir_mean_Metabol,
  Metabol_sd = myct_tidy$sd_Metabol,
  Species = myct_tidy$sciname
)

#### ELN - Electrona antarctica ####

# Subset by species

ELN <- filter(M_M_data, Species == "Electrona antarctica")
ELN <- as.list(ELN)

# Convert temperature to z-score

ELN_Metabol_mean <- mean(ELN$Metabol_obs) # Get species mean of metabolic rate
ELN_Metabol_sd <- sd(ELN$Metabol_obs) # Get species standard deviation of temperature
for(i in 1:length(ELN$Metabol_obs)){ # Loop to get z-scores
  ELN$Metabol_Z[i] <- (ELN$Metabol_obs[i] - ELN_Metabol_mean) / ELN_Metabol_sd
}

glimpse(ELN) # Check data

# Model

model_ELN <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a +
      b_M*Metabol_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    Metabol_Z ~ dnorm(Metabol_est, Metabol_sd),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_M ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = ELN,
  start = list(M_est = ELN$M_obs,
               Metabol_est = ELN$Metabol_obs),
  WAIC = FALSE,
  iter = 6000,
  warmup = 3000,
  control = list(adapt_delta = 0.90))

## Run diagnostics

check_energy(model_ELN@stanfit)
check_treedepth(model_ELN@stanfit)

divergent <- get_sampler_params(model_ELN@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_ELN, digits = 4, prob = 0.95, depth = 2)

# Check with data

plot(ELN$Metabol_Z, ELN$M_obs, pch = 16)
abline(0.2156, -0.0030)

## Save stanfit

saveRDS(model_ELN, "Outputs/Within_Species/ELN/M_T_model.rds")
model_ELN <- readRDS("Outputs/Within_Species/ELN/M_T_model.rds")

## Extract samples

post <- extract.samples(model_ELN)
post <- as.data.frame(post)

# Name columns to match vairables

colnames(post)[37:39] <- c("a", "b_M", "sigma")

## Plot pairs

pairs(model_ELN, pars = c("a", "b_M", "sigma")) # Check for autocorrelation

## Plot intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b_M", "sigma"),
               prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
  labs(x = "Posterior Predictions", y = "Parameters") +
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 10, face = "italic"),
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 10),
        axis.text.y = element_text(colour = "black", face = "plain", size = 10))

## Plot trace

p <- mcmc_trace(post, pars = c("a", "b_M", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))

#### ELC - Electrona carlsbergi ####

# Subset by species

ELC <- filter(M_M_data, Species == "Electrona carlsbergi")
ELC <- as.list(ELC)

# Convert temperature to z-score

ELC_Metabol_mean <- mean(ELC$Metabol_obs) # Get species mean of metabolic rate
ELC_Metabol_sd <- sd(ELC$Metabol_obs) # Get species standard deviation of temperature
for(i in 1:length(ELC$Metabol_obs)){ # Loop to get z-scores
  ELC$Metabol_Z[i] <- (ELC$Metabol_obs[i] - ELC_Metabol_mean) / ELC_Metabol_sd
}

glimpse(ELC) # Check data

# Model

model_ELC <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a +
      b_M*Metabol_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    Metabol_Z ~ dnorm(Metabol_est, Metabol_sd),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_M ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = ELC,
  start = list(M_est = ELC$M_obs,
               Metabol_est = ELC$Metabol_obs),
  WAIC = FALSE,
  iter = 6000,
  warmup = 3000,
  control = list(adapt_delta = 0.90))

## Run diagnostics

check_energy(model_ELC@stanfit)
check_treedepth(model_ELC@stanfit)

divergent <- get_sampler_params(model_ELC@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_ELC, digits = 4, prob = 0.95, depth = 2)

# Check with data

plot(ELC$Metabol_Z, ELC$M_obs, pch = 16)
abline(0.1664, -0.0033)

## Save stanfit

saveRDS(model_ELC, "Outputs/Within_Species/ELC/M_T_model.rds")
model_ELC <- readRDS("Outputs/Within_Species/ELC/M_T_model.rds")

## Extract samples

post <- extract.samples(model_ELC)
post <- as.data.frame(post)

# Name columns to match vairables

colnames(post)[21:23] <- c("a", "b_M", "sigma")

## Plot pairs

pairs(model_ELC, pars = c("a", "b_M", "sigma")) # Check for autocorrelation

## Plot intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b_M", "sigma"),
               prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
  labs(x = "Posterior Predictions", y = "Parameters") +
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 10, face = "italic"),
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 10),
        axis.text.y = element_text(colour = "black", face = "plain", size = 10))

## Plot trace

p <- mcmc_trace(post, pars = c("a", "b_M", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))

#### GYR - Electrona carlsbergi ####

# Subset by species

GYR <- filter(M_M_data, Species == "Gymnoscopelus braueri")
GYR <- as.list(GYR)

# Convert temperature to z-score

GYR_Metabol_mean <- mean(GYR$Metabol_obs) # Get species mean of metabolic rate
GYR_Metabol_sd <- sd(GYR$Metabol_obs) # Get species standard deviation of temperature
for(i in 1:length(GYR$Metabol_obs)){ # Loop to get z-scores
  GYR$Metabol_Z[i] <- (GYR$Metabol_obs[i] - GYR_Metabol_mean) / GYR_Metabol_sd
}

glimpse(GYR) # Check data

# Model

model_GYR <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a +
      b_M*Metabol_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    Metabol_Z ~ dnorm(Metabol_est, Metabol_sd),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_M ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = GYR,
  start = list(M_est = GYR$M_obs,
               Metabol_est = GYR$Metabol_obs),
  WAIC = FALSE,
  iter = 6000,
  warmup = 3000,
  control = list(adapt_delta = 0.90))

## Run diagnostics

check_energy(model_GYR@stanfit)
check_treedepth(model_GYR@stanfit)

divergent <- get_sampler_params(model_GYR@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_GYR, digits = 4, prob = 0.95, depth = 2)

# Check with data

plot(GYR$Metabol_Z, GYR$M_obs, pch = 16)
abline(0.2069, -0.0061)

## Save stanfit

saveRDS(model_GYR, "Outputs/Within_Species/GYR/M_T_model.rds")
model_GYR <- readRDS("Outputs/Within_Species/GYR/M_T_model.rds")

## Extract samples

post <- extract.samples(model_GYR)
post <- as.data.frame(post)

# Name columns to match vairables

colnames(post)[41:43] <- c("a", "b_M", "sigma")

## Plot pairs

pairs(model_GYR, pars = c("a", "b_M", "sigma")) # Check for autocorrelation

## Plot intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b_M", "sigma"),
               prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
  labs(x = "Posterior Predictions", y = "Parameters") +
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 10, face = "italic"),
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 10),
        axis.text.y = element_text(colour = "black", face = "plain", size = 10))

## Plot trace

p <- mcmc_trace(post, pars = c("a", "b_M", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))

#### GYN - Gymnoscopelus nicholsi ####

# Subset by species

GYN <- filter(M_M_data, Species == "Gymnoscopelus nicholsi")
GYN <- as.list(GYN)

# Convert temperature to z-score

GYN_Metabol_mean <- mean(GYN$Metabol_obs) # Get species mean of metabolic rate
GYN_Metabol_sd <- sd(GYN$Metabol_obs) # Get species standard deviation of temperature
for(i in 1:length(GYN$Metabol_obs)){ # Loop to get z-scores
  GYN$Metabol_Z[i] <- (GYN$Metabol_obs[i] - GYN_Metabol_mean) / GYN_Metabol_sd
}

glimpse(GYN) # Check data

# Model

model_GYN <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a +
      b_M*Metabol_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    Metabol_Z ~ dnorm(Metabol_est, Metabol_sd),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_M ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = GYN,
  start = list(M_est = GYN$M_obs,
               Metabol_est = GYN$Metabol_obs),
  WAIC = FALSE,
  iter = 6000,
  warmup = 3000,
  control = list(adapt_delta = 0.90))

## Run diagnostics

check_energy(model_GYN@stanfit)
check_treedepth(model_GYN@stanfit)

divergent <- get_sampler_params(model_GYN@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_GYN, digits = 4, prob = 0.95, depth = 2)

# Check with data

plot(GYN$Metabol_Z, GYN$M_obs, pch = 16)
abline(0.1427, -0.0016)

## Save stanfit

saveRDS(model_GYN, "Outputs/Within_Species/GYN/M_T_model.rds")
model_GYN <- readRDS("Outputs/Within_Species/GYN/M_T_model.rds")

## Extract samples

post <- extract.samples(model_GYN)
post <- as.data.frame(post)

# Name columns to match vairables

colnames(post)[25:27] <- c("a", "b_M", "sigma")

## Plot pairs

pairs(model_GYN, pars = c("a", "b_M", "sigma")) # Check for autocorrelation

## Plot intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b_M", "sigma"),
               prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
  labs(x = "Posterior Predictions", y = "Parameters") +
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 10, face = "italic"),
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 10),
        axis.text.y = element_text(colour = "black", face = "plain", size = 10))

## Plot trace

p <- mcmc_trace(post, pars = c("a", "b_M", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))

#### KRA - Krefftichthys anderssoni ####

# Subset by species

KRA <- filter(M_M_data, Species == "Krefftichthys anderssoni")
KRA <- as.list(KRA)

# Convert temperature to z-score

KRA_Metabol_mean <- mean(KRA$Metabol_obs) # Get species mean of metabolic rate
KRA_Metabol_sd <- sd(KRA$Metabol_obs) # Get species standard deviation of temperature
for(i in 1:length(KRA$Metabol_obs)){ # Loop to get z-scores
  KRA$Metabol_Z[i] <- (KRA$Metabol_obs[i] - KRA_Metabol_mean) / KRA_Metabol_sd
}

glimpse(KRA) # Check data

# Model

model_KRA <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a +
      b_M*Metabol_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    Metabol_Z ~ dnorm(Metabol_est, Metabol_sd),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_M ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = KRA,
  start = list(M_est = KRA$M_obs,
               Metabol_est = KRA$Metabol_obs),
  WAIC = FALSE,
  iter = 6000,
  warmup = 3000,
  control = list(adapt_delta = 0.90))

## Run diagnostics

check_energy(model_KRA@stanfit)
check_treedepth(model_KRA@stanfit)

divergent <- get_sampler_params(model_KRA@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_KRA, digits = 4, prob = 0.95, depth = 2)

# Check with data

plot(KRA$Metabol_Z, KRA$M_obs, pch = 16)
abline(0.1906, -0.0028)

## Save stanfit

saveRDS(model_KRA, "Outputs/Within_Species/KRA/M_T_model.rds")
model_KRA <- readRDS("Outputs/Within_Species/KRA/M_T_model.rds")

## Extract samples

post <- extract.samples(model_KRA)
post <- as.data.frame(post)

# Name columns to match vairables

colnames(post)[41:43] <- c("a", "b_M", "sigma")

## Plot pairs

pairs(model_KRA, pars = c("a", "b_M", "sigma")) # Check for autocorrelation

## Plot intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b_M", "sigma"),
               prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
  labs(x = "Posterior Predictions", y = "Parameters") +
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 10, face = "italic"),
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 10),
        axis.text.y = element_text(colour = "black", face = "plain", size = 10))

## Plot trace

p <- mcmc_trace(post, pars = c("a", "b_M", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))

#### PRM - Protomyctophum bolini ####

# Subset by species

PRM <- filter(M_M_data, Species == "Protomyctophum bolini")
PRM <- as.list(PRM)

# Convert temperature to z-score

PRM_Metabol_mean <- mean(PRM$Metabol_obs) # Get species mean of metabolic rate
PRM_Metabol_sd <- sd(PRM$Metabol_obs) # Get species standard deviation of temperature
for(i in 1:length(PRM$Metabol_obs)){ # Loop to get z-scores
  PRM$Metabol_Z[i] <- (PRM$Metabol_obs[i] - PRM_Metabol_mean) / PRM_Metabol_sd
}

glimpse(PRM) # Check data

# Model

model_PRM <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a +
      b_M*Metabol_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    Metabol_Z ~ dnorm(Metabol_est, Metabol_sd),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_M ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = PRM,
  start = list(M_est = PRM$M_obs,
               Metabol_est = PRM$Metabol_obs),
  WAIC = FALSE,
  iter = 6000,
  warmup = 3000,
  control = list(adapt_delta = 0.90))

## Run diagnostics

check_energy(model_PRM@stanfit)
check_treedepth(model_PRM@stanfit)

divergent <- get_sampler_params(model_PRM@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_PRM, digits = 4, prob = 0.95, depth = 2)

# Check with data

plot(PRM$Metabol_Z, PRM$M_obs, pch = 16)
abline(0.1678, -0.0145)

## Save stanfit

saveRDS(model_PRM, "Outputs/Within_Species/PRM/M_T_model.rds")
model_PRM <- readRDS("Outputs/Within_Species/PRM/M_T_model.rds")

## Extract samples

post <- extract.samples(model_PRM)
post <- as.data.frame(post)

# Name columns to match vairables

colnames(post)[41:43] <- c("a", "b_M", "sigma")

## Plot pairs

pairs(model_PRM, pars = c("a", "b_M", "sigma")) # Check for autocorrelation

## Plot intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b_M", "sigma"),
               prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
  labs(x = "Posterior Predictions", y = "Parameters") +
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 10, face = "italic"),
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 10),
        axis.text.y = element_text(colour = "black", face = "plain", size = 10))

## Plot trace

p <- mcmc_trace(post, pars = c("a", "b_M", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
