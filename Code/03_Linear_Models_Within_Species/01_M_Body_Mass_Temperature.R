#### Bayesian Linear Models - Weight & Temperature - Within Species ####

# Load required packages

library(tidyverse)
library(rethinking)
library(bayesplot)

options(max.print=999999) # Enables viewing of whole output

# Load data

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")
glimpse(myct) # Inspect data

# Tidy data

myct_tidy <- filter(myct, Weight_SD == "0") # Exclude those with uncertain weights
myct_tidy <- filter(myct, !is.na(mean_M)) # Exclude those without an M_oto
myct_tidy$log_Weight <- log(myct_tidy$Weight.x) # Natural log weights
glimpse(myct_tidy) # Inspect data

# Put into data fame

M_T_W_data <- data.frame(
  M_obs = myct_tidy$mean_M,
  M_sd = myct_tidy$sd_M,
  Temp_obs = myct_tidy$mean_Temp,
  Temp_sd = myct_tidy$sd_Temp,
  Weight = myct_tidy$log_Weight,
  Species = myct_tidy$sciname
)
glimpse(M_T_W_data) # Inspect data

#### ELN - Electrona antarctica ####

# Subset by species

ELN <- filter(M_T_W_data, Species == "Electrona antarctica")
ELN <- as.list(ELN)

# Convert weight to z-score

ELN_weight_mean <- mean(ELN$Weight) # Get species mean of weight
ELN_weight_sd <- sd(ELN$Weight) # Get species standard deviation of weight
for(i in 1:length(ELN$Weight)){ # Loop to get z-scores
  ELN$Weight_Z[i] <- (ELN$Weight[i] - ELN_weight_mean) / ELN_weight_sd
}

# Convert temperature to z-score

ELN_temp_mean <- mean(ELN$Temp_obs) # Get species mean of temperature
ELN_temp_sd <- sd(ELN$Temp_obs) # Get species standard deviation of temperature
for(i in 1:length(ELN$Temp_obs)){ # Loop to get z-scores
  ELN$Temp_Z[i] <- (ELN$Temp_obs[i] - ELN_temp_mean) / ELN_temp_sd
}

glimpse(ELN) # Check data

## Model

model_ELN <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a +
      b_W*Weight_Z +
      b_T*Temp_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    Temp_Z ~ dnorm(Temp_est, Temp_sd),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_W ~ dnorm(0, 1),
    b_T ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = ELN,
  start = list(M_est = ELN$M_obs,
               Temp_est = ELN$Temp_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 5000,
  control = list(adapt_delta = 0.90))

## Run diagnostics

check_energy(model_ELN@stanfit)
check_treedepth(model_ELN@stanfit)

divergent <- get_sampler_params(model_ELN@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_ELN, digits = 4, prob = 0.95, depth = 2)

# Check with data

plot(ELN$Weight_Z, ELN$M_obs, pch = 16)
abline(0.2146, 0.0015)

plot(ELN$Temp_Z, ELN$M_obs, pch = 16)
abline(0.2147, 0.0016)

## Save stanfit

saveRDS(model_ELN, "Outputs/03_Linear_Models_Within_Species/ELN/M_T_W_model.rds")
model_ELN <- readRDS("Outputs/03_Linear_Models_Within_Species/ELN/M_T_W_model.rds")

## Extract samples

## Plot intervals

post <- extract.samples(model_ELN)
post <- as.data.frame(post)

# Name columns to match vairables

colnames(post)[39:42] <- c("a", "b_W", "b_T", "sigma")

## Plot pairs

pairs(model_ELN, pars = c("a", "b_W", "b_T", "sigma")) # Check for autocorrelation

## Plot intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b_W", "b_T", "sigma"),
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

p <- mcmc_trace(post, pars = c("a", "b_W", "b_T", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))

#### ELC - Electrona carlsbergi ####

# Subset by species

ELC <- filter(M_T_W_data, Species == "Electrona carlsbergi")
ELC <- as.list(ELC)

# Convert weight to z-score

ELC_weight_mean <- mean(ELC$Weight) # Get species mean of weight
ELC_weight_sd <- sd(ELC$Weight) # Get species standard deviation of weight
for(i in 1:length(ELC$Weight)){ # Loop to get z-scores
  ELC$Weight_Z[i] <- (ELC$Weight[i] - ELC_weight_mean) / ELC_weight_sd
}

# Convert temperature to z-score

ELC_temp_mean <- mean(ELC$Temp_obs) # Get species mean of temperature
ELC_temp_sd <- sd(ELC$Temp_obs) # Get species standard deviation of temperature
for(i in 1:length(ELC$Temp_obs)){ # Loop to get z-scores
  ELC$Temp_Z[i] <- (ELC$Temp_obs[i] - ELC_temp_mean) / ELC_temp_sd
}

glimpse(ELC) # Check data

## Model

model_ELC <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a +
      b_W*Weight_Z +
      b_T*Temp_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    Temp_Z ~ dnorm(Temp_est, Temp_sd),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_W ~ dnorm(0, 1),
    b_T ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = ELC,
  start = list(M_est = ELC$M_obs,
               Temp_est = ELC$Temp_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 5000,
  control = list(adapt_delta = 0.90))

## Run diagnostics

check_energy(model_ELC@stanfit)
check_treedepth(model_ELC@stanfit)

divergent <- get_sampler_params(model_ELC@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_ELC, digits = 4, prob = 0.95, depth = 2)

# Check with data

plot(ELC$Weight_Z, ELC$M_obs, pch = 16)
abline(0.1687, -0.0034)

plot(ELC$Temp_Z, ELC$M_obs, pch = 16)
abline(0.1687, 0.0001)

## Save stanfit

saveRDS(model_ELC, "Outputs/03_Linear_Models_Within_Species/ELC/M_T_W_model.rds")
model_ELC <- readRDS("Outputs/03_Linear_Models_Within_Species/ELC/M_T_W_model.rds")

## Extract samples

## Plot intervals

post <- extract.samples(model_ELC)
post <- as.data.frame(post)

# Name columns to match vairables

colnames(post)[35:38] <- c("a", "b_W", "b_T", "sigma")

## Plot pairs

pairs(model_ELC, pars = c("a", "b_W", "b_T", "sigma")) # Check for autocorrelation

## Plot intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b_W", "b_T", "sigma"),
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

p <- mcmc_trace(post, pars = c("a", "b_W", "b_T", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))

#### GYR - Gymnoscopelus braueri ####

# Subset by species

GYR <- filter(M_T_W_data, Species == "Gymnoscopelus braueri")
GYR <- as.list(GYR)

# Convert weight to z-score

GYR_weight_mean <- mean(GYR$Weight) # Get species mean of weight
GYR_weight_sd <- sd(GYR$Weight) # Get species standard deviation of weight
for(i in 1:length(GYR$Weight)){ # Loop to get z-scores
  GYR$Weight_Z[i] <- (GYR$Weight[i] - GYR_weight_mean) / GYR_weight_sd
}

# Convert temperature to z-score

GYR_temp_mean <- mean(GYR$Temp_obs) # Get species mean of temperature
GYR_temp_sd <- sd(GYR$Temp_obs) # Get species standard deviation of temperature
for(i in 1:length(GYR$Temp_obs)){ # Loop to get z-scores
  GYR$Temp_Z[i] <- (GYR$Temp_obs[i] - GYR_temp_mean) / GYR_temp_sd
}

glimpse(GYR) # Check data

## Model

model_GYR <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a +
      b_W*Weight_Z +
      b_T*Temp_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    Temp_Z ~ dnorm(Temp_est, Temp_sd),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_W ~ dnorm(0, 1),
    b_T ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = GYR,
  start = list(M_est = GYR$M_obs,
               Temp_est = GYR$Temp_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 5000,
  control = list(adapt_delta = 0.90))

## Run diagnostics

check_energy(model_GYR@stanfit)
check_treedepth(model_GYR@stanfit)

divergent <- get_sampler_params(model_GYR@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_GYR, digits = 4, prob = 0.95, depth = 2)

# Check with data

plot(GYR$Weight_Z, GYR$M_obs, pch = 16)
abline(0.2073, -0.0051)

plot(GYR$Temp_Z, GYR$M_obs, pch = 16)
abline(0.2073, -0.0007)

## Save stanfit

saveRDS(model_GYR, "Outputs/03_Linear_Models_Within_Species/GYR/M_T_W_model.rds")
model_GYR <- readRDS("Outputs/03_Linear_Models_Within_Species/GYR/M_T_W_model.rds")

## Extract samples

post <- extract.samples(model_GYR)
post <- as.data.frame(post)

# Name columns to match vairables

colnames(post)[41:44] <- c("a", "b_W", "b_T", "sigma")

## Plot pairs

pairs(model_GYR, pars = c("a", "b_W", "b_T", "sigma")) # Check for autocorrelation

## Plot intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b_W", "b_T", "sigma"),
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

p <- mcmc_trace(post, pars = c("a", "b_W", "b_T", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))


#### GYN - Gymnoscopelus nicholsi ####

# Subset by species

GYN <- filter(M_T_W_data, Species == "Gymnoscopelus nicholsi")
GYN <- as.list(GYN)

# Convert weight to z-score

GYN_weight_mean <- mean(GYN$Weight) # Get species mean of weight
GYN_weight_sd <- sd(GYN$Weight) # Get species standard deviation of weight
for(i in 1:length(GYN$Weight)){ # Loop to get z-scores
  GYN$Weight_Z[i] <- (GYN$Weight[i] - GYN_weight_mean) / GYN_weight_sd
}

# Convert temperature to z-score

GYN_temp_mean <- mean(GYN$Temp_obs) # Get species mean of temperature
GYN_temp_sd <- sd(GYN$Temp_obs) # Get species standard deviation of temperature
for(i in 1:length(GYN$Temp_obs)){ # Loop to get z-scores
  GYN$Temp_Z[i] <- (GYN$Temp_obs[i] - GYN_temp_mean) / GYN_temp_sd
}

glimpse(GYN) # Check data

## Model

model_GYN <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a +
      b_W*Weight_Z +
      b_T*Temp_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    Temp_Z ~ dnorm(Temp_est, Temp_sd),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_W ~ dnorm(0, 1),
    b_T ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = GYN,
  start = list(M_est = GYN$M_obs,
               Temp_est = GYN$Temp_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 5000,
  control = list(adapt_delta = 0.90))

## Run diagnostics

check_energy(model_GYN@stanfit)
check_treedepth(model_GYN@stanfit)

divergent <- get_sampler_params(model_GYN@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_GYN, digits = 4, prob = 0.95, depth = 2)

# Check with data

plot(GYN$Weight_Z, GYN$M_obs, pch = 16)
abline(0.1429, -0.0029)

plot(GYN$Temp_Z, GYN$M_obs, pch = 16)
abline(0.1429, -0.0005)

## Save stanfit

saveRDS(model_GYN, "Outputs/03_Linear_Models_Within_Species/GYN/M_T_W_model.rds")
model_GYN <- readRDS("Outputs/03_Linear_Models_Within_Species/GYN/M_T_W_model.rds")

## Extract samples

post <- extract.samples(model_GYN)
post <- as.data.frame(post)

# Name columns to match vairables

colnames(post)[25:28] <- c("a", "b_W", "b_T", "sigma")

## Plot pairs

pairs(model_GYN, pars = c("a", "b_W", "b_T", "sigma")) # Check for autocorrelation

## Plot intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b_W", "b_T", "sigma"),
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

p <- mcmc_trace(post, pars = c("a", "b_W", "b_T", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))

#### KRA - Krefftichthys anderssoni ####

# Subset by species

KRA <- filter(M_T_W_data, Species == "Krefftichthys anderssoni")
KRA <- as.list(KRA)

# Convert weight to z-score

KRA_weight_mean <- mean(KRA$Weight) # Get species mean of weight
KRA_weight_sd <- sd(KRA$Weight) # Get species standard deviation of weight
for(i in 1:length(KRA$Weight)){ # Loop to get z-scores
  KRA$Weight_Z[i] <- (KRA$Weight[i] - KRA_weight_mean) / KRA_weight_sd
}

# Convert temperature to z-score

KRA_temp_mean <- mean(KRA$Temp_obs) # Get species mean of temperature
KRA_temp_sd <- sd(KRA$Temp_obs) # Get species standard deviation of temperature
for(i in 1:length(KRA$Temp_obs)){ # Loop to get z-scores
  KRA$Temp_Z[i] <- (KRA$Temp_obs[i] - KRA_temp_mean) / KRA_temp_sd
}

glimpse(KRA) # Check data

## Model

model_KRA <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a +
      b_W*Weight_Z +
      b_T*Temp_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    Temp_Z ~ dnorm(Temp_est, Temp_sd),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_W ~ dnorm(0, 1),
    b_T ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = KRA,
  start = list(M_est = KRA$M_obs,
               Temp_est = KRA$Temp_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 5000,
  control = list(adapt_delta = 0.90))

## Run diagnostics

check_energy(model_KRA@stanfit)
check_treedepth(model_KRA@stanfit)

divergent <- get_sampler_params(model_KRA@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_KRA, digits = 4, prob = 0.95, depth = 2)

# Check with data

plot(KRA$Weight_Z, KRA$M_obs, pch = 16)
abline(0.1908, 0.0015)

plot(KRA$Temp_Z, KRA$M_obs, pch = 16)
abline(0.1908, -0.0032)

## Save stanfit

saveRDS(model_KRA, "Outputs/03_Linear_Models_Within_Species/KRA/M_T_W_model.rds")
model_KRA <- readRDS("Outputs/03_Linear_Models_Within_Species/KRA/M_T_W_model.rds")

## Extract samples

post <- extract.samples(model_KRA)
post <- as.data.frame(post)

# Name columns to match vairables

colnames(post)[41:44] <- c("a", "b_W", "b_T", "sigma")

## Plot pairs

pairs(model_KRA, pars = c("a", "b_W", "b_T", "sigma")) # Check for autocorrelation

## Plot intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b_W", "b_T", "sigma"),
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

p <- mcmc_trace(post, pars = c("a", "b_W", "b_T", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))

#### PRM - Protomyctophum bolini ####

# Subset by species

PRM <- filter(M_T_W_data, Species == "Protomyctophum bolini")
PRM <- as.list(PRM)

# Convert weight to z-score

PRM_weight_mean <- mean(PRM$Weight) # Get species mean of weight
PRM_weight_sd <- sd(PRM$Weight) # Get species standard deviation of weight
for(i in 1:length(PRM$Weight)){ # Loop to get z-scores
  PRM$Weight_Z[i] <- (PRM$Weight[i] - PRM_weight_mean) / PRM_weight_sd
}

# Convert temperature to z-score

PRM_temp_mean <- mean(PRM$Temp_obs) # Get species mean of temperature
PRM_temp_sd <- sd(PRM$Temp_obs) # Get species standard deviation of temperature
for(i in 1:length(PRM$Temp_obs)){ # Loop to get z-scores
  PRM$Temp_Z[i] <- (PRM$Temp_obs[i] - PRM_temp_mean) / PRM_temp_sd
}

glimpse(PRM) # Check data

## Model

model_PRM <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a +
      b_W*Weight_Z +
      b_T*Temp_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    Temp_Z ~ dnorm(Temp_est, Temp_sd),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_W ~ dnorm(0, 1),
    b_T ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = PRM,
  start = list(M_est = PRM$M_obs,
               Temp_est = PRM$Temp_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 5000,
  control = list(adapt_delta = 0.90))

## Run diagnostics

check_energy(model_PRM@stanfit)
check_treedepth(model_PRM@stanfit)

divergent <- get_sampler_params(model_PRM@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_PRM, digits = 4, prob = 0.95, depth = 2)

# Check with data

plot(PRM$Weight_Z, PRM$M_obs, pch = 16)
abline(0.1678, 0.0115)

plot(PRM$Temp_Z, PRM$M_obs, pch = 16)
abline(0.1678, -0.0064)

## Save stanfit

saveRDS(model_PRM, "Outputs/03_Linear_Models_Within_Species/PRM/M_T_W_model.rds")
model_PRM <- readRDS("Outputs/03_Linear_Models_Within_Species/PRM/M_T_W_model.rds")

## Extract samples

post <- extract.samples(model_PRM)
post <- as.data.frame(post)

# Name columns to match vairables

colnames(post)[41:44] <- c("a", "b_W", "b_T", "sigma")

## Plot pairs

pairs(model_PRM, pars = c("a", "b_W", "b_T", "sigma")) # Check for autocorrelation

## Plot intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b_W", "b_T", "sigma"),
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

p <- mcmc_trace(post, pars = c("a", "b_W", "b_T", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
