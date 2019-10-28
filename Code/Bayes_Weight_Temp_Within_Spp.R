#### Bayesian Linear Models - Weight and Temp - Within Species ####

library(tidyverse)
library(rethinking)
library(bayesplot)

options(max.print=999999)

myct <- read.csv("Myctophids_M_Temp.csv")
glimpse(myct)

#### Overall Model with Weight and Temp ####

myct_tidy <- filter(myct, Weight_SD == "0")
myct_tidy <- filter(myct, !is.na(mean_M))
myct_tidy$log_Weight <- log(myct_tidy$Weight.x)
glimpse(myct_tidy)

M_T_W_list <- list(
  M_obs = myct_tidy$mean_M,
  M_sd = myct_tidy$sd_M,
  Weight = myct_tidy$log_Weight,
  Temp_obs = myct_tidy$mean_Temp,
  Temp_sd = myct_tidy$sd_Temp,
  Species = myct_tidy$sciname
)

## Convert to z-scores

# Weight

Weight_mean <- mean(M_T_W_list$Weight)
Weight_sd <- sd(M_T_W_list$Weight)

for(i in 1:length(M_T_W_list$Weight)){
  M_T_W_list$Weight_Z[i] <- (M_T_W_list$Weight[i] - Weight_mean) / Weight_sd
}

# Temp_obs

Temp_obs_mean <- mean(M_T_W_list$Temp_obs)
Temp_obs_sd <- sd(M_T_W_list$Temp_obs)

for(i in 1:length(M_T_W_list$Temp_obs)){
  M_T_W_list$Temp_Obs_Z[i] <- (M_T_W_list$Temp_obs[i] - Temp_obs_mean) / Temp_obs_sd
}

# Temp_sd

Temp_sd_mean <- mean(M_T_W_list$Temp_sd)
Temp_sd_sd <- sd(M_T_W_list$Temp_sd)

for(i in 1:length(M_T_W_list$Temp_sd)){
  M_T_W_list$Temp_SD_Z[i] <- abs((M_T_W_list$Temp_sd[i] - Temp_sd_mean) / Temp_sd_sd)
}

# Tidy list

mod_df <- data.frame(
  M_obs = M_T_W_list$M_obs,
  M_sd = M_T_W_list$M_sd,
  Weight = M_T_W_list$Weight_Z,
  Temp_obs = M_T_W_list$Temp_Obs_Z,
  Temp_sd = M_T_W_list$Temp_SD_Z,
  Species = M_T_W_list$Species
)

#### ELN ####

ELN <- filter(mod_df, Species == "Electrona antarctica")
ELN <- as.list(ELN)

## Model

model_ELN <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a +
      b_W*Weight +
      b_T*Temp_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    Temp_obs ~ dnorm(Temp_est, Temp_sd),
    
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
  iter = 3000,
  warmup = 1500)

## Run diagnostics

check_energy(model_ELN@stanfit)
check_treedepth(model_ELN@stanfit)

divergent <- get_sampler_params(model_ELN@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_ELN, digits = 4, prob = 0.95, depth = 2)

## Save stanfit

saveRDS(model_ELN, "Outputs/Within_Species/ELN/M_T_W_model.rds")

## Extract samples

## Plot intervals

post <- extract.samples(model_ELN)
post <- as.data.frame(post)

colnames(post)[39:42] <- c("a", "b_W", "b_T", "sigma")

## Plot pairs

pairs(model_ELN, pars = c("a", "b_W", "b_T", "sigma"))

## Plot intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b_W", "b_T", "sigma"),
               prob = 0.5, prob_outer = 0.95) +
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
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot

#### ELC ####

ELC <- filter(mod_df, Species == "Electrona carlsbergi")
ELC <- as.list(ELC)

## Model

model_ELC <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a +
      b_W*Weight +
      b_T*Temp_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    Temp_obs ~ dnorm(Temp_est, Temp_sd),
    
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
  iter = 6000,
  warmup = 3000,
  control = list(adapt_delta = 0.90))

## Run diagnostics

check_energy(model_ELC@stanfit)
check_treedepth(model_ELC@stanfit)

divergent <- get_sampler_params(model_ELC@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_ELC, digits = 4, prob = 0.95, depth = 2)

## Save stanfit

saveRDS(model_ELC, "Outputs/Within_Species/ELC/M_T_W_model.rds")

## Extract samples

## Plot intervals

post <- extract.samples(model_ELC)
post <- as.data.frame(post)

colnames(post)[35:38] <- c("a", "b_W", "b_T", "sigma")

## Plot pairs

pairs(model_ELC, pars = c("a", "b_W", "b_T", "sigma"))

## Plot intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b_W", "b_T", "sigma"),
               prob = 0.5, prob_outer = 0.95) +
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
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot

#### GYR ####

GYR <- filter(mod_df, Species == "Gymnoscopelus braueri")
GYR <- as.list(GYR)

## Model

model_GYR <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a +
      b_W*Weight +
      b_T*Temp_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    Temp_obs ~ dnorm(Temp_est, Temp_sd),
    
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
  iter = 6000,
  warmup = 3000,
  control = list(adapt_delta = 0.90))

## Run diagnostics

check_energy(model_GYR@stanfit)
check_treedepth(model_GYR@stanfit)

divergent <- get_sampler_params(model_GYR@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_GYR, digits = 4, prob = 0.95, depth = 2)

## Save stanfit

saveRDS(model_GYR, "Outputs/Within_Species/GYR/M_T_W_model.rds")

## Extract samples

## Plot intervals

post <- extract.samples(model_GYR)
post <- as.data.frame(post)

colnames(post)[41:44] <- c("a", "b_W", "b_T", "sigma")

## Plot pairs

pairs(model_GYR, pars = c("a", "b_W", "b_T", "sigma"))

## Plot intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b_W", "b_T", "sigma"),
               prob = 0.5, prob_outer = 0.95) +
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
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot

#### GYN ####

GYN <- filter(mod_df, Species == "Gymnoscopelus nicholsi")
GYN <- as.list(GYN)

## Model

model_GYN <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a +
      b_W*Weight +
      b_T*Temp_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    Temp_obs ~ dnorm(Temp_est, Temp_sd),
    
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
  iter = 6000,
  warmup = 3000,
  control = list(adapt_delta = 0.90))

## Run diagnostics

check_energy(model_GYN@stanfit)
check_treedepth(model_GYN@stanfit)

divergent <- get_sampler_params(model_GYN@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_GYN, digits = 4, prob = 0.95, depth = 2)

## Save stanfit

saveRDS(model_GYN, "Outputs/Within_Species/GYN/M_T_W_model.rds")

## Extract samples

## Plot intervals

post <- extract.samples(model_GYN)
post <- as.data.frame(post)

colnames(post)[25:28] <- c("a", "b_W", "b_T", "sigma")

## Plot pairs

pairs(model_GYN, pars = c("a", "b_W", "b_T", "sigma"))

## Plot intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b_W", "b_T", "sigma"),
               prob = 0.5, prob_outer = 0.95) +
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
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot

#### KRA ####

KRA <- filter(mod_df, Species == "Krefftichthys anderssoni")
KRA <- as.list(KRA)

## Model

model_KRA <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a +
      b_W*Weight +
      b_T*Temp_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    Temp_obs ~ dnorm(Temp_est, Temp_sd),
    
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
  iter = 6000,
  warmup = 3000,
  control = list(adapt_delta = 0.90))

## Run diagnostics

check_energy(model_KRA@stanfit)
check_treedepth(model_KRA@stanfit)

divergent <- get_sampler_params(model_KRA@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_KRA, digits = 4, prob = 0.95, depth = 2)

## Save stanfit

saveRDS(model_KRA, "Outputs/Within_Species/KRA/M_T_W_model.rds")

## Extract samples

## Plot intervals

post <- extract.samples(model_KRA)
post <- as.data.frame(post)

colnames(post)[41:44] <- c("a", "b_W", "b_T", "sigma")

## Plot pairs

pairs(model_KRA, pars = c("a", "b_W", "b_T", "sigma"))

## Plot intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b_W", "b_T", "sigma"),
               prob = 0.5, prob_outer = 0.95) +
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
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot

#### PRM ####

PRM <- filter(mod_df, Species == "Protomyctophum bolini")
PRM <- as.list(PRM)

## Model

model_PRM <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a +
      b_W*Weight +
      b_T*Temp_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    Temp_obs ~ dnorm(Temp_est, Temp_sd),
    
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
  iter = 6000,
  warmup = 3000,
  control = list(adapt_delta = 0.90))

## Run diagnostics

check_energy(model_PRM@stanfit)
check_treedepth(model_PRM@stanfit)

divergent <- get_sampler_params(model_PRM@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_PRM, digits = 4, prob = 0.95, depth = 2)

## Save stanfit

saveRDS(model_PRM, "Outputs/Within_Species/PRM/M_T_W_model.rds")

## Extract samples

## Plot intervals

post <- extract.samples(model_PRM)
post <- as.data.frame(post)

colnames(post)[41:44] <- c("a", "b_W", "b_T", "sigma")

## Plot pairs

pairs(model_PRM, pars = c("a", "b_W", "b_T", "sigma"))

## Plot intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b_W", "b_T", "sigma"),
               prob = 0.5, prob_outer = 0.95) +
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
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot
