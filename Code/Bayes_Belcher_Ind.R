#### Bayesian Linear Models - Belcher RMR Estimates ####

library(tidyverse)
library(rethinking)
library(bayesplot)

options(max.print=999999)

myct <- read.csv("Myctophids_M_Temp_Bel.csv")
glimpse(myct)
myct <- filter(myct, !is.na(mean_M))
myct <- filter(myct, !is.na(mean_Metabol))

## Convert to z-scores

# mean_Metabol

mean_Metabol_mean <- mean(myct$mean_Metabol)
mean_Metabol_sd <- sd(myct$mean_Metabol)

for(i in 1:length(myct$mean_Metabol)){
  myct$mean_Metabol_Z[i] <- (myct$mean_Metabol[i] - mean_Metabol_mean) / mean_Metabol_sd
}

# sd_Metabol

sd_Metabol_mean <- mean(myct$sd_Metabol)
sd_Metabol_sd <- sd(myct$sd_Metabol)

for(i in 1:length(myct$sd_Metabol)){
  myct$sd_Metabol_Z[i] <- (myct$sd_Metabol[i] - sd_Metabol_mean) / sd_Metabol_sd
}


# Tidy list

mod_list <- data.frame(
  Species = myct$Label,
  M_obs = myct$mean_M,
  M_sd = myct$sd_M,
  Metabol_obs = myct$mean_Metabol_Z,
  Metabol_sd = abs(myct$sd_Metabol_Z)
)

## ELN


ELN <- filter(mod_list, Species == "ELN")
ELN <- as.list(ELN)

mod_ELN <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b * Metabol_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    Metabol_obs ~ dnorm(Metabol_est, Metabol_sd),
    
    # Parameters
    a ~ dnorm(0, 1),
    b ~ dnorm(0, 1),
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

check_energy(mod_ELN@stanfit)
check_treedepth(mod_ELN@stanfit)

divergent <- get_sampler_params(mod_ELN@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

pairs(mod_ELN, pars = c("a", "b", "sigma"))

precis(mod_ELN, digits = 4, prob = 0.95, depth = 2)

## Save stanfit

saveRDS(mod_ELN, "Outputs/M_Belcher/ELN_Belcher_model.rds")

## Extract samples

## Plot intervals

post <- extract.samples(mod_ELN)
post <- as.data.frame(post)

## Plot intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b", "sigma"),
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

## Traceplot

p <- mcmc_trace(post, pars = c("a", "b", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot 

## ELC


ELC <- filter(mod_list, Species == "ELC")
ELC <- as.list(ELC)

mod_ELC <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b * Metabol_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    Metabol_obs ~ dnorm(Metabol_est, Metabol_sd),
    
    # Parameters
    a ~ dnorm(0, 1),
    b ~ dnorm(0, 1),
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

check_energy(mod_ELC@stanfit)
check_treedepth(mod_ELC@stanfit)

divergent <- get_sampler_params(mod_ELC@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

pairs(mod_ELC, pars = c("a", "b", "sigma"))

precis(mod_ELC, digits = 4, prob = 0.95, depth = 2)

## Save stanfit

saveRDS(mod_ELC, "Outputs/M_Belcher/ELC_Belcher_model.rds")

## Extract samples

## Plot intervals

post <- extract.samples(mod_ELC)
post <- as.data.frame(post)

## Plot intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b", "sigma"),
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

## Traceplot

p <- mcmc_trace(post, pars = c("a", "b", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot 

## GYR


GYR <- filter(mod_list, Species == "GYR")
GYR <- as.list(GYR)

mod_GYR <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b * Metabol_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    Metabol_obs ~ dnorm(Metabol_est, Metabol_sd),
    
    # Parameters
    a ~ dnorm(0, 1),
    b ~ dnorm(0, 1),
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

check_energy(mod_GYR@stanfit)
check_treedepth(mod_GYR@stanfit)

divergent <- get_sampler_params(mod_GYR@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

pairs(mod_GYR, pars = c("a", "b", "sigma"))

precis(mod_GYR, digits = 4, prob = 0.95, depth = 2)

## Save stanfit

saveRDS(mod_GYR, "Outputs/M_Belcher/GYR_Belcher_model.rds")

## Extract samples

## Plot intervals

post <- extract.samples(mod_GYR)
post <- as.data.frame(post)

## Plot intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b", "sigma"),
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

## Traceplot

p <- mcmc_trace(post, pars = c("a", "b", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot 

## GYN


GYN <- filter(mod_list, Species == "GYN")
GYN <- as.list(GYN)

mod_GYN <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b * Metabol_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    Metabol_obs ~ dnorm(Metabol_est, Metabol_sd),
    
    # Parameters
    a ~ dnorm(0, 1),
    b ~ dnorm(0, 1),
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

check_energy(mod_GYN@stanfit)
check_treedepth(mod_GYN@stanfit)

divergent <- get_sampler_params(mod_GYN@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

pairs(mod_GYN, pars = c("a", "b", "sigma"))

precis(mod_GYN, digits = 4, prob = 0.95, depth = 2)

## Save stanfit

saveRDS(mod_GYN, "Outputs/M_Belcher/GYN_Belcher_model.rds")

## Extract samples

## Plot intervals

post <- extract.samples(mod_GYN)
post <- as.data.frame(post)

## Plot intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b", "sigma"),
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

## Traceplot

p <- mcmc_trace(post, pars = c("a", "b", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot 



## PRM


PRM <- filter(mod_list, Species == "PRM")
PRM <- as.list(PRM)

mod_PRM <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b * Metabol_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    Metabol_obs ~ dnorm(Metabol_est, Metabol_sd),
    
    # Parameters
    a ~ dnorm(0, 1),
    b ~ dnorm(0, 1),
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

check_energy(mod_PRM@stanfit)
check_treedepth(mod_PRM@stanfit)

divergent <- get_sampler_params(mod_PRM@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

pairs(mod_PRM, pars = c("a", "b", "sigma"))

precis(mod_PRM, digits = 4, prob = 0.95, depth = 2)

## Save stanfit

saveRDS(mod_PRM, "Outputs/M_Belcher/PRM_Belcher_model.rds")

## Extract samples

## Plot intervals

post <- extract.samples(mod_PRM)
post <- as.data.frame(post)

## Plot intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b", "sigma"),
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

## Traceplot

p <- mcmc_trace(post, pars = c("a", "b", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot 

## KRA


KRA <- filter(mod_list, Species == "KRA")
KRA <- as.list(KRA)

mod_KRA <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b * Metabol_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    Metabol_obs ~ dnorm(Metabol_est, Metabol_sd),
    
    # Parameters
    a ~ dnorm(0, 1),
    b ~ dnorm(0, 1),
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

check_energy(mod_KRA@stanfit)
check_treedepth(mod_KRA@stanfit)

divergent <- get_sampler_params(mod_KRA@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

pairs(mod_KRA, pars = c("a", "b", "sigma"))

precis(mod_KRA, digits = 4, prob = 0.95, depth = 2)

## Save stanfit

saveRDS(mod_KRA, "Outputs/M_Belcher/KRA_Belcher_model.rds")

## Extract samples

## Plot intervals

post <- extract.samples(mod_KRA)
post <- as.data.frame(post)

## Plot intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b", "sigma"),
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

## Traceplot

p <- mcmc_trace(post, pars = c("a", "b", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot 
