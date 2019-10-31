#### Bayesian Linear Models - Belcher RMR Estimates ####

library(tidyverse)
library(rethinking)
library(bayesplot)

options(max.print=999999)

myct <- read.csv("Myctophids_M_Temp_Bel.csv")
glimpse(myct)

#### Overall Model with Weight and Temp ####

myct_tidy <- filter(myct, !is.na(mean_Metabol))
myct_tidy <- filter(myct_tidy, !is.na(mean_M))
glimpse(myct_tidy)

M_Metabol_list <- list(
  M_obs = myct_tidy$mean_M,
  M_sd = myct_tidy$sd_M,
  Metabol_obs = myct_tidy$mean_Metabol,
  Metabol_sd = myct_tidy$sd_Metabol
)

## Convert to z-scores

# Temp_obs

Temp_obs_mean <- mean(M_Metabol_list$Temp_obs)
Temp_obs_sd <- sd(M_Metabol_list$Temp_obs)

for(i in 1:length(M_Metabol_list$Temp_obs)){
  M_Metabol_list$Temp_Obs_Z[i] <- (M_Metabol_list$Temp_obs[i] - Temp_obs_mean) / Temp_obs_sd
}

# Temp_sd

Temp_sd_mean <- mean(M_Metabol_list$Temp_sd)
Temp_sd_sd <- sd(M_Metabol_list$Temp_sd)

for(i in 1:length(M_Metabol_list$Temp_sd)){
  M_Metabol_list$Temp_SD_Z[i] <- abs((M_Metabol_list$Temp_sd[i] - Temp_sd_mean) / Temp_sd_sd)
}

# Metabol_obs

Metabol_obs_mean <- mean(M_Metabol_list$Metabol_obs)
Metabol_obs_sd <- sd(M_Metabol_list$Metabol_obs)

for(i in 1:length(M_Metabol_list$Metabol_obs)){
  M_Metabol_list$Metabol_Obs_Z[i] <- (M_Metabol_list$Metabol_obs[i] - Metabol_obs_mean) / Metabol_obs_sd
}

# Metabol_sd

Metabol_sd_mean <- mean(M_Metabol_list$Metabol_sd)
Metabol_sd_sd <- sd(M_Metabol_list$Metabol_sd)

for(i in 1:length(M_Metabol_list$Metabol_sd)){
  M_Metabol_list$Metabol_SD_Z[i] <- (M_Metabol_list$Metabol_sd[i] - Metabol_sd_mean) / Metabol_sd_sd
}

# Tidy list

mod_list <- list(
  M_obs = M_Metabol_list$M_obs,
  M_sd = M_Metabol_list$M_sd,
  Metabol_obs = M_Metabol_list$Metabol_Obs_Z,
  Metabol_sd = abs(M_Metabol_list$Metabol_SD_Z)
)

## Model

  model_M_Metabol <- map2stan(
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
  data = mod_list,
  start = list(M_est = mod_list$M_obs,
               Metabol_est = mod_list$Metabol_obs),
  WAIC = FALSE,
  iter = 3000,
  warmup = 1500)

  ## Run diagnostics
  
  check_energy(model_M_Metabol@stanfit)
  check_treedepth(model_M_Metabol@stanfit)
  
  divergent <- get_sampler_params(model_M_Metabol@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
  sum(divergent)
  
  pairs(model_M_Metabol, pars = c("a", "b", "sigma"))
  
  precis(model_M_Metabol, digits = 4, prob = 0.95, depth = 2)
  
  ## Save stanfit
  
  saveRDS(model_M_Metabol, "Outputs/M_Belcher/M_Belcher_model.rds")

  ## Extract samples
  
  ## Plot intervals
  
  post <- extract.samples(model_M_Metabol)
  post <- as.data.frame(post)
  
  colnames(post)[201:203] <- c("a", "b", "sigma")
  
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
  