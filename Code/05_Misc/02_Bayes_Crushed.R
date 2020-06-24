#### Bayesian Linear Models - Crushed vs. Milled Comparison ####

library(tidyverse)
library(rethinking)
library(bayesplot)

options(max.print=999999)

myct <- read.csv("Data/Myctophids_M_Temp.csv")
glimpse(myct)

#### Within PRM ####

PRM_tidy <- filter(myct, Label == "PRM")
glimpse(PRM_tidy)

mod_list <- list(
  M_obs = PRM_tidy$mean_M,
  M_sd = PRM_tidy$sd_M,
  Crushed = PRM_tidy$Crushed
)

# Model

model_PRM_crush <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b_C*Crushed,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_C ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = mod_list,
  start = list(M_est = mod_list$M_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 5000,
  control = list(adapt_delta = 0.90))

# Check model

check_energy(model_PRM_crush@stanfit)
check_treedepth(model_PRM_crush@stanfit)

divergent <- get_sampler_params(model_PRM_crush@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_PRM_crush, digits = 4, prob = 0.95, depth = 2)

# Save model

saveRDS(model_PRM_crush, "Outputs/05_Misc/Supplementary_Crush_Mill/PRM_Crush_model.rds")

# Extract samples

post <- extract.samples(model_PRM_crush)
post <- as.data.frame(post)

colnames(post)[21:23] <- c("a", "b_C", "sigma")

## Plot pairs

pairs(model_PRM_crush, pars = c("a", "b_C", "sigma"))

## Plot intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("sigma", "b_C", "a"),
               prob = 0.5, prob_outer = 0.95) +
  labs(x = "Posterior Predictions", y = "Parameters") +
  scale_y_discrete(labels = c("sigma", expression("b" ["Prep"]), "a")) +
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 15, face = "italic"),
        text = element_text(size = 15, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 15),
        axis.text.y = element_text(colour = "black", face = "plain", size = 15))

## Plot trace

p <- mcmc_trace(post, pars = c("a", "b_C", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot

#### All with Species ####

myct_tidy <- filter(myct, !is.na(mean_M))

mod_list <- list(
  M_obs = myct_tidy$mean_M,
  M_sd = myct_tidy$sd_M,
  Crushed = myct_tidy$Crushed,
  Species = myct_tidy$sciname
)

# Model

model_all_crush <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + a_Var[Species] + b_C*Crushed,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_C ~ dnorm(0, 1),
    a_Var[Species] ~ dnorm(0 , sigma_Species),
    sigma_Species ~ dcauchy(0, 1),
    sigma ~ dexp(1)
  ),
  data = mod_list,
  start = list(M_est = mod_list$M_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 5000,
  control = list(adapt_delta = 0.90))

# Check model

check_energy(model_all_crush@stanfit)
check_treedepth(model_all_crush@stanfit)

divergent <- get_sampler_params(model_all_crush@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_all_crush, digits = 4, prob = 0.95, depth = 2)

# Save model

saveRDS(model_all_crush, "Outputs/05_Misc/Supplementary_Crush_Mill/All_Crush_model.rds")

# Extract samples

post <- extract.samples(model_all_crush)
post <- as.data.frame(post)

colnames(post)[109:118] <- c("a", "b_C", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "sigma_Species", "sigma")

## Plot pairs

pairs(model_all_crush, pars = c("a", "b_C", "sigma_Species", "sigma"))

## Plot intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("sigma", "sigma_Species", "a_Var_GYN", "a_Var_PRM", "a_Var_ELC", "a_Var_KRA", "a_Var_GYR",  "a_Var_ELN", "b_C", "a"),
               prob = 0.5, prob_outer = 0.95) +
  labs(x = "Posterior Predictions", y = "Parameters") +
  scale_y_discrete(labels = c("sigma", expression("sigma" ["species"]), expression("a_Var" ["GYN"]), expression("a_Var" ["PRM"]), expression("a_Var" ["ELC"]), expression("a_Var" ["KRA"]), expression("a_Var" ["GYR"]), expression("a_Var" ["ELN"]), expression ("b" ["C"]), "a")) +
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 15, face = "italic"),
        text = element_text(size = 15, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 15),
        axis.text.y = element_text(colour = "black", face = "plain", size = 15))

## Plot trace

p <- mcmc_trace(post, pars = c("a", "b_C", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "sigma_Species", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot
