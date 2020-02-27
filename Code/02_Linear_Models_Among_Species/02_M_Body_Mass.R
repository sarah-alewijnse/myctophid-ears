#### Bayesian Linear Models - Weight and Temp ####

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

M_W_list <- list(
  M_obs = myct_tidy$mean_M,
  M_sd = myct_tidy$sd_M,
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
  M_sd = M_W_list$M_sd,
  Weight = M_W_list$Weight_Z,
  Species = M_W_list$Species
)

## Model

model_M_W <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + a_Var[Species] +
      b_W*Weight,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    
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
  iter = 3000,
  warmup = 1500)

## Run diagnostics

check_energy(model_M_W@stanfit)
check_treedepth(model_M_W@stanfit)

divergent <- get_sampler_params(model_M_W@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_M_W, digits = 4, prob = 0.95, depth = 2)

## Save stanfit

saveRDS(model_M_W, "Outputs/M_W/M_W_model.rds")

## Extract samples

## Plot intervals

post <- extract.samples(model_M_W)
post <- as.data.frame(post)

colnames(post)[109:118] <- c("a", "b_W", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "sigma_Species", "sigma")

## Plot pairs

pairs(model_M_W, pars = c("a", "b_W", "sigma", "sigma_Species"))

## Plot intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "b_W", "sigma_Species", "sigma"),
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

p <- mcmc_trace(post, pars = c("a", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "b_W",  "sigma_Species", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot
