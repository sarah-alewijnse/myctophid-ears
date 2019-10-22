#### Bayesian Linear Models - Weight and Temp ####

setwd("~/PhD/GitHub/mytophid-ears/MixSIAR")
library(tidyverse)
library(rethinking)
library(bayesplot)

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

model_M_T_W <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + a_Var[Species] +
      b_W*Weight +
      b_T*Temp_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    Temp_obs ~ dnorm(Temp_est, Temp_sd),
    
    # Transformed parameters
    a <- 1*a_raw,
    b_T <- 1*b_T_raw,
    b_W <- 1*b_W_raw,
    sigma <- 1*sigma_raw,
    
    # Normal parameters
    a_raw ~ dnorm(0, 1),
    b_W_raw ~ dnorm(0, 1),
    b_T_raw ~ dnorm(0, 1),
    a_Var[Species] ~ dnorm(0 , sigma_Species),
    sigma_Species ~ dcauchy(0, 1),
    sigma_raw ~ dcauchy(0, 1)
  ),
  data = M_T_W_list,
  start = list(M_est = M_T_W_list$M_obs,
               Temp_est = M_T_W_list$Temp_obs),
  WAIC = FALSE,
  iter = 1000,
  warmup = 500,
  chains = 3,
  cores = 3,
  control = list(adapt_delta = 0.9999, max_treedepth = 15))

## Extract the posterior

post <- extract.samples(model_M_T_W)
post <- as.data.frame(post)
write.csv(post, "Outputs/M_T_W_Post_Short.csv")

## Run diagnostics

colnames(post)[217:227] <- c("a_1", "b_W", "b_T", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "sigma_Species", "sigma")

check_energy(model_M_T_W@stanfit)
check_treedepth(model_M_T_W@stanfit)

divergent <- get_sampler_params(model_M_T_W@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_M_T_W, digits = 4)

## Plot intervals

mcmc_intervals(post,
               pars = c("a_1", "b_W", "b_T", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "sigma_Species", "sigma"),
               prob = 0.5, prob_outer = 0.9) +
  labs(x = "Posterior Predictions", y = "Parameters") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"))  # Print the minor gridlines

## Plot pairs

pairs(model_M_T_W, pars = c("a_raw", "b_W_raw", "b_T_raw", "sigma_raw", "sigma_Species"))

## Plot trace

p <- mcmc_trace(post,  pars = c("a_1", "b_W", "b_T", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "sigma_Species", "sigma"),
                facet_args = list(nrow = 3, labeller = label_parsed))
plot <- p + facet_text(size = 15) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"))  # Print the minor gridlines
plot

