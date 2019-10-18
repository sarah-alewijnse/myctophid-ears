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
    mu <- a + a_Var[Species] +
      b_W*Weight +
      b_T*Temp_est[i],
    M_obs ~ dnorm(M_est, M_sd),
    Temp_obs ~ dnorm(Temp_est, Temp_sd),
    a ~ dnorm(0, 1),
    b_W ~ dnorm(0, 1),
    b_T ~ dnorm(0, 1),
    a_Var[Species] ~ dnorm(0 , sigma_Species),
    sigma_Species ~ dexp(1),
    sigma ~ dcauchy(0, 1)
  ),
  data = M_T_W_list,
  start = list(M_est = M_T_W_list$M_obs,
               Temp_est = M_T_W_list$Temp_obs),
  WAIC = FALSE,
  iter = 1e6,
  warmup = 1e5,
  chains = 1,
  cores = 1,
  control = list(adapt_delta = 0.999, max_treedepth = 15))

## Extract the posterior

post <- extract.samples(model_M_T_W)
post <- as.data.frame(post)
write.csv(post, "Outputs/M_T_W_Post.csv")

## Run diagnostics

colnames(post)[217:227] <- c("a_1", "b_W", "b_T", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "sigma_Species", "sigma")

check_energy(model_M_T_W@stanfit)
check_treedepth(model_M_T_W@stanfit)

check_n_eff <- function(fit) {
  fit_summary <- summary(fit, probs = c(0.5))$summary
  N <- dim(fit_summary)[[1]]
  
  iter <- dim(extract(fit)[[1]])[[1]]
  
  no_warning <- TRUE
  for (n in 1:N) {
    ratio <- fit_summary[,5][n] / iter
    if (ratio < 0.001) {
      print(sprintf('n_eff / iter for parameter %s is %s!',
                    rownames(fit_summary)[n], ratio))
      no_warning <- FALSE
    }
  }
  if (no_warning)
    print('n_eff / iter looks reasonable for all parameters')
  else
    print('  n_eff / iter below 0.001 indicates that the effective sample size has likely been overestimated')
}

check_n_eff(model_M_T_W@stanfit)

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

mcmc_pairs(post, c("a_1", "b_W", "b_T", "sigma", "sigma_Species"))

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

