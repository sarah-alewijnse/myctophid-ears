#### Bayesian Linear Models - Weight and Temp ####

library(tidyverse)
library(rethinking)
library(bayesplot)

myct <- read.csv("Outputs/Combined.csv")
glimpse(myct)

#### Overall Model with Weight and Temp ####

myct_tidy <- filter(myct, Weight_SD == "0")
myct_tidy$log10_Weight <- log10(myct_tidy$Weight.x)

M_T_W_list <- list(
  M_obs = myct_tidy$M,
  M_sd = myct_tidy$sd_M,
  Weight = myct_tidy$log10_Weight,
  Temp_obs = myct_tidy$temp,
  Temp_sd = myct_tidy$sd_temp,
  Species = myct_tidy$sciname
)

model_M_T_W <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    mu <- a +
      b_W*Weight +
      b_T*Temp_est[i] +
      a_Var[Species],
    M_obs ~ dnorm(M_est, M_sd),
    Temp_obs ~ dnorm(Temp_est, Temp_sd),
    a ~ dnorm(0.20, 0.5),
    b_W ~ dnorm(0, 0.5),
    b_T ~ dnorm(0, 0.5),
    a_Var[Species] ~ dnorm(0 , sigma_Species),
    sigma_Species ~ dcauchy(0, 1),
    sigma ~ dcauchy(0, 0.1)
  ),
  data = M_T_W_list,
  start = list(M_est = M_T_W_list$M_obs,
               Temp_est = M_T_W_list$Temp_obs),
  WAIC = FALSE,
  iter = 1000000,
  warmup = 5e+05,
  chains = 1,
  cores = 1,
  control = list(adapt_delta = 0.999))

post <- extract.samples(model_M_T_W)
post <- as.data.frame(post)

write.csv(post, "Outputs/Big_Model.csv")

## Precis Tables

precis(model_M_T_W)
p <- precis(model_M_T_W, depth = 2, digits = 4)

post <- read.csv("Outputs/Big_Model.csv")
colnames(post)[202:212] <- c("a_1", "b_W", "b_T", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "sigma_Species", "sigma")

mcmc_intervals(post,
           pars = c("a_1", "b_W", "b_T", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "sigma_Species", "sigma"),
           prob = 0.5, prob_outer = 0.9) +
  labs(x = "Posterior Predictions", y = "Parameters") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"))  # Print the minor gridlines

color_scheme_set("gray")

## Plot chains

post <- read.csv("Outputs/Big_Model.csv")

## Traceplot

color_scheme_set("gray")

p <- mcmc_trace(post,  pars = c("a_1", "b_W", "b_T", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "sigma_Species", "sigma"),
                facet_args = list(nrow = 3, labeller = label_parsed))
plot <- p + facet_text(size = 15) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"))  # Print the minor gridlines
