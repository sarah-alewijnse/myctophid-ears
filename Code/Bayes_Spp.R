#### Bayesian Models - Species ####

library(tidyverse)
library(rethinking)

myct <- read.csv("Outputs/Combined.csv")
glimpse(myct)

#### Overall Model with Species ####

M_S_list <- list(
  M_obs = myct$M,
  M_sd = myct$sd_M,
  Species = myct$sciname
)

model_S <- map2stan(
  alist(
    M_est ~ dnorm(mu, sigma),
    mu <- a + a_Var[Species],
    M_obs ~ dnorm(M_est, M_sd),
    a ~ dnorm(0.25, 1),
    a_Var[Species] ~ dnorm(0, sigma_Species),
    sigma_Species ~ dunif(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = M_S_list,
  start = list(M_est = M_S_list$M_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99))

## Precis Tables

precis(model_S, depth = 2, digits = 4)

post_spp <- extract.samples(model_S)
write.csv(post_spp, "Outputs/Posterior_Species.csv")

post_spp <- as.data.frame(post_spp)
colnames(post_spp)[109:117] <- c("a_1", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "sigma_Species", "sigma")

mcmc_trace(post_spp,  pars = c("a_1", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "sigma_Species", "sigma"),
           facet_args = list(nrow = 3, labeller = label_parsed))
plot <- p + facet_text(size = 15) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"))  # Print the minor gridlines