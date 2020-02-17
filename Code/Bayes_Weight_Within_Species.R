#### Bayesian Linear Models - Temp - Within Species ####

# Load required packages

library(tidyverse)
library(rethinking)
library(bayesplot)

options(max.print=999999) # Enables viewing of whole output

# Load data

myct <- read.csv("Myctophids_M_Temp_Bel.csv")
glimpse(myct) # Inspect data

# Tidy data

myct_tidy <- filter(myct, Weight_SD == "0") # Exclude those with uncertain weights
myct_tidy <- filter(myct, !is.na(mean_M)) # Exclude those without an M_oto
myct_tidy$log_Weight <- log(myct_tidy$Weight.x) # Natural log weights
glimpse(myct_tidy) # Inspect data

# Put into data fame

M_W_data <- data.frame(
  M_obs = myct_tidy$mean_M,
  M_sd = myct_tidy$sd_M,
  Weight = myct_tidy$Weight.x,
  Species = myct_tidy$sciname
)
glimpse(M_W_data) # Inspect data

#### ELN - Electrona antarctica ####

# Subset by species

ELN <- filter(M_W_data, Species == "Electrona antarctica")
ELN <- as.list(ELN)

# Convert weight to z-score

ELN_weight_mean <- mean(ELN$Weight) # Get species mean of weight
ELN_weight_sd <- sd(ELN$Weight) # Get species standard deviation of weight
for(i in 1:length(ELN$Weight)){ # Loop to get z-scores
  ELN$Weight_Z[i] <- (ELN$Weight[i] - ELN_weight_mean) / ELN_weight_sd
}

# Build and run model

model_ELN <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b_W*Weight,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_sd),
    
    # Parameter priors
    a ~ dnorm(0, 1),
    b_W ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = ELN,
  start = list(M_est = ELN$M_obs),
  WAIC = FALSE,
  iter = 3000,
  warmup = 1500,
  control = list(adapt_delta = 0.90))

# Check diagnostics

check_energy(model_ELN@stanfit) # Calculates E-BFMI

check_treedepth(model_ELN@stanfit) # Gives number of iterations outside of maximum treedepth

divergent <- get_sampler_params(model_ELN@stanfit, inc_warmup=FALSE)[[1]][,'divergent__'] # Calculates number of divergent iterations
sum(divergent)

# Precis output 

precis(model_ELN, digits = 4, prob = 0.95, depth = 2) # Mean, standard deviation, upper and lower 95% limits to 4 decimal places for each parameter

# Check with data

plot(ELN$Weight, ELN$M_obs, pch = 16)
abline(0.2124, 0.0004)

# Pairs plot

pairs(model_ELN, pars = c("a", "b_W", "sigma")) # Check for autocorrelation

# Save stanfit

saveRDS(model_ELN, "Outputs/Within_Species/ELN/M_W_model.rds")
model_ELN <- readRDS("Outputs/Within_Species/ELN/M_W_model.rds")

# Extract samples

post <- extract.samples(model_ELN)
post <- as.data.frame(post)

# Name columns to match variables

colnames(post)[20:22] <- c("a", "b_W", "sigma")

# Plot posterior intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "b_W", "sigma"),
               prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
  labs(x = "Posterior Predictions", y = "Parameters") +
  # Style adjustments
  theme(panel.background = element_blank(),
        legend.position = "none", 
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 10, face = "italic"),
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 10),
        axis.text.y = element_text(colour = "black", face = "plain", size = 10))

# Plot traceplots

p <- mcmc_trace(post, pars = c("a", "b_W", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
# Style adjustments
p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))