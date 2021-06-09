#### Size Analysis Outputs ####

# Load required packages

library(tidyverse)
library(rethinking) # Used to interface with rstan
library(bayesplot) # Gives nice plots

# Print out all results

options(max.print=999999)

#### Ratio Only ####

model_ratio <- readRDS("Outputs/04_Misc/02_Among_Species/Length_Ratio/Length_Ratio_Cresp.rds")

#### Table Outputs ####

## Get Precis table

table <- precis(model_ratio, digits = 4, prob = 0.95, depth = 2)
table

# Tidy

means <- data.frame()
for(i in 79:87){
  m <- table[i, 1]
  means <- rbind(means, m)
}

sds <- data.frame()
for(i in 79:87){
  m <- table[i, 2]
  sds <- rbind(sds, m)
}

# Add variable names

precis_tidy <- cbind(means, sds)
var_names <- c("a", "b_R", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_PRM", "sigma_Species", "sigma")
precis_tidy <- cbind(var_names, precis_tidy)
colnames(precis_tidy) <- c("var_names", "mean", "stan_dev")

precis_tidy[, 2:3] <- round(precis_tidy[,2:3], digits = 3)

precis_tidy ### Use this for results

write.csv(precis_tidy, "Outputs/04_Misc/02_Among_Species/Length_Ratio/Length_Ratio_Cresp_precis.csv", row.names = F)

#### Graph Outputs ####

# Extract samples

post <- extract.samples(model_ratio)
post <- as.data.frame(post)

colnames(post)[79:87] <- c("a", "b_R", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_PRM", "sigma_Species", "sigma")

# Plot pairs

svg("Outputs/04_Misc/02_Among_Species/Length_Ratio/Length_Ratio_Cresp_pairs.svg")
pairs(model_ratio, pars = c("a", "b_R", "sigma_Species", "sigma"))
dev.off()

# Plot intervals

color_scheme_set("darkgray")

svg("Outputs/04_Misc/02_Among_Species/Length_Ratio/Length_Ratio_Cresp_posteriors.svg")
mcmc_intervals(post,
               pars = c("sigma", "sigma_Species", "a_Var_GYN", "a_Var_PRM", "a_Var_ELC", "a_Var_GYR",  "a_Var_ELN", "b_R", "a"),
               prob = 0.5, prob_outer = 0.95) +
  labs(x = "Posterior Predictions", y = "Parameters") +
  scale_y_discrete(labels = c("sigma", expression("sigma" ["species"]), expression("a_Var" ["GYN"]), expression("a_Var" ["PRM"]), expression("a_Var" ["ELC"]), expression("a_Var" ["GYR"]), expression("a_Var" ["ELN"]), expression ("b" ["Ratio"]), "a")) +
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 15, face = "italic"),
        text = element_text(size = 15, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 15),
        axis.text.y = element_text(colour = "black", face = "plain", size = 15))
dev.off()

# Plot trace

svg("Outputs/04_Misc/02_Among_Species/Length_Ratio/Length_Ratio_Cresp_traceplots.svg")
p <- mcmc_trace(post, pars = c("a", "b_R", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_PRM", "sigma_Species", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot
dev.off()

#### Ratio and Temp ####

model_ratio <- readRDS("Outputs/04_Misc/02_Among_Species/Length_Ratio/Length_Ratio_Temp_Cresp.rds")

## Table outputs

table <- precis(model_ratio, digits = 4, prob = 0.95, depth = 2)
table

# Tidy

means <- data.frame()
for(i in 157:166){
  m <- table[i, 1]
  means <- rbind(means, m)
}

sds <- data.frame()
for(i in 157:166){
  m <- table[i, 2]
  sds <- rbind(sds, m)
}

# Add variable names

precis_tidy <- cbind(means, sds)
var_names <- c("a", "b_R", "b_T", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_PRM", "sigma_Species", "sigma")
precis_tidy <- cbind(var_names, precis_tidy)
colnames(precis_tidy) <- c("var_names", "mean", "stan_dev")

precis_tidy[, 2:3] <- round(precis_tidy[,2:3], digits = 3)

precis_tidy ### Use this for results

write.csv(precis_tidy, "Outputs/04_Misc/02_Among_Species/Length_Ratio/Length_Ratio_Temp_Cresp_precis.csv", row.names = F)

## Groph outputs

# Extract samples

post <- extract.samples(model_ratio)
post <- as.data.frame(post)

colnames(post)[157:166] <- c("a", "b_R", "b_T", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_PRM", "sigma_Species", "sigma")

# Plot pairs

svg("Outputs/04_Misc/02_Among_Species/Length_Ratio/Length_Ratio_Temp_Cresp_pairs.svg")
pairs(model_ratio, pars = c("a", "b_R", "b_T", "sigma_Species", "sigma"))
dev.off()

# Plot intervals

color_scheme_set("darkgray")

svg("Outputs/04_Misc/02_Among_Species/Length_Ratio/Length_Ratio_Temp_Cresp_posteriors.svg")
mcmc_intervals(post,
               pars = c("sigma", "sigma_Species", "a_Var_GYN", "a_Var_PRM", "a_Var_ELC", "a_Var_GYR",  "a_Var_ELN", "b_T", "b_R", "a"),
               prob = 0.5, prob_outer = 0.95) +
  labs(x = "Posterior Predictions", y = "Parameters") +
  scale_y_discrete(labels = c("sigma", expression("sigma" ["species"]), expression("a_Var" ["GYN"]), expression("a_Var" ["PRM"]), expression("a_Var" ["ELC"]), expression("a_Var" ["GYR"]), expression("a_Var" ["ELN"]), 
                              expression ("b" ["Temp"]), expression ("b" ["Ratio"]), "a")) +
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 15, face = "italic"),
        text = element_text(size = 15, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 15),
        axis.text.y = element_text(colour = "black", face = "plain", size = 15))
dev.off()

# Plot trace

svg("Outputs/04_Misc/02_Among_Species/Length_Ratio/Length_Ratio_Temp_Cresp_traceplots.svg")
p <- mcmc_trace(post, pars = c("a", "b_R", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_PRM", "sigma_Species", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot
dev.off()
