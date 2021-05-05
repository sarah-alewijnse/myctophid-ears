#### Crush/Mill Model Outputs ####

# Load required packages

library(tidyverse)
library(rethinking) # Used to interface with rstan
library(bayesplot) # Gives nice plots

# Print out all results

options(max.print=999999)

#### PRM Only ####

model_PRM_crush <- readRDS("Outputs/04_Misc/01_Methods/Crush_Mill/Temp/PRM_Crush_model.rds")

#### Table Outputs ####

## Get Precis table

table <- precis(model_PRM_crush, digits = 4, prob = 0.95, depth = 2)
table

## Get outputs

means <- data.frame()
for(i in 21:23){
  m <- table[i, 1]
  means <- rbind(means, m)
}

sds <- data.frame()
for(i in 21:23){
  m <- table[i, 2]
  sds <- rbind(sds, m)
}

## Add variable names

precis_tidy <- cbind(means, sds)
var_names <- c("a", "b", "sigma")
precis_tidy <- cbind(var_names, precis_tidy)
colnames(precis_tidy) <- c("var_names", "mean", "stan_dev")

precis_tidy[, 2:3] <- round(precis_tidy[,2:3], digits = 3)

precis_tidy ### Use this for results

write.csv(precis_tidy, "Outputs/04_Misc/01_Methods/Crush_Mill/Temp/PRM_Crush_precis.csv", row.names = F)

#### Graph Outputs ####

# Extract samples

post <- extract.samples(model_PRM_crush)
post <- as.data.frame(post)

colnames(post)[21:23] <- c("a", "b_C", "sigma")

# Plot pairs

svg("Outputs/04_Misc/01_Methods/Crush_Mill/Temp/PRM_Pairs.svg")
pairs(model_PRM_crush, pars = c("a", "b_C", "sigma"))
dev.off()

# Plot intervals

color_scheme_set("darkgray")

svg("Outputs/04_Misc/01_Methods/Crush_Mill/Temp/PRM_Posteriors.svg", width = 10, height = 5)
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
dev.off()

# Plot trace

svg("Outputs/04_Misc/01_Methods/Crush_Mill/Temp/PRM_Traceplots.svg")
p <- mcmc_trace(post, pars = c("a", "b_C", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot
dev.off()

#### All With Species ####

model_all_crush <- readRDS("Outputs/04_Misc/01_Methods/Crush_Mill/Temp/All_Crush_model.rds")

# Get Precis table

table <- precis(model_all_crush, digits = 4, prob = 0.95, depth = 2)
table

# Get outputs

means <- data.frame()
for(i in 109:118){
  m <- table[i, 1]
  means <- rbind(means, m)
}

sds <- data.frame()
for(i in 109:118){
  m <- table[i, 2]
  sds <- rbind(sds, m)
}

# Add variable names

precis_tidy <- cbind(means, sds)
var_names <- c("a", "b_C", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "sigma_Species", "sigma")
precis_tidy <- cbind(var_names, precis_tidy)
colnames(precis_tidy) <- c("var_names", "mean", "stan_dev")

precis_tidy[, 2:3] <- round(precis_tidy[,2:3], digits = 3)

precis_tidy ### Use this for results

write.csv(precis_tidy, "Outputs/04_Misc/01_Methods/Crush_Mill/Temp/All_Crush_precis.csv", row.names = F)

#### Graph Outputs ####

# Extract samples

post <- extract.samples(model_all_crush)
post <- as.data.frame(post)

colnames(post)[109:118] <- c("a", "b_C", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "sigma_Species", "sigma")

# Plot pairs

svg("Outputs/04_Misc/01_Methods/Crush_Mill/Temp/All_Crush_Pairs.svg")
pairs(model_all_crush, pars = c("a", "b_C", "sigma_Species", "sigma"))
dev.off()

# Plot intervals

color_scheme_set("darkgray")

svg("Outputs/04_Misc/01_Methods/Crush_Mill/Temp/All_Crush_Posteriors.svg")
mcmc_intervals(post,
               pars = c("sigma", "sigma_Species", "a_Var_GYN", "a_Var_PRM", "a_Var_ELC", "a_Var_KRA", "a_Var_GYR",  "a_Var_ELN", "b_C", "a"),
               prob = 0.5, prob_outer = 0.95) +
  labs(x = "Posterior Predictions", y = "Parameters") +
  scale_y_discrete(labels = c("sigma", expression("sigma" ["species"]), expression("a_Var" ["GYN"]), expression("a_Var" ["PRM"]), expression("a_Var" ["ELC"]), expression("a_Var" ["KRA"]), expression("a_Var" ["GYR"]), expression("a_Var" ["ELN"]), expression ("b" ["Prep"]), "a")) +
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

svg("Outputs/04_Misc/01_Methods/Crush_Mill/Temp/All_Crush_Traceplots.svg")
p <- mcmc_trace(post, pars = c("a", "b_C", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "sigma_Species", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot
dev.off()
