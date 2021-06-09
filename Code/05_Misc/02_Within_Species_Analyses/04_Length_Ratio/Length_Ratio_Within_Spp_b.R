#### Size Analysis Outputs ####

# Load required packages

library(tidyverse)
library(rethinking) # Used to interface with rstan
library(bayesplot) # Gives nice plots

# Print out all results

options(max.print=999999)

#### ELN ####

mod_ELN <- readRDS("Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/ELN/Length_Ratio_Cresp.rds")

#### Table Outputs ####

## Get Precis table

table <- precis(mod_ELN, digits = 4, prob = 0.95, depth = 2)
table

# Tidy

means <- data.frame()
for(i in 20:22){
  m <- table[i, 1]
  means <- rbind(means, m)
}

sds <- data.frame()
for(i in 20:22){
  m <- table[i, 2]
  sds <- rbind(sds, m)
}

# Add variable names

precis_tidy <- cbind(means, sds)
var_names <- c("a", "b_R", "sigma")
precis_tidy <- cbind(var_names, precis_tidy)
colnames(precis_tidy) <- c("var_names", "mean", "stan_dev")

precis_tidy[, 2:3] <- round(precis_tidy[,2:3], digits = 3)

precis_tidy ### Use this for results

write.csv(precis_tidy, "Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/ELN/Length_Ratio_precis.csv", row.names = F)

#### Graph Outputs ####

# Extract samples

post <- extract.samples(mod_ELN)
post <- as.data.frame(post)

colnames(post)[20:22] <- c("a", "b_R", "sigma")

# Plot pairs

svg("Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/ELN/Length_Ratio_Cresp_pairs.svg")
pairs(mod_ELN, pars = c("a", "b_R", "sigma"))
dev.off()

# Plot intervals

color_scheme_set("darkgray")

svg("Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/ELN/Length_Ratio_Cresp_posteriors.svg")
mcmc_intervals(post,
               pars = c("sigma", "b_R", "a"),
               prob = 0.5, prob_outer = 0.95) +
  labs(x = "Posterior Predictions", y = "Parameters") +
  scale_y_discrete(labels = c("sigma", expression ("b" ["Ratio"]), "a")) +
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

svg("Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/ELN/Length_Ratio_Cresp_traceplots.svg")
p <- mcmc_trace(post, pars = c("a", "b_R", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot
dev.off()

#### ELC ####

mod_ELC <- readRDS("Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/ELC/Length_Ratio_Cresp.rds")

#### Table Outputs ####

## Get Precis table

table <- precis(mod_ELC, digits = 4, prob = 0.95, depth = 2)
table

# Tidy

means <- data.frame()
for(i in 11:13){
  m <- table[i, 1]
  means <- rbind(means, m)
}

sds <- data.frame()
for(i in 11:13){
  m <- table[i, 2]
  sds <- rbind(sds, m)
}

# Add variable names

precis_tidy <- cbind(means, sds)
var_names <- c("a", "b_R", "sigma")
precis_tidy <- cbind(var_names, precis_tidy)
colnames(precis_tidy) <- c("var_names", "mean", "stan_dev")

precis_tidy[, 2:3] <- round(precis_tidy[,2:3], digits = 3)

precis_tidy ### Use this for results

write.csv(precis_tidy, "Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/ELC/Length_Ratio_precis.csv", row.names = F)

#### Graph Outputs ####

# Extract samples

post <- extract.samples(mod_ELC)
post <- as.data.frame(post)

colnames(post)[11:13] <- c("a", "b_R", "sigma")

# Plot pairs

svg("Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/ELC/Length_Ratio_Cresp_pairs.svg")
pairs(mod_ELC, pars = c("a", "b_R", "sigma"))
dev.off()

# Plot intervals

color_scheme_set("darkgray")

svg("Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/ELC/Length_Ratio_Cresp_posteriors.svg")
mcmc_intervals(post,
               pars = c("sigma", "b_R", "a"),
               prob = 0.5, prob_outer = 0.95) +
  labs(x = "Posterior Predictions", y = "Parameters") +
  scale_y_discrete(labels = c("sigma", expression ("b" ["Ratio"]), "a")) +
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

svg("Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/ELC/Length_Ratio_Cresp_traceplots.svg")
p <- mcmc_trace(post, pars = c("a", "b_R", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot
dev.off()

#### GYR ####

mod_GYR <- readRDS("Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/GYR/Length_Ratio_Cresp.rds")

#### Table Outputs ####

## Get Precis table

table <- precis(mod_GYR, digits = 4, prob = 0.95, depth = 2)
table

# Tidy

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

# Add variable names

precis_tidy <- cbind(means, sds)
var_names <- c("a", "b_R", "sigma")
precis_tidy <- cbind(var_names, precis_tidy)
colnames(precis_tidy) <- c("var_names", "mean", "stan_dev")

precis_tidy[, 2:3] <- round(precis_tidy[,2:3], digits = 3)

precis_tidy ### Use this for results

write.csv(precis_tidy, "Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/GYR/Length_Ratio_precis.csv", row.names = F)

#### Graph Outputs ####

# Extract samples

post <- extract.samples(mod_GYR)
post <- as.data.frame(post)

colnames(post)[21:23] <- c("a", "b_R", "sigma")

# Plot pairs

svg("Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/GYR/Length_Ratio_Cresp_pairs.svg")
pairs(mod_GYR, pars = c("a", "b_R", "sigma"))
dev.off()

# Plot intervals

color_scheme_set("darkgray")

svg("Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/GYR/Length_Ratio_Cresp_posteriors.svg")
mcmc_intervals(post,
               pars = c("sigma", "b_R", "a"),
               prob = 0.5, prob_outer = 0.95) +
  labs(x = "Posterior Predictions", y = "Parameters") +
  scale_y_discrete(labels = c("sigma", expression ("b" ["Ratio"]), "a")) +
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

svg("Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/GYR/Length_Ratio_Cresp_traceplots.svg")
p <- mcmc_trace(post, pars = c("a", "b_R", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot
dev.off()

#### GYN ####

mod_GYN <- readRDS("Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/GYN/Length_Ratio_Cresp.rds")

#### Table Outputs ####

## Get Precis table

table <- precis(mod_GYN, digits = 4, prob = 0.95, depth = 2)
table

# Tidy

means <- data.frame()
for(i in 13:15){
  m <- table[i, 1]
  means <- rbind(means, m)
}

sds <- data.frame()
for(i in 13:15){
  m <- table[i, 2]
  sds <- rbind(sds, m)
}

# Add variable names

precis_tidy <- cbind(means, sds)
var_names <- c("a", "b_R", "sigma")
precis_tidy <- cbind(var_names, precis_tidy)
colnames(precis_tidy) <- c("var_names", "mean", "stan_dev")

precis_tidy[, 2:3] <- round(precis_tidy[,2:3], digits = 3)

precis_tidy ### Use this for results

write.csv(precis_tidy, "Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/GYN/Length_Ratio_precis.csv", row.names = F)

#### Graph Outputs ####

# Extract samples

post <- extract.samples(mod_GYN)
post <- as.data.frame(post)

colnames(post)[13:15] <- c("a", "b_R", "sigma")

# Plot pairs

svg("Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/GYN/Length_Ratio_Cresp_pairs.svg")
pairs(mod_GYN, pars = c("a", "b_R", "sigma"))
dev.off()

# Plot intervals

color_scheme_set("darkgray")

svg("Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/GYN/Length_Ratio_Cresp_posteriors.svg")
mcmc_intervals(post,
               pars = c("sigma", "b_R", "a"),
               prob = 0.5, prob_outer = 0.95) +
  labs(x = "Posterior Predictions", y = "Parameters") +
  scale_y_discrete(labels = c("sigma", expression ("b" ["Ratio"]), "a")) +
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

svg("Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/GYN/Length_Ratio_Cresp_traceplots.svg")
p <- mcmc_trace(post, pars = c("a", "b_R", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot
dev.off()

#### PRM ####

mod_PRM <- readRDS("Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/PRM/Length_Ratio_Cresp.rds")

#### Table Outputs ####

## Get Precis table

table <- precis(mod_PRM, digits = 4, prob = 0.95, depth = 2)
table

# Tidy

means <- data.frame()
for(i in 18:20){
  m <- table[i, 1]
  means <- rbind(means, m)
}

sds <- data.frame()
for(i in 18:20){
  m <- table[i, 2]
  sds <- rbind(sds, m)
}

# Add variable names

precis_tidy <- cbind(means, sds)
var_names <- c("a", "b_R", "sigma")
precis_tidy <- cbind(var_names, precis_tidy)
colnames(precis_tidy) <- c("var_names", "mean", "stan_dev")

precis_tidy[, 2:3] <- round(precis_tidy[,2:3], digits = 3)

precis_tidy ### Use this for results

write.csv(precis_tidy, "Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/PRM/Length_Ratio_precis.csv", row.names = F)

#### Graph Outputs ####

# Extract samples

post <- extract.samples(mod_PRM)
post <- as.data.frame(post)

colnames(post)[18:20] <- c("a", "b_R", "sigma")

# Plot pairs

svg("Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/PRM/Length_Ratio_Cresp_pairs.svg")
pairs(mod_PRM, pars = c("a", "b_R", "sigma"))
dev.off()

# Plot intervals

color_scheme_set("darkgray")

svg("Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/PRM/Length_Ratio_Cresp_posteriors.svg")
mcmc_intervals(post,
               pars = c("sigma", "b_R", "a"),
               prob = 0.5, prob_outer = 0.95) +
  labs(x = "Posterior Predictions", y = "Parameters") +
  scale_y_discrete(labels = c("sigma", expression ("b" ["Ratio"]), "a")) +
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

svg("Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/PRM/Length_Ratio_Cresp_traceplots.svg")
p <- mcmc_trace(post, pars = c("a", "b_R", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot
dev.off()
