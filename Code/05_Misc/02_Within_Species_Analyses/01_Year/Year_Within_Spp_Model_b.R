#### Year Within Spp Model ####

# Load required packages

library(tidyverse)
library(rethinking) # Used to interface with rstan
library(bayesplot) # Gives nice plots

# Print out all results

options(max.print=999999)

#### ELN Only ####

model_ELN_year <- readRDS("Outputs/04_Misc/05_Year/ELN_Year_model.rds")

#### Table Outputs ####

## Get Precis table

table <- precis(model_ELN_year, digits = 4, prob = 0.95, depth = 2)
table

## Get outputs

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

## Add variable names

precis_tidy <- cbind(means, sds)
var_names <- c("a", "b", "sigma")
precis_tidy <- cbind(var_names, precis_tidy)
colnames(precis_tidy) <- c("var_names", "mean", "stan_dev")

precis_tidy[, 2:3] <- round(precis_tidy[,2:3], digits = 3)

precis_tidy ### Use this for results

write.csv(precis_tidy, "Outputs/04_Misc/05_Year/ELN_Year_precis.csv", row.names = F)

#### Graph Outputs ####

# Extract samples

post <- extract.samples(model_ELN_year)
post <- as.data.frame(post)

colnames(post)[20:22] <- c("a", "b_Y", "sigma")

# Plot pairs

svg("Outputs/04_Misc/05_Year/ELN_Pairs.svg")
pairs(model_ELN_year, pars = c("a", "b_Y", "sigma"))
dev.off()

# Plot intervals

color_scheme_set("darkgray")

svg("Outputs/04_Misc/05_Year/ELN_posterior.svg", width = 10, height = 5)
mcmc_intervals(post,
               pars = c("sigma", "b_Y", "a"),
               prob = 0.5, prob_outer = 0.95) +
  labs(x = "Posterior Predictions", y = "Parameters") +
  scale_y_discrete(labels = c("sigma", expression("b" ["Year"]), "a")) +
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

svg("Outputs/04_Misc/05_ELN_Traceplots.svg")
p <- mcmc_trace(post, pars = c("a", "b_Y", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot
dev.off()

#### ELC Only ####

model_ELC_year <- readRDS("Outputs/04_Misc/05_Year/ELC_Year_model.rds")

#### Table Outputs ####

## Get Precis table

table <- precis(model_ELC_year, digits = 4, prob = 0.95, depth = 2)
table

## Get outputs

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

## Add variable names

precis_tidy <- cbind(means, sds)
var_names <- c("a", "b", "sigma")
precis_tidy <- cbind(var_names, precis_tidy)
colnames(precis_tidy) <- c("var_names", "mean", "stan_dev")

precis_tidy[, 2:3] <- round(precis_tidy[,2:3], digits = 3)

precis_tidy ### Use this for results

write.csv(precis_tidy, "Outputs/04_Misc/05_Year/ELC_Year_precis.csv", row.names = F)

#### Graph Outputs ####

# Extract samples

post <- extract.samples(model_ELC_year)
post <- as.data.frame(post)

colnames(post)[18:20] <- c("a", "b_Y", "sigma")

# Plot pairs

svg("Outputs/04_Misc/05_Year/ELC_Pairs.svg")
pairs(model_ELC_year, pars = c("a", "b_Y", "sigma"))
dev.off()

# Plot intervals

color_scheme_set("darkgray")

svg("Outputs/04_Misc/05_Year/ELC_posterior.svg", width = 10, height = 5)
mcmc_intervals(post,
               pars = c("sigma", "b_Y", "a"),
               prob = 0.5, prob_outer = 0.95) +
  labs(x = "Posterior Predictions", y = "Parameters") +
  scale_y_discrete(labels = c("sigma", expression("b" ["Year"]), "a")) +
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

svg("Outputs/04_Misc/05_ELC_Traceplots.svg")
p <- mcmc_trace(post, pars = c("a", "b_Y", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot
dev.off()

#### GYR Only ####

model_GYR_year <- readRDS("Outputs/04_Misc/05_Year/GYR_Year_model.rds")

#### Table Outputs ####

## Get Precis table

table <- precis(model_GYR_year, digits = 4, prob = 0.95, depth = 2)
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

write.csv(precis_tidy, "Outputs/04_Misc/05_Year/GYR_Year_precis.csv", row.names = F)

#### Graph Outputs ####

# Extract samples

post <- extract.samples(model_GYR_year)
post <- as.data.frame(post)

colnames(post)[21:23] <- c("a", "b_Y", "sigma")

# Plot pairs

svg("Outputs/04_Misc/05_Year/GYR_Pairs.svg")
pairs(model_GYR_year, pars = c("a", "b_Y", "sigma"))
dev.off()

# Plot intervals

color_scheme_set("darkgray")

svg("Outputs/04_Misc/05_Year/GYR_posterior.svg", width = 10, height = 5)
mcmc_intervals(post,
               pars = c("sigma", "b_Y", "a"),
               prob = 0.5, prob_outer = 0.95) +
  labs(x = "Posterior Predictions", y = "Parameters") +
  scale_y_discrete(labels = c("sigma", expression("b" ["Year"]), "a")) +
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

svg("Outputs/04_Misc/05_GYR_Traceplots.svg")
p <- mcmc_trace(post, pars = c("a", "b_Y", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot
dev.off()

#### GYN Only ####

model_GYN_year <- readRDS("Outputs/04_Misc/05_Year/GYN_Year_model.rds")

#### Table Outputs ####

## Get Precis table

table <- precis(model_GYN_year, digits = 4, prob = 0.95, depth = 2)
table

## Get outputs

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

## Add variable names

precis_tidy <- cbind(means, sds)
var_names <- c("a", "b", "sigma")
precis_tidy <- cbind(var_names, precis_tidy)
colnames(precis_tidy) <- c("var_names", "mean", "stan_dev")

precis_tidy[, 2:3] <- round(precis_tidy[,2:3], digits = 3)

precis_tidy ### Use this for results

write.csv(precis_tidy, "Outputs/04_Misc/05_Year/GYN_Year_precis.csv", row.names = F)

#### Graph Outputs ####

# Extract samples

post <- extract.samples(model_GYN_year)
post <- as.data.frame(post)

colnames(post)[13:15] <- c("a", "b_Y", "sigma")

# Plot pairs

svg("Outputs/04_Misc/05_Year/GYN_Pairs.svg")
pairs(model_GYN_year, pars = c("a", "b_Y", "sigma"))
dev.off()

# Plot intervals

color_scheme_set("darkgray")

svg("Outputs/04_Misc/05_Year/GYN_posterior.svg", width = 10, height = 5)
mcmc_intervals(post,
               pars = c("sigma", "b_Y", "a"),
               prob = 0.5, prob_outer = 0.95) +
  labs(x = "Posterior Predictions", y = "Parameters") +
  scale_y_discrete(labels = c("sigma", expression("b" ["Year"]), "a")) +
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

svg("Outputs/04_Misc/05_GYN_Traceplots.svg")
p <- mcmc_trace(post, pars = c("a", "b_Y", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot
dev.off()

#### KRA Only ####

model_KRA_year <- readRDS("Outputs/04_Misc/05_Year/KRA_Year_model.rds")

#### Table Outputs ####

## Get Precis table

table <- precis(model_KRA_year, digits = 4, prob = 0.95, depth = 2)
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

write.csv(precis_tidy, "Outputs/04_Misc/05_Year/KRA_Year_precis.csv", row.names = F)

#### Graph Outputs ####

# Extract samples

post <- extract.samples(model_KRA_year)
post <- as.data.frame(post)

colnames(post)[21:23] <- c("a", "b_Y", "sigma")

# Plot pairs

svg("Outputs/04_Misc/05_Year/KRA_Pairs.svg")
pairs(model_KRA_year, pars = c("a", "b_Y", "sigma"))
dev.off()

# Plot intervals

color_scheme_set("darkgray")

svg("Outputs/04_Misc/05_Year/KRA_posterior.svg", width = 10, height = 5)
mcmc_intervals(post,
               pars = c("sigma", "b_Y", "a"),
               prob = 0.5, prob_outer = 0.95) +
  labs(x = "Posterior Predictions", y = "Parameters") +
  scale_y_discrete(labels = c("sigma", expression("b" ["Year"]), "a")) +
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

svg("Outputs/04_Misc/05_KRA_Traceplots.svg")
p <- mcmc_trace(post, pars = c("a", "b_Y", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot
dev.off()

#### PRM Only ####

model_PRM_year <- readRDS("Outputs/04_Misc/02_Within_Spp/01_Year/PRM/PRM_Year_model.rds")

#### Table Outputs ####

## Get Precis table

table <- precis(model_PRM_year, digits = 4, prob = 0.95, depth = 2)
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

write.csv(precis_tidy, "Outputs/04_Misc/05_Year/PRM_Year_precis.csv", row.names = F)

#### Graph Outputs ####

# Extract samples

post <- extract.samples(model_PRM_year)
post <- as.data.frame(post)

colnames(post)[21:23] <- c("a", "b_Y", "sigma")

# Plot pairs

svg("Outputs/04_Misc/05_Year/PRM_Pairs.svg")
pairs(model_PRM_year, pars = c("a", "b_Y", "sigma"))
dev.off()

# Plot intervals

color_scheme_set("darkgray")

svg("Outputs/04_Misc/05_Year/PRM_posterior.svg", width = 10, height = 5)
mcmc_intervals(post,
               pars = c("sigma", "b_Y", "a"),
               prob = 0.5, prob_outer = 0.95) +
  labs(x = "Posterior Predictions", y = "Parameters") +
  scale_y_discrete(labels = c("sigma", expression("b" ["Year"]), "a")) +
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

svg("Outputs/04_Misc/05_PRM_Traceplots.svg")
p <- mcmc_trace(post, pars = c("a", "b_Y", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot
dev.off()
