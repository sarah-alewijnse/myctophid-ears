#### Bayesian Linear Models - Weight & Temperature - Within Species ####

# Load required packages

library(tidyverse)
library(rethinking)
library(bayesplot)

options(max.print=999999) # Enables viewing of whole output

# Load data

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")
glimpse(myct) # Inspect data

#### ELN ####

#### Table Output ####

model_ELN <- readRDS("Outputs/03_Linear_Models_Within_Species/ELN/M_T_W_model.rds")

table_ELN <- precis(model_ELN, digits = 4, prob = 0.95, depth = 2)
table_ELN

## Get outputs

means <- data.frame()
for(i in 37:40){
  m <- table_ELN[i, 1]
  means <- rbind(means, m)
}

sds <- data.frame()
for(i in 37:40){
  m <- table_ELN[i, 2]
  sds <- rbind(sds, m)
}

precis_tidy_ELN <- cbind(means, sds)
var_names <- c("a", "b_W", "b_T", "sigma")
precis_tidy_ELN <- cbind(var_names, precis_tidy_ELN)
colnames(precis_tidy_ELN) <- c("var_names", "mean", "stan_dev")
precis_tidy_ELN[, 2:3] <- round(precis_tidy_ELN[,2:3], digits = 3)

precis_tidy_ELN
write.csv(precis_tidy_ELN, "Outputs/03_Linear_Models_Within_Species/ELN/M_T_W_precis.csv", row.names = F)

#### Graph Outputs ####

## Extract samples

post <- extract.samples(model_ELN)
post <- as.data.frame(post)

# Name columns to match vairables

colnames(post)[37:40] <- c("a", "b_W", "b_T", "sigma")

## Plot pairs

svg(file = "Outputs/03_Linear_Models_Within_Species/ELN/Pairs.svg")
pairs(model_ELN, pars = c("a", "b_W", "b_T", "sigma")) # Check for autocorrelation
dev.off()

## Plot intervals

color_scheme_set("darkgray")

svg(file = "Outputs/03_Linear_Models_Within_Species/ELN/Posterior.svg")
mcmc_intervals(post,
               pars = c("a", "b_W", "b_T", "sigma"),
               prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
  labs(x = "Posterior Predictions", y = "Parameters") +
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 10, face = "italic"),
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 10),
        axis.text.y = element_text(colour = "black", face = "plain", size = 10))
dev.off()

## Plot trace

svg(file = "Outputs/03_Linear_Models_Within_Species/ELN/Traceplot.svg")
p <- mcmc_trace(post, pars = c("a", "b_W", "b_T", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
dev.off()

### ELC ####

model_ELC <- readRDS("Outputs/03_Linear_Models_Within_Species/ELC/M_T_W_model.rds")

#### Table Output ####

table_ELC <- precis(model_ELC, digits = 4, prob = 0.95, depth = 2)
table_ELC

## Get outputs

means <- data.frame()
for(i in 21:24){
  m <- table_ELC[i, 1]
  means <- rbind(means, m)
}

sds <- data.frame()
for(i in 21:24){
  m <- table_ELC[i, 2]
  sds <- rbind(sds, m)
}

precis_tidy_ELC <- cbind(means, sds)
var_names <- c("a", "b_W", "b_T", "sigma")
precis_tidy_ELC <- cbind(var_names, precis_tidy_ELC)
colnames(precis_tidy_ELC) <- c("var_names", "mean", "stan_dev")
precis_tidy_ELC[, 2:3] <- round(precis_tidy_ELC[,2:3], digits = 3)

precis_tidy_ELC
write.csv(precis_tidy_ELC, "Outputs/03_Linear_Models_Within_Species/ELC/M_T_W_precis.csv", row.names = F)

#### Table Output ####

table_ELC <- precis(model_ELC, digits = 4, prob = 0.95, depth = 2)
table_ELC

## Get outputs

means <- data.frame()
for(i in 21:24){
  m <- table_ELC[i, 1]
  means <- rbind(means, m)
}

sds <- data.frame()
for(i in 21:24){
  m <- table_ELC[i, 2]
  sds <- rbind(sds, m)
}

precis_tidy_ELC <- cbind(means, sds)
var_names <- c("a", "b_W", "b_T", "sigma")
precis_tidy_ELC <- cbind(var_names, precis_tidy_ELC)
colnames(precis_tidy_ELC) <- c("var_names", "mean", "stan_dev")
precis_tidy_ELC[, 2:3] <- round(precis_tidy_ELC[,2:3], digits = 3)

precis_tidy_ELC
write.csv(precis_tidy_ELC, "Outputs/03_Linear_Models_Within_Species/ELC/M_T_W_precis.csv", row.names = F)

## Extract samples

post <- extract.samples(model_ELC)
post <- as.data.frame(post)

# Name columns to match vairables

colnames(post)[21:24] <- c("a", "b_W", "b_T", "sigma")

## Plot pairs

svg(file = "Outputs/03_Linear_Models_Within_Species/ELC/Pairs.svg")
pairs(model_ELC, pars = c("a", "b_W", "b_T", "sigma")) # Check for autocorrelation
dev.off()

## Plot intervals

color_scheme_set("darkgray")

svg(file = "Outputs/03_Linear_Models_Within_Species/ELC/Posterior.svg")
mcmc_intervals(post,
               pars = c("a", "b_W", "b_T", "sigma"),
               prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
  labs(x = "Posterior Predictions", y = "Parameters") +
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 10, face = "italic"),
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 10),
        axis.text.y = element_text(colour = "black", face = "plain", size = 10))
dev.off()

## Plot trace

svg(file = "Outputs/03_Linear_Models_Within_Species/ELC/Traceplot.svg")
p <- mcmc_trace(post, pars = c("a", "b_W", "b_T", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
dev.off()

#### GYR ####

model_GYR <- readRDS("Outputs/03_Linear_Models_Within_Species/GYR/M_T_W_model.rds")

#### Table Output ####

table_GYR <- precis(model_GYR, digits = 4, prob = 0.95, depth = 2)
table_GYR

## Get outputs

means <- data.frame()
for(i in 41:44){
  m <- table_GYR[i, 1]
  means <- rbind(means, m)
}

sds <- data.frame()
for(i in 41:44){
  m <- table_GYR[i, 2]
  sds <- rbind(sds, m)
}

precis_tidy_GYR <- cbind(means, sds)
var_names <- c("a", "b_W", "b_T", "sigma")
precis_tidy_GYR <- cbind(var_names, precis_tidy_GYR)
colnames(precis_tidy_GYR) <- c("var_names", "mean", "stan_dev")
precis_tidy_GYR[, 2:3] <- round(precis_tidy_GYR[,2:3], digits = 3)

precis_tidy_GYR
write.csv(precis_tidy_GYR, "Outputs/03_Linear_Models_Within_Species/GYR/M_T_W_precis.csv", row.names = F)

## Extract samples

post <- extract.samples(model_GYR)
post <- as.data.frame(post)

# Name columns to match vairables

colnames(post)[41:44] <- c("a", "b_W", "b_T", "sigma")

## Plot pairs

svg(file = "Outputs/03_Linear_Models_Within_Species/GYR/Pairs.svg")
pairs(model_GYR, pars = c("a", "b_W", "b_T", "sigma")) # Check for autocorrelation
dev.off()

## Plot intervals

color_scheme_set("darkgray")

svg(file = "Outputs/03_Linear_Models_Within_Species/GYR/Posterior.svg")
mcmc_intervals(post,
               pars = c("a", "b_W", "b_T", "sigma"),
               prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
  labs(x = "Posterior Predictions", y = "Parameters") +
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 10, face = "italic"),
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 10),
        axis.text.y = element_text(colour = "black", face = "plain", size = 10))
dev.off()

## Plot trace

svg(file = "Outputs/03_Linear_Models_Within_Species/GYR/Traceplot.svg")
p <- mcmc_trace(post, pars = c("a", "b_W", "b_T", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
dev.off()

#### GYN ####

model_GYN <- readRDS("Outputs/03_Linear_Models_Within_Species/GYN/M_T_W_model.rds")

#### Table Output ####

table_GYN <- precis(model_GYN, digits = 4, prob = 0.95, depth = 2)
table_GYN

## Get outputs

means <- data.frame()
for(i in 25:28){
  m <- table_GYN[i, 1]
  means <- rbind(means, m)
}

sds <- data.frame()
for(i in 25:28){
  m <- table_GYN[i, 2]
  sds <- rbind(sds, m)
}

precis_tidy_GYN <- cbind(means, sds)
var_names <- c("a", "b_W", "b_T", "sigma")
precis_tidy_GYN <- cbind(var_names, precis_tidy_GYN)
colnames(precis_tidy_GYN) <- c("var_names", "mean", "stan_dev")
precis_tidy_GYN[, 2:3] <- round(precis_tidy_GYN[,2:3], digits = 3)

precis_tidy_GYN
write.csv(precis_tidy_GYN, "Outputs/03_Linear_Models_Within_Species/GYN/M_T_W_precis.csv", row.names = F)

## Extract samples

post <- extract.samples(model_GYN)
post <- as.data.frame(post)

# Name columns to match vairables

colnames(post)[25:28] <- c("a", "b_W", "b_T", "sigma")

## Plot pairs

svg(file = "Outputs/03_Linear_Models_Within_Species/GYN/Pairs.svg")
pairs(model_GYN, pars = c("a", "b_W", "b_T", "sigma")) # Check for autocorrelation
dev.off()

## Plot intervals

color_scheme_set("darkgray")

svg(file = "Outputs/03_Linear_Models_Within_Species/GYN/Posterior.svg")
mcmc_intervals(post,
               pars = c("a", "b_W", "b_T", "sigma"),
               prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
  labs(x = "Posterior Predictions", y = "Parameters") +
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 10, face = "italic"),
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 10),
        axis.text.y = element_text(colour = "black", face = "plain", size = 10))
dev.off()

## Plot trace

svg(file = "Outputs/03_Linear_Models_Within_Species/GYN/Traceplot.svg")
p <- mcmc_trace(post, pars = c("a", "b_W", "b_T", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
dev.off()

#### KRA ####

model_KRA <- readRDS("Outputs/03_Linear_Models_Within_Species/KRA/M_T_W_model.rds")

#### Table Output ####

table_KRA <- precis(model_KRA, digits = 4, prob = 0.95, depth = 2)
table_KRA

## Get outputs

means <- data.frame()
for(i in 41:44){
  m <- table_KRA[i, 1]
  means <- rbind(means, m)
}

sds <- data.frame()
for(i in 41:44){
  m <- table_KRA[i, 2]
  sds <- rbind(sds, m)
}

precis_tidy_KRA <- cbind(means, sds)
var_names <- c("a", "b_W", "b_T", "sigma")
precis_tidy_KRA <- cbind(var_names, precis_tidy_KRA)
colnames(precis_tidy_KRA) <- c("var_names", "mean", "stan_dev")
precis_tidy_KRA[, 2:3] <- round(precis_tidy_KRA[,2:3], digits = 3)

precis_tidy_KRA
write.csv(precis_tidy_KRA, "Outputs/03_Linear_Models_Within_Species/KRA/M_T_W_precis.csv", row.names = F)

## Extract samples

post <- extract.samples(model_KRA)
post <- as.data.frame(post)

# Name columns to match vairables

colnames(post)[41:44] <- c("a", "b_W", "b_T", "sigma")

## Plot pairs

svg(file = "Outputs/03_Linear_Models_Within_Species/KRA/Pairs.svg")
pairs(model_KRA, pars = c("a", "b_W", "b_T", "sigma")) # Check for autocorrelation
dev.off()

## Plot intervals

color_scheme_set("darkgray")

svg(file = "Outputs/03_Linear_Models_Within_Species/KRA/Posterior.svg")
mcmc_intervals(post,
               pars = c("a", "b_W", "b_T", "sigma"),
               prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
  labs(x = "Posterior Predictions", y = "Parameters") +
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 10, face = "italic"),
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 10),
        axis.text.y = element_text(colour = "black", face = "plain", size = 10))
dev.off()

## Plot trace

svg(file = "Outputs/03_Linear_Models_Within_Species/KRA/Traceplot.svg")
p <- mcmc_trace(post, pars = c("a", "b_W", "b_T", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
dev.off()

#### PRM ####

model_PRM <- readRDS("Outputs/03_Linear_Models_Within_Species/PRM/M_T_W_model.rds")

#### Table Output ####

table_PRM <- precis(model_PRM, digits = 4, prob = 0.95, depth = 2)
table_PRM

## Get outputs

means <- data.frame()
for(i in 41:44){
  m <- table_PRM[i, 1]
  means <- rbind(means, m)
}

sds <- data.frame()
for(i in 41:44){
  m <- table_PRM[i, 2]
  sds <- rbind(sds, m)
}

precis_tidy_PRM <- cbind(means, sds)
var_names <- c("a", "b_W", "b_T", "sigma")
precis_tidy_PRM <- cbind(var_names, precis_tidy_PRM)
colnames(precis_tidy_PRM) <- c("var_names", "mean", "stan_dev")
precis_tidy_PRM[, 2:3] <- round(precis_tidy_PRM[,2:3], digits = 3)

precis_tidy_PRM
write.csv(precis_tidy_PRM, "Outputs/03_Linear_Models_Within_Species/PRM/M_T_W_precis.csv", row.names = F)

#### Graph Output ####

## Extract samples

post <- extract.samples(model_PRM)
post <- as.data.frame(post)

# Name columns to match vairables

colnames(post)[41:44] <- c("a", "b_W", "b_T", "sigma")

## Plot pairs

svg(file = "Outputs/03_Linear_Models_Within_Species/PRM/Pairs.svg")
pairs(model_PRM, pars = c("a", "b_W", "b_T", "sigma")) # Check for autocorrelation
dev.off()

## Plot intervals

color_scheme_set("darkgray")

svg(file = "Outputs/03_Linear_Models_Within_Species/PRM/Posterior.svg")
mcmc_intervals(post,
               pars = c("a", "b_W", "b_T", "sigma"),
               prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
  labs(x = "Posterior Predictions", y = "Parameters") +
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 10, face = "italic"),
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 10),
        axis.text.y = element_text(colour = "black", face = "plain", size = 10))
dev.off()

## Plot trace

svg(file = "Outputs/03_Linear_Models_Within_Species/PRM/Traceplot.svg")
p <- mcmc_trace(post, pars = c("a", "b_W", "b_T", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
dev.off()
