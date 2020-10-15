#### Body Mass Model Outputs ####

library(tidyverse)
library(rethinking)
library(bayesplot)

options(max.print=999999)

#### Table Output ####

model_M_W <- readRDS("Outputs/02_Linear_Models_Among_Species/02_M_Body_Mass/M_W_model.rds")

## Get Precis table

table <- precis(model_M_W, digits = 4, prob = 0.95, depth = 2)
table

## Get outputs

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

## Add variable names

precis_tidy <- cbind(means, sds)
var_names <- c("a", "b_W", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "sigma_Species", "sigma")
precis_tidy <- cbind(var_names, precis_tidy)
colnames(precis_tidy) <- c("var_names", "mean", "stan_dev")

## Get calculations

adj_means <- data.frame()
for(i in 3:8){
  a_mean <- precis_tidy[1,2]
  sp_var_mean <- precis_tidy[i, 2]
  adj <- a_mean + sp_var_mean
  adj_means <- rbind(adj_means, adj)
}

precis_adj <- precis_tidy
precis_adj[c(3:8), 2] <- adj_means
precis_adj[, 2:3] <- round(precis_adj[,2:3], digits = 3)

precis_adj ### Use this for results

#### Graph Output ####

## Extract samples

post <- extract.samples(model_M_W)
post <- as.data.frame(post)

colnames(post)[109:118] <- c("a", "b_W", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "sigma_Species", "sigma")

## Plot pairs

svg(file = "Outputs/02_Linear_Models_Among_Species/02_M_Body_Mass/Pairs.svg")
pairs(model_M_W, pars = c("a", "b_W", "sigma", "sigma_Species"))
dev.off()

## Plot intervals

color_scheme_set("darkgray")

svg(file = "Outputs/02_Linear_Models_Among_Species/02_M_Body_Mass/Posterior.svg")
mcmc_intervals(post,
               pars = c("a", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "b_W", "sigma_Species", "sigma"),
               prob = 0.5, prob_outer = 0.95) +
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

svg(file = "Outputs/02_Linear_Models_Among_Species/02_M_Body_Mass/Traceplot.svg")
p <- mcmc_trace(post, pars = c("a", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "b_W",  "sigma_Species", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot
dev.off()
