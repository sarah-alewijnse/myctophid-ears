#### Body Mass Temperature Model Outputs ####

library(tidyverse)
library(rethinking)
library(bayesplot)

options(max.print=999999)

#### Table output ####

model_M_T_W <- readRDS("Outputs/02_Linear_Models_Among_Species/01_M_Body_Mass_Temperature/M_T_W_model.rds")

## Get Precis table

table <- precis(model_M_T_W, digits = 4, prob = 0.95, depth = 2)
table

## Get outputs

means <- data.frame()
for(i in 201:211){
  m <- table[i, 1]
  means <- rbind(means, m)
}

sds <- data.frame()
for(i in 201:211){
  m <- table[i, 2]
  sds <- rbind(sds, m)
}

## Add variable names

precis_tidy <- cbind(means, sds)
var_names <- c("a", "b_W", "b_T", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "sigma_Species", "sigma")
precis_tidy <- cbind(var_names, precis_tidy)
colnames(precis_tidy) <- c("var_names", "mean", "stan_dev")

## Get calculations

adj_means <- data.frame()
for(i in 4:9){
  a_mean <- precis_tidy[1,2]
  sp_var_mean <- precis_tidy[i, 2]
  adj <- a_mean + sp_var_mean
  adj_means <- rbind(adj_means, adj)
}

precis_adj <- precis_tidy
precis_adj[c(4:9), 2] <- adj_means
precis_adj[, 2:3] <- round(precis_adj[,2:3], digits = 3)

precis_adj ### Use this for results

write.csv(precis_adj, "Outputs/02_Linear_Models_Among_Species/01_M_Body_Mass_Temperature/M_T_W_precis.csv", row.names = F)

#### Graph Output ####

## Extract samples

post <- extract.samples(model_M_T_W)
post <- as.data.frame(post)

colnames(post)[201:211] <- c("a", "b_W", "b_T", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "sigma_Species", "sigma")

## Plot pairs

svg(file = "Outputs/02_Linear_Models_Among_Species/01_M_Body_Mass_Temperature/Pairs.svg")
pairs(model_M_T_W, pars = c("a", "b_W", "b_T", "sigma", "sigma_Species"), cex.labels = 1)
dev.off()

## Plot intervals

color_scheme_set("darkgray")

svg(file = "Outputs/02_Linear_Models_Among_Species/01_M_Body_Mass_Temperature/Posterior.svg")
mcmc_intervals(post,
                                 pars = c("sigma", "sigma_Species", "a_Var_GYN", "a_Var_PRM", "a_Var_ELC", "a_Var_KRA", "a_Var_GYR",  "a_Var_ELN", "b_T", "b_W", "a"),
                                 prob = 0.5, prob_outer = 0.95) +
  labs(x = "Posterior Predictions", y = "Parameters") +
  scale_y_discrete(labels = c("sigma", expression("sigma" ["species"]), expression("a_Var" ["GYN"]), expression("a_Var" ["PRM"]), expression("a_Var" ["ELC"]), expression("a_Var" ["KRA"]), expression("a_Var" ["GYR"]), expression("a_Var" ["ELN"]), expression ("b" ["t"]), expression("b" ["W"]), "a")) +
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 15, face = "italic"),
        text = element_text(size = 15, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 15),
        axis.text.y = element_text(colour = "black", face = "plain", size = 15))
dev.off()

## Plot trace

svg(file = "Outputs/02_Linear_Models_Among_Species/01_M_Body_Mass_Temperature/Traceplot.svg")
p <- mcmc_trace(post, pars = c("a", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "b_W", "b_T",  "sigma_Species", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot
dev.off()

