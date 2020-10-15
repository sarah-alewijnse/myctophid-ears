#### Belcher Model Outputs ####

library(tidyverse)
library(rethinking)
library(bayesplot)

options(max.print=999999)

#### Table Output ####

model_M_Metabol <- readRDS("Outputs/02_Linear_Models_Among_Species/02_M_Oxygen_Consumption/M_Belcher_model.rds")

## Get Precis table

table <- precis(model_M_Metabol, digits = 4, prob = 0.95, depth = 2)
table

## Get outputs

means <- data.frame()
for(i in 201:203){
  m <- table[i, 1]
  means <- rbind(means, m)
}

sds <- data.frame()
for(i in 201:203){
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

#### Graph Output ####

## Extract samples

post <- extract.samples(model_M_Metabol)
post <- as.data.frame(post)

colnames(post)[201:203] <- c("a", "b", "sigma")

## Plot pairs

svg(file = "Outputs/02_Linear_Models_Among_Species/02_M_Oxygen_Consumption/Pairs.svg")
pairs(model_M_Metabol, pars = c("a", "b", "sigma"))
dev.off()

## Plot intervals

color_scheme_set("darkgray")

svg(file = "Outputs/02_Linear_Models_Among_Species/02_M_Oxygen_Consumption/Posterior.svg", width = 7, height = 4)
mcmc_intervals(post,
               pars = c("a", "b", "sigma"),
               prob = 0.5, prob_outer = 0.95) +
  labs(x = "Posterior Predictions", y = "Parameters") +
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 15, face = "italic"),
        text = element_text(size = 15, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 15),
        axis.text.y = element_text(colour = "black", face = "plain", size = 15))  
dev.off()

## Traceplot

svg(file = "Outputs/02_Linear_Models_Among_Species/02_M_Oxygen_Consumption/Traceplot.svg")
p <- mcmc_trace(post, pars = c("a", "b", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot  
dev.off()