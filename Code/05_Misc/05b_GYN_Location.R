#### GYN Year Model Outputs ####

library(tidyverse)
library(rethinking)
library(bayesplot)

options(max.print=999999)

#### GYN Only ####

model_GYN_loc <- readRDS("Outputs/04_Misc/06_GYN_Location/GYN_South_Ork_model.rds")

#### Table Outputs ####

## Get Precis table

table <- precis(model_GYN_loc, digits = 4, prob = 0.95, depth = 2)
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

write.csv(precis_tidy, "Outputs/04_Misc/06_GYN_Location/GYN_South_Ork_precis.csv", row.names = F)

#### Graph Outputs ####

# Extract samples

post <- extract.samples(model_GYN_loc)
post <- as.data.frame(post)

colnames(post)[13:15] <- c("a", "b_S", "sigma")

## Plot pairs

svg("Outputs/04_Misc/06_GYN_Location/GYN_Pairs.svg")
pairs(model_GYN_loc, pars = c("a", "b_S", "sigma"))
dev.off()

## Plot intervals

color_scheme_set("darkgray")

svg("Outputs/04_Misc/06_GYN_Location/GYN_Posterior.svg", width = 7, height = 3)
mcmc_intervals(post,
               pars = c("sigma", "b_S", "a"),
               prob = 0.5, prob_outer = 0.95) +
  labs(x = "Posterior Predictions", y = "Parameters") +
  scale_y_discrete(labels = c("sigma", expression("b" ["Location"]), "a")) +
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

svg("Outputs/04_Misc/06_GYN_Location/GYN_Traceplots.svg")
p <- mcmc_trace(post, pars = c("a", "b_S", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot
dev.off()
