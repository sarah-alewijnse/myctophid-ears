#### Crush vs. Mill Combined Posteriors ####

library(tidyverse)
library(rethinking) # Used to interface with rstan
library(bayesplot) # Gives nice plots
library(gridExtra)

# Print out all results

options(max.print=999999)

#### PRM Only ####

## Cresp

model_PRM_Cresp <- readRDS("Outputs/04_Misc/01_Methods/C_resp/PRM_Crush_model.rds")

# Extract samples

post <- extract.samples(model_PRM_Cresp)
post <- as.data.frame(post)

colnames(post)[21:23] <- c("a", "b_C", "sigma")

color_scheme_set("darkgray")

PRM_Cresp <- mcmc_intervals(post,
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

PRM_Cresp <- PRM_Cresp + labs(tag = "A")
PRM_Cresp

## Temperature

model_PRM_Temp <- readRDS("Outputs/04_Misc/01_Methods/Temp/PRM_Crush_model.rds")

# Extract samples

post <- extract.samples(model_PRM_Temp)
post <- as.data.frame(post)

colnames(post)[21:23] <- c("a", "b_C", "sigma")

color_scheme_set("darkgray")

PRM_Temp <- mcmc_intervals(post,
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

PRM_Temp <- PRM_Temp + labs(tag = "B")
PRM_Temp

grid.arrange(PRM_Cresp, PRM_Temp)

svg("Outputs/04_Misc/01_Methods/PRM_Posteriors.svg", width = 7, height = 6)
grid.arrange(PRM_Cresp, PRM_Temp)
dev.off()

#### All Species ####

## Cresp

model_All_Cresp <- readRDS("Outputs/04_Misc/01_Methods/C_resp/All_Crush_model.rds")

# Extract samples

# Extract samples

post <- extract.samples(model_All_Cresp)
post <- as.data.frame(post)

colnames(post)[109:118] <- c("a", "b_C", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "sigma_Species", "sigma")


color_scheme_set("darkgray")

All_Cresp <- mcmc_intervals(post,
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

All_Cresp <- All_Cresp + labs(tag = "A")
All_Cresp

## Cresp

model_All_Temp <- readRDS("Outputs/04_Misc/01_Methods/Temp/All_Crush_model.rds")

# Extract samples

# Extract samples

post <- extract.samples(model_All_Temp)
post <- as.data.frame(post)

colnames(post)[109:118] <- c("a", "b_C", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "sigma_Species", "sigma")


color_scheme_set("darkgray")

All_Temp <- mcmc_intervals(post,
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

All_Temp <- All_Temp + labs(tag = "B")
All_Temp

grid.arrange(All_Cresp, All_Temp, ncol = 2)

svg("Outputs/04_Misc/01_Methods/All_Posteriors.svg", width = 12, height = 6)
grid.arrange(All_Cresp, All_Temp, ncol = 2)
dev.off()
