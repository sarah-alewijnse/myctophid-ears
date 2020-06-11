#### Combined Intraspecific Posterior Plot ####

library(tidyverse)
library(bayesplot)
library(gridExtra)

#### ELN ####

model_ELN <- readRDS("Outputs/03_Linear_Models_Within_Species/ELN/M_T_W_model.rds")

## Extract samples

post_ELN <- extract.samples(model_ELN)
post_ELN <- as.data.frame(post_ELN)

# Name columns to match vairables

colnames(post_ELN)[39:42] <- c("a", "b_W", "b_T", "sigma")

## Plot intervals

color_scheme_set("darkgray")

ELN_plot <- mcmc_intervals(post_ELN,
                           pars = c("sigma", "b_T", "b_W", "a"),
                           prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
  labs(x = "Posterior Predictions", y = "Parameters") +
  scale_y_discrete(labels = c("sigma", expression("b" ["T"]), expression("b" ["W"]), "a")) +
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 15, face = "italic"),
        text = element_text(size = 15, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 15),
        axis.text.y = element_text(colour = "black", face = "plain", size = 15))

ELN_plot <- ELN_plot + labs(tag = "A")
ELN_plot

#### ELC ####

model_ELC <- readRDS("Outputs/03_Linear_Models_Within_Species/ELC/M_T_W_model.rds")

## Extract samples

post_ELC <- extract.samples(model_ELC)
post_ELC <- as.data.frame(post_ELC)

# Name columns to match vairables

colnames(post_ELC)[35:38] <- c("a", "b_W", "b_T", "sigma")

## Plot intervals

ELC_plot <- mcmc_intervals(post_ELC,
                           pars = c("sigma", "b_T", "b_W", "a"),
                           prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
  labs(x = "Posterior Predictions", y = "Parameters") +
  scale_y_discrete(labels = c("sigma", expression("b" ["T"]), expression("b" ["W"]), "a")) +
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 15, face = "italic"),
        text = element_text(size = 15, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 15),
        axis.text.y = element_text(colour = "black", face = "plain", size = 15))

ELC_plot <- ELC_plot + labs(tag = "B")
ELC_plot

#### GYR ####

model_GYR <- readRDS("Outputs/03_Linear_Models_Within_Species/GYR/M_T_W_model.rds")

## Extract samples

post_GYR <- extract.samples(model_GYR)
post_GYR <- as.data.frame(post_GYR)

# Name columns to match vairables

colnames(post_GYR)[41:44] <- c("a", "b_W", "b_T", "sigma")

## Plot intervals

color_scheme_set("darkgray")

GYR_plot <- mcmc_intervals(post_GYR,
                           pars = c("sigma", "b_T", "b_W", "a"),
                           prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
  labs(x = "Posterior Predictions", y = "Parameters") +
  scale_y_discrete(labels = c("sigma", expression("b" ["T"]), expression("b" ["W"]), "a")) +
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 15, face = "italic"),
        text = element_text(size = 15, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 15),
        axis.text.y = element_text(colour = "black", face = "plain", size = 15))

GYR_plot <- GYR_plot + labs(tag = "C")
GYR_plot

#### GYN ####

model_GYN <- readRDS("Outputs/03_Linear_Models_Within_Species/GYN/M_T_W_model.rds")

## Extract samples

post_GYN <- extract.samples(model_GYN)
post_GYN <- as.data.frame(post_GYN)

# Name columns to match vairables

colnames(post_GYN)[25:28] <- c("a", "b_W", "b_T", "sigma")

## Plot intervals

color_scheme_set("darkgray")

GYN_plot <- mcmc_intervals(post_GYN,
                           pars = c("sigma", "b_T", "b_W", "a"),
                           prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
  labs(x = "Posterior Predictions", y = "Parameters") +
  scale_y_discrete(labels = c("sigma", expression("b" ["T"]), expression("b" ["W"]), "a")) +
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 15, face = "italic"),
        text = element_text(size = 15, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 15),
        axis.text.y = element_text(colour = "black", face = "plain", size = 15))

GYN_plot <- GYN_plot + labs(tag = "D")
GYN_plot

#### KRA ####

model_KRA <- readRDS("Outputs/03_Linear_Models_Within_Species/KRA/M_T_W_model.rds")

## Extract samples

post_KRA <- extract.samples(model_KRA)
post_KRA <- as.data.frame(post_KRA)

# Name columns to match vairables

colnames(post_KRA)[41:44] <- c("a", "b_W", "b_T", "sigma")

## Plot intervals

color_scheme_set("darkgray")

KRA_plot <- mcmc_intervals(post_KRA,
                           pars = c("sigma", "b_T", "b_W", "a"),
                           prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
  labs(x = "Posterior Predictions", y = "Parameters") +
  scale_y_discrete(labels = c("sigma", expression("b" ["T"]), expression("b" ["W"]), "a")) +
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 15, face = "italic"),
        text = element_text(size = 15, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 15),
        axis.text.y = element_text(colour = "black", face = "plain", size = 15))

KRA_plot <- KRA_plot + labs(tag = "E")
KRA_plot

#### PRM ####

model_PRM <- readRDS("Outputs/03_Linear_Models_Within_Species/PRM/M_T_W_model.rds")

## Extract samples

post_PRM <- extract.samples(model_PRM)
post_PRM <- as.data.frame(post_PRM)

# Name columns to match vairables

colnames(post_PRM)[41:44] <- c("a", "b_W", "b_T", "sigma")

## Plot intervals

color_scheme_set("darkgray")

PRM_plot <- mcmc_intervals(post_PRM,
                           pars = c("sigma", "b_T", "b_W", "a"),
                           prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
  labs(x = "Posterior Predictions", y = "Parameters") +
  scale_y_discrete(labels = c("sigma", expression("b" ["T"]), expression("b" ["W"]), "a")) +
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 15, face = "italic"),
        text = element_text(size = 15, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 15),
        axis.text.y = element_text(colour = "black", face = "plain", size = 15))

PRM_plot <- PRM_plot + labs(tag = "F")
PRM_plot

#### Full Plot ####

grid.arrange(ELN_plot, ELC_plot,
             GYR_plot, GYN_plot,
             KRA_plot, PRM_plot)
