#### Combined Intraspecific Posterior Plot - Within Years ####

# Figure 5

# Load required packages

library(tidyverse)
library(bayesplot)
library(gridExtra)

#### ELN ####

model_ELN <- readRDS("Outputs/04_Misc/05_Year/ELN_Year_model.rds")

## Extract samples

post_ELN <- extract.samples(model_ELN)
post_ELN <- as.data.frame(post_ELN)

# Name columns to match vairables

colnames(post_ELN)[20:22] <- c("a", "b_Y", "sigma")

## Plot intervals

color_scheme_set("darkgray")

ELN_plot <- mcmc_intervals(post_ELN,
                           pars = c("sigma", "b_Y", "a"),
                           prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
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

ELN_plot <- ELN_plot + labs(tag = "F")
ELN_plot

#### ELC ####

model_ELC <- readRDS("Outputs/04_Misc/05_Year/ELC_Year_model.rds")

## Extract samples

post_ELC <- extract.samples(model_ELC)
post_ELC <- as.data.frame(post_ELC)

# Name columns to match vairables

colnames(post_ELC)[18:20] <- c("a", "b_Y", "sigma")

## Plot intervals

ELC_plot <- mcmc_intervals(post_ELC,
                           pars = c("sigma", "b_Y", "a"),
                           prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
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

ELC_plot <- ELC_plot + labs(tag = "C")
ELC_plot

#### GYR ####

model_GYR <- readRDS("Outputs/04_Misc/05_Year/GYR_Year_model.rds")

## Extract samples

post_GYR <- extract.samples(model_GYR)
post_GYR <- as.data.frame(post_GYR)

# Name columns to match vairables

colnames(post_GYR)[21:23] <- c("a", "b_Y", "sigma")

## Plot intervals

color_scheme_set("darkgray")

GYR_plot <- mcmc_intervals(post_GYR,
                           pars = c("sigma", "b_Y", "a"),
                           prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
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

GYR_plot <- GYR_plot + labs(tag = "E")
GYR_plot

#### GYN ####

model_GYN <- readRDS("Outputs/04_Misc/05_Year/GYN_Year_model.rds")

## Extract samples

post_GYN <- extract.samples(model_GYN)
post_GYN <- as.data.frame(post_GYN)

# Name columns to match vairables

colnames(post_GYN)[13:15] <- c("a", "b_Y", "sigma")

## Plot intervals

color_scheme_set("darkgray")

GYN_plot <- mcmc_intervals(post_GYN,
                           pars = c("sigma", "b_Y", "a"),
                           prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
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

GYN_plot <- GYN_plot + labs(tag = "A")
GYN_plot

#### KRA ####

model_KRA <- readRDS("Outputs/04_Misc/05_Year/KRA_Year_model.rds")

## Extract samples

post_KRA <- extract.samples(model_KRA)
post_KRA <- as.data.frame(post_KRA)

# Name columns to match vairables

colnames(post_KRA)[21:23] <- c("a", "b_Y", "sigma")

## Plot intervals

color_scheme_set("darkgray")

KRA_plot <- mcmc_intervals(post_KRA,
                           pars = c("sigma", "b_Y", "a"),
                           prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
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

KRA_plot <- KRA_plot + labs(tag = "D")
KRA_plot

#### PRM ####

model_PRM <- readRDS("Outputs/04_Misc/05_Year/PRM_Year_model.rds")

## Extract samples

post_PRM <- extract.samples(model_PRM)
post_PRM <- as.data.frame(post_PRM)

# Name columns to match vairables

colnames(post_PRM)[21:23] <- c("a", "b_Y", "sigma")

## Plot intervals

color_scheme_set("darkgray")

PRM_plot <- mcmc_intervals(post_PRM,
                           pars = c("sigma", "b_Y", "a"),
                           prob = 0.5, prob_outer = 0.95) + # Thick lines = 50% probablity, thin lines = 95% probability
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

PRM_plot <- PRM_plot + labs(tag = "B")
PRM_plot

#### Full Plot ####

# Saves as SVG

svg("Outputs/04_Misc/05_Year/Combined_Posterior.svg", width = 12, height = 6)
grid.arrange(GYN_plot, PRM_plot,
             ELC_plot, KRA_plot,
             GYR_plot, ELN_plot)
dev.off()
