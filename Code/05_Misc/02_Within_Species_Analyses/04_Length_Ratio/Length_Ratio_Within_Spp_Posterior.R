#### Combined Intraspecific Posterior Plot - Ratio ####

# Load required packages

library(tidyverse)
library(bayesplot)
library(gridExtra)

#### ELN ####

mod_ELN <- readRDS("Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/ELN/Length_Ratio_Cresp.rds")

# Extract samples

post <- extract.samples(mod_ELN)
post <- as.data.frame(post)

colnames(post)[20:22] <- c("a", "b_R", "sigma")

# Plot intervals

color_scheme_set("darkgray")

ELN_plot <- mcmc_intervals(post,
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

ELN_plot <- ELN_plot + labs(tag = "E")
ELN_plot

#### ELC ####

mod_ELC <- readRDS("Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/ELC/Length_Ratio_Cresp.rds")

# Extract samples

post <- extract.samples(mod_ELC)
post <- as.data.frame(post)

colnames(post)[11:13] <- c("a", "b_R", "sigma")

# Plot intervals

color_scheme_set("darkgray")

ELC_plot <- mcmc_intervals(post,
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

ELC_plot <- ELC_plot + labs(tag = "C")
ELC_plot

#### GYR ####

mod_GYR <- readRDS("Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/GYR/Length_Ratio_Cresp.rds")

# Extract samples

post <- extract.samples(mod_GYR)
post <- as.data.frame(post)

colnames(post)[21:23] <- c("a", "b_R", "sigma")

# Plot intervals

color_scheme_set("darkgray")

GYR_plot <- mcmc_intervals(post,
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

GYR_plot <- GYR_plot + labs(tag = "D")
GYR_plot

#### GYN ####

mod_GYN <- readRDS("Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/GYN/Length_Ratio_Cresp.rds")

# Extract samples

post <- extract.samples(mod_GYN)
post <- as.data.frame(post)

colnames(post)[13:15] <- c("a", "b_R", "sigma")

# Plot intervals

color_scheme_set("darkgray")

GYN_plot <- mcmc_intervals(post,
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

GYN_plot <- GYN_plot + labs(tag = "A")
GYN_plot

#### PRM ####

mod_PRM <- readRDS("Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/PRM/Length_Ratio_Cresp.rds")

# Extract samples

post <- extract.samples(mod_PRM)
post <- as.data.frame(post)

colnames(post)[18:20] <- c("a", "b_R", "sigma")

# Plot intervals

color_scheme_set("darkgray")

PRM_plot <- mcmc_intervals(post,
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

PRM_plot <- PRM_plot + labs(tag = "B")
PRM_plot

#### Full Plot ####

# Saves as SVG

svg("Outputs/04_Misc/02_Within_Spp/04_Length_Ratio/Combined_Posteriors.svg", width = 12, height = 6)
grid.arrange(GYN_plot, PRM_plot,
             ELC_plot, 
             GYR_plot, ELN_plot)
dev.off()
