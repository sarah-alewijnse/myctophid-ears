#### Sensitivity Graphs ####

library(tidyverse)
library(gridExtra)

setwd("~/PhD/GitHub/mytophid-ears/MixSIAR/Outputs/M/Sensitivity")

#### DIC ####

base <- read.csv("Baseline.csv")
base$DIC <- "Baseline"

DIC_0 <- read.csv("DIC_0.csv")
DIC_0$DIC <- 0

DIC_min <- read.csv("DIC_Min.csv")
DIC_min$DIC <- 1.46

DIC_mean <- read.csv("DIC_Mean.csv")
DIC_mean$DIC <- 1.56

DIC_max <- read.csv("DIC_Max.csv")
DIC_max$DIC <- 1.79

DIC_3 <- read.csv("DIC_3.csv")
DIC_3$DIC <- 3

full <- rbind(base, DIC_0, DIC_min, DIC_mean, DIC_max, DIC_3)

# Load data

DIC <- ggplot(full, aes(x = DIC, y = M_vals))+
  geom_violin(fill = "black") +
  xlab(expression(paste(delta^{13}, "C"["DIC"], "(\u2030)"))) +
  ylab(expression(paste(italic("M"), " " , "Value"))) +
  theme(panel.background = element_blank(),
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.title = element_text(),
        legend.key = element_blank(),
        legend.text = element_text(face = "italic"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))

#### Diet ####

colnames(base) <- c("M_vals", "Diet")

Diet_min <- read.csv("Diet_Min.csv")
Diet_min$Diet <- -29.55

Diet_mean <- read.csv("Diet_Mean.csv")
Diet_mean$Diet <- -27.00

Diet_max <- read.csv("Diet_Max.csv")
Diet_max$Diet <- -23.76

full <- rbind(base, Diet_min, Diet_mean, Diet_max)

# Load data

Diet <- ggplot(full, aes(x = Diet, y = M_vals))+
  geom_violin(fill = "black") +
  xlab(expression(paste(delta^{13}, "C"["Diet"], "(\u2030)"))) +
  ylab(expression(paste(italic("M"), " " , "Value"))) +
  theme(panel.background = element_blank(),
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.title = element_text(),
        legend.key = element_blank(),
        legend.text = element_text(face = "italic"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))

#### e-Term ####

colnames(base) <- c("M_vals", "e")

e_Term_Min <- read.csv("e_Term_Min.csv")
e_Term_Min$e <- -1.8

e_Term_Max <- read.csv("e_Term_Max.csv")
e_Term_Max$e <- 2.7

full <- rbind(base, e_Term_Min, e_Term_Max)

# Load data

e <- ggplot(full, aes(x = e, y = M_vals))+
  geom_violin(fill = "black") +
  xlab(expression(paste(epsilon["total"]))) +
  ylab(expression(paste(italic("M"), " " , "Value"))) +
  theme(panel.background = element_blank(),
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.title = element_text(),
        legend.key = element_blank(),
        legend.text = element_text(face = "italic"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))

DIC
Diet
e

grid.arrange(DIC, Diet, e, nrow = 1)
