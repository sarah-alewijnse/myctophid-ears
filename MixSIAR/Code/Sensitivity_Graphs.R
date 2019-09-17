#### Sensitivity Graphs ####

library(tidyverse)

#### DIC ####

base <- read.csv("Baseline.csv")
base$DIC <- "Baseline"

DIC_0 <- read.csv("DIC_0.csv")
DIC_0$DIC <- 0

DIC_min <- read.csv("DIC_Min.csv")
DIC_min$DIC <- 2.01

DIC_mean <- read.csv("DIC_mean.csv")
DIC_mean$DIC <- 2.17

DIC_max <- read.csv("DIC_Max.csv")
DIC_max$DIC <- 2.25

DIC_3 <- read.csv("DIC_3.csv")
DIC_3$DIC <- 3

full <- rbind(base, DIC_0, DIC_min, DIC_mean, DIC_max, DIC_3)

# Load data

ggplot(full, aes(x = DIC, y = M_vals))+
  geom_violin(fill = "black") +
  xlab(expression(paste(delta^{13}, "C"["DIC"], "(\u2030)"))) +
  ylab("M Value") +
  theme_light()
