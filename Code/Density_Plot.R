#### Density Plot of M - By Species ####

library(tidyverse)
library(truncnorm)

myct <- read.csv("Outputs/M_Values.csv")

## Density plot with mean M

ggplot(myct, aes(M))+
  geom_density()+
  facet_grid(. ~sciname)

## Get densities

M_values <- data.frame()

load("Functions/PseudoBayes_M_Dist.Rdata")

set.seed(1)

for(i in 1:nrow(myct)){
  ms <- with(myct[i,],
             PseudoBayes_M(d13C, 0.02, # SD based on values from isotope lab
                           Year.x, 10000,
                           DIC, 0.202, 0, 3, # From Tagliabue & Bopp, 2007
                           Suess, 0.202, -0.28, 0, # From Tagliabue & Bopp, 2007. SD same as for DIC
                           -0.07, 0.202, # From Tagliabue & Bopp, 2007. SD same as for DIC
                           Phyto, Phyto_sd, -31, -16.5, # From Magozzi et al. 2017. Based on values for Longhurst biogeographic provinces
                           -0.17, 0.202, # From Clive - per comms. SD same as for DIC
                           UseTroph, UseTrophSe, 2, 5, # From FishBase
                           0.8, 1.1, # From DeNiro & Epstein
                           0, 1.8)) # From Solomon et al. 
  M_values <- rbind(M_values, ms)
}

# Tidy for graph 

M_values <- as.data.frame(M_values)

M_values$sciname <- "sciname"

ELN <- M_values[1:190000,]
colnames(ELN) <- c("M", "sciname")
ELN$sciname <- "Electrona antarctica"

ELC <- M_values[190001:360000,]
colnames(ELC) <- c("M", "sciname")
ELC$sciname <- "Electrona carlsbergi"

GYR <- M_values[360001:560000,]
colnames(GYR) <- c("M", "sciname")
GYR$sciname <- "Gymnoscopelus braueri"

GYN <- M_values[560001:680000,]
colnames(GYN) <- c("M", "sciname")
GYN$sciname <- "Gymnoscopelus nicholsi"

KRA <- M_values[680001:880000,]
colnames(KRA) <- c("M", "sciname")
KRA$sciname <- "Krefftichthys anderssoni"

PRM <- M_values[880001:1080000,]
colnames(PRM) <- c("M", "sciname")
PRM$sciname <- "Protomyctophum bolini"

# Bind

add <- rbind(ELN, ELC, GYR, GYN, KRA, PRM)

## Get densities

max <- which.max(density(ELN$M)$y)
ELN_dens <- density(ELN$M)$x[max]

max <- which.max(density(ELC$M)$y)
ELC_dens <- density(ELC$M)$x[max]

max <- which.max(density(GYR$M)$y)
GYR_dens <- density(GYR$M)$x[max]

max <- which.max(density(GYN$M)$y)
GYN_dens <- density(GYN$M)$x[max]

max <- which.max(density(KRA$M)$y)
KRA_dens <- density(KRA$M)$x[max]

max <- which.max(density(PRM$M)$y)
PRM_dens <- density(PRM$M)$x[max]

max_dens <- data.frame(M = c(ELN_dens, ELC_dens, GYR_dens, GYN_dens, KRA_dens, PRM_dens),
                       sciname = c("Electrona antarctica", "Electrona carlsbergi", "Gymnoscopelus braueri", "Gymnoscopelus nicholsi", "Krefftichthys anderssoni", "Protomyctophum bolini"))


## Attempt plot

cbp1 <- c("#56B4E9", "#0072B2", "#E69F00", "#D55E00", "#009E73", "#CC79A7")

ggplot(add, aes(x = M))+
  geom_density()+
  facet_grid(.~sciname)

ggplot(add, aes(x = M, fill = sciname))+
  geom_density(alpha = 0.5, lwd = 1, colour = NA)+
  geom_vline(data = max_dens, aes(xintercept = M, colour = sciname), lwd = 2)+
  scale_fill_manual(name = "Species", values = cbp1) +
  scale_colour_manual(name = "Species", values = cbp1) +
  scale_x_continuous(breaks = round(seq(min(add$M), max(add$M), by = 0.05),1), limits = c(0.1, 0.5)) +
  scale_y_continuous(limits = c(0, 25), expand = c(0, 0))+
  ylab("Density")+
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.title = element_text(),
        legend.key=element_blank(),
        legend.text = element_text(face = "italic"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))

