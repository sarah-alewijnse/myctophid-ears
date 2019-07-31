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
GYN$sciname <- "Gymnoscopelus nichosi"

KRA <- M_values[680001:880000,]
colnames(KRA) <- c("M", "sciname")
KRA$sciname <- "Krefftichthys anderssoni"

PRM <- M_values[880001:1080000,]
colnames(PRM) <- c("M", "sciname")
PRM$sciname <- "Protomyctophum bolini"

# Bind

add <- rbind(ELN, ELC, GYR, GYN, KRA, PRM)

## Attempt plot

ggplot(add, aes(M))+
  geom_density()+
  facet_grid(. ~sciname)

ggplot(add, aes(x = M, colour = sciname))+
  geom_density()
