#### Supplementary Analyses of HDPI ####

library(tidyverse)
library(lme4)
library(lmerTest)

myct <- read.csv("Outputs/Combined.csv")

#### Crushed vs. Milled ####

## Test within PRM

PRM <- filter(myct, Label == "PRM")

PRM_c <- aov(M ~ Otolith, data = PRM)
summary(PRM_c)

## Test for all (accounting for species)

crushed <- lmer(M ~ Otolith + (1|sciname), data = myct)
summary(crushed)

#### Station ####

station <- lmer(M ~ Station + (1|sciname), data = myct)
summary(station)
