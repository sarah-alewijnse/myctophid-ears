#### Differences Between Species - M ####

library(tidyverse)
library(car)
library(FSA)
library(HDInterval)

myct <- read.csv("Outputs/M_Values.csv")

## Basic data

min(myct$M)
max(myct$M)

min(myct$sd_M)
max(myct$sd_M)

## Species means

avg <- aggregate(myct, FUN = mean, by = list(myct$sciname))
sd <- aggregate(myct, FUN = sd, by = list(myct$sciname))

## Check for normal distributon with K-S test

hist(myct$M)

avg <- mean(myct$M, na.rm = TRUE)
sd <- sd(myct$M, na.rm = TRUE)

ks.test(myct$M, "pnorm", avg, sd)

# Normal distribution ok

## Check for homogeneity of variances with Levene's test

leveneTest(M ~ sciname, data = myct)

# Homogeneity of variances not ok

## Proceed with Kruska-Wallis Test

set.seed(1)
mod <- kruskal.test(M ~ sciname, data = myct)

results <- replicate(1000, {
  values_T <- data.frame()
  values_S <- data.frame()
  
  for(i in 1:nrow(myct)){
    
    # Get values
    val_T <- with(myct[i,], rtruncnorm(1, min_M, max_M, M, sd_M))
    sci <- dplyr::select(myct, sciname)
    sci <- slice(sci, i)
    values_T <- rbind(values_T, val_T)
    values_S <- rbind(values_S, sci)
    values <- cbind(values_S, values_T)
    colnames(values) <- c("sciname", "M")
  }
  
  # Do test
  
  mod <- kruskal.test(M ~ sciname, data = values)
  chi <- mod[["statistic"]]
  df <- mod[["parameter"]]
  p_value <- mod[["p.value"]]
  return(rbind(chi, df, p_value))
})

results <- as.data.frame(results)
results_t <- t(results)
results_t <- as.data.frame(results_t)

# Highest density for chi

max <- which.max(density(results_t$chi)$y)
density(results_t$chi)$x[max]

hdi(results_t$chi, credMass = 0.95)

# Highest density for p

max <- which.max(density(results_t$p_value)$y)
density(results_t$p_value)$x[max]

hdi(results_t$p_value, credMass = 0.95)

## Write into file

write.csv(results_t, "Outputs/KW_M_Species.csv")

# Significant difference

## Proceed with post-hoc analysis (Dunn-Test)

# Order groups by median

boxplot(M ~ sciname,
        data = myct)

myct$sciname = factor(myct$sciname, 
                      levels = c("Krefftichthys anderssoni",
                                 "Gymnoscopelus braueri",
                                 "Electrona antarctica",
                                 "Electrona carlsbergi",
                                 "Protomyctophum bolini",
                                 "Gymnoscopelus nicholsi"))

# Carry out test

set.seed(1)
mod <- dunnTest(M ~ sciname,
         method = "bonferroni", data = myct)

results <- replicate(1000, {
  values_T <- data.frame()
  values_S <- data.frame()
  
  for(i in 1:nrow(myct)){
    
    # Get values
    val_T <- with(myct[i,], rtruncnorm(1, min_M, max_M, M, sd_M))
    sci <- dplyr::select(myct, sciname)
    sci <- slice(sci, i)
    values_T <- rbind(values_T, val_T)
    values_S <- rbind(values_S, sci)
    values <- cbind(values_S, values_T)
    colnames(values) <- c("sciname", "M")
  }
  
  # Do test
  
  mod <- dunnTest(M ~ sciname,
                  method = "bonferroni", data = values)
  Results <- mod[["res"]]
  Comp <- Results$Comparison
  Comp <- as.character(Comp)
  Z <- Results$Z
  p_value <- Results$P.adj
  return(rbind(Comp, Z, p_value))
})

results <- as.data.frame(results)
results_t <- t(results)
results_t <- as.data.frame(results_t)

write.csv(results_t, "Outputs/Dunn_M_Species.csv")

## Get results

## ELN - ELC

ELN_ELC <- filter(results_t, Comp == "Electrona antarctica - Electrona carlsbergi")
ELN_ELC$p_value <- as.character(ELN_ELC$p_value)
ELN_ELC$p_value <- as.numeric(ELN_ELC$p_value)

# Highest density for p

max <- which.max(density(ELN_ELC$p_value)$y)
density(ELN_ELC$p_value)$x[max]

hdi(ELN_ELC$p_value, credMass = 0.95)

## ELN - GYR

ELN_GYR <- filter(results_t, Comp == "Electrona antarctica - Gymnoscopelus braueri")
ELN_GYR$p_value <- as.character(ELN_GYR$p_value)
ELN_GYR$p_value <- as.numeric(ELN_GYR$p_value)

# Highest density for p

max <- which.max(density(ELN_GYR$p_value)$y)
density(ELN_GYR$p_value)$x[max]

hdi(ELN_GYR$p_value, credMass = 0.95)

## ELN - PRM

ELN_PRM <- filter(results_t, Comp == "Electrona antarctica - Gymnoscopelus nicholsi")
ELN_PRM$p_value <- as.character(ELN_PRM$p_value)
ELN_PRM$p_value <- as.numeric(ELN_PRM$p_value)

# Highest density for p

max <- which.max(density(ELN_PRM$p_value)$y)
density(ELN_PRM$p_value)$x[max]

hdi(ELN_PRM$p_value, credMass = 0.95)

## ELN - PRM

ELN_PRM <- filter(results_t, Comp == "Electrona antarctica - Protomyctophum bolini")
ELN_PRM$p_value <- as.character(ELN_PRM$p_value)
ELN_PRM$p_value <- as.numeric(ELN_PRM$p_value)

# Highest density for p

max <- which.max(density(ELN_PRM$p_value)$y)
density(ELN_PRM$p_value)$x[max]

hdi(ELN_PRM$p_value, credMass = 0.95)

## ELN - KRA

ELN_KRA <- filter(results_t, Comp == "Electrona antarctica - Krefftichthys anderssoni")
ELN_KRA$p_value <- as.character(ELN_KRA$p_value)
ELN_KRA$p_value <- as.numeric(ELN_KRA$p_value)

# Highest density for p

max <- which.max(density(ELN_KRA$p_value)$y)
density(ELN_KRA$p_value)$x[max]

hdi(ELN_KRA$p_value, credMass = 0.95)

## ELC - GYR

ELC_GYR <- filter(results_t, Comp == "Electrona carlsbergi - Gymnoscopelus braueri")
ELC_GYR$p_value <- as.character(ELC_GYR$p_value)
ELC_GYR$p_value <- as.numeric(ELC_GYR$p_value)

# Highest density for p

max <- which.max(density(ELC_GYR$p_value)$y)
density(ELC_GYR$p_value)$x[max]

hdi(ELC_GYR$p_value, credMass = 0.95)

## ELC - GYN

ELC_GYN <- filter(results_t, Comp == "Electrona carlsbergi - Gymnoscopelus nicholsi")
ELC_GYN$p_value <- as.character(ELC_GYN$p_value)
ELC_GYN$p_value <- as.numeric(ELC_GYN$p_value)

# Highest density for p

max <- which.max(density(ELC_GYN$p_value)$y)
density(ELC_GYN$p_value)$x[max]

hdi(ELC_GYN$p_value, credMass = 0.95)

## ELC - PRM

ELC_PRM <- filter(results_t, Comp == "Electrona carlsbergi - Protomyctophum bolini")
ELC_PRM$p_value <- as.character(ELC_PRM$p_value)
ELC_PRM$p_value <- as.numeric(ELC_PRM$p_value)

# Highest density for p

max <- which.max(density(ELC_PRM$p_value)$y)
density(ELC_PRM$p_value)$x[max]

hdi(ELC_PRM$p_value, credMass = 0.95)

## ELC - KRA

ELC_KRA <- filter(results_t, Comp == "Electrona carlsbergi - Krefftichthys anderssoni")
ELC_KRA$p_value <- as.character(ELC_KRA$p_value)
ELC_KRA$p_value <- as.numeric(ELC_KRA$p_value)

# Highest density for p

max <- which.max(density(ELC_KRA$p_value)$y)
density(ELC_KRA$p_value)$x[max]

hdi(ELC_KRA$p_value, credMass = 0.95)

## PRM - KRA

PRM_KRA <- filter(results_t, Comp == "Krefftichthys anderssoni - Protomyctophum bolini")
PRM_KRA$p_value <- as.character(PRM_KRA$p_value)
PRM_KRA$p_value <- as.numeric(PRM_KRA$p_value)

# Highest density for p

max <- which.max(density(PRM_KRA$p_value)$y)
density(PRM_KRA$p_value)$x[max]

hdi(PRM_KRA$p_value, credMass = 0.95)

## GYR - GYN

GYR_GYN <- filter(results_t, Comp == "Gymnoscopelus braueri - Gymnoscopelus nicholsi")
GYR_GYN$p_value <- as.character(GYR_GYN$p_value)
GYR_GYN$p_value <- as.numeric(GYR_GYN$p_value)

# Highest density for p

max <- which.max(density(GYR_GYN$p_value)$y)
density(GYR_GYN$p_value)$x[max]

hdi(GYR_GYN$p_value, credMass = 0.95)

## GYR - PRM

GYR_PRM <- filter(results_t, Comp == "Gymnoscopelus braueri - Protomyctophum bolini")
GYR_PRM$p_value <- as.character(GYR_PRM$p_value)
GYR_PRM$p_value <- as.numeric(GYR_PRM$p_value)

# Highest density for p

max <- which.max(density(GYR_PRM$p_value)$y)
density(GYR_PRM$p_value)$x[max]

hdi(GYR_PRM$p_value, credMass = 0.95)

## GYR - KRA

GYR_KRA <- filter(results_t, Comp == "Gymnoscopelus braueri - Krefftichthys anderssoni")
GYR_KRA$p_value <- as.character(GYR_KRA$p_value)
GYR_KRA$p_value <- as.numeric(GYR_KRA$p_value)

# Highest density for p

max <- which.max(density(GYR_KRA$p_value)$y)
density(GYR_KRA$p_value)$x[max]

hdi(GYR_KRA$p_value, credMass = 0.95)

## GYN - PRM

GYN_PRM <- filter(results_t, Comp == "Gymnoscopelus nicholsi - Protomyctophum bolini")
GYN_PRM$p_value <- as.character(GYN_PRM$p_value)
GYN_PRM$p_value <- as.numeric(GYN_PRM$p_value)

# Highest density for p

max <- which.max(density(GYN_PRM$p_value)$y)
density(GYN_PRM$p_value)$x[max]

hdi(GYN_PRM$p_value, credMass = 0.95)

## GYN - KRA

GYN_KRA <- filter(results_t, Comp == "Gymnoscopelus nicholsi - Krefftichthys anderssoni")
GYN_KRA$p_value <- as.character(GYN_KRA$p_value)
GYN_KRA$p_value <- as.numeric(GYN_KRA$p_value)

# Highest density for p

max <- which.max(density(GYN_KRA$p_value)$y)
density(GYN_KRA$p_value)$x[max]

hdi(GYN_KRA$p_value, credMass = 0.95)

#### Boxplot ####

ggplot(myct, aes(sciname, M)) +
  geom_boxplot(lwd = 0.7) +
  theme(legend.position = "none", # Get rid of the legend
        text = element_text(size = 15),
        panel.background = element_blank(), # Keep the background blank
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(face = "italic", colour = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(colour = "black")) + 
  ylab("M") + # Add the y label
  xlab("Species") # Add the x label
