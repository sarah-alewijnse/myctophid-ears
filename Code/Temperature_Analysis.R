#### Temperature Analysis ####

library(tidyverse)
library(car)
library(FSA)
library(HDInterval)

myct <- read.csv("Outputs/Combined.csv")

#### Initial ####

## Get min-max

min(myct$temp)
max(myct$temp)

min(myct$temp_HDI_range)
max(myct$temp_HDI_range)

## Plot 

with(myct,
     boxplot(temp ~ sciname))

# E carlsbergi temps

ELC <- filter(myct, Label == "ELC")
mean(ELC$temp)

# G nicholsi temps

GYN <- filter(myct, Label == "GYN")
mean(GYN$temp)

# G braueri temps

GYR <- filter(myct, Label == "GYR")
mean(GYR$temp)

#### Check normality with K-S test ####

hist(myct$temp)

avg <- mean(myct$temp, na.rm = TRUE)
sd <- sd(myct$temp, na.rm = TRUE)

ks.test(myct$temp, "pnorm", avg, sd)

#### Analysis by Species ####

## Check for homogeneity of variances with Levene's test

leveneTest(temp ~ sciname, data = myct)

# Not homogenous

## Proceed with Kruska-Wallis Test

mod <- kruskal.test(temp ~ sciname, data = myct)

set.seed(1)
results <- replicate(1000, {
  values_T <- data.frame()
  values_S <- data.frame()
  
  for(i in 1:nrow(myct)){
    
    # Get values
    val_T <- with(myct[i,], rnorm(1, temp, temp_HDI_range))
    sci <- dplyr::select(myct, sciname)
    sci <- slice(sci, i)
    values_T <- rbind(values_T, val_T)
    values_S <- rbind(values_S, sci)
    values <- cbind(values_S, values_T)
    colnames(values) <- c("sciname", "Temp")
  }
  
  # Do test
  
  mod <- kruskal.test(Temp ~ sciname, data = values)
  chi <- mod[["statistic"]]
  df <- mod[["parameter"]]
  p_value <- mod[["p.value"]]
  return(rbind(chi, df, p_value))
})

results <- as.data.frame(results)
results_t <- t(results)
results_t <- as.data.frame(results_t)

## Save outputs

write.csv(results_t, "Outputs/KW_Temp_Species.csv", row.names = F)

## Get densities of results

results_t <- read.csv("Outputs/KW_Temp_Species.csv")

# Highest density for chi

max <- which.max(density(results_t$chi)$y)
density(results_t$chi)$x[max]

hdi(results_t$chi, credMass = 0.95)

# Highest density for p

max <- which.max(density(results_t$p_value)$y)
density(results_t$p_value)$x[max]

hdi(results_t$p_value, credMass = 0.95)

#### Analysis with M ####

with(myct,
     plot(M ~ temp))

## Check for homogeneity of variances with F-test

var <- read.csv("Outputs/M_Temp_var.csv")

var.test(Value ~ Variable, data = var)

## Repeat 1000 Times

mod <- cor.test(~ M + temp,
                data = myct,
                method = "spearman",
                cof.level = 0.95)

set.seed(1)
  results <- replicate(1000, {
  values_T <- data.frame()
  values_S <- data.frame()
  
  for(i in 1:nrow(myct)){
    
    # Get values
    val_T <- with(myct[i,], rnorm(1, M, M_HDI_range))
    sci <- with(myct[i,], rnorm(1, temp, temp_HDI_range))
    values_T <- rbind(values_T, val_T)
    values_S <- rbind(values_S, sci)
    values <- cbind(values_S, values_T)
    colnames(values) <- c("Temperature", "M")
  }
  
  # Do test
  
  mod <- cor.test(~ values$M + values$Temperature,
                  data = myct_tidy,
                  method = "spearman",
                  cof.level = 0.95)
  rho <- mod[["estimate"]]
  p_value <- mod[["p.value"]]
  return(rbind(rho, p_value))
})

results <- as.data.frame(results)
results_t <- t(results)
results_t <- as.data.frame(results_t)

write.csv(results_t, "Outputs/Regression_M_Temp.csv")

# Highest density for rho

max <- which.max(density(results_t$rho)$y)
density(results_t$rho)$x[max]

hdi(results_t$rho, credMass = 0.95)

# Highest density for p-value

max <- which.max(density(results_t$p_value)$y)
density(results_t$p_value)$x[max]

hdi(results_t$p_value, credMass = 0.95)