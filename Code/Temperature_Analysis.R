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

with(myct,
     plot(M ~ temp))

#### Check normality with K-S test ####

hist(myct$temp)

avg <- mean(myct$temp, na.rm = TRUE)
sd <- sd(myct$temp, na.rm = TRUE)

ks.test(myct$temp, "pnorm", avg, sd)

#### Analysis by Species ####

## Check for homogeneity of variances with Levene's test

leveneTest(mean_temp ~ sciname, data = myct)

# Not homogenous

## Proceed with Kruska-Wallis Test

mod <- kruskal.test(temp ~ sciname, data = myct)

results <- replicate(1000, {
  values_T <- data.frame()
  values_S <- data.frame()
  
  for(i in 1:nrow(myct)){
    
    # Get values
    val_T <- with(myct[i,], rnorm(1, M, M_HDI_range))
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

## Repeat 1000 Times

results <- replicate(1000, {
  values_M <- data.frame()
  values_T <- data.frame()
  
  for(i in 1:nrow(myct)){
    
    # Get values
    val_M <- with(myct[i,], rnorm(1, mean_M, sd_M))
    values_M <- rbind(values_M, val_M)
    val_T <- with(myct[i,], rnorm(1, mean_temp, sd_temp))
    values_T <- rbind(values_T, val_T)
    values <- cbind(values_M, values_T)
    colnames(values) <- c("M", "Temp")
  }
  
  # Do regression
  
  mod <- lm(M ~ Temp, data = values)
  sum_mod <- summary(mod)
  coef_mod <- sum_mod[["coefficients"]]
  intercept_mod <- coef_mod[1,1]
  scal_mod <- coef_mod[2,1]
  p_mod <- coef_mod[2,4]
  rsq_mod <- sum_mod$r.squared
  adj_rsq_mod <- sum_mod$adj.r.squared
  aic_mod <- AIC(mod)
  return(rbind(intercept_mod, scal_mod, p_mod, rsq_mod, adj_rsq_mod, aic_mod))
})

results <- as.data.frame(results)
results_t <- t(results)
results_t <- as.data.frame(results_t)
