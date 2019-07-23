#### Model Comparison ####

library(tidyverse)
library(lme4)
library(lmerTest)
library(MuMIn)
library(HDInterval)

myct <- read.csv("Outputs/Combined.csv")

#### Model 1 - M ~ Temperature ####

mod_test <- lm(M ~ temp, data = myct)
plot(mod_test)

plot(M ~ temp, data = myct)

set.seed(1)
results <- replicate(1000, {
  values_M <- data.frame()
  values_T <- data.frame()
  
  for(i in 1:nrow(myct)){
    
    # Get values
    val_M <- with(myct[i,], rnorm(1, M, M_HDI_range))
    val_T <- with(myct[i,], rnorm(1, temp, temp_HDI_range))
    values_M <- rbind(values_M, val_M)
    values_T <- rbind(values_T, val_T)
    values <- cbind(values_M, values_T)
    colnames(values) <- c("M", "Temp")
  }
  
  # Do test
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

write.csv(results_t, "Outputs/M_Temp.csv")

results_t <- read.csv("Outputs/M_Temp.csv")

# Highest density for intercept

max <- which.max(density(results_t$intercept_mod)$y)
density(results_t$intercept_mod)$x[max]

hdi(results_t$intercept_mod, credMass = 0.95)

# Highest density for scaling exponent

max <- which.max(density(results_t$scal_mod)$y)
density(results_t$scal_mod)$x[max]

hdi(results_t$scal_mod, credMass = 0.95)

# Highest density for p value

max <- which.max(density(results_t$p_mod)$y)
density(results_t$p_mod)$x[max]

hdi(results_t$p_mod, credMass = 0.95)

# Highest density for r-squared

max <- which.max(density(results_t$rsq_mod)$y)
density(results_t$rsq_mod)$x[max]

hdi(results_t$rsq_mod, credMass = 0.95)

# Highest density for AIC

max <- which.max(density(results_t$aic_mod)$y)
density(results_t$aic_mod)$x[max]

hdi(results_t$aic_mod, credMass = 0.95)

#### Model 2 - M ~ Temperature + (1|Species) ####

mod_test <- lmer(M ~ temp + (1|sciname), data = myct)
mod_sum <- summary(mod_test)
coef_mod <- mod_sum[["coefficients"]]

set.seed(1)
results <- replicate(1000, {
  values_M <- data.frame()
  values_T <- data.frame()
  values_S <- data.frame()
  
  for(i in 1:nrow(myct)){
    
    # Get values
    val_M <- with(myct[i,], rnorm(1, M, M_HDI_range))
    val_T <- with(myct[i,], rnorm(1, temp, temp_HDI_range))
    values_M <- rbind(values_M, val_M)
    values_T <- rbind(values_T, val_T)
    sci <- dplyr::select(myct, sciname)
    sci <- slice(sci, i)
    values_S <- rbind(values_S, sci)
    values <- cbind(values_S, values_M, values_T)
    colnames(values) <- c("sciname", "M", "Temp")
  }
  
  # Do test
  mod <- lmer(M ~ Temp + (1|sciname), data = values)
  sum_mod <- summary(mod)
  coef_mod <- sum_mod[["coefficients"]]
  intercept_mod <- coef_mod[1,1]
  scal_mod <- coef_mod[2,1]
  p_mod <- coef_mod[2,5]
  r_sq_mod <- r.squaredGLMM(mod)
  r_sq_con <- r_sq_mod[1,2] # Conditional r-squared (whole model)
  adj_rsq_mod <- sum_mod$adj.r.squared
  aic_mod <- AIC(mod)
  return(rbind(intercept_mod, scal_mod, p_mod, r_sq_con, adj_rsq_mod, aic_mod))
})

results <- as.data.frame(results)
results_t <- t(results)
results_t <- as.data.frame(results_t)

write.csv(results_t, "Outputs/M_Temp_Rand.csv")

results_t <- read.csv("Outputs/M_Temp_Rand.csv")

# Highest density for intercept

max <- which.max(density(results_t$intercept_mod)$y)
density(results_t$intercept_mod)$x[max]

hdi(results_t$intercept_mod, credMass = 0.95)

# Highest density for scaling exponent

max <- which.max(density(results_t$scal_mod)$y)
density(results_t$scal_mod)$x[max]

hdi(results_t$scal_mod, credMass = 0.95)

# Highest density for p value

max <- which.max(density(results_t$p_mod)$y)
density(results_t$p_mod)$x[max]

hdi(results_t$p_mod, credMass = 0.95)

# Highest density for r-squared

max <- which.max(density(results_t$r_sq_con)$y)
density(results_t$r_sq_con)$x[max]

hdi(results_t$r_sq_con, credMass = 0.95)

# Highest density for AIC

max <- which.max(density(results_t$aic_mod)$y)
density(results_t$aic_mod)$x[max]

hdi(results_t$aic_mod, credMass = 0.95)

#### Model 3 - M ~ Weight ####

mod_test <- lm(M ~ log10(Weight.x), data = myct)
plot(mod_test$residuals)

plot(M ~ log10(Weight.x), data = myct)

set.seed(1)
results <- replicate(1000, {
  values_M <- data.frame()
  values_W <- data.frame()
  
  for(i in 1:nrow(myct)){
    
    # Get values
    val_M <- with(myct[i,], rnorm(1, M, M_HDI_range))
    val_W <- with(myct[i,], rnorm(1, log10(Weight.x), Weight_SD))
    values_M <- rbind(values_M, val_M)
    values_W <- rbind(values_W, val_W)
    values <- cbind(values_M, values_W)
    colnames(values) <- c("M", "Weight")
  }
  
  # Do test
  mod <- lm(M ~ Weight, data = values)
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

write.csv(results_t, "Outputs/M_Weight.csv")

results_t <- read.csv("Outputs/M_Weight.csv")

# Highest density for intercept

max <- which.max(density(results_t$intercept_mod)$y)
density(results_t$intercept_mod)$x[max]

hdi(results_t$intercept_mod, credMass = 0.95)

# Highest density for scaling exponent

max <- which.max(density(results_t$scal_mod)$y)
density(results_t$scal_mod)$x[max]

hdi(results_t$scal_mod, credMass = 0.95)

# Highest density for p value

max <- which.max(density(results_t$p_mod)$y)
density(results_t$p_mod)$x[max]

hdi(results_t$p_mod, credMass = 0.95)

# Highest density for r-squared

max <- which.max(density(results_t$rsq_mod)$y)
density(results_t$rsq_mod)$x[max]

hdi(results_t$rsq_mod, credMass = 0.95)

# Highest density for AIC

max <- which.max(density(results_t$aic_mod)$y)
density(results_t$aic_mod)$x[max]

hdi(results_t$aic_mod, credMass = 0.95)

#### Model 4 - M ~ Weight + (1|Species) ####

mod_test <- lmer(M ~ log10(Weight.x) + (1|sciname), data = myct)
summary(mod_test)

set.seed(1)
results <- replicate(1000, {
  values_M <- data.frame()
  values_W <- data.frame()
  values_S <- data.frame()
  
  for(i in 1:nrow(myct)){
    
    # Get values
    val_M <- with(myct[i,], rnorm(1, M, M_HDI_range))
    val_W <- with(myct[i,], rnorm(1, log10(Weight.x), Weight_SD))
    values_M <- rbind(values_M, val_M)
    values_W <- rbind(values_W, val_W)
    sci <- dplyr::select(myct, sciname)
    sci <- slice(sci, i)
    values_S <- rbind(values_S, sci)
    values <- cbind(values_S, values_M, values_W)
    colnames(values) <- c("sciname", "M", "Weight")
  }
  
  # Do test
  mod <- lmer(M ~ Weight + (1|sciname), data = values)
  sum_mod <- summary(mod)
  coef_mod <- sum_mod[["coefficients"]]
  intercept_mod <- coef_mod[1,1]
  scal_mod <- coef_mod[2,1]
  p_mod <- coef_mod[2,5]
  r_sq_mod <- r.squaredGLMM(mod)
  r_sq_con <- r_sq_mod[1,2] # Conditional r-squared (whole model)
  adj_rsq_mod <- sum_mod$adj.r.squared
  aic_mod <- AIC(mod)
  return(rbind(intercept_mod, scal_mod, p_mod, r_sq_con, adj_rsq_mod, aic_mod))
})

results <- as.data.frame(results)
results_t <- t(results)
results_t <- as.data.frame(results_t)

write.csv(results_t, "Outputs/M_Weight_Rand.csv")

results_t <- read.csv("Outputs/M_Weight_Rand.csv")

# Highest density for intercept

max <- which.max(density(results_t$intercept_mod)$y)
density(results_t$intercept_mod)$x[max]

hdi(results_t$intercept_mod, credMass = 0.95)

# Highest density for scaling exponent

max <- which.max(density(results_t$scal_mod)$y)
density(results_t$scal_mod)$x[max]

hdi(results_t$scal_mod, credMass = 0.95)

# Highest density for p value

max <- which.max(density(results_t$p_mod)$y)
density(results_t$p_mod)$x[max]

hdi(results_t$p_mod, credMass = 0.95)

# Highest density for r-squared

max <- which.max(density(results_t$r_sq_con)$y)
density(results_t$r_sq_con)$x[max]

hdi(results_t$r_sq_con, credMass = 0.95)

# Highest density for AIC

max <- which.max(density(results_t$aic_mod)$y)
density(results_t$aic_mod)$x[max]

hdi(results_t$aic_mod, credMass = 0.95)


