#### M with Weight ####

library(tidyverse)
library(HDInterval)

myct <- read.csv("Outputs/Combined.csv")
myct_tidy <- dplyr::select(myct, sciname, M, M_HDI_range, Weight.x)
myct_tidy <- na.omit(myct_tidy)

## Initial plot - weight vs d13C

plot(myct_tidy$Weight.x, myct_tidy$M)

# Log transform weight data

myct_tidy$log10_Weight <- log10(myct_tidy$Weight.x)

plot(myct_tidy$log10_Weight, myct_tidy$M)

## Check for normal distributon with K-S test

# M

hist(myct_tidy$M)

avg <- mean(myct_tidy$M, na.rm = TRUE)
sd <- sd(myct_tidy$M, na.rm = TRUE)

ks.test(myct_tidy$M, "pnorm", avg, sd)

# Normality ok

# log10_Weight

hist(myct_tidy$log10_Weight)

avg <- mean(myct_tidy$log10_Weight, na.rm = TRUE)
sd <- sd(myct_tidy$log10_Weight, na.rm = TRUE)

ks.test(myct_tidy$log10_Weight, "pnorm", avg, sd)

# Normality not OK

## Proceed with Spearman's Rank test as log10_Weight is not normally distributed

plot(myct_tidy$M ~ myct_tidy$log10_Weight)

mod <- cor.test(~ M + log10_Weight,
         data = myct_tidy,
         method = "spearman",
         cof.level = 0.95)

results <- replicate(1000, {
  values_T <- data.frame()
  values_S <- data.frame()
  
  for(i in 1:nrow(myct_tidy)){
    
    # Get values
    val_T <- with(myct_tidy[i,], rnorm(1, M, M_HDI_range))
    sci <- dplyr::select(myct_tidy, log10_Weight)
    sci <- slice(sci, i)
    values_T <- rbind(values_T, val_T)
    values_S <- rbind(values_S, sci)
    values <- cbind(values_S, values_T)
    colnames(values) <- c("log10_Weight", "M")
  }
  
  # Do test
  
  mod <- cor.test(~ values$M + values$log10_Weight,
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

## Save outputs

write.csv(results_t, "Outputs/Spearman_M_Weight.csv", row.names = F)

## Get densities of results

results_t <- read.csv("Outputs/Spearman_M_Weight.csv")

# Highest density for rho

max <- which.max(density(results_t$rho)$y)
density(results_t$rho)$x[max]

hdi(results_t$rho, credMass = 0.95)

# Highest density for p

max <- which.max(density(results_t$p_value)$y)
density(results_t$p_value)$x[max]

hdi(results_t$p_value, credMass = 0.95)

# No significant relationship

## Bayesian analysis - without n

d <- myct_tidy

model <- map(
  alist(
    M ~ dnorm(mu, sigma),
    mu <- a + b * log10_Weight,
    a ~ dnorm(-0.5, 10),
    b ~ dnorm(0.75, 1),
    sigma ~ dunif(0, 10)
  ),
  data = d)

# Precis output with correlation matrix

precis(model, corr = TRUE)

plot(M ~ log10_Weight, data = d)

# Repeat for every mu value

log10_Weight.seq <- seq(from = min(d$log10_Weight), to = max(d$log10_Weight), by = 0.01) # Horizontal axis
mu <- link(model, data = data.frame(log10_Weight = log10_Weight.seq)) # Link model to mu

# Summarise the distribution of mu

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.95)

# Plot with model and raw data

plot(M ~ log10_Weight, data = d, pch = 16, col = col.alpha("red", 0.7), cex = 1.5)
lines(log10_Weight.seq, mu.mean)
shade(mu.HPDI, log10_Weight.seq)
