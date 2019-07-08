#### M with Weight ####

library(tidyverse)

myct <- read.csv("Outputs/M_Values.csv")
myct_tidy <- dplyr::select(myct, mean_M, Weight.x)
myct_tidy <- na.omit(myct_tidy)

## Initial plot - weight vs d13C

plot(myct_tidy$Weight.x, myct_tidy$mean_M)

# Log transform weight data

myct_tidy$log10_Weight <- log10(myct_tidy$Weight.x)

plot(myct_tidy$log10_Weight, myct_tidy$mean_M)

## Check for normal distributon with K-S test

# M

hist(myct_tidy$mean_M)

avg <- mean(myct_tidy$mean_M, na.rm = TRUE)
sd <- sd(myct_tidy$mean_M, na.rm = TRUE)

ks.test(myct_tidy$mean_M, "pnorm", avg, sd)

# Normality ok

# log10_Weight

hist(myct_tidy$log10_Weight)

avg <- mean(myct_tidy$log10_Weight, na.rm = TRUE)
sd <- sd(myct_tidy$log10_Weight, na.rm = TRUE)

ks.test(myct_tidy$log10_Weight, "pnorm", avg, sd)

# Normality not OK

## Proceed with Spearman's Rank test as log10_Weight is not normally distributed

cor.test(~ mean_M + log10_Weight,
         data = myct_tidy,
         method = "spearman",
         cof.level = 0.95)

# No significant relationship
