#### d13C with Weight ####

library(tidyverse)

myct <- read.csv("Data/Myctophids_Master.csv")
myct_tidy <- select(myct, d13C, Weight.x)
myct_tidy <- na.omit(myct_tidy)

## Initial plot - weight vs d13C

plot(myct_tidy$Weight.x, myct_tidy$d13C)

# Log transform weight data

myct_tidy$log10_Weight <- log10(myct_tidy$Weight.x)

plot(myct_tidy$log10_Weight, myct_tidy$d13C)

## Check for normal distributon with K-S test

# d13C

hist(myct_tidy$d13C)

avg <- mean(myct_tidy$d13C, na.rm = TRUE)
sd <- sd(myct_tidy$d13C, na.rm = TRUE)

ks.test(myct_tidy$d13C, "pnorm", avg, sd)

# Normality ok

# log10_Weight

hist(myct_tidy$log10_Weight)

avg <- mean(myct_tidy$log10_Weight, na.rm = TRUE)
sd <- sd(myct_tidy$log10_Weight, na.rm = TRUE)

ks.test(myct_tidy$log10_Weight, "pnorm", avg, sd)

# Normality not OK

## Proceed with Spearman's Rank test as log10_Weight is not normally distributed

cor.test(~ d13C + log10_Weight,
         data = myct_tidy,
         method = "spearman",
         cof.level = 0.95)

# No significant relationship
