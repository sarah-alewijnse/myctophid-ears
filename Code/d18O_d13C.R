#### d13C with d18O ####

library(tidyverse)

myct <- read.csv("Data/Myctophids_Master.csv")
myct_tidy <- select(myct, d13C, d18O)
myct_tidy <- na.omit(myct_tidy)

## Initial plot - d18O vs d13C

plot(myct_tidy$d18O, myct_tidy$d13C)

## Check for normal distributon with K-S test

# d13C

hist(myct_tidy$d13C)

avg <- mean(myct_tidy$d13C, na.rm = TRUE)
sd <- sd(myct_tidy$d13C, na.rm = TRUE)

ks.test(myct_tidy$d13C, "pnorm", avg, sd)

# Normality ok

# d18O

hist(myct_tidy$d18O)

avg <- mean(myct_tidy$d18O, na.rm = TRUE)
sd <- sd(myct_tidy$d18O, na.rm = TRUE)

ks.test(myct_tidy$d18O, "pnorm", avg, sd)

# Normality OK

## Check for homogeneity of variances with F-test

var <- read.csv("Data/Extra/13C_18O_var.csv")

var.test(Value ~ Isotope, data = var)

# Homogeneity of variances not ok

## Proceed with Spearman's Rank test

cor.test(~ d13C + d18O,
         data = myct_tidy,
         method = "spearman",
         cof.level = 0.95)

# Significant negative correlation