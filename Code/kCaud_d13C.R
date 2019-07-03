#### K_caud vs. d13C ####

library(tidyverse)

d <- read.csv("Data/Sherwood_Rose_Myct.csv")
d <- select(d, -Troph)
d <- na.omit(d)
d_other <- filter(d, Family != "Myctophidae")
d_myct <- filter(d, Family == "Myctophidae")

## Intial plot

plot(d$K_caud, d$d13C)

## Test for normality

# K_caud

hist(d$K_caud)

avg <- mean(d$K_caud, na.rm = TRUE)
sd <- sd(d$K_caud, na.rm = TRUE)

ks.test(d$K_caud, "pnorm", avg, sd)

# Normal distribution OK

# d13C

hist(d$d13C)

avg <- mean(d$d13C, na.rm = TRUE)
sd <- sd(d$d13C, na.rm = TRUE)

ks.test(d$d13C, "pnorm", avg, sd)

# Normal distribution ok

## Check for homogeneity of variances with F-test

var <- read.csv("Data/Extra/13C_K_caud_var.csv")

var.test(Value ~ Factor, data = var)

# Variances not equal

## Proceed with Spearman's Rank test

cor.test(~ d13C + K_caud,
         data = d,
         method = "spearman",
         cof.level = 0.95)

# Significant negative correlation