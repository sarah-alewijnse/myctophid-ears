#### d18O vs. d13C - Sherwood & Rose ####

library(tidyverse)
library(rethinking)

d <- read.csv("Data/My_Data_and_SR.csv")
d <- select(d, -Troph)
d <- na.omit(d)
d_other <- filter(d, Family != "Myctophidae")
d_myct <- filter(d, Family == "Myctophidae")
d_sr <- filter(d, Paper == "Sherwood_Rose_2003")
d_ours <- filter(d, Paper == "My_data")

## Intial plot

plot(d$d18O, d$d13C)

## Test for normality

# d18O

hist(d$d18O)

avg <- mean(d$d18O, na.rm = TRUE)
sd <- sd(d$d18O, na.rm = TRUE)

ks.test(d$d18O, "pnorm", avg, sd)

# Normal distribution OK

# d13C

hist(d$d13C)

avg <- mean(d$d13C, na.rm = TRUE)
sd <- sd(d$d13C, na.rm = TRUE)

ks.test(d$d13C, "pnorm", avg, sd)

# Normal distribution ok

## Check for homogeneity of variances with F-test

var <- read.csv("Data/Extra/13C_18O_SR_var.csv")

var.test(Value ~ Isotope, data = var)

# Variances not equal

## Proceed with Spearman's Rank test

cor.test(~ d13C + d18O,
         data = d,
         method = "spearman",
         cof.level = 0.95)

# Significant negative correlation

## Bayesian analysis - without n

lm(d13C ~ d18O, data = d)

model <- map(
  alist(
    d13C ~ dnorm(mu, sigma),
    mu <- a + b * d18O,
    a ~ dnorm(-4.4, 10),
    b ~ dnorm(0.5, 1),
    sigma ~ dunif(0, 10)
  ),
  data = d)

# Repeat for every mu value

d18O.seq <- seq(from = min(d$d18O), to = max(d$d18O), by = 0.1) # Horizontal axis
mu <- link(model, data = data.frame(d18O = d18O.seq)) # Link model to mu

# Summarise the distribution of mu

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.95)

par(mar = c(5, 5, 3, 3))
plot(d13C ~ d18O, data = d_sr, pch = 17, col = col.alpha("#0072B2", 0.5), cex = 2, xlim = c(min(d$d18O), max(d$d18O)), 
     xlab = expression(delta^{18}*"O (\u2030)"),
     ylab = expression(delta^{13}*"C (\u2030)"))
points(d13C ~ d18O, data = d_myct, pch = 16, col = col.alpha("#D55E00", 0.7), cex = 2)
lines(d18O.seq, mu.mean)
shade(mu.HPDI, d18O.seq)
text(d_myct$d13C ~ d_myct$d18O, labels = d_myct$sciname, pos = 4, font = 3, cex = 0.7)
