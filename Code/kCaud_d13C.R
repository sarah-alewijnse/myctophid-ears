#### K_caud vs. d13C ####

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

mod <- lm(d13C ~ K_caud, data = d)
plot(mod)

## Proceed with Spearman's Rank test

cor.test(~ d13C + K_caud,
         data = d,
         method = "spearman",
         cof.level = 0.95)

# Significant negative correlation

## Bayesian analysis - without n

model <- map(
  alist(
    d13C ~ dnorm(mu, sigma),
    mu <- a + b * K_caud,
    a ~ dnorm(-0.5, 10),
    b ~ dnorm(-1.2, 1),
    sigma ~ dunif(0, 10)
  ),
  data = d)

# Precis output with correlation matrix

precis(model, corr = TRUE)

plot(d13C ~ K_caud, data = d)

# Repeat for every mu value

K_caud.seq <- seq(from = min(d$K_caud), to = max(d$K_caud), by = 0.1) # Horizontal axis
mu <- link(model, data = data.frame(K_caud = K_caud.seq)) # Link model to mu

# Summarise the distribution of mu

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.95)

# Plot with model and raw data

plot(d13C ~ K_caud, data = d_sr, pch = 17, col = col.alpha("#0072B2", 0.5), cex = 2, xlim = c(0.2, 6),
     xlab = "Caudal Aspect Ratio",
     ylab = expression(delta^{13}*"C (\u2030)"))
points(d13C ~ K_caud, data = d_myct, pch = 16, col = col.alpha("#D55E00", 0.7), cex = 2)
lines(K_caud.seq, mu.mean)
shade(mu.HPDI, K_caud.seq)
d_myct_lab <- d_myct
d_myct_lab[1, 4] <- d_myct_lab[1, 4] - 0.14
d_myct_lab[6, 4] <- d_myct_lab[6, 4] + 0.14
text(d_myct_lab$d13C ~ d_myct_lab$K_caud, labels = d_myct_lab$sciname, pos = 2, font = 3, cex = 0.7)

