#### Bayesian Linear Models - Weight ####

library(tidyverse)
library(rethinking)

myct <- read.csv("Outputs/Combined.csv")
glimpse(myct)

#### Overall Model with Weight ####

myct_tidy <- filter(myct, Weight_SD == "0")
myct_tidy$ln_Weight <- log(myct_tidy$Weight.x)

M_W_list <- list(
  M_obs = myct_tidy$M,
  M_sd = myct_tidy$sd_M,
  Weight = myct_tidy$ln_Weight,
  Species = myct_tidy$sciname
)

model_M_W <- map2stan(
  alist(
    M_est ~ dnorm(mu, sigma),
    mu <- a + b*Weight + a_Var[Species],
    M_obs ~ dnorm(M_est, M_sd),
    a ~ dnorm(0.25, 1),
    b ~ dnorm(0, 1),
    a_Var[Species] ~ dnorm(0 , sigma_Species),
    sigma_Species ~ dunif(0, 1),
    sigma ~ dunif(0, 1)
  ),
data = M_W_list,
start = list(M_est = M_W_list$M_obs),
WAIC = FALSE,
iter = 10000,
warmup = 1000,
chains = 4,
cores = 4,
control = list(adapt_delta = 0.99))

## Precis Tables

precis(model_M_W)
precis(model_M_W, depth = 2, digits = 4)

## Check chains

plot(model_M_W)

## Plot

weight_seq <- seq(from = min(M_W_list$Weight), to = max(M_W_list$Weight), by = 0.01) # Horizontal axis
mu <- link(model_M_W, data = data.frame(Weight = weight_seq)) # Link model to mu

# Summarise the distribution of mu

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.95)

# Plot with model and raw data

par(mfrow = c(1,1))
plot(M_obs ~ Weight, data = M_W_list, pch = 16, col = Species, cex = 1.5)
lines(weight_seq, mu.mean)
shade(mu.HPDI, weight_seq)

#### Model with Species ####

## ELN

ELN <- filter(myct_tidy, Label == "ELN")

ELN_M_W_list <- list(
  M_obs = ELN$M,
  M_sd = ELN$sd_M,
  Weight = ELN$ln_Weight
)

ELN_M_W <- map2stan(
  alist(
    M_est ~ dnorm(mu, sigma),
    mu <- a + b*Weight,
    M_obs ~ dnorm(M_est, M_sd),
    a ~ dnorm(0.25, 1),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = ELN_M_W_list,
  start = list(M_est = ELN_M_W_list$M_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99))

## Precis Tables

precis(ELN_M_W, digits = 4)

post_ELN <- extract.samples(ELN_M_W)
write.csv(post_ELN, "Outputs/Posterior_ELN_Weight.csv")

## Check chains

plot(ELN_M_W)

## Plot

weight_seq <- seq(from = min(ELN_M_W_list$Weight), to = max(ELN_M_W_list$Weight), by = 0.001) # Horizontal axis
mu <- link(ELN_M_W, data = data.frame(Weight = weight_seq)) # Link model to mu

# Summarise the distribution of mu

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.95)

# Plot with model and raw data

par(mfrow = c(1,1))
par(mar=c(5,5,3,3))
plot(M_obs ~ Weight, data = ELN_M_W_list, pch = 16, cex = 2, ylim = c(0.17, 0.36), xlab = "", ylab = "", tck = -0.01, cex.axis = 1.5)
mtext(text = "Ln(Body Mass) (g)", side = 1, line = 3, cex = 1.5)
mtext(text = "M Value", side = 2, line = 3, cex = 1.5)
with(ELN_M_W_list, arrows(Weight, M_obs - M_sd, Weight, M_obs + M_sd, length = 0.05, angle = 90 , code = 3))
lines(weight_seq, mu.mean, lwd = 3)
shade(mu.HPDI, weight_seq)

## ELC

ELC <- filter(myct_tidy, Label == "ELC")

ELC_M_W_list <- list(
  M_obs = ELC$M,
  M_sd = ELC$sd_M,
  Weight = ELC$ln_Weight
)

ELC_M_W <- map2stan(
  alist(
    M_est ~ dnorm(mu, sigma),
    mu <- a + b*Weight,
    M_obs ~ dnorm(M_est, M_sd),
    a ~ dnorm(0.25, 1),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = ELC_M_W_list,
  start = list(M_est = ELC_M_W_list$M_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99))

## Precis Tables

precis(ELC_M_W, digits = 4)

post_ELC <- extract.samples(ELC_M_W)
write.csv(post_ELC, "Outputs/Posterior_ELC_Weight.csv")

## Check chains

plot(ELC_M_W)

# Plot with model and raw data

## Plot

weight_seq <- seq(from = min(ELC_M_W_list$Weight), to = max(ELC_M_W_list$Weight), by = 0.001) # Horizontal axis
mu <- link(ELC_M_W, data = data.frame(Weight = weight_seq)) # Link model to mu

# Summarise the distribution of mu

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.95)

par(mfrow = c(1,1))
par(mar=c(5,5,3,3))
plot(M_obs ~ Weight, data = ELC_M_W_list, pch = 16, cex = 2, xlab = "", ylab = "", ylim = c(0.15, 0.25), tck = -0.01, cex.axis = 1.5)
mtext(text = "Ln(Body Mass) (g)", side = 1, line = 3, cex = 1.5)
mtext(text = "M Value", side = 2, line = 3, cex = 1.5)
with(ELC_M_W_list, arrows(Weight, M_obs - M_sd, Weight, M_obs + M_sd, length = 0.05, angle = 90 , code = 3))
lines(weight_seq, mu.mean, lwd = 3)
shade(mu.HPDI, weight_seq)

## GYR

GYR <- filter(myct_tidy, Label == "GYR")

GYR_M_W_list <- list(
  M_obs = GYR$M,
  M_sd = GYR$sd_M,
  Weight = GYR$ln_Weight
)

GYR_M_W <- map2stan(
  alist(
    M_est ~ dnorm(mu, sigma),
    mu <- a + b*Weight,
    M_obs ~ dnorm(M_est, M_sd),
    a ~ dnorm(0.25, 1),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = GYR_M_W_list,
  start = list(M_est = GYR_M_W_list$M_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99))

## Precis Tables

precis(GYR_M_W, digits = 4)

post_GYR <- extract.samples(GYR_M_W)
write.csv(post_GYR, "Outputs/Posterior_GYR_W.csv")

## Check chains

plot(GYR_M_W)

## Plot

weight_seq <- seq(from = min(GYR_M_W_list$Weight), to = max(GYR_M_W_list$Weight), by = 0.001) # Horizontal axis
mu <- link(GYR_M_W, data = data.frame(Weight = weight_seq)) # Link model to mu

# Summarise the distribution of mu

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.95)

# Plot with model and raw data

par(mfrow = c(1,1))
par(mar=c(5,5,3,3))
plot(M_obs ~ Weight, data = GYR_M_W_list, pch = 16, cex = 2, xlab = "", ylab = "", ylim = c(0.17, 0.35), tck = -0.01, cex.axis = 1.5)
mtext(text = "ln(Body mass) (g)", side = 1, line = 3, cex = 1.5)
mtext(text = "M Value", side = 2, line = 3, cex = 1.5)
with(GYR_M_W_list, arrows(Weight, M_obs - M_sd, Weight, M_obs + M_sd, length = 0.05, angle = 90 , code = 3))
lines(weight_seq, mu.mean, lwd = 3)
shade(mu.HPDI, weight_seq)

## GYN

GYN <- filter(myct_tidy, Label == "GYN")

GYN_M_W_list <- list(
  M_obs = GYN$M,
  M_sd = GYN$sd_M,
  Weight = GYN$ln_Weight
)

GYN_M_W <- map2stan(
  alist(
    M_est ~ dnorm(mu, sigma),
    mu <- a + b*Weight,
    M_obs ~ dnorm(M_est, M_sd),
    a ~ dnorm(0.25, 1),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = GYN_M_W_list,
  start = list(M_est = GYN_M_W_list$M_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99))

## Precis Tables

precis(GYN_M_W, digits = 4)

post_GYN <- extract.samples(GYN_M_W)
write.csv(post_GYN, "Outputs/Posterior_GYN_W.csv")

## Check chains

plot(GYN_M_W)

## Plot

weight_seq <- seq(from = min(GYN_M_W_list$Weight), to = max(GYN_M_W_list$Weight), by = 0.001) # Horizontal axis
mu <- link(GYN_M_W, data = data.frame(Weight = weight_seq)) # Link model to mu

# Summarise the distribution of mu

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.95)

# Plot with model and raw data

par(mfrow = c(1,1))
par(mar=c(5,5,3,3))
plot(M_obs ~ Weight, data = GYN_M_W_list, pch = 16, cex = 2, xlab = "", ylab = "", ylim = c(0.12, 0.25), tck = -0.01, cex.axis = 1.5)
mtext(text = "Ln(Body Mass) (g)", side = 1, line = 3, cex = 1.5)
mtext(text = "M Value", side = 2, line = 3, cex = 1.5)
with(GYN_M_W_list, arrows(Weight, M_obs - M_sd, Weight, M_obs + M_sd, length = 0.05, angle = 90 , code = 3))
lines(weight_seq, mu.mean, lwd = 3)
shade(mu.HPDI, weight_seq)

## KRA

KRA <- filter(myct_tidy, Label == "KRA")

KRA_M_W_list <- list(
  M_obs = KRA$M,
  M_sd = KRA$sd_M,
  Weight = KRA$ln_Weight
)

KRA_M_W <- map2stan(
  alist(
    M_est ~ dnorm(mu, sigma),
    mu <- a + b*Weight,
    M_obs ~ dnorm(M_est, M_sd),
    a ~ dnorm(0.25, 1),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = KRA_M_W_list,
  start = list(M_est = KRA_M_W_list$M_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99))

## Precis Tables

precis(KRA_M_W, digits = 4)

post_KRA <- extract.samples(KRA_M_W)
write.csv(post_KRA, "Outputs/Posterior_KRA_W.csv")

## Check chains

plot(KRA_M_W)

# Plot with model and raw data

## Plot

weight_seq <- seq(from = min(KRA_M_W_list$Weight), to = max(KRA_M_W_list$Weight), by = 0.001) # Horizontal axis
mu <- link(KRA_M_W, data = data.frame(Weight = weight_seq)) # Link model to mu

# Summarise the distribution of mu

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.95)

# Plot with model and raw data

par(mfrow = c(1,1))
par(mar=c(5,5,3,3))
plot(M_obs ~ Weight, data = KRA_M_W_list, pch = 16, cex = 2, xlab = "", ylab = "", ylim = c(0.17, 0.35), tck = -0.01, cex.axis = 1.5)
mtext(text = "Ln(Body Mass) (g)", side = 1, line = 3, cex = 1.5)
mtext(text = "M Value", side = 2, line = 3, cex = 1.5)
with(KRA_M_W_list, arrows(Weight, M_obs - M_sd, Weight, M_obs + M_sd, length = 0.05, angle = 90 , code = 3, lwd = 1))
lines(weight_seq, mu.mean, lwd = 3)
shade(mu.HPDI, weight_seq)

## PRM

PRM <- filter(myct_tidy, Label == "PRM")

PRM_M_W_list <- list(
  M_obs = PRM$M,
  M_sd = PRM$sd_M,
  Weight = PRM$ln_Weight
)

PRM_M_W <- map2stan(
  alist(
    M_est ~ dnorm(mu, sigma),
    mu <- a + b*Weight,
    M_obs ~ dnorm(M_est, M_sd),
    a ~ dnorm(0.25, 1),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = PRM_M_W_list,
  start = list(M_est = PRM_M_W_list$M_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99))

## Precis Tables

precis(PRM_M_W, digits = 4)

post_PRM <- extract.samples(PRM_M_W)
write.csv(post_PRM, "Outputs/Posterior_PRM_W.csv")

## Check chains

plot(PRM_M_W)

## Plot

weight_seq <- seq(from = min(PRM_M_W_list$Weight), to = max(PRM_M_W_list$Weight), by = 0.001) # Horizontal axis
mu <- link(PRM_M_W, data = data.frame(Weight = weight_seq)) # Link model to mu

# Summarise the distribution of mu

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.95)

# Plot with model and raw data

par(mfrow = c(1,1))
par(mar=c(5,5,3,3))
plot(M_obs ~ Weight, data = PRM_M_W_list, pch = 16, cex = 2, xlab = "", ylab = "", ylim = c(0.13, 0.3), tck = -0.01, cex.axis = 1.5)
mtext(text = "ln(Body Mass) (g)", side = 1, line = 3, cex = 1.5)
mtext(text = "M Value", side = 2, line = 3, cex = 1.5)
with(PRM_M_W_list, arrows(Weight, M_obs - M_sd, Weight, M_obs + M_sd, length = 0.05, angle = 90 , code = 3))
lines(weight_seq, mu.mean, lwd = 3)
shade(mu.HPDI, weight_seq)


## Posterior plots

post_ELN <- read.csv("Outputs/Posterior_ELN_Weight.csv")
post_ELC <- read.csv("Outputs/Posterior_ELC_Weight.csv")
post_GYR <- read.csv("Outputs/Posterior_GYR_W.csv")
post_KRA <- read.csv("Outputs/Posterior_KRA_W.csv")
post_PRM <- read.csv("Outputs/Posterior_PRM_W.csv")

post_ELN <- as.data.frame(post_ELN)
ELN_plot <- mcmc_intervals(post_ELN,
               pars = c("a", "b", "sigma"),
               prob = 0.5, prob_outer = 0.9) +
  labs(title = expression(paste(italic("Electrona antarctica"))), x = "Posterior Predictions", y = "Parameters") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15, family = "sans"),
        title = element_text(size = 10),
        axis.title = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"))  # Print the minor gridlines

post_ELC <- as.data.frame(post_ELC)
ELC_plot <- mcmc_intervals(post_ELC,
                           pars = c("a", "b", "sigma"),
                           prob = 0.5, prob_outer = 0.9) +
  labs(title = expression(paste(italic("Electrona carlsbergi"))), x = "Posterior Predictions", y = "Parameters") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15, family = "sans"),
        title = element_text(size = 10),
        axis.title = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"))  # Print the minor gridlines

post_GYR <- as.data.frame(post_GYR)
GYR_plot <- mcmc_intervals(post_GYR,
                           pars = c("a", "b", "sigma"),
                           prob = 0.5, prob_outer = 0.9) +
  labs(title = expression(paste(italic("Gymnoscopelus braueri"))), x = "Posterior Predictions", y = "Parameters") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15, family = "sans"),
        title = element_text(size = 10),
        axis.title = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"))  # Print the minor gridlines

post_KRA <- as.data.frame(post_KRA)
KRA_plot <- mcmc_intervals(post_KRA,
                           pars = c("a", "b", "sigma"),
                           prob = 0.5, prob_outer = 0.9) +
  labs(title = expression(paste(italic("Krefftichthys anderssoni"))), x = "Posterior Predictions", y = "Parameters") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15, family = "sans"),
        title = element_text(size = 10),
        axis.title = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"))  # Print the minor gridlines

post_PRM <- as.data.frame(post_PRM)
PRM_plot <- mcmc_intervals(post_PRM,
                           pars = c("a", "b", "sigma"),
                           prob = 0.5, prob_outer = 0.9) +
  labs(title = expression(paste(italic("Protomyctophum bolini"))), x = "Posterior Predictions", y = "Parameters") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15, family = "sans"),
        title = element_text(size = 10),
        axis.title = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"))  # Print the minor gridlines

grid.arrange(ELN_plot, ELC_plot, GYR_plot, KRA_plot, PRM_plot)

## Traceplots

post_GYN <- read.csv("Outputs/Posterior_GYN_W.csv")

post_ELN$Chain <- rep(c(1, 2, 3, 4), each = 9000)
trace_ELN <- mcmc_trace(post_ELN,  pars = c("a", "b", "sigma"),
                facet_args = list(nrow = 3, labeller = label_parsed)) +
  facet_text(size = 15) +
  labs(title = expression(paste(italic("Electrona antarctica"))), x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15, family = "sans"),
        title = element_text(size = 10),
        axis.title = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"))  # Print the minor gridlines
trace_ELN

post_ELC$Chain <- rep(c(1, 2, 3, 4), each = 9000)
trace_ELC <- mcmc_trace(post_ELC,  pars = c("a", "b", "sigma"),
                        facet_args = list(nrow = 3, labeller = label_parsed)) +
  facet_text(size = 15) +
  labs(title = expression(paste(italic("Electrona carlsbergi"))), x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15, family = "sans"),
        title = element_text(size = 10),
        axis.title = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"))  # Print the minor gridlines
trace_ELC

post_GYR$Chain <- rep(c(1, 2, 3, 4), each = 9000)
trace_GYR <- mcmc_trace(post_GYR,  pars = c("a", "b", "sigma"),
                        facet_args = list(nrow = 3, labeller = label_parsed)) +
  facet_text(size = 15) +
  labs(title = expression(paste(italic("Gymnoscopelus braueri"))), x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15, family = "sans"),
        title = element_text(size = 10),
        axis.title = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"))  # Print the minor gridliness
trace_GYR

post_GYN$Chain <- rep(c(1, 2, 3, 4), each = 9000)
trace_GYN <- mcmc_trace(post_GYN,  pars = c("a", "b", "sigma"),
                        facet_args = list(nrow = 3, labeller = label_parsed)) +
  facet_text(size = 15) +
  labs(title = expression(paste(italic("Gymnoscopelus nicholsi"))), x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15, family = "sans"),
        title = element_text(size = 10),
        axis.title = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"))  # Print the minor gridlines
trace_GYN

post_KRA$Chain <- rep(c(1, 2, 3, 4), each = 9000)
trace_KRA <- mcmc_trace(post_KRA,  pars = c("a", "b", "sigma"),
                        facet_args = list(nrow = 3, labeller = label_parsed)) +
  facet_text(size = 15) +
  labs(title = expression(paste(italic("Krefftichthys anderssoni"))), x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15, family = "sans"),
        title = element_text(size = 10),
        axis.title = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"))  # Print the minor gridlines
trace_KRA

post_PRM$Chain <- rep(c(1, 2, 3, 4), each = 9000)
trace_PRM <- mcmc_trace(post_PRM,  pars = c("a", "b", "sigma"),
                        facet_args = list(nrow = 3, labeller = label_parsed)) +
  facet_text(size = 15) +
  labs(title = expression(paste(italic("Protomyctophum bolini"))), x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15, family = "sans"),
        title = element_text(size = 10),
        axis.title = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"))  # Print the minor gridlines
trace_PRM
