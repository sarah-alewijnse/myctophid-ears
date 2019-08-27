#### Bayesian Linear Models - Respiration ####

library(tidyverse)
library(rethinking)
library(bayesplot)

myct <- read.csv("Outputs/Combined.csv")
myct_tidy <- filter(myct, Weight_SD == "0")
myct_tidy$ln_Weight <- log(myct_tidy$Weight.x)
resp <- read.csv("Outputs/Belcher_MR.csv")
myct_tidy <- cbind(myct_tidy, resp)

#### Overall Model with Temperature ####

M_R_list <- list(
  M_obs = myct_tidy$M,
  M_sd = myct_tidy$sd_M,
  Resp_obs = myct_tidy$MR,
  Resp_sd = myct_tidy$sd_MR,
  Species = myct_tidy$sciname
)

model_M_R <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    mu <- a +
      b*Resp_est[i],
    M_obs ~ dnorm(M_est, M_sd),
    Resp_obs ~ dnorm(Resp_est, Resp_sd),
    a ~ dnorm(0.2, 1),
    b ~ dnorm(0, 10),
    sigma ~ dcauchy(0.1, 1)
  ),
  data = M_R_list,
  start = list(M_est = M_R_list$M_obs,
               Resp_est = M_R_list$Resp_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99))

## Precis Tables

precis(model_M_R, digits = 4)
precis(model_M_R, depth = 2, digits = 4)

## Check chains

plot(model_M_R)

## Plot

## Plot

resp_seq <- seq(from = min(M_R_list$Resp_obs), to = max(M_R_list$Resp_obs), by = 0.001) # Horizontal axis
mu <- link(model_M_R, data = data.frame(Resp_est = resp_seq)) # Link model to mu

# Summarise the distribution of mu

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.95)

# Plot with model and raw data


p <- c("#56B4E9", "#0072B2", "#E69F00", "#D55E00", "#009E73", "#CC79A7")
palette(p)

par(mfrow = c(1,1))
par(mar=c(4,4,3,3))
shapes <- c(21:25, 24)
shapes <- shapes[as.numeric(M_R_list$Species)]
plot(M_obs ~ Resp_obs, data = M_R_list, pch = shapes, cex = 1.5, xlab = "", ylab = "", xlim = c(-2, 0.5), ylim = c(0.12, 0.35), tck = -0.01, bg = Species)
mtext(text = expression(paste("Ln(Mass-Specific Respiration) (", mu, "l O"["2"], " mg"["-1"], " h"["-1"], ")")), side = 1, line = 2)
mtext(text = "M Value", side = 2, line = 2)
with(M_R_list, arrows(Resp_obs, M_obs - M_sd, Resp_obs, M_obs + M_sd, length = 0.05, angle = 90 , code = 3, col = col.alpha("black", 0.2)))
with(M_R_list, arrows(Resp_obs - Resp_sd, M_obs, Resp_obs + Resp_sd, M_obs, length = 0.05, angle = 90 , code = 3, col = col.alpha("black", 0.2)))
points(M_obs ~ Resp_obs, data = M_R_list, pch = shapes, cex = 1.5, bg = Species)
lines(resp_seq, mu.mean, lwd = 3)
shade(mu.HPDI, resp_seq)
legend(0, 0.35, legend=c("Electrona antarctica", "Electrona carlsbergi", "Gymnoscopelus braueri", "Gymnoscopelus nicholsi", "Krefftichthys anderssoni", "Protomyctophum bolini"),
      pch = c(21, 22, 23, 24, 25, 24), cex=0.8, text.font = 3)

post <- extract.samples(model_M_R)
post <- as.data.frame(post)
write.csv(post, "Outputs/model_M_Resp.csv")

post <- read.csv("Outputs/Posterior_Resp.csv")

## Traceplot

color_scheme_set("grey")
post$Chain <- rep(c(1, 2, 3, 4), each = 9000)

p <- mcmc_trace(post,  pars = c("a", "b", "sigma"),
                facet_args = list(nrow = 3, labeller = label_parsed))
p + facet_text(size = 15) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"))  # Print the minor gridlines

## Interval plot


mcmc_intervals(post,
               pars = c("a", "b", "sigma"),
               prob = 0.5, prob_outer = 0.9) +
  labs(x = "Posterior Predictions", y = "Parameters") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 15, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black"))  # Print the minor gridlines

