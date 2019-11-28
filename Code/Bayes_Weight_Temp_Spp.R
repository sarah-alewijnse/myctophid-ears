#### Bayesian Linear Models - Weight and Temp ####

library(tidyverse)
library(rethinking)
library(bayesplot)

options(max.print=999999)

myct <- read.csv("Myctophids_M_Temp_Bel.csv")
glimpse(myct)

#### Overall Model with Weight and Temp ####

myct_tidy <- filter(myct, Weight_SD == "0")
myct_tidy <- filter(myct, !is.na(mean_M))
myct_tidy$log_Weight <- log(myct_tidy$Weight.x)
glimpse(myct_tidy)

M_T_W_list <- list(
  M_obs = myct_tidy$mean_M,
  M_se = myct_tidy$se_M,
  Weight = myct_tidy$log_Weight,
  Temp_obs = myct_tidy$mean_Temp,
  Temp_se = myct_tidy$se_Temp,
  Species = myct_tidy$sciname
)

## Convert to z-scores

# Weight

Weight_mean <- mean(M_T_W_list$Weight)
Weight_sd <- sd(M_T_W_list$Weight)

for(i in 1:length(M_T_W_list$Weight)){
  M_T_W_list$Weight_Z[i] <- (M_T_W_list$Weight[i] - Weight_mean) / Weight_sd
}

# Temp_obs

Temp_obs_mean <- mean(M_T_W_list$Temp_obs)
Temp_obs_sd <- sd(M_T_W_list$Temp_obs)

for(i in 1:length(M_T_W_list$Temp_obs)){
  M_T_W_list$Temp_Obs_Z[i] <- (M_T_W_list$Temp_obs[i] - Temp_obs_mean) / Temp_obs_sd
}

# Tidy list

mod_list <- list(
  M_obs = M_T_W_list$M_obs,
  M_se = M_T_W_list$M_se,
  Weight = M_T_W_list$Weight_Z,
  Temp_obs = M_T_W_list$Temp_Obs_Z,
  Temp_se = M_T_W_list$Temp_se,
  Species = M_T_W_list$Species
)

## Model

model_M_T_W <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + a_Var[Species] +
      b_W*Weight +
      b_T*Temp_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_se),
    Temp_obs ~ dnorm(Temp_est, Temp_se),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_W ~ dnorm(0, 1),
    b_T ~ dnorm(0, 1),
    a_Var[Species] ~ dnorm(0 , sigma_Species),
    sigma_Species ~ dcauchy(0, 1),
    sigma ~ dexp(1)
  ),
  data = mod_list,
  start = list(M_est = mod_list$M_obs,
               Temp_est = mod_list$Temp_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 5000,
  control = list(adapt_delta = 0.90))

## Run diagnostics

check_energy(model_M_T_W@stanfit)
check_treedepth(model_M_T_W@stanfit)

divergent <- get_sampler_params(model_M_T_W@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_M_T_W, digits = 4, prob = 0.95, depth = 2)

## Save stanfit

saveRDS(model_M_T_W, "Outputs/M_T_W/M_T_W_model.rds")

model_M_T_W <- readRDS("Outputs/M_T_W/M_T_W_model.rds")

## Extract samples

## Plot intervals

post <- extract.samples(model_M_T_W)
post <- as.data.frame(post)

colnames(post)[217:227] <- c("a", "b_W", "b_T", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "sigma_Species", "sigma")

## Plot pairs

pairs(model_M_T_W, pars = c("a", "b_W", "b_T", "sigma", "sigma_Species"), cex.labels = 1)

## Plot intervals

color_scheme_set("darkgray")

mcmc_intervals(post,
               pars = c("a", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "b_W", "b_T",  "sigma_Species", "sigma"),
               prob = 0.5, prob_outer = 0.95) +
  labs(x = "Posterior Predictions", y = "Parameters") +
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 10, face = "italic"),
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 10),
        axis.text.y = element_text(colour = "black", face = "plain", size = 10))

## Plot trace

p <- mcmc_trace(post, pars = c("a", "a_Var_ELN", "a_Var_ELC", "a_Var_GYR", "a_Var_GYN", "a_Var_KRA", "a_Var_PRM", "b_W", "b_T",  "sigma_Species", "sigma"),
                facet_args = list(nrow = 4, labeller = label_parsed))
plot <- p + facet_text(size = 10) +
  labs(x = "Number of Iterations", y = "Parameter Value") +
  theme(panel.background = element_blank(), # Keep the background blank
        text = element_text(size = 10, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(colour = "black", size = 10, face = "plain"))
plot

## Plot M v Temp

# Do in base

cbPalette <- c("#56B4E9", "#0072B2", "#E69F00", "#D55E00", "#009E73", "#CC79A7")

colours <- cbPalette[as.numeric(myct_tidy$sciname)]

shapes <- c(21, 22, 23, 24, 25, 21)
shapes <- shapes[as.numeric(myct_tidy$sciname)]

plot(mean_M ~ mean_Temp, data = myct_tidy, col = "black", bg = colours, pch = shapes,
     xlab = expression('Temperature ('*~degree*C*')'),
     ylab = expression("M" ["oto"]))
with(myct_tidy,
     arrows(x0 = mean_Temp, y0 = mean_M - se_M,
            x1 = mean_Temp, y1 = mean_M + se_M, code = 0, col = colours))
with(myct_tidy,
     arrows(x0 = mean_Temp - se_Temp, y0 = mean_M,
            x1 = mean_Temp + se_Temp, y1 = mean_M, code = 0, col = colours))
abline(a = 0.1825, b = -0.0036, lwd = 2)
x0 <- min(myct_tidy$mean_Temp)
x1 <- max(myct_tidy$mean_Temp)
y0 <- 0.1825 + x0*-0.0036
y1 <- 0.1825 + x1*-0.0036

segments(x0, y0, x1, y1, lwd = 2)

temp_seq_1 <- seq(from = min(myct_tidy$mean_Temp), to = 0, length.out = 10000)
temp_seq_2 <- seq(from = 0, to = max(myct_tidy$mean_Temp), length.out = 10000)

low_ci_1 <- 0.1528 + temp_seq_1*0.0019
low_ci_2 <- 0.1528 + temp_seq_2*-0.0089
low_ci <- c(low_ci_1, low_ci_2)

up_ci_1 <- 0.2168 + temp_seq_1*-0.0089
up_ci_2 <- 0.2168 + temp_seq_2*0.0019
up_ci <- c(up_ci_1, up_ci_2)

yy <- c(smooth$y, smooth_2$y)
xx <- c(temp_seq_1, temp_seq_2,
        temp_seq_1, temp_seq_2)

xnew <- xx[order(Arg(scale(xx) + scale(yy) * 1i))]
ynew <- yy[order(Arg(scale(xx) + scale(yy) * 1i))]

polygon(xnew, ynew, col = rgb(64, 64, 64, max = 255, alpha = 50), border = NA)

smooth <- smooth.spline(low_ci, spar = 1)
smooth_2 <- smooth.spline(up_ci, spar = 1)

lm.ci <- function(x_data,
                  a, b,
                  a_up_ci, b_up_ci,
                  a_low_ci, b_low_ci){
  # Plot model
  x0 <- min(x_data)
  x1 <- max(x_data)
  y0 <- a + x0*b
  y1 <- a + x1*b
  segments(x0, y0, x1, y1, lwd = 2)
  # Create sequence for x values
  x_seq_1 <- seq(from = min(x_data), to = 0, length.out = 10000)
  x_seq_2 <- seq(from = 0, to = max(x_data), length.out = 10000)
  # Create upper CI
  up_ci_1 <- a_up_ci + x_seq_1*b_low_ci
  up_ci_2 <- a_up_ci + x_seq_2*b_up_ci
  up_ci <- c(up_ci_1, up_ci_2)
  smooth_up_ci <- smooth.spline(up_ci, spar = 1)
  # Create lower CI
  low_ci_1 <- a_low_ci + x_seq_1*b_up_ci
  low_ci_2 <- a_low_ci + x_seq_2*b_low_ci
  low_ci <- c(low_ci_1, low_ci_2)
  smooth_low_ci <- smooth.spline(low_ci, spar = 1)
  # Create points for polygon
  yy <- c(smooth_low_ci$y, smooth_up_ci$y)
  xx <- c(x_seq_1, x_seq_2,
          x_seq_1, x_seq_2)
  # Order points for polygon
  xxnew <- xx[order(Arg(scale(xx) + scale(yy) * 1i))]
  yynew <- yy[order(Arg(scale(xx) + scale(yy) * 1i))]
  # Plot polygon
  polygon(xxnew, yynew, col = rgb(64, 64, 64, max = 255, alpha = 75), border = NA)
}

lm.ci(myct_tidy$mean_Temp,
      0.1825, -0.0036,
      0.1528, -0.0089,
      0.2168, 0.0019)
