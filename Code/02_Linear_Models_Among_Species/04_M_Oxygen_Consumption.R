#### Bayesian Linear Models - Belcher RMR Estimates ####

library(tidyverse)
library(rethinking)
library(bayesplot)

options(max.print=999999)

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")
glimpse(myct)

#### Overall Model with Weight and Temp ####

myct_tidy <- filter(myct, !is.na(mean_Metabol))
myct_tidy <- filter(myct_tidy, !is.na(mean_M))
glimpse(myct_tidy)

M_Metabol_list <- list(
  M_obs = myct_tidy$mean_M,
  M_se = myct_tidy$se_M,
  Metabol_obs = myct_tidy$mean_Metabol,
  Metabol_se = myct_tidy$se_Metabol
)

## Convert to z-scores

# Metabol_obs

Metabol_obs_mean <- mean(M_Metabol_list$Metabol_obs)
Metabol_obs_sd <- sd(M_Metabol_list$Metabol_obs)

for(i in 1:length(M_Metabol_list$Metabol_obs)){
  M_Metabol_list$Metabol_Obs_Z[i] <- (M_Metabol_list$Metabol_obs[i] - Metabol_obs_mean) / Metabol_obs_sd
}

# Tidy list

mod_list <- list(
  M_obs = M_Metabol_list$M_obs,
  M_se = M_Metabol_list$M_se,
  Metabol_obs = M_Metabol_list$Metabol_Obs_Z,
  Metabol_se = M_Metabol_list$Metabol_se
)

## Model

  model_M_Metabol <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b * Metabol_est[i],
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_se),
    Metabol_obs ~ dnorm(Metabol_est, Metabol_se),
    
    # Parameters
    a ~ dnorm(0, 1),
    b ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = mod_list,
  start = list(M_est = mod_list$M_obs,
               Metabol_est = mod_list$Metabol_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 5000)

  ## Run diagnostics
  
  check_energy(model_M_Metabol@stanfit)
  check_treedepth(model_M_Metabol@stanfit)
  
  divergent <- get_sampler_params(model_M_Metabol@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
  sum(divergent)
  
  pairs(model_M_Metabol, pars = c("a", "b", "sigma"))
  
  precis(model_M_Metabol, digits = 4, prob = 0.95, depth = 2)
  
  ## Save stanfit
  
  saveRDS(model_M_Metabol, "Outputs/02_Linear_Models_Among_Species/04_M_Oxygen_Consumption/M_Belcher_model.rds")
  
  model_M_Metabol <- readRDS("Outputs/02_Linear_Models_Among_Species/04_M_Oxygen_Consumption/M_Belcher_model.rds")

  ## Extract samples
  
  ## Plot intervals
  
  post <- extract.samples(model_M_Metabol)
  post <- as.data.frame(post)
  
  colnames(post)[201:203] <- c("a", "b", "sigma")
  
## Plot intervals
  
  color_scheme_set("darkgray")
  
  mcmc_intervals(post,
                 pars = c("a", "b", "sigma"),
                 prob = 0.5, prob_outer = 0.95) +
    labs(x = "Posterior Predictions", y = "Parameters") +
    theme(panel.background = element_blank(),
          legend.position = "none",
          strip.background = element_rect(fill = "white"),
          strip.text.x = element_text(size = 15, face = "italic"),
          text = element_text(size = 15, family = "sans"),
          panel.border = element_rect(colour = "black", fill = NA),
          axis.text.x = element_text(colour = "black", face = "plain", size = 15),
          axis.text.y = element_text(colour = "black", face = "plain", size = 15))  
  
## Traceplot
  
  p <- mcmc_trace(post, pars = c("a", "b", "sigma"),
                  facet_args = list(nrow = 4, labeller = label_parsed))
  plot <- p + facet_text(size = 10) +
    labs(x = "Number of Iterations", y = "Parameter Value") +
    theme(panel.background = element_blank(), # Keep the background blank
          text = element_text(size = 10, family = "sans"),
          panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_text(colour = "black", size = 10, face = "plain"))
  plot  
  
  ## Plot M v Metabol
  
  # Do in base
  
  cbPalette <- c("#56B4E9", "#0072B2", "#E69F00", "#D55E00", "#009E73", "#CC79A7")
  
  colours <- cbPalette[as.numeric(myct_tidy$sciname)]
  
  shapes <- c(21, 22, 23, 24, 25, 21)
  shapes <- shapes[as.numeric(myct_tidy$sciname)]
  
  par(mar=c(5.2, 4.5, 4.1, 14), xpd=TRUE)
  plot(mean_M ~ mean_Metabol, data = myct_tidy, col = "black", bg = colours, pch = shapes, cex = 1.3,
       xlab = expression(paste("Mass-Specific Oxygen Consumption (", mu, "l O"["2"], " mg"["-1"], "h"["-1"], ")")),
       ylab = expression("M" ["oto"]))
  with(myct_tidy,
  arrows(x0 = mean_Metabol - se_Metabol, y0 = mean_M,
  x1 = mean_Metabol + se_Metabol, y1 = mean_M, code = 0, col = colours))
  points(mean_M ~ mean_Metabol, data = myct_tidy, col = "black", bg = colours, pch = shapes, cex = 1.3)
  legend("right", inset = c(-0.6), legend = c("Electrona antarctica",
                                              "Electrona carlsbergi",
                                              "Gymnoscopelus braueri",
                                              "Gymnoscopelus nicholsi",
                                              "Protomyctophum bolini",
                                              "Krefftichthys anderssoni"),
         pch = c(21, 22, 23, 24, 25, 21), col = "black", text.font = 3,
         pt.bg = c("#56B4E9", "#0072B2", "#E69F00", "#D55E00", "#009E73", "#CC79A7"))
  
  load("LM_CI.rdata")
  
  lm.ci(myct_tidy$mean_Metabol,
        0.1861, -0.0047,
        0.1801, -0.0106,
        0.1919, 0.0010)


