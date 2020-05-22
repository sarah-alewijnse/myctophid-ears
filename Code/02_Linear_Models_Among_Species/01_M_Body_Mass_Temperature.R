#### Bayesian Linear Models - Weight and Temp ####

library(tidyverse)
library(rethinking)
library(bayesplot)

options(max.print=999999)

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")
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

saveRDS(model_M_T_W, "Outputs/02_Linear_Models_Among_Species/01_M_Body_Mass_Temperature/M_T_W_model.rds")

model_M_T_W <- readRDS("Outputs/02_Linear_Models_Among_Species/01_M_Body_Mass_Temperature/M_T_W_model.rds")

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
               pars = c("sigma", "sigma_Species", "a_Var_GYN", "a_Var_PRM", "a_Var_ELC", "a_Var_KRA", "a_Var_GYR",  "a_Var_ELN", "b_T", "b_W", "a"),
               prob = 0.5, prob_outer = 0.95) +
  labs(x = "Posterior Predictions", y = "Parameters") +
  scale_y_discrete(labels = c("sigma", expression("sigma" ["species"]), expression("a_Var" ["GYN"]), expression("a_Var" ["PRM"]), expression("a_Var" ["ELC"]), expression("a_Var" ["KRA"]), expression("a_Var" ["GYR"]), expression("a_Var" ["ELN"]), expression ("b" ["t"]), expression("b" ["W"]), "a")) +
  theme(panel.background = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 15, face = "italic"),
        text = element_text(size = 15, family = "sans"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(colour = "black", face = "plain", size = 15),
        axis.text.y = element_text(colour = "black", face = "plain", size = 15))

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

par(mar=c(5.2, 4.5, 4.1, 14), xpd=TRUE)
plot(mean_M ~ mean_Temp, data = myct_tidy, col = "black", bg = colours, pch = shapes, cex = 1.3,
     xlab = expression('Temperature ('*~degree*C*')'),
     ylab = expression("M" ["oto"]))
#with(myct_tidy,
     #arrows(x0 = mean_Temp, y0 = mean_M - se_M,
            #x1 = mean_Temp, y1 = mean_M + se_M, code = 0, col = colours))
#with(myct_tidy,
     #arrows(x0 = mean_Temp - se_Temp, y0 = mean_M,
            #x1 = mean_Temp + se_Temp, y1 = mean_M, code = 0, col = colours))
legend("right", inset = c(-0.6), legend = c("Electrona antarctica",
                                                  "Electrona carlsbergi",
                                                  "Gymnoscopelus braueri",
                                                  "Gymnoscopelus nicholsi",
                                                  "Protomyctophum bolini",
                                                  "Krefftichthys anderssoni"),
       pch = c(21, 22, 23, 24, 25, 21), col = "black", text.font = 3,
       pt.bg = c("#56B4E9", "#0072B2", "#E69F00", "#D55E00", "#009E73", "#CC79A7"))

load("LM_CI.rdata")

lm.ci(myct_tidy$mean_Temp,
      0.1825, -0.0036,
      0.1528, -0.0089,
      0.2168, 0.0019)

## Plot M v body mass

par(mar=c(5.2, 4.5, 4.1, 14), xpd=TRUE)
plot(mean_M ~ log_Weight, data = myct_tidy, col = "black", bg = colours, pch = shapes, cex = 1.3,
     xlab = "ln(Weight (g))",
     ylab = expression("M" ["oto"]))
legend("right", inset = c(-0.6), legend = c("Electrona antarctica",
                                            "Electrona carlsbergi",
                                            "Gymnoscopelus braueri",
                                            "Gymnoscopelus nicholsi",
                                            "Protomyctophum bolini",
                                            "Krefftichthys anderssoni"),
       pch = c(21, 22, 23, 24, 25, 21), col = "black", text.font = 3,
       pt.bg = c("#56B4E9", "#0072B2", "#E69F00", "#D55E00", "#009E73", "#CC79A7"))
