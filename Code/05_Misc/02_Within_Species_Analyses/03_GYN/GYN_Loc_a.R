### GYN Age Estimate ###

# Load tidyverse

library(tidyverse)

# Load and check data

myct <- read.csv("Data/Myctophids_M_Temp_Length_Maturity.csv")
glimpse(myct)

# Subset GYN

GYN <- filter(myct, Label == "GYN")

# South Shetland VBGF Parameters - Linkowski 1985

k <- 0.41
Lo <- 163.80
to <- 0.081

e <- exp(1)
a <- Lo / (Lo - GYN$SL)

GYN$Est_Age <- (1/k) * log(a, base = e) + to

min(GYN$Est_Age, na.rm = TRUE)
max(GYN$Est_Age, na.rm = TRUE)
mean(GYN$Est_Age, na.rm = TRUE)
sd(GYN$Est_Age, na.rm = TRUE)

# Length

min(GYN$SL, na.rm = TRUE)
max(GYN$SL, na.rm = TRUE)
mean(GYN$SL, na.rm = TRUE)
sd(GYN$SL, na.rm = TRUE)

unique(GYN$Station)

# Plot by year (proxy for location)

ggplot(data = GYN, aes(x = as.factor(Year.x), y = mean_M)) +
  geom_boxplot()

# Get means

S_Ork <- filter(GYN, Year.x == "2016")
mean(S_Ork$mean_M)
sd(S_Ork$mean_M)

Other <- filter(GYN, Year.x == "2008")
mean(Other$mean_M)
sd(Other$mean_M)

#### Model by Location ####

# Load required packages

library(rethinking)
library(bayesplot)

# View entire printout

options(max.print=999999)

#### Within GYN ####

GYN$South_Ork <- ifelse(GYN$Year.x == "2016", 1, 0)

mod_list <- list(
  M_obs = GYN$mean_M,
  M_se = GYN$se_M,
  South_Ork = GYN$South_Ork
)

# Model

model_GYN_South_Ork <- map2stan(
  alist(
    M_est ~ dnorm(mu , sigma),
    
    # Linear model
    mu <- a + b_S*South_Ork,
    
    # Data uncertainties
    M_obs ~ dnorm(M_est, M_se),
    
    # Parameters
    a ~ dnorm(0, 1),
    b_S ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = mod_list,
  start = list(M_est = mod_list$M_obs),
  WAIC = FALSE,
  iter = 10000,
  warmup = 5000,
  control = list(adapt_delta = 0.90))

# Check model

check_energy(model_GYN_South_Ork@stanfit)
check_treedepth(model_GYN_South_Ork@stanfit)

divergent <- get_sampler_params(model_GYN_South_Ork@stanfit, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)

precis(model_GYN_South_Ork, digits = 4, prob = 0.95, depth = 2)

# Save model

saveRDS(model_GYN_South_Ork, "Outputs/04_Misc/03_Within_Spp/03_GYN_Loc/GYN_South_Ork_model.rds")
