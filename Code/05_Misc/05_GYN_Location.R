### GYN Age Estimate ###

library(tidyverse)

GYN <- read.csv("Data/GYN_with_Length.csv")

# South Shetland Parameters - Linkowski 1985

k <- 0.41
Lo <- 163.80
to <- 0.081

a <- Lo / (Lo - GYN$Length_SL)

GYN$Est_Age <- (1/k) * log(a, base = e) + to

min(GYN$Est_Age, na.rm = TRUE)
max(GYN$Est_Age, na.rm = TRUE)
mean(GYN$Est_Age, na.rm = TRUE)
sd(GYN$Est_Age, na.rm = TRUE)

# Length

min(GYN$Length_SL, na.rm = TRUE)
max(GYN$Length_SL, na.rm = TRUE)
mean(GYN$Length_SL, na.rm = TRUE)
sd(GYN$Length_SL, na.rm = TRUE)

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

library(rethinking)
library(bayesplot)

options(max.print=999999)

myct <- read.csv("Data/Myctophids_M_Temp_Bel.csv")
glimpse(myct)

#### Within GYN ####

GYN_tidy <- filter(myct, Label == "GYN")
glimpse(GYN_tidy)

GYN_tidy$South_Ork <- ifelse(GYN_tidy$Year.x == "2016", 1, 0)

mod_list <- list(
  M_obs = GYN_tidy$mean_M,
  M_se = GYN_tidy$se_M,
  South_Ork = GYN_tidy$South_Ork
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

saveRDS(model_GYN_South_Ork, "Outputs/04_Misc/06_GYN_Location/GYN_South_Ork_model.rds")
