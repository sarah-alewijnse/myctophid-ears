#### Pseudo Bayesian Calculation of Temperature ####

library(tidyverse)
library(HDInterval)
library(truncnorm)

# Load data

myct <- read.csv("Data/Myctophids_Master.csv")
myct <- filter(myct, d13C != "NA")

d18O_sd <- sd(myct$D18O_vals)

#### Create function for temperature ####

PseudoBayes_Temp <- function(d18O, d18O_sd,
                             d18O_water, d18O_water_sd,
                             d18O_water_min, d18O_water_max,
                             reps){
  
  # Calculate distributions
  set.seed(d18O)
  dist_d18O <- rnorm(reps, d18O, d18O_sd)
  set.seed(d18O_water)
  dist_d18O_water <- rtruncnorm(reps, d18O_water_min, d18O_water_max, d18O_water, d18O_water_sd)
  # From Hoie et al. 2004
  set.seed(3.9)
  dist_param_1 <- rnorm(reps, 3.90, 0.24)
  set.seed(-0.20)
  dist_param_2 <- rnorm(reps, -0.20, 0.019)
  
  # Calculate temperature
  dist_d18 <- dist_d18O - dist_d18O_water
  dist_temp <- (dist_d18 - dist_param_1)/dist_param_2 # From Shephard et al. 2007
  max_dens <- which.max(density(dist_temp)$y)
  temp <- density(dist_temp)$x[max]
  min_temp <- min(dist_temp)
  max_temp <- max(dist_temp)
  sd_temp <- sd(dist_temp)
  result <- data.frame(temp, sd_temp, min_temp, max_temp)
  return(result)
}
save("PseudoBayes_Temp", file = "Functions/PseudoBayes_Temp.Rdata")

# Run for one

d18O_sd <- sd(myct$D18O_vals)
d18O_min <- min(myct$D18O_vals)
d18O_max <- max(myct$D18O_vals)

with(myct[5,],
     PseudoBayes_Temp(d18O, 0.02, # Based on NOCS values
                      D18O_vals, 0.138, # SD from data
                      -0.373, 0.034, # Global min-max
                      10000))
