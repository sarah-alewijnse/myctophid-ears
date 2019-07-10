#### Pseudo Bayesian Simulation of Temperature - All Data Points ####

library(tidyverse)

# Load data

myct <- read.csv("Data/Myctophids_Master.csv")
myct <- filter(myct, d13C != "NA")
d18O_sd <- sd(myct$D18O_vals)

#### Create function for temperature ####

Pseudo_Bayes_Temp <- function(d18O, d18O_sd,
                             d18O_water, d18O_water_sd,
                             reps){
  
  # Calculate distributions
  set.seed(d18O)
  dist_d18O <- rnorm(reps, d18O, d18O_sd)
  set.seed(d18O_water)
  dist_d18O_water <- rnorm(reps, d18O_water, d18O_water_sd)
  # From Hoie et al. 2004
  set.seed(3.9)
  dist_param_1 <- rnorm(reps, 3.90, 0.24)
  set.seed(-0.20)
  dist_param_2 <- rnorm(reps, -0.20, 0.019)
  
  # Calculate temperature
  dist_d18 <- dist_d18O - dist_d18O_water
  dist_temp <- (dist_d18 - dist_param_1)/dist_param_2 # From Shephard et al. 2007
  mean_temp <- mean(dist_temp)
  sd_temp<- sd(dist_temp)
  result <- data.frame(mean_temp, sd_temp)
  return(result)
}

# Run for one

with(myct[10,],
     Pseudo_Bayes_Temp(d18O, 0.02, # Based on NOCS values
                      D18O_vals, d18O_sd, # SD of values (as they span Scotia Sea depth and lat-long)
                      10000))


# Run for all

Temp_values <- data.frame()

for(i in 1:nrow(myct)){
  t <- with(myct[i,],
            Pseudo_Bayes_Temp(d18O, 0.02, # Based on NOCS values
                              D18O_vals, d18O_sd, # SD of values (as they span Scotia Sea depth and lat-long)
                              10000))
  Temp_values <- rbind(Temp_values, t)
}

myct_T <- cbind(myct, Temp_values)

# Do a test plots to check nothing cray is happening

plot(myct_T$mean_temp) 
with(myct_T,
     plot(Temp, mean_temp))
with(myct_T,
     boxplot(mean_temp ~ sciname))
min(myct_T$sd_temp)
max(myct_T$sd_temp)

write.csv(myct_T, "Outputs/Temperature.csv")
