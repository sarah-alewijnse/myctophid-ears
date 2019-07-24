#### Pseudo Bayesian Simulation of Temperature - All Data Points ####

library(tidyverse)

# Load data

myct <- read.csv("Data/Myctophids_Master.csv")
myct <- filter(myct, d13C != "NA")
d18O_sd <- sd(myct$D18O_vals)
d18O_min <- min(myct$D18O_vals)
d18O_max <- max(myct$D18O_vals)

# Load function

load("Functions/PseudoBayes_Temp.Rdata")

# Run for all

Temp_values <- data.frame()

for(i in 1:nrow(myct)){
  t <- with(myct[i,],
            PseudoBayes_Temp(d18O, 0.02, # Based on NOCS values
                             D18O_vals, d18O_sd, # SD of values (as they span Scotia Sea depth and lat-long)
                             d18O_min, d18O_max,
                             10000))
  Temp_values <- rbind(Temp_values, t)
}

myct_T <- cbind(myct, Temp_values)

# Do a test plots to check nothing cray is happening

plot(myct_T$temp) 
with(myct_T,
     plot(Temp, temp))
with(myct_T,
     boxplot(temp ~ sciname))
min(myct_T$sd_temp)
max(myct_T$sd_temp)
min(myct_T$temp)
max(myct_T$temp)

write.csv(myct_T, "Outputs/Temperature.csv")
