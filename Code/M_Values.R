#### Psuedo-Bayesian Simualtions of M - All Points ####

library(tidyverse)

# Load data

myct <- read.csv("Data/Myctophids_Master.csv")
myct <- filter(myct, d13C != "NA")

# Load function

load("Functions/PseudoBayes_M.Rdata")

# Run for all

M_values <- data.frame()

for(i in 1:nrow(myct)){
  ms <- with(myct[i,],
             PseudoBayes_M(d13C, 0.02, # 0.02 based on NOCS output
                           Year, 10000, # Add number of reps next to year
                           DIC, 0.202, # Sd from PISCES-A - 0-5500m - Tagliabue & Bopp 2008
                           Suess, 0.202, # Use 0.202 based on DIC (best I could find) - Tagliabue & Bopp 2008
                           -0.07, 0.202, # From Clive - per comms. with 0.202 based on DIC
                           -29, -26, # Based on Scotia Sea values from Magozzi et al. 2017
                           -0.17, 0.202, # From Clive - per comms.
                           UseTroph, UseTrophSe, # Se isn't great, but again, best we have - from FishBase
                           0.8, 1.1, # From De Niro & Epstein 1978
                           0, 1.8)) # Based on e-total from Solomon et al. 2006
  M_values <- rbind(M_values, ms)
}

metabol_M <- cbind(myct, M_values) # Join metabol with M values

hist(metabol_M$mean_M) # Do a test plot of M to check nothing cray is happening

plot(metabol_M$mean_M, metabol_M$sd_M)

# Write into file

write.csv(metabol_M, "Outputs/M_Values.csv")
