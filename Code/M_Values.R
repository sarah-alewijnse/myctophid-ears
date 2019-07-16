#### Psuedo-Bayesian Simualtions of M - All Points ####

library(tidyverse)
library(truncnorm)
library(HDInterval)

# Load data

myct <- read.csv("Data/Myctophids_Master.csv")
myct <- filter(myct, d13C != "NA")

# Load function

load("Functions/PseudoBayes_M.Rdata")

# Run for all

M_values <- data.frame()

for(i in 1:nrow(myct)){
  ms <- with(myct[i,],
             PseudoBayes_M(d13C, 0.02, # SD based on values from isotope lab
                           Year.x, 10000,
                           DIC, 0.202, 0, 3, # From Tagliabue & Bopp, 2007
                           Suess, 0.202, -0.28, 0, # From Tagliabue & Bopp, 2007. SD same as for DIC
                           -0.07, 0.202, # From Tagliabue & Bopp, 2007. SD same as for DIC
                           Phyto, Phyto_sd, -31, -16.5, # From Magozzi et al. 2017. Based on values for Longhurst biogeographic provinces
                           -0.17, 0.202, # From Clive - per comms. SD same as for DIC
                           UseTroph, UseTrophSe, 2, 5, # From FishBase
                           0.8, 1.1, # From DeNiro & Epstein
                           0, 1.8)) # From Solomon et al. 
  M_values <- rbind(M_values, ms)
}

metabol_M <- cbind(myct, M_values) # Join metabol with M values

hist(metabol_M$M) # Do a test plot of M to check nothing cray is happening

plot(metabol_M$M, metabol_M$HDI_range_M)

# Write into file

write.csv(metabol_M, "Outputs/M_Values.csv")
