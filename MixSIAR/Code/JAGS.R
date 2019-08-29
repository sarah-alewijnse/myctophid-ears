#### JAGS Temperature ####

library(tidyverse)
library(rjags)

### Load and partition data

myct <- read.csv("Myctophids_Master.csv")
myct_1 <- filter(myct, MyNumber == "BAS_19")

d18O <- myct_1$d18O
W <- myct_1$D18O_vals
a <- 3.9
b <- -0.20
n <- 1
iterations <- 1000
burnin <- floor(iterations/2)
chains <- 1

inits <- list( d = 0.0 )

jags <- jags.model("Temp_Mod.txt",
                   data = list ("d18O" = d18O,
                                "a" = a,
                                "b" = b, 
                                "n" = n),
                   n.chains = 1,
                   n.adapt = 100)
