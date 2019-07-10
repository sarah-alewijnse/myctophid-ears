#### Psuedo-Bayesian Calculation of M ####

library(tidyverse)

# Load data

myct <- read.csv("Data/Myctophids_Master.csv")

#### Create Function to calculate M ####

PseudoBayes_M <- function(d13C, d13C_sd,
                          Year, reps,
                          DIC_surf, DIC_surf_sd,
                          suess, suess_sd,
                          suess_1970, suess_1970_sd,
                          phyto_min, phyto_max,
                          phyto_suess, phyto_suess_sd,
                          troph, troph_se,
                          enrich, enrich_se,
                          e_value, e_sd){
  
  # Calculate distributions with set.seeds to ensure reproducibility
  set.seed(d13C)
  dist_d13C <- rnorm(reps, d13C, d13C_sd)
  set.seed(DIC_surf)
  dist_DIC_surf <- rnorm(reps, DIC_surf, DIC_surf_sd)
  set.seed(suess)
  dist_suess <- rnorm(reps, suess, suess_sd)
  set.seed(suess_1970)
  dist_suess_1970 <- rnorm(reps, suess_1970, suess_1970_sd)
  set.seed(phyto_min)
  dist_phyto <- runif(reps, phyto_min, phyto_max)
  set.seed(phyto_suess)
  dist_phyto_suess <- rnorm(reps, phyto_suess, phyto_suess_sd)
  set.seed(troph)
  dist_troph <- rnorm(reps, troph, troph_se)
  set.seed(enrich)
  dist_enrich <- rnorm(reps, enrich, enrich_se)
  set.seed(e_value)
  dist_e <- rnorm(reps, e_value, e_sd)
  set.seed(Sys.time())
  
  # Calculate DIC, correcting for the Suess effect
  dist_DIC <- if(Year < 1970){
    dist_DIC_surf-(dist_suess_1970*((1990-Year)/10))
  } else if(Year < 1990){
    dist_DIC_surf-(dist_suess*((1990-Year)/10))
  } else {
    dist_DIC_surf+(dist_suess*((1990-Year)/10))
  }
  
  # Calculate Phyto, correcting for the Suess effect
  dist_phyto_corr <- if(Year < 1970){
    dist_phyto-(dist_phyto_suess*((2001-Year)/10))
  } else if(Year < 2001){
    dist_phyto-(dist_phyto_suess*((2001-Year)/10))
  } else if(Year > 2010){
    dist_phyto+(dist_phyto_suess*((2010-Year)/10))
  } else {
    dist_phyto
  }
  
  # Calculate diet
  dist_diet <- dist_phyto_corr + (dist_troph - 1) * dist_enrich # -1 to get to prey and -1 get rid of phytoplankton
  
  # Calculate M
  dist_M <- (d13C - dist_DIC) / (dist_diet - dist_DIC)
  max <- which.max(density(dist_M)$y)
  M <- density(dist_M)$x[max]
  M_HDI <- hdi(dist_M, credMass = 0.68) # Set so it matches stan devs
  M_HDI_min <- unname(M_HDI[1])
  M_HDI_max <- unname(M_HDI[2])
  result <- data.frame(M, M_HDI_min, M_HDI_max)
  return(result)
}

save("PseudoBayes_M", file = "Functions/PseudoBayes_M.Rdata")

## Test

with(myct[2,],
     PseudoBayes_M(d13C, 0.02,
                   Year, 10000,
                   DIC, 0.202,
                   Suess, 0.202,
                   -0.07, 0.202,
                   -28, -26,
                   -0.17, 0.202,
                   UseTroph, UseTrophSe,
                   0.8, 1.1,
                   0, 1.8))
