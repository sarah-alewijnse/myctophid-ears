#### Psuedo-Bayesian Calculation of M Distribution ####

library(tidyverse)
library(HDInterval)
library(truncnorm)

# Load data

myct <- read.csv("Data/Myctophids_Master.csv")

#### Create Function to calculate M ####

M_Val_Dist <- function(d13C, d13C_sd,
                  Year, reps,
                  DIC_surf, DIC_surf_sd, DIC_min, DIC_max,
                  suess, suess_sd, suess_min, suess_max,
                  suess_1970, suess_1970_sd,
                  musc, musc_sd,
                  enrich, enrich_se,
                  e_value){
  
  # Calculate distributions with set.seeds to ensure reproducibility
  set.seed(d13C)
  dist_d13C <- rnorm(reps, d13C, d13C_sd)
  set.seed(DIC_surf)
  dist_DIC_surf <- rtruncnorm(reps, DIC_min, DIC_max, DIC_surf, DIC_surf_sd)
  set.seed(suess)
  dist_suess <- rtruncnorm(reps, suess_min, suess_max, suess, suess_sd)
  set.seed(suess_1970)
  dist_suess_1970 <- rtruncnorm(reps, suess_min, suess_max, suess_1970, suess_1970_sd)
  set.seed(musc)
  dist_musc <- rnorm(reps, musc, musc_sd)
  set.seed(enrich)
  dist_enrich <- rnorm(reps, enrich, enrich_se)
  set.seed(Sys.time())
  
  # Calculate DIC, correcting for the Suess effect
  dist_DIC <- if(Year < 1970){
    dist_DIC_surf-(dist_suess_1970*((1990-Year)/10))
  } else if(Year < 1990){
    dist_DIC_surf-(dist_suess*((1990-Year)/10))
  } else {
    dist_DIC_surf+(dist_suess*((1990-Year)/10))
  }
  
  # Calculate diet
  dist_diet <- dist_musc - (1*dist_enrich)
  
  # Calculate M
  dist_M <- (dist_d13C - dist_DIC) / (dist_diet - dist_DIC)
  return(dist_M)
}

save("M_Val_Dist", file = "Functions/M_Val_Dist.Rdata")

## Test

with(myct[29,],
     M_Val_Dist(d13C, 0.02, # SD based on values from isotope lab
                Year.x, 10000,
                DIC, 0.202, 0, 3, # From Tagliabue & Bopp, 2007
                Suess, 0.202, -0.28, 0, # From Tagliabue & Bopp, 2007. SD same as for DIC
                -0.07, 0.202, # From Tagliabue & Bopp, 2007. SD same as for DIC
                d13C_musc, d13C_musc_SD,
                0.8, 1.1, # From DeNiro & Epstein
                0)) # From Solomon et al. 
