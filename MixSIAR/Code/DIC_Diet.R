#### DIC ####

library(truncnorm)
library(tidyverse)

# Create function for DIC

calc_DIC <- function(Year, reps,
                     DIC_surf, DIC_surf_sd, DIC_min, DIC_max,
                     suess, suess_sd, suess_min, suess_max,
                     suess_1970, suess_1970_sd
){
  
  # Calculate distributions with set.seeds to ensure reproducibility
  set.seed(DIC_surf)
  dist_DIC_surf <- rtruncnorm(reps, DIC_min, DIC_max, DIC_surf, DIC_surf_sd)
  set.seed(suess)
  dist_suess <- rtruncnorm(reps, suess_min, suess_max, suess, suess_sd)
  set.seed(suess_1970)
  dist_suess_1970 <- rtruncnorm(reps, suess_min, suess_max, suess_1970, suess_1970_sd)
  
  # Calculate DIC, correcting for the Suess effect
  dist_DIC <- if(Year < 1970){
    dist_DIC_surf-(dist_suess_1970*((1990-Year)/10))
  } else if(Year < 1990){
    dist_DIC_surf-(dist_suess*((1990-Year)/10))
  } else {
    dist_DIC_surf+(dist_suess*((1990-Year)/10))
  }
  
  # Get distribution
  DIC <- mean(dist_DIC)
  sd_DIC <- sd(dist_DIC)
  result <- data.frame(DIC, sd_DIC)
  return(result)
}

# Calculate DIC values

DIC_vals <- read.csv("myct_source.csv")
ID <- select(DIC_vals, MyNumber)

DIC_values <- data.frame()

for(i in 1:nrow(DIC_vals)){
  ms <- with(DIC_vals[i,],
             calc_DIC(Year, 10000,
                           DIC, 0.202, 0, 3, # From Tagliabue & Bopp, 2007
                           Suess, 0.202, -0.28, 0, # From Tagliabue & Bopp, 2007. SD same as for DIC
                           -0.07, 0.202 # From Tagliabue & Bopp, 2007. SD same as for DIC
                          )) 
  DIC_values <- rbind(DIC_values, ms)
}

whole <- cbind(ID, DIC_values) # Join metabol with M values

#### Diet ####

calc_diet <- function(Year, reps,
                      musc, musc_sd,
                      enrich, enrich_se){
  
  # Calculate distributions with set.seeds to ensure reproducibility
  set.seed(musc)
  dist_musc <- rnorm(reps, musc, musc_sd)
  set.seed(enrich)
  dist_enrich <- rnorm(reps, enrich, enrich_se)
  
  # Calculate diet
  dist_diet <- dist_musc - dist_enrich
  
  # Calculate M
  diet <- mean(dist_diet)
  sd_diet <- sd(dist_diet)
  result <- data.frame(diet, sd_diet)
  return(result)
}

# Calculate diet 

diet_values <- data.frame()

for(i in 1:nrow(DIC_vals)){
  ms <- with(DIC_vals[i,],
             calc_diet(Year, 10000,
                      d13C_musc, d13C_musc_SD,
                      0.8, 1.1
             )) 
  diet_values <- rbind(diet_values, ms)
}

whole <- cbind(whole, diet_values) # Join metabol with M values

write.csv(whole, "myct_source.csv", row.names = F)
