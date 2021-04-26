#### DIC and Diet Parameters ####

# Estimates DIC and diet based on random sampling of normal distributions

# Load required packages

library(tidyverse)
library(truncnorm) # Allows for truncated normal distributions

# Read in data file

myct <- read.csv("Data/Myctophids_Master.csv")

# Tidy data

myct <- filter(myct, d13C != "NA") # Remove those with no d13C
ID <- select(myct, MyNumber) # Create data frame of just IDs

#### DIC ####

# Create function for calculating DIC d13C

calc_DIC <- function(Year, # Year of capture
                     reps, # Number of repetitions in the sample
                     DIC_surf, DIC_surf_sd, DIC_min, DIC_max, # Surface DIC (mean, stan dev, min and max)
                     suess, suess_sd, suess_min, suess_max, # Post 1970 Suess effect (mean, stan dev, min and max)
                     suess_1970, suess_1970_sd # Pre 1970 Suess effect (mean and stan dev)
){
  
  # Calculate distributions with set.seeds to ensure reproducibility
  
  # Distribution of DIC at the surface
  
  set.seed(DIC_surf)
  dist_DIC_surf <- rtruncnorm(reps, DIC_min, DIC_max, DIC_surf, DIC_surf_sd)
  
  # Post 1970 distribution of Suess effect
  
  set.seed(suess)
  dist_suess <- rtruncnorm(reps, suess_min, suess_max, suess, suess_sd)
  
  # Pre 1970 distribution of the Suess effect
  
  set.seed(suess_1970)
  dist_suess_1970 <- rtruncnorm(reps, suess_min, suess_max, suess_1970, suess_1970_sd)
  
  # Calculate DIC, correcting for the Suess effect (Tagliabue & Bopp 2008)
  
  dist_DIC <- if(Year < 1970){
    dist_DIC_surf-(dist_suess_1970*(abs(1990-Year)/10))
  } else if(Year < 1990){
    dist_DIC_surf-(dist_suess*(abs(1990-Year)/10))
  } else {
    dist_DIC_surf+(dist_suess*(abs(1990-Year)/10))
  }
  
  # Get distribution (mean and sd)
  
  DIC <- mean(dist_DIC)
  sd_DIC <- sd(dist_DIC)
  result <- data.frame(DIC, sd_DIC)
  return(result)
}

# Loop over dataset to calculate DIC values

DIC_values <- data.frame() # Empty data frame for storage

for(i in 1:nrow(myct)){
  ms <- with(myct[i,],
             calc_DIC(Year.x, 10000,
                           DIC, 0.202, 0, 3, # From Tagliabue & Bopp, 2007
                           Suess, 0.202, -0.28, 0, # From Tagliabue & Bopp, 2007. SD same as for DIC
                           -0.07, 0.202 # From Tagliabue & Bopp, 2007. SD same as for DIC
                          )) 
  DIC_values <- rbind(DIC_values, ms)
}

whole <- cbind(ID, DIC_values) # Join with individual ID

#### Diet ####

# Create function for calculating diet d13C

calc_diet <- function(Year, # Year of capture
                      reps, # Number of repetitions in sample
                      musc, musc_sd, # Muscle d13C (mean and stan dev)
                      enrich, enrich_se){ # Trophic enrichment factor (mean and stan dev)
  
  # Calculate distributions with set.seeds to ensure reproducibility
  
  # Muscle d13C distribution
  
  set.seed(musc)
  dist_musc <- rnorm(reps, musc, musc_sd)
  
  # Trophic enrichment factor distribution
  
  set.seed(enrich)
  dist_enrich <- rnorm(reps, enrich, enrich_se)
  
  # Calculate diet
  
  dist_diet <- dist_musc - dist_enrich
  
  # Get distribution (mean and stan dev)
  
  diet <- mean(dist_diet)
  sd_diet <- sd(dist_diet)
  result <- data.frame(diet, sd_diet)
  return(result)
}

# Loop over dataset to calculate diet 

diet_values <- data.frame() # Empty data frame for storage

for(i in 1:nrow(myct)){
  ms <- with(myct[i,],
             calc_diet(Year.x, 10000,
                      d13C_musc, d13C_musc_SD,
                      0.8, 1.1 # Trophic enrichment factor from DeNiro & Epstein 1978
             )) 
  diet_values <- rbind(diet_values, ms)
}

whole <- cbind(whole, diet_values) # Join with individual ID and DIC (will need to rearrange in Excel to fit into MixSIAR format)

#### Water d18O ####

# Create function for calculating d18O of seawater

calc_water <- function(reps, # Number of repetitions in sample
                       sal, # Salinity
                       a, a_sd, # Parameter a (mean and stan dev)
                       b, b_sd){ # Parameter b (mean and stan dev)
  
  # Calculate distributions with set.seeds to ensure reproducibility
  
  # a distribution
  
  set.seed(a)
  dist_a <- rnorm(reps, a, a_sd)
  
  # b distribution
  
  set.seed(b)
  dist_b <- rnorm(reps, b, b_sd)
  
  # Calculate d18O_SW
  
  dist_water <- dist_a + dist_b*sal
  
  # Get distribution (mean and stan dev)
  
  d18O_SW <- mean(dist_water)
  sd_d18O_SW <- sd(dist_water)
  result <- data.frame(d18O_SW, sd_d18O_SW)
  return(result)
}

# Loop over dataset to calculate diet 

water_values <- data.frame() # Empty data frame for storage

JCR <- read.csv("Data/JCR_Station_Dat.csv")

for(i in 1:nrow(JCR)){
  ms <- with(JCR[i,],
             calc_water(10000, Salinity,
                       -8.45, 0.478,
                       0.24, 0.014 # Trophic enrichment factor from DeNiro & Epstein 1978
             )) 
  water_values <- rbind(water_values, ms)
}

whole <- cbind(JCR, water_values) # Join with individual ID and DIC (will need to rearrange in Excel to fit into MixSIAR format)

#### Combine with myctophid data ####

write.csv(whole, "Data/JCR_Station_Dat.csv", row.names = F) # Write into CSV file

myct <- read.csv("Data/Myctophids_Master.csv")

JCR_d18O <- select(JCR, Station, d18O_SW, sd_d18O_SW)

myct_d18O <- left_join(myct, JCR_d18O, by = "Station")

# Write into file

write.csv(myct_d18O, "Data/Myctophids_Master.csv", row.names = F)
