#### C:N Ratios ####

library(tidyverse)

#### DO NO RUN ####

# Import data

c_n_ratio <- read.csv("Data/CN_Ratio.csv")

# Join with C_resp

c_resp <- read.csv("Data/Myctophids_M_Temp.csv")

joined <- left_join(c_resp, c_n_ratio, by = "MyNumber")

# Write into file and edit to correct mislabelling

write.csv(joined, "Outputs/04_Misc/Lipid_Corrected/Lipid_Corrected_Data_1.csv")

#### RUN FROM HERE ONLY ####

# Read in again

joined <- read.csv("Outputs/04_Misc/Lipid_Corrected/Lipid_Corrected_Data.csv")

min(joined$C_N_Ratio, na.rm = TRUE)
max(joined$C_N_Ratio, na.rm = TRUE)

# Get muscle d13C with lipid correction
  # Using Hoffman & Sutton model

calc_d13C_corr <- function(reps,
                           musc, musc_sd,
                           c_n, c_n_sd,
                           L, L_sd,
                           pro, pro_sd){
  
  # Calculate distributions with set.seeds to ensure reproduciblity
  set.seed(musc)
  dist_musc <- rnorm(reps, musc, musc_sd)
  set.seed(L)
  dist_L <- rnorm(reps, L, L_sd)
  set.seed(pro)
  dist_pro <- rnorm(reps, pro, pro_sd)
  set.seed(c_n)
  dist_c_n <- rnorm(reps, c_n, c_n_sd)
  
  # Calculate corrected muscle d13C
  dist_d13C_corr <- dist_musc + (dist_L * (dist_pro - dist_c_n))/dist_c_n
  
  # Get distribution
  d13C_corr <- mean(dist_d13C_corr)
  sd_d13C_corr <- sd(dist_d13C_corr)
  result <- data.frame(d13C_corr, sd_d13C_corr)
  return(result)
}

# Test

calc_d13C_corr(10000, 
               -27.68, 0,
               4.44, 0,
               -6.39, 1.21,
               3.76, 0.07)

# Remove NAs

joined<- joined[-58,]

# Loop

d13C_corrected <- data.frame()

for(i in 1:nrow(joined)){
  ms <- with(joined[i,],
             calc_d13C_corr(100000,
                            d13C_musc, d13C_musc_SD,
                            C_N_Ratio, c_n_sd,
                            -6.39, 1.21,
                            3.76, 0.07
             )) 
  d13C_corrected <- rbind(d13C_corrected, ms)
}

joined_corr <- cbind(joined, d13C_corrected)


#### Calculate Diet d13C ####

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
  
  # Get distribution
  diet <- mean(dist_diet)
  sd_diet <- sd(dist_diet)
  result <- data.frame(diet, sd_diet)
  return(result)
}

# Calculate diet 

diet_values <- data.frame()

for(i in 1:nrow(joined_corr)){
  ms <- with(joined_corr[i,],
             calc_diet(Year.x, 10000,
                       d13C_corr, sd_d13C_corr,
                       0.8, 1.1
             )) 
  diet_values <- rbind(diet_values, ms)
}

# Join

joined_diet_corr <- cbind(joined_corr, diet_values)

write.csv(joined_diet_corr, "Outputs/04_Misc/Lipid_Corrected/Lipid_Corrected_Diet.csv", row.names = F)
