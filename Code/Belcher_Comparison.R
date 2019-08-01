#### Belcher Comparison ####

library(tidyverse)
library(rtruncnorm)

myct <- read.csv("Outputs/Combined.csv")
myct_tidy <- filter(myct, Weight_SD == "0")
myct_tidy$ln_Weight <- log(myct_tidy$Weight.x)

Belcher_MR <- function(param_1, param_1_sd,
                       param_2, param_2_sd,
                       Ln_Weight, reps,
                       param_3, param_3_sd,
                       temp, temp_sd, temp_min, temp_max){
  
  # Calculate distributions with set.seeds to ensure reproducibility
  set.seed(param_1)
  dist_param_1 <- rnorm(reps, param_1, param_1_sd)
  set.seed(param_2)
  dist_param_2 <- rnorm(reps, param_2, param_2_sd)
  set.seed(param_3)
  dist_param_3 <- rnorm(reps, param_3, param_3_sd)
  set.seed(temp)
  dist_temp <- rtruncnorm(reps, temp_min, temp_max, temp, temp_sd)
  set.seed(Sys.time())
  
  # Calculate MR
  dist_MR <- dist_param_1 - dist_param_2*Ln_Weight + dist_param_3*dist_temp
  min_MR <- min(dist_MR)
  max_MR <- max(dist_MR)
  max_dens <- which.max(density(dist_MR)$y)
  MR <- density(dist_MR)$x[max_dens]
  sd_MR <- sd(dist_MR)
  result <- data.frame(MR, sd_MR, min_MR, max_MR)
  return(result)
}

with(myct_tidy[4,],
     Belcher_MR(-1.315, 0.468,
                -0.2665, 0.0516,
                ln_Weight, 10000,
                0.0848, 0.0108,
                temp, sd_temp, min_temp, max_temp))

M_values <- data.frame()

for(i in 1:nrow(myct_tidy)){
  ms <- with(myct_tidy[i,],
             Belcher_MR(-1.315, 0.468,
                        -0.2665, 0.0516,
                        ln_Weight, 10000,
                        0.0848, 0.0108,
                        temp, sd_temp, min_temp, max_temp))
  M_values <- rbind(M_values, ms)
}

write.csv(M_values, "Outputs/Belcher_MR.csv", row.names = F)
