#### Belcher Oxygen Consumption ####

# Estimates log oxygen consumption (ul/mg/h) based on equation 1 (Belcher et al. 2019)

# Load packages

library(tidyverse)
library(rjags) # Gibbs sampler
library(coda) # Summarises MCMC outputs

# Load in data file

myct <- read.csv("Data/Myctophids_M_Temp.csv")

# Remove those with missing body mass data

myct_tidy <- myct[!is.na(myct$Weight.x),]

# Log body masses

myct_tidy$ln_Weight <- log(myct_tidy$Weight.x)

#### Create oxygen consumption function ####

Bel <- function(Num){
  myct_1 <- dplyr::filter(myct_tidy, MyNumber == Num)
  
  # Set priors/data
  
  param_list <- list(
    param_1 = -1.315,
    param_1_var = 1/(0.468^2),
    param_2 = -0.2665,
    param_2_var = 1/(0.0516^2),
    param_3 = 0.0848,
    param_3_var = 1/(0.0108^2),
    weight = myct_1$ln_Weight,
    temp = myct_1$mean_Temp,
    temp_var = 1/(myct_1$sd_Temp^2),
    sigma = 1/(1^2),
    N = 1
  )
  
  inits <- list(temp_est = 0.0)
  
  # Write the JAGS model
  
  cat("model
      {
      for (i in 1:N){
      mu[i] <- param_1_est + param_2_est * weight + param_3_est * temp_est
      Metabol[i] ~ dnorm(mu[i], tau)
      }
      param_1_est ~ dnorm(param_1, param_1_var)
      param_2_est ~ dnorm(param_2, param_2_var)
      param_3_est ~ dnorm(param_3, param_3_var)
      temp_est ~ dnorm(temp, temp_var)
      tau <- sigma
      }", file="Outputs/04_Misc/01_JAGS_Model_Text_Files/Belcher_JAGS.txt")

  # Run JAGS model
  
  jags_mod <- jags.model(file = "Outputs/04_Misc/04_JAGS_Model_Text_Files/Belcher_JAGS.txt", data = param_list, inits = inits, n.chains = 3, n.adapt = 50000)
  
  # Get outputs for temperature and equation parameters
  
  output <- coda.samples(jags_mod,
                         c("Metabol", "param_1_est", "param_2_est", "param_3_est", "temp_est"),
                         n.iter = 100000,
                         thin = 50)
  
  output_graph <- coda.samples(jags_mod,
                               c("Metabol", "temp_est"),
                               n.iter = 100000,
                               thin = 50)
  
  # Get parameter summaries and traceplots
  
  sum <- summary(output)
  print(sum)
  
  capture.output(c(sum), file = paste("Outputs/01_Parameter_Calculations/03_Oxygen_Consumption/Summaries/Summary_", Num, ".txt", sep = ""))
  
  bmp(file = paste("Outputs/01_Parameter_Calculations/03_Oxygen_Consumption/Traceplots/Plot_Metabol", Num, ".bmp", sep = ""))
  plot(output_graph)
  dev.off()
  
  # Get diagnostics
  
  gel <- gelman.diag(output, confidence = 0.95)
  gel <- gel$psrf
  gelman <- as.data.frame(gel[, 1])
  print(gelman)
  #capture.output(gelman, file = paste("Outputs/01_Parameter_Calculations/03_Oxygen_Consumption/Gelmen_Metabol", Num, ".txt"))
  
  samp_size <- as.data.frame(effectiveSize(output))
  print(samp_size)
  
  geweke <- coda::geweke.diag(output)
  geweke.all <- data.frame(matrix(NA, nrow = 5, ncol = 3))
  colstring <- rep(NA, 3)
  for (i in 1:3) {
    geweke.tmp <- as.data.frame(geweke[[i]]$z)
    geweke.all[, i] <- geweke.tmp
    colstring[i] <- c(paste("chain", i, sep = ""))
  }
  rownames(geweke.all) <- coda::varnames(output)
  colnames(geweke.all) <- colstring
  geweke.all <- round(geweke.all, 3)
  w <- which(!is.nan(geweke[[1]]$z))
  geweke.all <- geweke.all[w, ]
  geweke_fail <- matrix(NA, nrow = 1, ncol = 3)
  for (i in 1:3) {
    geweke_fail[1, i] <- sum(abs(geweke.all[, i]) > 1.96)
  }
  colnames(geweke_fail) <- paste("Chain", 1:3)
  rownames(geweke_fail) <- "Geweke"
  print(geweke_fail)
  
  capture.output(c(gelman, samp_size, geweke_fail), file = paste("Outputs/01_Parameter_Calculations/03_Oxygen_Consumption/Diagnostics/Diagnostics_", Num, ".txt", sep = ""))
  
  ## Posterior
  
  post_1 <- as.data.frame(output[[1]])
  post_2 <- as.data.frame(output[[2]])
  post_3 <- as.data.frame(output[[3]])
  
  post_full <- rbind(post_1, post_2, post_3)
  
  write.csv(post_full, paste("Outputs/01_Parameter_Calculations/03_Oxygen_Consumption/Posteriors/Post_", Num, ".csv", sep = ""))
}

# Test with a single individual

Bel("BAS_221")

# Loop over data

for(i in 1:nrow(myct_tidy)){
  with(myct_tidy[i,],
       Bel(MyNumber))
}
