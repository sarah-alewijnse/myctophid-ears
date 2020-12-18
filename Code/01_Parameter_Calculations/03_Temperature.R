#### Temeprature ####

# Estimates experienced temperature of myctophids based on oxygen isotopes in RJAGS

# Load required packages

library(tidyverse)
library(rjags) # Gibbs sampler
library(coda) # Summarises MCMC outputs

# Read in data file

myct <- read.csv("Data/Myctophids_Master.csv")

# Calculate difference between d18O_oto and d18O_water

myct$dif <- myct$d18O - myct$D18O_vals

# Create histogram of priors (based on Scotia Sea temperatures)

hist(rnorm(1000, 2, 3))

#### Create temperature function ####

# Enables you to loop the temperature function over the whole dataset

Temp <- function(Num){
myct_1 <- dplyr::filter(myct, MyNumber == Num)

# Set priors/data

iso_list <- list(
  iso = myct_1$dif, # Difference between d18O_oto and d18O_water
  sigma = 1/(0.33^2),
  
  # Parameters from Hoie et al. 2004
  
  a_obs = 3.90,
  a_var = 1/(0.24^2),
  b_obs = -0.20,
  b_var = 1/(0.019^2),
  N = 1
  #temp_mu = temp_mu,
  #temp_sigma = 1/(temp_sigma^2)
)

inits <- list(Temp = 0.0)

# Write the JAGS model

cat("model
    {
    for (i in 1:N){
    mu[i] <- a_est + Temp * b_est
    iso[i] ~ dnorm(mu[i], tau)
    }
    a_est ~ dnorm(a_obs, a_var)
    b_est ~ dnorm(b_obs, b_var)
    Temp ~ dnorm(2, 1/(3^2))
    tau <- sigma
    }", file="Temp_Jags.txt")

# Run JAGS model

jags_mod <- jags.model(file = "Outputs/04_Misc/01_JAGS_Model_Text_Files/Temp_Jags.txt", data = iso_list, inits = inits, n.chains = 3, n.adapt = 50000)

# Get outputs for temperature, a and b

output <- coda.samples(jags_mod,
                       c("Temp", "a_est", "b_est"),
                       n.iter = 100000,
                       thin = 50)

# Get summary and traceplots

sum <- summary(output)
print(sum)

capture.output(c(sum), file = paste("Summary_", Num, ".txt", sep = ""))

bmp(file = paste("Plot_Temp", Num, ".bmp", sep = ""))
plot(output)
dev.off()

# Get Gelman-Rubin diagnostic

gel <- gelman.diag(output, confidence = 0.95)
gel <- gel$psrf
gelman <- as.data.frame(gel[, 1])
print(gelman)
capture.output(gelman, file = "Gelmen_Temp.txt")

# Get effective sample size

samp_size <- as.data.frame(effectiveSize(output))
print(samp_size)

# Get Geweke diagnostic

geweke <- coda::geweke.diag(output)
geweke.all <- data.frame(matrix(NA, nrow = 3, ncol = 3))
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

# Output diagnostics

capture.output(c(gelman, samp_size, geweke_fail), file = paste("Diagnostics_", Num, ".txt", sep = ""))

# Save and output postiors

post_1 <- as.data.frame(output[[1]])
post_2 <- as.data.frame(output[[2]])
post_3 <- as.data.frame(output[[3]])

post_full <- rbind(post_1, post_2, post_3)

write.csv(post_full, paste("Outputs/01_Parameter_Calculations/02_Temperature/Post_Temp_", Num, ".csv", sep = ""))
}

# Test with a single individual

Temp("BAS_220")

# Loop over whole dataset

for(i in 1:nrow(myct)){
  with(myct[i,],
       Temp(MyNumber))
}


