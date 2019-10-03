#### Rethinking Temperature ####

setwd("~/PhD/GitHub/mytophid-ears/MixSIAR")

library(tidyverse)
library(rjags)
library(coda)

### Load in data

myct <- read.csv("Myctophids_Master.csv")
myct$dif <- myct$d18O - myct$D18O_vals

hist(rnorm(1000, 2, 3))

## Create temperature function

Temp <- function(Num){
myct_1 <- dplyr::filter(myct, MyNumber == Num)

## Try in JAGS

iso_list <- list(
  iso = myct_1$dif,
  sigma = 1/(0.33^2),
  a_obs = 3.90,
  a_var = 1/(0.24^2),
  b_obs = -0.20,
  b_var = 1/(0.019^2),
  N = 1
  #temp_mu = temp_mu,
  #temp_sigma = 1/(temp_sigma^2)
)

inits <- list(Temp = 0.0)

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

jags_mod <- jags.model(file = "Temp_Jags.txt", data = iso_list, inits = inits, n.chains = 3, n.adapt = 50000)

output <- coda.samples(jags_mod,
                       c("Temp", "a_est", "b_est"),
                       n.iter = 100000,
                       thin = 50)

## Summary and traceplots

sum <- summary(output)
print(sum)

capture.output(c(sum), file = paste("Summary_", Num, ".txt", sep = ""))

bmp(file = paste("Plot_Temp", Num, ".bmp", sep = ""))
plot(output)
dev.off()

## Diagnostics

gel <- gelman.diag(output, confidence = 0.95)
gel <- gel$psrf
gelman <- as.data.frame(gel[, 1])
print(gelman)
capture.output(gelman, file = "Gelmen_Temp.txt")

samp_size <- as.data.frame(effectiveSize(output))
print(samp_size)

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

capture.output(c(gelman, samp_size, geweke_fail), file = paste("Diagnostics_", Num, ".txt", sep = ""))

## Posterior

post_1 <- as.data.frame(output[[1]])
post_2 <- as.data.frame(output[[2]])
post_3 <- as.data.frame(output[[3]])

post_full <- rbind(post_1, post_2, post_3)

write.csv(post_full, paste("Post_Temp_", Num, ".csv", sep = ""))
}

for(i in 1:nrow(myct)){
  with(myct[i,],
       Temp(MyNumber))
}

Temp("BAS_220")
