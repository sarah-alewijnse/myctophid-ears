#### Pseudo-Bayesian Kruskall Wallis Test ####

library(tidyverse)
library(car)
library(FSA)
library(HDInterval)

myct <- read.csv("Outputs/Combined.csv")

PseudoBayes_KW <- function(reps, n_row,
                           cat_var, cat_var_label,
                           cont_var, cont_var_HDI, cont_var_label){
  set.seed(Sys.time())
  results <- replicate(reps, {
    values_T <- data.frame()
    values_S <- data.frame()
    
    for(i in 1:n_row){
      
      # Get values
      val_T <- rnorm(1, cont_var, cont_var_HDI)
      sci <- cat_var
      values_T <- rbind(values_T, val_T)
      values_S <- rbind(values_S, sci)
      values <- cbind(values_S, values_T)
      colnames(values) <- c(cat_var_label, cont_var_label)
    }
    
    # Do test
    
    mod <- kruskal.test(cont_var ~ cat_var, data = values)
    chi <- mod[["statistic"]]
    df <- mod[["parameter"]]
    p_value <- mod[["p.value"]]
    return(rbind(chi, df, p_value))
  })
  
  results <- as.data.frame(results)
  results_t <- t(results)
  results_t <- as.data.frame(results_t)
  return(results_t)
}

save("PseudoBayes_KW", file = "Functions/PseudoBayes_KW.Rdata")

## Test

with(myct,
     PseudoBayes_KW(1000, nrow(myct),
                    sciname, "sciname",
                    temp, temp_HDI_range, "Temperature"))
