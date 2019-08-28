#### MixSIAR ####

library(tidyverse)
library(MixSIAR)

### Edit load_mix_data function

load_mix_data_mod <- function (filename, iso_names, factors, fac_random, fac_nested, 
          cont_effects) 
{
  X <- filename
  n.iso <- length(iso_names)
  if (n.iso == 0) {
    stop(paste("*** Error: No isotopes/tracers selected. Please select 1 or more\n        isotopes/tracers, and then load your consumer/mixture data again. ***", 
               sep = ""))
  }
  if (length(fac_random) != length(factors)) {
    stop(paste("*** Error: You have specified factors to include without saying\n        if they are random or fixed effects (length of fac_random should\n        match length of factors). Please check your load_mix_data line and try again. ***", 
               sep = ""))
  }
  if (length(factors) == 2 && length(fac_nested) != 2) {
    stop(paste("*** Error: You have specified factors to include without saying\n        if they are nested or independent (length of fac_nested should\n        match length of factors). Please check your load_mix_data line and try again. ***", 
               sep = ""))
  }
  if (length(factors) == 2) {
    if (!is.na(fac_nested[1])) {
      if (fac_nested[1] == TRUE && fac_nested[2] == TRUE) {
        stop(paste("*** Error: Both factors cannot be nested within each other. Please check\n              the fac_nested argument in your load_mix_data line and try again. ***", 
                   sep = ""))
      }
    }
  }
  n.effects <- length(factors)
  n.re <- sum(fac_random)
  n.fe <- n.effects - n.re
  fere <- ifelse(n.effects == 2 & n.re < 2, TRUE, FALSE)
  if (n.effects == 1) 
    fac_nested <- FALSE
  if (n.effects > 2) {
    stop(paste("*** Error: More than 2 random/fixed effects selected (MixSIAR can only\n        currently handle 0, 1, or 2 random/fixed effects). Please choose 0, 1,\n        or 2 random/fixed effects and then load your consumer/mixture data again. ***", 
               sep = ""))
  }
  n.ce <- length(cont_effects)
  if (n.ce > 1) {
    stop(paste("*** Error: More than 1 continuous effect selected (MixSIAR can only\n        currently handle 0 or 1 continuous effects). Please choose 0 or 1\n        continuous effects and then load your consumer/mixture data again. ***", 
               sep = ""))
  }
  if (sum(is.na(match(iso_names, colnames(X)))) > 0) {
    stop(paste("*** Error: Your 'iso_names' do not match column names in your\n        mixture data file (case sensitive). Please check your mix .csv data \n        file and load_mix_data line, then try again. ***", 
               sep = ""))
  }
  if (sum(is.na(match(factors, colnames(X)))) > 0) {
    stop(paste("*** Error: Your 'factors' do not match column names in your\n        mixture data file (case sensitive). Please check your mix .csv data\n        file and load_mix_data line, then try again. ***", 
               sep = ""))
  }
  if (sum(is.na(match(cont_effects, colnames(X)))) > 0) {
    stop(paste("*** Error: Your 'cont_effects' do not match column names in your\n        mixture data file (case sensitive). Please check your mix .csv data \n        file and load_mix_data line, then try again. ***", 
               sep = ""))
  }
  N <- dim(X)[1]
  X_iso_cols <- match(iso_names, colnames(X))
  X_iso <- as.matrix(X[, X_iso_cols[]])
  MU_names <- paste("Mean", iso_names, sep = "")
  SIG_names <- paste("SD", iso_names, sep = "")
  FAC <- replicate(n.effects, NULL)
  if (n.effects > 0) {
    for (i in 1:n.effects) {
      re <- fac_random[i]
      fac_values <- X[, factors[i]]
      fac_name <- factors[i]
      fac_levels <- length(unique(fac_values))
      if (is.numeric(fac_values)) {
        fac_labels <- paste(rep(factors[i], fac_levels), 
                            levels(factor(fac_values)), sep = " ")
      }
      else {
        fac_labels <- levels(factor(fac_values))
      }
      fac_values <- as.numeric(factor(fac_values))
      FAC[[i]] <- list(values = fac_values, levels = fac_levels, 
                       labels = fac_labels, lookup = NULL, re = re, 
                       name = fac_name)
    }
    if (n.re == 2 & !is.na(fac_nested[1])) {
      if (n.re == 2 & fac_nested[2]) {
        for (lev in 1:FAC[[2]]$levels) {
          FAC[[2]]$lookup[lev] <- FAC[[1]]$values[which(FAC[[2]]$values == 
                                                          lev)][1]
        }
      }
      if (n.re == 2 & fac_nested[1]) {
        for (lev in 1:FAC[[1]]$levels) {
          FAC[[1]]$lookup[lev] <- FAC[[2]]$values[which(FAC[[1]]$values == 
                                                          lev)][1]
        }
      }
    }
    if (n.fe == 1 & n.re == 1 & fac_random[1]) {
      tmp <- FAC[[1]]
      FAC[[1]] <- FAC[[2]]
      FAC[[2]] <- tmp
      factors <- rev(factors)
      fac_random <- rev(fac_random)
      fac_nested <- rev(fac_nested)
    }
  }
  CE_orig <- replicate(n.ce, NULL)
  CE <- replicate(n.ce, NULL)
  CE_center <- rep(NA, n.ce)
  CE_scale <- rep(NA, n.ce)
  if (n.ce > 0) {
    for (i in 1:n.ce) {
      CE_orig[[i]] <- X[, cont_effects[i]]
      CE[[i]] <- scale(X[, cont_effects[i]], center = TRUE, 
                       scale = TRUE)
      CE_center[i] <- attributes(CE[[i]])$"scaled:center"
      CE_scale[i] <- attributes(CE[[i]])$"scaled:scale"
    }
  }
  return(list(data = X, data_iso = X_iso, n.iso = n.iso, n.re = n.re, 
              n.ce = n.ce, FAC = FAC, CE = CE, CE_orig = CE_orig, CE_center = CE_center, 
              CE_scale = CE_scale, cont_effects = cont_effects, MU_names = MU_names, 
              SIG_names = SIG_names, iso_names = iso_names, N = N, 
              n.fe = n.fe, n.effects = n.effects, factors = factors, 
              fac_random = fac_random, fac_nested = fac_nested, fere = fere))
}

### Partition mixture data

mixture <- read.csv("myct_mix.csv")
mix_1 <- slice(mixture, 1)

### Load mixture data

mix <- load_mix_data_mod(mix_1,
                       iso_names = "d13C",
                       factors = "MyNumber",
                       fac_random = FALSE,
                       fac_nested = FALSE,
                       cont_effects = NULL)

### Partition source data

source <- read.csv("myct_source.csv")

