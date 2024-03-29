#### lm.ci function ####

# A function which plots a linear model and shaded 95% confidence intervals in base R

lm.ci <- function(x_data, # Data
                  a, # Intercept 
                  b, # Slope
                  a_up_ci, b_up_ci, # Upper 95% CI of intercept and slope
                  a_low_ci, b_low_ci){ # Lower 95% CI of intercept and slope
  # Plot model
  
  x0 <- min(x_data)
  x1 <- max(x_data)
  y0 <- a + x0*b
  y1 <- a + x1*b
  #segments(x0, y0, x1, y1, lwd = 2)
  df <- data.frame()
  if(x0 < 0){
    
  # Create sequence for x values
    
  x_seq_1 <- seq(from = min(x_data), to = 0, length.out = 10000)
  x_seq_2 <- seq(from = 0, to = max(x_data), length.out = 10000)
  
  # Create upper CI
  
  up_ci_1 <- a_up_ci + x_seq_1*b_low_ci
  up_ci_2 <- a_up_ci + x_seq_2*b_up_ci
  up_ci <- c(up_ci_1, up_ci_2)
  smooth_up_ci <- smooth.spline(up_ci, spar = 1)
  
  # Create lower CI
  
  low_ci_1 <- a_low_ci + x_seq_1*b_up_ci
  low_ci_2 <- a_low_ci + x_seq_2*b_low_ci
  low_ci <- c(low_ci_1, low_ci_2)
  smooth_low_ci <- smooth.spline(low_ci, spar = 1)
  
  # Create points for polygon
  
  yy <- c(smooth_low_ci$y, smooth_up_ci$y)
  xx <- c(x_seq_1, x_seq_2,
          x_seq_1, x_seq_2)
  
  # Order points for polygon
  
  xxnew <- xx[order(Arg(scale(xx) + scale(yy) * 1i))]
  yynew <- yy[order(Arg(scale(xx) + scale(yy) * 1i))]
  }
  else {
    x0 <- min(x_data)
    x1 <- max(x_data)
    y0 <- a + x0*b
    y1 <- a + x1*b
    #segments(x0, y0, x1, y1, lwd = 2)
    
    # Create sequence for x values
    
    x_seq_1 <- seq(from = min(x_data), to = max(x_data), length.out = 10000)
    
    # Create upper CI
    
    up_ci <- a_up_ci + x_seq_1*b_up_ci
    smooth_up_ci <- smooth.spline(up_ci, spar = 1)
    
    # Create lower CI
    
    low_ci <- a_low_ci + x_seq_1*b_low_ci
    smooth_low_ci <- smooth.spline(low_ci, spar = 1)
    
    # Create points for polygon
    
    yy <- c(smooth_low_ci$y, smooth_up_ci$y)
    xx <- c(x_seq_1, x_seq_1)
    
    # Order points for polygon
    
    xxnew <- xx[order(Arg(scale(xx) + scale(yy) * 1i))]
    yynew <- yy[order(Arg(scale(xx) + scale(yy) * 1i))]
  }
outputs <- list(x_ribbon = xxnew, y_ribbon = yynew)
outputs <- as.data.frame(outputs)
return(outputs)
}

# Save function

save(lm.ci, file = "LM_CI.Rdata")


